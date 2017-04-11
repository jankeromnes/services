port module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import String
import RouteUrl exposing (UrlChange)
import RouteUrl.Builder as Builder exposing (Builder, builder, replacePath)
import RemoteData exposing (WebData, RemoteData(..))
import Navigation exposing (Location)
import BugzillaLogin as Bugzilla
import TaskclusterLogin as User
import Hawk
import Utils
import App.Utils exposing (eventLink)
import App.Home as Home
import App.ReleaseDashboard as ReleaseDashboard
import App.Types
import App.Pipeline


type
    Msg
    -- Extensions integration
    = BugzillaMsg Bugzilla.Msg
    | UserMsg User.Msg
    | HomeMsg Home.Msg
    | HawkRequest Hawk.Msg
      -- App code
    | ShowPage App.Types.Page
    | ReleaseDashboardMsg ReleaseDashboard.Msg
    | PipelineMsg App.Pipeline.Msg


type alias Role =
    { roleId : String
    , scopes : List String
    }


type alias Model =
    { -- Extensions integration
      user : User.Model
    , bugzilla :
        Bugzilla.Model
        -- App code
    , current_page : App.Types.Page
    , release_dashboard : ReleaseDashboard.Model
    }


type alias Flags =
    { taskcluster : Maybe User.Credentials
    , bugzilla : Maybe Bugzilla.Credentials
    , backend_uplift_url : String
    , bugzilla_url : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- Extensions integration
        ( bz, bzCmd ) =
            Bugzilla.init flags.bugzilla_url flags.bugzilla

        ( user, userCmd ) =
            User.init flags.taskcluster

        -- App init
        ( dashboard, newCmd ) =
            ReleaseDashboard.init flags.backend_uplift_url

        model =
            { bugzilla = bz
            , user = user
            , current_page = App.Types.Home
            , release_dashboard = dashboard
            }
    in
        ( model
        , -- Follow through with sub parts init
          Cmd.batch
            [ -- Extensions integration
              Cmd.map BugzillaMsg bzCmd
            , Cmd.map UserMsg userCmd
            , loadAllAnalysis model
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Extensions integration
        BugzillaMsg bzMsg ->
            let
                ( newBz, bzCmd ) =
                    Bugzilla.update bzMsg model.bugzilla
            in
                ( { model | bugzilla = newBz }
                , Cmd.map BugzillaMsg bzCmd
                )

        UserMsg userMsg ->
            let
                -- Update current user
                ( user, userCmd ) =
                    User.update userMsg model.user

                l =
                    Debug.log "new user" user

                -- Load analysis on user login
                commands =
                    List.concat
                        [ [ Cmd.map UserMsg userCmd ]
                        , case userMsg of
                            User.Logged _ ->
                                [ loadAllAnalysis model ]

                            _ ->
                                []
                        ]
            in
                ( { model | user = user }, Cmd.batch commands )

        HawkRequest hawkMsg ->
            let
                -- Always Redirect to release dashboard
                -- If we need another module, a prefix in requestId would be needed
                ( requestId, cmd, response ) =
                    Hawk.update hawkMsg

                dashboardCmd =
                    requestId
                        |> Maybe.map (ReleaseDashboard.routeHawkRequest response)
                        |> Maybe.withDefault Cmd.none
            in
                ( model
                , Cmd.batch
                    [ Cmd.map HawkRequest cmd
                    , Cmd.map ReleaseDashboardMsg dashboardCmd
                    ]
                )

        -- Routing
        ShowPage page ->
            ( { model | current_page = page }, Cmd.none )

        HomeMsg homeMsg ->
            -- Does nothing
            ( model, Cmd.none )

        -- Dashboard updates
        ReleaseDashboardMsg dashMsg ->
            let
                ( dashboard, cmd ) =
                    ReleaseDashboard.update dashMsg model.release_dashboard model.user model.bugzilla
            in
                ( { model | release_dashboard = dashboard, current_page = App.Types.ReleaseDashboard }
                , Cmd.map ReleaseDashboardMsg cmd
                )

        PipelineMsg pipelineMsg ->
            -- Does nothing
            ( model, Cmd.none )



loadAllAnalysis : Model -> Cmd Msg
loadAllAnalysis model =
    -- (Re)Load all dashboard analysis
    -- when user is loaded or is logged in
    case model.user of
        Just user ->
            Cmd.map ReleaseDashboardMsg (ReleaseDashboard.fetchAllAnalysis model.release_dashboard model.user)

        Nothing ->
            Cmd.none



-- Demo view


view : Model -> Html Msg
view model =
    div []
        [ nav [ class "navbar navbar-toggleable-md navbar-inverse bg-inverse" ]
            (viewNavBar model)
        , div [ id "content" ]
              [ div [ class "container-fluid" ]
                    [ viewDashboardStatus model.release_dashboard
                    , viewPage model
                    ]
              ]
        , viewFooter
        ]


viewPage : Model -> Html Msg
viewPage model =
    case model.current_page of
        App.Types.Home ->
            Html.map HomeMsg (Home.view model)

        App.Types.Bugzilla ->
            Html.map BugzillaMsg (Bugzilla.view model.bugzilla)

        App.Types.ReleaseDashboard ->
            Html.map ReleaseDashboardMsg (ReleaseDashboard.view model.release_dashboard model.bugzilla)

        App.Types.Pipeline ->
            Html.map PipelineMsg (App.Pipeline.view model)


viewNavBar : Model -> List (Html Msg)
viewNavBar model =
    [ button
        [ class "navbar-toggler hidden-md-up navbar-toggler-right"
        , type_ "button"
        , attribute "data-toggle" "collapse"
        , attribute "data-target" ".navbar-collapse"
        , attribute "aria-controls" "navbar-header"
        ]
        [ text "Menu" ]
    , pageLink App.Types.Home
        [ class "navbar-brand" ]
        [ text "ShipIt v2" ]
    , div [ class "collapse navbar-collapse" ]
        [ ul [ class "navbar-nav mr-auto " ] (viewNavDashboard model)
        , ul [ class "navbar-nav" ] (viewUser model)
        ]
    ]


viewUser : Model -> List (Html Msg)
viewUser model =
    case model.user of
        Just user ->
            viewDropdown user.clientId
                [ -- Link to TC manager
                  a
                    [ class "dropdown-item"
                    , href "https://tools.taskcluster.net/credentials"
                    , target "_blank"
                    ]
                    [ text "Manage credentials" ]
                  -- Display bugzilla status
                , viewBugzillaCreds model.bugzilla
                , -- Logout from TC
                  div [ class "dropdown-divider" ] []
                , a
                    [ Utils.onClick (UserMsg User.Logout)
                    , href "#"
                    , class "dropdown-item"
                    ]
                    [ text "Logout" ]
                ]

        Nothing ->
            viewLogin


viewBugzillaCreds : Bugzilla.Model -> Html Msg
viewBugzillaCreds bugzilla =
    case bugzilla.check of
        NotAsked ->
            a [ class "dropdown-item text-info" ]
                [ span [] [ text "No bugzilla auth" ]
                , span [] viewLoginBugzilla
                ]

        Loading ->
            a [ class "dropdown-item text-info disabled" ] [ text "Loading Bugzilla auth." ]

        Failure err ->
            a [ class "dropdown-item text-danger" ]
                [ span [] [ text ("Error while loading bugzilla auth: " ++ toString err) ]
                , span [] viewLoginBugzilla
                ]

        Success valid ->
            if valid then
                a [ class "dropdown-item text-success disabled" ] [ text "Valid bugzilla auth" ]
            else
                a [ class "dropdown-item text-danger" ]
                    [ span [] [ text "Invalid bugzilla auth" ]
                    , span [] viewLoginBugzilla
                    ]


viewLoginBugzilla : List (Html Msg)
viewLoginBugzilla =
    [ eventLink (ShowPage App.Types.Bugzilla) [ class "nav-link" ] [ text "Login Bugzilla" ]
    ]


viewNavDashboard : Model -> List (Html Msg)
viewNavDashboard model =
    case model.release_dashboard.all_analysis of
        NotAsked ->
            []

        Loading ->
            [ li [ class "nav-item text-info" ] [ text "Loading Bugs analysis..." ]
            ]

        Failure err ->
            [ li [ class "nav-item text-danger" ] [ text "No analysis available." ]
            ]

        Success allAnalysis ->
            (List.map viewNavAnalysis allAnalysis)


viewDashboardStatus : ReleaseDashboard.Model -> Html Msg
viewDashboardStatus dashboard =
    -- Display explicit error messages
    case dashboard.all_analysis of
        Failure err ->
            div [ class "alert alert-danger" ]
                [ h4 [] [ text "Error while loading analysis" ]
                , case err of
                    Http.Timeout ->
                        span [] [ text "A timeout occured during the request." ]

                    Http.NetworkError ->
                        span [] [ text "A network error occuring during the request, check your internet connectivity." ]

                    Http.BadPayload data response ->
                        let
                            l =
                                Debug.log "Unexpected payload: " data
                        in
                            span [] [ text "An unexpected payload was received, check your browser logs" ]

                    Http.BadStatus response ->
                        case response.status.code of
                            401 ->
                                p []
                                    ([ p [] [ text "You are not authenticated: please login again." ]
                                     ]
                                        ++ viewLogin
                                    )

                            _ ->
                                span [] [ text ("The backend produced an error " ++ (toString response)) ]

                    Http.BadUrl url ->
                        span [] [ text ("Invalid url : " ++ url) ]
                ]

        _ ->
            div [] []


viewNavAnalysis : ReleaseDashboard.Analysis -> Html Msg
viewNavAnalysis analysis =
    li [ class "nav-item analysis" ]
        [ analysisLink analysis.id
            [ class "nav-link" ]
            [ span [ class "name" ] [ text (analysis.name ++ " " ++ (toString analysis.version)) ]
            , if analysis.count > 0 then
                span [ class "badge badge-pill badge-primary" ] [ text (toString analysis.count) ]
              else
                span [ class "badge badge-pill badge-success" ] [ text (toString analysis.count) ]
            ]
        ]


viewLogin : List (Html Msg)
viewLogin =
    [ a
        [ Utils.onClick
            (User.redirectToLogin
                UserMsg
                "/login"
                "Uplift dashboard helps Mozilla Release Management team in their workflow."
            )
        , href "#"
        , class "nav-link"
        ]
        [ text "Login" ]
    ]


viewFooter : Html msg
viewFooter =
    footer []
        [ ul []
            [ li [] [ a [ href "https://github.com/mozilla-releng/services" ] [ text "Github" ] ]
            , li [] [ a [ href "#" ] [ text "Contribute" ] ]
            , li [] [ a [ href "#" ] [ text "Contact" ] ]
              -- TODO: add version / revision
            ]
        ]


viewDropdown : String -> List (Html msg) -> List (Html msg)
viewDropdown title pages =
    [ div [ class "dropdown" ]
        [ a
            [ class "nav-link dropdown-toggle btn btn-primary"
            , id ("dropdown" ++ title)
            , href "#"
            , attribute "data-toggle" "dropdown"
            , attribute "aria-haspopup" "true"
            , attribute "aria-expanded" "false"
            ]
            [ text title ]
        , div
            [ class "dropdown-menu dropdown-menu-right"
            , attribute "aria-labelledby" "dropdownServices"
            ]
            pages
        ]
    ]



-- Routing


pageLink : App.Types.Page -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
pageLink page attributes =
    eventLink (ShowPage page) attributes


analysisLink : Int -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
analysisLink analysis attributes =
    eventLink (ReleaseDashboardMsg (ReleaseDashboard.FetchAnalysis analysis)) attributes


location2messages : Location -> List Msg
location2messages location =
    let
        builder =
            Builder.fromUrl location.href
    in
        case Builder.path builder of
            first :: rest ->
                -- Extensions integration
                case first of
                    "login" ->
                        [ Builder.query builder
                            |> User.convertUrlQueryToUser
                            |> Maybe.map
                                (\x ->
                                    x
                                        |> User.Logging
                                        |> UserMsg
                                )
                            |> Maybe.withDefault (ShowPage App.Types.Home)
                        , ShowPage App.Types.Home
                        ]

                    "bugzilla" ->
                        [ ShowPage App.Types.Bugzilla
                        ]

                    "pipeline" ->
                        [ ShowPage App.Types.Pipeline
                        ]

                    "release-dashboard" ->
                        let
                            messages =
                                if List.length rest == 1 then
                                    case List.head rest of
                                        Just analysisId ->
                                            case String.toInt analysisId |> Result.toMaybe of
                                                Just analysisId_ ->
                                                    -- Load specified analysis
                                                    [ ReleaseDashboardMsg (ReleaseDashboard.FetchAnalysis analysisId_) ]

                                                Nothing ->
                                                    []

                                        -- not a string
                                        Nothing ->
                                            []
                                    -- empty string
                                else
                                    []

                            -- No sub query parts
                        in
                            -- Finish by showing the page
                            messages ++ [ ShowPage App.Types.ReleaseDashboard ]

                    _ ->
                        [ ShowPage App.Types.Home ]

            _ ->
                [ ShowPage App.Types.Home ]


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    Maybe.map Builder.toUrlChange <|
        case current.current_page of
            App.Types.ReleaseDashboard ->
                let
                    path =
                        case current.release_dashboard.current_analysis of
                            Success analysis ->
                                [ "release-dashboard", (toString analysis.id) ]

                            _ ->
                                [ "release-dashboard" ]
                in
                    Maybe.map
                        (Builder.prependToPath path)
                        (Just builder)

            App.Types.Bugzilla ->
                Maybe.map
                    (Builder.prependToPath [ "bugzilla" ])
                    (Just builder)

            App.Types.Pipeline ->
                Maybe.map
                    (Builder.prependToPath [ "pipeline" ])
                    (Just builder)

            _ ->
                Maybe.map
                    (Builder.prependToPath [])
                    (Just builder)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Extensions integration
          Sub.map BugzillaMsg (Bugzilla.bugzillalogin_get (Bugzilla.Logged))
        , Sub.map UserMsg (User.taskclusterlogin_get (User.Logged))
        , Sub.map HawkRequest (Hawk.hawk_send_request (Hawk.SendRequest))
        ]
