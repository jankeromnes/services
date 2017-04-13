module Main exposing (..)

import Navigation
import App
import App.Page1
import App.Page2
import BugzillaLogin


main : Program App.Flags App.Model App.Msg
main =
    Navigation.programWithFlags App.UrlChange
        { init = init
        , view = App.view viewRoute
        , update = update
        , subscriptions = subscriptions
        }


init : App.Flags -> Navigation.Location -> ( App.Model, Cmd App.Msg )
init flags location =
    let
        route =
            App.parseLocation location  -- XXX

        ( bugzilla, bugzillaCmd ) =
            BugzillaLogin.init flags.bugzillaUrl flags.bugzilla

        ( user, userCmd ) =
            TaskclusterLogin.init flags.taskcluster

        model =
            { history = [ location ]
            , route = route
            , bugzilla = bugzilla
            , user = user
            , userScopes = App.UserScopes.init
            , userRoles = App.UserRoles.init
            -- PAGES
            , page1 = App.Page1.init
            , page2 = App.Page2.init
            }

        ( model, cmd ) =
            initRoute model route
    in
       ( model
       , Cmd.batch
            [
              Cmd.map BugzillaMsg bugzillaCmd
            , Cmd.map UserMsg userCmd
            , cmd
            ]


initRoute : App.Model -> App.Route -> ( App.Model, Cmd App.Msg )
initRoute model route =
    case route of
        App.NotFoundRoute ->
            model ! []

        App.HomeRoute ->
            { model
                | trychooser = App.TryChooser.init
                , treestatus =
                    App.TreeStatus.init model.treestatus.baseUrl
            }
                ! [ Utils.performMsg (App.UserScopesMsg App.UserScopes.FetchScopes) ]

        App.LoginRoute clientId accessToken certificate ->
            let
                -- TODO: parsing of the arguments should go into TaskclusterLogin.elm
                certificate_ =
                    case certificate of
                        Just text ->
                            TaskclusterLogin.decodeCertificate text
                                |> Result.toMaybe

                        Nothing ->
                            Nothing

                credentials =
                    case ( clientId, accessToken ) of
                        ( Just clientId_, Just accessToken_ ) ->
                            Just
                                (TaskclusterLogin.Credentials
                                    clientId_
                                    accessToken_
                                    certificate_
                                )

                        _ ->
                            Nothing

                loginCmd =
                    case credentials of
                        Just credentials_ ->
                            Utils.performMsg
                                (App.TaskclusterLoginMsg
                                    (TaskclusterLogin.Logging credentials_)
                                )

                        Nothing ->
                            Cmd.none
            in
                model
                    ! [ loginCmd
                      , App.navigateTo App.HomeRoute
                      ]

        App.LogoutRoute ->
            model
                ! [ Utils.performMsg (App.TaskclusterLoginMsg TaskclusterLogin.Logout)
                    -- TODO: we should be redirecting to the url that we were loging in from
                  , Utils.performMsg (App.NavigateTo App.HomeRoute)
                  ]

        App.Page1Route ->
            model
                ! [ Utils.performMsg (App.Page1Msg App.Page1.Load)
                  , Utils.performMsg (App.UserScopesMsg App.UserScopes.FetchScopes)
                  ]

        App.Page2Route route ->
            model
                ! [ Utils.performMsg (App.Page2Msg (App.Page2.Types.NavigateTo route))
                  , Utils.performMsg (App.UserScopesMsg App.UserScopes.FetchScopes)
                  ]



update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of

        --
        -- ROUTING
        --
        App.UrlChange location ->
            { model
                | history = location :: model.history
                , route = App.parseLocation location
            }
                ! []

        App.NavigateTo route ->
            let
                ( newModel, newCmd ) =
                    initRoute model route
            in
                ( newModel
                , Cmd.batch
                    [ App.navigateTo route
                    , newCmd
                    ]
                )

        --
        -- LOGIN / LOGOUT
        --

        App.Tick time ->
            if TaskclusterLogin.isCertificateExpired time model.taskcluster then
                update (App.TaskclusterLoginMsg TaskclusterLogin.Logout) model
            else
                ( model, Cmd.none )

        App.TaskclusterLoginMsg taskclusterMsg ->
            let
                ( newUser, taskclusterCmd ) =
                    TaskclusterLogin.update taskclusterMsg model.taskcluster
            in
                ( { model | taskcluster = newUser }
                , Cmd.map App.TaskclusterLoginMsg taskclusterCmd
                )


        -- 
        -- BUGZILLA
        --
        App.BugzillaMsg bugzillaMsg ->
            let
                ( newBugzilla, bugzillaCmd ) =
                    BugzillaLogin.update bugzillaMsg model.bugzilla
            in
                ( { model | bugzilla = newBugzilla}
                , Cmd.map BugzillaMsg bugzillaCmd
                )

        --
        -- HAWK REQUESTS
        --
        App.HawkMsg hawkMsg ->
            let
                ( requestId, cmd, response ) =
                    Hawk.update hawkMsg

                routeHawkMsg route =
                    if String.startsWith "TaskclusterScopes" route then
                        route
                            |> String.dropLeft (String.length "TaskclusterScopes")
                            |> App.UserScopes.hawkResponse response
                            |> Cmd.map App.TaskclusterScopesMsg

                    else if String.startsWith "TaskclusterRoles" route then
                        route
                            |> String.dropLeft (String.length "TaskclusterRoles")
                            |> App.UserScopes.hawkResponse response
                            |> Cmd.map App.TaskclusterRolesMsg

                    -- PAGES 

                    else if String.startsWith "Page1" route then
                        route
                            |> String.dropLeft (String.length "Page1")
                            |> App.Page1.Api.hawkResponse response
                            |> Cmd.map App.Page1Msg

                    else if String.startsWith "Page2" route then
                        route
                            |> String.dropLeft (String.length "Page2")
                            |> App.Page2.Api.hawkResponse response
                            |> Cmd.map App.Page2Msg
                    else
                        Cmd.none

                appCmd =
                    requestId
                        |> Maybe.map routeHawkMsg
                        |> Maybe.withDefault Cmd.none
            in
                ( model
                , Cmd.batch
                    [ Cmd.map App.HawkMsg cmd
                    , appCmd
                    ]
                )

        App.TaskclusterScopes msg_ ->
            let
                ( newModel, newCmd, hawkCmd ) =
                    App.TaskclusterScopes.update msg_ model.taskclusterScopes
            in
                ( { model | taskclusterScopes = newModel }
                , hawkCmd
                    |> Maybe.map (\req -> [ hawkSend model.taskcluster "TaskclusterScopes" req ])
                    |> Maybe.withDefault []
                    |> List.append [ Cmd.map App.TaskclusterScopesMsg newCmd ]
                    |> Cmd.batch
                )

        App.TaskclusterRoles msg_ ->
            let
                ( newModel, newCmd, hawkCmd ) =
                    App.TaskclusterRoles .update msg_ model.taskclusterRoles
            in
                ( { model | taskclusterRoles = newModel }
                , hawkCmd
                    |> Maybe.map (\req -> [ hawkSend model.taskcluster "TaskclusterRoles" req ])
                    |> Maybe.withDefault []
                    |> List.append [ Cmd.map App.TaskclusterRolesMsg newCmd ]
                    |> Cmd.batch
                )

        -- PAGES

        App.Page1 msg_ ->
            let
                ( newModel, newCmd ) =
                    App.TryChooser.update msg_ model.trychooser
            in
                ( { model | trychooser = newModel }
                , Cmd.map App.TryChooserMsg newCmd
                )

        App.Page2 msg_ ->
            let
                ( newModel, newCmd, hawkCmd ) =
                    App.Page2.update model.page2 msg_
            in
                ( { model | page2 = newModel }
                , hawkCmd
                    |> Maybe.map (\req -> [ hawkSend model.taskcluster "Page2" req ])
                    |> Maybe.withDefault []
                    |> List.append [ Cmd.map App.Page2Msg newCmd ]
                    |> Cmd.batch
                )


hawkSend :
    TaskclusterLogin.Model
    -> String
    -> Hawk.Request
    -> Cmd App.Msg
hawkSend user page request =
    let
        pagedRequest =
            { request | id = (page ++ request.id) }
    in
        case user of
            Just user2 ->
                Hawk.send pagedRequest user2
                    |> Cmd.map App.HawkMsg

            Nothing ->
                Cmd.none


viewRoute : App.Model -> Html App.Msg
viewRoute model =
    case model.route of
        App.NotFoundRoute ->
            App.Layout.viewNotFound model

        App.HomeRoute ->
            App.Home.view model

        App.LoginRoute _ _ _ ->
            -- TODO: this should be already a view on TaskclusterLogin
            text "Logging you in ..."

        App.LogoutRoute ->
            -- TODO: this should be already a view on TaskclusterLogin
            text "Logging you out ..."

        -- PAES 
        App.Page1 ->
            App.Page.view model.page1
                |> Html.map App.Page1Msg

        App.Page2 route ->
            App.Page2.view model.page2 route
                |> Html.map App.Page2Msg


subscriptions : App.Model -> Sub App.Msg
subscriptions model =
    Sub.batch
        [ TaskclusterLogin.subscriptions App.TaskclusterLoginMsg
        , Hawk.subscriptions App.HawkMsg
        , Time.every (50 * Time.second) App.Tick
        ]
