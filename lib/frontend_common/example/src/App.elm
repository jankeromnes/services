module App exposing (..)

import App.Page1
import App.Page2
import AppCommon
import Navigation
import UrlParser


--
-- ROUTING
--
-- inspired by https://github.com/rofrol/elm-navigation-example
--


type Route
    = NotFoundRoute
    | HomeRoute
    | LoginRoute (Maybe String) (Maybe String) (Maybe String)
    | LogoutRoute
    -- PAGES
    | Page1Route 
    | Page2Route App.Page2.Route


-- PAGES
pages : List (AppCommon.Page Route b)
pages =
    [ App.Page1.page Page1Route
    , App.Page2.page Page2Route
    ]


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    pages
        |> List.map (\x -> x.matcher)
        |> List.append
            [ UrlParser.map HomeRoute UrlParser.top
            , UrlParser.map NotFoundRoute (UrlParser.s "404")
            , UrlParser.map LoginRoute
                (UrlParser.s "login"
                    <?> UrlParser.stringParam "clientId"
                    <?> UrlParser.stringParam "accessToken"
                    <?> UrlParser.stringParam "certificate"
                )
            , UrlParser.map LogoutRoute (UrlParser.s "logout")
            ]
        |> UrlParser.oneOf


reverseRoute : Route -> String
reverseRoute route =
    case route of
        NotFoundRoute ->
            "/404"
        HomeRoute ->
            "/"
        LoginRoute _ _ _ ->
            "/login"
        LogoutRoute ->
            "/logout"
        -- PAGES
        Page1Route ->
            "/page1"
        Page2Route route ->
            App.Page2.reverseRoute route


parseLocation : Navigation.Location -> Route
parseLocation location =
    location
        |> UrlParser.parsePath routeParser
        |> Maybe.withDefault NotFoundRoute


navigateTo : Route -> Cmd Msg
navigateTo route =
    route
        |> reverseRoute
        |> Navigation.newUrl



--
-- FLAGS
--


type alias Flags =
    { bugzilla : BugzillaLogin.Model
    , bugzillaUrl : String
    , taskcluster : TaskclusterLogin.Model
    , version : String
    }



--
-- MODEL
--


type alias Model =
    { history : List Navigation.Location
    , route : Route
    , taskcluster : TaskclusterLogin.Model
    , taskclusterScopes : App.UserScopes.Model
    , taskclusterRoles : App.UserRoles.Model
    , version : String
    -- PAGES
    , page1: App.Page1.Model
    , page2: App.Page2.Model
    }


--
-- MESSAGES
--


type Msg
    = UrlChange Navigation.Location
    | NavigateTo Route
    | Tick Time.Time
    | HawkMsg Hawk.Msg
    | TaskclusterLoginMsg TaskclusterLogin.Msg
    | TaskclusterScopesMsg App.TaskClusterScopes.Msg
    | TaskclusterRolesMsg App.TaskclusterRoles.Msg
    -- PAGES
    | Page1Msg App.Page1.Msg
    | Page2Msg App.Page2.Msg
