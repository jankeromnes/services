module AppCommon exposing (..)

import UrlParser


type alias Page a b =
    { title : String
    , description : String
    , matcher : UrlParser.Parser (a -> b) b
    }
