module App.Home exposing (..)

import App.Types
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils


type Msg
    = Nop



data =
    [ { id = "desktop-firefox-nightly"
      , name = "Mozilla Firefox Nightly"
      }
    , { id = "desktop-firefox-aurora"
      , name = "Mozilla Firefox Aurora"
      }
    , { id = "desktop-firefox-beta"
      , name = "Mozilla Firefox Beta"
      }
    , { id = "desktop-firefox-release"
      , name = "Mozilla Firefox Release"
      }
    , { id = "desktop-firefox-esr"
      , name = "Mozilla Firefox ESR"
      }
    ]
      

viewPipeline pipeline = 
    div
        [ class "list-group-item" ]
        [ a
            [ href "/pipeline"
            ]
            [ text pipeline.name
            ]
        ]

view : a -> Html Msg
view model =
    div 
        [ class "container" ]
        [ h1 [] [ text "Pipelines" ]
        , div [ class "list-group" ] (List.map viewPipeline data)
        ]
