port module App.Pipeline exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)


-- 
-- Static data
--


pipeline : Pipeline
pipeline =
    { id = "desktop-firefox-beta"
    , name = "Mozilla Firefox Beta"
    , steps =
        [ { id = "step-1"
          , name = "Step 1"
          }
        , { id = "step-2"
          , name = "Step 2"
          }
        , { id = "step-2"
          , name = "Step 2"
          }
        ]
    }


stepsData : StepsData
stepsData =
    Dict.fromList
        [ ("step-1", { status = StepRunning })
        ]



--
-- Types
--

type Msg
    = None


type alias PipelineId =
    String


type alias Pipeline =
    { id : PipelineId
    , name : String
    , steps : List Step
    }


type alias Step =
    { id : StepId
    , name : String
    }


type alias StepId =
    String


type alias StepsData =
    Dict.Dict StepId StepData


type alias StepData =
    { status : StepStatus
    }


type StepStatus
    = StepFailed
    | StepPaused
    | StepWaiting
    | StepRunning
    | StepDone
    | StepPending


initialStepStatus =
    StepPending


--
-- View
--


view model =
    let
        stepsByStatus = groupStepsByStatus pipeline.steps stepsData 
    in
        div [ class "container" ]
            (List.append
                [ h1 [] [ text pipeline.name ]
                , viewProgress pipeline.steps stepsByStatus
                , div [ class "text-center" ]
                      [ a [ href "#" ] [ text "Show in graph" ]
                      ]
                ]
                (viewSteps stepsByStatus)
            )


viewProgressItem : List Step -> (String, List Step) -> Html Msg
viewProgressItem allSteps (stepStatus, steps) =
    let

        stepsCount =
            List.length allSteps
                |> toString
                |> (\x -> "(" ++ x ++ ")")

        percentage =
            (List.length steps |> toFloat) * 100.0 / (List.length allSteps |> toFloat)

        title =
            String.left 1 stepStatus
                |> String.toUpper
    in
        div
            [ class ("progress-bar progress-bar-" ++ stepStatus)
            , style [ ("width", (toString percentage) ++ "%")
                    , ("height", "2em")
                    , ("line-height", "2em")
                    ]
            , attribute "role" "progressbar"
            , attribute "aria-valuenow" "15"
            , attribute "aria-valuemin" "0"
            , attribute "aria-valuemax" "100"
            ]
            [ text (title ++ " " ++ stepsCount) ]


viewProgressLegend =
    ul
        [ class "progress-legend" ] 
        (List.map
           (\x -> li [ class ("progress-legend-" ++ x) ] [ text x ])
           [ "failed"
           , "paused"
           , "waiting"
           , "running"
           , "done"
           , "pending"
           ]
        )



viewProgress allSteps stepsByStatus =
    div
        []
        [ viewProgressLegend
        , div
            [ class "progress" ]
            (Dict.toList stepsByStatus |> List.map (viewProgressItem allSteps))
        ]




viewSteps : Dict.Dict String (List Step) -> List (Html Msg)
viewSteps stepsByStatus =
    Dict.toList stepsByStatus
        |> List.map viewStepsByStatus
        |> List.concat


viewStepsByStatus : (String, List Step) -> List (Html Msg)
viewStepsByStatus (stepStatus, steps) =
    [ h2 [] [ text stepStatus ]
    , div [ class "list-group" ] (List.map viewStep steps)
    ]


viewStep step =
    div
        [ class "list-group-item" ]
        [ a
            [ href "#" ]
            [ text step.name ]
        ]
    


--
-- Utils
--


groupStepsByStatus : (List Step) -> StepsData -> Dict.Dict String (List Step)
groupStepsByStatus steps stepsData =
    List.foldl groupStepByStatus Dict.empty steps


groupStepByStatus step = 
    Dict.update
        (getStepStatusAsString step stepsData)
        (\x -> case x of
                Nothing -> Just [step]
                Just xs -> Just (xs ++ [step])
        )


getStepStatusAsString : Step -> StepsData -> String
getStepStatusAsString step stepsData =
    let
      stepData = Dict.get step.id stepsData
    in
       case stepData of
           Just x -> status2string x.status
           Nothing -> status2string initialStepStatus


status2string status =
    case status of
       StepFailed ->
           "failed"
       StepPaused ->
           "paused"
       StepWaiting ->
           "waiting"
       StepRunning ->
           "running"
       StepDone ->
           "done"
       StepPending ->
           "pending"


string2status text =
    case text of
       "failed" ->
           StepFailed
       "paused" ->
           StepPaused
       "waiting" ->
           StepWaiting
       "running" ->
           StepRunning
       "done" ->
           StepDone
       "pending" ->
           StepPending
       _ ->
           initialStepStatus
