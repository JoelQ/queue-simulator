module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import List.NonEmpty exposing (NonEmpty)


type AgentPool
    = AgentPool (NonEmpty Agent)


processQueue : Queue -> AgentPool -> AgentPool
processQueue (Queue queue) pool =
    List.foldl processBuild pool queue


processBuild : Build -> AgentPool -> AgentPool
processBuild build pool =
    pool
        |> nextAvailableAgent
        |> agentProcessBuild build
        |> replaceAgentIn pool


nextAvailableAgent : AgentPool -> Agent
nextAvailableAgent (AgentPool pool) =
    pool
        |> List.NonEmpty.sortBy (durationToSeconds << agentTotalTime)
        |> List.NonEmpty.head


replaceAgentIn : AgentPool -> Agent -> AgentPool
replaceAgentIn (AgentPool pool) newAgent =
    List.NonEmpty.map
        (\agent ->
            if agent.id == newAgent.id then
                newAgent

            else
                agent
        )
        pool
        |> AgentPool


type alias Agent =
    { id : AgentId
    , builds : List Build
    }


type AgentId
    = AgentId Int


agentTotalTime : Agent -> Duration
agentTotalTime agent =
    agent.builds
        |> List.map .duration
        |> List.foldr durationAdd (Duration 0)


agentProcessBuild : Build -> Agent -> Agent
agentProcessBuild build agent =
    { agent | builds = agent.builds ++ [ build ] }


type alias Build =
    { verifyId : VerifyId
    , duration : Duration
    }


type VerifyId
    = VerifyId Int


type Duration
    = Duration Int


durationAdd : Duration -> Duration -> Duration
durationAdd (Duration d1) (Duration d2) =
    Duration (d1 + d2)


durationToSeconds : Duration -> Int
durationToSeconds (Duration d) =
    d


type Queue
    = Queue (List Build)



-- PROGRAM


type alias Model =
    { additionalAgents : Int }


initialModel : Model
initialModel =
    { additionalAgents = 1 }


firstAgent : Agent
firstAgent =
    Agent (AgentId 1) []


buildAgents : Int -> List Agent
buildAgents n =
    List.range 1 n
        |> List.map (\id -> Agent (AgentId id) [])


agentPool : Int -> AgentPool
agentPool additionalAgents =
    AgentPool
        (List.NonEmpty.fromCons
            firstAgent
            (buildAgents additionalAgents)
        )


buildQueue : Queue
buildQueue =
    Queue
        [ Build (VerifyId 1) (Duration 5)
        , Build (VerifyId 1) (Duration 3)
        , Build (VerifyId 1) (Duration 7)
        , Build (VerifyId 2) (Duration 5)
        , Build (VerifyId 2) (Duration 3)
        , Build (VerifyId 2) (Duration 7)
        ]


results : Int -> AgentPool
results additionalAgents =
    processQueue buildQueue (agentPool additionalAgents)


main : Program Flags Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Flags =
    ()


type Msg
    = AgentCountChanged Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgentCountChanged newAdditionalAgents ->
            { model | additionalAgents = newAdditionalAgents }


view : Model -> Html Msg
view model =
    Html.section []
        [ controls model.additionalAgents
        , dataList model.additionalAgents
        ]


controls : Int -> Html Msg
controls additionalAgents =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Agents" ]
        , Html.span [] [ Html.text <| String.fromInt (additionalAgents + 1) ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.value (String.fromInt additionalAgents)
            , onIntInput AgentCountChanged
            ]
            []
        ]


onIntInput : (Int -> msg) -> Html.Attribute msg
onIntInput toMsg =
    Html.Events.on "input" (Decode.map toMsg intTarget)


intTarget : Decoder Int
intTarget =
    Html.Events.targetValue
        |> Decode.andThen (Json.Decode.Extra.fromMaybe "not an int" << String.toInt)


dataList : Int -> Html a
dataList additionalAgents =
    let
        (AgentPool pool) =
            results additionalAgents

        agents =
            List.NonEmpty.toList pool
    in
    Html.ul [] <|
        List.map (\agent -> Html.li [] [ Html.text <| String.fromInt <| durationToSeconds <| agentTotalTime agent ])
            agents
