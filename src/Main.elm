module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
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


firstAgent : Agent
firstAgent =
    Agent (AgentId 1) []


additionalAgents : Int -> List Agent
additionalAgents n =
    List.range 1 n
        |> List.map (\id -> Agent (AgentId id) [])


agentPool : AgentPool
agentPool =
    AgentPool
        (List.NonEmpty.fromCons
            (Agent (AgentId 1) [])
            [ Agent (AgentId 2) []
            ]
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


results : AgentPool
results =
    processQueue buildQueue agentPool


main : Html a
main =
    let
        (AgentPool pool) =
            results

        agents =
            List.NonEmpty.toList pool
    in
    Html.section []
        [ Html.ul [] <|
            List.map (\agent -> Html.li [] [ Html.text <| String.fromInt <| durationToSeconds <| agentTotalTime agent ])
                agents
        ]
