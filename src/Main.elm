module Main exposing (main)

import Browser
import Chart
import Chart.Attributes
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import List.Extra
import List.NonEmpty exposing (NonEmpty)


type AgentPool
    = AgentPool (NonEmpty Agent)


buildTimesByVerify : AgentPool -> List ProcessedBuild
buildTimesByVerify (AgentPool pool) =
    pool
        |> List.NonEmpty.map agentBuildStats
        |> List.NonEmpty.toList
        |> List.concat
        |> List.Extra.gatherEqualsBy .verifyId
        |> List.foldr
            (\( _, builds ) acc ->
                List.Extra.maximumBy
                    (durationToSeconds << processedBuildTotalTime)
                    builds
                    :: acc
            )
            []
        |> List.filterMap identity


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
    buildTotalDuration agent.builds


agentProcessBuild : Build -> Agent -> Agent
agentProcessBuild build agent =
    { agent | builds = agent.builds ++ [ build ] }


agentBuildStats : Agent -> List ProcessedBuild
agentBuildStats agent =
    List.foldl
        (\build stats ->
            queueBuild
                (List.head stats
                    |> Maybe.map processedBuildTotalTime
                    |> Maybe.withDefault (Duration 0)
                )
                build
                :: stats
        )
        []
        agent.builds


type alias Build =
    { verifyId : VerifyId
    , duration : Duration
    }


buildTotalDuration : List Build -> Duration
buildTotalDuration builds =
    builds
        |> List.map .duration
        |> List.foldr durationAdd (Duration 0)


type alias ProcessedBuild =
    { verifyId : VerifyId
    , queueTime : Duration
    , buildTime : Duration
    }


queueBuild : Duration -> Build -> ProcessedBuild
queueBuild queueTime build =
    { verifyId = build.verifyId
    , buildTime = build.duration
    , queueTime = queueTime
    }


processedBuildTotalTime : ProcessedBuild -> Duration
processedBuildTotalTime { queueTime, buildTime } =
    durationAdd queueTime buildTime


type VerifyId
    = VerifyId Int


rawVerifyId : VerifyId -> Int
rawVerifyId (VerifyId id) =
    id


type Duration
    = Duration Int


durationAdd : Duration -> Duration -> Duration
durationAdd (Duration d1) (Duration d2) =
    Duration (d1 + d2)


durationToSeconds : Duration -> Int
durationToSeconds (Duration d) =
    d


durationToMinutes : Duration -> Int
durationToMinutes (Duration d) =
    d // 60


type Queue
    = Queue (List Build)



-- PROGRAM


type alias Model =
    { additionalAgents : Int
    , verifies : Int
    , buildType : BuildType
    }


initialModel : Model
initialModel =
    { additionalAgents = 1
    , verifies = 2
    , buildType = Current
    }


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


verify : VerifyId -> BuildType -> List Build
verify id buildType =
    case buildType of
        Current ->
            [ Build id (Duration 630) -- Background Job
            , Build id (Duration 788) -- CCD
            , Build id (Duration 445) -- E2E
            , Build id (Duration 1500) -- Rails 3
            , Build id (Duration 1500) -- Rails 4
            , Build id (Duration 330) -- Security
            ]

        Optimized ->
            [ Build id (Duration 630) -- Background Job
            , Build id (Duration 788) -- CCD
            , Build id (Duration 445) -- E2E
            , Build id (Duration 900) -- Rails 3 (optimized)
            , Build id (Duration 900) -- Rails 4 (optimized)
            , Build id (Duration 330) -- Security
            ]

        TwoAgents ->
            [ Build id (Duration 630) -- Background Job
            , Build id (Duration 788) -- CCD
            , Build id (Duration 445) -- E2E
            , Build id (Duration 720) -- Rails 3 slice 1
            , Build id (Duration 720) -- Rails 3 slice 2
            , Build id (Duration 720) -- Rails 4 slice 1
            , Build id (Duration 720) -- Rails 4 slice 2
            , Build id (Duration 330) -- Security
            ]


buildQueue : Int -> BuildType -> Queue
buildQueue verifies buildType =
    List.range 1 verifies
        |> List.map (\n -> verify (VerifyId n) buildType)
        |> List.concat
        |> Queue


results : Model -> AgentPool
results { verifies, additionalAgents, buildType } =
    processQueue (buildQueue verifies buildType) (agentPool additionalAgents)


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
    | VerifyCountChanged Int
    | BuildTypeChosen BuildType


type BuildType
    = Current
    | Optimized
    | TwoAgents


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgentCountChanged newAdditionalAgents ->
            { model | additionalAgents = newAdditionalAgents }

        VerifyCountChanged newVerifies ->
            { model | verifies = newVerifies }

        BuildTypeChosen newBuild ->
            { model | buildType = newBuild }


view : Model -> Html Msg
view model =
    let
        finalPool =
            results model
    in
    Html.section []
        [ controls model
        , verifyData finalPool
        , verifyChart finalPool
        , agentChart finalPool
        , dataList finalPool
        ]


controls : Model -> Html Msg
controls { verifies, additionalAgents, buildType } =
    Html.section []
        [ fieldset "Agents"
            [ Html.span [] [ Html.text <| String.fromInt (additionalAgents + 1) ]
            , range AgentCountChanged additionalAgents
            ]
        , fieldset "Verifies"
            [ Html.span [] [ Html.text <| String.fromInt verifies ]
            , range VerifyCountChanged verifies
            ]
        , fieldset "Build type"
            [ buildTypeRadio Current "Current (1 agent, 25 mins)" buildType
            , buildTypeRadio Optimized "Optimized (1 agent, 15 mins)" buildType
            , buildTypeRadio TwoAgents "2 agents (12 mins each)" buildType
            ]
        ]


buildTypeRadio : BuildType -> String -> BuildType -> Html Msg
buildTypeRadio buildType labelText selectedBuildType =
    radio (BuildTypeChosen buildType)
        labelText
        (buildType == selectedBuildType)


radio : msg -> String -> Bool -> Html msg
radio msg labelText isSelected =
    Html.div []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "radio"
                , Html.Events.onCheck (\_ -> msg)
                , Html.Attributes.checked isSelected
                ]
                []
            , Html.text labelText
            ]
        ]


fieldset : String -> List (Html msg) -> Html msg
fieldset legend children =
    Html.fieldset []
        (Html.legend [] [ Html.text legend ] :: children)


range : (Int -> msg) -> Int -> Html msg
range toMsg value =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min "0"
        , Html.Attributes.value (String.fromInt value)
        , onIntInput toMsg
        ]
        []


onIntInput : (Int -> msg) -> Html.Attribute msg
onIntInput toMsg =
    Html.Events.on "input" (Decode.map toMsg intTarget)


intTarget : Decoder Int
intTarget =
    Html.Events.targetValue
        |> Decode.andThen (Json.Decode.Extra.fromMaybe "not an int" << String.toInt)


dataList : AgentPool -> Html a
dataList (AgentPool pool) =
    let
        agents =
            List.NonEmpty.toList pool
    in
    Html.section []
        [ Html.h2 [] [ Html.text "Work done by agents - raw data" ]
        , Html.ul [] <|
            List.map (\agent -> Html.li [] [ Html.text <| String.fromInt <| durationToSeconds <| agentTotalTime agent ])
                agents
        ]


verifyData : AgentPool -> Html a
verifyData pool =
    Html.section []
        [ Html.h2 [] [ Html.text "Longest build for each verify" ]
        , Html.ul [] <|
            List.map (\build -> Html.li [] [ Html.text <| verifyDataString build ])
                (List.sortBy (durationToSeconds << processedBuildTotalTime) <| buildTimesByVerify pool)
        ]


verifyDataString : ProcessedBuild -> String
verifyDataString build =
    "Verify "
        ++ (String.fromInt <| rawVerifyId build.verifyId)
        ++ " (queue time: "
        ++ (String.fromInt <| durationToMinutes build.queueTime)
        ++ " build time: "
        ++ (String.fromInt <| durationToMinutes build.buildTime)
        ++ ") - total "
        ++ (String.fromInt <| durationToMinutes <| processedBuildTotalTime build)



-- CHART


agentChart : AgentPool -> Html a
agentChart (AgentPool pool) =
    Html.section
        [ Html.Attributes.style "width" "300px"
        , Html.Attributes.style "height" "400px"
        , Html.Attributes.style "padding-left" "50px"
        ]
        [ Html.h2 [] [ Html.text "Work done by agents" ]
        , Chart.chart []
            [ Chart.bars []
                [ Chart.bar (toFloat << durationToMinutes << agentTotalTime) [] ]
                (List.NonEmpty.toList pool)
            , Chart.xTicks []
            , Chart.xAxis []
            , Chart.yTicks []
            , Chart.yAxis []
            , Chart.yLabels []
            ]
        ]


verifyChart : AgentPool -> Html a
verifyChart pool =
    Html.section
        [ Html.Attributes.style "width" "300px"
        , Html.Attributes.style "height" "400px"
        , Html.Attributes.style "padding-left" "50px"
        ]
        [ Html.h2 [] [ Html.text "Verify times" ]
        , Chart.chart []
            [ Chart.bars []
                [ Chart.stacked
                    [ Chart.bar (toFloat << durationToMinutes << .buildTime) []
                    , Chart.bar (toFloat << durationToMinutes << .queueTime) []
                    ]
                ]
                (List.sortBy (durationToSeconds << processedBuildTotalTime) <| buildTimesByVerify pool)
            , Chart.xTicks []
            , Chart.xAxis []
            , Chart.yTicks []
            , Chart.yAxis []
            , Chart.yLabels []
            , Chart.xLabels []
            ]
        ]
