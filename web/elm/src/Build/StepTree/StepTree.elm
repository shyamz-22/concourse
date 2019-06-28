module Build.StepTree.StepTree exposing
    ( extendHighlight
    , finished
    , init
    , setHighlight
    , switchTab
    , toggleStep
    , updateTooltip
    , view
    )

import Ansi.Log
import Application.Models exposing (Session)
import Array exposing (Array)
import Build.Models exposing (StepHeaderType(..))
import Build.StepTree.Models
    exposing
        ( HookedStep
        , MetadataField
        , Step
        , StepName
        , StepState(..)
        , StepTree(..)
        , StepTreeModel
        , TabFocus(..)
        , Version
        , finishStep
        , focusRetry
        , updateAt
        , wrapHook
        , wrapMultiStep
        , wrapStep
        )
import Build.Styles as Styles
import Concourse
import DateFormat
import Dict exposing (Dict)
import Duration
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, href, style, target)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Keyed
import Message.Effects exposing (Effect(..))
import Message.Message exposing (BuildOutputDomID(..), DomID(..), Message(..))
import Routes exposing (Highlight(..), StepID, showHighlight)
import StrictEvents
import Time
import Url exposing (fromString)
import Views.DictView as DictView
import Views.Icon as Icon
import Views.Spinner as Spinner


init :
    Highlight
    -> Concourse.BuildResources
    -> Concourse.BuildPlan
    -> StepTreeModel
init hl resources buildPlan =
    { tree = planToTree buildPlan
    , foci = Dict.empty
    , steps = insertTree resources buildPlan Dict.empty
    , highlight = HighlightNothing
    , tooltip = Nothing
    }


insertTree :
    Concourse.BuildResources
    -> Concourse.BuildPlan
    -> Dict StepID Step
    -> Dict StepID Step
insertTree resources buildPlan =
    let
        emptyStep =
            { id = buildPlan.id
            , name = ""
            , state = StepStatePending
            , log = Ansi.Log.init Ansi.Log.Cooked
            , error = Nothing
            , expanded = False
            , version = Nothing
            , metadata = []
            , firstOccurrence = False
            , timestamps = Dict.empty
            , initialize = Nothing
            , start = Nothing
            , finish = Nothing
            }
    in
    case buildPlan.step of
        Concourse.BuildStepTask name ->
            Dict.insert
                buildPlan.id
                { emptyStep | name = name }

        Concourse.BuildStepGet name version ->
            Dict.insert
                buildPlan.id
                { emptyStep | name = name, version = version }

        Concourse.BuildStepPut name ->
            Dict.insert
                buildPlan.id
                { emptyStep | name = name }

        Concourse.BuildStepAggregate plans ->
            Array.foldl (>>) identity (Array.map (insertTree resources) plans)

        Concourse.BuildStepInParallel plans ->
            Array.foldl (>>) identity (Array.map (insertTree resources) plans)

        Concourse.BuildStepDo plans ->
            Array.foldl (>>) identity (Array.map (insertTree resources) plans)

        Concourse.BuildStepRetry plans ->
            Array.foldl (>>) identity (Array.map (insertTree resources) plans)

        Concourse.BuildStepOnSuccess { hook, step } ->
            insertTree resources hook >> insertTree resources step

        Concourse.BuildStepOnFailure { hook, step } ->
            insertTree resources hook >> insertTree resources step

        Concourse.BuildStepOnAbort { hook, step } ->
            insertTree resources hook >> insertTree resources step

        Concourse.BuildStepOnError { hook, step } ->
            insertTree resources hook >> insertTree resources step

        Concourse.BuildStepEnsure { hook, step } ->
            insertTree resources hook >> insertTree resources step

        Concourse.BuildStepTry tryPlan ->
            insertTree resources tryPlan

        Concourse.BuildStepTimeout timeoutPlan ->
            insertTree resources timeoutPlan

        Concourse.BuildStepArtifactInput name ->
            Dict.insert
                buildPlan.id
                { emptyStep | name = name }

        Concourse.BuildStepArtifactOutput name ->
            Dict.insert
                buildPlan.id
                { emptyStep | name = name }


planToTree : Concourse.BuildPlan -> StepTree
planToTree plan =
    case plan.step of
        Concourse.BuildStepTask _ ->
            Task plan.id

        Concourse.BuildStepGet _ _ ->
            Get plan.id

        Concourse.BuildStepPut _ ->
            Put plan.id

        Concourse.BuildStepAggregate plans ->
            Aggregate (Array.map planToTree plans)

        Concourse.BuildStepInParallel plans ->
            InParallel (Array.map planToTree plans)

        Concourse.BuildStepDo plans ->
            Do (Array.map planToTree plans)

        Concourse.BuildStepRetry plans ->
            Retry plan.id (Array.length plans) Auto (Array.map planToTree plans)

        Concourse.BuildStepOnSuccess { hook, step } ->
            OnSuccess { hook = planToTree hook, step = planToTree step }

        Concourse.BuildStepOnFailure { hook, step } ->
            OnFailure { hook = planToTree hook, step = planToTree step }

        Concourse.BuildStepOnAbort { hook, step } ->
            OnAbort { hook = planToTree hook, step = planToTree step }

        Concourse.BuildStepOnError { hook, step } ->
            OnError { hook = planToTree hook, step = planToTree step }

        Concourse.BuildStepEnsure { hook, step } ->
            Ensure { hook = planToTree hook, step = planToTree step }

        Concourse.BuildStepTry tryPlan ->
            Try (planToTree tryPlan)

        Concourse.BuildStepTimeout timeoutPlan ->
            Timeout (planToTree timeoutPlan)

        Concourse.BuildStepArtifactInput _ ->
            ArtifactInput plan.id

        Concourse.BuildStepArtifactOutput _ ->
            ArtifactOutput plan.id



-- case buildPlan.step of
--     Concourse.BuildStepTask name ->
--         initBottom hl (.id >> Task) buildPlan.id name
--     Concourse.BuildStepArtifactInput name ->
--         initBottom hl
--             (.id >> ArtifactInput)
--             buildPlan.id
--             name
--             |> (\m ->
--                     { m
--                         | steps =
--                             Dict.update buildPlan.id
--                                 (Maybe.map (\s -> { s | state = StepStateSucceeded }))
--                                 m.steps
--                     }
--                )
--     Concourse.BuildStepGet name version ->
--         initBottom hl
--             (.id >> Get)
--             buildPlan.id
--             name
--             |> (\m ->
--                     { m
--                         | steps =
--                             Dict.update buildPlan.id
--                                 (Maybe.map (setupGetStep resources name version))
--                                 m.steps
--                     }
--                )
--     Concourse.BuildStepArtifactOutput name ->
--         initBottom hl ArtifactOutput buildPlan.id name
--     Concourse.BuildStepPut name ->
--         initBottom hl Put buildPlan.id name
--     Concourse.BuildStepAggregate plans ->
--         initMultiStep hl resources buildPlan.id Aggregate plans
--     Concourse.BuildStepInParallel plans ->
--         initMultiStep hl resources buildPlan.id InParallel plans
--     Concourse.BuildStepDo plans ->
--         initMultiStep hl resources buildPlan.id Do plans
--     Concourse.BuildStepRetry plans ->
--         initMultiStep hl resources buildPlan.id (Retry buildPlan.id 1 Auto) plans
--     Concourse.BuildStepOnSuccess hookedPlan ->
--         initHookedStep hl resources OnSuccess hookedPlan
--     Concourse.BuildStepOnFailure hookedPlan ->
--         initHookedStep hl resources OnFailure hookedPlan
--     Concourse.BuildStepOnAbort hookedPlan ->
--         initHookedStep hl resources OnAbort hookedPlan
--     Concourse.BuildStepOnError hookedPlan ->
--         initHookedStep hl resources OnError hookedPlan
--     Concourse.BuildStepEnsure hookedPlan ->
--         initHookedStep hl resources Ensure hookedPlan
--     Concourse.BuildStepTry plan ->
--         initWrappedStep hl resources Try plan
--     Concourse.BuildStepTimeout plan ->
--         initWrappedStep hl resources Timeout plan
-- initMultiStep :
--     Highlight
--     -> Concourse.BuildResources
--     -> String
--     -> (Array StepTree -> StepTree)
--     -> Array Concourse.BuildPlan
--     -> StepTreeModel
-- initMultiStep hl resources planId constructor plans =
--     let
--         inited =
--             Array.map (init hl resources) plans
--
--         trees =
--             Array.map .tree inited
--
--         selfFoci =
--             Dict.singleton planId identity
--
--         foci =
--             inited
--                 |> Array.map .foci
--                 |> Array.indexedMap wrapMultiStep
--                 |> Array.foldr Dict.union selfFoci
--     in
--     StepTreeModel (constructor trees) foci hl Nothing


initBottom :
    Highlight
    -> (Step -> StepTree)
    -> StepID
    -> StepName
    -> StepTreeModel
initBottom hl create id name =
    let
        step =
            { id = id
            , name = name
            , state = StepStatePending
            , log = Ansi.Log.init Ansi.Log.Cooked
            , error = Nothing
            , expanded =
                case hl of
                    HighlightNothing ->
                        False

                    HighlightLine stepID _ ->
                        if id == stepID then
                            True

                        else
                            False

                    HighlightRange stepID _ _ ->
                        if id == stepID then
                            True

                        else
                            False
            , version = Nothing
            , metadata = []
            , firstOccurrence = False
            , timestamps = Dict.empty
            , initialize = Nothing
            , start = Nothing
            , finish = Nothing
            }
    in
    { tree = create step
    , foci = Dict.singleton id identity
    , steps = Dict.singleton id step
    , highlight = hl
    , tooltip = Nothing
    }



-- initWrappedStep :
--     Highlight
--     -> Concourse.BuildResources
--     -> (StepTree -> StepTree)
--     -> Concourse.BuildPlan
--     -> StepTreeModel
-- initWrappedStep hl resources create plan =
--     let
--         { tree, foci } =
--             init hl resources plan
--     in
--     { tree = create tree
--     , foci = Dict.map (always wrapStep) foci
--     , highlight = hl
--     , tooltip = Nothing
--     }
-- initHookedStep :
--     Highlight
--     -> Concourse.BuildResources
--     -> (HookedStep -> StepTree)
--     -> Concourse.HookedPlan
--     -> StepTreeModel
-- initHookedStep hl resources create hookedPlan =
--     let
--         stepModel =
--             init hl resources hookedPlan.step
--
--         hookModel =
--             init hl resources hookedPlan.hook
--     in
--     { tree = create { step = stepModel.tree, hook = hookModel.tree }
--     , foci =
--         Dict.union
--             (Dict.map (always wrapStep) stepModel.foci)
--             (Dict.map (always wrapHook) hookModel.foci)
--     , highlight = hl
--     , tooltip = Nothing
--     }


treeIsActive : StepTreeModel -> StepTree -> Bool
treeIsActive root stepTree =
    case stepTree of
        Aggregate trees ->
            List.any (treeIsActive root) (Array.toList trees)

        InParallel trees ->
            List.any (treeIsActive root) (Array.toList trees)

        Do trees ->
            List.any (treeIsActive root) (Array.toList trees)

        OnSuccess { step } ->
            treeIsActive root step

        OnFailure { step } ->
            treeIsActive root step

        OnAbort { step } ->
            treeIsActive root step

        OnError { step } ->
            treeIsActive root step

        Ensure { step } ->
            treeIsActive root step

        Try tree ->
            treeIsActive root tree

        Timeout tree ->
            treeIsActive root tree

        Retry _ _ _ trees ->
            List.any (treeIsActive root) (Array.toList trees)

        Task stepID ->
            stepIsActive root.steps stepID

        ArtifactInput _ ->
            False

        Get stepID ->
            stepIsActive root.steps stepID

        ArtifactOutput stepID ->
            stepIsActive root.steps stepID

        Put stepID ->
            stepIsActive root.steps stepID


stepIsActive : Dict StepID Step -> StepID -> Bool
stepIsActive steps id =
    Dict.get id steps
        |> Maybe.map (.state >> isActive)
        |> Maybe.withDefault False


setupGetStep : Concourse.BuildResources -> StepName -> Maybe Version -> Step -> Step
setupGetStep resources name version step =
    { step
        | version = version
        , firstOccurrence = isFirstOccurrence resources.inputs name
    }


isFirstOccurrence : List Concourse.BuildResourcesInput -> StepName -> Bool
isFirstOccurrence resources step =
    case resources of
        [] ->
            False

        { name, firstOccurrence } :: rest ->
            if name == step then
                firstOccurrence

            else
                isFirstOccurrence rest step


finished : StepTreeModel -> StepTreeModel
finished root =
    { root | steps = Dict.map (always finishStep) root.steps }


toggleStep : StepID -> StepTreeModel -> ( StepTreeModel, List Effect )
toggleStep id root =
    ( { root
        | steps =
            Dict.update
                id
                (Maybe.map (\step -> { step | expanded = not step.expanded }))
                root.steps
      }
    , []
    )


switchTab : StepID -> Int -> StepTreeModel -> ( StepTreeModel, List Effect )
switchTab id tab root =
    ( updateAt id (focusRetry tab) root, [] )


setHighlight : StepID -> Int -> StepTreeModel -> ( StepTreeModel, List Effect )
setHighlight id line root =
    let
        hl =
            HighlightLine id line
    in
    ( { root | highlight = hl }, [ ModifyUrl (showHighlight hl) ] )


extendHighlight : StepID -> Int -> StepTreeModel -> ( StepTreeModel, List Effect )
extendHighlight id line root =
    let
        hl =
            case root.highlight of
                HighlightNothing ->
                    HighlightLine id line

                HighlightLine currentID currentLine ->
                    if currentID == id then
                        if currentLine < line then
                            HighlightRange id currentLine line

                        else
                            HighlightRange id line currentLine

                    else
                        HighlightLine id line

                HighlightRange currentID currentLine _ ->
                    if currentID == id then
                        if currentLine < line then
                            HighlightRange id currentLine line

                        else
                            HighlightRange id line currentLine

                    else
                        HighlightLine id line
    in
    ( { root | highlight = hl }, [ ModifyUrl (showHighlight hl) ] )


updateTooltip :
    { a | hovered : Maybe BuildOutputDomID }
    -> { b | hoveredCounter : Int }
    -> StepTreeModel
    -> ( StepTreeModel, List Effect )
updateTooltip { hovered } { hoveredCounter } model =
    let
        newTooltip =
            case hovered of
                Just (FirstOccurrenceIcon _) ->
                    if hoveredCounter > 0 then
                        hovered

                    else
                        Nothing

                Just (StepState _) ->
                    if hoveredCounter > 0 then
                        hovered

                    else
                        Nothing

                _ ->
                    Nothing
    in
    ( { model | tooltip = newTooltip }, [] )


view :
    { timeZone : Time.Zone, hovered : Maybe BuildOutputDomID }
    -> StepTreeModel
    -> Html Message
view session model =
    viewTree session model model.tree


viewTree :
    { timeZone : Time.Zone, hovered : Maybe BuildOutputDomID }
    -> StepTreeModel
    -> StepTree
    -> Html Message
viewTree session model tree =
    case tree of
        Task stepID ->
            viewStep model session stepID StepHeaderTask

        ArtifactInput stepID ->
            viewStep model session stepID (StepHeaderGet False)

        Get stepID ->
            let
                isYellow =
                    Dict.get stepID model.steps
                        |> Maybe.map .firstOccurrence
                        |> Maybe.withDefault False
            in
            viewStep model session stepID (StepHeaderGet isYellow)

        ArtifactOutput stepID ->
            viewStep model session stepID StepHeaderPut

        Put stepID ->
            viewStep model session stepID StepHeaderPut

        Try stepTree ->
            viewTree session model stepTree

        Retry id tab _ steps ->
            Html.div [ class "retry" ]
                [ Html.ul
                    (class "retry-tabs" :: Styles.retryTabList)
                    (Array.toList <| Array.indexedMap (viewTab session model id tab) steps)
                , case Array.get (tab - 1) steps of
                    Just step ->
                        viewTree session model step

                    Nothing ->
                        -- impossible (bogus tab selected)
                        Html.text ""
                ]

        Timeout stepTree ->
            viewTree session model stepTree

        Aggregate steps ->
            Html.div [ class "aggregate" ]
                (Array.toList <| Array.map (viewSeq session model) steps)

        InParallel steps ->
            Html.div [ class "parallel" ]
                (Array.toList <| Array.map (viewSeq session model) steps)

        Do steps ->
            Html.div [ class "do" ]
                (Array.toList <| Array.map (viewSeq session model) steps)

        OnSuccess { step, hook } ->
            viewHooked session "success" model step hook

        OnFailure { step, hook } ->
            viewHooked session "failure" model step hook

        OnAbort { step, hook } ->
            viewHooked session "abort" model step hook

        OnError { step, hook } ->
            viewHooked session "error" model step hook

        Ensure { step, hook } ->
            viewHooked session "ensure" model step hook


viewTab :
    { timeZone : Time.Zone, hovered : Maybe BuildOutputDomID }
    -> StepTreeModel
    -> StepID
    -> Int
    -> Int
    -> StepTree
    -> Html Message
viewTab { hovered } root id currentTab idx step =
    let
        tab =
            idx + 1
    in
    Html.li
        ([ classList
            [ ( "current", currentTab == tab )
            , ( "inactive", not <| treeIsActive root step )
            ]
         , onMouseEnter <| Hover <| Just <| BuildOutput <| StepTab id tab
         , onMouseLeave <| Hover Nothing
         , onClick <| Click <| BuildOutput <| StepTab id tab
         ]
            ++ Styles.retryTab
                { isHovered = hovered == (Just <| StepTab id tab)
                , isCurrent = currentTab == tab
                , isStarted = treeIsActive root step
                }
        )
        [ Html.text (String.fromInt tab) ]


viewSeq :
    { timeZone : Time.Zone
    , hovered : Maybe BuildOutputDomID
    }
    -> StepTreeModel
    -> StepTree
    -> Html Message
viewSeq session model tree =
    Html.div [ class "seq" ] [ viewTree session model tree ]


viewHooked :
    { timeZone : Time.Zone
    , hovered : Maybe BuildOutputDomID
    }
    -> String
    -> StepTreeModel
    -> StepTree
    -> StepTree
    -> Html Message
viewHooked session name model step hook =
    Html.div [ class "hooked" ]
        [ Html.div [ class "step" ] [ viewTree session model step ]
        , Html.div [ class "children" ]
            [ Html.div [ class ("hook hook-" ++ name) ] [ viewTree session model hook ]
            ]
        ]


isActive : StepState -> Bool
isActive state =
    state /= StepStatePending && state /= StepStateCancelled


viewStep :
    StepTreeModel
    ->
        { timeZone : Time.Zone
        , hovered : Maybe BuildOutputDomID
        }
    -> StepID
    -> StepHeaderType
    -> Html Message
viewStep model session stepID headerType =
    case Dict.get stepID model.steps of
        Nothing ->
            Html.text ""

        Just { id, name, log, state, error, expanded, version, metadata, timestamps, initialize, start, finish } ->
            Html.div
                [ classList
                    [ ( "build-step", True )
                    , ( "inactive", not <| isActive state )
                    ]
                , attribute "data-step-name" name
                ]
                [ Html.div
                    ([ class "header"
                     , onClick <| Click <| BuildOutput <| StepHeader id
                     ]
                        ++ Styles.stepHeader
                    )
                    [ Html.div
                        [ style "display" "flex" ]
                        [ viewStepHeaderIcon headerType (model.tooltip == Just (FirstOccurrenceIcon id)) id
                        , Html.h3 [] [ Html.text name ]
                        ]
                    , Html.div
                        [ style "display" "flex" ]
                        [ viewVersion version
                        , viewStepState state id (viewDurationTooltip initialize start finish (model.tooltip == Just (StepState id)))
                        ]
                    ]
                , if expanded then
                    Html.div
                        [ class "step-body"
                        , class "clearfix"
                        ]
                        [ viewMetadata metadata
                        , Html.Keyed.node "pre" [ class "timestamped-logs" ] <|
                            viewLogs log timestamps model.highlight session.timeZone id
                        , case error of
                            Nothing ->
                                Html.span [] []

                            Just msg ->
                                Html.span [ class "error" ] [ Html.pre [] [ Html.text msg ] ]
                        ]

                  else
                    Html.text ""
                ]


viewLogs :
    Ansi.Log.Model
    -> Dict Int Time.Posix
    -> Highlight
    -> Time.Zone
    -> String
    -> List ( String, Html Message )
viewLogs { lines } timestamps hl timeZone id =
    Array.toList <|
        Array.indexedMap
            (\idx line ->
                ( String.fromInt idx
                , viewTimestampedLine
                    { timestamps = timestamps
                    , highlight = hl
                    , id = id
                    , lineNo = idx + 1
                    , line = line
                    , timeZone = timeZone
                    }
                )
            )
            lines


viewTimestampedLine :
    { timestamps : Dict Int Time.Posix
    , highlight : Highlight
    , id : StepID
    , lineNo : Int
    , line : Ansi.Log.Line
    , timeZone : Time.Zone
    }
    -> Html Message
viewTimestampedLine { timestamps, highlight, id, lineNo, line, timeZone } =
    let
        highlighted =
            case highlight of
                HighlightNothing ->
                    False

                HighlightLine hlId hlLine ->
                    hlId == id && hlLine == lineNo

                HighlightRange hlId hlLine1 hlLine2 ->
                    hlId == id && lineNo >= hlLine1 && lineNo <= hlLine2

        ts =
            Dict.get lineNo timestamps
    in
    Html.tr
        [ classList
            [ ( "timestamped-line", True )
            , ( "highlighted-line", highlighted )
            ]
        , Html.Attributes.id <| id ++ ":" ++ String.fromInt lineNo
        ]
        [ viewTimestamp
            { id = id
            , line = lineNo
            , date = ts
            , timeZone = timeZone
            }
        , viewLine line
        ]


viewLine : Ansi.Log.Line -> Html Message
viewLine line =
    Html.td [ class "timestamped-content" ]
        [ Ansi.Log.viewLine line
        ]


viewTimestamp :
    { id : String
    , line : Int
    , date : Maybe Time.Posix
    , timeZone : Time.Zone
    }
    -> Html Message
viewTimestamp { id, line, date, timeZone } =
    Html.a
        [ href (showHighlight (HighlightLine id line))
        , StrictEvents.onLeftClickOrShiftLeftClick
            (SetHighlight id line)
            (ExtendHighlight id line)
        ]
        [ case date of
            Just d ->
                Html.td
                    [ class "timestamp" ]
                    [ Html.text <|
                        DateFormat.format
                            [ DateFormat.hourMilitaryFixed
                            , DateFormat.text ":"
                            , DateFormat.minuteFixed
                            , DateFormat.text ":"
                            , DateFormat.secondFixed
                            ]
                            timeZone
                            d
                    ]

            _ ->
                Html.td [ class "timestamp placeholder" ] []
        ]


viewVersion : Maybe Version -> Html Message
viewVersion version =
    Maybe.withDefault Dict.empty version
        |> Dict.map (always Html.text)
        |> DictView.view []


viewMetadata : List MetadataField -> Html Message
viewMetadata =
    List.map
        (\{ name, value } ->
            ( name
            , Html.pre []
                [ case fromString value of
                    Just _ ->
                        Html.a
                            [ href value
                            , target "_blank"
                            , style "text-decoration-line" "underline"
                            ]
                            [ Html.text value ]

                    Nothing ->
                        Html.text value
                ]
            )
        )
        >> Dict.fromList
        >> DictView.view []


viewStepState : StepState -> StepID -> List (Html Message) -> Html Message
viewStepState state id tooltip =
    let
        eventHandlers =
            [ onMouseLeave <| Hover Nothing
            , onMouseEnter <| Hover <| Just <| BuildOutput <| StepState id
            , style "position" "relative"
            ]
    in
    case state of
        StepStateRunning ->
            Spinner.spinner
                { sizePx = 14
                , margin = "7px"
                }

        StepStatePending ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-pending.svg"
                }
                (attribute "data-step-state" "pending"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip

        StepStateInterrupted ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-interrupted.svg"
                }
                (attribute "data-step-state" "interrupted"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip

        StepStateCancelled ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-cancelled.svg"
                }
                (attribute "data-step-state" "cancelled"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip

        StepStateSucceeded ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-success-check.svg"
                }
                (attribute "data-step-state" "succeeded"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip

        StepStateFailed ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-failure-times.svg"
                }
                (attribute "data-step-state" "failed"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip

        StepStateErrored ->
            Icon.iconWithTooltip
                { sizePx = 28
                , image = "ic-exclamation-triangle.svg"
                }
                (attribute "data-step-state" "errored"
                    :: Styles.stepStatusIcon
                    ++ eventHandlers
                )
                tooltip


viewStepHeaderIcon : StepHeaderType -> Bool -> StepID -> Html Message
viewStepHeaderIcon headerType tooltip id =
    let
        eventHandlers =
            if headerType == StepHeaderGet True then
                [ onMouseLeave <| Hover Nothing
                , onMouseEnter <| Hover <| Just <| BuildOutput <| FirstOccurrenceIcon id
                ]

            else
                []
    in
    Html.div
        (Styles.stepHeaderIcon headerType ++ eventHandlers)
        (if tooltip then
            [ Html.div
                Styles.firstOccurrenceTooltip
                [ Html.text "new version" ]
            , Html.div
                Styles.firstOccurrenceTooltipArrow
                []
            ]

         else
            []
        )


viewDurationTooltip : Maybe Time.Posix -> Maybe Time.Posix -> Maybe Time.Posix -> Bool -> List (Html Message)
viewDurationTooltip minit mstart mfinish tooltip =
    if tooltip then
        case ( minit, mstart, mfinish ) of
            ( Just initializedAt, Just startedAt, Just finishedAt ) ->
                let
                    initDuration =
                        Duration.between initializedAt startedAt

                    stepDuration =
                        Duration.between startedAt finishedAt
                in
                [ Html.div
                    [ style "position" "inherit"
                    , style "margin-left" "-500px"
                    ]
                    [ Html.div
                        Styles.durationTooltip
                        [ DictView.view []
                            (Dict.fromList
                                [ ( "initialization"
                                  , Html.text (Duration.format initDuration)
                                  )
                                , ( "step", Html.text (Duration.format stepDuration) )
                                ]
                            )
                        ]
                    ]
                , Html.div
                    Styles.durationTooltipArrow
                    []
                ]

            _ ->
                []

    else
        []
