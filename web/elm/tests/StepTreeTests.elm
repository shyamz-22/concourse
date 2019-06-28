module StepTreeTests exposing
    ( all
    , initAggregate
    , initAggregateNested
    , initEnsure
    , initGet
    , initInParallel
    , initInParallelNested
    , initOnFailure
    , initOnSuccess
    , initPut
    , initTask
    , initTimeout
    , initTry
    )

import Ansi.Log
import Array
import Build.StepTree.Models as Models
import Build.StepTree.StepTree as StepTree
import Concourse exposing (BuildStep(..), HookedPlan)
import Dict
import Expect exposing (..)
import Routes
import Test exposing (..)


all : Test
all =
    describe "StepTree"
        [ initTask
        , initGet
        , initPut
        , initAggregate
        , initAggregateNested
        , initInParallel
        , initInParallelNested
        , initOnSuccess
        , initOnFailure
        , initEnsure
        , initTry
        , initTimeout
        ]


someStep : Routes.StepID -> Models.StepName -> Models.StepState -> Models.Step
someStep =
    someVersionedStep Nothing


someVersionedStep : Maybe Models.Version -> Routes.StepID -> Models.StepName -> Models.StepState -> Models.Step
someVersionedStep version id name state =
    { id = id
    , name = name
    , state = state
    , log = cookedLog
    , error = Nothing
    , expanded = False
    , version = version
    , metadata = []
    , firstOccurrence = False
    , timestamps = Dict.empty
    , initialize = Nothing
    , start = Nothing
    , finish = Nothing
    }


emptyResources : Concourse.BuildResources
emptyResources =
    { inputs = [], outputs = [] }


initTask : Test
initTask =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "some-id"
                , step = BuildStepTask "some-name"
                }
    in
    describe "init with Task"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Task "some-id")
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "some-id"
                          , { id = "some-id"
                            , name = "some-name"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initGet : Test
initGet =
    let
        version =
            Dict.fromList [ ( "some", "version" ) ]

        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "some-id"
                , step = BuildStepGet "some-name" (Just version)
                }
    in
    describe "init with Get"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Get "some-id")
                    tree
        , test "using the focus" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "some-id"
                          , { id = "some-id"
                            , name = "some-name"
                            , state = Models.StepStatePending
                            , log = cookedLog
                            , error = Nothing
                            , expanded = False
                            , version = Just version
                            , metadata = []
                            , firstOccurrence = False
                            , timestamps = Dict.empty
                            , initialize = Nothing
                            , start = Nothing
                            , finish = Nothing
                            }
                          )
                        ]
                    )
                    steps
        ]


initPut : Test
initPut =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "some-id"
                , step = BuildStepPut "some-name"
                }
    in
    describe "init with Put"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Put "some-id")
                    tree
        , test "using the focus" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "some-id"
                          , { id = "some-id"
                            , name = "some-name"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initAggregate : Test
initAggregate =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "aggregate-id"
                , step =
                    BuildStepAggregate
                        << Array.fromList
                    <|
                        [ { id = "task-a-id", step = BuildStepTask "task-a" }
                        , { id = "task-b-id", step = BuildStepTask "task-b" }
                        ]
                }
    in
    describe "init with Aggregate"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Aggregate
                        << Array.fromList
                     <|
                        [ Models.Task "task-a-id"
                        , Models.Task "task-b-id"
                        ]
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initAggregateNested : Test
initAggregateNested =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "aggregate-id"
                , step =
                    BuildStepAggregate
                        << Array.fromList
                    <|
                        [ { id = "task-a-id", step = BuildStepTask "task-a" }
                        , { id = "task-b-id", step = BuildStepTask "task-b" }
                        , { id = "nested-aggregate-id"
                          , step =
                                BuildStepAggregate
                                    << Array.fromList
                                <|
                                    [ { id = "task-c-id", step = BuildStepTask "task-c" }
                                    , { id = "task-d-id", step = BuildStepTask "task-d" }
                                    ]
                          }
                        ]
                }
    in
    describe "init with Aggregate nested"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Aggregate
                        << Array.fromList
                     <|
                        [ Models.Task "task-a-id"
                        , Models.Task "task-b-id"
                        , Models.Aggregate
                            << Array.fromList
                          <|
                            [ Models.Task "task-c-id"
                            , Models.Task "task-d-id"
                            ]
                        ]
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-c-id"
                          , { id = "task-c-id"
                            , name = "task-c"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-d-id"
                          , { id = "task-d-id"
                            , name = "task-d"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initInParallel : Test
initInParallel =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "parallel-id"
                , step =
                    BuildStepInParallel
                        << Array.fromList
                    <|
                        [ { id = "task-a-id", step = BuildStepTask "task-a" }
                        , { id = "task-b-id", step = BuildStepTask "task-b" }
                        ]
                }
    in
    describe "init with Parallel"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.InParallel
                        << Array.fromList
                     <|
                        [ Models.Task "task-a-id"
                        , Models.Task "task-b-id"
                        ]
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initInParallelNested : Test
initInParallelNested =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "parallel-id"
                , step =
                    BuildStepInParallel
                        << Array.fromList
                    <|
                        [ { id = "task-a-id", step = BuildStepTask "task-a" }
                        , { id = "task-b-id", step = BuildStepTask "task-b" }
                        , { id = "nested-parallel-id"
                          , step =
                                BuildStepInParallel
                                    << Array.fromList
                                <|
                                    [ { id = "task-c-id", step = BuildStepTask "task-c" }
                                    , { id = "task-d-id", step = BuildStepTask "task-d" }
                                    ]
                          }
                        ]
                }
    in
    describe "init with Parallel nested"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.InParallel
                        << Array.fromList
                     <|
                        [ Models.Task "task-a-id"
                        , Models.Task "task-b-id"
                        , Models.InParallel
                            << Array.fromList
                          <|
                            [ Models.Task "task-c-id"
                            , Models.Task "task-d-id"
                            ]
                        ]
                    )
                    tree
        , test "using the focuses for nested elements" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-c-id"
                          , { id = "task-c-id"
                            , name = "task-c"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-d-id"
                          , { id = "task-d-id"
                            , name = "task-d"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initOnSuccess : Test
initOnSuccess =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "on-success-id"
                , step =
                    BuildStepOnSuccess <|
                        HookedPlan
                            { id = "task-a-id", step = BuildStepTask "task-a" }
                            { id = "task-b-id", step = BuildStepTask "task-b" }
                }
    in
    describe "init with OnSuccess"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.OnSuccess <|
                        Models.HookedStep
                            (Models.Task "task-a-id")
                            (Models.Task "task-b-id")
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initOnFailure : Test
initOnFailure =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "on-success-id"
                , step =
                    BuildStepOnFailure <|
                        HookedPlan
                            { id = "task-a-id", step = BuildStepTask "task-a" }
                            { id = "task-b-id", step = BuildStepTask "task-b" }
                }
    in
    describe "init with OnFailure"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.OnFailure <|
                        Models.HookedStep
                            (Models.Task "task-a-id")
                            (Models.Task "task-b-id")
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initEnsure : Test
initEnsure =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "on-success-id"
                , step =
                    BuildStepEnsure <|
                        HookedPlan
                            { id = "task-a-id", step = BuildStepTask "task-a" }
                            { id = "task-b-id", step = BuildStepTask "task-b" }
                }
    in
    describe "init with Ensure"
        [ test "the tree" <|
            \_ ->
                Expect.equal
                    (Models.Ensure <|
                        Models.HookedStep
                            (Models.Task "task-a-id")
                            (Models.Task "task-b-id")
                    )
                    tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        , ( "task-b-id"
                          , { id = "task-b-id"
                            , name = "task-b"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initTry : Test
initTry =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "on-success-id"
                , step =
                    BuildStepTry
                        { id = "task-a-id"
                        , step = BuildStepTask "task-a"
                        }
                }
    in
    describe "init with Try"
        [ test "the tree" <|
            \_ ->
                Expect.equal (Models.Try <| Models.Task "task-a-id") tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


initTimeout : Test
initTimeout =
    let
        { tree, steps } =
            StepTree.init Routes.HighlightNothing
                emptyResources
                { id = "on-success-id"
                , step =
                    BuildStepTimeout { id = "task-a-id", step = BuildStepTask "task-a" }
                }
    in
    describe "init with Timeout"
        [ test "the tree" <|
            \_ ->
                Expect.equal (Models.Timeout <| Models.Task "task-a-id") tree
        , test "the steps" <|
            \_ ->
                Expect.equal
                    (Dict.fromList
                        [ ( "task-a-id"
                          , { id = "task-a-id"
                            , name = "task-a"
                            , state = Models.StepStatePending
                            , log = cookedLog
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
                          )
                        ]
                    )
                    steps
        ]


cookedLog : Ansi.Log.Model
cookedLog =
    Ansi.Log.init Ansi.Log.Cooked
