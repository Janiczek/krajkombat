module Resource exposing
    ( Resources
    , applyDelta
    , applyDeltas
    , canApplyDeltas
    , init
    )

import ResourceDelta exposing (ResourceDelta(..))


type alias Resources =
    { ap : Int
    , apPerMonth : Int
    , gref : Int
    , bref : Int
    , bbv : Int
    , bbvPerMonth : Int
    }


init : Resources
init =
    { ap = 100
    , apPerMonth = 100
    , gref = 60
    , bref = 40
    , bbv = 0
    , bbvPerMonth = 4
    }


applyDelta : ResourceDelta -> Resources -> Resources
applyDelta delta resources =
    let
        f : Int -> Int -> Int
        f a b =
            max 0 (a + b)
    in
    if canApplyDelta resources delta then
        case delta of
            AP n ->
                { resources | ap = f resources.ap n }

            APPerMonth n ->
                { resources | apPerMonth = f resources.apPerMonth n }

            GREF n ->
                { resources | gref = f resources.gref n }

            BREF n ->
                { resources | bref = f resources.bref n }

            BBV n ->
                { resources | bbv = f resources.bbv n }

            BBVPerMonth n ->
                { resources | bbvPerMonth = f resources.bbvPerMonth n }

    else
        resources


canApplyDelta : Resources -> ResourceDelta -> Bool
canApplyDelta resources delta =
    let
        rule : number -> number -> Bool
        rule n resource =
            resource + n >= 0
    in
    case delta of
        AP n ->
            rule n resources.ap

        APPerMonth n ->
            rule n resources.apPerMonth

        GREF n ->
            rule n resources.gref

        BREF n ->
            rule n resources.bref

        BBV n ->
            rule n resources.bbv

        BBVPerMonth n ->
            rule n resources.bbvPerMonth


canApplyDeltas : Resources -> List ResourceDelta -> Bool
canApplyDeltas resources deltas =
    List.all (canApplyDelta resources) deltas


applyDeltas : { alwaysApply : Bool } -> List ResourceDelta -> Resources -> Resources
applyDeltas { alwaysApply } deltas resources =
    if alwaysApply || canApplyDeltas resources deltas then
        List.foldl applyDelta resources deltas

    else
        resources
