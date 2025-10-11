module Resource exposing
    ( Resources
    , applyDelta
    , canApplyDelta
    , init
    )

import ResourceDelta exposing (ResourceDelta(..))


type alias Resources =
    { ap : Int
    , apPerMonth : Int
    , gref : Float
    , bref : Float
    , bbv : Int
    , bbvPerMonth : Int
    }


init : Resources
init =
    { ap = 100
    , apPerMonth = 100
    , gref = 1.0
    , bref = 1.0
    , bbv = 0
    , bbvPerMonth = 4 -- just under 100 after 24 months
    }


applyDelta : ResourceDelta -> Resources -> Resources
applyDelta delta resources =
    if canApplyDelta resources delta then
        case delta of
            AP n ->
                { resources | ap = resources.ap + n }

            APPerMonth n ->
                { resources | apPerMonth = resources.apPerMonth + n }

            GREF n ->
                { resources | gref = resources.gref + n }

            BREF n ->
                { resources | bref = resources.bref + n }

            BBV n ->
                { resources | bbv = resources.bbv + n }

            BBVPerMonth n ->
                { resources | bbvPerMonth = resources.bbvPerMonth + n }

    else
        resources


canApplyDelta : Resources -> ResourceDelta -> Bool
canApplyDelta resources delta =
    let
        rule : number -> number -> Bool
        rule n resource =
            let
                _ =
                    Debug.log "rule" ( n, resource, ( resource + n, resource + n >= 0 ) )
            in
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
