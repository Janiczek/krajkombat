module Ranking exposing (Ranking, rank)


type alias Ranking =
    List
        { rank : Int
        , bbv : Int
        , names : List String
        }


type alias StepState =
    { previousBBV : Maybe Int
    , currentRank : Int
    , index : Int
    }


rank :
    { game
        | you : { region | name : String, stats : { stats | bbv : Int } }
        , others : List { region | name : String, stats : { stats | bbv : Int } }
    }
    -> Ranking
rank { you, others } =
    let
        regionToItem :
            { region | name : String, stats : { stats | bbv : Int } }
            -> { name : String, bbv : Int }
        regionToItem r =
            { name = r.name, bbv = r.stats.bbv }

        allRegions : List { name : String, bbv : Int }
        allRegions =
            List.map regionToItem (you :: others)

        sorted : List { name : String, bbv : Int }
        sorted =
            allRegions
                |> List.sortBy (\x -> -x.bbv)

        step :
            { name : String, bbv : Int }
            ->
                ( List { rank : Int, bbv : Int, names : List String }
                , StepState
                )
            ->
                ( List { rank : Int, bbv : Int, names : List String }
                , StepState
                )
        step current ( ranked, state ) =
            let
                rank_ =
                    case state.previousBBV of
                        Just prevBbv ->
                            if current.bbv == prevBbv then
                                state.currentRank

                            else
                                state.index

                        Nothing ->
                            0
            in
            ( ranked
                ++ [ { rank = rank_
                     , bbv = current.bbv
                     , names = [ current.name ]
                     }
                   ]
            , { previousBBV = Just current.bbv, currentRank = rank_, index = state.index + 1 }
            )

        init =
            ( [], { previousBBV = Nothing, currentRank = 0, index = 0 } )
    in
    List.foldl step init sorted
        |> Tuple.first
        -- After foldl, combine entries with same rank (for names-tied)
        |> List.foldr
            (\r acc ->
                case acc of
                    h :: t ->
                        if r.rank == h.rank then
                            { h | names = r.names ++ h.names } :: t

                        else
                            r :: acc

                    [] ->
                        [ r ]
            )
            []
