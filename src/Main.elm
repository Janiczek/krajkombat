module Main exposing (Flags, Model, Msg, main)

import Browser
import Game exposing (Game)
import Html
import Random


type alias Flags =
    ()


type alias Model =
    { gamePhase : Game.Phase
    }


type Msg
    = StartGame
    | GameLoopInitialized Game


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { gamePhase = Game.MainMenu }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model
            , Random.generate GameLoopInitialized Game.gameInitGenerator
            )

        GameLoopInitialized game ->
            ( { model | gamePhase = Game.GameLoop game }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "KrajKombat: MSK Edition"
    , body = [ Html.text "Hello, World!" ]
    }
