module Main exposing (..)

import Html
import IronFire.Model exposing (..)
import IronFire.Update exposing (..)
import IronFire.View exposing (..)
import IronFire.Interop exposing (..)


main : Program (Maybe Value) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe Value -> ( Model, Cmd Msg )
init phxInfo =
    let
        defaultinfo =
            { userid = "", token = "", phxUrl = "ws://localhost:4000/socket/websocket" }

        info =
            phxInfo
                |> Maybe.andThen (decodePhxInfo >> Result.toMaybe)
                |> Maybe.withDefault defaultinfo

        model =
            newModel info
    in
        model ! [ connectLocal info.userid ]
