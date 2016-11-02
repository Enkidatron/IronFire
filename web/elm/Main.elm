module Main exposing (..)

import Html.App as App
import IronFire.Model exposing (..)
import IronFire.Update exposing (..)
import IronFire.View exposing (..)
import IronFire.Interop exposing (..)
import Phoenix.Socket
import Phoenix.Channel


main : Program (Maybe Value)
main =
    App.programWithFlags
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
                `Maybe.andThen` (decodePhxInfo >> Result.toMaybe)
                |> Maybe.withDefault defaultinfo

        userChannel =
            "user:" ++ info.userid

        initPhoenix =
            Phoenix.Socket.init info.phxUrl
                |> Phoenix.Socket.on "new_todo" userChannel RxTodoPhx
                |> Phoenix.Socket.on "ack_todo" userChannel AckTodoPhx
                |> Phoenix.Socket.on "set_settings" userChannel RxSettings
                |> Phoenix.Socket.on "app_status" userChannel RxStatus

        channel =
            Phoenix.Channel.init userChannel
                |> Phoenix.Channel.withPayload (jsonAuthPayload info)

        ( phxSocket', phxCmd ) =
            Phoenix.Socket.join channel initPhoenix

        model =
            newModel info phxSocket'
    in
        model
            ! [ Cmd.map PhoenixMsg phxCmd
              , connectLocal info.userid
              ]
