module Main exposing (..)

import Html.App as App
import IronFire.Model exposing (..)
import IronFire.Update exposing (..)
import IronFire.View exposing (..)
import IronFire.Interop exposing (..)
import Phoenix.Socket
import Phoenix.Channel


--TODO:
-- edit tasks text after creation
-- hotkeys
-- be able to select task, only show buttons for that one
--   hotkeys would apply to that task
--   be able to add notes that only show when task is selected
-- eventually show task Temperature by color instead of words
--show Phoenix connection status? have button to reconnect?


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
            { userid = "", token = "" }

        info =
            phxInfo
                `Maybe.andThen` (decodePhxInfo >> Result.toMaybe)
                |> Maybe.withDefault defaultinfo

        userChannel =
            "user:" ++ info.userid

        initPhoenix =
            Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "new_todo" userChannel RxTodoPhx
                |> Phoenix.Socket.on "ack_todo" userChannel AckTodoPhx
                |> Phoenix.Socket.on "set_settings" userChannel RxSettings

        channel =
            Phoenix.Channel.init userChannel
                |> Phoenix.Channel.withPayload (jsonAuthPayload info)

        ( phxSocket', phxCmd ) =
            Phoenix.Socket.join channel initPhoenix

        model =
            newModel info.userid phxSocket'
    in
        model
            ! [ Cmd.map PhoenixMsg phxCmd
              , connectLocal info.userid
              ]
