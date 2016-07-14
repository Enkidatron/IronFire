module Main exposing (..)

import Html.App as App
import IronFire.Model exposing (..)
import IronFire.Update exposing (..)
import IronFire.View exposing (..)
import IronFire.Interop exposing (..)
import Phoenix.Socket
import Phoenix.Channel


{--
TODO:
   be able to add notes that only show when task is selected
eventually show task Temperature by color instead of words
show Phoenix connection status? Can't.
Have button to reconnect to phoenix?
Separate update settings from saving settings functionality?

Have javascript boot code just fetch our locally saved settings
  and todos before booting us, and pass them in with the phxInfo
  Then we can just initialize our todos to that value, and don't
  have to worry about collisions with phoenix tasks
--}


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
            Phoenix.Socket.init "wss://whispering-wave-44301.herokuapp.com/socket/websocket"
                |> Phoenix.Socket.on "new_todo" userChannel RxTodoPhx
                |> Phoenix.Socket.on "ack_todo" userChannel AckTodoPhx
                |> Phoenix.Socket.on "set_settings" userChannel RxSettings

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
