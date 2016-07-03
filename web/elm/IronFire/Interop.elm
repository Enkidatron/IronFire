module IronFire.Interop exposing (..)

import IronFire.Model exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias Value =
    JE.Value



-- ENCODE


jsonTodos : List Todo -> Value
jsonTodos todos =
    JE.list <| List.map jsonTodo todos


jsonTodo : Todo -> Value
jsonTodo todo =
    JE.object
        [ ( "phxId", JE.int <| Maybe.withDefault -1 todo.phxId )
        , ( "elmId", JE.int todo.elmId )
        , ( "text", JE.string todo.text )
        , ( "timesRenewed", JE.int todo.timesRenewed )
        , ( "lastTouched", JE.float todo.lastTouched )
        ]


jsonSettings : AppSettings -> Value
jsonSettings settings =
    JE.object
        [ ( "freezeThreshold", JE.int settings.freezeThreshold )
        , ( "coldCheckInterval", JE.int settings.coldCheckInterval )
        , ( "coldCheckIntervalUnit", JE.string <| toString settings.coldCheckIntervalUnit )
        , ( "coldLength", JE.int settings.coldLength )
        , ( "coldLengthUnit", JE.string <| toString settings.coldLengthUnit )
        ]


jsonAuthPayload : PhxInfo -> Value
jsonAuthPayload info =
    JE.object [ ( "token", JE.string info.token ) ]


encodeLocalTodos : String -> List Todo -> String
encodeLocalTodos userid todos =
    let
        value =
            JE.object
                [ ( "userid", JE.string userid )
                , ( "todos", JE.list <| List.map jsonTodo todos )
                ]
    in
        JE.encode 0 value


encodeLocalSettings : String -> AppSettings -> String
encodeLocalSettings userid settings =
    let
        value =
            JE.object
                [ ( "userid", JE.string userid )
                , ( "settings", jsonSettings settings )
                ]
    in
        JE.encode 0 value



-- DECODE


phxInfoDecoder : JD.Decoder PhxInfo
phxInfoDecoder =
    JD.succeed PhxInfo
        |: ("userid" := JD.string)
        |: ("token" := JD.string)


decodePhxInfo : Value -> Result String PhxInfo
decodePhxInfo =
    JD.decodeValue phxInfoDecoder


appSettingsDecoder : JD.Decoder AppSettings
appSettingsDecoder =
    JD.succeed AppSettings
        |: (JD.succeed False)
        |: ("freezeThreshold" := JD.int)
        |: ("coldCheckInterval" := JD.int)
        |: ("coldCheckIntervalUnit" := JD.string `JD.andThen` toTimeInterval)
        |: ("coldLength" := JD.int)
        |: ("coldLengthUnit" := JD.string `JD.andThen` toTimeInterval)


toTimeInterval : String -> JD.Decoder TimeInterval
toTimeInterval text =
    case text of
        "Seconds" ->
            JD.succeed Seconds

        "Minutes" ->
            JD.succeed Minutes

        "Hours" ->
            JD.succeed Hours

        "Days" ->
            JD.succeed Days

        _ ->
            JD.fail "Invalid TimeInterval"


decodeAppSettings : Value -> Result String AppSettings
decodeAppSettings =
    JD.decodeValue appSettingsDecoder


decodeStringAppSettings : String -> Result String AppSettings
decodeStringAppSettings =
    JD.decodeString appSettingsDecoder


todosDecoder : JD.Decoder (List Todo)
todosDecoder =
    JD.list todoDecoder


todoDecoder : JD.Decoder Todo
todoDecoder =
    JD.succeed Todo
        |: (JD.maybe ("phxId" := JD.int))
        |: ("elmId" := JD.int)
        |: ("text" := JD.string)
        |: (JD.succeed Hot)
        |: ("timesRenewed" := JD.int)
        |: ("lastTouched" := JD.float)
        |: (JD.succeed Nothing)


decodeTodos : Value -> Result String (List Todo)
decodeTodos =
    JD.decodeValue todosDecoder
