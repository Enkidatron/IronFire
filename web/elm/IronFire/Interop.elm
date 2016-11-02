module IronFire.Interop exposing (..)

import IronFire.Model exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import String
import Time exposing (Time)


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
        , ( "notes", JE.string todo.notes )
        , ( "status", JE.string <| toString todo.status )
        , ( "timesRenewed", JE.int todo.timesRenewed )
        , ( "lastWorked", JE.float todo.lastWorked )
        , ( "lastModified", JE.float todo.lastModified )
        , ( "saveStatus", JE.string <| toString todo.saveStatus )
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


jsonTimedAppStatus : Time -> AppStatus -> Value
jsonTimedAppStatus time status =
    let
        frozen =
            case status of
                Frozen ->
                    True

                Normal ->
                    False
    in
        JE.object [ ( "timestamp", JE.float time ), ( "frozen", JE.bool frozen ) ]


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
        |: ("phxUrl" := JD.string)


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
        |: (JD.maybe ("phxId" := JD.int) `JD.andThen` toPhxId)
        |: (JD.oneOf [ ("elmId" := JD.int), (JD.succeed 0) ])
        |: ("text" := JD.string)
        |: (JD.oneOf [ ("notes" := JD.string), (JD.succeed "") ])
        |: ("status" := JD.string `JD.andThen` toStatus)
        |: ("timesRenewed" := JD.int)
        |: (JD.oneOf [ ("lastTouched" := JD.float), ("lastWorked" := JD.float) ])
        |: (JD.oneOf [ ("lastModified" := JD.float), (JD.succeed 0) ])
        |: (JD.succeed Nothing)
        |: (JD.oneOf [ ("saveStatus" := JD.string `JD.andThen` stringToSaveStatus), ("phxId" := JD.int `JD.andThen` phxIdToSaveStatus), (JD.succeed Unsaved) ])


toPhxId : Maybe number -> JD.Decoder (Maybe number)
toPhxId id =
    case id of
        Just -1 ->
            JD.succeed Nothing

        Just num ->
            JD.succeed (Just num)

        Nothing ->
            JD.succeed Nothing


toStatus : String -> JD.Decoder TodoStatus
toStatus text =
    case text of
        "Hot" ->
            JD.succeed Hot

        "Warm" ->
            JD.succeed Warm

        "Cool" ->
            JD.succeed Cool

        "Cold" ->
            JD.succeed Cold

        "Finished" ->
            JD.succeed Finished

        "Dead" ->
            JD.succeed Dead

        _ ->
            JD.fail "Invalid TodoStatus"


phxIdToSaveStatus : Int -> JD.Decoder TaskSaveStatus
phxIdToSaveStatus phxId =
    case phxId of
        -1 ->
            JD.succeed Unsaved

        _ ->
            JD.succeed Saved


stringToSaveStatus : String -> JD.Decoder TaskSaveStatus
stringToSaveStatus text =
    case (String.toLower text) of
        "unsaved" ->
            JD.succeed Unsaved

        "modified" ->
            JD.succeed Modified

        "saved" ->
            JD.succeed Saved

        _ ->
            JD.fail "Invalid saveStatus"


decodeTodos : Value -> Result String (List Todo)
decodeTodos =
    JD.decodeValue todosDecoder


decodeTodo : Value -> Result String Todo
decodeTodo =
    JD.decodeValue todoDecoder


ackDecoder : JD.Decoder ( Int, Int )
ackDecoder =
    JD.succeed (,)
        |: ("phxId" := JD.int)
        |: ("elmId" := JD.int)


decodeAck : Value -> Result String ( Int, Int )
decodeAck =
    JD.decodeValue ackDecoder


timedAppStatusDecoder : JD.Decoder ( Time, AppStatus )
timedAppStatusDecoder =
    JD.succeed (,)
        |: ("timestamp" := JD.float)
        |: appStatusDecoder


appStatusDecoder : JD.Decoder AppStatus
appStatusDecoder =
    JD.map toAppStatus ("frozen" := JD.bool)


toAppStatus : Bool -> AppStatus
toAppStatus text =
    case text of
        True ->
            Frozen

        False ->
            Normal


decodeTimedAppStatus : Value -> Result String ( Time, AppStatus )
decodeTimedAppStatus =
    JD.decodeValue timedAppStatusDecoder
