module IronFire.Interop exposing (..)

import IronFire.Model exposing (..)
import Json.Encode as JE
import Json.Decode as JD
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


encodeLocalAppStatus : String -> AppStatus -> String
encodeLocalAppStatus userid status =
    let
        value =
            JE.object
                [ ( "userid", JE.string userid )
                , ( "appstatus", JE.string <| toString status )
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
        |: (JD.field "userid" JD.string)
        |: (JD.field "token" JD.string)
        |: (JD.field "phxUrl" JD.string)


decodePhxInfo : Value -> Result String PhxInfo
decodePhxInfo =
    JD.decodeValue phxInfoDecoder


appSettingsDecoder : JD.Decoder AppSettings
appSettingsDecoder =
    JD.succeed AppSettings
        |: (JD.succeed False)
        |: (JD.field "freezeThreshold" JD.int)
        |: (JD.field "coldCheckInterval" JD.int)
        |: (JD.field "coldCheckIntervalUnit" JD.string |> JD.andThen toTimeInterval)
        |: (JD.field "coldLength" JD.int)
        |: (JD.field "coldLengthUnit" JD.string |> JD.andThen toTimeInterval)


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
        |: (JD.maybe (JD.field "phxId" JD.int) |> JD.andThen toPhxId)
        |: (JD.oneOf [ (JD.field "elmId" JD.int), (JD.succeed 0) ])
        |: (JD.field "text" JD.string)
        |: (JD.oneOf [ (JD.field "notes" JD.string), (JD.succeed "") ])
        |: (JD.field "status" JD.string |> JD.andThen toStatus)
        |: (JD.field "timesRenewed" JD.int)
        |: (JD.oneOf [ (JD.field "lastTouched" JD.float), (JD.field "lastWorked" JD.float) ])
        |: (JD.oneOf [ (JD.field "lastModified" JD.float), (JD.succeed 0) ])
        |: (JD.succeed Nothing)
        |: (JD.oneOf [ (JD.field "saveStatus" JD.string |> JD.andThen stringToSaveStatus), (JD.field "phxId" JD.int |> JD.andThen phxIdToSaveStatus), (JD.succeed Unsaved) ])


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
        (-1) ->
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
        |: (JD.field "phxId" JD.int)
        |: (JD.field "elmId" JD.int)


decodeAck : Value -> Result String ( Int, Int )
decodeAck =
    JD.decodeValue ackDecoder


timedAppStatusDecoder : JD.Decoder ( Time, AppStatus )
timedAppStatusDecoder =
    JD.succeed (,)
        |: (JD.field "timestamp" JD.float)
        |: appStatusDecoder


appStatusDecoder : JD.Decoder AppStatus
appStatusDecoder =
    JD.map toAppStatus (JD.field "frozen" JD.bool)


toAppStatus : Bool -> AppStatus
toAppStatus text =
    case text of
        True ->
            Frozen

        False ->
            Normal


localAppStatusDecoder : JD.Decoder AppStatus
localAppStatusDecoder =
    JD.string |> JD.andThen stringToAppStatus


stringToAppStatus : String -> JD.Decoder AppStatus
stringToAppStatus text =
    case text of
        "Normal" ->
            JD.succeed Normal

        "Frozen" ->
            JD.succeed Frozen

        _ ->
            JD.fail "Unexpected AppStatus value"


decodeTimedAppStatus : Value -> Result String ( Time, AppStatus )
decodeTimedAppStatus =
    JD.decodeValue timedAppStatusDecoder


decodeLocalAppStatus : Value -> Result String AppStatus
decodeLocalAppStatus =
    JD.decodeValue localAppStatusDecoder
