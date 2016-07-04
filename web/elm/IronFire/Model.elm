module IronFire.Model exposing (..)

--import IronFire.Update exposing (Msg)

import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JE


type Msg
    = NoOp
    | AddTodo
    | SetInput String
    | TouchTodo Todo
    | FinishTodo Todo
    | KillTodo Todo
    | RenewTodo Todo
    | CheckForColdTodos Time
    | UpdateTodoTime Todo Time
    | SetViewFilter ViewFilter
    | ToggleSettings
    | SetThreshold String
    | SetColdCheckInterval String
    | SetColdCheckIntervalUnit String
    | SetColdLength String
    | SetColdLengthUnit String
    | RxTodosLocal JE.Value
    | RxTodoPhx JE.Value
    | RxSettings JE.Value
    | AckTodoPhx JE.Value
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SaveAllUnsaved


type TodoStatus
    = Hot
    | Warm
    | Cool
    | Cold
    | Finished
    | Dead


type alias Todo =
    { phxId : Maybe Int
    , elmId : Int
    , text : String
    , status : TodoStatus
    , timesRenewed : Int
    , lastTouched : Time
    , input : Maybe String
    }


type ViewFilter
    = ViewAll
    | ViewAlive
    | ViewFinished
    | ViewDead


type AppStatus
    = Normal
    | Frozen


type TimeInterval
    = Seconds
    | Minutes
    | Hours
    | Days


type alias AppSettings =
    { show : Bool
    , freezeThreshold : Int
    , coldCheckInterval : Int
    , coldCheckIntervalUnit : TimeInterval
    , coldLength : Int
    , coldLengthUnit : TimeInterval
    }


type alias Model =
    { inputText : String
    , todos : List Todo
    , status : AppStatus
    , viewFilter : ViewFilter
    , nextId : Int
    , settings : AppSettings
    , userid : String
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type alias PhxInfo =
    { userid : String
    , token : String
    }



--, phxSocket : Phoenix.Socket.Socket Msg


defaultSettings : AppSettings
defaultSettings =
    { show = False
    , freezeThreshold = 3
    , coldCheckInterval = 30
    , coldCheckIntervalUnit = Seconds
    , coldLength = 7
    , coldLengthUnit = Days
    }


newModel : String -> Phoenix.Socket.Socket Msg -> Model
newModel userid' socket =
    { inputText = ""
    , todos = []
    , status = Normal
    , viewFilter = ViewAlive
    , nextId = 1
    , settings = defaultSettings
    , userid = userid'
    , phxSocket = socket
    }



-- HELPERS


isAlive : Todo -> Bool
isAlive task =
    List.member task.status [ Hot, Warm, Cool, Cold ]


toTime : TimeInterval -> Time
toTime interval =
    case interval of
        Seconds ->
            Time.second

        Minutes ->
            Time.minute

        Hours ->
            Time.hour

        Days ->
            24 * Time.hour
