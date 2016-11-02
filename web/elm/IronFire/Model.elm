module IronFire.Model exposing (..)

--import IronFire.Update exposing (Msg)

import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JE


type Msg
    = NoOp
    | AddTodo
    | SetInput String
    | DoWorkOnTodo Int
    | FinishTodo Int
    | KillTodo Int
    | RenewTodo Int
    | SetTodoInput Int String
    | SetTodoNotes Int String
    | CancelTodoInput Int
    | FinishTodoInput Int
    | SelectTodo Int
    | UnselectTodo
    | FocusNotes Int
    | BlurNotes Int
    | SetEditingNotes Bool
    | SelectBefore Time
    | SelectAfter Time
    | CheckForColdTodos Time
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
    | ItIsNow Time
    | ClearLocalTodos
    | RxStatus JE.Value


type TodoStatus
    = Hot
    | Warm
    | Cool
    | Cold
    | Finished
    | Dead


type TaskSaveStatus
    = Unsaved
    | Modified
    | Saved


type alias Todo =
    { phxId : Maybe Int
    , elmId : Int
    , text : String
    , notes : String
    , status : TodoStatus
    , timesRenewed : Int
    , lastWorked : Time
    , lastModified : Time
    , input : Maybe String
    , saveStatus : TaskSaveStatus
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
    , selectedId : Maybe Int
    , status : AppStatus
    , viewFilter : ViewFilter
    , nextId : Int
    , settings : AppSettings
    , phxInfo : PhxInfo
    , phxSocket : Phoenix.Socket.Socket Msg
    , currentTime : Time
    , statusTimestamp : Time
    , editingNotes : Bool
    }


type alias PhxInfo =
    { userid : String
    , token : String
    , phxUrl : String
    }


defaultSettings : AppSettings
defaultSettings =
    { show = False
    , freezeThreshold = 3
    , coldCheckInterval = 30
    , coldCheckIntervalUnit = Seconds
    , coldLength = 7
    , coldLengthUnit = Days
    }


newModel : PhxInfo -> Phoenix.Socket.Socket Msg -> Model
newModel info' socket =
    { inputText = ""
    , todos = []
    , selectedId = Nothing
    , status = Normal
    , viewFilter = ViewAlive
    , nextId = 1
    , settings = defaultSettings
    , phxInfo = info'
    , phxSocket = socket
    , currentTime = 0
    , statusTimestamp = 0
    , editingNotes = False
    }


newTodo : Int -> String -> Time -> Todo
newTodo id text' timestamp =
    { phxId = Nothing
    , elmId = id
    , text = text'
    , notes = ""
    , status = Hot
    , timesRenewed = 0
    , lastWorked = timestamp
    , lastModified = timestamp
    , input = Nothing
    , saveStatus = Unsaved
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
