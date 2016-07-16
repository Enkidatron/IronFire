port module IronFire.Update exposing (..)

import IronFire.Model exposing (..)
import IronFire.Interop exposing (..)
import Time exposing (..)
import String exposing (toInt)
import Task exposing (perform)
import Phoenix.Socket
import Phoenix.Push
import Keyboard
import Char exposing (fromCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        AddTodo ->
            case String.trim model.inputText of
                "" ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | todos = (newTodo model.nextId model.inputText model.currentTime) :: model.todos
                        , inputText = ""
                        , nextId = model.nextId + 1
                      }
                    , focus "#task-input"
                    )
                        |> withSaveTodosWhere (.elmId >> (==) model.nextId)

        SetInput text ->
            { model | inputText = text } ! []

        DoWorkOnTodo id ->
            ( updateSpecificTodo model id (\t -> { t | status = Hot, lastWorked = model.currentTime, lastModified = model.currentTime, saved = False }), Cmd.none )
                |> withSaveTodosWhere (.elmId >> (==) id)

        FinishTodo id ->
            ( updateSpecificTodo { model | selectedId = Nothing } id (\t -> { t | status = Finished, lastModified = model.currentTime, saved = False }), Cmd.none )
                |> withSaveTodosWhere (.elmId >> (==) id)

        KillTodo id ->
            ( updateSpecificTodo { model | selectedId = Nothing } id (\t -> { t | status = Dead, lastModified = model.currentTime, saved = False }), Cmd.none )
                |> withSaveTodosWhere (.elmId >> (==) id)

        RenewTodo id ->
            ( updateSpecificTodo model id (\t -> { t | status = Hot, timesRenewed = t.timesRenewed + 1, lastWorked = model.currentTime, lastModified = model.currentTime, saved = False }), Cmd.none )
                |> withSaveTodosWhere (.elmId >> (==) id)

        SetTodoInput id input ->
            ( updateSpecificTodo model id (\t -> { t | input = Just input }), focus <| "#todo-input-" ++ toString id )

        CancelTodoInput id ->
            ( updateSpecificTodo model id (\t -> { t | input = Nothing }), Cmd.none )

        FinishTodoInput id ->
            let
                getNewText oldText input =
                    case Maybe.map String.trim input of
                        Nothing ->
                            oldText

                        Just "" ->
                            oldText

                        Just str ->
                            str
            in
                ( updateSpecificTodo model id (\t -> { t | input = Nothing, text = getNewText t.text t.input, lastModified = model.currentTime, saved = False }), Cmd.none )
                    |> withSaveTodosWhere (.elmId >> (==) id)

        SelectTodo id ->
            { model | selectedId = Just id, todos = List.map (\t -> { t | input = Nothing }) model.todos } ! []

        UnselectTodo ->
            { model | selectedId = Nothing, todos = List.map (\t -> { t | input = Nothing }) model.todos } ! []

        SelectBefore time ->
            let
                nextSelect =
                    case model.selectedId of
                        Nothing ->
                            List.head <| List.map .elmId <| List.reverse <| List.sortBy .lastWorked <| List.filter isAlive model.todos

                        Just _ ->
                            List.head <| List.map .elmId <| List.reverse <| List.sortBy .lastWorked <| List.filter (.lastWorked >> (>) time) <| List.filter isAlive model.todos

                cmd =
                    if nextSelect == Nothing then
                        focus "#task-input"
                    else
                        blur "#task-input"
            in
                { model | selectedId = nextSelect } ! [ cmd ]

        SelectAfter time ->
            let
                nextSelect =
                    List.head <| List.map .elmId <| List.sortBy .lastWorked <| List.filter (.lastWorked >> (<) time) <| List.filter isAlive model.todos

                cmd =
                    if nextSelect == Nothing then
                        focus "#task-input"
                    else
                        blur "#task-input"
            in
                { model | selectedId = nextSelect } ! [ cmd ]

        CheckForColdTodos time ->
            let
                coldLength =
                    (toFloat model.settings.coldLength) * (toTime model.settings.coldLengthUnit)

                newTodos =
                    List.map
                        (\t ->
                            if isAlive t then
                                if (time - t.lastWorked > coldLength) then
                                    { t | status = Cold }
                                else if (time - t.lastWorked > coldLength * (2 / 3)) then
                                    { t | status = Cool }
                                else if (time - t.lastWorked > coldLength * (1 / 3)) then
                                    { t | status = Warm }
                                else
                                    { t | status = Hot }
                            else
                                t
                        )
                        model.todos

                newStatus =
                    if model.status == Frozen || (List.length <| List.filter (.status >> (==) Cold) newTodos) >= model.settings.freezeThreshold then
                        Frozen
                    else
                        Normal
            in
                ( { model | todos = newTodos, status = newStatus }, Cmd.none )
                    |> withSaveTodosWhere (.saved >> (==) False)

        SetViewFilter newFilter ->
            { model | viewFilter = newFilter } ! []

        ToggleSettings ->
            let
                update settings =
                    { settings | show = not settings.show }
            in
                { model | settings = update model.settings } ! []

        SetThreshold text ->
            let
                settingsUpdater =
                    case toPositiveNumber text of
                        Ok num ->
                            (\s -> { s | freezeThreshold = num })

                        Err _ ->
                            Basics.identity
            in
                updateSettings { model | status = Normal } settingsUpdater

        SetColdCheckInterval text ->
            let
                settingsUpdater =
                    case toPositiveNumber text of
                        Ok num ->
                            (\s -> { s | coldCheckInterval = num })

                        Err _ ->
                            Basics.identity
            in
                updateSettings { model | status = Normal } settingsUpdater

        SetColdCheckIntervalUnit text ->
            updateSettings { model | status = Normal } (\s -> { s | coldCheckIntervalUnit = getTimeIntervalFromText text })

        SetColdLength text ->
            let
                settingsUpdater =
                    case toPositiveNumber text of
                        Ok num ->
                            (\s -> { s | coldLength = num })

                        Err _ ->
                            Basics.identity
            in
                updateSettings { model | status = Normal } settingsUpdater

        SetColdLengthUnit text ->
            updateSettings { model | status = Normal } (\s -> { s | coldLengthUnit = getTimeIntervalFromText text })

        RxTodosLocal value ->
            let
                newTodos =
                    case decodeTodos value of
                        Ok todos ->
                            todos

                        Err err ->
                            []

                highestId =
                    Maybe.withDefault 0 <| List.maximum <| List.map .elmId newTodos
            in
                { model | todos = newTodos ++ model.todos, nextId = max (highestId + 1) model.nextId } ! [ checkForFreezeNow ]

        RxTodoPhx value ->
            let
                shouldUpdate phxTodo todo =
                    phxTodo.phxId == todo.phxId && phxTodo.lastModified >= todo.lastModified

                model' =
                    case decodeTodo value of
                        Ok phxTodo ->
                            case List.filter (.phxId >> (==) phxTodo.phxId) model.todos of
                                [] ->
                                    { model | todos = { phxTodo | elmId = model.nextId } :: model.todos, nextId = model.nextId + 1 }

                                _ ->
                                    { model
                                        | todos =
                                            List.map
                                                (\t ->
                                                    if shouldUpdate phxTodo t then
                                                        { phxTodo | elmId = t.elmId }
                                                    else
                                                        t
                                                )
                                                model.todos
                                    }

                        Err err ->
                            Debug.log err model
            in
                model' ! [ saveTodosLocal <| encodeLocalTodos model.phxInfo.userid model'.todos, checkForFreezeNow ]

        RxSettings value ->
            let
                settings' =
                    case decodeAppSettings value of
                        Ok settings ->
                            { settings | show = model.settings.show }

                        Err err ->
                            model.settings
            in
                { model | settings = settings' } ! []

        AckTodoPhx value ->
            let
                updateTodo =
                    case decodeAck value of
                        Ok ( phxId', elmId' ) ->
                            (\todo ->
                                if todo.elmId == elmId' then
                                    { todo | phxId = Just phxId', saved = True }
                                else
                                    todo
                            )

                        Err err ->
                            identity

                newTodos =
                    List.map updateTodo model.todos
            in
                { model | todos = newTodos }
                    ! [ saveTodosLocal <| encodeLocalTodos model.phxInfo.userid newTodos ]

        PhoenixMsg msg ->
            let
                ( phxSocket', phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                { model | phxSocket = phxSocket' } ! [ Cmd.map PhoenixMsg phxCmd ]

        SaveAllUnsaved ->
            ( model, Cmd.none ) |> withSaveTodosWhere (.saved >> (==) False)

        ItIsNow time ->
            { model | currentTime = time } ! []



-- UPDATE HELPERS


checkUnfreeze : Model -> Model
checkUnfreeze model =
    if model.status == Frozen && List.all (.status >> (/=) Cold) model.todos then
        { model | status = Normal }
    else
        model


toPositiveNumber : String -> Result String Int
toPositiveNumber text =
    case toInt text of
        Ok num ->
            if num > 0 then
                Ok num
            else
                Err "Number is negative or zero"

        Err err ->
            Err err


getTimeIntervalFromText : String -> TimeInterval
getTimeIntervalFromText text =
    case text of
        "Seconds" ->
            Seconds

        "Minutes" ->
            Minutes

        "Hours" ->
            Hours

        "Days" ->
            Days

        _ ->
            Debug.crash "Tried to convert an invalid string into a TimeInterval"


updateSpecificTodo : Model -> Int -> (Todo -> Todo) -> Model
updateSpecificTodo model id updateTodo =
    let
        newTodos =
            List.map
                (\t ->
                    if t.elmId == id then
                        updateTodo t
                    else
                        t
                )
                model.todos
    in
        checkUnfreeze { model | todos = newTodos }


withSaveTodosWhere : (Todo -> Bool) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSaveTodosWhere gate ( model, cmdMsg ) =
    let
        reducer : Todo -> ( Phoenix.Socket.Socket Msg, Cmd Msg ) -> ( Phoenix.Socket.Socket Msg, Cmd Msg )
        reducer todo ( socket, cmd ) =
            let
                push' =
                    Phoenix.Push.init "set_todo" ("user:" ++ model.phxInfo.userid)
                        |> Phoenix.Push.withPayload (jsonTodo todo)

                ( newSocket, newCmd ) =
                    Phoenix.Socket.push push' socket
            in
                ( newSocket, Cmd.batch [ Cmd.map PhoenixMsg newCmd, cmd ] )

        ( phxSocket', cmd' ) =
            List.foldl reducer ( model.phxSocket, Cmd.none ) (List.filter gate model.todos)
    in
        { model | phxSocket = phxSocket' } ! [ cmdMsg, cmd', saveTodosLocal <| encodeLocalTodos model.phxInfo.userid model.todos ]


updateSettings : Model -> (AppSettings -> AppSettings) -> ( Model, Cmd Msg )
updateSettings model update =
    let
        newSettings =
            update model.settings

        push' =
            Phoenix.Push.init "set_settings" ("user:" ++ model.phxInfo.userid)
                |> Phoenix.Push.withPayload (jsonSettings newSettings)

        ( phxSocket', phxCmd ) =
            Phoenix.Socket.push push' model.phxSocket
    in
        { model
            | settings = newSettings
            , phxSocket = phxSocket'
        }
            ! [ checkForFreezeNow
              , saveSettingsLocal <| encodeLocalSettings model.phxInfo.userid newSettings
              , Cmd.map PhoenixMsg phxCmd
              ]



-- COMMANDS


port focus : String -> Cmd msg


port blur : String -> Cmd msg


port connectLocal : String -> Cmd msg


port saveTodosLocal : String -> Cmd msg


port saveSettingsLocal : String -> Cmd msg


checkForFreezeNow : Cmd Msg
checkForFreezeNow =
    Task.perform (\_ -> Debug.crash "Time Fetch Failed") CheckForColdTodos Time.now



-- SUBSCRIPTIONS


port rxTodos : (Value -> msg) -> Sub msg


port rxSettings : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interval =
            (toFloat model.settings.coldCheckInterval) * (toTime model.settings.coldCheckIntervalUnit)

        selectedTodoStatus =
            List.foldl
                (\t s ->
                    if Just t.elmId == model.selectedId then
                        t.status
                    else
                        s
                )
                Hot
                model.todos

        selectedTodoTime =
            List.foldl
                (\todo time ->
                    if Just todo.elmId == model.selectedId then
                        todo.lastWorked
                    else
                        time
                )
                0
                model.todos

        selectedTodoInput =
            List.foldl
                (\todo input ->
                    if Just todo.elmId == model.selectedId then
                        todo.input
                    else
                        input
                )
                Nothing
                model.todos

        hotkeys =
            case selectedTodoInput of
                Nothing ->
                    [ Keyboard.presses <| hotkeysFor model.selectedId model.status selectedTodoStatus
                    , Keyboard.ups <| selectHotkeysFor selectedTodoTime
                    ]

                Just _ ->
                    []
    in
        Sub.batch
            ([ Time.every interval CheckForColdTodos
             , Time.every second ItIsNow
             , rxTodos RxTodosLocal
             , rxSettings RxSettings
             , Phoenix.Socket.listen model.phxSocket PhoenixMsg
             ]
                ++ hotkeys
            )


hotkeysFor : Maybe Int -> AppStatus -> TodoStatus -> Keyboard.KeyCode -> Msg
hotkeysFor maybeid appstatus todostatus keycode =
    case maybeid of
        Nothing ->
            NoOp

        Just id ->
            case ( appstatus, todostatus ) of
                ( Frozen, Cold ) ->
                    case fromCode keycode of
                        'w' ->
                            DoWorkOnTodo id

                        'W' ->
                            DoWorkOnTodo id

                        'f' ->
                            FinishTodo id

                        'F' ->
                            FinishTodo id

                        'r' ->
                            RenewTodo id

                        'R' ->
                            RenewTodo id

                        'k' ->
                            KillTodo id

                        'K' ->
                            KillTodo id

                        _ ->
                            NoOp

                ( Frozen, _ ) ->
                    NoOp

                ( Normal, Dead ) ->
                    NoOp

                ( Normal, Finished ) ->
                    NoOp

                ( Normal, _ ) ->
                    case fromCode keycode of
                        'w' ->
                            DoWorkOnTodo id

                        'W' ->
                            DoWorkOnTodo id

                        'f' ->
                            FinishTodo id

                        'F' ->
                            FinishTodo id

                        'k' ->
                            KillTodo id

                        'K' ->
                            KillTodo id

                        _ ->
                            NoOp


selectHotkeysFor : Time -> Keyboard.KeyCode -> Msg
selectHotkeysFor time code =
    case code of
        38 ->
            SelectBefore time

        40 ->
            SelectAfter time

        _ ->
            NoOp
