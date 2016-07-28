module IronFire.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode, onFocus, onBlur)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes as SA
import IronFire.Model exposing (..)
import Json.Decode as JD


view : Model -> Html Msg
view model =
    let
        frozen =
            model.status == Frozen

        selectedId =
            Maybe.withDefault -1 model.selectedId

        viewFilter =
            case model.viewFilter of
                ViewAll ->
                    always True

                ViewAlive ->
                    isAlive

                ViewFinished ->
                    .status >> (==) Finished

                ViewDead ->
                    .status >> (==) Dead

        saveAllDisabled =
            not <| List.any (.saveStatus >> (/=) Saved) model.todos
    in
        div [ class "container" ]
            [ table [ class "table table-condensced" ]
                [ tr []
                    [ displayViewFilterButtons model.viewFilter
                    , button [ type' "button", class "btn btn-primary pull-right", onClick SaveAllUnsaved, disabled saveAllDisabled ] [ text "Save All" ]
                    ]
                ]
            , table [ class "table table-condensced" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Task" ]
                        , th [] [ text "Status" ]
                        , th [] []
                        , th [] []
                        , th [] []
                        ]
                    ]
                , tbody []
                    ((List.concatMap (displayTodo frozen selectedId) <| List.filter viewFilter <| List.sortBy .lastWorked model.todos)
                        ++ [ displayInputRow model.inputText ]
                    )
                ]
            , displaySettingsArea model.settings
            ]


displayViewFilterButtons : ViewFilter -> Html Msg
displayViewFilterButtons filter =
    let
        getClass =
            (\c ->
                if c == filter then
                    "btn btn-info"
                else
                    "btn"
            )

        displayViewButton view label =
            button [ type' "button", class <| getClass view, onClick <| SetViewFilter view ] [ text label ]
    in
        div [ class "btn-group" ]
            [ displayViewButton ViewAll "All"
            , displayViewButton ViewAlive "Active"
            , displayViewButton ViewFinished "Finished"
            , displayViewButton ViewDead "Dead"
            ]


displayTodo : Bool -> Int -> Todo -> List (Html Msg)
displayTodo frozen selectedId todo =
    let
        isSelected =
            todo.elmId == selectedId

        rowClass =
            if isSelected then
                "info"
            else if (frozen && todo.status == Cold) then
                "active"
            else
                ""

        rowAttributes =
            [ class rowClass ]
                ++ if isSelected then
                    []
                   else
                    [ onClick <| SelectTodo todo.elmId ]

        tdTodoTextElement =
            case ( todo.input, isAlive todo, isSelected ) of
                ( Just inputText, True, _ ) ->
                    td []
                        [ input
                            [ type' "text"
                            , class "form-control"
                            , id <| "todo-input-" ++ toString todo.elmId
                            , onInput <| SetTodoInput todo.elmId
                            , placeholder todo.text
                            , value inputText
                            , onEnter NoOp <| FinishTodoInput todo.elmId
                            , onEsc NoOp <| CancelTodoInput todo.elmId
                            ]
                            []
                        ]

                ( Nothing, True, True ) ->
                    td [ onClick <| SetTodoInput todo.elmId "" ] [ text todo.text ]

                ( _, _, _ ) ->
                    td [] [ text todo.text ]

        ironColor =
            case todo.status of
                Hot ->
                    "yellow"

                Warm ->
                    "orange"

                Cool ->
                    "red"

                Cold ->
                    "darkgray"

                _ ->
                    "black"

        tempElement =
            Svg.svg [ SA.width "50", SA.height "20" ]
                [ Svg.rect [ SA.x "0", SA.y "0", SA.width "50", SA.height "20", SA.rx "5", SA.ry "5", SA.fill ironColor ] []
                ]

        extraButtons =
            if frozen && todo.status == Cold then
                [ button [ type' "button", class "btn btn-warning", onClick <| RenewTodo todo.elmId ] [ text "Renew" ]
                ]
            else
                []

        buttons =
            case ( todo.input, (isSelected && (isAlive todo) && (not frozen)) || (frozen && todo.status == Cold) ) of
                ( Nothing, True ) ->
                    div [ class "btn-group" ]
                        ([ button [ type' "button", class "btn btn-info", onClick <| DoWorkOnTodo todo.elmId ] [ text "I Worked On This" ]
                         , button [ type' "button", class "btn btn-success", onClick <| FinishTodo todo.elmId ] [ text "Finish" ]
                         ]
                            ++ extraButtons
                            ++ [ button [ type' "button", class "btn btn-danger", onClick <| KillTodo todo.elmId ] [ text "Kill" ]
                               ]
                        )

                ( Just _, _ ) ->
                    div [ class "btn-group" ]
                        [ button [ type' "button", class "btn btn-info", onClick <| FinishTodoInput todo.elmId ] [ text "Update" ]
                        , button [ type' "button", class "btn btn-warning", onClick <| CancelTodoInput todo.elmId ] [ text "Cancel" ]
                        ]

                ( _, _ ) ->
                    div [] []

        saveText =
            case todo.saveStatus of
                Saved ->
                    ""

                Modified ->
                    "*"

                Unsaved ->
                    "!"

        notesrow =
            if isSelected then
                [ tr [ class "info" ]
                    [ td [ colspan 5 ]
                        [ textarea
                            [ class "form-control"
                            , id <| "todo-notes-" ++ toString todo.elmId
                            , placeholder "Notes"
                            , value todo.notes
                            , maxlength 255
                            , onInput <| SetTodoNotes todo.elmId
                            , onEsc NoOp <| BlurNotes todo.elmId
                            , onFocus <| SetEditingNotes True
                            , onBlur <| SetEditingNotes False
                            ]
                            []
                        ]
                    ]
                ]
            else
                []
    in
        [ tr rowAttributes
            [ tdTodoTextElement
            , td [] [ tempElement ]
            , td [] [ span [ class "badge" ] [ text <| toString todo.timesRenewed ] ]
            , td [] [ buttons ]
            , td [] [ text saveText ]
            ]
        ]
            ++ notesrow


displayInputRow : String -> Html Msg
displayInputRow inputText =
    tr []
        [ td []
            [ div [ class "form-group" ]
                [ input [ type' "text", class "form-control", id "task-input", onInput SetInput, placeholder "New Todo", value inputText, onEnter NoOp AddTodo, onFocus <| UnselectTodo ] []
                ]
            ]
        , td [] []
        , td [] []
        , td []
            [ button [ type' "button", class "btn btn-primary", onClick AddTodo ] [ text "Add Todo" ]
            ]
        , td [] []
        ]


onEnter : msg -> msg -> Attribute msg
onEnter fail success =
    let
        tagger code =
            if code == 13 then
                success
            else
                fail
    in
        on "keypress" (JD.map tagger keyCode)


onKeyup : Int -> msg -> msg -> Attribute msg
onKeyup code fail success =
    let
        tagger code' =
            if code' == code then
                success
            else
                fail
    in
        on "keyup" (JD.map tagger keyCode)


onEsc : msg -> msg -> Attribute msg
onEsc =
    onKeyup 27


displaySettingsArea : AppSettings -> Html Msg
displaySettingsArea settings =
    let
        body =
            if settings.show then
                displaySettingsPanelBody settings
            else
                div [] []
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading", onClick ToggleSettings ]
                [ h3 [ class "panel-title" ] [ text "Settings" ] ]
            , body
            ]


displaySettingsPanelBody : AppSettings -> Html Msg
displaySettingsPanelBody settings =
    div [ class "panel-body" ]
        [ Html.form [ class "form-horizontal" ]
            [ div [ class "form-group" ]
                [ label [ for "freezeThreshold", class "col-sm-3 control-label" ] [ text "Freeze Threshold" ]
                , div [ class "col-sm-9" ]
                    [ input [ type' "number", class "form-control", id "freezeThreshold", onInput SetThreshold, value <| toString settings.freezeThreshold ] [] ]
                ]
            , div [ class "form-group" ]
                [ label [ for "intervalNumber", class "col-sm-3 control-label" ] [ text "Interval" ]
                , div [ class "col-sm-6" ]
                    [ input [ type' "number", class "form-control", id "intervalNumber", onInput SetColdCheckInterval, value <| toString settings.coldCheckInterval ] [] ]
                , div [ class "col-sm-3" ]
                    [ label [ for "intervalUnit", class "sr-only" ] [ text "Inverval Unit" ]
                    , select [ class "form-control", onInput SetColdCheckIntervalUnit, id "intervalUnit" ]
                        [ option [ selected <| settings.coldCheckIntervalUnit == Seconds ] [ text <| toString Seconds ]
                        , option [ selected <| settings.coldCheckIntervalUnit == Minutes ] [ text <| toString Minutes ]
                        ]
                    ]
                ]
            , div [ class "form-group" ]
                [ label [ for "coldLength", class "col-sm-3 control-label" ] [ text "Time Before Cold" ]
                , div [ class "col-sm-6" ]
                    [ input [ type' "number", class "form-control", id "coldLength", onInput SetColdLength, value <| toString settings.coldLength ] [] ]
                , div [ class "col-sm-3" ]
                    [ label [ for "coldLengthUnit", class "sr-only" ] [ text "Cold Length Unit" ]
                    , select [ class "form-control", onInput SetColdLengthUnit, id "coldLengthUnit" ]
                        [ option [ selected <| settings.coldLengthUnit == Seconds ] [ text <| toString Seconds ]
                        , option [ selected <| settings.coldLengthUnit == Minutes ] [ text <| toString Minutes ]
                        , option [ selected <| settings.coldLengthUnit == Hours ] [ text <| toString Hours ]
                        , option [ selected <| settings.coldLengthUnit == Days ] [ text <| toString Days ]
                        ]
                    ]
                ]
            ]
        , button [ type' "button", class "btn btn-danger", onClick ClearLocalTodos ] [ text "Clear Local Todos" ]
        ]
