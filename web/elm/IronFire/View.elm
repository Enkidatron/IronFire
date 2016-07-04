module IronFire.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Html.Attributes exposing (..)
import IronFire.Model exposing (..)
import Json.Decode as JD


view : Model -> Html Msg
view model =
    let
        frozen =
            model.status == Frozen

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
            not <| List.any (.phxId >> (==) Nothing) model.todos
    in
        div [ class "container" ]
            [ table [ class "table table-condensced" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Task" ]
                        , th [] [ text "Status" ]
                        , th [] [ text "Times Renewed" ]
                        , th [] [ displayViewFilterButtons model.viewFilter ]
                        , th [] [ button [ type' "button", class "btn btn-primary", onClick SaveAllUnsaved, disabled saveAllDisabled ] [ text "Save All" ] ]
                        ]
                    ]
                , tbody []
                    ((List.map (displayTodo frozen) <| List.filter viewFilter <| List.sortBy .lastTouched model.todos)
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


displayTodo : Bool -> Todo -> Html Msg
displayTodo frozen todo =
    let
        isDisabled =
            frozen && (todo.status /= Cold)

        extraButtons =
            if frozen && todo.status == Cold then
                [ button [ type' "button", class "btn btn-warning", onClick <| RenewTodo todo ] [ text "Renew" ]
                ]
            else
                []

        buttons =
            if isAlive todo then
                div [ class "btn-group" ]
                    ([ button [ type' "button", class "btn btn-info", disabled isDisabled, onClick <| TouchTodo todo ] [ text "I Worked On This" ]
                     , button [ type' "button", class "btn btn-success", disabled isDisabled, onClick <| FinishTodo todo ] [ text "Finish" ]
                     ]
                        ++ extraButtons
                        ++ [ button [ type' "button", class "btn btn-danger", disabled isDisabled, onClick <| KillTodo todo ] [ text "Kill" ]
                           ]
                    )
            else
                div [] []

        saveText =
            case todo.phxId of
                Nothing ->
                    "Unsaved"

                Just _ ->
                    ""
    in
        tr []
            [ td [] [ text todo.text ]
            , td [] [ text <| toString todo.status ]
            , td [] [ text <| toString todo.timesRenewed ]
            , td [] [ buttons ]
            , td [] [ text saveText ]
            ]


displayInputRow : String -> Html Msg
displayInputRow inputText =
    tr []
        [ td []
            [ div [ class "form-group" ]
                [ input [ type' "text", class "form-control", id "task-input", onInput SetInput, placeholder "New Todo", value inputText, onEnter NoOp AddTodo ] []
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
        ]
