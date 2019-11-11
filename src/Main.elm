module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as HtmlKeyed
import Json.Decode as Json


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Model


type alias Task =
    { id : Int
    , todo : String
    , completed : Bool
    , editing : Bool
    }


type alias Model =
    { newTodo : String
    , tasks : List Task
    , uid : Int
    }


initialModel : Model
initialModel =
    { newTodo = ""
    , tasks = []
    , uid = 0
    }



-- Update


type Msg
    = InputChanged String
    | CreateTask
    | MarkAllCompleted Bool
    | TaskCompletionToggled Int Bool
    | DeleteTask Int
    | EnterEditingMode Int
    | TaskUpdated Int String
    | SaveUpdatedTask Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newTodo ->
            { model | newTodo = newTodo }

        CreateTask ->
            createNewTodo model

        MarkAllCompleted completed ->
            { model | tasks = List.map (\task -> { task | completed = completed }) model.tasks }

        TaskCompletionToggled id completed ->
            { model | tasks = toggleTaskCompletion model.tasks id completed }

        DeleteTask id ->
            { model | tasks = deleteTask id model.tasks }

        EnterEditingMode id ->
            { model | tasks = setEditingField id True model.tasks }

        TaskUpdated id updatedTask ->
            { model | tasks = updateTask id updatedTask model.tasks }

        SaveUpdatedTask id ->
            { model | tasks = model.tasks |> filterInvalidTasks |> trimTasks |> setEditingField id False }



-- View


view : Model -> H.Html Msg
view model =
    H.section
        [ Attr.class "todoapp" ]
        [ inputElement model
        , renderTodos model
        , renderFooter model
        ]


inputElement : Model -> H.Html Msg
inputElement model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text "todos" ]
        , H.input [ Attr.class "new-todo", Attr.placeholder "What needs to be done?", Attr.autofocus True, Events.onInput InputChanged, onEnter CreateTask, Attr.value model.newTodo ] []
        ]


renderTodos : Model -> H.Html Msg
renderTodos { tasks } =
    let
        inputId =
            "toggle-all"

        thereAreNoTasks =
            List.isEmpty tasks
    in
    if thereAreNoTasks then
        H.text ""

    else
        H.section
            [ Attr.class "main" ]
            [ H.input [ Attr.id inputId, Attr.class inputId, Attr.type_ "checkbox", Events.onCheck MarkAllCompleted ] []
            , H.label [ Attr.for inputId ] [ H.text "Mark all as complete" ]
            , HtmlKeyed.ul [ Attr.class "todo-list" ] (List.map renderTask tasks)
            ]


renderTask : Task -> ( String, H.Html Msg )
renderTask task =
    let
        taskHtml =
            H.li
                [ Attr.class
                    (if task.completed then
                        "completed"

                     else
                        ""
                    )
                , Attr.class
                    (if task.editing then
                        "editing"

                     else
                        ""
                    )
                ]
                [ H.div [ Attr.class "view" ]
                    [ H.input [ Attr.class "toggle", Attr.type_ "checkbox", Attr.checked task.completed, Events.onCheck (TaskCompletionToggled task.id) ] []
                    , H.label [ Events.onDoubleClick (EnterEditingMode task.id) ] [ H.text task.todo ]
                    , H.button [ Attr.class "destroy", Events.onClick (DeleteTask task.id) ] []
                    ]
                , H.input [ Attr.class "edit", Attr.value task.todo, Events.onInput (TaskUpdated task.id), onEnter (SaveUpdatedTask task.id), Events.onBlur (SaveUpdatedTask task.id) ] []
                ]

        key =
            String.fromInt task.id
    in
    ( key, taskHtml )


renderFooter : Model -> H.Html Msg
renderFooter model =
    let
        showFooter =
            not (List.isEmpty model.tasks)
    in
    if showFooter then
        H.footer
            [ Attr.class "footer" ]
            [ renderItemsLeft model.tasks
            ]

    else
        H.text ""


renderItemsLeft : List Task -> H.Html Msg
renderItemsLeft tasks =
    let
        reducer task itemsLeft =
            if task.completed then
                itemsLeft

            else
                itemsLeft + 1

        defaultForItemsLeft =
            0

        itemsLeftCount =
            List.foldl reducer defaultForItemsLeft tasks

        itemsText =
            if itemsLeftCount == 1 then
                "item"

            else
                "items"
    in
    H.span
        [ Attr.class "todo-count" ]
        [ H.strong [] [ H.text (String.fromInt itemsLeftCount) ]
        , H.text (" " ++ itemsText ++ " left")
        ]



-- Helpers


onEnter : Msg -> H.Attribute Msg
onEnter msg =
    let
        enterKeyCode =
            13

        isEnterPressed keyCode =
            if keyCode == enterKeyCode then
                Json.succeed msg

            else
                Json.fail "Not enter"
    in
    Events.on "keyup" (Json.andThen isEnterPressed Events.keyCode)


createNewTodo : Model -> Model
createNewTodo model =
    if isValidTaskString model.newTodo then
        Model "" (model.tasks ++ [ Task model.uid (model.newTodo |> String.trim) False False ]) (model.uid + 1)

    else
        model


isValidTaskString : String -> Bool
isValidTaskString newTodo =
    let
        isGreaterThanZero =
            (<) 0
    in
    newTodo |> String.trim |> String.length |> isGreaterThanZero


toggleTaskCompletion : List Task -> Int -> Bool -> List Task
toggleTaskCompletion tasks targetId completed =
    let
        toggler task =
            if task.id == targetId then
                { task | completed = completed }

            else
                task
    in
    List.map toggler tasks


deleteTask : Int -> List Task -> List Task
deleteTask targetId tasks =
    List.filter (\task -> task.id /= targetId) tasks


setEditingField : Int -> Bool -> List Task -> List Task
setEditingField targetId editing tasks =
    let
        updater task =
            if task.id == targetId then
                { task | editing = editing }

            else
                task
    in
    List.map updater tasks


updateTask : Int -> String -> List Task -> List Task
updateTask targetId updatedTask tasks =
    let
        updater task =
            if targetId == task.id then
                { task | todo = updatedTask }

            else
                task
    in
    List.map updater tasks


filterInvalidTasks : List Task -> List Task
filterInvalidTasks tasks =
    List.filter (\task -> isValidTaskString task.todo) tasks


trimTasks : List Task -> List Task
trimTasks tasks =
    List.map (\task -> { task | todo = String.trim task.todo }) tasks
