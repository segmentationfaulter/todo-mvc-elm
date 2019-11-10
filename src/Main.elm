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
    | ToggleEditingMode Int


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

        ToggleEditingMode id ->
            { model | tasks = toggleEditingState id model.tasks }



-- View


view : Model -> H.Html Msg
view model =
    H.section
        [ Attr.class "todoapp" ]
        [ inputElement model
        , renderTodos model
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
                    , H.label [ Events.onDoubleClick (ToggleEditingMode task.id) ] [ H.text task.todo ]
                    , H.button [ Attr.class "destroy", Events.onClick (DeleteTask task.id) ] []
                    ],
                  H.input [Attr.class "edit", Attr.value task.todo] []
                ]

        key =
            String.fromInt task.id
    in
    ( key, taskHtml )



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


toggleEditingState : Int -> List Task -> List Task
toggleEditingState targetId tasks =
    let
        toggler task =
            if task.id == targetId then
                { task | editing = not task.editing }

            else
                task
    in
    List.map toggler tasks
