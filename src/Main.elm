module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as HtmlKeyed
import Json.Decode as Decode


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Model


type alias Task =
    { todo : String
    , completed : Bool
    , id: Int
    }


type alias Model =
    { newTodo : String
    , tasks : List Task
    , uid: Int
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
    | KeyPressed Int
    | MarkAllCompleted Bool
    | TaskCompletionToggled Int Bool
    | DeleteTask Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newTodo ->
            { model | newTodo = newTodo }

        KeyPressed keyCode ->
            createNewTodo keyCode model

        MarkAllCompleted completed ->
            { model | tasks = List.map (\task -> { task | completed = completed }) model.tasks }

        TaskCompletionToggled id completed ->
            { model | tasks = toggleTaskCompletion model.tasks id completed }

        DeleteTask id ->
            { model | tasks = deleteTask id model.tasks}



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
        , H.input [ Attr.class "new-todo", Attr.placeholder "What needs to be done?", Attr.autofocus True, Events.onInput InputChanged, onKeyUp KeyPressed, Attr.value model.newTodo ] []
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
                ]
                [ H.div [ Attr.class "view" ]
                    [ H.input [ Attr.class "toggle", Attr.type_ "checkbox", Attr.checked task.completed, Events.onCheck (TaskCompletionToggled task.id) ] []
                    , H.label [] [ H.text task.todo ]
                    , H.button [Attr.class "destroy", Events.onClick (DeleteTask task.id)] []
                    ]
                ]

        key =
            String.fromInt task.id
    in
    ( key, taskHtml )



-- Helpers


onKeyUp : (Int -> msg) -> H.Attribute msg
onKeyUp tagger =
    Events.on "keyup" (Decode.map tagger Events.keyCode)


createNewTodo : Int -> Model -> Model
createNewTodo keyCode model =
    let
        enterKeyCode =
            13
    in
    if keyCode == enterKeyCode && isValidTaskString model.newTodo then
        Model "" (model.tasks ++ [ Task (model.newTodo |> String.trim) False model.uid ]) (model.uid + 1)

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


deleteTask: Int -> List Task -> List Task
deleteTask targetId tasks =
    List.filter (\task -> task.id /= targetId) tasks
