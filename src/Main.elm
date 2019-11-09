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
    }


type alias Model =
    { newTodo : String
    , tasks : List Task
    }


initialModel : Model
initialModel =
    { newTodo = ""
    , tasks = []
    }



-- Update


type Msg
    = InputChanged String
    | KeyPressed Int
    | MarkAllCompleted Bool
    | TaskCompletionToggled Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newTodo ->
            { model | newTodo = newTodo }

        KeyPressed keyCode ->
            createNewTodo keyCode model

        MarkAllCompleted completed ->
            { model | tasks = List.map (\task -> { task | completed = completed }) model.tasks }

        TaskCompletionToggled index completed ->
            { model | tasks = toggleTaskCompletion model.tasks index completed }



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
            , HtmlKeyed.ul [ Attr.class "todo-list" ] (List.indexedMap renderTask tasks)
            ]


renderTask : Int -> Task -> ( String, H.Html Msg )
renderTask index task =
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
                    [ H.input [ Attr.class "toggle", Attr.type_ "checkbox", Attr.checked task.completed, Events.onCheck (TaskCompletionToggled index) ] []
                    , H.label [] [ H.text task.todo ]
                    ]
                ]

        key =
            String.fromInt index
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
        Model "" (model.tasks ++ [ Task (model.newTodo |> String.trim) False ])

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
toggleTaskCompletion tasks targetIndex completed =
    let
        toggler index task =
            if index == targetIndex then
                { task | completed = completed }

            else
                task
    in
    List.indexedMap toggler tasks
