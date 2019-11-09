module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newTodo ->
            { model | newTodo = newTodo }

        KeyPressed keyCode ->
            createNewTodo keyCode model



-- View


view : Model -> H.Html Msg
view model =
    H.section
        [ Attr.class "todoapp" ]
        [ inputElement model ]


inputElement : Model -> H.Html Msg
inputElement model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text "todos" ]
        , H.input [ Attr.class "new-todo", Attr.placeholder "What needs to be done?", Attr.autofocus True, Events.onInput InputChanged, onKeyUp KeyPressed, Attr.value model.newTodo ] []
        ]



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
    if keyCode == enterKeyCode then
        Model "" (Task model.newTodo False :: model.tasks)

    else
        model
