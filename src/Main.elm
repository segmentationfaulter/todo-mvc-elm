module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { textInput : String
    }


initialModel : Model
initialModel =
    { textInput = ""
    }



-- Update


type Msg
    = InputChanged String


update : Msg -> Model -> Model
update msg model =
    model



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
        , H.input [ Attr.class "new-todo", Attr.placeholder "What needs to be done?", Attr.autofocus True ] []
        ]
