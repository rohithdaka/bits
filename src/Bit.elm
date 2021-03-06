module Bit exposing (..) -- where
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick,onMouseEnter,onMouseLeave)
import Browser
import String

type alias Model =
            { value: Int
            , position: Int
            , category: String
            , highlight: Bool
            }


defaultBit: Int -> Int -> String -> Bool -> Model
defaultBit v p c h = 
    { value = v
    , position = p
    , category = c
    , highlight = h
    }

initialModel = 
    defaultBit 1 1 "data" False

-- The length of the edges of each Bit block 

sizeOfBit: Float
sizeOfBit = 60

type Msg 
    = Click   
    | MouseHover
    | ParityMouseClick

bitHighlighter: Model -> Msg -> Model
bitHighlighter bit msg =
    case msg of 
        MouseHover -> { bit | highlight = (not bit.highlight) }
        _ -> bit

update: Msg -> Model -> Model
update msg bit = 
    case msg of
        Click -> (bitToggle bit)
        MouseHover -> (bitHighlighter bit msg)
        ParityMouseClick -> bit


colorOf : String -> String
colorOf category=
    case category of 
        "data" -> "green"
        "parity" -> "blue"
        "empty" -> "black"
        _ -> "red"

clickType : String -> Msg
clickType category=
    case category of 
        "data" -> Click
        "parity" -> ParityMouseClick
        _ -> MouseHover

bitToggle : Model -> Model
bitToggle bit = {bit | value = Basics.modBy 2 (bit.value + 1)}


highlightValue bit = 
    case bit.highlight of
        True -> "1"
        False -> "0.5"

view bit =
    let xOrigin = (String.fromInt 0)
        yOrigin = (String.fromInt 0)
        bitDimension = (String.fromFloat sizeOfBit)
        fillColor = (colorOf bit.category)
        textOrigin = (String.fromFloat  (sizeOfBit/2))
        idOrigin = (String.fromFloat (sizeOfBit/6))
        clickEvent = clickType bit.category
    in 
    Svg.svg 
        [ onClick clickEvent, onMouseEnter MouseHover, onMouseLeave MouseHover, x "0", y "0",width bitDimension, height bitDimension]
        [ 
            rect
                [ fill fillColor, x xOrigin, y yOrigin, width bitDimension, height bitDimension, fillOpacity (highlightValue bit)] 
                []
        ,   text_
                [x textOrigin, y textOrigin, fill "black", fontFamily "monospace", fontSize textOrigin, textAnchor "middle", alignmentBaseline "middle"]
                [Html.text (String.fromInt bit.value)]
        ,   text_
                [x idOrigin, y idOrigin, fill "black", fontFamily "monospace", fontSize idOrigin, textAnchor "left", alignmentBaseline "top"]
                [Html.text (String.fromInt bit.position)]
        ]

main = 
    Browser.sandbox { 
        init = initialModel
    ,   view = view
    ,   update = update 
    }