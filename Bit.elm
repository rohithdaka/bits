module Bit exposing (..) -- where
import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as HA

type alias Location = 
            { x: Float
            , y: Float
            }

type alias Model =
            { value: Int
            , location: Location
            , category: String
            }


defaultBit: Int -> Location -> String -> Model
defaultBit v l c = 
    { value = v
    , location = l
    , category = c
    }

initialModel = 
    (defaultBit 1 {x=0,y=0} "data")

-- The length of the edges of each Bit block 

sizeOfBit: Float
sizeOfBit = 50

type Msg = Click 

update: Msg -> Model -> Model
update msg bit = 
    case msg of
        Click -> (bitToggle bit)

colorOf : String -> String
colorOf category=
    case category of 
        "data" -> "green"
        "parity" -> "blue"
        "empty" -> "white"
        _ -> "red"


bitToggle : Model -> Model
bitToggle bit = 
    case bit.category of 
        "data" -> {bit | value = (bit.value + 1) % 2}
        _ -> bit

view bit =
    let xOrigin = (toString bit.location.x)
        yOrigin = (toString bit.location.y)
        bitDimension = (toString sizeOfBit)
        fillColor = colorOf(bit.category)
        textOrigin = (toString  (sizeOfBit/2))
    in 
    Svg.svg 
        [ onClick Click, x xOrigin, y yOrigin,width bitDimension, height bitDimension]
        [ 
            rect 
                [ fill fillColor, x "0", y "0", width bitDimension, height bitDimension] 
                []
        ,   text' 
                [x textOrigin, y textOrigin, fill "black", fontFamily "monospace", fontSize textOrigin, textAnchor "middle", alignmentBaseline "middle"]
                [Html.text (toString bit.value)]
        ]

main = 
    HA.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }