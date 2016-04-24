module Bit where 

import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events exposing (onClick)

type alias Location = 
            { x: Float
            , y: Float
            }

type alias Bit =
            { value: Int
            , location: Location
            , category: String
            }


defaultBit: Int -> Location -> String -> Bit
defaultBit v l c = 
    { value = v
    , location = l
    , category = c
    }

{- The length of the edges of each Bit block -}                 
sizeOfBit: Float
sizeOfBit = 50

type Action = Click 

updateBit: Action -> Bit -> Bit
updateBit action bit = 
    case action of
        Click -> (bitToggle bit)

colorOf : String -> String
colorOf category=
    case category of 
        "data" -> "green"
        "parity" -> "blue"
        _ -> "red"


bitToggle : Bit -> Bit
bitToggle bit = 
    case bit.category of 
        "data" -> {bit | value = (bit.value + 1) % 2}
        _ -> bit

viewBit: Signal.Address Action -> Bit -> Html
viewBit address bit =
    let xOrigin = (toString bit.location.x)
        yOrigin = (toString bit.location.y)
        bitDimension = (toString sizeOfBit)
        fillColor = colorOf(bit.category)
        textOrigin = (toString  (sizeOfBit/2))
    in 
    svg 
        [ onClick address Click, x xOrigin, y yOrigin,width bitDimension, height bitDimension]
        [ 
            rect 
                [ fill fillColor, x "0", y "0", width bitDimension, height bitDimension] 
                []
        ,   text' 
                [x textOrigin, y textOrigin, fill "black", fontFamily "monospace", fontSize textOrigin, textAnchor "middle", alignmentBaseline "middle"]
                [text (toString bit.value)]
        ]