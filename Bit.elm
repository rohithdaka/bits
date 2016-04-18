module Bit where 

import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html,div)
import Html.Events exposing (onClick)

import Color exposing (..)
import StartApp.Simple as StartApp

type alias Location = 
            { x: Float
            , y: Float
            }

type alias Bit =
            { value: Int
            , location: Location
            , size: Float
            , category: String
            }

{- The length of the edges of each Bit block -}                 

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

viewBit address bit =
    let xOrigin = (toString bit.location.x)
        yOrigin = (toString bit.location.y)
        bitDimension = (toString bit.size)
        fillColor = colorOf(bit.category)
        xTextOrigin = (toString (bit.location.x + bit.size/2))
    in 
    svg 
        [ onClick address Click, x xOrigin, y yOrigin,width bitDimension, height bitDimension]
        [ 
            rect 
                [ fill fillColor, x "0", y "0", width bitDimension, height bitDimension] 
                []
        ,   text' 
                [x "50", y "50", fill "black", fontFamily "monospace", fontSize "60", textAnchor "middle", alignmentBaseline "middle"]
                [text (toString bit.value)]
        ]

defaultBit = { value = 1
            , location = {x=0,y=0}
            , size = 100
            , category ="data"
            }

main = StartApp.start { model = defaultBit, view = viewBit, update = updateBit }