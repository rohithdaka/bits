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
    {bit | value = (bit.value + 1) % 2}


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

defaultBit = { value = 0
            , location = {x=0,y=0}
            , size = 100
            , category ="data"
            }

main = StartApp.start { model = defaultBit, view = viewBit, update = updateBit }

{-
main: Html
main = 
    svg 
        [ version "1.1", width "100", height "100"]
                  [x "50", y "50", fill "black", fontFamily "monospace", fontSize "60", textAnchor "middle", alignmentBaseline "middle"]
      [
            svg [ x "0", y "0", width "50", height "50"]
                [ 
                    rect 
                        [ fill "green", x "0", y "0", width "50", height "50"] 
                        []
                ,   text' 
                       [x "25", y "25", fill "black", fontFamily "monospace", fontSize "60", textAnchor "middle", alignmentBaseline "middle"]
                       [text "1"]
                ]
        ,   svg [ x "50", y "50", width "50", height "50"]
                [ 
                    rect 
                        [ fill "blue", x "0", y "0", width "50", height "50"] 
                        []
                ,   text' 
                        [x "25", y "25", fill "black", fontFamily "monospace", fontSize "60", textAnchor "middle", alignmentBaseline "middle"]
                        [text "0"]
                ]
        ]
-}
