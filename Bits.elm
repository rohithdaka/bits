module Bits where

import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse 

type alias Bit =
            { value: Int
            , location: Int
            }

{- The length of the edges of each Bit block -}                 
size : Float
size = 30

colorOf : Int -> Color
colorOf bit=
    case bit of 
        0 -> Color.black
        1 -> Color.white
        _ -> Color.red

{- Given a Bit type, converts it to a Form. -}       
convertToForm : Bit -> Form
convertToForm {value, location} = 
    let shape = square size
        border = outlined (solid Color.darkCharcoal) shape 
        bitDisplay = toString value
                    |> Text.fromString 
                    |> Text.color Color.green
                    |> Text.monospace
                    |> Text.height (0.8*size)
                    |> centered 
                    |> toForm
    in  group [filled (colorOf value) shape, border, bitDisplay]


bitToggle : Bit -> Bit
bitToggle bit = 
    {bit | value = (bit.value + 1) % 2}


main : Element     
main = collage 500 500 [convertToForm (bitToggle (Bit 1 1))]
