module Bits where

import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse 

type alias Bit = Int

{- The length of the edges of each Bit block -}                 
size : Float
size = 40

colorOf : Bit -> Color
colorOf bit=
    case bit of 
        0 -> Color.black
        1 -> Color.white
        _ -> Color.red

{- Given a Bit type, converts it to a Form. -}       
toForm : Bit -> Form
toForm bit = 
    let shape = square size
        border = outlined (solid Color.darkCharcoal) shape 
        bitDisplay = toString bit
                    |> Text.fromString 
                    |> Text.color Color.green
                    |> Text.monospace
                    |> Text.height (0.8*size)
                    |> text 
    in  group [filled (colorOf bit) shape, border, bitDisplay]


bitToggle : Bit -> Bit
bitToggle bit = 
    (bit + 1) % 2


main : Element     
main = collage 400 400 [toForm (bitToggle 0)]
