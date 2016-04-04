module Bits where

import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Bit ={   value : Int
                ,   color : Color
                }

{- The length of the edges of each Bit block -}                 
size : Float
size = 40

{- Given a Bit type, converts it to a Form. -}       
toForm : Bit -> Form
toForm bit = 
    let shape = square size
        border = outlined (solid Color.darkCharcoal) shape
        bitDisplay = toString bit.value 
                    |> Text.fromString 
                    |> Text.color Color.green
                    |> Text.monospace
                    |> Text.height (0.8*size)
                    |> text 
    in  group [filled bit.color shape, border, bitDisplay]

main : Element     
main = collage 400 400 [toForm (Bit 0 Color.black)]
