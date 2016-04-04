module Bits where

import Basics exposing (..)
import Color exposing (Color)
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Bit ={   value : Int
                ,   color : Color
                }

{- The length of the edges of each block -}                 
size : Float
size = 25

{- Given a Block, converts it to a Form. -}       
toForm : Bit -> Form
toForm bit = 
    let shape = square size
        border = outlined (solid Color.gray) shape
        bitDisplay = text (fromString (toString bit.value))
    in  group [filled bit.color shape, border, bitDisplay]

main : Element     
main = collage 400 400 [toForm (Bit 0 Color.red)]
