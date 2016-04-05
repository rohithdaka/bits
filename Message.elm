module Message where

import Bits exposing(..)
import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List

type alias Message = List Bit

numberOfBits : Int
numberOfBits = 4

bitPositions: List Int
bitPositions = [1..numberOfBits]

messageGenerator : Int -> Bit
messageGenerator = 
    \n -> {location = {x =n, y=0},value = 1}

transmittedMessage: Message
transmittedMessage=
    List.map messageGenerator bitPositions



main : Element     
main = collage 900 900 (List.map convertToForm transmittedMessage)



