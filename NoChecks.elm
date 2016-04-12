module NoChecks where

import Bits exposing(..)
import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List

-- Model
type alias Model = Bit

numberOfBits : Int
numberOfBits = 4

bitPositions: List Int
bitPositions = [1..numberOfBits]

transmittedMessage: Model
transmittedMessage=
    List.map bitGenerator bitPositions

-- Update
type Action = Toggle

update: Action -> Bit -> Bit
update action bit =
    case action of
        Toggle -> (bitToggle bit)


-- View 






