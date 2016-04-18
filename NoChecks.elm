module NoChecks where

import Bits exposing(..)
import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List
import StartApp.Simple exposing (..)

-- Model
type alias Model = Bit

numberOfBits : Int
numberOfBits = 4

bitPositions: List Int
bitPositions = [1..numberOfBits]


-- Update
type Action = Toggle

update: Action -> Bit -> Bit
update action bit =
    case action of
        Toggle -> (bitToggle bit)


-- View 

view: Signal.Address Action -> Model -> Html
view address model 



clickPositions: Signal
clickPositions = merge Mouse.position Mouse.isDown

type alias Position = (Int, Int)
insideBox: Bit-> Position -> Size -> Bool 
insideBox bit (a,b) size =
    if abs(bit.location.x - a) < size//2


isClicked: Bool -> Signal-> Signal
isClicked  = 
    Signal.filter insideBox clickPositions