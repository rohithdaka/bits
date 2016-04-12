module Bits where

import Basics exposing (..)
import Color exposing (Color)
-- Not exposing Text functions as they may conflict with Graphics functions
import Text
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Transform2D exposing (translation)
import Mouse 

type alias Location = 
            { x: Int
            , y: Int
            }

type alias Bit =
            { value: Int
            , location: Location
            , category: String
            }

{- The length of the edges of each Bit block -}                 
size : Float
size = 30

colorOf : String -> Color
colorOf category=
    case category of 
        "data" -> Color.green
        "parity" -> Color.blue
        _ -> Color.red

bitGenerator : Int -> Location -> String -> Bit
bitGenerator v l c= 
    {location = l,value = v, category = c}

{- Given a Bit type, converts it to a Form. -}       
convertToForm : Bit -> Form
convertToForm {value, location, category} = 
    let x = toFloat(location.x) * size
        y = toFloat(location.y) * size
        shape = square size
        border = outlined (solid Color.darkCharcoal) shape 
        bitDisplay = toString value
                    |> Text.fromString 
                    |> Text.color Color.black
                    |> Text.monospace
                    |> Text.height (0.8*size)
                    |> centered 
                    |> toForm
    in  [filled (colorOf category) shape, border, bitDisplay] 
        |> groupTransform (translation x y)


bitToggle : Bit -> Bit
bitToggle bit = 
    {bit | value = (bit.value + 1) % 2}


main : Element     
main = collage 500 500 [convertToForm {location = {x =0, y=-1}, value = 0, category="adf"}]