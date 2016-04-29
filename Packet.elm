module Packet where

import Bit
import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Html exposing (Html,div,button)
import Html.Events exposing (onClick)
import List
import Text exposing(fromString,concat)
import Graphics.Element exposing(show)
import String exposing(repeat,length)


-- Model
type alias Model = 
    { bits  : List ( ID, Bit.Model )
    , msb   : ID
    }

type alias ID = Int



defaultPacket : Model 
defaultPacket = 
    { bits = [(0, Bit.defaultBit 0 {x = 0, y = 0} "data")]
    , msb = 1
    }

-- Update

type Action 
    = Add
    | Remove 
    | Modify ID Bit.Action

update: Action -> Model -> Model 
update action packet = 
    case action of
        Add -> 
            let newBitPosition = {x = 0, y = 0}
                newBit = (packet.msb, (Bit.defaultBit 0 newBitPosition "data"))
                newBits = [newBit] ++ packet.bits 
            in 
                { packet |
                    bits = newBits
                ,   msb = packet.msb + 1
                }

        Remove ->
            if packet.msb >1 then
                { packet | 
                    bits = List.drop 1 packet.bits
                ,       msb = packet.msb - 1
                }
            else 
                packet

        Modify id bitAction -> 
            let updateSpecificBit (bitID, bit) = 
                    if bitID == id then
                        (bitID, Bit.update bitAction bit)
                    else 
                        (bitID, bit)
            in
                { packet | 
                    bits = List.map updateSpecificBit packet.bits 
                }

packetValue: Model -> List Int
packetValue packet =
    let getBitValue (_, bit) = bit.value 
    in List.map getBitValue packet.bits

dec2bin: Int -> String 
dec2bin v =
    case v of
        1 -> "1"
        0 -> "0"
        _ -> ( dec2bin (v // 2) ) ++ (toString (v % 2))

allPossibleValues : Int -> List String
allPossibleValues n =
    let maxPossibleValue = 2^n - 1
        valueStrings = List.map dec2bin [0..maxPossibleValue]
        padZeroes binaryString = (String.repeat (n - (String.length binaryString)) "0" ) ++ binaryString 
    in List.map padZeroes valueStrings


-- View 
view: Signal.Address Action -> Model -> Html
view address packet =
    let bits = List.map (viewSpecificBit address) packet.bits
        remove  = div [] [button [ onClick address Remove] [text "Remove a bit"]]
        add     = div [] [ button [ onClick address Add] [text "Add a bit"] ]
    in 
        div [] ([add] ++ bits ++ [remove])

viewSpecificBit: Signal.Address Action -> (ID, Bit.Model) -> Html
viewSpecificBit address (id, bit) = 
    Bit.view (Signal.forwardTo address (Modify id)) bit


main = defaultPacket |> packetValue |> List.map toString |> show