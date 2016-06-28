module Packet exposing (..) --where

import Bit 
import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Html exposing (Html,div,button)
import Html.Events exposing (onClick)
import List
import String exposing(repeat,length)
import Html.App as HA


-- Model
type alias Model = 
    { bits  : List Bit.Model
    , msb   : ID
    }

type alias ID = Int



defaultPacket : Model 
defaultPacket = 
    { bits = 
        [ Bit.defaultBit 0 4 "data" False
        , Bit.defaultBit 1 3 "data" False
        , Bit.defaultBit 0 2 "data" False
        , Bit.defaultBit 1 1 "data" False
        ]
    , msb = 5
    }


-- Update

type Msg 
    = Add
    | Remove 
    | Modify ID Bit.Msg

update: Msg -> Model -> Model 
update msg packet = 
    case msg of
        Add -> 
            let newBitPosition = packet.msb 
                newBit = (Bit.defaultBit 0 newBitPosition "data" False)
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
            let updateSpecificBit bit = 
                    if bit.position == id then
                        Bit.update bitAction bit
                    else 
                        bit
            in
                { packet | 
                    bits = List.map updateSpecificBit packet.bits 
                }

packetValue: Model -> List Int
packetValue packet =
    let getBitValue bit = bit.value 
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
view packet =
    let bits = List.map viewSpecificBit packet.bits
        remove  = button [ onClick Remove] [text "Remove a bit"]
        add     = button [ onClick Add] [text "Add a bit"]
    in 
        div [] 
            ([remove] ++ [add] ++ [div [] bits])


viewSpecificBit: Bit.Model -> Html Msg
viewSpecificBit bit = 
    HA.map (Modify bit.position) (Bit.view  bit)
