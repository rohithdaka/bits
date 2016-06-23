module HammingPacket exposing (..) -- where

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
    , n: Int
    , k : Int
    }


defaultPacket: Model
defaultPacket =  
    { bits = 
        [ Bit.defaultBit 0 7 "data"
        , Bit.defaultBit 0 6 "data"
        , Bit.defaultBit 0 5 "data"
        , Bit.defaultBit (hammingParityValue 4 defaultPacket) 4 "parity"
        , Bit.defaultBit 1 3 "data"
        , Bit.defaultBit (hammingParityValue 2 defaultPacket) 2 "parity"
        , Bit.defaultBit (hammingParityValue 1 defaultPacket) 1 "parity"]
    , n = 8
    , k = 3
    }


hammingParityValue: Int -> Model -> Int
hammingParityValue x packet =
    x

type Msg 
    = AddBit 
    | RemoveBit
    | ModifyBit Int Bit.Msg

update: Msg -> Model -> Model 
update msg packet = 
    case msg of
        AddBit -> 
            let newBitPosition = packet.n
                newBit = 
                    if 2^(round (logBase 2 (toFloat packet.n))) == packet.n then 
                        Bit.defaultBit (hammingParityValue packet.n packet) newBitPosition "parity"
                    else
                         Bit.defaultBit 0 newBitPosition "data"

                newBits = [newBit] ++ packet.bits 
                nPlusPlus = packet.n + 1
            in 
                { packet |
                    bits = newBits
                ,   n = nPlusPlus
                ,   k = ceiling (logBase 2 (toFloat nPlusPlus))
                }
    
        RemoveBit ->
            let nMinusMinus = packet.n - 1
            in 
                if packet.n > 1 then
                    { packet | 
                        bits = List.drop 1 packet.bits
                    ,   n = nMinusMinus
                    ,   k = ceiling (logBase 2 (toFloat nMinusMinus))
                    }
                else 
                    packet

        ModifyBit id bitAction -> 
            let updateSpecificBit bit = 
                    if bit.position == id then
                        Bit.update bitAction bit
                    else 
                        bit
            in
                { packet | 
                    bits = List.map updateSpecificBit packet.bits 
                }


view packet =
    let bits = List.map viewSpecificBit packet.bits
        removeBit  = button [ onClick RemoveBit] [text "-"]
        addBit     = button [ onClick AddBit] [text "+"]
    in 
        div 
            []
            (   [removeBit] ++ 
                [Html.text ("n = " ++ (toString (packet.n-1)))] ++
                [addBit] ++ 
                [Html.br [] []] ++
                bits 
            )

viewSpecificBit: Bit.Model -> Html Msg
viewSpecificBit bit = 
    case bit.category of
        "data" ->  HA.map (ModifyBit bit.position) (Bit.view  bit)
        "parity" -> HA.map (ModifyBit bit.position) (Bit.view  bit)
        _ -> HA.map (ModifyBit bit.position) (Bit.view  bit)


main = 
    HA.beginnerProgram { 
        model = defaultPacket
    ,   view = view
    ,   update = update 
    }