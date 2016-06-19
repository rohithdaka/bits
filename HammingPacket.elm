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
    { bits  : List ( Int, Bit.Model )
    , n: Int
    , k : Int
    }


defaultPacket: Model
defaultPacket =  
    { bits = 
        [ (7, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (6, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (5, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (4, Bit.defaultBit (hammingParityValue 4 defaultPacket) {x = 0, y = 0} "parity")
        , (3, Bit.defaultBit 1 {x = 0, y = 0} "data")
        , (2, Bit.defaultBit (hammingParityValue 2 defaultPacket) {x = 0, y = 0} "parity")
        , (1, Bit.defaultBit (hammingParityValue 1 defaultPacket) {x = 0, y = 0} "parity")]
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
            let newBitPosition = {x = 0, y = 0}
                newBit = 
                    if 2^(round (logBase 2 (toFloat packet.n))) == packet.n then 
                        (packet.n, (Bit.defaultBit (hammingParityValue packet.n packet) newBitPosition "parity"))
                    else
                         (packet.n, (Bit.defaultBit 0 newBitPosition "data"))

                newBits = [newBit] ++ packet.bits 
            in 
                { packet |
                    bits = newBits
                ,   n = packet.n + 1
                ,   k = ceiling (logBase 2 (toFloat packet.n))
                }
    
        RemoveBit ->
            if packet.n >1 then
                { packet | 
                    bits = List.drop 1 packet.bits
                ,   n = packet.n - 1
                ,   k = ceiling (logBase 2 (toFloat packet.n))
                }
            else 
                packet

        ModifyBit id bitAction -> 
            let updateSpecificBit (bitID, bit) = 
                    if bitID == id then
                        (bitID, Bit.update bitAction bit)
                    else 
                        (bitID, bit)
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
            (   [addBit] ++ 
                [Html.text ("n = " ++ (toString (packet.n-1)))] ++
                [removeBit] ++ 
                [Html.br [] []] ++
                bits 
            )

viewSpecificBit: (Int, Bit.Model) -> Html Msg
viewSpecificBit (id, bit) = 
    case bit.category of
        "data" ->  HA.map (ModifyBit id) (Bit.view  bit)
        "parity" -> HA.map (ModifyBit id) (Bit.view  bit)
        _ -> HA.map (ModifyBit id) (Bit.view  bit)


main = 
    HA.beginnerProgram { 
        model = defaultPacket
    ,   view = view
    ,   update = update 
    }