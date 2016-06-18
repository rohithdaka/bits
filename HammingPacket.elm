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
    { bits  : List ( ID, Bit.Model )
    , msb: ID
    }

type alias ID = Int

defaultPacket: Model
defaultPacket =  
    { bits = 
        [ (7, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (6, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (5, Bit.defaultBit 0 {x = 0, y = 0} "data")
        , (4, Bit.defaultBit 0 {x = 0, y = 0} "parity")
        , (3, Bit.defaultBit 1 {x = 0, y = 0} "data")
        , (2, Bit.defaultBit 0 {x = 0, y = 0} "parity")
        , (1, Bit.defaultBit 1 {x = 0, y = 0} "parity")]
    , msb = 4
    }


type Msg 
    = AddParity
    | AddData
    | RemoveParity
    | RemoveData
    | ModifyParity ID Bit.Msg
    | ModifyData ID Bit.Msg

update: Msg -> Model -> Model 
update msg packet = 
    case msg of
        AddParity -> 
            let newBitPosition = {x = 0, y = 0}
                newBit = (packet.msb, (Bit.defaultBit 0 newBitPosition "parity"))
                newBits = [newBit] ++ packet.parityBits 
            in 
                { packet |
                    parityBits = newBits
                ,   msb = packet.msb + 1
                }
        AddData ->
            let newBitPosition = {x = 0, y = 0}
                newBit = (packet.msb, (Bit.defaultBit 0 newBitPosition "data"))
                newBits = [newBit] ++ packet.dataBits 
            in 
                { packet |
                    dataBits = newBits
                ,   msb = packet.msb + 1
                }


        RemoveData ->
            if packet.msb >1 then
                { packet | 
                    dataBits = List.drop 1 packet.dataBits
                ,       msb = packet.msb - 1
                }
            else 
                packet

        RemoveParity ->
            if packet.msb >1 then
                { packet | 
                    parityBits = List.drop 1 packet.parityBits
                ,       msb = packet.msb - 1
                }
            else 
                packet


        ModifyData id bitAction -> 
            let updateSpecificBit (bitID, bit) = 
                    if bitID == id then
                        (bitID, Bit.update bitAction bit)
                    else 
                        (bitID, bit)
            in
                { packet | 
                    dataBits = List.map updateSpecificBit packet.dataBits 
                }

        ModifyParity id bitAction -> 
            let updateSpecificBit (bitID, bit) = 
                    if bitID == id then
                        (bitID, Bit.update bitAction bit)
                    else 
                        (bitID, bit)
            in
                { packet | 
                    parityBits = List.map updateSpecificBit packet.parityBits 
                }





view packet =
    let dataBits = List.map viewSpecificBit packet.dataBits
        parityBits = List.map viewSpecificBit packet.parityBits
        removeData  = div [] [button [ onClick RemoveData] [text "data--"]]
        addData     = div [] [ button [ onClick AddData] [text "data++"] ]
        removeParity  = div [] [button [ onClick RemoveParity] [text "parity--"]]
        addParity     = div [] [ button [ onClick AddParity] [text "parity++"] ]
    in 
        div 
            []
            (   [addParity] ++
                parityBits ++ 
                [removeParity] ++
                [addData] ++
                dataBits ++
                [removeData]
            )

viewSpecificBit: (ID, Bit.Model) -> Html Msg
viewSpecificBit (id, bit) = 
    case bit.category of
        "data" ->  HA.map (ModifyData id) (Bit.view  bit)
        "parity" -> HA.map (ModifyParity id) (Bit.view  bit)
        _ -> HA.map (ModifyParity id) (Bit.view  bit)


main = 
    HA.beginnerProgram { 
        model = defaultPacket
    ,   view = view
    ,   update = update 
    }