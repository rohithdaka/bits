module Packet where

import Bit
import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Html exposing (Html,div,button)
import Html.Events exposing (onClick)
import List


-- Model
type alias Model = 
    { bits  : List ( ID, Bit.Model )
    , msb   : ID
    }

type alias ID = Int

emptyPacket : Model 
emptyPacket = 
    { bits = []
    , msb = 0
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
            { packet | 
                bits = List.drop 1 packet.bits
            ,   msb = packet.msb - 1
            }

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
