module Packet where

import Bit exposing (..)
import Basics exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Html exposing (Html,div)
import Html.Events exposing (onClick)
import List

import StartApp.Simple as StartApp

-- Model
type alias Packet = List ( ID, Bit )
type alias ID = Int

type Action 
    = Add Bit
    | Remove Bit
    | Modify Bit

packet: numberOfBits -> Packet -> Packet


-- Update


-- View 
packetView address