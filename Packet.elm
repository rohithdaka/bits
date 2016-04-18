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


-- Update
type Action = Increment | Decrement 


-- View 
packetView 