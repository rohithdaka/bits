module Header exposing (..) --where

import Html exposing (Html,div,h1,text)
import Html.Events 

type alias Model = String

initialModel: Model 
initialModel = 
    "Error Detection and Error Correction - A Quick Interactive Primer"


view model =
    Html.h1 
        []
        [text model]
