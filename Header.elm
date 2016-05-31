module Header exposing (..) --where

import Html exposing (Html,div,h1,text)
import Html.Events 

type alias Model = String

initialModel: Model 
initialModel = 
    "Error Correction and Error Detection - An Interactive Primer"


view model =
    Html.h1 
        []
        [text model]
