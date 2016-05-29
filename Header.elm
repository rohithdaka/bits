module Header exposing (..)

import Html exposing (Html,div,h1,text)
import Html.Events 

type alias Model = String

initialModel: Model 
initialModel = 
    "Hamming Code - An Interactive Primer"


view model =
    Html.h1 
        []
        [text model]