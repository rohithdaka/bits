module Header where

import Html exposing (Html,div,h1,text)
import Html.Events 

type alias Model = String

initialModel: Model 
initialModel = 
    "Hamming Code - An Interactive Primer"




view: Model -> Html
view model =
    Html.h1 
        []
        [text model]
