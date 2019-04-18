module Main exposing (AppModel, Msg(..), initialModel, main, update, view)

import Bit
import Header
import Html
import Html.App as HA


type alias AppModel =
    { headerModel : Header.Model
    , bitModel : Bit.Model
    }


initialModel : AppModel
initialModel =
    { headerModel = Header.initialModel
    , bitModel = Bit.defaultBit 0 { x = 0, y = 0 } "data"
    }


type Msg
    = BitMsg Bit.Msg


view : AppModel -> Html.Html Msg
view model =
    Html.div []
        [ Html.header
            []
            [ Header.view model.headerModel ]
        , Html.body
            []
            [ Html.p
                []
                [ Html.text "This is a bit\n" ]
            , HA.map BitMsg (Bit.view model.bitModel)
            ]
        ]


update : Msg -> AppModel -> AppModel
update msg model =
    case msg of
        BitMsg subAction ->
            let
                updatedBitModel =
                    Bit.update subAction model.bitModel
            in
            { model | bitModel = updatedBitModel }


main =
    HA.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
