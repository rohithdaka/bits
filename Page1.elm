import Bit 
import Header 
import Html
import StartApp.Simple exposing(start)

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model 
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = (Bit.defaultBit 0 {x=0,y=0} "data")
    }

type Action 
    = BitAction Bit.Action


view: Signal.Address Action -> AppModel -> Html.Html
view address model =
    Html.div []
        [ Html.header 
            [] 
            [Header.view model.headerModel]
        , Html.body
            []
            [ Html.p 
                []
                [ Html.text "This is a bit\n" ]
            , Bit.view (Signal.forwardTo address BitAction) model.bitModel
            ]
        ]


update: Action -> AppModel -> AppModel
update action model =
    case action of
        BitAction subAction ->
            let 
                updatedBitModel = 
                    Bit.update subAction model.bitModel
            in
                {model | bitModel = updatedBitModel}

main = 
    start { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }