import Packet 
import Header 
import Html
import StartApp.Simple exposing(start)

type alias AppModel = 
    { headerModel: Header.Model
    , packetModel: Packet.Model 
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , packetModel = (Packet.emptyPacket)
    }

type Action 
    = PacketAction Packet.Action


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
                [ Html.text "This is a packet\n" ]
            , Packet.view (Signal.forwardTo address PacketAction) model.packetModel
            ]
        ]


update: Action -> AppModel -> AppModel
update action model =
    case action of
        PacketAction subAction ->
            let 
                updatedPacketModel = 
                    Packet.update subAction model.packetModel
            in
                {model | packetModel = updatedPacketModel}

main = 
    start { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }