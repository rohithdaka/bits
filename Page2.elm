import Packet 
import Header 
import Html
import Html.App as HA
import String exposing(concat)

type alias AppModel = 
    { headerModel: Header.Model
    , packetModel: Packet.Model 
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , packetModel = (Packet.defaultPacket)
    }

type Msg 
    = PacketAction Packet.Msg


view model =
    Html.div []
        [ Html.header 
            [] 
            [Header.view model.headerModel]
        , Html.body
            []
            [ Html.p 
                []
                [ Html.text "This is a packet\n" ]
            , HA.map PacketAction (Packet.view model.packetModel)
            , Html.p 
                []
                [ model.packetModel |> Packet.packetValue |> List.map toString |> concat |> Html.text]
            ]
        ]


update: Msg -> AppModel -> AppModel
update msg model =
    case msg of
        PacketAction subAction ->
            let 
                updatedPacketModel = 
                    Packet.update subAction model.packetModel
            in
                {model | packetModel = updatedPacketModel}

main = 
    HA.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }