import Bit
import Packet 
import Header 
import Html
import Html.App as HA
import String exposing(concat)

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model
    , packetModel: Packet.Model 
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    }

type Msg 
    = PacketAction Packet.Msg

tutorialIntroText =
    Html.div []
        [ Html.p 
            []
            [ Html.text "This essay a quick introduction to the concept of Error Correcting Codes. As you may already know, all data in computers is stored and transmitted in binary format, a set of 0s and 1s. For efficient storage and transmission mechanisms, the systems need to know a way to detect errors and correct them if necessary. "
            ]
        , Html.p
            []
            [ Html.text "In 1950, Richard Hamming introduced the concept of Error Detecting and Error Correcting mechanism. This essay is inspired by his original paper and written to serve as a preliminary guide to understand various concepts in this field. No prior knowledge of anykind is assumed. "
            ]
        ]


view model =
    Html.div []
        [ Html.header 
            [] 
            [Header.view model.headerModel]
        , Html.body
            []
            [ tutorialIntroText 
            , Html.p 
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