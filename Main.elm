import Bit
import Packet 
import Header 
import Html
import Html.Attributes as HA
import Html.App as HApp
import Html.Events as HE
import Json.Decode as J
import String exposing(concat)

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model
    , packetModel: Packet.Model 
    , noise: Int
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    , noise = 1
    }

type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateNoise String 

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

bitIntro model =
    Html.div
        []
        [Html.h3 [] [Html.text "A Bit"]
        , Html.p 
                []
                [ Html.text "Let's start with the fundamental unit of communication: Binary Digit or a Bit" ]
        , HApp.map BitMsg (Bit.view model.bitModel)
        , Html.p 
            []
            [ Html.text "You can click on the bit to toggle its value." ]
        ]

packetIntro model =
    Html.div
        []
        [ Html.h3 [] [Html.text "A Packet"]
        , Html.p 
                []
                [ Html.text "Now, lets build a packet. It is simply a group of bits. " ]
        , HApp.map PacketMsg (Packet.view model.packetModel)
        , Html.p
            []
            [ Html.text "Change the packet to whatever size you want and toggle the bits to set the packet value." ]
        , Html.p
            []
            [Html.text "Current value is: "    
            , Html.span 
                []
                [ model.packetModel |> Packet.packetValue |> List.map toString |> concat |> Html.text]
            ]
        ]

transmitPacket model =
    Html.div 
        [] 
        [ Html.h3 [] [Html.text "Transmitting this Packet "]
        , Html.p 
            []
            [Html.text "The packet you just formed is transmitted to another machine. For the purpose of this tutorial, lets just assume that the noise of the channel can be represented by this number: "
            , Html.text (toString model.noise)
            , Html.input 
                [ HA.type' "range"
                , HA.value (toString model.noise)
                , HA.max "10"
                , HA.min "0"
                , HA.step "1"
                , HE.on "input" (J.map UpdateNoise HE.targetValue)
                ]
                []
            ]
        ]

parityIntro model= 
    Html.div [] []

singleErrorCorrection model =
    Html.div [] []

view model =
    Html.div []
        [ Html.header 
            [] 
            [Header.view model.headerModel]
        , Html.body
            []
            [ tutorialIntroText
            , bitIntro model 
            , packetIntro model
            , transmitPacket model
            , parityIntro model
            , singleErrorCorrection model
            ]
        ]


update: Msg -> AppModel -> AppModel 
update msg model =
    case msg of
        PacketMsg subAction ->
            let 
            updatedPacketModel = 
                Packet.update subAction model.packetModel
            in
                {model | packetModel = updatedPacketModel}
        BitMsg subAction ->
            let 
                updatedBitModel = 
                    Bit.update subAction model.bitModel
            in
                {model | bitModel = updatedBitModel}

        UpdateNoise n ->
                {model | noise = (n |> String.toInt |> Result.toMaybe |> Maybe.withDefault 0)}


main = 
    HApp.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }