import Bit
import Packet 
import Header 
import Html
import Html.Attributes as HA
import Html.App as HApp
import Html.Events as HE
import Json.Decode as J
import String exposing(concat)
import Basics

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model
    , packetModel: Packet.Model 
    , noParityReceivedPacket: Packet.Model
    , noise: Int 
    , bitProbability: Float
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    , noParityReceivedPacket = Packet.defaultPacket
    , bitProbability = 0.01
    , noise = 1
    }

type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateProbability String 

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
        [Html.h4 [] [Html.text "A Bit"]
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
        [ Html.h4 [] [Html.text "A Packet"]
        , Html.p 
                []
                [ Html.text "Now, lets build a packet. It is simply a group of bits. You can change the packet size and toggle the bits to set a value." ]
        , HApp.map PacketMsg (Packet.view model.packetModel)
        ]

errorProbability model =
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Probability of Error"]
        , Html.p 
            []
            [ Html.text "If we transmit the above packet, there is a chance of error in receiving the packet. We shall calculate the probability of correct reception of the packet."
            , Html.br [] [] 
            , Html.text "Suppose the probability of a bit error, P"
            , Html.sub [] [Html.text "e"] 
            , Html.text (" = " ++ (toString model.bitProbability))
            , Html.input 
                [ HA.type' "range"
                , HA.value (toString model.bitProbability) 
                , HA.max "1"
                , HA.min "0"
                , HA.step "0.01"
                , HE.on "change" (J.map UpdateProbability HE.targetValue)
                ]
                []
            , Html.br [][]
            , Html.text ("Then the probability of zero bit error in reception of " ++ (toString model.packetModel.msb) ++ " bit packet is (1 - ")
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString model.packetModel.msb)] 
            , Html.text (" = " ++ (toString ((1- model.bitProbability)^(toFloat model.packetModel.msb))))
            , Html.br [][]
            , Html.text ("and probability of 1 bit error in reception of " ++ (toString model.packetModel.msb) ++ " bit packet is (1 - ")
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-1))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.text (" = " ++ (toString ((1- model.bitProbability)^(toFloat (model.packetModel.msb-1)) * model.bitProbability)))
            ]
        ]

parityIntro model= 
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Single Error Detection"]
        , Html.p 
                []
                [ Html.text "If we transmit the above packet, there are chances that " ]
        , HApp.map PacketMsg (Packet.view model.packetModel)
        , Html.p
            []
            [ Html.text "You can change the packet size and toggle the bits to set a value."     
            ]
        ]

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
            , errorProbability model
            , parityIntro model
            , singleErrorCorrection model
            ]
        ]


update: Msg -> AppModel -> AppModel
update msg model =
    case msg of
        PacketMsg subAction ->
            let 
            updatedPacketModel = Packet.update subAction model.packetModel
            in
                {model | packetModel = updatedPacketModel}
        BitMsg subAction ->
            let 
                updatedBitModel = Bit.update subAction model.bitModel
            in
                {model | bitModel = updatedBitModel}

        UpdateProbability n ->
                {model | bitProbability = (n |> String.toFloat |> Result.toMaybe |> Maybe.withDefault 0.01)}


main = 
    HApp.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }