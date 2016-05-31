import Bit
import Packet 
import Header 
import Html
import Html.Attributes as HA
import Html.App as HApp
import Html.Events as HE
import Json.Decode as J
import String exposing(concat,split,join,length)
import Basics

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model
    , packetModel: Packet.Model 
    , noParityReceivedPacket: Packet.Model
    , oddeven: Int 
    , bitProbability: Float
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    , noParityReceivedPacket = Packet.defaultPacket
    , bitProbability = 0
    , oddeven = 1
    -- 1 for even and 0 for odd
    }

type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateProbability String 
    | ToggleOddEven

singleParity: AppModel -> Int
singleParity model =
    (( model.packetModel |> Packet.packetValue |> List.map toString |> concat |> split "0" |> join "" |> length) + model.oddeven) % 2 

oddEven: AppModel -> String
oddEven model =
    case model.oddeven of 
        1 -> "Even"
        0 -> "Odd"
        _ -> ""


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

combinatricsNotation x y =
    Html.span
        []
        [ Html.sup [] [Html.text (toString x)]
        , Html.text "C"
        , Html.sub [] [Html.text (toString y)]
        ]

errorProbability model =
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Probability of Error"]
        , Html.p 
            []
            [ Html.text "If we transmit, there is a chance of error while receiving the packet."
            , Html.br [] [] 
            , Html.text "Suppose the probability of individual bit error, P"
            , Html.sub [] [Html.text "e"] 
            , Html.text (", is " ++ (toString model.bitProbability))
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
            , Html.text ("Then the probability of zero bit error in reception of " ++ (toString model.packetModel.msb) ++ " bit packet is " )
            , (combinatricsNotation model.packetModel.msb model.packetModel.msb) 
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString model.packetModel.msb)] 
            , Html.text (" which is " ++ (toString ((1- model.bitProbability)^(toFloat model.packetModel.msb) *100)) ++ "%" )
            , Html.br [][]
            , Html.text ("and probability of 1 bit error in reception of " ++ (toString model.packetModel.msb) ++ " bit packet is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-1))
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-1))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.text (" which is " ++ (toString ((1- model.bitProbability)^(toFloat (model.packetModel.msb-1)) * model.bitProbability *100 * (toFloat model.packetModel.msb))) ++ "%" )
            , Html.br [] []
            , Html.text ("and probability of 2 bit error in reception of " ++ (toString model.packetModel.msb) ++ " bit packet is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-2))
            , Html.text "(1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-2))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.sup [] [Html.text "2"]
            , Html.text " which is "
            , Html.text (toString 
                            ( (1- model.bitProbability)^(toFloat (model.packetModel.msb-2))
                            * (model.bitProbability^2) 
                            * 50 
                            * (toFloat 
                                    ( model.packetModel.msb 
                                    * (model.packetModel.msb - 1)
                                    )
                                )
                            )
                        )
            , Html.text "%"
            , Html.br [] []
            , Html.text "To get a better understanding of the probability of error, I highly recommend you play around with packet size and P"
            , Html.sub [] [Html.text "e"]
            , Html.text "."
            ]
        ]

parityIntro model= 
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Single Error Detection"]
        , Html.p 
            []
            [ Html.text "As you may have noticed from the probability model above, there is a need to at least detect that received packet has errors. Let us look at the most popular and oldest trick to detect a single error: parity check"
            , HApp.map PacketMsg (Packet.view model.packetModel)
            , HApp.map BitMsg (Bit.view (Bit.defaultBit (singleParity model) {x=0,y=0} "parity"))
            , Html.br [] []
            , Html.text ("It is " ++ (oddEven model) ++ " Parity Check now. ")
            , Html.button [ HE.onClick ToggleOddEven] [Html.text "Switch" ]
            , Html.br [] []
            , Html.text "The parity bit is shown in a different color. In Odd Parity Check, if number of 1s in the packet are odd, then parity bit is set to 1. Not surprisingly, in Even Parity Check, parity bit is set to 1 if number of 1s in the packet are even."
            ]
        , Html.p 
            []
            [Html.text "At the receiver, this parity check is helpful to detect error. This parity check is useful to detect errors if they occur in odd number. None of the even number of errors can be detected with single parity bit check method. Morever, We can't correct the errors that we can detect. In the next section, we shall discuss the fundamentals of building a packet with single error correction capabilities."]
        ]

singleErrorCorrection model =
    Html.div [] []

transmissionEfficiency model =
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
            , transmissionEfficiency model
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

        ToggleOddEven ->
            {model | oddeven = (model.oddeven + 1 ) % 2 }


main = 
    HApp.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }