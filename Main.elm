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
    , oddeven = 0
    -- 0 for even and 1 for odd
    }

type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateProbability String 
    | ToggleOddEven

singleParity: AppModel -> Int
singleParity model =
    case ((( model.packetModel |> Packet.packetValue |> List.map toString |> concat |> split "0" |> join "" |> length) ) % 2 ) of 
        0 -> model.oddeven
        1 -> ( model.oddeven + 1) % 2 
        _ -> 5 -- This should never happen as the case is determined by dividing with 2 

oddEven: AppModel -> String
oddEven model =
    case model.oddeven of 
        1 -> "Odd"
        0 -> "Even"
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
            [ Html.text "The whole point of building the packet above is the transmit it. Unfortunately, there is a chance that a bit can be flippled/toggled when we transmit."
            , Html.br [] [] 
            , Html.text "Suppose the probability of error if we send just one bit at a time is, P"
            , Html.sub [] [Html.text "e"] 
            , Html.text (", is " ++ (toString model.bitProbability))
            , Html.input 
                [ HA.type' "range"
                , HA.size 100
                , HA.value (toString model.bitProbability) 
                , HA.max "1"
                , HA.min "0"
                , HA.step "0.01"
                , HE.on "change" (J.map UpdateProbability HE.targetValue)
                ]
                []
            , Html.br [][]
            , Html.text ("Then the probability of 0 errors during transmission of " ++ (toString model.packetModel.msb) ++ " bit packet is " )
            , (combinatricsNotation model.packetModel.msb model.packetModel.msb) 
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString model.packetModel.msb)] 
            , Html.text (" which is " ++ (toString ((1- model.bitProbability)^(toFloat model.packetModel.msb) *100)) ++ "%" )
            , Html.br [][]
            , Html.text ("and the probability of 1 error during transmission of " ++ (toString model.packetModel.msb) ++ " bit packet is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-1))
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-1))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.text (" which is " ++ (toString ((1- model.bitProbability)^(toFloat (model.packetModel.msb-1)) * model.bitProbability *100 * (toFloat model.packetModel.msb))) ++ "%" )
            , Html.br [] []
            , Html.text ("and probability of 2 errors during transmission of" ++ (toString model.packetModel.msb) ++ " bit packet is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-2))
            , Html.text " * (1 - "
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
            , Html.text "To understand the importance of upcoming sections, analysing these three probabilities are enough. As an engineer, it is essential to get an intuition of what is happening. So I highly recommend you play around with packet size and P"
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
            [ Html.text "If you have played enough with the model above, you may have noticed some patterns. For small P"
            , Html.sub [] [Html.text "e"]
            , Html.text " < 0.2, vast majority of the transmissions are either zero errors or 1 error. This gives us some hope that if we somehow detect and correct one error during transmission, it will work most of the time. For that, we must first detect single error. So lets look at the most popular and oldest trick to detect a single error: parity check"
            , HApp.map PacketMsg (Packet.view model.packetModel)
            , HApp.map BitMsg (Bit.view (Bit.defaultBit (singleParity model) {x=0,y=0} "parity"))
            , Html.br [] []
            , Html.text ( (oddEven model) ++ " Parity Check ")
            , Html.button [ HE.onClick ToggleOddEven] [Html.text "Change" ]
            , Html.br [] []
            , Html.text "The parity bit is shown in a different color and you can't toggle it. Its value is set based on the type of parity check we use. In Even Parity Check, the total number of 1s in the packet must be even. So the parity bit value is set to ensure that total 1s are even. Similarly, in Odd Parity Check, the parity bit value is set to make sure that total 1s are Odd"
            ]
        , Html.p 
            []
            [Html.text ("When, the receiver gets this packet. It knows that there must be " ++ (oddEven model) ++ " number of 1s in the packet. If not then we have detected an error. ") ]
        ]

singleErrorCorrection model =
    Html.div 
        [] 
        [ Html.h4 [] [Html.text "Single Error Correction"]
        , Html.p 
            []
            [ Html.text "Usually after detecting an error, recievers ask for retransmission. This reduces the number of bits we can send in a given amount of time (usually refered to as throughput). What if there is a way to identify the errorenous bit? We can simply toggle to correct it. So we must find a way to identiy that single error in the packet. In order to do this we need some extra parity bits that help us pin point the location."
            , Html.br [] []
            , Html.br [] []
            , Html.text "Suppose we want to send n bit packet. The number of parity bits, k, in these n bits should be determined such that all possible single bit error cases can mapped to these k bits. The number of actual possibilities are no errors, error at location 1, error at location 2 ... error at location n. So a total of n+1. So all we have to do is to find minimum k such that 2"
            , Html.sup [] [Html.text "k"]
            , Html.text " is greater or equal to n+1."
            ]
        ]

transmissionEfficiency model =
    Html.div [] []

view model =
    Html.div 
        [HA.width 900]
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