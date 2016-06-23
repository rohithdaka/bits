import Bit
import Packet 
import HammingPacket
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
    , hammingModel: HammingPacket.Model
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    , noParityReceivedPacket = Packet.defaultPacket
    , bitProbability = 0
    , oddeven = 0
    , hammingModel = HammingPacket.defaultPacket
    -- 0 for even and 1 for odd
    }



type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateProbability String 
    | ToggleOddEven
    | HammingMsg HammingPacket.Msg

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
            [ Html.text "This explorable essay is a quick introduction to the concept of Error Correcting Codes. As you may already know, all information in computers is stored and exchanged with other computers in binary format, a set of 0s and 1s. For prolonged storage and to efficiently exchange this information, we had to invent techniques that can increase the reliability of the information. "
            ]
        , Html.p
            []
            [ Html.text "In April 1950, Richard Hamming introduced one such technique: Error Detecting and Error Correcting Codes. This essay is inspired by his original paper and written to serve as a preliminary guide to understand some concepts in this paper. No prior knowledge of anykind is assumed on the part of the reader. This essay intends to reason about the decisions that make this technique work. The models that are presented for you to explore, hopefully, will provide necessary insights to grasp the concepts. However, this essay doesnt deal with implementation details of this technique."
            ]
        ]

bitIntro model =
    Html.div
        []
        [Html.h4 [] [Html.text "A Bit"]
        , Html.p 
                []
                [ Html.text "Let's start with the fundamental unit of communication: Binary Digit or a Bit. In English, we have 26 alphabets, 10 digits and several punctuation marks to store and exchange information. In computers, we have just 2. " ]
        , HApp.map BitMsg (Bit.view model.bitModel)
        , Html.p 
            []
            [ Html.text "Click anywhere within the green square to toggle its value between 0 and 1." ]
        ]

packetIntro model =
    Html.div
        []
        [ Html.h4 [] [Html.text "A Packet"]
        , Html.p 
            []
            [ Html.text "A Packet is like a word. It is simply a group of bits. You can build whatever word you want to build with the bits you have. The computers which use this word must understand what it means. Just like how we can talk to some of our close friends with seemingly random words but can make total sense to each other, a packet can be of any set of bits as long as the computers that have to deal with this packet understand the meaning behind it. " ]
        , Html.p 
            [] 
            [ Html.text "Go ahead and build a packet. You can increase or decrease the packet size by adding or removing a bit. And of course, you can set the value of each bit by clicking within the green squares. "]
        , HApp.map PacketMsg (Packet.view model.packetModel)
        ]

combinatricsNotation x y =
    Html.span
        []
        [ Html.sup [] [Html.text (toString x)]
        , Html.text "C"
        , Html.sub [] [Html.text (toString y)]
        ]


pluralString: (String, String) -> Int -> String
pluralString (a, b) n =
    if n == 1 then
        " " ++ a
    else 
        " " ++ b

errorProbability model =
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Probability of Error"]
        , Html.p 
            []
            [ Html.text "The whole point of building a vocabulary of words is to store your memories on a diary or send letters to someone else. Unfortunately, pages on which the words are written can get partially spoiled and make certain words illegible. Similary, in computers, we build a set of packets so that we can store or transmit information. However, the disk drives we store the information on can get corrupted (scratches, lose mechanical parts etc); the cables/wireless environment through which we transmit the information can distort the packets. The exact mechanism of such corruption is beyond the scope of this essay. Let us just focus on how to deal with such corrupted bits."
            ]
        , Html.p 
            []
            [ Html.text "Such corruption and distortion increases the chance of accidental toggling of a single bit. P"
            , Html.sub [] [Html.text "e"] 
            , Html.text (" = " ++ (toString model.bitProbability) ++ " " )
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
            , Html.text "Higher the number, more the chances of corruption. So for the packet built above with "
            , Html.text (toString model.packetModel.msb)
            , Html.text (pluralString ("bit","bits") model.packetModel.msb)
            , Html.br [] []
            , Html.text ("the chance of having absolutely no corrupt bits is " )
            , (combinatricsNotation model.packetModel.msb model.packetModel.msb) 
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString model.packetModel.msb)] 
            , Html.text " = "
            , Html.text (toString 
                            (
                                (toFloat 
                                    (round 
                                        (
                                            (
                                                (1- model.bitProbability)^(toFloat model.packetModel.msb) *100
                                            ) * 100 
                                        )
                                    ) 
                                ) / 100  
                            )
                        ) 
            , Html.text "%, " 
            , Html.br [][]
            , Html.text ("the chance of having exactly 1 corrupt bit is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-1))
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-1))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.text " = "
            , Html.text (toString 
                             (
                                (toFloat 
                                    (round 
                                        (
                                            (
                                                (1- model.bitProbability)^(toFloat (model.packetModel.msb-1)) 
                                                * model.bitProbability 
                                                * 100 
                                                * (toFloat model.packetModel.msb)
                                            ) * 100 
                                        )
                                    ) 
                                ) / 100  
                            )
                        ) 
            , Html.text "%" 
            , Html.br [] []
            , Html.text ("and chances of exactly 2 corrupt bits is ")
            , (combinatricsNotation model.packetModel.msb (model.packetModel.msb-2))
            , Html.text " * (1 - "
            , Html.text ((toString model.bitProbability) ++ ")")
            , Html.sup [] [Html.text (toString (model.packetModel.msb-2))] 
            , Html.text (" * " ++ (toString model.bitProbability))
            , Html.sup [] [Html.text "2"]
            , Html.text " = "
            , Html.text (toString 
                            (
                                (toFloat 
                                    (round 
                                        (
                                            (
                                                (1- model.bitProbability)^(toFloat (model.packetModel.msb-2))
                                                * (model.bitProbability^2) 
                                                * 50 
                                                * (toFloat 
                                                        ( model.packetModel.msb 
                                                        * (model.packetModel.msb - 1)
                                                        )
                                                    ) 
                                            )* 100 
                                        )
                                    ) 
                                ) / 100  
                            )
                        ) 
            , Html.text "%"
            ]
        , Html.p 
            []
            [ Html.text "play around with packet size (by adding/removing bits from packet) and P"
            , Html.sub [] [Html.text "e"]
            , Html.text ". "
            ]
        ]

se = Html.sub [] [Html.text "e"]


parityIntro model= 
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Single Error Detection"]
        , Html.p 
            []
            [ Html.text "If you have played enough with the model above, you may have noticed some patterns. Try to see which packet size and which P"
            , se
            , Html.text " has the highest chance of zero corrupt bits and 1 corrupt bits. In real world application, we usually deal with low P"
            , Html.sub [] [Html.text "e"]
            , Html.text ". If it is so high, we will try to fix that situation by other means. Using the words and letters analogy, we can tolerate ink blotting of certain words in our letters but if the letters get torn/burnt/lost during travel, we change the postal service first but not think about how to write stuff so that words are torn/burn proof. So for smaller P"
            , Html.sub [] [Html.text "e"]
            , Html.text ", it will be wise to deal with 1 bit corruptions as it will improve the reliability. In order to detect such single error, lets look at the most popular and oldest trick: parity check"
            ]
        , HApp.map PacketMsg (Packet.view model.packetModel)
        , HApp.map BitMsg (Bit.view (Bit.defaultBit (singleParity model) 1 "parity"))    
        , Html.br [] []
        , Html.text ( (oddEven model) ++ " Parity Check ")
        , Html.button [ HE.onClick ToggleOddEven] [Html.text "Change" ]
        , Html.p 
            []
            [ Html.text "The parity bit is shown in blue and you can't set its value directly by clicking on it. Its value is set based on the type of parity check we use. In Even Parity Check, the total number of 1s in the packet (including that of blue parity bit) must be even. So the parity bit value is set to ensure that total 1s are even. Similarly, in Odd Parity Check, the parity bit value is set to make sure that total 1s are Odd."
            ]
        , Html.p 
            []
            [Html.text ("When, the receiver gets this packet. It knows that there must be " ++ (oddEven model) ++ " number of 1s in the packet. If not, then we are certain that there is an error. ") ]
        ]



singleErrorCorrection model =
    Html.div 
        [] 
        [ Html.h4 [HA.id "ec"] [Html.text "Single Error Correction"]
        , Html.p 
            []
            [ Html.text "If your friend detects an error in your letter but doesn't know where is it, he/she will request you to send the letter again. It may look inefficient but when the chances of no corruption are high hence is used only for really small P"
            , Html.sub [] [Html.text "e"]
            , Html.text " as such requests would be small in number. Similarly the receivers request a retransmission of such error packets. Try to play with the knobs above to see which values enable us to resort to retransmission method to correct the error packets. "
            ]
        , Html.p 
            []
            [ Html.text "But for other cases, we need to correct a single error. Since there are only two alphabets (or bits) in our case, we can correct an error by identifying the error position within the packet and toggling it. Hence, we need identifiers. So we use k parity bits. As you may know, with k bits we can create 2^k possible names. The actual possibilities are no errors, error at location 1, error at location 2, error at location n. So a total of n+1. So all we have to do is to find minimum k such that 2"
            , Html.sup [] [Html.text "k"]
            , Html.text " is greater or equal to n+1."
            , Html.br [] []
            , Html.text "For the packet below, (n+1) = " 
            , Html.text (toString (model.hammingModel.n))
            , Html.text " and k = "
            , Html.text (toString (model.hammingModel.k)) 
            , Html.text " as 2"
            , Html.sup [] [Html.text (toString (model.hammingModel.k))]
            , Html.text " = "
            , Html.text (toString (2^model.hammingModel.k))
            , Html.br [] []
            , Html.text "Before seeing how this packet can correct one error, play with the packet size. Which packet sizes do you think are the most efficient in terms of sending more data bits per packet?"
            ]
        , Html.p 
            []
            [HApp.map HammingMsg (HammingPacket.view model.hammingModel)]
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

        HammingMsg subAction ->
            let 
                updatedHammingModel = HammingPacket.update subAction model.hammingModel
            in
                {model | hammingModel = updatedHammingModel}


main = 
    HApp.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update 
    }


--- explain about identifiers, why and one example of how. Explain constraints of n and k. 

--- any bit position can be expressed as linear combination of parity bit positions. 
--- So any bit flip in a position affects all the parity bits that are calculated by receiver.
--- By noticing the differences of these parity bits... we can determine the position of error.
