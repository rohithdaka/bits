import Bit
import Packet 
import HammingPacket
import Header 
import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as J
import String exposing(concat,split,join,length)
import Basics
import Random as R

type alias AppModel = 
    { headerModel: Header.Model
    , bitModel: Bit.Model
    , packetModel: Packet.Model 
    , noParityReceivedPacket: Packet.Model
    , oddeven: Int 
    , bitProbability: Float
    , hammingModel: HammingPacket.Model
    , hammingReceivedModel: HammingPacket.Model
    }

initialModel: AppModel
initialModel = 
    { headerModel = Header.initialModel
    , bitModel = Bit.initialModel
    , packetModel = Packet.defaultPacket
    , noParityReceivedPacket = Packet.defaultPacket
    , bitProbability = 0.15
    , oddeven = 0
    , hammingModel = HammingPacket.defaultPacket
    , hammingReceivedModel = HammingPacket.receivedDefaultPacket
    -- 0 for even and 1 for odd
    }



type Msg 
    = PacketMsg Packet.Msg
    | BitMsg Bit.Msg
    | UpdateProbability String 
    | ToggleOddEven
    | HammingMsg HammingPacket.Msg
    | HammingRMsg HammingPacket.Msg
    | TransmitPacket

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
            [ Html.text "This explorable essay is a quick introduction to the concept of Error Detecting and Error Correcting Codes. "
            ]
        , Html.p
            []
            [ Html.text "In April 1950, Richard Hamming introduced a technique to improve the reliability of communications and storage using bits. This essay is inspired by his original paper and written to serve as a preliminary guide to understand some concepts in that paper. No prior knowledge is assumed on your part. This essay intends to reason about the decisions that make this technique work. The models that are presented for you to explore, hopefully, will provide necessary insights to grasp the concepts. However, this essay doesnt deal with implementation details of this technique."
            ]
        ]

tutorialConclusionText = 
    Html.div []
        [ Html.h4 [] [Html.text "Conclusion"]
        , Html.p 
            []
            [ Html.text "The goal of this essay in using this visual explanation is to reduce the difficulty in understanding this concept. One advantage with this medium is that you are no longer a passive reader. To paraphrase a famous quote: You hear and you forget. You see and You remember. You do and You understand. I hope this active reading helped you understand the fundamentals of Error Detection and Error Correction. You can read the " 
            , Html.a 
                [HA.href "http://wayback.archive.org/web/20060525060427/http://www.caip.rutgers.edu/~bushnell/dsdwebsite/hamming.pdf"]
                [Html.text "original paper"]
            , Html.text " by Richard Hamming for more clarification."
            ]
        ]

tutorialFooterText =
    [ Html.p
        []
        [ Html.text "If you find any error in technical details of this essay, please file an issue on "
        , Html.a 
            [HA.href "https://github.com/rohithdaka/bits/issues"]
            [Html.text "github"]
        , Html.text ". Please email me at rohith.daka@gmail.com if you have any suggestions, comments about improving this essay. "
        ]
    , Html.p
        []
        [Html.text "Thanks to Bret Victor, for the inspiration and his work on explorable explanations; Evan Czaplicki, for designing Elm language; and Pooja Tyagi, for helping me brain storm the visual models for this essay."]
    ]


bitIntro model =
    Html.div
        []
        [Html.h4 [] [Html.text "A Bit"]
        , Html.p 
            []
            [ Html.text "Let's start with the fundamental unit of communication: Binary Digit or a Bit. In English, we have 26 alphabets, 10 digits and several punctuation marks to store and exchange information. In computers, we have just 2. " ]
        , Html.map BitMsg (Bit.view model.bitModel)
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
            [ Html.text "A Packet (a group of bits) is like a word (a group of alphabets). You can build whatever packet you want with the bits you have. The computers which use this packet must understand what it means. Just like how we can invent words to talk to friends. A packet can be of any set of bits as long as the computers that have to deal with this packet understand the meaning behind it." ]
        , Html.p 
            [] 
            [ Html.text "You can build a packet. Just add, remove or toggle a bit. The number on the top left corner in the square represent the position of that bit in the packet. You will need it later in this essay."]
        , Html.map PacketMsg (Packet.view model.packetModel)
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
    let n = (model.packetModel.msb - 1)
    in  
        Html.div 
            [] 
            [Html.h4 [] [Html.text "Probability of Error"]
            , Html.p 
                []
                [ Html.text "The whole point of building a vocabulary of words is to store your memories on a diary or send mail to friends. Unfortunately, pages on which the words are written can be spoiled making certain words illegible. Similary, in computers, we build a vocabulary of these packets to store or transmit information. However, the disk drives we store these packets on can get corrupted, the cables/wireless environment through which we transmit these packets can distort them. The exact mechanism of such corruption is beyond the scope of this essay. Let us just focus on how to deal with such corrupted packets."
                ]
            , Html.p 
                []
                [ Html.text "We use a probability metric (0 to 1) to quantify such accidental toggling of a bit, let it be P"
                , se 
                , Html.text " = "
                , Html.input 
                    [ HA.type' "number"
                    , HA.size 50
                    , HA.value (toString model.bitProbability) 
                    , HA.max "1"
                    , HA.min "0"
                    , HA.step "0.01"
                    , HE.on "change" (J.map UpdateProbability HE.targetValue)
                    ]
                    []
                , Html.br [][]
                , Html.text "Higher this number, more the chances of corruption of entire packet. For the above packet you built with "
                , Html.text (toString n)
                , Html.text (pluralString ("bit","bits") n)
                , Html.br [] []
                , Html.text ("the chance of having absolutely no corrupt bits is:  " )
                , Html.span [] 
                    [(combinatricsNotation n n) 
                    , Html.text " * (1 - "
                    , Html.text ((toString model.bitProbability) ++ ")")
                    , Html.sup [] [Html.text (toString n)] 
                    , Html.text " = "
                    , Html.text (toString 
                                    (
                                        (toFloat 
                                            (round 
                                                (
                                                    (
                                                        (1- model.bitProbability)^(toFloat n) *100
                                                    ) * 100 
                                                )
                                            ) 
                                        ) / 100  
                                    )
                                ) 
                    , Html.text "%, "
                ]
                , Html.br [][]
                , Html.text ("the chance of having exactly 1 corrupt bit is: ")
                , Html.span [] 
                    [ (combinatricsNotation n (n-1))
                    , Html.text " * (1 - "
                    , Html.text ((toString model.bitProbability) ++ ")")
                    , Html.sup [] [Html.text (toString (n-1))] 
                    , Html.text (" * " ++ (toString model.bitProbability))
                    , Html.text " = "
                    , Html.text (toString 
                                     (
                                        (toFloat 
                                            (round 
                                                (
                                                    (
                                                        (1- model.bitProbability)^(toFloat (n-1)) 
                                                        * model.bitProbability 
                                                        * 100 
                                                        * (toFloat n)
                                                    ) * 100 
                                                )
                                            ) 
                                        ) / 100  
                                    )
                                ) 
                    , Html.text "%" 
                    ]
                , Html.br [] []
                , Html.text ("and chances of exactly 2 corrupt bits is:  ")
                , Html.span [] 
                    [ (combinatricsNotation n (n-2))
                    , Html.text " * (1 - "
                    , Html.text ((toString model.bitProbability) ++ ")")
                    , Html.sup [] [Html.text (toString (n-2))] 
                    , Html.text (" * " ++ (toString model.bitProbability))
                    , Html.sup [] [Html.text "2"]
                    , Html.text " = "
                    , Html.text (toString 
                                    (
                                        (toFloat 
                                            (round 
                                                (
                                                    (
                                                        (1- model.bitProbability)^(toFloat (n-2))
                                                        * (model.bitProbability^2) 
                                                        * 50 
                                                        * (toFloat 
                                                                ( n 
                                                                * (n- 1)
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
                ]
            , Html.p 
                []
                [ Html.text "You can get an intution of how packet sizes and  P"
                , se
                , Html.text " affect the chances of corruption in the packet,  by playing around with those values. If you are wondering about how the chances are calculated you need to learn Probability, Permutations and Combinations. They are not necessary to understand the rest of this essay. For those who are familiar with the concept, the substituted equations are a hint."
                ]
            ]

se = Html.sub [] [Html.text "e"]


parityIntro model= 
    Html.div 
        [] 
        [Html.h4 [] [Html.text "Single Error Detection"]
        , Html.p 
            []
            [ Html.text "In real world application, we usually use these error detection and error correcting techniques for low P"
            , se
            , Html.text " situations. The higher cases are dealt by other means. To give you an example, we can tolerate ink blotting of certain words in our mail but if the mail is torn/burnt/lost during travel, we should change the postal service first but not think about writing words which are torn/burn proof. As we assume smaller P"
            , se
            , Html.text ", handling 1 bit corruptions (or single errors) increases chances of reliable transmission tremendously (Dont take my word, check it by tinkering the packet above). "
            ]
        , Html.p 
            []
            [ Html.text "First step is to 'detect' a single error and to deal with it, we shall use the most popular and oldest trick: parity check. The parity bit is shown in blue below. Unlike data bit, you can't set its value directly by clicking on it. However, its value is set based on the type of parity check we use. The job of this Parity Bit is to make sure that there are certain number of 1s in the packet (including the parity bit). In Even Parity Check, its job is to make sure that there are even number of 1s. In Odd Parity Check, its job is to make sure that there are odd number of 1s. There is a button below the packet. You can use it to change the Parity Check type. Along with changing bit values and packet size, you can see how parity bit is set."
            ]
        , Html.map PacketMsg (Packet.view model.packetModel)
        , Html.map BitMsg (Bit.view (Bit.defaultBit (singleParity model) 1 "parity" False))    
        , Html.p 
            []
            [ Html.text ( (oddEven model) ++ " Parity Check ")
            , Html.button [ HE.onClick ToggleOddEven] [Html.text "Change" ]
            ]
        , Html.p 
            []
            [Html.text ("When, the receiver gets this packet. It knows that there must be " ++ (oddEven model) ++ " number of 1s in the packet. If not, then we are certain that there is an error. When the receiver finds out that there is an error, it asks for retransmission.") ]
        ]



singleErrorCorrection model =
    Html.div 
        [] 
        [ Html.h4 [HA.id "ec"] [Html.text "Single Error Correction"]
        , Html.p 
            []
            [ Html.text "If your first thought of retransmission is: oh! it is inefficient, you are not wrong. If your friend detects an error in your mail but doesn't know where it is, he/she requesting you to send the mail again is not practical if errors in your mail are slightly more than tolerable. To make the process more efficient we must figure out a way to detect the exact alphabet that is in error. English words and sentences have this error correcting capability. We usually can understand the words without all the alphabets most of the time. In this section, let us try to build that capability into computer words (packets)"
            ]
        , Html.p 
            []
            [ Html.text "Since there are only two alphabets (or bits) in our case, we can correct an error by identifying the error position within the packet (of size n) and toggling it. So all we need to think about is a way to identify the error bit. But first, we need a set of identifiers for this purpose to represent each bit. We also need an additional identifier to represent the 'no error situation' (a total of n + 1). So, the packet with n bits must include k parity bits along with the data bits. This reduces the number of data bits in a packet to n-k. The k bits are used to generate 2"
            , Html.sup [] [Html.text "k"]
            , Html.text " identifiers at the receiver. So we have to make sure that 2"
            , Html.sup [] [Html.text "k"]
            , Html.text " is greater or equal to n+1."
            ]
        , Html.p 
            []
            [Html.text "To avoid confusion, let us assume the following:" 
            , Html.ul []
                [ Html.li [] 
                    [ Html.text "We are limited by the total packet size, n. So we have to fix the k parity bits for this packet and adjust our data into the remaining bits." ]
                , Html.li []
                    [ Html.text "These parity bits are all even parity check bits."] 
                , Html.li []
                    [ Html.text "Each bit does the even parity check on a set of bits in the packet but not the whole packet. (You can mouseover the blue parity bits to see which bits does it keep a watch on to maintain even parity)."] 
                , Html.li []
                    [ Html.text "Arranging these k parity bits is a vital aspect of building this packet. Only rule to follow is to make sure that no parity bit includes another parity bit while checking for even parity."]
                ]
            , Html.text "Thus we end up the following arrangement. (Exercise for the readers: Find out the criteria that decided the set of bits for each parity bit. Clue: convert the position number on top corner into binary format.)"
            ]
        , Html.p
            []
            [ Html.text "Hovering over data bits (green) will highlight all parity bits (blue) it will influence. And Hovering over parity bits (blue) will highlight all data bits (green) it keeps a watch on. And you can change the value of data bits by clicking on them. Parity bits will be updated immediately."
            ]
        , Html.p 
            []
            [Html.map HammingMsg (HammingPacket.view model.hammingModel)]
        ,Html.p 
            []
            [ Html.text "For the packet above, (n+1) = " 
            , Html.text (toString (model.hammingModel.n))
            , Html.text " and k = "
            , Html.text (toString (model.hammingModel.k)) 
            , Html.text " as 2"
            , Html.sup [] [Html.text (toString (model.hammingModel.k))]
            , Html.text " = "
            , Html.text (toString (2^model.hammingModel.k))
            , Html.text ". Lets see how the receiver can detect the exact bit that is corrupted. Click the button below to transmit the packet. It will randomly flip a bit or not. Everytime you change the above packet, click the transmit button again. You can then follow these simple rules to detect that corrupted bit."
            , Html.ol []
                [ Html.li [] 
                    [ Html.text "Hover/Click on the parity bits to highlight the data bits that contribute to that parity bit. "]
                , Html.li [] 
                    [ Html.text "Verify if the parity bit satisfies the even parity check rule."]
                    
                , Html.li []
                    [ Html.text "Note the parity bit positions (top left corner) that violate rule."]
                , Html.li []
                    [ Html.text "Add these position numbers."]
                , Html.li []
                    [ Html.text "Voila! You found the corrupted bit!! You can verify by comparing it with the packet above."]

                ]
            ]
        , Html.button [ HE.onClick TransmitPacket] [Html.text "Transmit Packet" ]
        , Html.p 
            []
            [Html.map HammingRMsg (HammingPacket.view model.hammingReceivedModel)]
        ]


transmissionEfficiency model =
    Html.div [] []


view model =
    Html.body 
        [HA.width 900]
        [ Html.header 
            [] 
            [Header.view model.headerModel]
        , Html.section
            []
            [ tutorialIntroText
            , bitIntro model 
            , packetIntro model
            , errorProbability model
            , parityIntro model
            , transmissionEfficiency model
            , singleErrorCorrection model
            , tutorialConclusionText
            ]
        , Html.footer 
            []
            tutorialFooterText
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
                let x = (n |> String.toFloat |> Result.toMaybe |> Maybe.withDefault 0.01)
                in 
                    if (x >= 0 && x <= 1) then
                        {model | bitProbability = x}
                    else
                        model

        ToggleOddEven ->
            {model | oddeven = (model.oddeven + 1 ) % 2 }

        HammingMsg subAction ->
            let 
                updatedHammingModel = HammingPacket.update subAction model.hammingModel
            in
                {model | hammingModel = updatedHammingModel}
        HammingRMsg subAction ->
             let 
                updatedHammingRModel = HammingPacket.update subAction model.hammingReceivedModel
            in
                {model | hammingReceivedModel = updatedHammingRModel}
        TransmitPacket ->
            let 
                transmittedModel = model.hammingModel
                rModel = model.hammingReceivedModel
                (bitToBeToggled, nextSeed) = (R.step (R.int 0 transmittedModel.n) rModel.seed)
                receivedModel = {transmittedModel | status = "R", bits = (transmittedBits bitToBeToggled transmittedModel.bits), seed = nextSeed}
            in  
                {model | hammingReceivedModel = receivedModel} 


transmittedBits x bits =
    List.map (corruptTheBit x) bits


corruptTheBit x bit =
    if (bit.position == x ) then 
        Bit.bitToggle bit
    else 
        bit



main = 
    Html.beginnerProgram { 
        model = initialModel
    ,   view = view
    ,   update = update
    }


--- explain about identifiers, why and one example of how. Explain constraints of n and k. 

--- any bit position can be expressed as linear combination of parity bit positions. 
--- So any bit flip in a position affects all the parity bits that are calculated by receiver.
--- By noticing the differences of these parity bits... we can determine the position of error.
