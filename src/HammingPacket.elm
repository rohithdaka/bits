module HammingPacket exposing (Model, Msg(..), dec2bin, defaultPacket, hammingParity, isParityBit, main, receivedDefaultPacket, specificParityValue, update, view, viewSpecificBit)

-- where

import Array
import Basics exposing (..)
import Bit
import Browser
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import List
import Random as R
import String exposing (concat, join, length, split)
import Svg exposing (..)
import Svg.Attributes



-- Model


type alias Model =
    { bits : List Bit.Model
    , n : Int
    , k : Int
    , status : String
    , seed : R.Seed
    }


defaultPacket : Model
defaultPacket =
    { bits =
        [ Bit.defaultBit 0 7 "data" False
        , Bit.defaultBit 0 6 "data" False
        , Bit.defaultBit 1 5 "data" False
        , Bit.defaultBit 1 4 "parity" False
        , Bit.defaultBit 0 3 "data" False
        , Bit.defaultBit 0 2 "parity" False
        , Bit.defaultBit 1 1 "parity" False
        ]
    , n = 8
    , k = 3
    , status = "T"
    , seed = R.initialSeed 4
    }


receivedDefaultPacket : Model
receivedDefaultPacket =
    { bits =
        [ Bit.defaultBit 0 7 "data" False
        , Bit.defaultBit 0 6 "data" False
        , Bit.defaultBit 0 5 "data" False
        , Bit.defaultBit 0 4 "parity" False
        , Bit.defaultBit 0 3 "data" False
        , Bit.defaultBit 0 2 "parity" False
        , Bit.defaultBit 0 1 "parity" False
        ]
    , n = 8
    , k = 3
    , status = "R"
    , seed = R.initialSeed 5
    }


specificParityValue : Int -> Model -> List Int
specificParityValue x packet =
    let
        getBitValue bit =
            case Array.get (round (logBase 2 (toFloat x)) + 1) (dec2bin packet.n) |> Maybe.withDefault 0 of
                1 ->
                    bit.value

                _ ->
                    0
    in
    List.map getBitValue packet.bits


hammingParity : Int -> Model -> Int
hammingParity x packet =
    case
        modBy 2
            (packet
                |> specificParityValue x
                |> List.map String.fromInt
                |> concat
                |> split "0"
                |> join ""
                |> length
            )
    of
        0 ->
            0

        1 ->
            1

        _ ->
            5



-- This should never happen as the case is determined by dividing with 2


type Msg
    = AddBit
    | RemoveBit
    | ModifyBit Int Bit.Msg
    | JustHighlightBit Int Bit.Msg


update : Msg -> Model -> Model
update msg packet =
    case msg of
        AddBit ->
            let
                newBitPosition =
                    packet.n

                newBit =
                    if 2 ^ round (logBase 2 (toFloat newBitPosition)) == packet.n then
                        Bit.defaultBit (hammingParity packet.n packet) newBitPosition "parity" False

                    else
                        Bit.defaultBit 0 newBitPosition "data" False

                newBits =
                    [ newBit ] ++ packet.bits

                nPlusPlus =
                    packet.n + 1
            in
            { packet
                | bits = newBits
                , n = nPlusPlus
                , k = ceiling (logBase 2 (toFloat nPlusPlus))
            }

        RemoveBit ->
            let
                nMinusMinus =
                    packet.n - 1
            in
            if packet.n > 0 then
                { packet
                    | bits = List.drop 1 packet.bits
                    , n = nMinusMinus
                    , k = ceiling (logBase 2 (toFloat nMinusMinus))
                }

            else
                packet

        ModifyBit id bitAction ->
            let
                updateSpecificBit bit =
                    if bit.position == id then
                        Bit.update bitAction bit
                        -- highlights data bits that are associated with hovered parity bit

                    else if 2 ^ round (logBase 2 (toFloat id)) == id then
                        if (Array.get (round (logBase 2 (toFloat id))) (dec2bin bit.position) |> Maybe.withDefault 0) == 1 then
                            Bit.bitHighlighter bit bitAction

                        else
                            bit
                        -- highlights parity bits that are influenced by the hovered data bit

                    else if isParityBit bit.position id then
                        Bit.update bitAction bit

                    else
                        bit
            in
            { packet
                | bits = List.map updateSpecificBit packet.bits
            }

        JustHighlightBit id bitAction ->
            let
                updateSpecificBit bit =
                    -- highlights data bits that are associated with hovered parity bit
                    if 2 ^ round (logBase 2 (toFloat id)) == id then
                        if (Array.get (round (logBase 2 (toFloat id))) (dec2bin bit.position) |> Maybe.withDefault 0) == 1 then
                            Bit.bitHighlighter bit bitAction

                        else
                            bit
                        -- highlights parity bits that are influenced by the hovered data bit

                    else if isParityBit bit.position id then
                        Bit.bitHighlighter bit bitAction

                    else
                        bit
            in
            { packet
                | bits = List.map updateSpecificBit packet.bits
            }


isParityBit : Int -> Int -> Bool
isParityBit p i =
    if 2 ^ round (logBase 2 (toFloat p)) == p then
        dec2bin i |> Array.toIndexedList |> List.member ( round (logBase 2 (toFloat p)), 1 )

    else
        False


dec2bin : Int -> Array.Array Int
dec2bin v =
    case v of
        1 ->
            Array.repeat 1 1

        0 ->
            Array.repeat 1 0

        _ ->
            Array.append (Array.repeat 1 (modBy 2 v)) (dec2bin (v // 2))


view packet =
    case packet.status of
        "T" ->
            let
                bits =
                    List.map (viewSpecificBit packet.status) packet.bits

                removeBit =
                    button [ onClick RemoveBit ] [ text "Remove" ]

                addBit =
                    button [ onClick AddBit ] [ text "Add" ]
            in
            div
                []
                ([ Html.br [] [] ]
                    ++ [ removeBit ]
                    ++ [ Html.text (" n = " ++ String.fromInt (packet.n - 1) ++ " ") ]
                    ++ [ addBit ]
                    ++ [ Html.br [] [] ]
                    ++ bits
                )

        "R" ->
            let
                bits =
                    List.map (viewSpecificBit packet.status) packet.bits
            in
            div
                []
                ([ Html.br [] [] ]
                    ++ bits
                )

        _ ->
            div [] []


viewSpecificBit : String -> Bit.Model -> Html Msg
viewSpecificBit status bit =
    case status of
        "T" ->
            Html.map (ModifyBit bit.position) (Bit.view bit)

        "R" ->
            Html.map (JustHighlightBit bit.position) (Bit.view bit)

        _ ->
            div [] []


main =
    Browser.sandbox
        { init = defaultPacket
        , view = view
        , update = update
        }
