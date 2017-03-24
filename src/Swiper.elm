module Swiper
    exposing
        ( SwipeEvent
        , SwipingState
        , initialSwipingState
        , hasSwipedLeft
        , hasSwipedRight
        , hasSwipedUp
        , hasSwipedDown
        , onSwipeEvents
        )

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


type SwipeEvent
    = TouchStart Coords
    | TouchEnd Coords


type alias Coords =
    { clientX : Float
    , clientY : Float
    }


type Direction
    = Right
    | Left
    | Up
    | Down


type SwipingState
    = SwipingState InternalState


initialSwipingState : SwipingState
initialSwipingState =
    SwipingState <| InternalState Nothing


type alias InternalState =
    { touchSequence : Maybe (Coords -> Direction -> Bool) }


startTouchSequence : Coords -> SwipingState
startTouchSequence coords =
    SwipingState { touchSequence = Just <| checkSwiped coords }


hasSwipedLeft : SwipingState -> SwipeEvent -> ( SwipingState, Bool )
hasSwipedLeft =
    hasSwiped Left


hasSwipedRight : SwipingState -> SwipeEvent -> ( SwipingState, Bool )
hasSwipedRight =
    hasSwiped Right


hasSwipedUp : SwipingState -> SwipeEvent -> ( SwipingState, Bool )
hasSwipedUp =
    hasSwiped Up


hasSwipedDown : SwipingState -> SwipeEvent -> ( SwipingState, Bool )
hasSwipedDown =
    hasSwiped Down


hasSwiped : Direction -> SwipingState -> SwipeEvent -> ( SwipingState, Bool )
hasSwiped dir (SwipingState { touchSequence }) evt =
    case evt of
        TouchStart coords ->
            ( startTouchSequence coords, False )

        TouchEnd coords ->
            case touchSequence of
                Nothing ->
                    ( initialSwipingState, False )

                Just f ->
                    ( initialSwipingState, f coords dir )


checkSwiped : Coords -> Coords -> Direction -> Bool
checkSwiped start end dir =
    case dir of
        Left ->
            start.clientX < end.clientX

        Right ->
            start.clientX > end.clientX

        Up ->
            start.clientY > end.clientY

        Down ->
            start.clientY < end.clientY



-- Event handler/decoders


onSwipeEvents : (SwipeEvent -> msg) -> List (Attribute msg)
onSwipeEvents msg =
    [ onTouchStart msg
    , onTouchEnd msg
    ]


onTouchStart : (SwipeEvent -> msg) -> Attribute msg
onTouchStart msg =
    touchDecoder "targetTouches"
        |> Json.map TouchStart
        |> Json.map msg
        |> on "touchstart"


onTouchEnd : (SwipeEvent -> msg) -> Attribute msg
onTouchEnd msg =
    touchDecoder "changedTouches"
        |> Json.map TouchEnd
        |> Json.map msg
        |> on "touchend"


touchDecoder : String -> Json.Decoder Coords
touchDecoder eventType =
    Json.at [ eventType, "0" ] coordDecoder


coordDecoder : Json.Decoder Coords
coordDecoder =
    Json.map2 Coords
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
