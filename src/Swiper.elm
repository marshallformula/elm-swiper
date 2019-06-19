module Swiper exposing
    ( SwipingState, initialSwipingState
    , SwipeEvent, onSwipeEvents
    , hasSwipedLeft, hasSwipedRight, hasSwipedUp, hasSwipedDown, touchFinished
    )

{-| This library handles detection of specific touch events (for mobile) that indicates a user swiping across
the specified DOM element.


# State

@docs SwipingState, initialSwipingState


# Events

@docs SwipeEvent, onSwipeEvents


# Swipe Detection

@docs hasSwipedLeft, hasSwipedRight, hasSwipedUp, hasSwipedDown, touchFinished

-}

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


{-| Coordinates of touch events
-}
type alias Coords =
    { clientX : Float
    , clientY : Float
    }


{-| This event is either a "touchstart" or "touchend" event. You don't need to worry about which -
just need to hang on to it so you can pass it back to a "hasSwipedXXX" function.
-}
type SwipeEvent
    = TouchStart Coords
    | TouchEnd Coords


{-| Swiping Directions
-}
type Direction
    = Right 
    | Left
    | Up
    | Down


{-| Since there is no actual "swipe" event - the detection of a swipe is determined by evaluating the coordinates of "touchstart" and "touchend" events. This means some "state" must be stored by the application between events. That state is encapsulated in _SwipingState_. Store this so it can be passed to a hasSwipedXXX function.
-}
type SwipingState
    = SwipingState InternalState


{-| Returns an initial SwipingState with which to initialize the application.
-}
initialSwipingState : SwipingState
initialSwipingState =
    SwipingState <| InternalState Nothing


{-| Internal representation of the state of the swiping events
-}
type alias InternalState =
    { touchStarted : Maybe Coords }


{-| Helper method to set SwipingState based on new coordinates
-}
startTouchSequence : Coords -> SwipingState
startTouchSequence coords =
    SwipingState { touchStarted = Just coords }


{-| Checks whether the the event & state indicates a swipe to the left.
Returns a tuple with the new SwipingState and the Bool answer.
-}
hasSwipedLeft : Float -> SwipeEvent -> SwipingState -> ( SwipingState, Bool )
hasSwipedLeft threshold =
    hasSwiped threshold Left


{-| Checks whether the event & state indicates a swipe to the right.
Returns a tuple with the new SwipingState and the Bool answer.
-}
hasSwipedRight : Float -> SwipeEvent -> SwipingState -> ( SwipingState, Bool )
hasSwipedRight threshold =
    hasSwiped threshold Right


{-| Checks whether the event & state indicates a swipe upward.
Returns a tuple with the new SwipingState and the Bool answer.
-}
hasSwipedUp : Float -> SwipeEvent -> SwipingState -> ( SwipingState, Bool )
hasSwipedUp threshold =
    hasSwiped threshold Up


{-| Checks whther the event & state indicates a swipe downward.
Returns a tuple with the new SwipingState and the Bool answer.
-}
hasSwipedDown : Float -> SwipeEvent -> SwipingState -> ( SwipingState, Bool )
hasSwipedDown threshold =
    hasSwiped threshold Down 


{-| Helper function to detect swipe direction.
-}
hasSwiped : Float -> Direction -> SwipeEvent -> SwipingState -> ( SwipingState, Bool )
hasSwiped threshold dir evt (SwipingState { touchStarted }) =
    case evt of
        TouchStart coords ->
            ( startTouchSequence coords, False )

        TouchEnd coords ->
            case touchStarted of
                Nothing ->
                    ( initialSwipingState, False )

                Just firstTouch ->
                    ( initialSwipingState, checkSwiped firstTouch coords dir threshold)


{-| Checks the swipe direction based on the coords from the touchstart vs the touchend
-}
checkSwiped : Coords -> Coords -> Direction -> Float -> Bool
checkSwiped start end dir threshold =
    let 
        isGreater = isGreaterThanThreshold threshold
    in
    case dir of
        Left ->
            isGreater end.clientX start.clientX

        Right ->
            isGreater start.clientX end.clientX

        Up ->
            isGreater start.clientY end.clientY

        Down ->
            isGreater end.clientY start.clientY


{-| Helper function that checks is a number is greater than another and the 
    difference is above a specified threshold -}
isGreaterThanThreshold : Float -> Float -> Float -> Bool
isGreaterThanThreshold threshold shouldBeGreater shouldBeSmaller =
    shouldBeGreater > shouldBeSmaller && shouldBeGreater - shouldBeSmaller > threshold


{-| Convenience function that will indicate if this is a "touchend" event.
-}
touchFinished : SwipeEvent -> Bool
touchFinished evt =
    case evt of
        TouchStart _ ->
            False

        TouchEnd _ ->
            True



-- Event handler/decoders


{-| Function that detects the touch events. A message wrapper is passed in to be handled in the application update handler.
It returns a list of Attributes (it must be a list because it can fire both "touchstart" and "touchend" states)
-}
onSwipeEvents : (SwipeEvent -> msg) -> List (Attribute msg)
onSwipeEvents msg =
    [ onTouchStart msg
    , onTouchEnd msg
    ]


{-| Touch start event handler
-}
onTouchStart : (SwipeEvent -> msg) -> Attribute msg
onTouchStart msg =
    touchDecoder "targetTouches"
        |> Json.map TouchStart
        |> Json.map msg
        |> on "touchstart"


{-| Touch end event handler
-}
onTouchEnd : (SwipeEvent -> msg) -> Attribute msg
onTouchEnd msg =
    touchDecoder "changedTouches"
        |> Json.map TouchEnd
        |> Json.map msg
        |> on "touchend"


{-| Decodes touch events
-}
touchDecoder : String -> Json.Decoder Coords
touchDecoder eventType =
    Json.at [ eventType, "0" ] coordDecoder


{-| Decodes the clientX/Y coordinates from touch events
-}
coordDecoder : Json.Decoder Coords
coordDecoder =
    Json.map2 Coords
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
