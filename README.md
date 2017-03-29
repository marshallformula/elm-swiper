# elm-swiper

This package has a very specific use-case.  It's purpose is to make it easier to detect and react to "swiping" gestures on mobile devices.
There are no actual "swipe" events in the DOM - so this package evaluates "touchstart" and "touchend" events to determine if a user _swiped_ in a specific direction.

Because this has to be handled in a two-step fashion, the application using this library must store some "state" that can be passed back to the library to determine 
a swipe event.

## Installation

```bash
elm-package install marshallformula/elm-swiper
```

## Usage

```elm
import Swiper

-- Application Model
type alias Model =
    { swipingState : Swiper.SwipingState
    , userSwipedLeft : Bool
    }

-- Messages
type Msg 
    = Swiped Swiper.SwipeEvent

-- Update
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model = 
    case msg of 
        Swiped evt ->
            let 
                ( newState, swipedLeft) =
                    Swiper.hasSwipedLeft evt model.swipingState
            in
                ( { model | swipingState = newState, userSwipedLeft = swipedLeft }, Cmd.none )

-- View
view : Model -> Html Msg
view model =
    div ( [ id "Main" ] ++ Swiper.onSwipeEvents Swiped ) [ text "main website" ] 
```
