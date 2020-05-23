module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Swiper



-- Model


type alias Model =
    { menuOpen : Bool
    , swipingState : Swiper.SwipingState
    }


initialModel : Model
initialModel =
    { menuOpen = False
    , swipingState = Swiper.initialSwipingState
    }



-- Update


init : {} -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        threshold =
            100
    in
    case msg of
        Swiped evt ->
            let
                ( newState, swipedLeft ) =
                    Swiper.hasSwipedLeft threshold evt model.swipingState
            in
            ( { model | menuOpen = swipedLeft, swipingState = newState }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Msg


type Msg
    = NoOp
    | Swiped Swiper.SwipeEvent



-- Styling


navStyle : Bool -> List (Attribute Msg)
navStyle menuOpen =
    [ style "padding" "1em"
    , style "list-style" "none"
    , style "width" "100%"
    , style "height" "100%"
    , style "position" "fixed"
    , style "top" "0"
    , style "right" "0"
    , style "bottom" "0"
    , style "left" <|
        if menuOpen then
            "0"

        else
            "-50px"
    , style "z-index" "0"
    , style "transition" "left 0.2s"
    ]


siteStyle : Bool -> List (Attribute Msg)
siteStyle menuOpen =
    [ style "padding" "4rem"
    , style "min-width" "100%"
    , style "min-height" "100%"
    , style "position" "absolute"
    , style "top" "0"
    , style "bottom" "100%"
    , style "left" <|
        if menuOpen then
            "200px"

        else
            "0"
    , style "z-index" "1"
    , style "transition" "left 0.2s"
    , style "background-color" "white"
    , style "border-left" "1px solid rgba(0, 0, 0, 0.25)"
    , style "box-shadow" "0 14px 28px rgba(0, 0, 0, 0.25)"
    ]



-- View


documentView : Model -> Browser.Document Msg
documentView model =
    { title = "elm-swiper example"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    div [ style "font-family" "Helvetica, Arial, sans-serif" ]
        [ navigation model
        , div (id "MainSite" :: siteStyle model.menuOpen ++ Swiper.onSwipeEvents Swiped)
            [ h1 [] [ text "This is the main website" ]
            , p [] [ text "swipe left here" ]
            , p [] [ em [] [ text "use chrome dev-tools to simulate mobile" ] ]
            ]
        ]


navigation : Model -> Html Msg
navigation model =
    nav []
        [ ul (class "navigtion" :: navStyle model.menuOpen)
            [ li [ class "nav-item" ] [ a [ href "#" ] [ text "Home" ] ]
            , li [ class "nav-item" ] [ a [ href "#" ] [ text "About" ] ]
            , li [ class "nav-item" ] [ a [ href "#" ] [ text "Contact" ] ]
            ]
        ]



-- App


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = documentView
        , subscriptions = \_ -> Sub.none
        }
