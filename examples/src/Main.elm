module Main exposing (..)

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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        threshold = 100
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


navStyle : Bool -> Attribute Msg
navStyle menuOpen =
    style
        [ ( "padding", "1em" )
        , ( "list-style", "none" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        , ( "position", "fixed" )
        , ( "top", "0" )
        , ( "right", "0" )
        , ( "bottom", "0" )
        , ( "left"
          , if menuOpen then
                "0"
            else
                "-50px"
          )
        , ( "z-index", "0" )
        , ( "transition", "left 0.2s" )
        ]


siteStyle : Bool -> Attribute Msg
siteStyle menuOpen =
    style
        [ ( "padding", "4rem" )
        , ( "min-width", "100%" )
        , ( "min-height", "100%" )
        , ( "position", "absolute" )
        , ( "top", "0" )
        , ( "bottom", "100%" )
        , ( "left"
          , if menuOpen then
                "200px"
            else
                "0"
          )
        , ( "z-index", "1" )
        , ( "transition", "left 0.2s" )
        , ( "background-color", "white" )
        , ( "border-left", "1px solid rgba(0, 0, 0, 0.25)" )
        , ( "box-shadow", "0 14px 28px rgba(0, 0, 0, 0.25)" )
        ]



-- View


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "Helvetica, Arial, sans-serif" ) ] ]
        [ navigation model
        , div (id "MainSite" :: siteStyle model.menuOpen :: Swiper.onSwipeEvents Swiped)
            [ h1 [] [ text "This is the main website" ]
            , p [] [ text "swipe left here" ]
            , p [] [ em [] [ text "use chrome dev-tools to simulate mobile" ] ]
            ]
        ]


navigation : Model -> Html Msg
navigation model =
    nav []
        [ ul [ class "navigtion", navStyle model.menuOpen ]
            [ li [ class "nav-item" ] [ a [ href "#" ] [ text "Home" ] ]
            , li [ class "nav-item" ] [ a [ href "#" ] [ text "About" ] ]
            , li [ class "nav-item" ] [ a [ href "#" ] [ text "Contact" ] ]
            ]
        ]



-- App


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }
