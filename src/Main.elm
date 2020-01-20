module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, int, map2, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type ApiCallState
    = Failure
    | Loading
    | Success


type alias Model =
    { apiCallState : ApiCallState, characters : List Character }


exampleCharacter =
    { name = "Kyle Katarn"
    , height = "40"
    }


initialModel =
    { apiCallState = Loading, characters = [ exampleCharacter ] }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getCharacter )


apiUrl =
    "https://swapi.co/api/people/20/"


type Msg
    = RequestCharacter
    | GotCharacter (Result Http.Error Character)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCharacter ->
            ( { model | apiCallState = Loading }, getCharacter )

        GotCharacter result ->
            case result of
                Ok character ->
                    ( { model | apiCallState = Success, characters = character :: model.characters }, Cmd.none )

                Err _ ->
                    ( { model | apiCallState = Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        myStr =
            case model.apiCallState of
                Success ->
                    "Success"

                Failure ->
                    "Failure"

                Loading ->
                    "Contacting API"
    in
    Element.layout []
        (Element.column [ centerX, centerY, spacing 20 ]
            [ Input.button
                [ centerX
                , Background.color (rgb255 219 219 219)
                , padding 10
                ]
                { onPress = Just RequestCharacter
                , label = text "Get Character"
                }
            , el [ centerX ] (text myStr)
            , Element.column [ spacing 10 ]
                (List.map characterToString model.characters)
            ]
        )


characterToString : Character -> Element msg
characterToString character =
    text ("Name: " ++ character.name ++ " Height: " ++ character.height)


getCharacter : Cmd Msg
getCharacter =
    Http.get
        { url = apiUrl
        , expect = Http.expectJson GotCharacter characterDecoder
        }


type alias Character =
    { name : String
    , height : String
    }


characterDecoder : Decoder Character
characterDecoder =
    map2 Character
        (field "name" string)
        (field "height" string)
