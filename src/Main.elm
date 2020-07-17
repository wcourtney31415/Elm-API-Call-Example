module Main exposing (main)

import Browser
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as JsonDecoder
    exposing
        ( Decoder
        , field
        , map2
        , string
        )
import Style as S


main : Program () Model Msg
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
    { apiCallState : ApiCallState
    , characters : List Character
    }


type alias Character =
    { name : String
    , height : String
    }


type Msg
    = RequestCharacter
    | GotCharacter (Result Http.Error Character)


exampleCharacter : { name : String, height : String }
exampleCharacter =
    { name = "Kyle Katarn"
    , height = "40"
    }


initialModel : { apiCallState : ApiCallState, characters : List Character }
initialModel =
    { apiCallState = Loading
    , characters = [ exampleCharacter ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getCharacter )


apiUrl : String
apiUrl =
    "https://swapi.dev/api/people/20/"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCharacter ->
            ( { model | apiCallState = Loading }
            , getCharacter
            )

        GotCharacter result ->
            case result of
                Ok character ->
                    ( { model
                        | apiCallState = Success
                        , characters = character :: model.characters
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | apiCallState = Failure }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        tup =
            case model.apiCallState of
                Success ->
                    ( "Success", S.green )

                Failure ->
                    ( "Failure", S.red )

                Loading ->
                    ( "Contacting API", S.blue )

        col =
            Tuple.second tup

        txt =
            Tuple.first tup

        pageState =
            E.el
                [ E.centerX
                , Background.color col
                , E.padding 10
                , Font.bold
                , Border.rounded 5
                , S.shadow
                ]
            <|
                E.text txt
    in
    E.layout [ Background.color S.grey ]
        (E.column
            [ E.centerX
            , E.centerY
            , E.spacing 20
            ]
            [ getCharButton
            , pageState
            , characterList model
            ]
        )


characterList : Model -> E.Element Msg
characterList model =
    E.column
        [ E.spacing 10
        ]
        (List.map characterToText model.characters)


getCharButton : E.Element Msg
getCharButton =
    Input.button
        [ E.centerX
        , Background.color S.btnGrey
        , E.padding 10
        , Border.rounded 5
        , S.shadow
        ]
        { onPress = Just RequestCharacter
        , label = E.el [ Font.size 20, Font.bold ] <| E.text "Get Character"
        }


getCharacter : Cmd Msg
getCharacter =
    Http.get
        { url = apiUrl
        , expect = Http.expectJson GotCharacter characterDecoder
        }


characterDecoder : Decoder Character
characterDecoder =
    JsonDecoder.map2 Character
        (field "name" string)
        (field "height" string)


ffield ( fieldName, val ) =
    E.row
        [ E.spacing 5 ]
        [ E.el [ Font.bold ] <|
            E.text <|
                fieldName
                    ++ ":"
        , E.text val
        ]


characterToText : Character -> E.Element msg
characterToText character =
    E.el
        [ Background.color S.purple
        , Border.rounded 10
        , E.padding 10
        , S.shadow
        ]
    <|
        E.column [ E.spacing 10 ]
            [ ffield ( "Name", character.name )
            , ffield ( "Height", character.height )
            ]
