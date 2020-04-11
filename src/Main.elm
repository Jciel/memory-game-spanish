module Main exposing (..)

import Browser exposing (element)
import Html exposing (button, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (..)


type CardStatus
    = FlippedIn String
    | FlippedOut String


type Button
    = Correct
    | Wrong


type alias DescriptionCard =
    { id : Int
    , description : String
    , flipped : CardStatus
    }


type alias ImageCard =
    { id : Int
    , name : String
    , imagePath : String
    , flipped : CardStatus
    }


type Card
    = Description DescriptionCard
    | Image ImageCard


type alias Model =
    { firstFlippedCard : Maybe Card
    , secondFlippedCard : Maybe Card
    , descriptionCards : List DescriptionCard
    , imageCards : List ImageCard
    , buttonStatus : Button
    }


type Msg
    = FlipCard Card
    | RemoveCards


initialModel : Model
initialModel =
    Model Nothing Nothing descriptionCards imageCards Wrong


init : () -> ( Model, Cmd msg )
init flags =
    ( initialModel, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \n -> Sub.none
        , view = view
        }


getCardId : Card -> Int
getCardId card =
    case card of
        Image imageCard ->
            imageCard.id

        Description descriptionCard ->
            descriptionCard.id


compareCardIds : Card -> Card -> Button
compareCardIds cardA cardB =
    if getCardId cardA == getCardId cardB then
        Correct

    else
        Wrong


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FlipCard card ->
            let
                newModel =
                    selectedCardUpdate card model
                        |> flippedCardUpdate card

                compara =
                    case Maybe.map2 compareCardIds newModel.firstFlippedCard newModel.secondFlippedCard of
                        Just status ->
                            status

                        Nothing ->
                            Wrong
            in
            --update VerifyCardsSelected newModel
            ( { newModel | buttonStatus = compara }, Cmd.none )

        RemoveCards ->
            let
                newModel =
                    removeCards model.firstFlippedCard model
                    |> removeCards model.secondFlippedCard
            in
            ( newModel, Cmd.none )


removeCards : Maybe Card -> Model -> Model
removeCards maybeCard model =
  case maybeCard of
    Just card ->
      case card of
        Image imageCard ->
          let
            newListImageCard = List.filter (\modelImageCard -> modelImageCard.id /= imageCard.id) model.imageCards
          in
            { model | imageCards = newListImageCard}

        Description descriptionCard ->
          let
            newListDescriptionCard = List.filter (\modelDescriptionCard -> modelDescriptionCard.id /= descriptionCard.id) model.descriptionCards
          in
            { model | descriptionCards = newListDescriptionCard}

    Nothing ->
      model

mountButton : Button -> Html.Html Msg
mountButton buttonStatus =
    case buttonStatus of
        Correct ->
            div
                [ class "button-container" ]
                [ button
                    [ class "button-result btn-success", onClick RemoveCards ]
                    [ text "remover!" ]
                ]

        Wrong ->
            div
                [ class "button-container" ]
                [ button
                    [ class "button-result btn-wrong" ]
                    [ text "err!" ]
                ]


view model =
    div
        [ class "game-container" ]
        [ mountButton model.buttonStatus
        , div
            [ class "cards-container" ]
            [ div
                [ class "frame-description-cards" ]
                (List.map mountDescriptionCard model.descriptionCards)
            , div
                [ class "frame-image-cards" ]
                (List.map mountImageCard model.imageCards)
            ]
        ]


classActive : CardStatus -> String
classActive flipped =
    case flipped of
        FlippedIn status ->
            status

        FlippedOut status ->
            status


mountImageCard : ImageCard -> Html.Html Msg
mountImageCard imageCard =
    div
        [ class <| classActive imageCard.flipped ++ " image-card", onClick <| FlipCard (Image imageCard) ]
        [ div
            [ class "flip" ]
            [ div
                [ class "front" ]
                [ span [ class "back-detail" ] [] ]
            , div
                [ class "back" ]
                [ img
                    [ class "image", src <| "public/images/" ++ imageCard.imagePath ]
                    []
                , span
                    [ class "name-container" ]
                    [ text imageCard.name ]
                ]
            ]
        ]


mountDescriptionCard : DescriptionCard -> Html.Html Msg
mountDescriptionCard descriptionCard =
    div
        [ class <| classActive descriptionCard.flipped ++ " description-card", onClick <| FlipCard (Description descriptionCard) ]
        [ div
            [ class "flip" ]
            [ div
                [ class "text-container front" ]
                [ span [ class "back-detail" ] [] ]
            , div
                [ class "back" ]
                [ text descriptionCard.description ]
            ]
        ]


selectedCardUpdate : Card -> Model -> Model
selectedCardUpdate card model =
    case card of
        Image imageCard ->
            --case model.firstFlippedCard of
            --    Just _ ->
            --        { model | firstFlippedCard = Nothing }
            --
            --    Nothing ->
            { model | firstFlippedCard = Just (Image imageCard) }

        Description descriptionCard ->
            --case model.secondFlippedCard of
            --    Just _ ->
            --        { model | secondFlippedCard = Nothing }
            --
            --    Nothing ->
            { model | secondFlippedCard = Just (Description descriptionCard) }


flippedCardUpdate : Card -> Model -> Model
flippedCardUpdate card model =
    case card of
        Image imageCard ->
            let
                toggleFlipped cardInList =
                    case cardInList.flipped of
                        FlippedIn _ ->
                            { cardInList | flipped = FlippedOut "flippedOut" }

                        FlippedOut _ ->
                            if cardInList.id == imageCard.id then
                                { cardInList | flipped = FlippedIn "flippedIn" }

                            else
                                cardInList

                newListCards =
                    List.map toggleFlipped model.imageCards
            in
            { model | imageCards = newListCards }

        Description descriptionCard ->
            let
                toggleFlipped cardInList =
                    case cardInList.flipped of
                        FlippedIn _ ->
                            { cardInList | flipped = FlippedOut "flippedOut" }

                        FlippedOut _ ->
                            if cardInList.id == descriptionCard.id then
                                { cardInList | flipped = FlippedIn "flippedIn" }

                            else
                                cardInList

                newListCards =
                    List.map toggleFlipped model.descriptionCards
            in
            { model | descriptionCards = newListCards }


descriptionCards : List DescriptionCard
descriptionCards =
    [ DescriptionCard 1 "Es una persona muy valiente Siempre está mirando al mar Para salvar de repente Al que no sabe nadar." (FlippedOut "flippedOut")
    , DescriptionCard 2 "Se lleva entre la basura Papeles, zapatos viejos Desperdicios y procura Ir a tirarlos bien lejos." (FlippedOut "flippedOut")
    , DescriptionCard 3 "Maneja la camioneta Y también el ruletero, Si detiene en la banqueta Y transporta el pasajero." (FlippedOut "flippedOut")
    ]


imageCards : List ImageCard
imageCards =
    [ ImageCard 3 "MOTORISTA" "motorista.png" (FlippedOut "flippedOut")
    , ImageCard 2 "BASURERO" "basurero.png" (FlippedOut "flippedOut")
    , ImageCard 1 "SALVA VIDAS" "salva-vidas.png" (FlippedOut "flippedOut")
    ]
