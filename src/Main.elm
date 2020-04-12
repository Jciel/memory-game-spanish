module Main exposing (..)

import Browser exposing (element)
import Html exposing (button, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)


type CardStatus
    = FlippedIn String
    | FlippedOut String


type Button
    = Correct
    | Wrong


type Card
    = Description DescriptionCard
    | Image ImageCard


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


type alias Model =
    { imageCardFlipped : Maybe Card
    , descriptionCardFlipped : Maybe Card
    , descriptionCards : List DescriptionCard
    , imageCards : List ImageCard
    , buttonResponse : Button
    }


type Msg
    = FlipCard Card
    | RemoveCards
    | ShuffledImageCards (List ImageCard)
    | ShuffledDescriptionCards (List DescriptionCard)


initialModel : Model
initialModel =
    Model Nothing Nothing descriptionCards imageCards Wrong


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel, generate ShuffledImageCards (shuffle imageCards) )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \n -> Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FlipCard card ->
            let
                newModel =
                    selectedCardUpdate card model
                        |> flippedCardUpdate card

                compareIds =
                    case Maybe.map2 compareCardIds newModel.imageCardFlipped newModel.descriptionCardFlipped of
                        Just status ->
                            status

                        Nothing ->
                            Wrong
            in
            ( { newModel | buttonResponse = compareIds }, Cmd.none )

        RemoveCards ->
            let
                newModel =
                    removeCards model.imageCardFlipped model
                        |> removeCards model.descriptionCardFlipped
            in
            ( newModel, Cmd.none )

        ShuffledImageCards shuffledList ->
            ( { model | imageCards = shuffledList }, generate ShuffledDescriptionCards (shuffle descriptionCards) )

        ShuffledDescriptionCards shuffledList ->
            ( { model | descriptionCards = shuffledList }, Cmd.none )


view model =
    div
        [ class "game-container" ]
        [ mountButton model.buttonResponse
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


mountButton : Button -> Html.Html Msg
mountButton buttonResponse =
    case buttonResponse of
        Correct ->
            div
                [ class "button-container" ]
                [ button
                    [ class "button-result btn-success", onClick RemoveCards ]
                    [ text "Remove Cards" ]
                ]

        Wrong ->
            div
                [ class "button-container" ]
                [ button
                    [ class "button-result btn-wrong" ]
                    [ text "Wrong" ]
                ]


classActive : CardStatus -> String
classActive flipped =
    case flipped of
        FlippedIn status ->
            status

        FlippedOut status ->
            status


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


selectedCardUpdate : Card -> Model -> Model
selectedCardUpdate card model =
    case card of
        Image imageCard ->
            { model | imageCardFlipped = Just (Image imageCard) }

        Description descriptionCard ->
            { model | descriptionCardFlipped = Just (Description descriptionCard) }


removeCards : Maybe Card -> Model -> Model
removeCards maybeCard model =
    case maybeCard of
        Just card ->
            case card of
                Image imageCard ->
                    let
                        newListImageCard =
                            List.filter (\modelImageCard -> modelImageCard.id /= imageCard.id) model.imageCards
                    in
                    { model | imageCards = newListImageCard }

                Description descriptionCard ->
                    let
                        newListDescriptionCard =
                            List.filter (\modelDescriptionCard -> modelDescriptionCard.id /= descriptionCard.id) model.descriptionCards
                    in
                    { model | descriptionCards = newListDescriptionCard }

        Nothing ->
            model


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
    , DescriptionCard 4 "Su maquinita hace ruido Trabajando sin parar Y me hace un lindo vestido Para poderlo estrenar." (FlippedOut "flippedOut")
    , DescriptionCard 5 "Con las manos en la masa Está en la panadería Para que no falte en casa Nuestro pan de cada día." (FlippedOut "flippedOut")
    , DescriptionCard 6 "Día y noche cuida al niño Recluido en el hospital Y lo trata con cariño Para que no se sienta mal." (FlippedOut "flippedOut")
    , DescriptionCard 7 "Con letra clara ha escrito 'a b c' en el pizarrón, Lo copio pues necesito Aprenderme la lección." (FlippedOut "flippedOut")
    , DescriptionCard 8 "Trabajando silenciosa/o La/lo miro por todos lados; Barre, tropea la casa Y corre a hacer los mandados." (FlippedOut "flippedOut")
    , DescriptionCard 9 "Hay que usar el teodolito Para medir el terreno Construir un puente bonito Y adornarlo en el estreno." (FlippedOut "flippedOut")
    , DescriptionCard 10 "Por comer mil golosinas Y otros dulces diferentes La “doctora” me examina Para cuidarme los dientes." (FlippedOut "flippedOut")
    , DescriptionCard 11 "Mi perrito estaba triste El pobre se me enfermó Me lo inyectaron ¿ya viste? Y rápido se curó." (FlippedOut "flippedOut")
    , DescriptionCard 12 "Al quemarse un edificio Los llaman y llegan luego; Haciendo gran sacrificio Logran apagar el fuego." (FlippedOut "flippedOut")
    ]


imageCards : List ImageCard
imageCards =
    [ ImageCard 1 "SALVA VIDAS" "salva-vidas.png" (FlippedOut "flippedOut")
    , ImageCard 2 "BASURERO/A" "basurero.png" (FlippedOut "flippedOut")
    , ImageCard 3 "MOTORISTA" "motorista.png" (FlippedOut "flippedOut")
    , ImageCard 4 "COSTURERA/O" "costurero.png" (FlippedOut "flippedOut")
    , ImageCard 5 "PANADERO/A" "panadero.png" (FlippedOut "flippedOut")
    , ImageCard 6 "ENFERMERO/A" "enfermero.png" (FlippedOut "flippedOut")
    , ImageCard 7 "MAESTRO/A" "maestro.png" (FlippedOut "flippedOut")
    , ImageCard 8 "LIMPIADOR/A" "limpiador.png" (FlippedOut "flippedOut")
    , ImageCard 9 "INGENIERO/A" "ingeniero.png" (FlippedOut "flippedOut")
    , ImageCard 10 "DENTISTA" "dentista.png" (FlippedOut "flippedOut")
    , ImageCard 11 "VETERINARIO/A" "veterinario.png" (FlippedOut "flippedOut")
    , ImageCard 12 "BOMBERO/A" "bombero.png" (FlippedOut "flippedOut")
    ]
