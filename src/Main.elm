module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, checked)
import Html.Events exposing (onClick)
import Json.Decode exposing (string, int, list, Decoder, field, map3, maybe)
import Http
import Random


---- MODEL ----
-- updated the alias type of our photo to reflect
-- the additional properties of it.


type alias Photo =
    { url : String
    , size : Int
    , title : Maybe String
    }


photoDecoder : Decoder Photo
photoDecoder =
    map3 Photo
        (field "url" string)
        (field "size" int)
        (maybe (field "title" string))


photoArray : Array Photo
photoArray =
    Array.fromList (Tuple.first init).photos


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    }



{-
   http.get takes a decoder and string, returns a Request value, not just a Request String like getString. Well, we build a decoder.
-}


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


init : ( Model, Cmd Msg )
init =
    ( { photos = []
      , selectedUrl = Nothing
      , loadingError = Nothing
      , chosenSize = Medium
      }
    , initialCmd
    )



---- UPDATE ----
--Defining a union type


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))


getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            Just photo.url

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Message Data" msg
    in
        --case expression
        case msg of
            SelectByUrl url ->
                ( { model | selectedUrl = Just url }, Cmd.none )

            SurpriseMe ->
                let
                    randomPhotoPicker : Random.Generator Int
                    randomPhotoPicker =
                        Random.int 0 (List.length model.photos - 1)
                in
                    ( model, Random.generate SelectByIndex randomPhotoPicker )

            SetSize size ->
                ( { model | chosenSize = size }, Cmd.none )

            SelectByIndex index ->
                let
                    newSelectedUrl : Maybe String
                    newSelectedUrl =
                        model.photos
                            |> Array.fromList
                            |> Array.get index
                            |> Maybe.map .url
                in
                    ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

            LoadPhotos (Ok response) ->
                ( { model
                    | photos = response
                    , selectedUrl = Maybe.map .url (List.head response)
                  }
                , Cmd.none
                )

            LoadPhotos (Err _) ->
                ( { model | loadingError = Just "Error! (Try turning it off and on again?)" }, Cmd.none )



---- VIEW ----


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChoose model.chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , viewLarge model.selectedUrl
        ]


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            img
                [ class "large"
                , src (urlPrefix ++ "large/" ++ url)
                ]
                []


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)

        -- , hasTitle thumbnail.title thumbnail.size
        , title ((Maybe.withDefault "None" thumbnail.title) ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


hasTitle : Maybe String -> Int -> Attribute msg
hasTitle maybeTitle size =
    case maybeTitle of
        Nothing ->
            title "None"

        Just photoTitle ->
            title (photoTitle ++ " [" ++ toString size ++ " KB]")


viewSizeChoose : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChoose chosenSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (SetSize size)
            , if size == chosenSize then
                checked True
              else
                checked False
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = viewOrError
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
