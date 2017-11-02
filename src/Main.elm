module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random


---- MODEL ----

type alias Photo =
    { url : String }


photoArray : Array Photo
photoArray =
    Array.fromList (Tuple.first init).photos


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    }


initialCmd : Cmd Msg
initialCmd =
    "http://elm-in-action.com/photos/list"
        |> Http.getString
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
    | LoadPhotos (Result Http.Error String)


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

        LoadPhotos (Ok responseStr) ->
                    let
                        urls =
                            String.split "," responseStr

                        photos =
                            List.map Photo urls
                    in
                    ( { model
                        | photos = photos
                        , selectedUrl = List.head urls
                      }
                    , Cmd.none
                    )

        LoadPhotos (Err _) ->
            ( { model | loadingError = Just "Error! (Try turning it off and on again?)"}, Cmd.none)



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
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


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
            div [ class "error-message"]
                [ h1 [] [text "Photo Groove"] 
                , p [] [text errorMessage]
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
