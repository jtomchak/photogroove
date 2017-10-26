module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


---- MODEL ----
-- getting this right is gonna take a lot of practice. I keep wanting to dive into the implementation, and the compiler is telling me NOPE, not yet. But the pay off is once the compiler is happy, it's always working from the browser!
--We're saying that a Photo is the same type as a record with property url as a sting, so we can use type Photo where we'd usually say { url : String }


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


init : ( Model, Cmd Msg )
init =
    ( { photos = []
      , selectedUrl = Nothing
      , loadingError = Nothing
      , chosenSize = Medium
      }
    , Cmd.none
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
                -- 1 case expression
                -- newSelectedPhoto : Maybe Photo
                -- newSelectedPhoto =
                --     Array.get index (Array.fromList model.photos)
                -- newSelectedUrl : Maybe String
                -- newSelectedUrl =
                --     case newSelectedPhoto of
                --         Just photo ->
                --             Just photo.url
                --         Nothing ->
                --             Nothing
                -- 2 maybe map
                -- newSelectedUrl : Maybe String
                -- newSelectedUrl =
                --     Maybe.map (\photo -> photo.url) (Array.get index (Array.fromList model.photos))
                -- 3 pipes
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
            ( { model | selectedUrl = newSelectedUrl }, Cmd.none )



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

        -- [ viewSizeChoose Small, viewSizeChoose Medium, viewSizeChoose Large ]
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            -- (List.map (\photo -> viewThumbnail model.selectedUrl photo) model.photos)
            -- partially applying a function..Definition needed audio
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



--type infered copy to code is super nice
--classList remindes me of ng-class from AngularJS
--on shit it's like connect(mapStatetoProps)(App) *facepalm*
{--Version in Javascipt, bc JS -> Elm
“function viewThumbnail(selectedUrl) {
    return function(thumbnail) {
        if (selectedUrl === thumbnail.url) {
            // Render a selected thumbnail here
        } else {
            // Render a non-selected thumbnail here
        }
    };
}”
Excerpt From: Richard Feldman. “Elm in Action MEAP V05.” iBooks.
--}
--Before we added an onclick to it, now it's Html and a record msg?
-- viewThumbnail : String -> { a | url : String } -> Html msg


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []



--helper function with radio buttons to choose size,
--_type bc type is a reserved word in Elm.


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



--no need for a default. we've defined thumbnailsize, and it can only be one of these 3. that's pretty rad.


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
