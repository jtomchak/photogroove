module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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
    , selectedUrl : String
    }


init : ( Model, Cmd Msg )
init =
    ( { photos =
            [ { url = "1.jpeg" }
            , { url = "2.jpeg" }
            , { url = "3.jpeg" }
            ]
      , selectedUrl = "2.jpeg"
      }
    , Cmd.none
    )



---- UPDATE ----


type alias Msg =
    { operation : String, data : String }



--with only one action, we don't need to pattern match


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Message Data" msg.data
    in
    case msg.operation of
        "SELECTED_PHOTO" ->
            ( { model | selectedUrl = msg.data }, Cmd.none )

        "SUPRISE_ME" ->
            ( { model | selectedUrl = "2.jpeg" }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { operation = "SUPRISE_ME", data = "" } ]
            [ text "Surprise Me!" ]
        , div [ id "thumbnails" ]
            -- (List.map (\photo -> viewThumbnail model.selectedUrl photo) model.photos)
            -- partially applying a function..Definition needed audio
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]



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


viewThumbnail :
    String
    -> Photo
    -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick { operation = "SELECTED_PHOTO", data = thumbnail.url }
        ]
        []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
