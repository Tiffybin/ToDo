module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, id, name, placeholder, tabindex, type_, value)
import Html.Events exposing (onClick, onInput)
import Html5.DragDrop
import Json.Encode
import List.Extra
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { items = [], userInput = "", dragDrop = Html5.DragDrop.init  }, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
        --time = Time.millisToPosix 0


type alias Model =
    { items : List Bullet
    , userInput : String
    , dragDrop : Html5.DragDrop.Model DragId DropId
    -- , time : Time.Posix
    }


type alias DragId =
    Int


type alias DropId =
    Int


type alias Bullet =
    { title : String }


type Msg
    = Change String
    | Delete Int
    | Add
    | Edit Int String
    | DragDropMsg (Html5.DragDrop.Msg DragId DropId)
    -- | Time Time.Posix


remove : Int -> List Bullet -> List Bullet
remove i list =
    List.Extra.removeAt i list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        x =
            Debug.log "logging this" (Json.Encode.encode 0 (encoder model.items))
    in
    case msg of
        Delete i ->
            ( { model | items = remove i model.items }, Cmd.none )

        Add ->
            ( { model | items = { title = model.userInput } :: model.items }, Cmd.none )

        Change userI ->
            ( { model | userInput = userI }, Cmd.none )

        Edit i newS ->
            ( { model | items = List.Extra.updateAt i (editBullet newS) model.items }, Cmd.none )

        DragDropMsg dragDrop ->
            let
                ( model_, result ) =
                    Html5.DragDrop.update dragDrop model.dragDrop
            in
            ( { model
                | dragDrop = model_
                , items =
                    case result of
                        Just ( dragI, dropI, _ ) ->
                            moveId dragI dropI model.items

                        Nothing ->
                            model.items
              }
            , Cmd.none
            )

        -- Time psx ->
        --     ( Debug.log "logging 2" { model | time = psx }, getTime )


-- getTime : Cmd Msg
-- getTime =
--     Task.perform Time Time.now


editBullet : String -> Bullet -> Bullet
editBullet newString bullet =
    { bullet | title = newString }


view : Model -> Html Msg
view model =
    div [ class "background" ]
        [ p [ class "text-center fs-1 fw-bold font-monospace text-title " ] [ text "To-Do List" ]
        , div [] []
        -- , h1 [] [ text (String.fromInt (Time.posixToMillis model.time)) ]
        , div [ class "d-flex justify-content-center align-items-center" ]
            [ div [ class "d-flex mb-3" ]
                [ input [ class "form-control me-3", placeholder "Write something", value model.userInput, onInput Change ] []
                , button [ class "btn button", onClick Add ] [ text "+" ]
                ]
            ]
        , div [ class "d-flex justify-content-center" ]
            [ ul [ class "list-unstyled flex-column align-items-center" ]
                (List.Extra.interweave (List.Extra.initialize (List.length model.items + 1) viewDropZone)
                    (List.indexedMap viewBullet model.items)
                )
            ]
        ]


getModalId : Int -> String
getModalId i =
    "id" ++ String.fromInt i


viewModal : Int -> Bullet -> Html Msg
viewModal i b =
    div [ class "modal fade", id (getModalId i), tabindex -1 ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text ("Additional Details for " ++ b.title) ]
                    , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
                    ]
                , div [ class "modal-body" ]
                    [ Html.form []
                        [ div [ class "form-group" ]
                            [ label [ for "notes", class "col-form-label" ] [ text "Notes:" ]
                            , input [ type_ "text", class "form-control", id "notes" ] []
                            ]
                        ]
                    ]
                , div [] [ text "Time" ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "radio1" ] [ text "Morning" ]
                    , input [ class "form-check-input", type_ "radio", name "radio", id "radio1" ] []
                    ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "radio2" ] [ text "Afternoon" ]
                    , input [ class "form-check-input", type_ "radio", name "radio", id "radio2" ] []
                    ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "radio3" ] [ text "Evening" ]
                    , input [ class "form-check-input", type_ "radio", name "radio", id "radio3" ] []
                    ]
                , div [] [ text "Progress" ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "progress1" ] [ text "Not Started" ]
                    , input [ class "form-check-input", type_ "radio", name "progress", id "progress1" ] []
                    ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "progress2" ] [ text "In Progress" ]
                    , input [ class "form-check-input", type_ "radio", name "progress", id "progress2" ] []
                    ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label", for "progress3" ] [ text "Completed" ]
                    , input [ class "form-check-input", type_ "radio", name "progress", id "progress3" ] []
                    ]
                , div [ class "modal-footer" ]
                    [ button [ type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                    , button [ type_ "button", class "btn button" ] [ text "Save changes" ]
                    ]
                ]
            ]
        ]


moveId : DragId -> DropId -> List a -> List a
moveId dragId dropId items =
    let
        to =
            if dragId == dropId then
                dragId

            else if dragId < dropId then
                dropId - 1

            else
                dropId

        maybeItem =
            List.Extra.getAt dragId items
    in
    case maybeItem of
        Just item ->
            insertItem to item (List.Extra.removeAt dragId items)

        Nothing ->
            items


insertItem : Int -> a -> List a -> List a
insertItem index item list =
    let
        before =
            List.take index list

        after =
            List.drop index list
    in
    before ++ [ item ] ++ after


viewDropZone : Int -> Html Msg
viewDropZone index =
    hr (Html5.DragDrop.droppable DragDropMsg index ++ [ class "m-2" ]) []


viewBullet : Int -> Bullet -> Html Msg
viewBullet i b =
    div (Html5.DragDrop.draggable DragDropMsg i)
        [ li []
            [ input [ value b.title, onInput (Edit i) ] []
            , button [ class "btn button-small", onClick (Delete i) ] [ text "-" ]
            , button [ class "btn button-small", type_ "button", attribute "data-bs-toggle" "modal", attribute "data-bs-target" ("#" ++ getModalId i) ] [ text "i" ]
            , viewModal i b
            ]
        ]


encoder : List Bullet -> Json.Encode.Value
encoder items =
    Json.Encode.list encodeBullet items


encodeBullet : Bullet -> Json.Encode.Value
encodeBullet b =
    Json.Encode.object [ ( "input", Json.Encode.string b.title ) ]
