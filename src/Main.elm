port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, id, name, placeholder, tabindex, type_, value)
import Html.Events exposing (onClick, onInput)
import Html5.DragDrop
import Iso8601 as Iso
import Json.Decode as Decode
import Json.Encode
import List.Extra
import Task
import Time


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : String -> ( Model, Cmd Msg )
init s =
    ( { items = decoder s, userInput = "", dragDrop = Html5.DragDrop.init, zone = Maybe.Nothing }, getZone )


type alias Model =
    { items : List Bullet
    , userInput : String
    , dragDrop : Html5.DragDrop.Model DragId DropId
    , zone : Maybe Time.Zone
    }


type alias DragId =
    Int


type alias DropId =
    Int


type alias Bullet =
    { title : String, time : Time.Posix }


type Msg
    = Change String
    | Delete Int
    | AddBefore
    | AddAfter Time.Posix
    | Edit Int String
    | DragDropMsg (Html5.DragDrop.Msg DragId DropId)
    | Time Int Time.Posix
    | IntialZone Time.Zone


remove : Int -> List Bullet -> List Bullet
remove i list =
    List.Extra.removeAt i list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            case msg of
                Delete i ->
                    ( { model | items = remove i model.items }, Cmd.none )

                AddAfter psx ->
                    ( { model | items = { title = model.userInput, time = psx } :: model.items }, Cmd.none )

                AddBefore ->
                    ( model, getTime )

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

                Time i psx ->
                    ( { model | items = List.Extra.updateAt i (updateTime psx) model.items }, Cmd.none )

                IntialZone zn ->
                    ( { model | zone = Just zn }, Cmd.none )

        x =
            Debug.log "logging this" (Json.Encode.encode 0 (encoder newModel.items))
    in
    ( newModel, Cmd.batch [ save x, cmd ] )


updateTime : Time.Posix -> Bullet -> Bullet
updateTime t b =
    { b | time = t }


getZone : Cmd Msg
getZone =
    Task.perform IntialZone Time.here


getTime : Cmd Msg
getTime =
    Task.perform AddAfter Time.now


editBullet : String -> Bullet -> Bullet
editBullet newString bullet =
    { bullet | title = newString }


view : Model -> Html Msg
view model =
    div [ class "background" ]
        [ p [ class "text-center fs-1 fw-bold font-monospace text-title " ] [ text "To-Do List" ]
        , div [] []
        , div [ class "d-flex justify-content-center align-items-center" ]
            [ div [ class "d-flex mb-3" ]
                [ input [ class "form-control me-3", placeholder "Write something", value model.userInput, onInput Change ] []
                , button [ class "btn button", onClick AddBefore ] [ text "+" ]
                ]
            ]
        , div [ class "d-flex justify-content-center" ]
            [ ul [ class "list-unstyled flex-column align-items-center" ]
                (List.Extra.interweave (List.Extra.initialize (List.length model.items + 1) viewDropZone)
                    (List.indexedMap (\i _ -> viewBullet i model) model.items)
                )
            ]
        ]


getModalId : Int -> String
getModalId i =
    "id" ++ String.fromInt i


standardTime : Int -> String
standardTime time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


viewModal : Model -> Int -> Bullet -> Html Msg
viewModal model i b =
    let
        maybeZone =
            model.zone
    in
    case maybeZone of
        Just zone ->
            let
                hour =
                    standardTime (Time.toHour zone b.time)

                minutes =
                    standardTime (Time.toMinute zone b.time)

                seconds =
                    standardTime (Time.toSecond zone b.time)
            in
            div [ class "modal fade", id (getModalId i), tabindex -1 ]
                [ div [ class "modal-dialog" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-header" ]
                            [ h3 [ class "modal-title" ] [ text ("Additional Details for " ++ b.title) ]
                            , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
                            ]
                        , div [ class "modal-body" ]
                            [ Html.form []
                                [ div [ class "form-group" ]
                                    [ h5 [] [ text ("Created at " ++ hour ++ ":" ++ minutes ++ ":" ++ seconds) ]
                                    , label [ for "notes", class "col-form-label" ] [ text "Notes:" ]
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

        Nothing ->
            text "No zone"


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


viewBullet : Int -> Model -> Html Msg
viewBullet i model =
    let
        maybeBullet =
            List.Extra.getAt i model.items
    in
    case maybeBullet of
        Just bullet ->
            div (Html5.DragDrop.draggable DragDropMsg i)
                [ li []
                    [ input [ value bullet.title, onInput (Edit i) ] []
                    , button [ class "btn button-small", onClick (Delete i) ] [ text "-" ]
                    , button [ class "btn button-small", type_ "button", attribute "data-bs-toggle" "modal", attribute "data-bs-target" ("#" ++ getModalId i) ] [ text "i" ]
                    , viewModal model i bullet
                    ]
                ]

        Nothing ->
            text "No Bullet"


encoder : List Bullet -> Json.Encode.Value
encoder items =
    Json.Encode.list encodeBullet items


encodeBullet : Bullet -> Json.Encode.Value
encodeBullet b =
    Json.Encode.object [ ( "input", Json.Encode.string b.title ), ( "time", Iso.encode b.time ) ]


port save : String -> Cmd msg


decoder : String -> List Bullet
decoder string =
    case Decode.decodeString decodeListBullets string of
        Ok str ->
            str

        Err _ ->
            []


decodeListBullets : Decode.Decoder (List Bullet)
decodeListBullets =
    Decode.list decodeBullet


decodeBullet : Decode.Decoder Bullet
decodeBullet =
    Decode.map2 Bullet
        (Decode.field "input" Decode.string)
        (Decode.field "time" Iso.decoder)
