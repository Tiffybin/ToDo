module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, id, placeholder, tabindex, type_, value)
import Html.Events exposing (onClick, onInput)
import Html5.DragDrop
import List.Extra


main : Program () Model Msg
main =
    Browser.sandbox { init = { items = [], userInput = "", dragDrop = Html5.DragDrop.init }, update = update, view = view }


type alias Model =
    { items : List Bullet
    , userInput : String
    , dragDrop : Html5.DragDrop.Model DragId DropId
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


remove : Int -> List Bullet -> List Bullet
remove i list =
    List.Extra.removeAt i list


update : Msg -> Model -> Model
update msg model =
    case msg of
        Delete i ->
            { model | items = remove i model.items }

        Add ->
            { model | items = { title = model.userInput } :: model.items }

        Change userI ->
            { model | userInput = userI }

        Edit i newS ->
            { model | items = List.Extra.updateAt i (editBullet newS) model.items }

        DragDropMsg dragDrop ->
            let
                ( model_, result ) =
                    Html5.DragDrop.update dragDrop model.dragDrop
            in
            { model
                | dragDrop = model_
                , items =
                    case result of
                        Just ( dragI, dropI, _ ) ->
                            moveId dragI dropI model.items

                        Nothing ->
                            model.items
            }


editBullet : String -> Bullet -> Bullet
editBullet newString bullet =
    { bullet | title = newString }


view : Model -> Html Msg
view model =
    div []
        [ text "To-Do List"
        , div [] []
        , input [ placeholder "Write something", value model.userInput, onInput Change ] []
        , button [ class "btn btn-primary", onClick Add ] [ text "+" ]
        , ul [] (List.Extra.interweave (List.indexedMap viewBullet model.items) (List.indexedMap viewDropZone model.items))
        ]
        


getModalId : Int -> String
getModalId i =
    "id" ++ String.fromInt i


viewModal : Int -> Html Msg
viewModal i =
    div [ class "modal fade", id (getModalId i), tabindex -1 ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text "" ]
                    , button [ type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
                    ]
                , div [ class "modal-body" ]
                    [ text ""
                    ]
                , div [ class "modal-footer" ]
                    [ button [ type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                    , button [ type_ "button", class "btn btn-primary" ] [ text "Save changes" ]
                    ]
                ]
            ]
        ]


moveId : DragId -> DropId -> List Bullet -> List Bullet
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


viewDropZone : Int -> Bullet ->Html Msg
viewDropZone index b=
    div ( Html5.DragDrop.droppable DragDropMsg index ++ [class "m-2"])[]


viewBullet : Int -> Bullet -> Html Msg
viewBullet i b =
    div (Html5.DragDrop.draggable DragDropMsg i)[
    li []
        [ input [ value b.title, onInput (Edit i) ] []
        , button [ onClick (Delete i) ] [ text "-" ]
        , button [ type_ "button", attribute "data-bs-toggle" "modal", attribute "data-bs-target" ("#" ++ getModalId i) ] [ text "i" ]
        , viewModal i
        ]
    ]
