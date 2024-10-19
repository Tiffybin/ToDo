port module Main exposing (..)

import Browser
import Dict
import Environment
import Firestore
import Firestore.Codec as Codec
import Firestore.Types.Reference as Reference
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, for, id, name, placeholder, tabindex, type_, value)
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

    -- , firestore : Firestore.Firestore
    -- , document : Maybe (Firestore.Document Document)
    }


type alias Document =
    { reference : Reference.Reference
    , integer : Int
    , string : String
    , list : List String
    , map : Dict.Dict String String
    , boolean : Bool
    , nullable : Maybe String
    }


type alias DragId =
    Int


type alias DropId =
    Int


type alias Bullet =
    { title : String
    , time : Time.Posix
    , timeOfDay : Maybe TimeOfDay
    , progress : Maybe Status
    , checked : Bool
    }


type TimeOfDay
    = Morning
    | Evening
    | Afternoon


type Status
    = Completed
    | NotStarted
    | InProgress


type Msg
    = Change String
    | Delete Int
    | AddBefore
    | AddAfter Time.Posix
    | Edit Int String
    | DragDropMsg (Html5.DragDrop.Msg DragId DropId)
    | Time Int Time.Posix
    | IntialZone Time.Zone
    | UpdateTimeOfDay Int TimeOfDay
    | Progress Int Status
    | CheckedOff Int Bool


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
                    ( { model | items = { title = model.userInput, time = psx, timeOfDay = Maybe.Nothing, progress = Maybe.Nothing, checked = False } :: model.items }, Cmd.none )

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

                UpdateTimeOfDay i timeOfDay ->
                    ( { model | items = List.Extra.updateAt i (updateTimeOfDay (Just timeOfDay)) model.items }, Cmd.none )

                Progress i status ->
                    ( { model | items = List.Extra.updateAt i (updateProgress (Just status)) model.items }, Cmd.none )

                CheckedOff i bool ->
                    ( { model | items = List.Extra.updateAt i (updateCheckedOff bool) model.items }, Cmd.none )

        x =
            Debug.log "logging this" (Json.Encode.encode 0 (encoder newModel.items))
    in
    ( newModel, Cmd.batch [ save x, cmd ] )


updateCheckedOff : Bool -> Bullet -> Bullet
updateCheckedOff bool b =
    { b | checked = bool }


updateProgress : Maybe Status -> Bullet -> Bullet
updateProgress status b =
    { b | progress = status }


updateTimeOfDay : Maybe TimeOfDay -> Bullet -> Bullet
updateTimeOfDay t b =
    { b | timeOfDay = t }


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
                        , checkTime b i "Morning" Morning
                        , checkTime b i "Afternoon" Afternoon
                        , checkTime b i "Evening" Evening
                        , div [] [ text "Progress" ]
                        , checkStatus b i "Completed" Completed
                        , checkStatus b i "In Progress" InProgress
                        , checkStatus b i "Not Started" NotStarted
                        , div [ class "modal-footer" ]
                            [ button [ type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                            , button [ type_ "button", class "btn button" ] [ text "Save changes" ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text "No zone"


checkTime : Bullet -> Int -> String -> TimeOfDay -> Html Msg
checkTime b i str timeOfDay =
    div [ class "form-check" ]
        [ label [ class "form-check-label", for str ] [ text str ]
        , input
            [ checked
                (b.timeOfDay == Just timeOfDay)
            , onClick (UpdateTimeOfDay i timeOfDay)
            , class "form-check-input"
            , type_ "radio"
            , name "radio"
            , id str
            ]
            []
        ]


checkStatus : Bullet -> Int -> String -> Status -> Html Msg
checkStatus b i str status =
    div [ class "form-check" ]
        [ label [ class "form-check-label", for str ] [ text str ]
        , input
            [ checked
                (b.progress == Just status)
            , onClick (Progress i status)
            , class "form-check-input"
            , type_ "radio"
            , name "radio"
            , id str
            ]
            []
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
                    [ div [ class "d-flex justify-content-between align-items-center" ]
                        [ div [ class "form-check me-2" ]
                            [ input [ class "form-check-input", type_ "checkbox", value "", id "unchecked" ] []
                            , label [ class "form-check-label", for "unchecked" ] []
                            ]
                        , input [ value bullet.title, onInput (Edit i) ] []
                        , div []
                            [ button [ class "btn button-small", onClick (Delete i) ] [ text "-" ]
                            , button [ class "btn button-small", type_ "button", attribute "data-bs-toggle" "modal", attribute "data-bs-target" ("#" ++ getModalId i) ] [ text "i" ]
                            , viewModal model i bullet
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text "No bullet"


encoder : List Bullet -> Json.Encode.Value
encoder items =
    Json.Encode.list encodeBullet items


encodeBullet : Bullet -> Json.Encode.Value
encodeBullet b =
    Json.Encode.object [ ( "input", Json.Encode.string b.title ), ( "time", Iso.encode b.time ), ( "timeOfDay", encodeTimeOfDayToJson b.timeOfDay ), ( "progress", encodeStatusToJson b.progress ), ( "checked", Json.Encode.bool b.checked ) ]


encodeStatusToJson : Maybe Status -> Json.Encode.Value
encodeStatusToJson status =
    case status of
        Just aStatus ->
            let
                stat =
                    encodeStatus aStatus
            in
            Json.Encode.string stat

        Nothing ->
            Json.Encode.null


encodeStatus : Status -> String
encodeStatus s =
    case s of
        Completed ->
            "Completed"

        InProgress ->
            "In Progress"

        NotStarted ->
            "Not Started"


encodeTimeOfDayToJson : Maybe TimeOfDay -> Json.Encode.Value
encodeTimeOfDayToJson t =
    case t of
        Just time ->
            let
                to =
                    encodeTimeOfDay time
            in
            Json.Encode.string to

        Nothing ->
            Json.Encode.null


encodeTimeOfDay : TimeOfDay -> String
encodeTimeOfDay t =
    case t of
        Morning ->
            "Morning"

        Afternoon ->
            "Afternoon"

        Evening ->
            "Evening"


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
    Decode.map5 Bullet
        (Decode.field "input" Decode.string)
        (Decode.field "time" Iso.decoder)
        (Decode.field "timeOfDay" (Decode.maybe decodeTimeOfDay))
        (Decode.field "progress" (Decode.maybe decodeStatus))
        (Decode.field "checked" Decode.bool)


decodeTimeOfDay : Decode.Decoder TimeOfDay
decodeTimeOfDay =
    Decode.oneOf [ decoderExactString "Morning" Morning, decoderExactString "Afternoon" Afternoon, decoderExactString "Evening" Evening ]


decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.oneOf [ decoderExactString "Completed" Completed, decoderExactString "In Progress" InProgress, decoderExactString "Not Started" NotStarted ]


decoderExactString : String -> b -> Decode.Decoder b
decoderExactString expected t =
    Decode.string
        |> Decode.andThen
            (\actual ->
                if actual == expected then
                    Decode.succeed t

                else
                    Decode.fail "doesn't match"
            )


codecBullet : Codec.Codec Bullet
codecBullet =
    Codec.document Bullet
        |> Codec.required "title" .title Codec.string
        |> Codec.required "time" .time Codec.timestamp
        |> Codec.required "timeOfDay" .timeOfDay (Codec.maybe codecTime)
        |> Codec.required "progress" .progress (Codec.maybe codecStatus)
        |> Codec.required "checked" .checked Codec.bool
        |> Codec.build


codecTime : Codec.Field TimeOfDay
codecTime =
    codecOneOf
        encodeTimeOfDay
        Codec.string
        [ ( "Morning", Morning )
        , ( "Afternoon", Afternoon )
        , ( "Evening", Evening )
        ]


codecStatus : Codec.Field Status
codecStatus =
    codecOneOf
        encodeStatus
        Codec.string
        [ ( "Completed", Completed )
        , ( "In Progress", InProgress )
        , ( "Not Started", NotStarted )
        ]



-- codecExactString : String -> TimeOfDay -> Codec.Field TimeOfDay
-- codecExactString expected t =
--     Codec.string
--         |> Codec.andThen
--             (\actual ->
--                 if actual == expected then
--                     Codec.succeed t
--                 else
--                     Codec.fail "failed"
--             )
--             encodeTimeOfDay
-- codecExactString : String -> TimeOfDay
-- codecExactString expected t =
--     Codec.string
--         |> Codec.andThen
--             (\actual ->
--                 if actual == expected then
--                     Codec.succeed t
--                 else
--                     Codec.fail "failed"
--             )
--             -- encodeTimeOfDay
--             -- --Time-String


codecOneOf : (a -> b) -> Codec.Field b -> List ( b, a ) -> Codec.Field a
codecOneOf show codec =
    let
        tryCodec ( expected, wouldProduce ) restCodec =
            codec
                |> Codec.andThen
                    (\actual ->
                        if expected == actual then
                            Codec.succeed wouldProduce

                        else
                            restCodec
                    )
                    show
    in
    List.foldr tryCodec (Codec.fail "None matched")
