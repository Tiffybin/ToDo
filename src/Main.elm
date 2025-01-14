port module Main exposing (..)

import Browser
import Dict
import Environment
import Firestore
import Firestore.Codec as Codec
import Firestore.Config
import Firestore.Options.List
import Firestore.Types.Reference as Reference
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, name, placeholder, tabindex, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html5.DragDrop
import Iso8601 as Iso
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode
import List.Extra
import Task
import Time
import Tuple


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : String -> ( Model, Cmd Msg )
init user =
    let
        firestore =
            Firestore.Config.new
                { apiKey = Environment.apiKey
                , project = Environment.projectId
                }
                |> Firestore.init
    in
    ( { items = decoder user, userInput = "", dragDrop = Html5.DragDrop.init, zone = Maybe.Nothing, firestore = firestore, searchedString = "", index = Maybe.Nothing, list = [], listInput = "" }
    , Cmd.batch
        [ getZone
        , getFromDb firestore
        ]
    )


toTask : Result error a -> Task.Task error a
toTask result =
    case result of
        Err r ->
            Task.fail r

        Ok r ->
            Task.succeed r


type alias Model =
    { items : List Bullet
    , userInput : String
    , dragDrop : Html5.DragDrop.Model DragId DropId
    , zone : Maybe Time.Zone
    , firestore : Firestore.Firestore
    , searchedString : String
    , index : Maybe Int
    , list : List Nav
    , listInput : String
    }


type alias Nav =
    String


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
    , dropdownStatus : Maybe DropDownStatus
    , selectedMonth : String
    , nav : Nav
    , day : Int
    }


type TimeOfDay
    = Morning
    | Evening
    | Afternoon


type Status
    = Completed
    | NotStarted
    | InProgress


type DropDownStatus
    = Month
    | NavSelect


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
    | Get (Result Firestore.Error (Firestore.Documents Bullet))
    | Upsert (Result Firestore.Error (Firestore.Document Bullet))
    | DeleteFromDB (Result Firestore.Error ())
    | GetFromDB
    | SearchAndRemoved String
    | Search String
    | UpdateIndex (Maybe Int)
    | OpenDropdown Int DropDownStatus
    | SelectedMonth Int String
    | IncrementDay Int
    | DecrementDay Int
    | SignOut
    | AddList
    | EditNav String
    | SelectedNav Int Nav
    | DeleteNav String
    | GetNav (Result Firestore.Error (Firestore.Documents Nav))


remove : Int -> List Bullet -> List Bullet
remove i list =
    List.Extra.removeAt i list


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.searchedString == "" then
        Time.every 10000 (always GetFromDB)

    else
        Sub.none


getFromDb : Firestore.Firestore -> Cmd Msg
getFromDb firestore =
    firestore
        |> Firestore.root
        |> Firestore.collection "Bullets"
        |> Firestore.build
        |> toTask
        |> Task.andThen (Firestore.list (Codec.asDecoder codecBullet) Firestore.Options.List.default)
        |> Task.attempt Get



-- getNavFromDb : Firestore.Firestore -> Cmd Msg
-- getNavFromDb firestore =
--     firestore
--         |> Firestore.root
--         |> Firestore.collection "Navs"
--         |> Firestore.build
--         |> toTask
--         |> Task.andThen (Firestore.list (Codec.asDecoder codecNav) Firestore.Options.List.default)
--         |> Task.attempt GetNav


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            case msg of
                Delete i ->
                    let
                        bullet =
                            List.Extra.getAt i model.items
                    in
                    case bullet of
                        Just b ->
                            ( { model | items = remove i model.items }, deleteBullet model.firestore b )

                        Nothing ->
                            ( { model | items = remove i model.items }, Cmd.none )

                AddAfter psx ->
                    let
                        bullet =
                            { title = model.userInput
                            , time = psx
                            , timeOfDay = Maybe.Nothing
                            , progress = Maybe.Nothing
                            , checked = False
                            , selectedMonth = ""
                            , dropdownStatus = Nothing
                            , nav = ""
                            , day = 0
                            }
                    in
                    ( { model
                        | items =
                            bullet :: model.items
                      }
                    , upsertBullet model.firestore
                        bullet
                    )

                AddBefore ->
                    ( model, getTime )

                Change userI ->
                    ( { model | userInput = userI }, Cmd.none )

                Edit i newString ->
                    let
                        editString =
                            updated model.items model.firestore i (editBullet newString)
                    in
                    ( { model | items = Tuple.first editString }, Tuple.second editString )

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

                Time i timePosix ->
                    let
                        editTimePosix =
                            updated model.items model.firestore i (updateTime timePosix)
                    in
                    ( { model | items = Tuple.first editTimePosix }, Tuple.second editTimePosix )

                IntialZone zn ->
                    ( { model | zone = Just zn }, Cmd.none )

                UpdateTimeOfDay i timeOfDay ->
                    let
                        editTimeOfDay =
                            updated model.items model.firestore i (updateTimeOfDay (Just timeOfDay))
                    in
                    ( { model | items = Tuple.first editTimeOfDay }, Tuple.second editTimeOfDay )

                Progress i status ->
                    let
                        editStatus =
                            updated model.items model.firestore i (updateProgress (Just status))
                    in
                    ( { model | items = Tuple.first editStatus }, Tuple.second editStatus )

                CheckedOff i checked ->
                    let
                        editChecked =
                            updated model.items model.firestore i (updateCheckedOff checked)
                    in
                    ( { model | items = Tuple.first editChecked }, Tuple.second editChecked )

                Get result ->
                    ( { model
                        | items =
                            case result of
                                Err _ ->
                                    let
                                        _ =
                                            Debug.log "nothing there"
                                    in
                                    []

                                Ok r ->
                                    let
                                        listDB =
                                            List.map .fields r.documents
                                    in
                                    everythingButFocused listDB model.items model.index
                      }
                    , Cmd.none
                    )

                GetNav result ->
                    ( { model
                        | list =
                            case result of
                                Err _ ->
                                    []

                                Ok r ->
                                    List.map .fields r.documents
                      }
                    , Cmd.none
                    )

                Upsert result ->
                    case result of
                        Err error ->
                            let
                                _ =
                                    Debug.log "upsert failed" error
                            in
                            ( { model | items = model.items }, Cmd.none )

                        Ok _ ->
                            ( { model | items = model.items }, Cmd.none )

                DeleteFromDB result ->
                    case result of
                        Err error ->
                            let
                                _ =
                                    Debug.log "upsert failed" error
                            in
                            ( { model | items = model.items }, Cmd.none )

                        Ok _ ->
                            ( { model | items = model.items }, Cmd.none )

                GetFromDB ->
                    ( model, getFromDb model.firestore )

                SearchAndRemoved userInput ->
                    ( { model | items = removeEverythingButsearched userInput (List.map identity model.items) }
                    , if userInput == "" then
                        getFromDb model.firestore

                      else
                        Cmd.none
                    )

                Search userInput ->
                    ( { model | searchedString = userInput }, Cmd.none )

                UpdateIndex indx ->
                    ( { model | index = indx }, Cmd.none )

                OpenDropdown index dropdownToggle ->
                    let
                        newItems =
                            List.Extra.updateAt index (updateDropDownStatus dropdownToggle) model.items
                    in
                    ( { model | items = newItems }, Cmd.none )

                SelectedMonth index month ->
                    let
                        editSelectedMonth =
                            updated model.items model.firestore index (updateSelectedMonth month)
                    in
                    ( { model | items = Tuple.first editSelectedMonth }, Tuple.second editSelectedMonth )

                IncrementDay index ->
                    let
                        editDay =
                            updated model.items model.firestore index incrementSelectedDay
                    in
                    ( { model | items = Tuple.first editDay }, Tuple.second editDay )

                DecrementDay index ->
                    let
                        editDay =
                            updated model.items model.firestore index decrementSelectedDay
                    in
                    ( { model | items = Tuple.first editDay }, Tuple.second editDay )

                SignOut ->
                    ( model, signout () )

                AddList ->
                    let
                        nav =
                            model.listInput
                    in
                    ( { model | list = model.list ++ [ nav ] }, Cmd.none )

                EditNav userInput ->
                    ( { model | listInput = userInput }, Cmd.none )

                SelectedNav index nav ->
                    let
                        editSelectedNav =
                            updated model.items model.firestore index (updateSelectedNav nav)
                    in
                    ( { model | items = Tuple.first editSelectedNav }, Tuple.second editSelectedNav )

                DeleteNav title ->
                    ( { model | list = List.filter (\name -> title /= name) model.list }, Cmd.none )

        x =
            Debug.log "logging this" (Json.Encode.encode 0 (encoder newModel.items))
    in
    ( newModel, Cmd.batch [ save x, cmd ] )


decrementSelectedDay : Bullet -> Bullet
decrementSelectedDay bullet =
    if bullet.day == 0 then
        { bullet | day = 0 }

    else
        { bullet | day = bullet.day - 1 }


incrementSelectedDay : Bullet -> Bullet
incrementSelectedDay bullet =
    if bullet.day < 31 then
        { bullet | day = bullet.day + 1 }

    else
        { bullet | day = 31 }


updateSelectedNav : Nav -> Bullet -> Bullet
updateSelectedNav nav bullet =
    { bullet | nav = nav, dropdownStatus = Just NavSelect }


updateSelectedMonth : String -> Bullet -> Bullet
updateSelectedMonth month bullet =
    { bullet | selectedMonth = month, dropdownStatus = Just Month }


updateDropDownStatus : DropDownStatus -> Bullet -> Bullet
updateDropDownStatus status bullet =
    case bullet.dropdownStatus of
        Just _ ->
            { bullet | dropdownStatus = Nothing }

        Nothing ->
            { bullet | dropdownStatus = Just status }


everythingButFocused : List a -> List a -> Maybe Int -> List a
everythingButFocused listDB listLocal index =
    case index of
        Just indx ->
            let
                item =
                    List.Extra.getAt indx listLocal
            in
            case item of
                Just b ->
                    List.Extra.updateAt indx (\_ -> b) listDB

                Nothing ->
                    listDB

        Nothing ->
            listDB


removeEverythingButsearched : String -> List Bullet -> List Bullet
removeEverythingButsearched userString list =
    List.filter (\bullet -> String.contains userString bullet.title) list


updated : List Bullet -> Firestore.Firestore -> Int -> (Bullet -> Bullet) -> ( List Bullet, Cmd Msg )
updated lst fs i func =
    let
        newItems =
            List.Extra.updateAt i func lst
    in
    ( newItems
    , case
        List.Extra.getAt i newItems
      of
        Just bullet ->
            upsertBullet fs bullet

        Nothing ->
            Cmd.none
    )


upsertBullet : Firestore.Firestore -> Bullet -> Cmd Msg
upsertBullet f b =
    let
        time =
            Iso.fromTime b.time
    in
    f
        |> Firestore.root
        |> Firestore.collection "Bullets"
        |> Firestore.document time
        |> Firestore.build
        |> toTask
        |> Task.andThen (Firestore.upsert (Codec.asDecoder codecBullet) (Codec.asEncoder codecBullet b))
        |> Task.attempt Upsert


deleteBullet : Firestore.Firestore -> Bullet -> Cmd Msg
deleteBullet firestore bullet =
    let
        time =
            Iso.fromTime bullet.time
    in
    firestore
        |> Firestore.root
        |> Firestore.collection "Bullets"
        |> Firestore.document time
        |> Firestore.build
        |> toTask
        |> Task.andThen Firestore.delete
        |> Task.attempt DeleteFromDB


deleteNav : Firestore.Firestore -> Nav -> Cmd Msg
deleteNav firestore nav =
    firestore
        |> Firestore.root
        |> Firestore.collection "Bullets"
        |> Firestore.document nav
        |> Firestore.build
        |> toTask
        |> Task.andThen Firestore.delete
        |> Task.attempt DeleteFromDB


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



--Helper


getModalId : Int -> String
getModalId i =
    "id" ++ String.fromInt i


standardTime : Int -> String
standardTime time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time



--View


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
                            , button [ onClick (UpdateIndex Nothing), type_ "button", class "btn-close", attribute "data-bs-dismiss" "modal", attribute "aria-label" "Close" ] []
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
                        , h5 [] [ text "Time" ]
                        , checkTime b i "Morning" Morning
                        , checkTime b i "Afternoon" Afternoon
                        , checkTime b i "Evening" Evening
                        , h5 [] [ text "Progress" ]
                        , checkStatus b i "Completed" Completed
                        , checkStatus b i "In Progress" InProgress
                        , checkStatus b i "Not Started" NotStarted
                        , h5 [] [ text "Due Date" ]
                        , div [ class "d-flex align-items-center" ]
                            [ text "Month: "
                            , div [ class "dropdown" ]
                                [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", id "dropdownMenuButton", attribute "data-bs-toggle" "dropdown" ] [ text b.selectedMonth ]
                                , div
                                    [ classList
                                        [ ( "dropdown-menu", True )
                                        ]
                                    ]
                                    [ selectMonth i "January"
                                    , selectMonth i "February"
                                    , selectMonth i "March"
                                    , selectMonth i "April"
                                    , selectMonth i "May"
                                    , selectMonth i "June"
                                    , selectMonth i "July"
                                    , selectMonth i "August"
                                    , selectMonth i "September"
                                    , selectMonth i "November"
                                    , selectMonth i "December"
                                    ]
                                ]
                            ]
                        , div [ class "d-flex align-items-center" ]
                            [ text "Day"
                            , button [ onClick (DecrementDay i) ] [ text "-" ]
                            , div [] [ text (String.fromInt b.day) ]
                            , button [ onClick (IncrementDay i) ] [ text "+" ]
                            ]
                        , h5 [] [ text "List" ]
                        , div [ class "dropdown" ]
                            [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", id "dropdownMenuButton", attribute "data-bs-toggle" "dropdown" ]
                                [ text b.nav ]
                            , div
                                [ classList
                                    [ ( "dropdown-menu", True )
                                    ]
                                ]
                                [ ul []
                                    (List.map (\ item-> li [] [selectNav i item]) model.list)
  
                                ]

                            ]
                        , div [ class "modal-footer" ]
                            [ button [ onClick (UpdateIndex Nothing), type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text "No zone"


view : Model -> Html Msg
view model =
    div [ class "background" ]
        [ p [ class "text-center fs-1 fw-bold font-monospace text-title " ] [ text "To-Do List" ]
        , div [] []
        , div [ class "d-flex justify-content-center align-items-center" ]
            [ div [ class "d-flex mb-3" ]
                [ input [ class "form-control me-3", placeholder "Write something", value model.userInput, onInput Change ] []
                , button [ class "btn button", onClick AddBefore ] [ text "+" ]
                , button [ class "btn button", onClick (SearchAndRemoved model.searchedString) ] [ text "Q" ]
                , input [ class "form-control me-3", placeholder "Search", value model.searchedString, onInput Search ] []
                ]
            ]
        , div [ class "d-flex justify-content-center" ]
            [ ul [ class "list-unstyled flex-column align-items-center" ]
                (List.Extra.interweave (List.Extra.initialize (List.length model.items + 1) viewDropZone)
                    (List.indexedMap (\i _ -> viewBullet i model) model.items)
                )
            ]
        , div [ class "fixed-bottom" ] [ button [ class "btn button", onClick SignOut ] [ text "Sign Out" ] ]
        , div []
            [ viewSideBar model
            ]
        ]


viewSideBar : Model -> Html Msg
viewSideBar model =
    div [ class "sidebar sidebar-narrow-unfoldable border-end" ]
        [ div [ class "sidebar-header border-bottom" ]
            [ div [ class "sidebar-brand" ] [ text "Menu" ]
            ]
        , ul [ class "sidebar-nav" ]
            [ li [ class "nav-title" ] [ text "Navigation" ]
            , li [ class "nav-item" ]
                [ a [] [ text "Lists" ] ]
            , button [ class "btn button", onClick AddList ] [ text "+" ]
            , div []
                [ input [ placeholder "Add a list", value model.listInput, onInput EditNav ] []
                ]
            , div []
                [ ul [] (List.map viewNav model.list)
                ]
            ]
        ]


selectMonth : Int -> String -> Html Msg
selectMonth i month =
    a [ class "dropdown-item", href "#", onClick (SelectedMonth i month) ] [ text month ]


selectNav : Int -> Nav  -> Html Msg
selectNav i nav  =
    a [ class "dropdown-item", href "#", onClick (SelectedNav i nav) ] [ text nav ]
    
        


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
            , name "radio1"
            , id str
            ]
            []
        ]


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
                [ li [ class "d-flex justify-content-between align-items-center mb-2" ]
                    [ div [ class "d-flex justify-content-between align-items-center" ]
                        [ div [ class "form-check me-2" ]
                            [ checkedBullet bullet i
                            ]
                        ]
                    , input
                        [ value bullet.title
                        , class
                            (if bullet.checked then
                                "text-decoration-line-through"

                             else if bullet.progress == Just Completed then
                                "text-decoration-line-through"

                             else
                                "text-decoration-none"
                            )
                        , onInput (Edit i)
                        , onFocus (UpdateIndex (Just i))
                        , onBlur (UpdateIndex Nothing)
                        ]
                        []
                    , div []
                        [ button [ class "btn button-small", onClick (Delete i) ] [ text "-" ]
                        , button [ onClick (UpdateIndex (Just i)), class "btn button-small", type_ "button", attribute "data-bs-toggle" "modal", attribute "data-bs-target" ("#" ++ getModalId i) ] [ text "i" ]
                        , viewModal model i bullet
                        ]
                    ]
                ]

        Nothing ->
            text "No bullet"


viewNav : Nav -> Html Msg
viewNav nav =
    div [ class "d-flex justify-content-center" ]
        [ input [ value nav, disabled True ] []
        , button [ class "btn button", onClick (DeleteNav nav) ] [ text "-" ]
        ]


checkedBullet : Bullet -> Int -> Html Msg
checkedBullet bullet index =
    div [ class "form-check me-2" ]
        [ label [ class "form-check-label", for "flexCheckChecked" ] []
        , input
            [ checked
                bullet.checked
            , onClick (CheckedOff index (not bullet.checked))
            , class "form-check-input"
            , type_ "checkbox"
            , value ""
            , id "flexCheckChecked"
            ]
            []
        ]



--Drag/Drop


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



--encoders/decoders


encoderNav : List Nav -> Json.Encode.Value
encoderNav navs =
    Json.Encode.list encodeNav navs


encodeNav : Nav -> Json.Encode.Value
encodeNav nav =
    Json.Encode.string nav


encoder : List Bullet -> Json.Encode.Value
encoder items =
    Json.Encode.list encodeBullet items


encodeBullet : Bullet -> Json.Encode.Value
encodeBullet b =
    Json.Encode.object
        [ ( "input", Json.Encode.string b.title )
        , ( "time", Iso.encode b.time )
        , ( "timeOfDay", encodeTimeOfDayToJson b.timeOfDay )
        , ( "progress", encodeStatusToJson b.progress )
        , ( "checked", Json.Encode.bool b.checked )
        , ( "dropdownStatus", encodeDropDownToJson b.dropdownStatus )
        , ( "selectedMonth", Json.Encode.string b.selectedMonth )
        , ( "nav", Json.Encode.string b.nav )
        , ( "day", Json.Encode.int b.day )
        ]


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


encodeDropDownToJson : Maybe DropDownStatus -> Json.Encode.Value
encodeDropDownToJson dropDownStatus =
    case dropDownStatus of
        Just NavSelect ->
            let
                to =
                    encodeDropDown NavSelect
            in
            Json.Encode.string to

        Just Month ->
            let
                to =
                    encodeDropDown Month
            in
            Json.Encode.string to

        Nothing ->
            Json.Encode.null


encodeDropDown : DropDownStatus -> String
encodeDropDown dropDownStatus =
    case dropDownStatus of
        NavSelect ->
            "NavSelect"

        Month ->
            "Month"


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


decodeListNav : Decode.Decoder (List Nav)
decodeListNav =
    Decode.list Decode.string


decodeBullet : Decode.Decoder Bullet
decodeBullet =
    Decode.succeed Bullet
        |> Json.Decode.Pipeline.required "input" Decode.string
        |> Json.Decode.Pipeline.required "time" Iso.decoder
        |> Json.Decode.Pipeline.required "timeOfDay" (Decode.maybe decodeTimeOfDay)
        |> Json.Decode.Pipeline.required "progress" (Decode.maybe decodeStatus)
        |> Json.Decode.Pipeline.required "checked" Decode.bool
        |> Json.Decode.Pipeline.required "dropdownStatus" (Decode.maybe decodeDropDown)
        |> Json.Decode.Pipeline.required "selectedMonth" Decode.string
        |> Json.Decode.Pipeline.required "nav" Decode.string
        |> Json.Decode.Pipeline.required "day" Decode.int


decodeTimeOfDay : Decode.Decoder TimeOfDay
decodeTimeOfDay =
    Decode.oneOf [ decoderExactString "Morning" Morning, decoderExactString "Afternoon" Afternoon, decoderExactString "Evening" Evening ]


decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.oneOf [ decoderExactString "Completed" Completed, decoderExactString "In Progress" InProgress, decoderExactString "Not Started" NotStarted ]


decodeDropDown : Decode.Decoder DropDownStatus
decodeDropDown =
    Decode.oneOf [ decoderExactString "NavSelect" NavSelect, decoderExactString "Month" Month ]


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
        |> Codec.required "dropdownStatus" .dropdownStatus (Codec.maybe codecDropDown)
        |> Codec.required "selectedMonth" .selectedMonth Codec.string
        |> Codec.required "nav" .nav Codec.string
        |> Codec.required "day" .day Codec.int
        |> Codec.build



-- codecNav : Codec.Codec Nav
-- codecNav =


codecDropDown : Codec.Field DropDownStatus
codecDropDown =
    codecOneOf
        encodeDropDown
        Codec.string
        [ ( "NavSelect", NavSelect )
        , ( "Month", Month )
        ]


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



--ports


port save : String -> Cmd msg


port signout : () -> Cmd msg
