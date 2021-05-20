module Main exposing (main)

import Api exposing (Action(..), Option, OptionId, Share, ShareId, Type(..), Wheel, WheelId)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import RemoteData exposing (RemoteData(..), WebData, fromResult)
import RemoteData.Http exposing (delete, get, post, put)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentWheel : WebData Wheel
    , currentShare : WebData Share
    , currentOption : WebData Option
    , wheels : WebData (List Wheel)
    , shares : WebData (List Share)
    , options : WebData (List Option)
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            initModel
    in
    ( model, listWheels model )


initModel : Model
initModel =
    { currentWheel = NotAsked
    , currentShare = NotAsked
    , currentOption = NotAsked
    , wheels = NotAsked
    , shares = NotAsked
    , options = NotAsked
    }



-- UPDATE


type Msg
    = NoOp
      -- Wheels
    | HandleGetWheels (WebData (List Wheel))
    | HandleGetWheel (WebData Wheel)
    | HandleCreateWheel (WebData Wheel)
    | HandleUpdateWheel (WebData Wheel)
    | HandleDeleteWheel (WebData String)
      -- Options
    | HandleGetOptions (WebData (List Option))
    | HandleGetOption (WebData Option)
    | HandleCreateOption (WebData Option)
    | HandleUpdateOption (WebData Option)
    | HandleDeleteOption (WebData String)
      -- Shares
    | HandleGetShares (WebData (List Share))
    | HandleGetShare (WebData Share)
    | HandleCreateShare (WebData Share)
    | HandleUpdateShare (WebData Share)
    | HandleDeleteShare (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleGetWheels response ->
            ( { model | wheels = response }, Cmd.none )

        HandleGetWheel response ->
            ( { model | currentWheel = response }, Cmd.none )

        HandleCreateWheel response ->
            ( { model | currentWheel = response }, Cmd.none )

        HandleUpdateWheel response ->
            ( { model | currentWheel = response }, Cmd.none )

        HandleDeleteWheel response ->
            ( { model | currentWheel = NotAsked }, Cmd.none )

        HandleGetOptions response ->
            ( { model | options = response }, Cmd.none )

        HandleGetOption response ->
            ( { model | currentOption = response }, Cmd.none )

        HandleCreateOption response ->
            ( { model | currentOption = response }, Cmd.none )

        HandleUpdateOption response ->
            ( { model | currentOption = response }, Cmd.none )

        HandleDeleteOption response ->
            ( { model | currentOption = NotAsked }, Cmd.none )

        HandleGetShares response ->
            ( { model | shares = response }, Cmd.none )

        HandleGetShare response ->
            ( { model | currentShare = response }, Cmd.none )

        HandleCreateShare response ->
            ( { model | currentShare = response }, Cmd.none )

        HandleUpdateShare response ->
            ( { model | currentShare = response }, Cmd.none )

        HandleDeleteShare response ->
            ( { model | currentShare = NotAsked }, Cmd.none )


listWheels : Model -> Cmd Msg
listWheels model =
    Api.listWheels HandleGetWheels


listOptions : Model -> Cmd Msg
listOptions model =
    case model.currentWheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.listOptions wheelId HandleGetOptions

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


listShares : Model -> Cmd Msg
listShares model =
    case model.currentWheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.listShares wheelId HandleGetShares

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getWheel : Model -> Cmd Msg
getWheel model =
    case model.currentWheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.getWheel wheelId HandleGetWheel

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getOption : Model -> Cmd Msg
getOption model =
    case ( model.currentWheel, model.currentOption ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.getOption wheelId optionId HandleGetOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getShare : Model -> Cmd Msg
getShare model =
    case ( model.currentWheel, model.currentShare ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.getShare wheelId shareId HandleGetShare

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


createWheel : Model -> Cmd Msg
createWheel model =
    case model.currentWheel of
        Success wheel ->
            Api.createWheel wheel HandleCreateWheel

        _ ->
            Cmd.none


createOption : Model -> Cmd Msg
createOption model =
    case ( model.currentWheel, model.currentOption ) of
        ( Success wheel, Success option ) ->
            case wheel.id_ of
                Just wheelId ->
                    Api.createOption wheelId option HandleCreateOption

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


createShare : Model -> Cmd Msg
createShare model =
    case ( model.currentWheel, model.currentShare ) of
        ( Success wheel, Success share ) ->
            case wheel.id_ of
                Just wheelId ->
                    Api.createShare wheelId share HandleCreateShare

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


updateWheel : Model -> Cmd Msg
updateWheel model =
    case model.currentWheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.updateWheel wheelId wheel HandleUpdateWheel

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


updateOption : Model -> Cmd Msg
updateOption model =
    case ( model.currentWheel, model.currentOption ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.updateOption wheelId optionId option HandleUpdateOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateShare : Model -> Cmd Msg
updateShare model =
    case ( model.currentWheel, model.currentShare ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.updateShare wheelId shareId share HandleUpdateShare

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteWheel : Model -> Cmd Msg
deleteWheel model =
    case model.currentWheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.deleteWheel wheelId HandleDeleteWheel

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteOption : Model -> Cmd Msg
deleteOption model =
    case ( model.currentWheel, model.currentOption ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.deleteOption wheelId optionId HandleDeleteOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteShare : Model -> Cmd Msg
deleteShare model =
    case ( model.currentWheel, model.currentShare ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.deleteShare wheelId shareId HandleDeleteShare

                _ ->
                    Cmd.none

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
