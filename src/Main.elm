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


type PageState
    = ListWheels
    | ViewWheel
    | ModifyWheel
    | ViewOption
    | ModifyOption
    | ViewShare
    | ModifyShare


type alias Model =
    { pageState : PageState
    , wheel : WebData Wheel
    , share : WebData Share
    , option : WebData Option
    , wheelList : WebData (List Wheel)
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
    { pageState = ListWheels
    , wheel = NotAsked
    , share = NotAsked
    , option = NotAsked
    , wheelList = NotAsked
    }



-- UPDATE


type Msg
    = NoOp
      -- Wheels
    | GotWheels (WebData (List Wheel))
    | GotWheel (WebData Wheel)
    | ModifiedWheel (WebData Wheel)
    | DeletedWheel (WebData String)
      -- Options
    | GotOptions (Model -> Cmd Msg) (WebData (List Option))
    | GotOption (WebData Option)
    | ModifiedOption (WebData Option)
    | DeletedOption (WebData String)
      -- Shares
    | GotShares (WebData (List Share))
    | GotShare (WebData Share)
    | ModifiedShare (WebData Share)
    | DeletedShare (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotWheels response ->
            ( { model | wheelList = response, pageState = ListWheels }, Cmd.none )

        GotWheel response ->
            let
                newModel =
                    { model | wheel = response, pageState = ViewWheel }
            in
            -- Chain to filling options data for the wheel, which chains to filling shares data too
            ( newModel, newModel |> fillWheel )

        ModifiedWheel response ->
            ( { model | wheel = response, pageState = ViewWheel }, Cmd.none )

        DeletedWheel response ->
            ( { model | wheel = NotAsked }, model |> listWheels )

        GotOptions nextMsg response ->
            let
                newModel =
                    { model
                        | wheel =
                            case ( model.wheel, response ) of
                                ( Success wheel, Success optList ) ->
                                    RemoteData.succeed { wheel | optionsList = optList }

                                _ ->
                                    model.wheel
                        , pageState = ViewWheel
                    }
            in
            -- Chains directly to also filling in shares for the wheel too
            ( newModel, newModel |> nextMsg )

        GotOption response ->
            ( { model | option = response, pageState = ViewOption }, Cmd.none )

        ModifiedOption _ ->
            let
                newModel =
                    { model | option = NotAsked }
            in
            ( newModel, newModel |> listOptions )

        DeletedOption response ->
            let
                newModel =
                    { model | option = NotAsked }
            in
            ( newModel, newModel |> listOptions )

        GotShares response ->
            let
                newModel =
                    { model
                        | wheel =
                            case ( model.wheel, response ) of
                                ( Success wheel, Success shareList ) ->
                                    RemoteData.succeed { wheel | sharesList = shareList }

                                _ ->
                                    model.wheel
                        , pageState = ViewWheel
                    }
            in
            ( newModel, Cmd.none )

        GotShare response ->
            ( { model | share = response, pageState = ViewShare }, Cmd.none )

        ModifiedShare _ ->
            let
                newModel =
                    { model | share = NotAsked }
            in
            ( newModel, newModel |> listShares )

        DeletedShare response ->
            let
                newModel =
                    { model | share = NotAsked }
            in
            ( newModel, newModel |> listShares )


listWheels : Model -> Cmd Msg
listWheels model =
    Api.listWheels GotWheels


listOptions : Model -> Cmd Msg
listOptions model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.listOptions wheelId (GotOptions (\_ -> Cmd.none))

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


fillWheel : Model -> Cmd Msg
fillWheel model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.listOptions wheelId (GotOptions listShares)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


listShares : Model -> Cmd Msg
listShares model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.listShares wheelId GotShares

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getWheel : Model -> Cmd Msg
getWheel model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.getWheel wheelId GotWheel

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getOption : Model -> Cmd Msg
getOption model =
    case ( model.wheel, model.option ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.getOption wheelId optionId GotOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


getShare : Model -> Cmd Msg
getShare model =
    case ( model.wheel, model.share ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.getShare wheelId shareId GotShare

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


createWheel : Model -> Cmd Msg
createWheel model =
    case model.wheel of
        Success wheel ->
            Api.createWheel wheel ModifiedWheel

        _ ->
            Cmd.none


createOption : Model -> Cmd Msg
createOption model =
    case ( model.wheel, model.option ) of
        ( Success wheel, Success option ) ->
            case wheel.id_ of
                Just wheelId ->
                    Api.createOption wheelId option ModifiedOption

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


createShare : Model -> Cmd Msg
createShare model =
    case ( model.wheel, model.share ) of
        ( Success wheel, Success share ) ->
            case wheel.id_ of
                Just wheelId ->
                    Api.createShare wheelId share ModifiedShare

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


updateWheel : Model -> Cmd Msg
updateWheel model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.updateWheel wheelId wheel ModifiedWheel

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


updateOption : Model -> Cmd Msg
updateOption model =
    case ( model.wheel, model.option ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.updateOption wheelId optionId option ModifiedOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateShare : Model -> Cmd Msg
updateShare model =
    case ( model.wheel, model.share ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.updateShare wheelId shareId share ModifiedShare

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteWheel : Model -> Cmd Msg
deleteWheel model =
    case model.wheel of
        Success wheel ->
            case wheel.id_ of
                Just wheelId ->
                    Api.deleteWheel wheelId DeletedWheel

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteOption : Model -> Cmd Msg
deleteOption model =
    case ( model.wheel, model.option ) of
        ( Success wheel, Success option ) ->
            case ( wheel.id_, option.id_ ) of
                ( Just wheelId, Just optionId ) ->
                    Api.deleteOption wheelId optionId DeletedOption

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


deleteShare : Model -> Cmd Msg
deleteShare model =
    case ( model.wheel, model.share ) of
        ( Success wheel, Success share ) ->
            case ( wheel.id_, share.id_ ) of
                ( Just wheelId, Just shareId ) ->
                    Api.deleteShare wheelId shareId DeletedShare

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
