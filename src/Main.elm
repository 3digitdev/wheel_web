module Main exposing (main)

import Api exposing (Option, Share, Wheel)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentWheel : Maybe Wheel
    , currentShare : Maybe Share
    , currentOption : Maybe Option
    , wheels : List Wheel
    , shares : List Share
    , options : List Option
    , errors : String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    update (DeleteWheel 8) initModel


initModel : Model
initModel =
    { currentWheel = Maybe.Nothing
    , currentShare = Maybe.Nothing
    , currentOption = Maybe.Nothing
    , wheels = []
    , shares = []
    , options = []
    , errors = ""
    }



-- UPDATE


type Msg
    = NoOp
      -- Wheels
    | GetWheels
    | GotWheels (Result Http.Error (List Wheel))
    | GetWheel Int
    | GotWheel (Result Http.Error Wheel)
      -- TODO: POST, PUT - HOW TO SEND BODY?
    | DeleteWheel Int
    | WheelDeleted (Result Http.Error ())
      -- Options
    | GetOptions Int
    | GotOptions (Result Http.Error (List Option))
    | GetOption Int Int
    | GotOption (Result Http.Error Option)
      -- TODO: POST, PUT - HOW TO SEND BODY?
    | DeleteOption Int Int
    | OptionDeleted (Result Http.Error ())
      -- Shares
    | GetShares Int
    | GotShares (Result Http.Error (List Share))
    | GetShare Int Int
    | GotShare (Result Http.Error Share)
      -- TODO: POST, PUT - HOW TO SEND BODY?
    | DeleteShare Int Int
    | ShareDeleted (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetWheels ->
            ( model, Api.get Api.GetAllWheels Api.decodeWheelListResponse GotWheels )

        GotWheels result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , wheels = []
                      }
                    , Cmd.none
                    )

                Ok wheelList ->
                    ( { model | wheels = wheelList, errors = "" }, Cmd.none )

        GetWheel wheelId ->
            ( model, Api.get (Api.GetWheelById wheelId) Api.decodeWheelResponse GotWheel )

        GotWheel result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , currentWheel = Maybe.Nothing
                      }
                    , Cmd.none
                    )

                Ok wheel ->
                    ( { model | currentWheel = Just wheel, errors = "" }, Cmd.none )

        DeleteWheel wheelId ->
            ( model, Api.delete (Api.DeleteWheel wheelId) WheelDeleted )

        WheelDeleted result ->
            case result of
                Err httpError ->
                    ( { model | errors = httpError |> Api.errorToString }, Cmd.none )

                Ok _ ->
                    ( { model | currentWheel = Maybe.Nothing, errors = "" }, Cmd.none )

        GetOptions wheelId ->
            ( model, Api.get (Api.GetAllOptions wheelId) Api.decodeOptionListResponse GotOptions )

        GotOptions result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , options = []
                      }
                    , Cmd.none
                    )

                Ok optionList ->
                    ( { model | options = optionList, errors = "" }, Cmd.none )

        GetOption wheelId optionId ->
            ( model, Api.get (Api.GetOptionById wheelId optionId) Api.decodeOptionResponse GotOption )

        GotOption result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , currentOption = Maybe.Nothing
                      }
                    , Cmd.none
                    )

                Ok option ->
                    ( { model | currentOption = Just option, errors = "" }, Cmd.none )

        DeleteOption wheelId optionId ->
            ( model, Api.delete (Api.DeleteOption wheelId optionId) OptionDeleted )

        OptionDeleted result ->
            case result of
                Err httpError ->
                    ( { model | errors = httpError |> Api.errorToString }, Cmd.none )

                Ok _ ->
                    ( { model | currentOption = Maybe.Nothing, errors = "" }, Cmd.none )

        GetShares wheelId ->
            ( model, Api.get (Api.GetAllShares wheelId) Api.decodeShareListResponse GotShares )

        GotShares result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , shares = []
                      }
                    , Cmd.none
                    )

                Ok shareList ->
                    ( { model | shares = shareList, errors = "" }, Cmd.none )

        GetShare wheelId shareId ->
            ( model, Api.get (Api.GetShareById wheelId shareId) Api.decodeShareResponse GotShare )

        GotShare result ->
            case result of
                Err httpError ->
                    ( { model
                        | errors = httpError |> Api.errorToString
                        , currentShare = Maybe.Nothing
                      }
                    , Cmd.none
                    )

                Ok share ->
                    ( { model | currentShare = Just share, errors = "" }, Cmd.none )

        DeleteShare wheelId shareId ->
            ( model, Api.delete (Api.DeleteShare wheelId shareId) ShareDeleted )

        ShareDeleted result ->
            case result of
                Err httpError ->
                    ( { model | errors = httpError |> Api.errorToString }, Cmd.none )

                Ok _ ->
                    ( { model | currentShare = Maybe.Nothing, errors = "" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
