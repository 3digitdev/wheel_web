module Api exposing
    ( Action
    , Endpoint(..)
    , Option
    , Share
    , Type
    , Wheel
    , decodeOptionListResponse
    , decodeOptionResponse
    , decodeShareListResponse
    , decodeShareResponse
    , decodeWheelListResponse
    , decodeWheelResponse
    , delete
    , errorToString
    , get
    , post
    , put
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD exposing (dict, list, string)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value, object)



-- TYPES


type Action
    = Buy
    | Sell
    | ActionError


type Type
    = Call
    | Put
    | TypeError


type alias Share =
    { id_ : Int
    , quantity : Int
    , cost : Float
    , saleDate : String
    , wheelId : Int
    , action : Action
    }


type alias Option =
    { id_ : Int
    , quantity : Int
    , strike : Float
    , premium : Float
    , open : Bool
    , saleDate : String
    , expDate : String
    , wheelId : Int
    , action : Action
    , optType : Type
    }


type alias Wheel =
    { id_ : Int
    , ticker : String
    , description : String
    , subtotal : Float
    , positionsClosed : Bool
    , assignedShares : Bool
    , sharesList : List Share
    , optionsList : List Option
    }


type Endpoint
    = GetAllWheels
    | GetWheelById Int
    | CreateWheel Http.Body
    | UpdateWheel Int Http.Body
    | DeleteWheel Int
    | GetAllOptions Int
    | GetOptionById Int Int
    | CreateOption Int Http.Body
    | UpdateOption Int Int Http.Body
    | DeleteOption Int Int
    | GetAllShares Int
    | GetShareById Int Int
    | CreateShare Int Http.Body
    | UpdateShare Int Int Http.Body
    | DeleteShare Int Int


urlBase : String
urlBase =
    "http://localhost:4000"


urlBuilder : Endpoint -> String
urlBuilder endpoint =
    urlBase
        ++ (case endpoint of
                GetAllWheels ->
                    "/wheels"

                GetWheelById wheelId ->
                    "/wheels/" ++ String.fromInt wheelId

                CreateWheel _ ->
                    "/wheels"

                UpdateWheel wheelId _ ->
                    "/wheels/" ++ String.fromInt wheelId

                DeleteWheel wheelId ->
                    "/wheels/" ++ String.fromInt wheelId

                GetAllOptions wheelId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/options"

                GetOptionById wheelId optionId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/options/" ++ String.fromInt optionId

                CreateOption wheelId _ ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/options"

                UpdateOption wheelId optionId _ ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/options/" ++ String.fromInt optionId

                DeleteOption wheelId optionId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/options/" ++ String.fromInt optionId

                GetAllShares wheelId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/shares"

                GetShareById wheelId shareId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/shares/" ++ String.fromInt shareId

                CreateShare wheelId _ ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/shares"

                UpdateShare wheelId shareId _ ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/shares/" ++ String.fromInt shareId

                DeleteShare wheelId shareId ->
                    "/wheels/" ++ String.fromInt wheelId ++ "/shares/" ++ String.fromInt shareId
           )


decodeAction : JD.Decoder Action
decodeAction =
    JD.string
        |> JD.andThen
            (\string ->
                case string of
                    "BUY" ->
                        JD.succeed Buy

                    "SELL" ->
                        JD.succeed Sell

                    _ ->
                        JD.succeed ActionError
            )


decodeShare : JD.Decoder Share
decodeShare =
    JD.succeed Share
        |> JDP.required "id" JD.int
        |> JDP.required "quantity" JD.int
        |> JDP.required "cost" JD.float
        |> JDP.required "sale_date" JD.string
        |> JDP.required "wheel_id" JD.int
        |> JDP.required "action" decodeAction


decodeShareResponse : JD.Decoder Share
decodeShareResponse =
    JD.field "share" decodeShare


decodeShareListResponse : JD.Decoder (List Share)
decodeShareListResponse =
    JD.field "shares" (JD.list decodeShare)


decodeOption : JD.Decoder Option
decodeOption =
    JD.succeed Option
        |> JDP.required "id" JD.int
        |> JDP.required "quantity" JD.int
        |> JDP.required "strike" JD.float
        |> JDP.required "premium" JD.float
        |> JDP.required "open" JD.bool
        |> JDP.required "sale_date" JD.string
        |> JDP.required "exp_date" JD.string
        |> JDP.required "wheel_id" JD.int
        |> JDP.required "action" decodeAction
        |> JDP.required "type"
            (JD.string
                |> JD.andThen
                    (\t ->
                        case t of
                            "CALL" ->
                                JD.succeed Call

                            "PUT" ->
                                JD.succeed Put

                            _ ->
                                JD.succeed TypeError
                    )
            )


decodeOptionResponse : JD.Decoder Option
decodeOptionResponse =
    JD.field "option" decodeOption


decodeOptionListResponse : JD.Decoder (List Option)
decodeOptionListResponse =
    JD.field "options" (JD.list decodeOption)


decodeWheel : JD.Decoder Wheel
decodeWheel =
    JD.succeed Wheel
        |> JDP.required "id" JD.int
        |> JDP.required "ticker" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "subtotal" JD.float
        |> JDP.required "positions_closed" JD.bool
        |> JDP.required "assigned_shares" JD.bool
        |> JDP.hardcoded []
        |> JDP.hardcoded []


decodeWheelResponse : JD.Decoder Wheel
decodeWheelResponse =
    JD.field "wheel" decodeWheel


decodeWheelListResponse : JD.Decoder (List Wheel)
decodeWheelListResponse =
    JD.field "wheels" (JD.list decodeWheel)


get : Endpoint -> JD.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
get endpoint decoder cmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = urlBuilder endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson cmd decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> JE.Value -> JD.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
post endpoint body decoder cmd =
    Http.request
        { method = "POST"
        , headers = []
        , url = urlBuilder endpoint

        -- TODO:  HOW TO SEND BODY?
        , body = Http.jsonBody body
        , expect = Http.expectJson cmd decoder
        , timeout = Nothing
        , tracker = Nothing
        }


put : Endpoint -> JE.Value -> JD.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
put endpoint body decoder cmd =
    Http.request
        { method = "PUT"
        , headers = []
        , url = urlBuilder endpoint

        -- TODO:  HOW TO SEND BODY?
        , body = Http.jsonBody body
        , expect = Http.expectJson cmd decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Endpoint -> (Result Http.Error () -> msg) -> Cmd msg
delete endpoint cmd =
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , url = urlBuilder endpoint
        , expect = Http.expectWhatever cmd
        , timeout = Nothing
        , tracker = Nothing
        }


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus text ->
            "No wheel match:  " ++ String.fromInt text

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url
