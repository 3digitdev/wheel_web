module Api exposing
    ( Action(..)
    , Option
    , OptionId
    , Share
    , ShareId
    , Type(..)
    , Wheel
    , WheelId
    , createOption
    , createShare
    , createWheel
    , deleteOption
    , deleteShare
    , deleteWheel
    , getOption
    , getShare
    , getWheel
    , listOptions
    , listShares
    , listWheels
    , updateOption
    , updateShare
    , updateWheel
    )

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import RemoteData exposing (RemoteData, WebData, fromResult)
import RemoteData.Http exposing (delete, get, post, put)



-- import RemoteData.Http exposing (delete, get, post, put)
-- TYPES


type alias WheelId =
    Int


type alias OptionId =
    Int


type alias ShareId =
    Int


type Action
    = Buy
    | Sell
    | ActionError


type Type
    = Call
    | Put
    | TypeError


type alias Share =
    { id_ : Maybe ShareId
    , quantity : Int
    , cost : Float
    , saleDate : String
    , wheelId : WheelId
    , action : Action
    }


type alias Option =
    { id_ : Maybe OptionId
    , quantity : Int
    , strike : Float
    , premium : Float
    , open : Bool
    , saleDate : String
    , expDate : String
    , wheelId : WheelId
    , action : Action
    , optType : Type
    }


type alias Wheel =
    { id_ : Maybe WheelId
    , ticker : String
    , description : String
    , subtotal : Float
    , positionsClosed : Bool
    , assignedShares : Bool
    , sharesList : List Share
    , optionsList : List Option
    }


type
    Endpoint
    -- Wheels
    = GetAllWheels
    | GetWheelById WheelId
    | CreateWheel Http.Body
    | UpdateWheel WheelId Http.Body
    | DeleteWheel WheelId
      -- Options
    | GetAllOptions WheelId
    | GetOptionById WheelId OptionId
    | CreateOption WheelId Http.Body
    | UpdateOption WheelId OptionId Http.Body
    | DeleteOption WheelId OptionId
      -- Shares
    | GetAllShares WheelId
    | GetShareById WheelId ShareId
    | CreateShare WheelId Http.Body
    | UpdateShare WheelId ShareId Http.Body
    | DeleteShare WheelId ShareId



-- GET Endpoints


getRequest : Endpoint -> (WebData a -> msg) -> JD.Decoder a -> Cmd msg
getRequest endpoint msg decoder =
    get (urlBuilder endpoint) msg decoder


listWheels : (WebData (List Wheel) -> msg) -> Cmd msg
listWheels msg =
    getRequest GetAllWheels msg decodeWheelListResponse


getWheel : WheelId -> (WebData Wheel -> msg) -> Cmd msg
getWheel wheelId msg =
    getRequest (GetWheelById wheelId) msg decodeWheelResponse


listOptions : WheelId -> (WebData (List Option) -> msg) -> Cmd msg
listOptions wheelId msg =
    getRequest (GetAllOptions wheelId) msg decodeOptionListResponse


getOption : WheelId -> OptionId -> (WebData Option -> msg) -> Cmd msg
getOption wheelId optionId msg =
    getRequest (GetOptionById wheelId optionId) msg decodeOptionResponse


listShares : WheelId -> (WebData (List Share) -> msg) -> Cmd msg
listShares wheelId msg =
    getRequest (GetAllShares wheelId) msg decodeShareListResponse


getShare : WheelId -> ShareId -> (WebData Share -> msg) -> Cmd msg
getShare wheelId shareId msg =
    getRequest (GetShareById wheelId shareId) msg decodeShareResponse



-- POST Endpoints


postRequest : Endpoint -> (WebData a -> msg) -> JD.Decoder a -> JE.Value -> Cmd msg
postRequest endpoint msg decoder encodedObject =
    post (urlBuilder endpoint) msg decoder encodedObject


createWheel : Wheel -> (WebData Wheel -> msg) -> Cmd msg
createWheel wheel msg =
    let
        encodedWheel =
            encodeWheel wheel
    in
    postRequest (CreateWheel (Http.jsonBody encodedWheel)) msg decodeWheelResponse encodedWheel


createOption : WheelId -> Option -> (WebData Option -> msg) -> Cmd msg
createOption wheelId option msg =
    let
        encodedOption =
            encodeOption option
    in
    postRequest (CreateOption wheelId (Http.jsonBody encodedOption)) msg decodeOptionResponse encodedOption


createShare : WheelId -> Share -> (WebData Share -> msg) -> Cmd msg
createShare wheelId share msg =
    let
        encodedShare =
            encodeShare share
    in
    postRequest (CreateShare wheelId (Http.jsonBody encodedShare)) msg decodeShareResponse encodedShare



-- PUT Endpoints


putRequest : Endpoint -> (WebData a -> msg) -> JD.Decoder a -> JE.Value -> Cmd msg
putRequest endpoint msg decoder encodedObject =
    put (urlBuilder endpoint) msg decoder encodedObject


updateWheel : WheelId -> Wheel -> (WebData Wheel -> msg) -> Cmd msg
updateWheel wheelId wheel msg =
    let
        encodedWheel =
            encodeWheel wheel
    in
    putRequest (UpdateWheel wheelId (Http.jsonBody encodedWheel)) msg decodeWheelResponse encodedWheel


updateOption : WheelId -> OptionId -> Option -> (WebData Option -> msg) -> Cmd msg
updateOption wheelId optionId option msg =
    let
        encodedOption =
            encodeOption option
    in
    putRequest (UpdateOption wheelId optionId (Http.jsonBody encodedOption)) msg decodeOptionResponse encodedOption


updateShare : WheelId -> ShareId -> Share -> (WebData Share -> msg) -> Cmd msg
updateShare wheelId shareId share msg =
    let
        encodedShare =
            encodeShare share
    in
    putRequest (UpdateShare wheelId shareId (Http.jsonBody encodedShare)) msg decodeShareResponse encodedShare



-- DELETE Endpoints


deleteRequest : Endpoint -> (WebData String -> msg) -> Cmd msg
deleteRequest endpoint msg =
    delete (urlBuilder endpoint) msg JE.null


deleteWheel : WheelId -> (WebData String -> msg) -> Cmd msg
deleteWheel wheelId msg =
    deleteRequest (DeleteWheel wheelId) msg


deleteOption : WheelId -> OptionId -> (WebData String -> msg) -> Cmd msg
deleteOption wheelId optionId msg =
    deleteRequest (DeleteOption wheelId optionId) msg


deleteShare : WheelId -> ShareId -> (WebData String -> msg) -> Cmd msg
deleteShare wheelId shareId msg =
    deleteRequest (DeleteShare wheelId shareId) msg



-- HELPER FUNCTIONS


urlBuilder : Endpoint -> String
urlBuilder endpoint =
    "http://localhost:4000"
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



-- DECODERS / ENCODERS


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


encodeAction : Action -> JE.Value
encodeAction action =
    case action of
        Buy ->
            JE.string "BUY"

        Sell ->
            JE.string "SELL"

        ActionError ->
            JE.null


decodeShare : JD.Decoder Share
decodeShare =
    JD.succeed Share
        |> JDP.required "id" (JD.nullable JD.int)
        |> JDP.required "quantity" JD.int
        |> JDP.required "cost" JD.float
        |> JDP.required "sale_date" JD.string
        |> JDP.required "wheel_id" JD.int
        |> JDP.required "action" decodeAction


encodeShare : Share -> JE.Value
encodeShare share =
    JE.object
        [ ( "quantity", JE.int share.quantity )
        , ( "cost", JE.float share.cost )
        , ( "sale_date", JE.string share.saleDate )
        , ( "action", encodeAction share.action )
        ]


decodeShareResponse : JD.Decoder Share
decodeShareResponse =
    JD.field "share" decodeShare


decodeShareListResponse : JD.Decoder (List Share)
decodeShareListResponse =
    JD.field "shares" (JD.list decodeShare)


decodeOption : JD.Decoder Option
decodeOption =
    JD.succeed Option
        |> JDP.required "id" (JD.nullable JD.int)
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


encodeOption : Option -> JE.Value
encodeOption option =
    JE.object
        [ ( "quantity", JE.int option.quantity )
        , ( "strike", JE.float option.strike )
        , ( "premium", JE.float option.premium )
        , ( "open", JE.bool option.open )
        , ( "sale_date", JE.string option.saleDate )
        , ( "exp_date", JE.string option.expDate )
        , ( "action", encodeAction option.action )
        , ( "optType"
          , case option.optType of
                Call ->
                    JE.string "CALL"

                Put ->
                    JE.string "PUT"

                TypeError ->
                    JE.null
          )
        ]


decodeOptionResponse : JD.Decoder Option
decodeOptionResponse =
    JD.field "option" decodeOption


decodeOptionListResponse : JD.Decoder (List Option)
decodeOptionListResponse =
    JD.field "options" (JD.list decodeOption)


decodeWheel : JD.Decoder Wheel
decodeWheel =
    JD.succeed Wheel
        |> JDP.required "id" (JD.nullable JD.int)
        |> JDP.required "ticker" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "subtotal" JD.float
        |> JDP.required "positions_closed" JD.bool
        |> JDP.required "assigned_shares" JD.bool
        |> JDP.hardcoded []
        |> JDP.hardcoded []


encodeWheel : Wheel -> JE.Value
encodeWheel wheel =
    JE.object
        [ ( "ticker", JE.string wheel.ticker )
        , ( "description", JE.string wheel.description )
        , ( "subtotal", JE.float wheel.subtotal )
        , ( "positions_closed", JE.bool wheel.positionsClosed )
        , ( "assigned_shares", JE.bool wheel.assignedShares )
        ]


decodeWheelResponse : JD.Decoder Wheel
decodeWheelResponse =
    JD.field "wheel" decodeWheel


decodeWheelListResponse : JD.Decoder (List Wheel)
decodeWheelListResponse =
    JD.field "wheels" (JD.list decodeWheel)
