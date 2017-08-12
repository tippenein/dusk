module Request.Event exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)
import Data.Gen


authEvents : Maybe AuthToken -> Http.Request (List Data.Gen.Event)
authEvents s =
    Decode.field "events" (Decode.list Data.Gen.decodeEvent)
        |> Http.get (apiUrl "/events")

-- public, no auth
events : Http.Request (List Data.Gen.Event)
events = apiUrl "/events"
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "events" (Decode.list Data.Gen.decodeEvent)))
        |> HttpBuilder.toRequest

getJsonWith url decoder = url
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
