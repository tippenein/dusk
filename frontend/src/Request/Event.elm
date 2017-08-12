module Request.Event
    exposing
        ( events
        , authEvents
        )

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
-- import Data.User as User exposing (Username)
import Http
-- import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)
import Data.Gen


authEvents : Maybe AuthToken -> Http.Request (List Data.Gen.Event)
authEvents s =
    (Decode.list Data.Gen.eventDecoder)
        |> Http.get (apiUrl "/events")

-- public, no auth
events : String -> Http.Request (List Data.Gen.Event)
events s =
    Decode.field "events" (Decode.list Data.Gen.eventDecoder)
        |> Http.get (apiUrl "/events")
