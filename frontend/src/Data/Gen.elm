module Data.Gen exposing (..)

import Json.Decode exposing (..)

import Json.Decode.Pipeline exposing (..)

import Json.Encode

import Http

import String

type alias Event =
    { eventName : String
    , eventDescription : Maybe (String)
    , eventAsset_id : String
    , eventOwner_id : Int
    , eventAll_day : Bool
    , eventStart_datetime : Date
    , eventEnd_datetime : Maybe (Date)
    , id : Int
    }

decodeEvent : Decoder Event
decodeEvent =
    decode Event
        |> required "eventName" string
        |> required "eventDescription" (maybe string)
        |> required "eventAsset_id" string
        |> required "eventOwner_id" int
        |> required "eventAll_day" bool
        |> required "eventStart_datetime" decodeDate
        |> required "eventEnd_datetime" (maybe decodeDate)
        |> required "id" int

encodeEvent : Event -> Json.Encode.Value
encodeEvent x =
    Json.Encode.object
        [ ( "eventName", Json.Encode.string x.eventName )
        , ( "eventDescription", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.eventDescription )
        , ( "eventAsset_id", Json.Encode.string x.eventAsset_id )
        , ( "eventOwner_id", Json.Encode.int x.eventOwner_id )
        , ( "eventAll_day", Json.Encode.bool x.eventAll_day )
        , ( "eventStart_datetime", (Json.Encode.string << toString) x.eventStart_datetime )
        , ( "eventEnd_datetime", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << toString)) x.eventEnd_datetime )
        , ( "id", Json.Encode.int x.id )
        ]

type alias User =
    { userIdent : String
    , userName : Maybe (String)
    , id : Int
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userIdent" string
        |> required "userName" (maybe string)
        |> required "id" int

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "userIdent", Json.Encode.string x.userIdent )
        , ( "userName", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.userName )
        , ( "id", Json.Encode.int x.id )
        ]