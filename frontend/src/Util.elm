module Util exposing (..)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Decode
import Date exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.

    session.user
        |> User.Request.foo
        |> Task.attempt Foo
        |> pair { model | something = blah }

-}
pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }

-- Thurs, Jan 1
formatDate : Date -> String
formatDate d =
    dayToString (dayOfWeek d)
    ++ ", "
    ++ monthToString (month d)
    ++ " "
    ++ toString (day d)

monthToString m = case m of
  Jan -> "Jan"
  Feb -> "Feb"
  Mar -> "Mar"
  Apr -> "Apr"
  May -> "May"
  Jun -> "Jun"
  Jul -> "Jul"
  Aug -> "Aug"
  Sep -> "Sep"
  Oct -> "Oct"
  Nov -> "Nov"
  Dec -> "Dec"

dayToString d = case d of
  Mon -> "Mon"
  Tue -> "Tue"
  Wed -> "Wed"
  Thu -> "Thu"
  Fri -> "Fri"
  Sat -> "Sat"
  Sun -> "Sun"
