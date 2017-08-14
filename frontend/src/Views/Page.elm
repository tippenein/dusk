module Views.Page exposing (ActivePage(..), bodyId, frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Data.User as User exposing (User, Username)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util exposing ((=>))
import Views.Spinner exposing (spinner)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home
    -- | Login
    -- | Settings
    -- | Profile Username


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , content
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "dusk" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                lazy2 Util.viewIf isLoading spinner
                    :: navbarLink (page == Home) Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ text "cool" ] -- navbarLink (page == Login) Route.Login [ text "Sign in" ]

        Just user ->
            [ text "cool" ]
            -- [ navbarLink (page == Settings) Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
            -- , navbarLink
            --     (page == Profile user.username)
            --     (Route.Profile user.username)
            --     [ img [ class "user-pic", UserPhoto.src user.image ] []
            --     , User.usernameToHtml user.username
            --     ]
            -- , navbarLink False Route.Logout [ text "Sign out" ]
            -- ]


viewFooter : Html msg
viewFooter =
    div [ class "row" ]
        [ div [ class "col-lg-12" ]
              [ div [ class "footer pull-right"]
                    [ text "dusk" ]
              ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


{-| This id comes from index.html.

The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.

-}
bodyId : String
bodyId =
    "page-body"
