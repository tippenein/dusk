{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RankNTypes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.GoogleEmail2 (forwardUrl, authGoogleEmail)
import Yesod.Default.Util   (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import Routes
import AppType
import Model.Instances

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

navLayout :: Maybe (Entity User) -> Widget
navLayout user =
  [whamlet|
<div class="top-bar">
  <div class="top-bar-left">
    <ul class="menu">
      <li .menu-logo>
        <a href="@{HomeR}" .plain>RSVP
  <div class="top-bar-right">
    <ul class="menu">
      $maybe _ <- user
        <li>
          <a href="@{ProfileR}">Signout
      $nothing
        <li>
          <a href="@{AuthR LoginR}">Login
|]

baseLayout :: Html -> Maybe (Entity User) -> WidgetT App IO () -> Handler Html
baseLayout title user content = do
  defaultLayout $ do
    setTitle title
    [whamlet|
^{navLayout user}
^{content}
|]

errorFragment' :: Maybe Text -> Text -> Widget
errorFragment' mmsg t =
  [whamlet|
<div #error-block .container-lg>
  <h1 .error-title>#{t}
  $maybe msg <- mmsg
    <h2 .error-explanation>
      #{msg}
|]

errorFragment :: Text -> Widget
errorFragment = errorFragment' Nothing

instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout widget = do


        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Curators"
                    , menuItemRoute = CuratorsR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Events"
                    , menuItemRoute = AdminEventR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = userIdent $ snd $ fromMaybe (error "no user") muser
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- isAuthorized route isWrite = do
    --   mauth <- maybeAuthPair
    --   mauth `isAuthorizedTo` permissionsRequiredFor route isWrite
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (CuratorR _) _ = return Authorized
    isAuthorized CuratorsR _ = return Authorized

    isAuthorized ProfileR _ = isLoggedIn
    isAuthorized EventsR _ = return Authorized
    isAuthorized (EventR _) _ = return Authorized
    isAuthorized AdminEventR _ = isAdmin

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb EventsR = return ("Events", Nothing)
  breadcrumb (EventR _) = return ("Event", Just EventsR)
  breadcrumb AdminEventR = return ("Events", Just HomeR)
  breadcrumb  _ = return ("Home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    authenticate creds = runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing -> do
          user_id <- insert User
            { userIdent = credsIdent creds
            , userName = Nothing
            }

          _ <- insert $ UserRole user_id Fan
          return $ Authenticated user_id

    authPlugins app = [authGoogleEmail authKey authSecret] ++ extraAuthPlugins
      where
        settings = appSettings app
        authKey = appGoogleAuthKey settings
        authSecret = appGoogleAuthSecret settings

        extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    authHttpManager = getHttpManager

    loginHandler = lift $ do
      app <- getYesod
      murl <- runInputGet $ iopt textField "dest"
      mapM_ setUltDest murl

      defaultLayout $ do
        setTitle' "Login"
        $(widgetFile "login")

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  muid <- maybeAuthId
  return $ case muid of
      Nothing -> Unauthorized "You must log in to access this page"
      Just _  -> Authorized

data PermissionTo = AllEvent | CreateEvent | CreateCurator

-- permissionsRequiredFor :: Route App -> Bool -> [PermissionTo]
-- permissionsRequiredFor AdminEventR   True = [CreateEvent]
-- -- permissionsRequiredFor AdminCuratorR True = [Comment]
-- permissionsRequiredFor _ _                = []

-- hasPermissionTo :: (UserId, User)
--                 -> PermissionTo
--                 -> DB AuthResult
-- hasPermissionTo (uid, _user) CreateEvent = do
--   b <- isAdmin uid -- <|> isCurator uid
--   return $ if b then Authorized else Unauthorized "Not allowed to Create Events"
--   -- | otherwise      = return $ Unauthorized "Can not create events" -- Msg.NoCreateEvent
-- hasPermissionTo (uid, _user) AllEvent = do
--   b <- isAdmin uid
--   return $ if b then Authorized else Unauthorized "Not allowed to Modify Events"
-- hasPermissionTo (uid, _user) CreateCurator
--   | _ <- isAdmin uid    = return Authorized
--   | otherwise      = return $ Unauthorized "not an admin" -- msg.notanadmin


-- isAuthorizedTo :: Maybe (UserId, User)
--                -> [PermissionTo]
--                -> Handler AuthResult
-- isAuthorizedTo _        []     = return Authorized
-- isAuthorizedTo Nothing  (_:_)  = return AuthenticationRequired
-- isAuthorizedTo (Just u)   (p:ps) = do
--   r <- runDB $ u `hasPermissionTo` p
--   case r of
--     Authorized -> (Just u) `isAuthorizedTo` ps
--     _          -> return r -- unauthorized

-- XXX This is overly simplistic
-- checkRole
--   :: Role    -- | role requested
--   -> [Role]  -- | roles given
--   -> AuthResult
-- checkRole role roles =
--   if role `elem` roles

isCurator :: Handler AuthResult
isCurator = isAuthenticated Curator

isAdmin :: Handler AuthResult
isAdmin = isAuthenticated Admin

isAuthenticated :: Role -> Handler AuthResult
isAuthenticated role = do
  muid <- maybeAuthId
  b <- case muid of
    Nothing -> return False
    Just uid -> do
      roles <- runDB $ getUserRoles uid
      return $ role `elem` roles
  return $ if b then Authorized else Unauthorized $ "Must at least be a(n) " <> tshow role

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger


runDBor404 :: DB (Maybe a) -> Handler a
runDBor404 dba = do
  ma <- runDB dba
  case ma of
    Nothing -> notFound
    Just a -> return a

setTitle' :: MonadWidget m => Text -> m ()
setTitle' t = setTitle $ toHtml $ "RSVP - " <> t

-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
 
