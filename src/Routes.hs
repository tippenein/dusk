{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation
import AppType


uploadDirectory :: FilePath
uploadDirectory = "static/upload"

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f


-- http://www.yesodweb.com/book/routing-and-handlers
mkYesodData "App" [parseRoutes|
/static       StaticR Static appStatic
/favicon.ico  FaviconR GET
/robots.txt   RobotsR GET

/auth         AuthR Auth   getAuth

/             HomeR GET
/events       EventR GET
/admin/events AdminEventR GET POST
/profile      ProfileR GET
|]

-- /signup      SignupR  GET POST
-- /signout     SignoutR GET
