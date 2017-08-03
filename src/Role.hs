module Role where


import Import.NoFoundation
import Model.Instances
import Model.User



-- XXX This should ideally be some hierarchy, but don't need it yet
checkRole
  :: Role    -- | role requested
  -> [Role]  -- | roles given
  -> Bool
checkRole role roles = role `elem` roles

isCurator :: Maybe UserId -> DB Bool
isCurator = isAuthenticated Curator

isAdmin :: Maybe UserId -> DB Bool
isAdmin = isAuthenticated Admin

canCreateCurator :: Maybe UserId -> DB Bool
canCreateCurator Nothing = return False
canCreateCurator (Just uid) = isAdmin (Just uid)

canCreateEvent :: Maybe UserId -> DB Bool
canCreateEvent Nothing = return False
canCreateEvent (Just uid) = do
  r <- isAdmin (Just uid)
  r' <- isCurator (Just uid)
  return $ r || r'

isAuthenticated :: Role -> Maybe UserId -> DB Bool
isAuthenticated role muid = do
  case muid of
    Nothing -> return False
    Just uid -> do
      roles <- getUserRoles uid
      return $ role `elem` roles
  -- return $ if b then Authorized else Unauthorized $ "Must at least be a(n) " <> tshow role

-- data PermissionTo = AllEvent | CreateEvent | CreateCurator

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
