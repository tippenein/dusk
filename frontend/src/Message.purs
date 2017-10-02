module Message where
import App.Crud
import Prelude

loggedOut = "Successfully logged out"
loginRequired = "You must be logged in for this action"

eventFormHeader = "Announce"
eventFormHelp = "Your followers will be notified of the upcoming event and the event will appear in the event list until the event ends"

class Translatable b where
  t :: String -> b -> String
  fallback :: b -> String

instance translatableCreateResponse :: Translatable CreateResponse where
  t "admin.curator" g = case g of
    CreateSuccess _ -> "Successfully invited"
    FailedUniquenessConstraint -> "This person has been sent an invite already"
    CreateFailure t -> "Failed to invite this person, sorry!"

  t "admin.event" g = case g of
      CreateSuccess _ -> "Event Created!"
      FailedUniquenessConstraint -> "This Event already exists"
      CreateFailure t -> fallback g

  t _ g = fallback g

  fallback (CreateSuccess _) = "Successfully Created!"
  fallback FailedUniquenessConstraint = "This already exists"
  fallback (CreateFailure t) = "Failed: " <> t

t' :: forall a. Translatable a => a -> String
t' = fallback
