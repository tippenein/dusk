module Component.Curator where

import Control.Monad.Aff (Aff)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Data.Generic (gShow)
import Routes as Routes

import Helper (apiUrl, styleClass, placeholder)
import Import hiding (div)
import App.Data.Curator (Curator(..), Curators(..), decodeCurators)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State =
  { loading :: Boolean
  , curators :: Array Curator
  , error :: Maybe String
  }

data Input a
  = Noop a
  | GetCurators a

ui :: H.Component HTML Input Unit Routes.ChildAction Top
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , initializer: Just (H.action GetCurators)
    , finalizer: Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, curators: [], error: Nothing }

  eval :: Input ~> H.ComponentDSL State Input Routes.ChildAction Top
  eval = case _ of
    Noop next -> pure next
    GetCurators next -> do
      curators <- H.gets _.curators
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get (apiUrl <> "/curators")
      es <- pure $ decodeCurators response.response
      case es of
        Left e ->
          H.modify (_ { error = Just e, loading = false, curators = curators })
        Right (Curators {curators: res}) ->
          H.modify (_ { error = Nothing, loading = false, curators = res})
      pure next

render :: State -> H.ComponentHTML Input
render st =
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "loading..." else "") ]
        , viewCurators st.curators
        ]
    ]
  where
    viewCurators curators =
      div [ styleClass "curator-list" ] (map viewCurator curators)



viewCurator :: forall t. Curator -> HTML t (Input Unit)
viewCurator (Curator curator) =
  div [ styleClass "curator-profile row" ]
    [ div [ styleClass "col-sm-3" ]
      [ placeholder 200 200 ]
    , div [ styleClass "col-sm-6" ]
      [ h2_ [ text curator.name ] ]
    ]

