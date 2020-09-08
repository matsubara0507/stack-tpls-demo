module StackTemplates.Hsfiles where

import           RIO
import qualified RIO.Text                   as Text

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as JSON
import           Data.Extensible
import           StackTemplates.GitHub.Data

type Hsfiles = Record
   '[ "name"   >: Text
    , "owner"  >: Text
    , "domain" >: Domain
    ]

data Domain
  = GitHub
  | GitLab
  | BitBucket
  deriving (Eq)

instance Show Domain where
  show GitHub    = "github"
  show GitLab    = "gitlab"
  show BitBucket = "bitbucket"

instance FromJSON Domain where
  parseJSON = JSON.withText "Domain" $ \case
    "github"    -> pure GitHub
    "gitlab"    -> pure GitLab
    "bitbucket" -> pure BitBucket
    txt         -> fail $ "expected 'github' or 'gitlab' or 'bitbucket', but '" ++ Text.unpack txt ++ "'"

instance ToJSON Domain where
  toJSON = JSON.String . tshow

domains :: [Domain]
domains = [ GitHub, GitLab, BitBucket ]

fromRepository :: Domain -> Repository -> [Hsfiles]
fromRepository domain repo = flip map files $ \file ->
     #name   @= (file ^. #name)
  <: #owner  @= getOwner repo
  <: #domain @= domain
  <: nil
  where
    files = filter isHsfiles $ maybe [] (view #entries . view #tree) (repo ^. #object)

isHsfiles :: TreeEntry -> Bool
isHsfiles ent = isBlob ent && Text.isSuffixOf ".hsfiles" (ent ^. #name)

toStackArg :: Hsfiles -> Text
toStackArg file = mconcat
  [ tshow (file ^. #domain), ":", file ^. #owner, "/", file ^. #name ]

toRawUrl :: Hsfiles -> Text
toRawUrl file = Text.intercalate "/" $
  case file ^. #domain of
    GitHub    -> [ "https://raw.githubusercontent.com", file ^. #owner, "stack-templates/master", file ^. #name ]
    GitLab    -> [ "https://gitlab.com", file ^. #owner, "stack-templates/raw/master", file ^. #name ]
    BitBucket -> [ "https://bitbucket.org", file ^. #owner, "stack-templates/raw/master", file ^. #name ]

toUrl :: Hsfiles -> Text
toUrl file = Text.intercalate "/" $
  case file ^. #domain of
    GitHub    -> [ "https://github.com", file ^. #owner, "stack-templates/blob/master", file ^. #name ]
    GitLab    -> [ "https://gitlab.com", file ^. #owner, "stack-templates/blob/master", file ^. #name ]
    BitBucket -> [ "https://bitbucket.org", file ^. #owner, "stack-templates/src/master", file ^. #name ]
