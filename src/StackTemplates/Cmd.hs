module StackTemplates.Cmd where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub.GraphQL   as GraphQL
import           StackTemplates.Env
import           StackTemplates.GitHub.Data
import           StackTemplates.GitHub.Query
import           StackTemplates.Hsfiles

cmd :: RIO Env ()
cmd = fetchTplList

fetchTplList :: RIO Env ()
fetchTplList = do
  logDebug "run: fetch hsfiles"
  tpls <- mapHsfilesWithFilter <$> fetchTplListFromGitHub [] sOpts
  forM_ tpls $ \tpl -> logInfo (display $ toStackArg tpl)
  where
    sOpts = #first @= 100 <: #after @= Nothing <: nil

fetchTplListFromGitHub :: [Repository] -> SearchOpts -> RIO Env [Repository]
fetchTplListFromGitHub acc opts = do
  let query = searchQuery "stack-templates in:name" Repository opts
  logDebug $ "query: " <> display query
  resp <- GraphQL.fetch query :: RIO Env Response
  let page  = resp ^. #data ^. #search ^. #pageInfo
      repos = acc ++ map (view #node) (resp ^. #data ^. #search ^. #edges)
      opts' = opts & #after `set` (page ^. #endCursor)
  if | page ^. #hasNextPage -> fetchTplListFromGitHub repos opts'
     | otherwise            -> pure repos

mapHsfilesWithFilter :: [Repository] -> [Hsfiles]
mapHsfilesWithFilter = mconcat . map (fromRepository GitHub) . filter isStackTemplates

isStackTemplates :: Repository -> Bool
isStackTemplates repo = repo ^. #name == "stack-templates"

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
