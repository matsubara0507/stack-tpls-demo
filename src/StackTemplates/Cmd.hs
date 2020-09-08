module StackTemplates.Cmd where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub.GraphQL   as GraphQL
import           Mix.Plugin.XdgCache         (withCacheOn)
import qualified Mix.Plugin.XdgCache         as MixCache
import           StackTemplates.Env
import           StackTemplates.GitHub.Data
import           StackTemplates.GitHub.Query
import           StackTemplates.Hsfiles

cmd :: RIO Env ()
cmd = fetchTplList

fetchTplList :: RIO Env ()
fetchTplList = do
  logDebug "run: fetch hsfiles"
  whenM (asks $ view #with_update) $ MixCache.expireCache "stack-teplates"
  tpls <- fetchTplList' `withCacheOn` "stack-teplates"
  forM_ tpls $ \tpl -> logInfo (display $ toStackArg tpl)

fetchTplList' :: RIO Env [Hsfiles]
fetchTplList' =
  mapHsfilesWithFilter <$> fetchTplListFromGitHub [] sOpts
  where
    sOpts = #first @= 100 <: #after @= Nothing <: nil

fetchTplListFromGitHub :: [Repository] -> SearchOpts -> RIO Env [Repository]
fetchTplListFromGitHub acc opts = do
  let query = searchRepositoryQuery "stack-templates in:name" opts
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
