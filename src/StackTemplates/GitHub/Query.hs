module StackTemplates.GitHub.Query where

import           RIO
import qualified RIO.Text                   as Text

import           Data.Extensible
import           StackTemplates.GitHub.Data (PageInfo, Repository)

type Response = Record '[ "data" >: ResponseData ]

type ResponseData = Record '[ "search" >: SearchResult ]

type SearchResult = Record
  '[ "repositoryCount" >: Int
   , "pageInfo"        >: PageInfo
   , "edges"           >: [SearchResultEdge]
   ]

type SearchResultEdge = Record
  '[ "node" >: Repository
   ]

type SearchOpts = Record
   '[ "first" >: Int
    , "after" >: Maybe Text
    ]

searchRepositoryQuery :: Text -> SearchOpts -> Text
searchRepositoryQuery query opts = mconcat
  [ "query{search("
  , "query:", tshow query, ","
  , "type:REPOSITORY,"
  , toSearchArgsText opts
  , "){", Text.intercalate "," fields, "}}"
  ]
  where
    fields =
      [ "repositoryCount"
      , "pageInfo{ endCursor, hasNextPage }"
      , "edges{ node{ ... on Repository{ nameWithOwner, name, " <> obj <> " }}}"
      ]
    obj = "object(expression:\"master\"){ ... on Commit{ tree{ entries{ name, type }}}}"

toSearchArgsText :: SearchOpts -> Text
toSearchArgsText opts = mconcat
  [ "first:", tshow (opts ^. #first)
  , maybe "" (\txt -> ", after: " <> tshow txt) (opts ^. #after)
  ]
