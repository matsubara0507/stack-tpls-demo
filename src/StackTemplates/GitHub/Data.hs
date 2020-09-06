module StackTemplates.GitHub.Data where

import           RIO
import qualified RIO.Text        as Text

import           Data.Extensible

type PageInfo = Record
   '[ "endCursor"   >: Maybe Text
    , "hasNextPage" >: Bool
    ]

type Commit = Record
   '[ "tree" >: Tree
    ]

type Tree = Record
   '[ "entries" >: [TreeEntry]
    ]

type TreeEntry = Record
   '[ "name" >: Text
    , "type" >: Text
    ]

isBlob :: TreeEntry -> Bool
isBlob ent = ent ^. #type == "blob"

type Repository = Record
   '[ "name"          >: Text
    , "nameWithOwner" >: Text
    , "object"        >: Maybe Commit
    ]

getOwner :: Repository -> Text
getOwner = Text.takeWhile (/= '/') . view #nameWithOwner
