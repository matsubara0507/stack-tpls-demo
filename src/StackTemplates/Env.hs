module StackTemplates.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub   as MixGitHub
import qualified Mix.Plugin.Logger   as MixLogger
import qualified Mix.Plugin.XdgCache as MixCache

type Env = Record
  '[ "logger"      >: MixLogger.LogFunc
   , "github"      >: MixGitHub.Token
   , "xdgcache"    >: MixCache.Config
   , "with_update" >: Bool
   ]
