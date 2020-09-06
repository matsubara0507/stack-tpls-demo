module StackTemplates.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as MixGitHub
import qualified Mix.Plugin.Logger as MixLogger

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "github" >: MixGitHub.Token
   ]
