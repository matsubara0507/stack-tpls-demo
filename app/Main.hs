module Main where

import           Paths_stack_tpls_demo  (version)
import           RIO

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           Mix
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           Mix.Plugin.XdgCache    as MixCache
import           StackTemplates.Cmd     (cmd)
import           System.Environment     (getEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version <> "\n")
     | otherwise     -> runCmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #update  @= updateOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "update"  >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

updateOpt :: OptDescr' Bool
updateOpt = optFlag [] ["update"] "Update stack templates list in local cache"

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts _path = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  let plugin = hsequence
             $ #logger      <@=> MixLogger.buildPlugin logOpts
            <: #github      <@=> MixGitHub.buildPlugin gToken
            <: #xdgcache    <@=> MixCache.buildPlugin "stack-tpls"
            <: #with_update <@=> pure (opts ^. #update)
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
