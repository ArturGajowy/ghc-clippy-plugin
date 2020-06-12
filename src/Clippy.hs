{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Clippy
  ( plugin
  )
where

import           Bag
import           Control.Exception     (SomeException, tryJust)
import           Data.Bifunctor
import           Data.Function
import           Data.IORef
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text.ICU         (regex)
import           Data.Text.ICU.Replace (replaceAll)
import           Dhall
import           ErrUtils
import           GhcPlugins            hiding (Rule, (<>))
import           Prelude               hiding (print)
import           TcPluginM
import           TcRnTypes

plugin :: Plugin
plugin =
  defaultPlugin
  { tcPlugin = const $ Just $
      TcPlugin
      { tcPluginInit  = pure ()
      , tcPluginSolve = \_ _ _ _ -> pure $ TcPluginOk [] []
      , tcPluginStop  = const $ loadConfig >>= either cantInitializeWarning replaceMessages
      }
  , pluginRecompile = purePlugin
  }

newtype Config = Config { rules :: [Rule] } deriving Generic
instance FromDhall Config

data Rule = Rule
    { match :: Text
    , print :: Text
    }
    deriving Generic
instance FromDhall Rule

data PEnv = PEnv
    { showMsgDoc :: MsgDoc -> Text
    , config     :: Config
    }

loadConfig :: TcPluginM (Either String Config)
loadConfig = tcPluginIO
  . tryJust (Just . show @SomeException)
  $ inputFile auto "./.clippy.dhall"

cantInitializeWarning :: String -> TcPluginM ()
cantInitializeWarning cause = do
  dynFlags <- hsc_dflags <$> getTopEnv
  let warning = mkPlainWarnMsg dynFlags _span msgDoc
      _span   = mkGeneralSrcSpan $ mkFastString "ghc-clippy-plugin"
      msgDoc  = text "Clippy plugin couldn't start. Cause:" $$ text cause
  tcPluginIO $ printOrThrowWarnings dynFlags $ unitBag warning

replaceMessages :: Config -> TcPluginM ()
replaceMessages conf = do
  errsRef  <- tcl_errs . snd <$> getEnvs
  dynFlags <- hsc_dflags <$> getTopEnv
  let _showMsgDoc = T.pack . showSDoc dynFlags
      replaceErrMsgs = fmap $ replaceErrMsgDoc $ PEnv _showMsgDoc conf
  tcPluginIO $ modifyIORef errsRef (bimap replaceErrMsgs replaceErrMsgs)

replaceErrMsgDoc :: PEnv -> ErrMsg -> ErrMsg
replaceErrMsgDoc env e = e { errMsgDoc = replaceMsgDocs env (errMsgDoc e) }

replaceMsgDocs :: PEnv -> ErrDoc -> ErrDoc
replaceMsgDocs env e = e
  { errDocImportant     = replaceMsgDocsGroup env "I" (errDocImportant e)
  , errDocContext       = replaceMsgDocsGroup env "C" (errDocContext e)
  , errDocSupplementary = replaceMsgDocsGroup env "S" (errDocSupplementary e)
  }

replaceMsgDocsGroup :: PEnv -> Text -> [MsgDoc] -> [MsgDoc]
replaceMsgDocsGroup env label msgDocs = text . T.unpack <$> filtered
 where
  filtered = filter (not . T.null . T.strip) (T.lines replaced)
  replaced = replaceText env wrapped
  wrapped  = (env & showMsgDoc) . vcat $ wrapGroup label msgDocs

replaceText :: PEnv -> Text -> Text
replaceText env t = foldr replaceRule t (rules . config $ env)

replaceRule :: Rule -> Text -> Text
replaceRule rule = replaceAll (regex [] $ rule & match) (fromString . T.unpack $ rule & print)

wrapGroup :: Text -> [MsgDoc] -> [MsgDoc]
wrapGroup label group =
  [open $ ">" <> label] ++ (wrapDoc label <$> group) ++ [close $ label <> "<"]

wrapDoc :: Text -> MsgDoc -> MsgDoc
wrapDoc label doc = vcat [open label, doc, close label]

open :: Text -> MsgDoc
open label = text . T.unpack $ ">" <> label <> ">"

close :: Text -> MsgDoc
close label = text . T.unpack $ "<" <> label <> "<"
