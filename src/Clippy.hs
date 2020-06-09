{-# LANGUAGE OverloadedStrings #-}

module Clippy
  ( plugin
  )
where

import           Data.Bifunctor
import           Data.Function
import           Data.IORef
import           Data.String           (fromString)
import qualified Data.Text             as T
import           Data.Text             (Text)
import           Data.Text.ICU         (regex)
import           Data.Text.ICU.Replace (replaceAll)
import           ErrUtils
import           GhcPlugins            hiding (Rule, (<>))
import           TcPluginM
import           TcRnTypes

plugin :: Plugin
plugin =
  defaultPlugin
  { tcPlugin = const $ Just $
      TcPlugin
      { tcPluginInit  = pure ()
      , tcPluginSolve = \_ _ _ _ -> pure $ TcPluginOk [] []
      , tcPluginStop  = const $ loadConfig >>= replaceMessages
      }
  , pluginRecompile = purePlugin
  }

newtype Config = Config { rules :: [Rule] }

data Rule = Rule
    { match   :: Text
    , replace :: Text
    }

data PEnv = PEnv
    { showMsgDoc :: MsgDoc -> Text
    , config     :: Config
    }

loadConfig :: TcPluginM Config
loadConfig = pure defaultConfig

replaceMessages :: Config -> TcPluginM ()
replaceMessages config = do
  errsRef  <- tcl_errs . snd <$> getEnvs
  dynFlags <- hsc_dflags <$> getTopEnv
  let showMsgDoc = T.pack . showSDoc dynFlags
      replaceErrMsgs = fmap $ replaceErrMsgDoc $ PEnv showMsgDoc config
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

wrapGroup :: Text -> [MsgDoc] -> [MsgDoc]
wrapGroup label group =
  [open $ ">" <> label] ++ (wrapDoc label <$> group) ++ [close $ label <> "<"]

wrapDoc :: Text -> MsgDoc -> MsgDoc
wrapDoc label doc = vcat [open label, doc, close label]

open :: Text -> MsgDoc
open label = text . T.unpack $ ">" <> label <> ">"

close :: Text -> MsgDoc
close label = text . T.unpack $ "<" <> label <> "<"

replaceText :: PEnv -> Text -> Text
replaceText env t = foldr replaceRule t (rules . config $ env)

replaceRule :: Rule -> Text -> Text
replaceRule rule = replaceAll (regex [] $ match rule) (fromString . T.unpack $ replace rule)

defaultConfig :: Config
defaultConfig = Config
    [ Rule "(>>[ICS]>)|(<[ICS]<<)|(>[ICS]>)|(<[ICS]<)" ""
    , Rule "Couldn't match type"                  "Couldn't match"
    , Rule "Expected type:"                       "Expected:"
    , Rule "  Actual type:"                       "     Got:"
    , Rule "Couldn't match expected type ‘(.*?)’" "Expected: $1"
    , Rule "            with actual type ‘(.*?)’" "     Got: $1"
    , Rule "Couldn't match expected type ‘(.*)’ with actual type ‘(.*)’"
                 "Expected: $1\n     Got: $2"
    -- , Rule "(?s)>>C>.*?<C<<"                    ""
    , Rule "(?s)In the \\w+ argument of.*?<C<<" "<C<<"
    , Rule "(?s)In the expression.*?<C<<"       "<C<<"
    , Rule "\\(bound at ([^)]*)\\)"             " -- $1"
    , Rule
        "Ambiguous type variable (‘\\w+’) arising from a use of (‘\\w+’)"
        "Type variable $1 is ambiguous in $2."
    , Rule "prevents the constraint (‘.+’) from being solved.*"
                 "Can't pick an instance for $1."
    , Rule "(Probable|Possible) fix:" "---\nMaybe-fix:"
    , Rule "use a type annotation to specify what.*"
                 "add type annotations to disambiguate."
    , Rule "No instance for (.*?) arising from (a|the)( use of)? (.*)"
                 "Need a $1 instance for usage of $4"
    , Rule "(?s)the type signature for:\n  " "\n"
    , Rule
        "(?s)These potential instances .*? -fprint-potential-instances to see them all\\)"
        "More info: compile with -fprint-potential-instances."
    , Rule "Relevant bindings include" "Known types:\n"
    ]
