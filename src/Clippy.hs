{-# LANGUAGE OverloadedStrings #-}

module Clippy
  ( plugin
  )
where

import           Data.IORef
import qualified Data.Text             as T
import           Data.Text             (Text)
import           Data.Text.ICU.Replace
import           ErrUtils
import           GhcPlugins            hiding ((<>))
import           TcPluginM
import           TcRnTypes

plugin :: Plugin
plugin =
  defaultPlugin
  { tcPlugin = const $ Just $
      TcPlugin
      { tcPluginInit  = pure ()
      , tcPluginSolve = \_ _ _ _ -> pure $ TcPluginOk [] []
      , tcPluginStop  = const replaceMessages
      }
  , pluginRecompile = purePlugin
  }

replaceMessages :: TcPluginM ()
replaceMessages = do
  errsRef  <- tcl_errs . snd <$> getEnvs
  dynFlags <- hsc_dflags <$> getTopEnv
  let showMsgDoc = T.pack . showSDoc dynFlags

  (warns, errs) <- tcPluginIO $ readIORef errsRef
  let newWarns = replaceErrMsgDoc showMsgDoc <$> warns
  let newErrs  = replaceErrMsgDoc showMsgDoc <$> errs
  tcPluginIO $ writeIORef errsRef (newWarns, newErrs)

replaceErrMsgDoc :: (MsgDoc -> Text) -> ErrMsg -> ErrMsg
replaceErrMsgDoc f e = e { errMsgDoc = replaceMsgDocs f (errMsgDoc e) }

replaceMsgDocs :: (MsgDoc -> Text) -> ErrDoc -> ErrDoc
replaceMsgDocs f e = e
  { errDocImportant     = replaceMsgDocsGroup f "I" (errDocImportant e)
  , errDocContext       = replaceMsgDocsGroup f "C" (errDocContext e)
  , errDocSupplementary = replaceMsgDocsGroup f "S" (errDocSupplementary e)
  }

replaceMsgDocsGroup :: (MsgDoc -> Text) -> Text -> [MsgDoc] -> [MsgDoc]
replaceMsgDocsGroup showDoc label msgDocs = text . T.unpack <$> filtered
 where
  filtered = filter (not . T.null . T.strip) (T.lines replaced)
  replaced = replaceText wrapped
  wrapped  = showDoc . vcat $ wrapGroup label msgDocs

wrapGroup :: Text -> [MsgDoc] -> [MsgDoc]
wrapGroup label group =
  [open $ ">" <> label] ++ (wrapDoc label <$> group) ++ [close $ label <> "<"]

wrapDoc :: Text -> MsgDoc -> MsgDoc
wrapDoc label doc = vcat [open label, doc, close label]

open :: Text -> MsgDoc
open label = text . T.unpack $ ">" <> label <> ">"

close :: Text -> MsgDoc
close label = text . T.unpack $ "<" <> label <> "<"

replaceText :: Text -> Text
replaceText =
  replaceAll "(>>[ICS]>)|(<[ICS]<<)|(>[ICS]>)|(<[ICS]<)" ""
    . replaceAll "Couldn't match type"                 "Couldn't match"
    . replaceAll "Expected type"                       "Expected"
    . replaceAll "Actual type"                         "   Got"
    . replaceAll "Couldn't match expected type ‘(.*)‘" "Expected: $1"
    . replaceAll "            with actual type ‘(.*)‘" "     Got: $1"
    . replaceAll "Couldn't match expected type ‘(.*)’ with actual type ‘(.*)’"
                 "Expected: $1\n     Got: $2"
    -- . replaceAll "(?s)>>C>.*?<C<<"                    ""
    . replaceAll "(?s)In the \\w+ argument of.*?<C<<" "<C<<"
    . replaceAll "(?s)In the expression.*?<C<<"       "<C<<"
    . replaceAll "\\(bound at ([^)]*)\\)"             " -- $1"
    . replaceAll
        "Ambiguous type variable (‘\\w+’) arising from a use of (‘\\w+’)"
        "Type variable $1 is ambiguous in $2."
    . replaceAll "prevents the constraint (‘.+’) from being solved.*"
                 "Can't pick an instance for $1."
    . replaceAll "(Probable|Possible) fix:" "---\nMaybe-fix:"
    . replaceAll "use a type annotation to specify what.*"
                 "add type annotations to disambiguate."
    . replaceAll "No instance for (.*?) arising from (a|the)( use of)? (.*)"
                 "Need a $1 instance for usage of $4"
    . replaceAll "(?s)the type signature for:\n  " "\n"
    . replaceAll
        "(?s)These potential instances .*? -fprint-potential-instances to see them all\\)"
        "More info: compile with -fprint-potential-instances."
    . replaceAll "Relevant bindings include" "Known types:\n"
