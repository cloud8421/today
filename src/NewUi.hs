module NewUi where

import Options.Applicative.Help.Pretty

import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import qualified Refs

refList :: Refs.RefMap -> Doc
refList refMap = hardline <+> indent 1 content <+> hardline
  where
    content =
      if Map.null refMap
        then text "No ref lookup rules setup in the current Taskfile"
        else fillSep refItems
    refItems = map refItem (Map.toList refMap)
    refItem :: (Refs.Service, Refs.UrlTemplate) -> Doc
    refItem (service, urlTemplate) =
      text (unpack service) <+> text "->" <+> text (unpack urlTemplate)
