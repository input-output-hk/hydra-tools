{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib.Data.Text where

import           Data.Text (Text)
import qualified Data.Text as Text

indent :: Text -> Text
indent = Text.unlines . (map indentLine) . Text.lines

indentLine :: Text -> Text
indentLine = ("    " <>)
