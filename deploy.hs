{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Data.Text as T

default (T.Text)

main = shelly $ verbosely $ errExit False $ do
  _ <- escaping False $ cmd "test" "-z" "\"$(git status --porcelain)\""
  status <- lastExitCode
  if status == 1 then
    do
      errorExit "You have uncommitted changes."
  else
    do
      run "stack" ["clean"]
      run "stack" ["setup"]
      run "stack" ["build"]
      run "git" ["checkout", "master"]
      run "cp" ["-a", "_site/.", "."]
      run "git" ["add", "-A"]
      run "git" ["commit", "-m", "Publish to GitHub Pages"]
      run "git" ["push", "origin", "master"]
      run "git" ["checkout", "develop"]
      echo "Finished publishing to GitHub Pages."
      exit 0
