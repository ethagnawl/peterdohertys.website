{-# LANGUAGE OverloadedStrings #-}

import Shelly

main = shelly $ do
  run "git" ["stash"]
  run "stack" ["clean"]
  run "stack" ["setup"]
  run "stack" ["build"]
  run "git" ["checkout", "master"]
  run "cp" ["-a", "_site/.", "."]
  run "git" ["add", "-A"]
  run "git" ["commit", "-m", "Publish to GitHub Pages"]
  run "git" ["push", "origin", "master"]
  run "git" ["checkout", "develop"]
  run "git" ["stash", "pop"]
  echo "Finished publishing to GitHub Pages."
