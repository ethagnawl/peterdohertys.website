{-# LANGUAGE OverloadedStrings #-}

import Shelly

main = shelly $ do
    run "echo" ["hello", "world"]
    run "git" ["stash", "save"]
    run "git" ["checkout", "develop"]
    run "stack" ["clean"]
    run "stack" ["build"]
    run "git" ["checkout", "master"]
    run "cp" ["-a", "_site/.", "."]
    run "git" ["add", "-A"]
    run "git" ["commit", "-m", "'Publish.'"]
    run "git" ["push", "origin", "master"]
    run "git" ["checkout", "develop"]
    run "git" ["stash", "pop"]
    echo "Finished publishing to GitHub Pages."
