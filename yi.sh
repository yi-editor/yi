set -e
cd yi/yi && sudo cabal install
runhaskell HackerMain.hs
