set -e
cd yi/yi && cabal install
runhaskell HackerMain.hs
