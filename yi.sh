set -e
cd yi/yi
case $1 in
	--rebuild)
		cabal install
esac
runhaskell HackerMain.hs
