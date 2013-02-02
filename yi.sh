set -e
cd yi/yi
case $1 in
	--rebuild)
		sudo cabal install
esac
runhaskell HackerMain.hs
