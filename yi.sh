set -e
cd yi/yi
case $1 in
	--rebuild)
		sudo cabal install --enable-library-profiling
esac
runhaskell HackerMain.hs
