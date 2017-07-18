#! /bin/sh

cabal install --only-dependencies \
    && cabal build \
    && dist/build/rimworld-modlist-depsorter/rimworld-modlist-depsorter rules.txt \
    && dot -Tpng mod-dependencies.dot \
	|pngtopnm \
	|pnmcut -top 0 -height 1080 \
	|pnmtopng > example.png
