GHC=ghc -O2 --make -threaded -fforce-recomp

all:
	$(GHC) binary-trees.hs
	$(GHC) knuc.hs
	$(GHC) pidigits.hs
	ghc-gc-tune -t pngcairo ./binary-trees 16       --RTS +RTS -N2
	ghc-gc-tune -t pngcairo ./pidigits 730          --RTS +RTS -N2
	ghc-gc-tune -t pngcairo ./knuc fasta.500000.txt --RTS +RTS -N2
	ghc-gc-tune -t pngcairo ghc -v0 -O2 -fforce-recomp --make knuc.hs -threaded
	cp /tmp/{binary-treees,pidigits,knuc,ghc}-gc-space.png .
