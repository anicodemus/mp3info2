PFlags= -rtsopts -prof -auto-all -caf-all -fforce-recomp
Flags= -XFlexibleInstances -XExistentialQuantification -XTypeSynonymInstances -O -odir bin -hidir bin -isrc -o mp32
build:
	ghc $(Flags) src/Main.hs
pro:
	ghc $(PFlags) $(Flags) src/mp3.hs

clean:
	rm -rf bin/* mp32
