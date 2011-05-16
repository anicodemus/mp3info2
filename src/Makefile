PFlags= -rtsopts -prof -auto-all -caf-all -fforce-recomp
Flags= -XExistentialQuantification -XTypeSynonymInstances -O2
build:
	ghc $(Flags) mp3.hs
pro:
	ghc $(PFlags) $(Flags) mp3.hs

clean:
	rm -rf *hi *o mp3