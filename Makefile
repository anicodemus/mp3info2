Flags=-XExistentialQuantification -XTypeSynonymInstances
build:
	ghc $(Flags) mp3.hs
clean:
	rm -rf *hi *o