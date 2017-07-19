GHC = ghc -funbox-strict-fields -O2 

test: main.hs cpsmemo.hs
	${GHC} --make -o $@ main.hs cpsmemo.hs

clean : 
	rm -f *.o *.hi

