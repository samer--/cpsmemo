GHC = ghc -funbox-strict-fields -O2 

test :
	${GHC} --make -o $@ main.hs cpsmemo.hs

clean : 
	rm -f *.o *.hi

