
GHC = ghc -fglasgow-exts -fallow-overlapping-instances -O 

# unboxing doesn't seem to make much diff
GHC = ghc -fglasgow-exts -fallow-overlapping-instances -funbox-strict-fields -O2 

GHC = ghc -fglasgow-exts -fallow-overlapping-instances -O2 
# MAYBE: -fno-full-laziness


PROF = 
PFLAGS = 
HEAP=-H1000M -K20M

DATE=${shell date +"%a %d %h -- %H:%M:%S"}
NAME="${TESTCASE}-${DATE}"

test :
	${GHC} --make -rtsopts -o a.out ${PROF} Main.hs
	./a.out ${NAME}.log ${TESTCASE} +RTS -s ${HEAP} ${PFLAGS} -RTS 
	cp a.out.stat ${NAME}.stat
	cat a.out.stat 

prof :
	make it PROF="-prof -auto-all"

ptest : 
	make test PROF="-prof -auto-all" PFLAGS="-h -p" 
	echo TESTCASE=\"${TESTCASE}
	cp a.out.prof ${NAME}.full.prof
	reformat a.out.prof
	reformat-prof sorted.prof
	cp sorted.prof ${NAME}.prof
	cp sorted.gra  ${NAME}.gra
	hp2ps a.out.hp
	cp a.out.ps    ${NAME}.ps

ci :
	ci -l *hs

clean : 
	rm -f *.o *.hi

T1 : 
	make test TESTCASE="T1 ${TESTCASE}"
	gzip *log

T2 : 
	make test TESTCASE="T2 ${TESTCASE}"
	gzip *log

T3 : 
	make test TESTCASE="T3 ${TESTCASE}"
	gzip *log

T4 : 
	make test TESTCASE="T4 ${TESTCASE}"
	gzip *log

################################################################################
series :
	- make ${GRAM} TESTCASE="pp_ambig 10"
	- make ${GRAM} TESTCASE="pp_ambig 20"
	- make ${GRAM} TESTCASE="pp_ambig 30"
	- make ${GRAM} TESTCASE="pp_ambig 40"
	- make ${GRAM} TESTCASE="pp_ambig 50"
	- make ${GRAM} TESTCASE="pp_ambig 60"
	- make ${GRAM} TESTCASE="pp_ambig 70"
	- make ${GRAM} TESTCASE="pp_ambig 80"
	# - make ${GRAM} TESTCASE="pp_ambig 90"
	# - make ${GRAM} TESTCASE="pp_ambig 100"
	# - make ${GRAM} TESTCASE="pp_ambig 120"
	# - make ${GRAM} TESTCASE="pp_ambig 150"
	# - make ${GRAM} TESTCASE="pp_ambig 200"

all_tests :
	make -k series GRAM=T1
	make -k series GRAM=T2
	make -k series GRAM=T3
	make -k series GRAM=T4

STORE=run-${shell date +"%a%d%h--%H:%M:%S"}
 
report :
	make -k all_tests
	mkdir "${STORE}"
	mv ATIS* "${STORE}"
	egrep 'MUT.*time|%GC' ${STORE}/*stat | sed -e 's/.*\///;s/pp_ambig/pp/' | sort -n -k3,3 | sort --key=1,1 --stable > ${STORE}/table

################################################################################

aaa_series :
	- make TESTCASE="${GRAM} aaa 6"
	- make TESTCASE="${GRAM} aaa 12"
	- make TESTCASE="${GRAM} aaa 24"
	- make TESTCASE="${GRAM} aaa 36"
	- make TESTCASE="${GRAM} aaa 48"
	- make TESTCASE="${GRAM} aaa 72"
	- make TESTCASE="${GRAM} aaa 96"
	#- make TESTCASE="${GRAM} aaa 144"
	#- make TESTCASE="${GRAM} aaa 192"
	#- make TESTCASE="${GRAM} aaa 288"
	#- make TESTCASE="${GRAM} aaa 384"

AAA_STORE=aaa-run-${shell date +"%a%d%h--%H:%M:%S"}
aaa_run : 
	- make aaa_series GRAM=sm
	- gzip sm*aaa*log
	- make aaa_series GRAM=sml
	- gzip sm*aaa*log
	- make aaa_series GRAM=smml
	- gzip sm*aaa*log
	mkdir "${AAA_STORE}"
	mv s*aaa* "${AAA_STORE}"
	egrep 'MUT.*time|%GC' ${AAA_STORE}/*stat | sed -e 's/.*\///;' | sort -n -k3,3 | sort --key=1,1 --stable > ${AAA_STORE}/table


				  
################################################################################
zip : 
	zip faster.zip PP.lhs Parser*.lhs BasicTest.hs T_G*.hs Misc.hs Main.hs Makefile Extensions.lhs 
up : zip
	scp faster.zip vega2:

dist :
	zip distrib.zip PP.lhs Parser*.lhs BasicTest.hs T_G*.hs Misc.hs \
		Main.hs Extensions.lhs Makefile TODO LICENCE README

