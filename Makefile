exe:
	dune build
	mv ./src/lmoch.exe .

cleanall:
	dune clean
	rm ./lmoch.exe

%.ml:
	dune build
	mv ./src/lmoch.exe .

.PHONY: %.ex
%.ex:
	./lmoch.exe $(@:.ex=.lus) check -v > $(@:.ex=.lus).expected 2> /dev/null
