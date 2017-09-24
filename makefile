
all: Main
	./Main
	./to-pdf

Main: Main.hs CleanUp.hs CalMake.hs Svg.hs
	ghc Main.hs

clean:
	rm -vf cal*.svg cal*.pdf Main *.hi *.o
