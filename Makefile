OPTIONS := -Wall

build: hpack
	cabal build --ghc-options='${OPTIONS}'

run: hpack
	cabal run

hpack:
	hpack .

test: hpack
	cabal test --ghc-options='${OPTIONS}'

format:
	alejandra .
	find src/ test/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

format-check:
	find src/ test/ -name "*.hs" -exec fourmolu -m check -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

ghcid: hpack
	ghcid -c "cabal repl --ghc-options='${OPTIONS}'"

clean:
	cabal clean

hlint:
	hlint .

.PHONY: build run hpack test format format-check ghcid clean hlint
