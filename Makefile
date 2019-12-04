.PHONY: ghcid

ghcid:
	cd aoc && make ghcid

test:
	cd aoc && cabal new-test