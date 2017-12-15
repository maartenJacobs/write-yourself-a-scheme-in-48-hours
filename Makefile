# Utilises .hspec
test:
	cabal test --show-details=direct

lint:
	hlint lint --git --no-exit-code