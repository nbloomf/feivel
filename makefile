feivel: FORCE
	@echo "Compiling"
	cabal configure --user --enable-tests
	cabal build
	cabal test
	cabal install

everything: feivel golden tutorial vis

tutorial: doc/tutorial/tutorial.md
	@echo "Build Tutorial"
	pandoc --from markdown+pandoc_title_block --to html --toc \
           --self-contained \
           --output doc/tutorial/tutorial.html \
           --number-sections doc/tutorial/tutorial.md

vis: FORCE
	find src -name '*.hs' | xargs graphmod -q -p > doc/dev/modules.dot
	dot -Tsvg doc/dev/modules.dot > doc/dev/modules.svg
	rm doc/dev/modules.dot


# Golden Tests

golden: FORCE
	@echo "Golden Tests"
	shelltest --color --execdir test/gold/ -- --threads=16 --hide-successes

FORCE:
