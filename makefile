feivel: FORCE
	@echo "Compiling"
	cabal configure --user --enable-tests
	cabal build
	cabal test
	cabal install

golden: FORCE
	@echo "Golden Tests"
	shelltest --color --execdir test/gold/

tutorial: doc/tutorial/tutorial.md
	@echo "Build Tutorial"
	pandoc --from markdown+pandoc_title_block --to html --toc \
           --output doc/tutorial/tutorial.html \
           --number-sections doc/tutorial/tutorial.md

vis: FORCE
	find src -name '*.hs' | xargs graphmod -q -p > doc/dev/modules.dot

everything: feivel golden tutorial vis

FORCE:
