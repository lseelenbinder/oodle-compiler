compile: Main.hs Oodle/Parser.hs stdlib.o
	ghc -O Main.hs -o oodle

Oodle/Parser.hs: Oodle.y
	~/.cabal/bin/happy -gac -o Oodle/Parser.hs --magic=oodle Oodle.y

stdlib.o: stdlib.c
	gcc -ostdlib.o -c stdlib.c

test:
	@echo "Testing Oodle Compiler...\n(an empty diff is a passing test)\n\n"
	@echo "Testing with debug flag..."
	./oodle -l -ds test_files/phase1_test.ood | diff test_files/phase1_debug_test.txt -
	@echo
	@echo "Testing without debug flag..."
	./oodle -l test_files/phase1_test.ood | diff test_files/phase1_test.txt -
	@echo
	@echo "Testing multiple files..."
	./oodle -l test_files/phase1_test.ood test_files/phase2_test.ood | diff test_files/phase1_multiple_test.txt -
	@echo "\n********************"
	@echo "***ALL TESTS PASS***"
	@echo "********************"

install_build_tools:
	sudo apt-get install ghc cabal-install
	cabal update
	cabal install happy
	cabal install split
