build_and_compile: build_happy compile 

build_happy:
	~/.cabal/bin/happy -gc -o Oodle/Parser.hs --magic=oodle Oodle.y
compile:
	ghc -O Main.hs -o oodle

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
	sudo apt-get install ghc6 cabal-install
	cabal update
	cabal install happy
