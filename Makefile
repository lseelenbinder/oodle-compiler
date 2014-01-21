everything: compile build_happy

build_happy:
	happy Oodle.y
compile:
	ghc Main.hs -o oodle

