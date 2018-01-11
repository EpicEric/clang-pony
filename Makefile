build/clang-pony: build clang/*.pony
	ponyc clang -o build

build:
	mkdir build

clean:
	rm -rf build

.PHONY: clean
