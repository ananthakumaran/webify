run:
	cabal build
	./dist/build/webify/webify +RTS -xc -RTS ./fonts/proximanova-black-webify.ttf
	xxd ./fonts/proximanova-black-webify.eot > ./fonts/webify.hex
	xxd ./fonts/proximanova-black-test.eot > ./fonts/test.hex
	diff ./fonts/webify.hex ./fonts/test.hex
	cd ./fonts && rm proximanova-black-webify_validate.html && ./woff_validate proximanova-black-webify.woff
