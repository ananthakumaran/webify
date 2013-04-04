run:
	cd src && runhaskell Webify.hs ./../fonts/proximanova-black-webify.ttf
	xxd ./fonts/proximanova-black-webify.eot > ./fonts/webify.hex
	xxd ./fonts/proximanova-black-test.eot > ./fonts/test.hex
	diff ./fonts/webify.hex ./fonts/test.hex
