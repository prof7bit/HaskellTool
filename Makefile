all:
	${MAKE} -C src all
	
install:
	${MAKE} -C src install
	
uninstall:
	${MAKE} -C src uninstall
	
clean:
	${MAKE} -C src clean

