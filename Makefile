all:
	lazbuild src/HaskellTool.lpr
	
clean:
	rm -rf src/lib src/*.res src/haskelltool
	
install:
	install -s src/haskelltool /usr/local/bin/
	
uninstall:
	rm /usr/local/bin/haskelltool
	