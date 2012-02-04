LPR = HaskellTool.lpr
EXE = haskelltool
LAZ = $(shell which lazbuild)
ASSERT_LAZ = $(if $(LAZ), ,lazbuild_not_found)

all: ${ASSERT_LAZ}
	${LAZ} ${LPR}
	@echo "*** ${EXE} successfully built :-)"
	
clean:
	rm -rf lib *.res ${EXE}
	
install:
	install -s ${EXE} /usr/local/bin/
	
uninstall:
	rm /usr/local/bin/${EXE}
	
lazbuild_not_found:
	@echo "\n\
	*** ERROR \n\
	The lazbuild binary could not be found. In order to build this program\n\
	you need to have the Free Pascal compiler and the LCL units installed.\n\
	On a Debian or Ubuntu system this should be satisfied by\n\
	\n\
	  sudo apt-get install lcl\n\
	\n\
	which in turn would install the compiler along with all required tools,\n\
	units and other dependencies. Note that these are only build dependencies,\n\
	they are not needed to run the compiled program\n\
	"
	@exit 2
