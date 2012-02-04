LAZ = $(shell which lazbuild)
LAZ_MSG = lazbuild not found. You need to apt-get install lcl
LAZ_ERROR = $(error $(LAZ_MSG))
ASSERT_LAZ = $(if $(LAZ), , $(LAZ_ERROR))

all:
	${ASSERT_LAZ}
	${LAZ} HaskellTool.lpr
	
clean:
	rm -rf lib *.res haskelltool
	
install:
	install -s haskelltool /usr/local/bin/
	
uninstall:
	rm /usr/local/bin/haskelltool
	