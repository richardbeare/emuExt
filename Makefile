# a makefile to create installations of tRMA

windows:
	mkdir tmp
	R INSTALL -l tmp emuExt
	(cd tmp; zip -r emuExt.zip emuExt;mv emuExt.zip ../)
 
	${RM} -rf tmp

unix:
	tar czf emuExt.tgz emuExt

all: windows unix
