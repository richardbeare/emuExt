# a makefile to create installations of tRMA

windows:
	mkdir tmp
	R CMD build --force emuExt
	R INSTALL -l tmp emuExt_0.1.tar.gz
	(cd tmp; zip -r emuExt.zip emuExt;mv emuExt.zip ../)
 
	${RM} -rf tmp

unix:
	R CMD build --force emuExt
##tar czf emuExt.tgz emuExt

all: windows unix
