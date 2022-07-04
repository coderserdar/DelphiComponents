all : main

DCC=/usr/local/bin/dcc
LIBPATH1=../../../
LIBPATH2=${PWD}

clean :
	find . -type f -iname "*~*" -exec rm {} \;
	find . -type f -iname "*.dcu" -exec rm {} \;
	find . -type f -iname "*.dpu" -exec rm {} \;
	find . -type f -iname "*.dcp" -exec rm {} \;
	find . -type f -iname "*.kof" -exec rm {} \;
	find . -type f -perm -755 -exec rm {} \;	

main  : 
	# compile the libraries
	${DCC} -B -W -H -O${LIBPATH1} -I${LIBPATH1} -R${LIBPATH1} -U${LIBPATH1} -U${LIBPATH2} EXml.dpr
