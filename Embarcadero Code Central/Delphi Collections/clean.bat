@ECHO OFF
REM Copyright 2003 by Matthew Greet.  See test\Licence.rtf
erase *.dcu
erase *.bpl
erase *.dcp
erase *.~*
erase test\*.dcu
erase test\*.exe
erase test\*.bpl
erase test\*.dcp
erase test\*.~*
erase help\*.hlp
attrib -H help\*.gid
erase help\*.gid
