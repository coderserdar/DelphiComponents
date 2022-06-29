# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = Tnclient.exe
OBJFILES = \FPiette\CPP\Delphi\VC32\formpos.obj \
   \FPiette\CPP\Delphi\VC32\tnoptfrm.obj Tnclient.obj \
   \FPiette\CPP\INTERNET\tncli1.obj
RESFILES = tnclient.res
RESDEPEN = $(RESFILES) \FPiette\CPP\INTERNET\tncli1.dfm \
   \FPiette\CPP\Delphi\VC32\tnoptfrm.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Hc -w -k -r- -y -v -vi- -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -Id:\fpiette\cpp\delphi\vc32;d:\fpiette\cpp\internet;d:\fpiette\delphi\vc32;$(BCB)\include;$(BCB)\include\vcl \
   -H=$(BCB)\lib\vcld.csm 
PFLAGS = -Ud:\fpiette\cpp\delphi\vc32;d:\fpiette\cpp\internet;d:\fpiette\delphi\vc32;$(BCB)\lib\obj;$(BCB)\lib \
   -Id:\fpiette\cpp\delphi\vc32;d:\fpiette\cpp\internet;d:\fpiette\delphi\vc32;$(BCB)\include;$(BCB)\include\vcl \
   -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -v \
   -$Y -$W -$O- -JPHNV -M     
RFLAGS = -id:\fpiette\cpp\delphi\vc32;d:\fpiette\cpp\internet;d:\fpiette\delphi\vc32;$(BCB)\include;$(BCB)\include\vcl 
LFLAGS = -Ld:\fpiette\cpp\delphi\vc32;d:\fpiette\cpp\internet;d:\fpiette\delphi\vc32;$(BCB)\lib\obj;$(BCB)\lib \
   -aa -Tpe -x -v -V4.0 
IFLAGS = 
LINKER = tlink32
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) vcl.lib import32.lib cp32mt.lib 
# ---------------------------------------------------------------------------
.autodepend

$(PROJECT): $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES) 
!

.pas.hpp:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.pas.obj:
    $(BCB)\BIN\dcc32 $(PFLAGS) { $** }

.cpp.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $* 

.c.obj:
    $(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**

.rc.res:
    $(BCB)\BIN\brcc32 $(RFLAGS) $<
#-----------------------------------------------------------------------------
