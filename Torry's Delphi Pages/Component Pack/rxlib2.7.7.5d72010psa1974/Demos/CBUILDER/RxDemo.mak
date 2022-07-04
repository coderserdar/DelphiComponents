# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = Rxdemo.exe
OBJFILES = Rxdemo.obj mainunit.obj About.obj ctrls.obj DBAware.obj \
   LinkUnit.obj Tools.obj PageDemo.obj
RESFILES = RxDemo.res
RESDEPEN = $(RESFILES) mainunit.dfm About.dfm ctrls.dfm DBAware.dfm Tools.dfm \
   PageDemo.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -w -k- -r -vi -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -I$(BCB)\projects;$(BCB)\bin;$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb \
   -H=$(BCB)\lib\vcl.csm 
PFLAGS = -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE \
   -U$(BCB)\projects;$(BCB)\bin;$(BCB)\rx;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rxcb \
   -I$(BCB)\projects;$(BCB)\bin;$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb \
   -$V- -$L- -$D- -JPHNV -M     
RFLAGS = -i$(BCB)\projects;$(BCB)\bin;$(BCB)\rx;$(BCB)\include;$(BCB)\include\vcl;e:\work\rxcb 
LFLAGS = -L$(BCB)\projects;$(BCB)\bin;$(BCB)\rx;$(BCB)\lib\obj;$(BCB)\lib;e:\work\rxcb \
   -aa -Tpe -x -V4.0 
IFLAGS = 
LINKER = ilink32
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
