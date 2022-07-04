# ---------------------------------------------------------------------------
VERSION = BCB.01
# ---------------------------------------------------------------------------
!ifndef BCB
BCB = $(MAKEDIR)\..
!endif
# ---------------------------------------------------------------------------
PROJECT = RVEditCB1.exe
OBJFILES = RVUndoStr.obj CPFrm.obj PropFrm.obj PreviewFrm.obj ListFrm.obj \
   Unit1.obj RVEditCB1.obj
RESFILES = RVEditCB1.res
RESDEPEN = $(RESFILES) Unit1.dfm ListFrm.dfm PreviewFrm.dfm PropFrm.dfm \
   CPFrm.dfm
LIBFILES = 
DEFFILE = 
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Hc -w -k -r- -y -v -vi- -c -a4 -b- -w-par -w-inl -Vx -Ve -x
CFLAG2 = -Ie:\richview\cb1;$(BCB)\include;$(BCB)\include\vcl;e:\richview\temp \
   -H=$(BCB)\lib\vcld.csm 
PFLAGS = -Ue:\richview\cb1;$(BCB)\lib\obj;$(BCB)\lib;e:\richview\temp \
   -Ie:\richview\cb1;$(BCB)\include;$(BCB)\include\vcl;e:\richview\temp \
   -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -v \
   -$Y -$W -$O- -JPHNV -M 
RFLAGS = -ie:\richview\cb1;$(BCB)\include;$(BCB)\include\vcl;e:\richview\temp 
LFLAGS = -Le:\richview\cb1;$(BCB)\lib\obj;$(BCB)\lib;e:\richview\temp -aa -Tpe \
   -x -v -V4.0 
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
