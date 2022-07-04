# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.06.00
# ---------------------------------------------------------------------------
PROJECT = ff2_r61.bpl
OBJFILES = ff2_r61.obj ..\source\ffsrsort.obj ..\source\ffsrcur.obj \
    ..\source\ffllcoll.obj ..\source\cocobase.obj ..\source\ffclbase.obj \
    ..\source\ffclbde.obj ..\source\ffclcfg.obj ..\source\ffclconv.obj \
    ..\source\ffclintf.obj ..\source\ffclreng.obj ..\source\ffcltbrg.obj \
    ..\source\ffdb.obj ..\source\fffile.obj ..\source\ffhash.obj \
    ..\source\ffllbase.obj ..\source\ffllcomm.obj ..\source\ffllcomp.obj \
    ..\source\fflldict.obj ..\source\fflleng.obj ..\source\ffllexcp.obj \
    ..\source\ffllgrid.obj ..\source\ffconst.obj ..\source\ffconvff.obj \
    ..\source\ffdbbase.obj ..\source\ffdtmsgq.obj ..\source\fflllgcy.obj \
    ..\source\fflllog.obj ..\source\ffllprot.obj ..\source\ffllreq.obj \
    ..\source\ffllthrd.obj ..\source\ffllunc.obj ..\source\ffllwsck.obj \
    ..\source\ffllwsct.obj ..\source\fflogdlg.obj ..\source\ffnetmsg.obj \
    ..\source\ffsql.obj ..\source\ffsqlbas.obj ..\source\ffsqldb.obj \
    ..\source\ffsqldef.obj ..\source\ffsqleng.obj ..\source\ffsrbase.obj \
    ..\source\ffsrbde.obj ..\source\ffsrblob.obj ..\source\ffsrcfg.obj \
    ..\source\ffsrcmd.obj ..\source\ffsrcvex.obj ..\source\ffsreng.obj \
    ..\source\ffsrfltr.obj ..\source\ffsrfmap.obj ..\source\ffsrfold.obj \
    ..\source\ffsrintf.obj ..\source\ffsrintm.obj ..\source\ffsrixhl.obj \
    ..\source\ffsrlock.obj ..\source\ffsrmgr.obj ..\source\ffsrsec.obj \
    ..\source\ffsrstat.obj ..\source\ffsrtran.obj ..\source\ffsrvdlg.obj \
    ..\source\ffstdate.obj ..\source\fftbbase.obj ..\source\fftbblob.obj \
    ..\source\fftbcryp.obj ..\source\fftbdata.obj ..\source\fftbdict.obj \
    ..\source\fftbindx.obj ..\source\fftbstrm.obj ..\source\ffutil.obj \
    ..\source\ffsrjour.obj
RESFILES = ff2_r61.res
MAINSOURCE = ff2_r61.cpp
RESDEPEN = $(RESFILES) ..\source\fflogdlg.dfm ..\source\ffsrvdlg.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = bdertl.lib
PACKAGES = rtl.bpi vcl.bpi dbrtl.bpi vcldb.bpi
SPARELIBS = rtl.lib vcl.lib dbrtl.lib vcldb.lib bdertl.lib
DEFFILE = 
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = 
SYSDEFINES = NO_STRICT;_VIS_NOLIB;USEPACKAGES
INCLUDEPATH = ..\source;$(BCB)\include;$(BCB)\include\vcl
LIBPATH = ..\source;$(bcb)\lib;$(BCB)\lib\obj
WARNINGS= -w-par -w-8027 -w-8026
PATHCPP = .;
PATHASM = .;
PATHPAS = .;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source;..\source
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Vx -Ve -Tkh30000 -X- -a8 -b- -k- -vi -c -tWM
IDLCFLAGS = -I..\source -I$(BCB)\include -I$(BCB)\include\vcl -src_suffix cpp -boa
PFLAGS = -$Y- -$L- -$D- -$A8 -v -M -JPHNE
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"TurboPower FlashFiler Runtime Package - VCL61" -aa -Tpp -Gpr -x -Gn -Gl \
    -Gi
# ---------------------------------------------------------------------------
ALLOBJ = c0pkg32.obj $(PACKAGES) sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mt.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif

!if $d(PATHOBJ)
.PATH.OBJ  = $(PATHOBJ)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<



# ---------------------------------------------------------------------------




