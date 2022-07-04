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

VERSION = BCB.05.03
# ---------------------------------------------------------------------------
PROJECT = EXMLPro.exe
OBJFILES = ExProps.obj EXMLPro.obj ExMain.obj ExAttr.obj ExCommnt.obj ExURL.obj \
    ExText.obj ExUtil.obj ExSelAtt.obj ExProcIn.obj exPrefs.obj exErr.obj \
    ExElemnt.obj ExChildW.obj ExView.obj
RESFILES = EXMLPro.res
MAINSOURCE = EXMLPro.cpp
RESDEPEN = $(RESFILES) ExMain.dfm ExAttr.dfm ExCommnt.dfm ExURL.dfm ExText.dfm \
    ExSelAtt.dfm ExProcIn.dfm exPrefs.dfm exErr.dfm ExElemnt.dfm ExChildW.dfm \
    ExView.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = vcl50.lib
PACKAGES = vclx50.bpi vcl50.bpi vcldb50.bpi vclbde50.bpi vcldbx50.bpi bcbsmp50.bpi \
    dclocx50.bpi qrpt50.bpi teeui50.bpi vclsmp50.bpi teedb50.bpi tee50.bpi \
    ibsmp50.bpi nmfast50.bpi inetdb50.bpi inet50.bpi
SPARELIBS = vcl50.lib
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;
PATHASM = .;
PATHPAS = .;
PATHRC = .;
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = XPDPRO
SYSDEFINES = NO_STRICT;_RTLDLL;_VIS_NOLIB
INCLUDEPATH = ..\..\..;$(BCB)\projects;$(BCB)\include;$(BCB)\include\vcl;..\..\..\headers\hpp5;..\..\..\hpp5
LIBPATH = ..\..\..;$(BCB)\projects;$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-par -w-8027 -w-8026
# ---------------------------------------------------------------------------
CFLAG1 = -Od -H=e:\dev\bcb\5.0\lib\vcl50.csm -Hc -Vx -Ve -Tkh30000 -X- -r- -a8 -b- \
    -k -y -v -vi- -c -tW -tWM
IDLCFLAGS = -I..\..\.. -I$(BCB)\projects -I$(BCB)\include -I$(BCB)\include\vcl \
    -I..\..\..\headers\hpp5 -I..\..\..\hpp5 -src_suffix cpp -DXPDPRO -boa
PFLAGS = -$YD -$W -$O- -v -M -JPHNE
RFLAGS = 
AFLAGS = /mx /w2 /zi
LFLAGS = -D"" -aa -Tpe -x -Gn -v
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj Memmgr.Lib sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mti.lib
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
# ---------------------------------------------------------------------------
$(PROJECT): $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
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




