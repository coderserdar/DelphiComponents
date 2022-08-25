!include <win32.mak>

# CAPICOM_DLL_INSTALLED=1

!ifndef CAPICOM_DLL_INSTALLED
!message To build this sample you must have the capicom.dll installed. 
!message When Capicom is installed, set CAPICOM_DLL_INSTALLED=1 
!message in the sample makefile and build.
!else

ALL : $(OUTDIR) $(OUTDIR)\Store.exe

$(OUTDIR) :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

LINK32_OBJS= \
             $(OUTDIR)\Store.obj

.cpp{$(OUTDIR)}.obj:
    $(cc) $(cdebug) $(cflags) $(cvars) /GX /D "_CONSOLE" /D "_MBCS" /D "_AFXDLL" /Fo"$(OUTDIR)\\" /Fd"$(OUTDIR)\\" /D"_WIN32_DCOM" $**

$(OUTDIR)\Store.exe : $(LINK32_OBJS)
    $(link) $(ldebug) $(conlflags) /PDB:$(OUTDIR)\Store.pdb /machine:$(CPU) -out:$(OUTDIR)\Store.exe $(LINK32_OBJS) $(conlibs)

CLEAN : 
        $(CLEANUP)
!endif
