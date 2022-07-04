Compilation Readme
==================

To open UV project in Delphi IDE, you have first to install:
1. ATViewer package
2. Tnt Unicode Controls package (http://www.yunqa.de)

Before compiling UV project, it's recommended to edit
"Source\ATViewerOptions.inc" file, and uncomment there defines:
IVIEW, OFFLINE. Otherwise "IrfanView integration" button and
"View -- Internet -- Offline mode" option will be disabled.


Compiling without Tnt
=====================

You can compile UV without Tnt Unicode Controls:
1. by enabling TntFake units in UV project options
2. by commenting TNT define in all Source\*.inc files.

In this case program will show "(ANSI build)" in its About window.
(This is useful for debugging purposes, as Tnt will not install
its hooks and debugging will be easier.)



TListView patch for Delphi 5-6
==============================

Since program uses TListView control, VCL must be patched to prevent AV
during ListView drawing under Windows XP. Patch is the following. Open 
ComCtrls.pas, search for TCustomListView.UpdateColumn. Replace:

    if FImageIndex <> -1 then 
        fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES;

with:

     if FImageIndex <> -1 then 
       fmt := fmt or LVCFMT_IMAGE or LVCFMT_COL_HAS_IMAGES 
     else 
       mask := mask and not (LVCF_IMAGE);

Recompile and update ComCtrls.dcu in Delphi Lib folder.
Patch by Matteo Riso.


TOpenDialog/TSaveDialog patch for Delphi 5
==========================================

Delphi 5 doesn't support "Vertical buttons row on the left" style in browse
dialogs. Patch is the following.

1. Open commdlg.pas, search for 2 definitions:
  tagOFNA = packed record
  tagOFNW = packed record

  At the end of both records, after:
    lpTemplateName: PXXXChar;

  add fields:
    pvReserved: Pointer;
    dwReserved: DWORD;
    FlagsEx: DWORD;

2. Open Dialogs.pas, search for TOpenDialog.DoExecute.
  Replace:
    lStructSize := SizeOf(TOpenFilename);

  with:

    if (Win32MajorVersion >= 5) and (Win32Platform = VER_PLATFORM_WIN32_NT) or { Win2k }
    ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MajorVersion >= 4) and (Win32MinorVersion >= 90)) then { WinME }
      lStructSize := SizeOf(TOpenFilename)
    else
      lStructSize := SizeOf(TOpenFilename) - (SizeOf(DWORD) shl 1) - SizeOf(Pointer); { subtract size of added fields }

3. Recompile commdlg.pas and Dialogs.pas and update their *.DCU in Delphi Lib folder.

Patch by AT based on Delphi 7 code.
