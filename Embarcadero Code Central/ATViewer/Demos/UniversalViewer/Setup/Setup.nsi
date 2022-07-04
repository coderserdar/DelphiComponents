!include "Ver.nsh"

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;General

  ;Name and file
  Caption "Universal Viewer ${Ver} - Setup"
  Name    "Universal Viewer"
  OutFile "UniversalViewer.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\Universal Viewer"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\UniversalViewer" "InstallationFolder"

  ; Request application privileges for Windows Vista
  RequestExecutionLevel admin

  ; Other
  SetCompressor /FINAL lzma

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING
  !define MUI_FINISHPAGE_RUN "$INSTDIR\Viewer.exe"

  !define MUI_WELCOMEFINISHPAGE_BITMAP  "${NSISDIR}\Contrib\Graphics\Wizard\orange.bmp"
  !define MUI_UNWELCOMEFINISHPAGE_BITMAP  "${NSISDIR}\Contrib\Graphics\Wizard\orange-uninstall.bmp"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "License.txt"
  ;!insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  ;!insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Application" SecApp

  ;"All Users"
  SetShellVarContext all

  SetOutPath "$INSTDIR"
  File "..\Viewer.exe"
  File /oname=Nav.exe "..\Nav-Prot.exe"
  File "..\ijl15.dll"
  File "..\amnani.dll"
  File "..\un*.dll"
  File "..\v*.dll"

  SetOutPath "$INSTDIR\Language"
  File "..\Language\*.lng"
  
  SetOutPath "$INSTDIR\Help"
  File "..\Help\*.chm"

  SetOutPath "$INSTDIR\Icons"
  File "..\Icons\*.bmp"
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ;Create shortcuts
  CreateDirectory "$SMPROGRAMS\Universal Viewer"
  CreateShortCut "$SMPROGRAMS\Universal Viewer\Universal Viewer.lnk" "$INSTDIR\Viewer.exe"
  CreateShortCut "$SMPROGRAMS\Universal Viewer\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  CreateShortCut "$SMPROGRAMS\Universal Viewer\Universal Viewer Help.lnk" "$INSTDIR\Help\Viewer.English.chm"

  ;Create "Add/Remove Programs" item
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "DisplayName" "Universal Viewer"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "Publisher" "UVViewSoft"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "HelpLink"     "http://www.uvviewsoft.com/support.htm"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "URLInfoAbout" "http://www.uvviewsoft.com"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer" "NoRepair" 1
  
  ;Write installation folder
  WriteRegStr HKCU "Software\UniversalViewer" "InstallationFolder" $INSTDIR

  ;Write shell extension
  WriteRegStr HKCR "*\shell\Universal Viewer\command"         "" '"$INSTDIR\Viewer.exe" "%1"'
  WriteRegStr HKCR "Directory\shell\Universal Viewer\command" "" '"$INSTDIR\Viewer.exe" "@@%1"'

SectionEnd


;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;"All Users"
  SetShellVarContext all

  ;Delete files
  Delete "$INSTDIR\*.exe"
  Delete "$INSTDIR\*.dll"
  Delete "$INSTDIR\*.ini"

  Delete "$INSTDIR\Help\*.*"
  Delete "$INSTDIR\Icons\*.*"
  Delete "$INSTDIR\Language\*.*"

  RMDir "$INSTDIR\Help"
  RMDir "$INSTDIR\Icons"
  RMDir "$INSTDIR\Language"
  RMDir "$INSTDIR"

  ;Remove shortcuts
  Delete "$SMPROGRAMS\Universal Viewer\*.*"
  RMDir "$SMPROGRAMS\Universal Viewer"

  ;Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Universal Viewer"

  DeleteRegValue HKCU "Software\UniversalViewer" "InstallationFolder"
  DeleteRegValue HKCU "Software\UniversalViewer" "ConfigurationFolder"
  DeleteRegValue HKCU "Software\UniversalViewer" "StartMenuFolder"

  DeleteRegValue HKCR "*\shell\Universal Viewer\command" ""
  DeleteRegKey   HKCR "*\shell\Universal Viewer\command"
  DeleteRegKey   HKCR "*\shell\Universal Viewer"
  DeleteRegValue HKCR "Directory\shell\Universal Viewer\command" ""
  DeleteRegKey   HKCR "Directory\shell\Universal Viewer\command"
  DeleteRegKey   HKCR "Directory\shell\Universal Viewer"

SectionEnd
