unit CakProcs;

// ## GC function Get_fileSize(FileName: String): Integer removed
// ## GC procedure OpenFolder(Folder: String); added
// ## GC procedure ExploreFolder(Folder: String); modified
// ## procedure FileSetDate from Zipsfx code

interface

uses
  CakDefs2, Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  FileCtrl, Registry, Inifiles, Shellapi, ShlObj, Graphics, MAPI, Floppy,
  Vwin32, Links, Jpeg, Syncobjs, ActiveX, Comobj;

var
  CancelWait, TerminateRun: Boolean;
  AForm:     TForm;
  ACheckbox: TCheckbox;
  ALabel:    TStaticText;

//Path Grabbing
function GrabCurrentPath: string;
function GrabDesktopPath: string;
function GrabMydocuPath: string;
function GrabProgramPath: string;
function GrabSystemPath: string;
function GrabTempPath: string;
function GrabWindowPath: string;

//Slash Functions
function AppendSlash(Input: string): string;
function ModifySlash(Input: string): string; overload;
function ModifySlash(Input: string; Fromm, tto: Char): string; overload;
function RemoveSlash(Input: string): string;
function RemoveFileExt(Input: string): string;
function RemoveDrive(Input: string): string;

//Files and Directories
function ConvFNameToDos(ExtFileName: string): string;
function SizeInK(Size: Int64): string;
function IsHarddrive(Drive: Char): Boolean;
function IsCDRom(Drive: Char): Boolean;
function IsFloppy(Drive: Char): Boolean;
function IsLocked(FileName: string): Boolean;

procedure Run(ProgramPath, ProgramParam: string);
procedure RunAndwait(ProgramPath, ProgramParam: string);
procedure RunWWW(WWWPath: string);
procedure ExecReg(var Path: string);
function ExecInf(var Path, Param: string): Cardinal;
procedure OpenFolder(Folder: String);
procedure ExploreFolder(Folder: String);
procedure File_Set_Date(const hFile: THandle; const iAge: Integer); // ## GC
function Get_File_Size(const FileName: String): Int64;
function CalcFolderSize(const aRootPath: string): Int64;
procedure MakeDirectory(DirName: string);
function PollFileList(MaskedName: string; SubDir: Boolean): TStrings;
function SubDirList(Dir: string): TStrings;
function DeleteAllFiles(FilesOrDir: string): Boolean;
procedure DeleteDir(aDir: string);
function CreateShortcut(LinkFileName, FilePath: string): Boolean;overload;
//www.SwissDelphiCenter.ch - create a shortcut in the Startmenu, on the Desktop
//Author: Tom (tom@swissdelphicenter.ch) - Added by GC July31, 2001
procedure CreateShortcut(SourceFileName: String; Location: ShortcutType;
  SubDirectory: String); overload;

function DiskMakeImage(Drive: Integer; FileName: string): Boolean;
function DiskWriteImage(Drive: Integer; FileName: string): Boolean;

procedure DiskUnSpan(FileName: string);
function DiskSpan(Source, Target: string; DiskSize: Longint;
  MakeBatch: Boolean): Integer;
procedure Combine(FirstFile, SecondFile, ThirdFile, TargetFile: string);

//INI support features
function GetValInIni(FileName: string; Section: string; Key: string;
  default: string): string;
procedure SetValInIni(FileName: string; Section: string; Key, Value: string);

//Registry support features
function GetValInReg(RKey: HKey; KeyPath: string; Valname: string): string;
procedure SetValInReg(RKey: HKey; KeyPath: string; ValName: string; NewVal: string);
procedure DelValInReg(RKey: HKey; KeyPath: string; Key: string);
procedure DelKeyInReg(RKey: HKey; KeyPath: string);
function MakeRegnode(RootKey: HKEY; Path: AnsiString): TList;
procedure CleanRegnode(aList: TList);
procedure AddRegnode(RootKey: Hkey; aList: TList; var aString: TStrings;
  Key, SubKey: string);
procedure CompareRegnode(RootKey: HKEY; List1, List2: TList;
  var aString: TStrings; Key, SubKey: string);
function RKeyName(RootKey: HKEY): string;
function Name2RKey(Key: string): HKey;
function RegListsubkey(RKey: HKey; KeyPath: string): TStrings;
function RegListVal(RKey: HKey; KeyPath: string): TStrings;
procedure RegBackup(RKey: HKey; KeyPath, Value: string; FileName: string);

//Associating
procedure AssociateProgram(Ext, Path, Icon: string);
procedure UnAssociateProgram(Ext: string);
function GetAssociatedProgram(Ext: string): string;

//Simple dialogs
procedure PlainDialog;
procedure FreePlainDialog;
function ShowAgainDialog(DCaption, Msg: string): Boolean;
procedure RegAskShowAgainDialog(DCaption, Msg: string; Path, Key: string);
procedure IniAskShowAgainDialog(DCaption, Msg: string; FileName,
  Section, Key: string);
function YesNoShowAgainDialog(DCaption, Msg: string; var YesNo: Boolean): Boolean;
procedure RegYesNoAskShowAgainDialog(DCaption, Msg: string; Path,
  Section, Key: string; var YesNo: Boolean);
procedure IniYesNoAskShowAgainDialog(DCaption, Msg: string; FileName,
  Product, Section, Key: string; var YesNo: Boolean);

//Misc
function TColor2WebColor(aColor: TColor): string;
procedure SendMail(Subject, Mailtext, FromName, FromAdress, ToName,
  ToAdress, AttachedFileName, DisplayFileName: string; ShowDialog: Boolean);

implementation

function GrabCurrentPath: string;
var
  Path: array [0..260] of Char;
begin
  GetCurrentDirectory(SizeOf(Path), Path);
  Result := AppendSlash(Path);
end;

function GrabDesktopPath: string;
begin
  Result := SpecialDirectory(CSIDL_Desktopdirectory);
end;

function GrabMydocuPath: string;
var
  Path:       array [0..260] of Char;
  ItemIDList: PItemIDList;
begin
  SHGetSpecialFolderLocation(Application.handle, CSIDL_PERSONAL, ItemIDList);
  SHGetPathFromIDList(ITEMIDLIST, Path);
  Result := AppendSlash(Path);
end;

function GrabProgramPath: string;
begin
  Result := AppendSlash(ExtractFilePath(ParamStr(0)));
end;

function GrabSystemPath: string;
var
  Path: array [0..260] of Char;
begin
  GetSystemDirectory(Path, SizeOf(Path));
  Result := AppendSlash(Path);
end;

function GrabTempPath: string;
var
  Path: array [0..260] of Char;
begin
  GetTempPath(SizeOf(Path), Path);;
  MakeDirectory(AppendSlash(Path) + TMPDIR); // ## GC TMPDIR def in CakDefs
  Result := AppendSlash(Path) + TMPDIR; // ## GC TMPDIR def in CakDefs
end;

function GrabWindowPath: string;
var
  Path: array [0..260] of Char;
begin
  GetWindowsDirectory(Path, SizeOf(Path));
  Result := AppendSlash(Path);
end;

function AppendSlash(Input: string): string;
begin
  if Length(Input) > 0 then
    if Input[Length(Input)] = '\' then
      Result := Input
    else
      Result := Input + '\'
  else
    Result := Input;
end;

function ModifySlash(Input: string): string;
var
  i: Integer;
  k: String;
begin
  k := Input;
  for i := 1 to Length(k) do // GC September 18, 2001 - code <mslash>
    if (k[i] = '/') or (k[i] = '-') then //Mr April 4, 2002 - '-'
      k[i] := '\';
  Result := k;
end;

function ModifySlash(Input: string; Fromm, tto: Char): string;
var
  i: Integer;
  k: String;
begin
  k := Input;
  for i := 1 to Length(k) do // GC December 24, 2001 - code <mslash>
    if k[i] = Fromm then
      k[i] := tto;
  Result := k;
end;

function RemoveSlash(Input: string): string;
begin
  if Input <> '' then
    if Input[Length(Input)] = '\' then
      Result := Copy(Input, 0, Length(Input) - 1)
    else
      Result := Input;
end;

function RemoveFileExt(Input: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', Input);
  if (I > 0) and (Input[I] = '.') then
    Result := Copy(Input, 0, i - 1)
  else
    Result := Input;
end;

function RemoveDrive(Input: string): string;
var
  I: Integer;
begin
  I := Pos(':\', Input);
  if (I > 0) and (Input[I] = ':') then
    Result := Copy(Input, I + 2, Length(Input) - 3)
  else
    Result := Input;
end;

function ConvFNameToDos(ExtFileName: string): string;
var
  FindData: TWin32FindDataA;
  HSearch:  THANDLE;
begin
  HSearch := FindFirstFile(PChar(ExtFileName), FindData);
  if FindData.cAlternateFileName = '' then
    Result := FindData.cFileName
  else
    Result := FindData.cAlternateFileName;
  Windows.FindClose(HSearch);
end;

function SizeInK(Size: Int64): string;
var
  j: Real;
  k: String;
begin
  if Size = 0 then
    Result := '0 kb'
  else
   begin
    j := (Size / 1000);
    if j <= 999.99 then
      k := FormatFloat('##0.00', j)
    else
      k := FormatFloat('###,###,###,##0', j);
    Result := k + ' kb';
   end;
end;

function IsHarddrive(Drive: Char): Boolean;
begin
  Result := (GetDriveType(PChar(Drive + ':\')) = DRIVE_FIXED);
end;

function IsCDRom(Drive: Char): Boolean;
begin
  Result := (GetDriveType(PChar(Drive + ':\')) = DRIVE_CDROM);
end;

function IsFloppy(Drive: Char): Boolean;
begin
  Result := (GetDriveType(PChar(Drive + ':\')) = DRIVE_REMOVABLE);
end;

function IsLocked(FileName: string): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
    fs.Free;
  except
    Result := True;
   end;
end;

procedure Run(ProgramPath, ProgramParam: string);
var
  k: String;
begin
  if UpperCase(ExtractFileExt(ProgramPath)) = '.INF' then
   begin
    ExecInf(ProgramPath, k);
    Exit;
   end;
  if UpperCase(ExtractFileExt(ProgramPath)) = '.REG' then
   begin
    ExecReg(ProgramPath);
    Exit;
   end;

  ShellExecute(Application.Handle, 'open', PChar(ExtractFileName(ProgramPath)),
    PChar(ProgramParam),
    PChar(ExtractFilePath(ProgramPath)), SW_SHOWNORMAL);
end;

procedure RunAndwait(ProgramPath, ProgramParam: string);
var
  sei: SHELLEXECUTEINFO;
  FileToOpen, Param: array[0..255] of Char;
  k:   String;
  i:   Integer;
begin
  CancelWait   := False;
  TerminateRun := False;
  if UpperCase(ExtractFileExt(ProgramPath)) = '.INF' then
   begin
    ExecInf(ProgramPath, k);
    Exit;
   end;
  if UpperCase(ExtractFileExt(ProgramPath)) = '.REG' then
   begin
    ExecReg(ProgramPath);
    Exit;
   end;
  // Get the file to use
  StrPCopy(FileToOpen, ProgramPath);
  StrPCopy(Param, ProgramParam);
  // Run (exe), open (documents) or install (inf)
  // the file using ShellExecuteEx
  sei.cbSize := SizeOf(sei);
  sei.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_NOCLOSEPROCESS;
  sei.wnd    := Application.MainForm.handle;
  if (StrPos(FileToOpen, '.inf') <> NIL) then
    sei.lpVerb := 'Install'
  else
    sei.lpVerb := NIL;
  sei.lpFile := FileToOpen;
  if ProgramParam <> '' then
    sei.lpParameters := Param
  else
    sei.lpParameters := NIL;
  sei.lpDirectory := NIL;
  sei.nShow       := SW_SHOWDEFAULT;
  if (ShellExecuteEx(@sei) = True) then
   begin
    // Wait for it to terminate
    WaitForInputIdle(sei.hProcess, 1000);
    while (WaitForSingleObject(sei.hProcess, 10) = WAIT_TIMEOUT) and
      not CancelWait and not TerminateRun do
       begin
        // Keep watch for messages so that we
        // don't appear to "stop responding"
        Application.ProcessMessages();
      Sleep(500);
     end;
    i := 0;
    if TerminateRun then
      TerminateProcess(sei.hProcess, i);
    CloseHandle(sei.hProcess);
   end
  else
    MessageBox(Application.Mainform.Handle, 'Unable to run or open this file',
      PChar(Application.Mainform.Caption), mb_ok or mb_iconstop);
end;

procedure RunWWW(WWWPath: string);
begin
  ShellExecute(Application.Handle, 'open', PChar(WWWPath), '',
    '', SW_SHOWNORMAL);
end;

procedure ExecReg(var Path: string);
var
  k: String;
begin
  k := '/s /y ' + Path;
  Shellexecute(application.handle, 'open', 'Regedit.exe',
    PChar(k), PChar(GrabWindowPath), SW_NORMAL);
end;

function ExecInf(var Path, Param: string): Cardinal;
var
  osvi: TOSVersionInfo;
begin
  Result := 0;

  if Param = '.ntx86' then
    Param := Param + ' '
  else
    Param := '';

  osvi.dwOSVersionInfoSize := SizeOf(OSvi);
  if GetVersionEx(OSVI) then
   begin
    case osvi.dwPlatformID of
      VER_PLATFORM_WIN32_WINDOWS: Path :=
          'rundll.exe setupx.dll,InstallHinfSection DefaultInstall 132 ' + Path;
      VER_PLATFORM_WIN32_NT: Path      :=
          'rundll32.exe setupapi.dll,InstallHinfSection DefaultInstall 132 ' + Path;
     end;
    Result := WinExec(PChar(Path), SW_SHOW);
   end;
end;

procedure OpenFolder(Folder: String);
begin
  ShellExecute(Application.Handle, 'open', PChar(Folder), '', PChar(Folder),
    SW_SHOWNORMAL);
end;

procedure ExploreFolder(Folder: String);
begin
  ShellExecute(Application.Handle, 'explore', PChar(Folder), '', PChar(Folder),
    SW_SHOWNORMAL);
end;

procedure File_Set_Date(const hFile: THandle; const iAge: Integer);
var
  LocalFileTime, FileTime: TFileTime;
begin
  DosDateTimeToFileTime(HIWORD(iAge), LOWORD(iAge), LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, FileTime);
  SetFileTime(hFile, NIL, NIL, @FileTime);
end;

function Get_File_Size(const FileName: string): Int64;
var
  myFile:     THandle;
  myFindData: TWin32FindData;
begin
  Result := 0;
  myFile := FindFirstFile(PChar(FileName), myFindData);
  if myFile <> INVALID_HANDLE_VALUE then
   begin
    Windows.FindClose(myFile);
    Result := Int64(myFindData.nFileSizeHigh) shl Int64(32) +
      Int64(myFindData.nFileSizeLow);
   end;
end;

function CalcFolderSize(const aRootPath: string): Int64;
  procedure Traverse(const aFolder: string);
  var
    Data:       TWin32FindData;
    FileHandle: THandle;
   begin
    FileHandle := FindFirstFile(PChar(aFolder + '*'), Data);
    if FileHandle <> INVALID_HANDLE_VALUE then
      try
        repeat
          if (Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0) and
            (Data.cFileName[0] <> '.') then
            Traverse(aFolder + Data.cFilename + '\')
          else
            Inc(Result, (Data.nFileSizeHigh * MAXDWORD) +
              Data.nFileSizeLow);
        until not FindNextFile(FileHandle, Data);
      finally
        Windows.FindClose(FileHandle);
       end;
   end;
begin
  Result := 0;
  Traverse(aRootPath);
end;

procedure MakeDirectory(DirName: string);
begin
  if DirName <> '' then
    ForceDirectories(DirName);
end;

function PollFileList(MaskedName: string; SubDir: Boolean): TStrings;
var
  sr:       TSearchRec;
  aStrings: TStrings;
  k:        String;
begin
  aStrings := TStringList.Create();
  k        := AppendSlash(ExtractFilePath(MaskedName));

  if FindFirst(MaskedName, faAnyfile and faHidden, sr) = 0 then
   begin
    if (sr.Name <> '.') and (sr.Name <> '..') then
      if FileExists(k + sr.Name) then
        aStrings.Add(k + sr.Name);
    while FindNext(sr) = 0 do
        if (sr.Name <> '.') and (sr.Name <> '..') then
        if FileExists(k + sr.Name) then
          aStrings.Add(k + sr.Name);
   end;
  FindClose(sr);

  if SubDir then
    if Pos('*', MaskedName) <> 0 then
     begin
      if FindFirst(AppendSlash(ExtractFilePath(MaskedName)) + '*',
        faDirectory + faHidden, sr) = 0 then
       begin
        if (sr.Name <> '.') and (sr.Name <> '..') then
          if DirectoryExists(k + sr.Name) then
            aStrings.AddStrings(PollFileList(AppendSlash(k + sr.Name) +
              ExtractFileName(MaskedName), SubDir));

        while FindNext(sr) = 0 do
            if (sr.Name <> '.') and (sr.Name <> '..') then
            if DirectoryExists(k + sr.Name) then
              aStrings.AddStrings(PollFileList(AppendSlash(k + sr.Name) +
                ExtractFileName(MaskedName), SubDir));
       end;
      FindClose(sr);
     end;

  Result := aStrings;
end;

function SubDirList(Dir: string): TStrings;
var
  sr:        TSearchRec;
  FileAttrs: Integer;
  aStrings:  TStrings;
  k:         String;
begin
  aStrings  := TStringList.Create;
  FileAttrs := 0;
  FileAttrs := FileAttrs + faDirectory;
  k         := AppendSlash(Dir);
  if FindFirst(k + '*', FileAttrs, sr) = 0 then
   begin
    if DirectoryExists(k + sr.Name) then
      if (sr.Name <> '.') and (sr.Name <> '..') then
        aStrings.Add(AppendSlash(k + sr.Name));
    while (FindNext(sr) = 0) do
        if DirectoryExists(k + sr.Name) then
        if (sr.Name <> '.') and (sr.Name <> '..') then
          aStrings.Add(AppendSlash(k + sr.Name));
    FindClose(sr);
   end;
  Result := aStrings;
end;

function DeleteAllFiles(FilesOrDir: string): Boolean;
  { Sends files or directory to the recycle bin. }
var
  F:         TSHFileOpStruct;
  From:      String;
  ResultVal: Integer;
begin
  Result := False;
  if Length(filesordir) <= 3 then Exit;// (delete root?)
  FillChar(F, SizeOf(F), #0);
  From          := FilesOrDir + #0;
  Screen.Cursor := crHourGlass;
  try
    F.wnd   := 0;
    F.wFunc := FO_DELETE;
    F.pFrom := PChar(From);
    F.pTo   := NIL;

    F.fFlags := FOF_ALLOWUNDO or
      FOF_NOCONFIRMATION or
      FOF_SIMPLEPROGRESS or
      FOF_FILESONLY;

    F.fAnyOperationsAborted := False;
    F.hNameMappings := NIL;
    ResultVal := ShFileOperation(F);
    Result    := (ResultVal = 0);
  finally
    Screen.Cursor := crDefault;
   end;
end;

procedure DeleteDir(aDir: string);
  { delete directory & everything in it }
var
  T: TSHFileOpStruct;  {here is a compiler error, tshfileopstruct not
found}
begin
  FillChar(T, SizeOf(T), #0);
  aDir := aDir + #0;
  with T do
   begin
    Wnd    := 0; // no handle -> no animation
    wFunc  := FO_DELETE;
    pFrom  := PChar(aDir);
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION; // just do it
   end;
  if (SHFileOperation(T) <> 0) then
    MessageDlg('Error deleting directory!', mtError, [mbOk], 0);
end; {DeleteDir}

function CreateShortcut(LinkFileName, FilePath: string): Boolean;
var
  k: String;
begin
  k := FilePath;
  if Links.CreateLink(k,
    LinkFileName,
    ExtractFileName(k)) = True then
    Result := True
  else
    Result := False;
end;

//www.SwissDelphiCenter.ch - create a shortcut in the Startmenu, on the Desktop
//Author: Tom (tom@swissdelphicenter.ch) - Added by GC July31, 2001
procedure CreateShortcut(SourceFileName: String; Location: ShortcutType;
  SubDirectory: String);
var
  MyObject:       IUnknown;
  MySLink:        IShellLink;
  MyPFile:        IPersistFile;
  Directory,
  LinkName:       String;
  WFileName:      WideString;
  MyReg,
  QuickLaunchReg: TRegIniFile;
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink  := MyObject as IShellLink;
  MyPFile  := MyObject as IPersistFile;

  MySLink.SetPath(PChar(SourceFileName));
  MySLink.SetDescription(PChar(ExtractFileName(SourceFileName)));

  MyReg := TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
  try
    LinkName := SourceFileName + '.lnk';
    LinkName := ExtractFileName(LinkName);
    case Location of
      stOTHERFOLDER:
        Directory := SubDirectory;
      stDESKTOP:
        Directory := MyReg.ReadString('Shell Folders', 'Desktop', '');
      stSTARTMENU:
        Directory := MyReg.ReadString('Shell Folders', 'Start Menu', '');
      stPROGRAMS:
        Directory := MyReg.ReadString('Shell Folders', 'Programs', '');
      stSENDTO:
        Directory := MyReg.ReadString('Shell Folders', 'SendTo', '');
      stQUICKLAUNCH:
       begin
        QuickLaunchReg := TRegIniFile.Create(
          'Software\MicroSoft\Windows\CurrentVersion\GrpConv');
        try
          Directory := QuickLaunchReg.ReadString('MapGroups', 'Quick Launch', '');
        finally
          QuickLaunchReg.Free;
         end;
       end;
     end;
    if Directory <> '' then
     begin
      if (SubDirectory <> '') and (Location <> stOTHERFOLDER) then
       begin
        if not DirectoryExists(AppendSlash(Directory) + AppendSlash(SubDirectory)) then
          ForceDirectories(AppendSlash(Directory) + AppendSlash(SubDirectory));
        WFileName := AppendSlash(Directory) + AppendSlash(SubDirectory) + LinkName;
       end
      else
       begin
        if not DirectoryExists(AppendSlash(Directory)) then
          ForceDirectories(AppendSlash(Directory));
        WFileName := AppendSlash(Directory) + LinkName;
       end;
      MyPFile.Save(PWChar(WFileName), False);
     end;
  finally
    MyReg.Free;
   end;
end;

function DiskMakeImage(Drive: Integer; FileName: string): Boolean;
var
  F:     TMemoryStream;
  FBuf:  Pointer;
  nSize: Integer;
  FSBR:  PFSBR;
begin
  Result := False;
  F      := TMemoryStream.Create;
  FBuf   := AllocMem(512);
  try
    if ExtractFileName(FileName) <> '' then
      if ReadFloppyFSBR(Drive, FSBR) then
        if 1474560 = FSBR.BPB.BytesPerSector * FSBR.BPB.SectorsOnDrive then
         begin
          nsize := FSBR.BPB.BytesPerSector * FSBR.BPB.SectorsOnDrive;
          F.SetSize(nsize);
          F.Seek(0, 0);
          FreeMem(FBuf);
          FBuf := AllocMem(nSize);
          if not ReadSector(Drive, 0, FSBR.BPB.SectorsOnDrive, FBuf) then
            MessageDlg('Error reading sector', mtError, [mbOk], 0);
          F.Seek(0, 0);
          F.Write(FBuf^, nSize);
          F.Seek(0, 0);
          F.SaveToFile(FileName);
          MessageDlg('Created ' + FileName, mtInformation, [mbOk], 0);
          Result := True;
         end;
  finally
    F.Free;
    FreeMem(FBuf);
   end;
end;

function DiskWriteImage(Drive: Integer; FileName: string): Boolean;
var
  F:     TMemoryStream;
  FBuf:  Pointer;
  nSize: Integer;
  FSBR:  PFSBR;
begin
  Result := False;
  if not ReadFloppyFSBR(Drive, FSBR) then
   begin
    MessageDlg('Floppy not ready!', mtError, [mbOk], 0);
    Exit;
   end;

  if not DriveIsRemovable(Drive) then
   begin
    MessageDlg('Not a Floppy', mtError, [mbOk], 0);
    Exit;
   end;

  if not DirectAccessAllowed(Drive) then
   begin
    MessageDlg('Not accessable', mtError, [mbOk], 0);
    Exit;
   end;
  nsize := FSBR.BPB.BytesPerSector * FSBR.BPB.SectorsOnDrive;
  if 1474560 = nsize then
   begin
    F    := TMemoryStream.Create;
    FBuf := AllocMem(512);
    try
      F.SetSize(nSize);
      F.Seek(0, 0);
      FreeMem(FBuf);
      FBuf := AllocMem(nSize);
      F.LoadFromFile(FileName);
      F.Seek(0, 0);
      F.Read(FBuf^, nSize);
      F.Seek(0, 0);

      if not WriteSector(Drive, 0, FSBR.BPB.SectorsOnDrive, FBuf, $0000) then
        MessageDlg('Error writing sectors', mtError, [mbOk], 0);

      FreeFloppyFSBR(FSBR);
      MessageDlg('Restored ' + FileName, mtInformation, [mbOk], 0);
      Result := True;
    finally
      F.Free;
      FreeMem(FBuf);
     end;
   end;
end;

procedure DiskUnSpan(FileName: string);
var
  tf, sf:  file;
  buf:     array[1..500] of Byte;
  textf:   TStrings;
  numread: Longint;
  i:       Integer;
begin
  textf := TStringList.Create;
  textf.LoadFromFile(FileName);
  AssignFile(tf, textf.Strings[0]);
  Rewrite(tf, 1);
  for i := 1 to textf.Count - 1 do
   begin
    AssignFile(sf, textf.Strings[i]);
    Reset(sf, 1);
    numread := 1;
    while numread > 0 do
     begin
      BlockRead(sf, buf, SizeOf(buf), numread);
      BlockWrite(tf, buf, numread);
     end;
    CloseFile(sf);
   end;
  CloseFile(tf);
  textf.Free;
end;

function DiskSpan(Source, Target: string; DiskSize: Longint;
  MakeBatch: Boolean): Integer;
const
  Break   = #13#10;
  batadd1 = '@echo off' + Break +
    'set lbl=a' + Break +
    'goto logo' + Break +
    ':a' + Break +
    'if "%1"=="/auto" goto b' + Break +
    'choice /C:yn /N /T:Y,3 Reconstruct archive [will default to Yes in 3 secs]?' +
    Break +
    'echo.' + Break +
    'if errorlevel 2 goto end' + Break +
    ':b' + Break +
    'set lbl=c' + Break +
    'goto logo' + Break +
    ':c' + Break +
    'echo Reconstructing archive, please wait.....';
  batadd2 = 'Echo                                         ....done' + Break +
    'goto end' + Break +
    ':logo' + Break +
    'cls' + Break +
    'Echo ' + PRODUCT + ' UnSpanner' + Break +
    'Echo.' + Break +
    'Echo Copyright (c) Joseph Leung, 1999-2001' + Break +
    'echo.' + Break +
    'goto %lbl%' + Break +
    ':end' + Break +
    'echo.' + Break +
    'echo Press any key to exit...' + Break +
    'if not "%1"=="/auto" pause > nul' + Break +
    'cls';
var
  tf, sf:  file;
  textf:   textfile;
  fsize, remainsize: Longint;
  buf:     array[1..500] of Byte;
  numread: Longint;
  disk:    Integer;
  k, l:    String;
  i:       Integer;
begin
  AssignFile(sf, Source);
  Reset(sf, 1);
  fsize := FileSize(sf);
  Seek(sF, 0);
  disk := 0;
  while fsize > 0 do
   begin
    Inc(disk);
    AssignFile(tf, Target + '.' + IntToStr(disk));
    Rewrite(tf, 1);
    remainsize := DiskSize;
    numread    := -1;
    while (remainsize >= 0) and (numread <> 0) do
     begin
      BlockRead(sf, buf, SizeOf(buf), numread);
      Dec(Remainsize, numread);
      if numread > 0 then
        BlockWrite(tf, Buf, numread);
     end;
    if IsFloppy(Source[1]) then
      Writeln('Please insert another floppy disk');

    CloseFile(tf);
    Dec(fsize, DiskSize);
   end;
  CloseFile(sf);
  k := ExtractFileName(Target);
  l := ExtractFileName(Source);

  AssignFile(textf, Target + '.x');
  Rewrite(textf);
  Writeln(textf, l);
  for i := 1 to disk do
    Write(textf, k + '.' + IntToStr(i));
  CloseFile(textf);

  if MakeBatch then
   begin
    AssignFile(textf, Target + '.bat');
    Rewrite(textf);
    Writeln(textf, batadd1);

    Write(textf, 'Copy /b ');
    Write(textf, k + '.1');
    for i := 2 to disk do
      Write(textf, '+' + k + '.' + IntToStr(i));
    Writeln(textf, ' ' + l + ' >nul');

    Writeln(textf, batadd2);
    CloseFile(textf);
   end;
  Result := disk;
end;

procedure Combine(FirstFile, SecondFile, ThirdFile, TargetFile: string);
var
  tf, sf:  file;
  buf:     array[1..500] of Byte;
  numread: Longint;
  i:       Integer;
begin
  AssignFile(tf, TargetFile);
  Rewrite(tf, 1);

  if FirstFile <> '' then
   begin
    AssignFile(sf, FirstFile);
    Reset(sf, 1);
    numread := 1;
    while numread > 0 do
     begin
      BlockRead(sf, buf, SizeOf(buf), numread);
      BlockWrite(tf, buf, numread);
     end;
    CloseFile(sf);
   end;

  if SecondFile <> '' then
   begin
    AssignFile(sf, SecondFile);
    Reset(sf, 1);
    numread := 1;
    while numread > 0 do
     begin
      BlockRead(sf, buf, SizeOf(buf), numread);
      BlockWrite(tf, buf, numread);
     end;
    CloseFile(sf);
   end;

  if ThirdFile <> '' then
   begin
    AssignFile(sf, ThirdFile);
    Reset(sf, 1);
    numread := 1;
    while numread > 0 do
     begin
      BlockRead(sf, buf, SizeOf(buf), numread);
      BlockWrite(tf, buf, numread);
     end;
    CloseFile(sf);
   end;

  CloseFile(tf);
end;

function GetValInIni(FileName: string; Section: string; Key: string;
  default: string): string;
var
  Ini: TInifile;
begin
  Ini := TIniFile.Create(FileName);
  try
    with Ini do
      Result := ReadString(Section, Key, '');
  finally
    Ini.Free;
   end;
  if Result = '' then Result := default;
end;

procedure SetValInIni(FileName: string; Section: string; Key, Value: string);
var
  Ini: TInifile;
begin
  Ini := TIniFile.Create(FileName);
  try
    with Ini do
      WriteString(Section, Key, Value);
  finally
    Ini.Free;
   end;
end;

function GetValInReg(RKey: HKey; KeyPath: string;
  Valname: string): string;
begin
  with TRegistry.Create do
    try
      Access  := KEY_READ;
      RootKey := RKey;
      OpenKey(KeyPath, True);
      Result := ReadString(ValName);
    finally
      Free;
     end;
end;

procedure SetValInReg(RKey: HKey; KeyPath: string;
  ValName: string; NewVal: string);
begin
  with TRegistry.Create do
    try
      RootKey := RKey;
      OpenKey(KeyPath, True);
      WriteString(ValName, NewVal);
    finally
      Free;
     end;
end;

procedure DelValInReg(RKey: HKey; KeyPath: string; Key: string);
begin
  with TRegistry.Create do
    try
      RootKey := RKey;
      OpenKey(KeyPath, True);
      if ValueExists(Key) then
        DeleteValue(Key);
    finally
      Free;
     end;
end;

procedure DelKeyInReg(RKey: HKey; KeyPath: string);
var
  valstrings, subkeystrings: TStrings;
  i: Integer;
begin
  if keypath = '' then Exit;
  valstrings    := RegListVal(Rkey, Keypath);
  subkeystrings := RegListsubkey(RKey, Keypath);
  for i := 0 to subkeystrings.Count - 1 do
    DelKeyInReg(RKey, Keypath + subkeystrings.Strings[i]);
  for i := 0 to valstrings.Count - 1 do
    DelValInReg(RKey, Keypath, valstrings.Strings[i]);
  subkeystrings.Free;
  valstrings.Free;
  RegDeleteKey(Rkey, PChar(keypath));
end;

function MakeRegnode(RootKey: HKEY; Path: AnsiString): TList;
var
  aList: TList;
  anode, asubnode: PRegnodeType;
  KeyList, SubKeyList: TStrings;
  i: Integer;
begin
  aList := TList.Create;
  aList.Clear;

  KeyList    := RegListVal(RootKey, Path);
  SubKeyList := RegListsubkey(RootKey, Path);

  for i := 0 to KeyList.Count - 1 do
   begin
    New(anode);
    anode^.IsKey    := True;
    anode^.SubKey   := TList.Create;
    //anode^.valuetype :=Reg.GetDataType(keylist.strings[i]);
    anode^.FullPath := Path + '\' + KeyList.Strings[i];
    anode^.KeyName  := KeyList.Strings[i];

    aList.Add(anode);
    {anode^.dataS := '';
    anode^.dataES := '';
    anode^.dataI := 0;
    anode^.dataB := 0;
    Case anode^.valuetype of
    rdString : anode^.dataS := Reg.ReadString(keylist.strings[i]);
    rdExpandString : anode^.dataES := Reg.ReadString(keylist.strings[i]);
    rdInteger : anode^.dataI := Reg.ReadInteger(keylist.strings[i]);
    rdBinary : anode^.dataB := 0//Reg.ReadBinaryData(keylist.strings[i],j,2147483647);

    end;}
   end;


  for i := 0 to SubKeyList.Count - 1 do
   begin
    New(asubnode);
    asubnode^.IsKey    := False;
    asubnode^.FullPath := Path + '\' + SubKeyList.Strings[i];
    asubnode^.KeyName  := SubKeyList.Strings[i];
    asubnode^.SubKey   := TList.Create;
    asubnode^.SubKey   := MakeRegnode(RootKey, asubnode^.FullPath);
    aList.Add(asubnode);
   end;

  KeyList.Free;
  SubKeyList.Free;
  Result := aList;
end;

procedure CleanRegnode(aList: TList);
var
  i:     Integer;
  anode: PRegnodeType;
begin
  for i := aList.Count - 1 downto 0 do
   begin
    anode := aList.Items[i];
    CleanRegnode(anode^.SubKey);
    Dispose(anode);
   end;
end;

procedure AddRegnode(RootKey: Hkey; aList: TList; var aString: TStrings;
  Key, SubKey: string);
var
  i:     Integer;
  anode: PRegnodeType;
begin
  aString := TStringList.Create;
  for i := aList.Count - 1 downto 0 do
   begin
    anode := aList.Items[i];
    if not anode^.IsKey then
      aString.Add(SubKey + RKeyName(RootKey) + anode^.FullPath)
    else
      aString.Add(Key + RKeyName(RootKey) + anode^.FullPath);
    if not anode^.IsKey then
      AddRegnode(RootKey, anode^.SubKey, aString, Key, SubKey);
   end;
end;

procedure CompareRegnode(RootKey: HKEY; List1, List2: TList;
  var aString: TStrings; Key, SubKey: string);
var
  i, j:         Integer;
  node1, node2: PRegnodeType;
  bstring:      TStrings;
begin
  bstring := TStringList.Create;
  for i := 0 to List2.Count - 1 do
   begin
    node2 := List2.items[i];
    if node2^.IsKey then
     begin
      j := 0;
      if List1.Count > 0 then
       begin
        node1 := List1.Items[j];
        while ((not node1^.IsKey) or
          (node1^.FullPath <> node2^.FullPath)) and (j < List1.Count) do
         begin
          node1 := List1.Items[j];
          Inc(j);
         end;
        if (node1^.FullPath <> node2^.FullPath) then
          aString.Add(Key + RKeyName(RootKey) +
            node2^.FullPath);
       end
      else if List2.Count > 0 then aString.Add(Key + RKeyName(RootKey) + node2^.FullPath)
     end
    else

     begin
      j := 0;
      if List1.Count > 0 then
       begin
        node1 := List1.Items[j];
        while ((node1^.IsKey) or (node1^.FullPath <> node2^.FullPath)) and
          (j < List1.Count) do
         begin
          node1 := List1.Items[j];
          Inc(j);
         end;
        if (node1^.FullPath = node2^.FullPath) then
          CompareRegnode(RootKey, node1^.SubKey,
            node2^.SubKey, aString, Key, SubKey)
        else
         begin
          aString.Add(SubKey + RKeyName(RootKey) + node2^.FullPath);
          AddRegnode(RootKey, node2^.SubKey, bstring, Key, SubKey);
          aString.AddStrings(bstring);
         end;
       end
      else if List2.Count > 0 then aString.Add(SubKey + RKeyName(RootKey) +
          node2^.FullPath);
     end;
   end;
  bstring.Free;
end;

function RKeyName(RootKey: HKEY): string;
begin
  case RootKey of
    HKEY_CLASSES_ROOT: Result   := 'HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER: Result   := 'HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE: Result  := 'HKEY_LOCAL_MACHINE';
    HKEY_USERS: Result          := 'HKEY_USERS';
    HKEY_CURRENT_CONFIG: Result := 'HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA: Result       := 'HKEY_DYN_DATA';
    else
      Result := '??';
   end;
end;

function Name2RKey(Key: string): HKey;
var
  k: String;
begin
  k      := UpperCase(Key);
  Result := HKEY_CLASSES_ROOT;
  if k = 'HKCR' then
    Result := HKEY_CLASSES_ROOT
  else if k = 'HKCU' then
    Result := HKEY_CURRENT_USER
  else if k = 'HKLL' then
    Result := HKEY_LOCAL_MACHINE
  else if k = 'HKU' then
    Result := HKEY_USERS
  else if k = 'HKCC' then
    Result := HKEY_CURRENT_CONFIG
  else if k = 'HKDD' then
    Result := HKEY_DYN_DATA;
end;

function RegListSubKey(RKey: HKey; KeyPath: string): TStrings;
var
  KeyList: TStrings;
  Reg:     TRegistry;
  k:       String;
begin
  Reg     := TRegistry.Create;
  KeyList := TStringList.Create;

  Reg.RootKey := RKEY;
  k           := keypath;
  if k = '' then k := '\';

  if Reg.OpenKey(K, False) then
    Reg.GetKeyNames(KeyList);
  Reg.CloseKey;
  Reg.Free;
  Result := KeyList;
end;

function RegListVal(RKey: HKey; KeyPath: string): TStrings;
var
  KeyList: TStrings;
  Reg:     TRegistry;
  k:       String;
begin
  Reg     := TRegistry.Create;
  KeyList := TStringList.Create;

  Reg.RootKey := RKEY;
  k           := keypath;
  if k = '' then k := '\';

  if Reg.OpenKey(K, False) then
    Reg.GetValueNames(KeyList);
  Reg.CloseKey;
  Reg.Free;
  Result := KeyList;
end;

procedure RegBackup(RKey: HKey; KeyPath, Value: string; FileName: string);
var
  ValList:    TStrings;
  SubKeyList: TStrings;
  tf:         textfile;
  i:          Integer;
begin
  if Value = '' then
   begin
    ValList    := RegListVal(RKey, Keypath);
    SubKeyList := RegListsubkey(RKey, Keypath);
    for i := 0 to ValList.Count - 1 do
      RegBackup(RKey, Keypath, ValList.Strings[i], FileName);
    for i := 0 to SubKeyList.Count - 1 do
      RegBackup(RKey, Keypath + '\' + SubKeyList.Strings[i], '', FileName);
   end
  else if GetValInReg(RKey, Keypath, Value) <> '' then
   begin
    AssignFile(tf, FileName);
    if FileExists(FileName) then
      Append(tf)
    else
     begin
      Rewrite(tf);
      Writeln(tf, 'REGEDIT4');
      Writeln(tf);
     end;

    Writeln(tf, '[' + RKeyName(rkey) + '\' + keypath + ']');
    Write(tf, '"' + Value + '"=');
    Writeln(tf, '"' + GetValInReg(RKey, Keypath, Value) + '"');
    Writeln(tf);

    CloseFile(tf);
   end;
end;

procedure AssociateProgram(Ext, Path, Icon: string);
begin
  { ALL extensions must be in lowercase to avoid trouble! }
  Ext := LowerCase(Ext);
  if FileExists(Path) then
   begin
    SetValInReg(HKEY_CLASSES_ROOT,
      '.' + Ext, { extension we want to define }
      '', { specify the default data item }
      LeadChar + Ext); { This is the value of the default data item -
                                     this referances our new type to be defined  }
    SetValInReg(HKEY_CLASSES_ROOT,
      LeadChar + Ext, { this is the type we want to define }
      '', { specify the default data item }
      Ext + ' Archive'); { This is the value of the default data item -
                              this is the English description of the file type }
    Ext := UpperCase(Ext);
    SetValInReg(HKEY_CLASSES_ROOT,
      LeadChar + Ext + '\DefaultIcon', { Create a file...DefaultIcon.}
      '', { Specify the default data item.}
      Icon + ',0'); { Executable where icon is in and it's Sequence number.}

    SetValInReg(HKEY_CLASSES_ROOT,
      LeadChar + Ext + '\shell\open\command', { create a file...open key }
      '', { specify the default data item }
      Path + ' "%1"'); { command line to open file with }
   end;
end;

procedure UnAssociateProgram(Ext: string);
begin
  Ext := LowerCase(Ext);
  DelKeyInReg(HKEY_CLASSES_ROOT,
    '.' + Ext);     { extension we want to undefine }
  DelKeyInReg(HKEY_CLASSES_ROOT,
    LeadChar + Ext + '\DefaultIcon');
  DelKeyInReg(HKEY_CLASSES_ROOT,
    LeadChar + Ext + '\shell\open\command');
  DelKeyInReg(HKEY_CLASSES_ROOT,
    LeadChar + Ext);
  DelKeyInReg(HKEY_CLASSES_ROOT,
    LeadChar + Ext);
end;

function GetAssociatedProgram(Ext: string): string;
begin
  Ext    := LowerCase(Ext);
  Result := GetValInReg(HKEY_CLASSES_ROOT, '.' + Ext, '');
end;

procedure PlainDialog;
begin
  aForm         := TForm.Create(NIL);
  aCheckbox     := TCheckbox.Create(aForm);
  aCheckbox.Parent := aForm;
  aLabel        := TStatictext.Create(aForm);
  aLabel.Parent := aForm;

  aLabel.AutoSize   := False;
  aCheckbox.Checked := False;

  aForm.Width       := 286;
  aForm.Height      := 240;
  aForm.Position    := poDesktopCenter;
  aForm.BorderStyle := bsDialog;

  aLabel.Left      := 10;
  aLabel.Top       := 30;
  aLabel.Width     := aForm.Width - (aLabel.Left * 2);
  aLabel.Alignment := taCenter;
  aLabel.Height    := 60;

  aCheckbox.Width := 180;

  aCheckbox.Checked := True;
  aCheckbox.Caption := MSG_SHOWAGAIN;

  aCheckbox.Top  := 120;
  aCheckbox.Left := (aForm.Width - aCheckbox.Width) div 2;
end;

procedure FreePlainDialog;
begin
  aCheckbox.Free;
  aLabel.Free;
  aForm.Free;
end;

function ShowAgainDialog(DCaption, Msg: string): Boolean;
var
  aButton: TButton;
begin
  Result := True;
  PlainDialog;
  aButton         := TButton.Create(aForm);
  aButton.Parent  := aForm;
  aButton.ModalResult := 1;
  aButton.Default := True;

  try
    aForm.Caption  := DCaption;
    aLabel.Caption := Msg;
    aButton.Top    := 160;
    aButton.Left   := (aForm.Width - aButton.Width) div 2;

    aButton.Width   := 75;
    aButton.Caption := 'Close';
    aForm.Showmodal;

    if not aCheckbox.Checked then
      Result := False;
  finally

    abutton.Free;
    FreePlainDialog;
   end;
end;

procedure RegAskShowAgainDialog(DCaption, Msg: string; Path, Key: string);
begin
  if GetValInReg(HKEY_CLASSES_ROOT, Path, Key) <> 'FALSE' then
    if ShowAgainDialog(DCaption, Msg) then
      SetValInReg(HKEY_CLASSES_ROOT, Path, Key, 'TRUE')
    else
      SetValInReg(HKEY_CLASSES_ROOT, Path, Key, 'FALSE')
end;

procedure IniAskShowAgainDialog(DCaption, Msg: string; FileName,
  Section, Key: string);
begin
  if GetValInIni(FileName, Section, Key, 'TRUE') <> 'FALSE' then
    if ShowAgainDialog(DCaption, Msg) then
      SetValInIni(FileName, Section, Key, 'TRUE')
    else
      SetValInIni(FileName, Section, Key, 'FALSE')
end;

function YesNoShowAgainDialog(DCaption, Msg: string;
  var YesNo: Boolean): Boolean;
var
  yButton, nButton: TButton;
begin
  Result := True;
  PlainDialog;
  yButton         := TButton.Create(aForm);
  yButton.Parent  := aForm;
  yButton.ModalResult := 1;
  yButton.Default := True;
  nButton         := TButton.Create(aForm);
  nButton.Parent  := aForm;
  nButton.ModalResult := 2;
  nButton.Cancel  := True;
  try
    aForm.Caption   := DCaption;
    aLabel.Caption  := Msg;
    yButton.Top     := 160;
    nButton.Top     := 160;
    yButton.Width   := 75;
    yButton.Caption := 'Yes';
    nButton.Width   := 75;
    nButton.Caption := 'No';
    yButton.Left    := (aForm.Width - yButton.Width) div 2 - 75;
    nButton.Left    := (aForm.Width - nButton.Width) div 2 + 75;

    aForm.Showmodal;

    if aForm.ModalResult = 1 then
      YesNo := True
    else
      YesNo := False;
    if not aCheckbox.Checked then
      Result := False;
  finally
    ybutton.Free;
    nbutton.Free;
    FreePlainDialog;
   end;
end;

procedure RegYesNoAskShowAgainDialog(DCaption, Msg: string;
  Path, Section, Key: string; var YesNo: Boolean);
begin
  if GetValInReg(HKEY_CLASSES_ROOT, Path, Key) <> 'FALSE' then
    if YesNoShowAgainDialog(DCaption, Msg, YesNo) then
      SetValInReg(HKEY_CLASSES_ROOT, Path, Key, 'TRUE')
    else
      SetValInReg(HKEY_CLASSES_ROOT, Path, Key, 'FALSE')
end;

procedure IniYesNoAskShowAgainDialog(DCaption, Msg: string;
  FileName, Product, Section, Key: string; var YesNo: Boolean);
begin
  if GetValInIni(FileName, Product, Key, 'TRUE') <> 'FALSE' then
    if YesNoShowAgainDialog(DCaption, Msg, YesNo) then
      SetValInIni(FileName, Section, Key, 'TRUE')
    else
      SetValInIni(FileName, Section, Key, 'FALSE')
end;

function TColor2WebColor(aColor: TColor): string;
begin
  Result := Format('%s%s%s',
    [IntToHex(GetRValue(aColor), 2),
    IntToHex(GetGValue(aColor), 2),
    IntToHex(GetBValue(aColor), 2)]);
end;

procedure SendMail(Subject, Mailtext,
  FromName, FromAdress,
  ToName, ToAdress,
  AttachedFileName,
  DisplayFileName: string;
  ShowDialog: Boolean);
var
  MapiMessage: TMapiMessage;
  MError:      Cardinal;
  Empfaenger:  array[0..1] of TMapiRecipDesc;
  Absender:    TMapiRecipDesc;
  Datei:       array[0..1] of TMapiFileDesc;
begin
  with MapiMessage do
   begin
    ulReserved        := 0;
    lpszSubject       := PChar(Subject);
    lpszNoteText      := PChar(Mailtext);
    lpszMessageType   := NIL;
    lpszDateReceived  := NIL;
    lpszConversationID := NIL;
    flFlags           := 0;
    Absender.ulReserved := 0;
    Absender.ulRecipClass := MAPI_ORIG;
    Absender.lpszName := PChar(FromName);
    Absender.lpszAddress := PChar(FromAdress);
    Absender.ulEIDSize := 0;
    Absender.lpEntryID := NIL;
    lpOriginator      := @Absender;
    nRecipCount       := 1;
    Empfaenger[0].ulReserved := 0;
    Empfaenger[0].ulRecipClass := MAPI_TO;
    Empfaenger[0].lpszName := PChar(ToName);
    Empfaenger[0].lpszAddress := PChar(ToAdress);
    Empfaenger[0].ulEIDSize := 0;
    Empfaenger[0].lpEntryID := NIL;
    lpRecips          := @Empfaenger;
    nFileCount        := 1;
    Datei[0].lpszPathName := PChar(AttachedFilename);
    Datei[0].lpszFileName := PChar(DisplayFilename);
    Datei[0].ulReserved := 0;
    Datei[0].flFlags  := 0;
    Datei[0].nPosition := Cardinal(-1);
    Datei[0].lpFileType := NIL;
    lpFiles           := @Datei;
   end;
  // Senden
  if ShowDialog then
    MError := MapiSendMail(0, application.Handle, MapiMessage,
      MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0)
  else
    // Wenn kein Dialogfeld angezeigt werden soll:
    MError := MapiSendMail(0, Application.Handle, MapiMessage, 0, 0);
  case MError of
    //MAPI_E_AMBIGUOUS_RECIPIENT:
    // MessageDlg('Empfänger nicht eindeutig. (Nur möglich, wenn Emailadresse nicht angegeben.)',mterror,[mbok],0);
    MAPI_E_ATTACHMENT_NOT_FOUND:
      MessageDlg('Cannot find the attachment', mtError, [mbOK], 0);
    MAPI_E_ATTACHMENT_OPEN_FAILURE:
      MessageDlg('Cant open the attachment.', mtError, [mbOK], 0);
    MAPI_E_BAD_RECIPTYPE:
      MessageDlg('BAD MAPI_TO, MAPI_CC or MAPI_BCC.', mtError, [mbOK], 0);
    MAPI_E_FAILURE:
      MessageDlg('Unknown error.', mtError, [mbOK], 0);
    MAPI_E_INSUFFICIENT_MEMORY:
      MessageDlg('Not enough memory.', mtError, [mbOK], 0);
    MAPI_E_LOGIN_FAILURE:
      MessageDlg('Unable to login.', mtError, [mbOK], 0);
    MAPI_E_TEXT_TOO_LARGE:
      MessageDlg('Text too large', mtError, [mbOK], 0);
    MAPI_E_TOO_MANY_FILES:
      MessageDlg('Too many files.', mtError, [mbOK], 0);
    MAPI_E_TOO_MANY_RECIPIENTS:
      MessageDlg('Too many recipients.', mtError, [mbOK], 0);
    MAPI_E_UNKNOWN_RECIPIENT: MessageDlg('Unknown receipients', mtError, [mbOK], 0);
    MAPI_E_USER_ABORT:
      MessageDlg('User Abort!', mtError, [mbOK], 0);
    SUCCESS_SUCCESS:
     begin
     end;
   end;
end;

end.
