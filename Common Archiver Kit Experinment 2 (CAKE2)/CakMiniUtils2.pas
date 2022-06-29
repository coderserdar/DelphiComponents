unit CakMiniUtils2;

// ## GC function Get_fileSize(FileName: String): Integer removed
// ## GC procedure OpenFolder(Folder: String); added
// ## GC procedure ExploreFolder(Folder: String); modified
// ## procedure FileSetDate from Zipsfx code

interface

uses
  CakDefs2, Windows, SysUtils, Classes, Shellapi, StrUtils, ShlObj, Links;

var
  CancelWait, TerminateRun: Boolean;


//Path Grabbing
function GrabCurrentPath: string;
function GrabDesktopPath: string;
function GrabMydocuPath: string;
function GrabProgramPath: string;
function GrabSystemPath: string;
function GrabTempPath: string;
function GrabWindowPath: string;
function NewTempPath: string;
function FileListPath : string;

//Slash Functions
function AppendSlash(Input: string): string;
function ModifySlash(Input: string): string; overload;
function ModifySlash(Input: string; Fromm, tto: Char): string; overload;
function RemoveSlash(Input: string): string;
function RemoveFrontSlash(Input: string): string;
function RemoveFileExt(Input: string): string;
function RemoveDrive(Input: string): string;

//Files and Directories
function ConvFNameToDos(ExtFileName: string): string;
function SizeInK(Size: Int64): string;
function KtoSize(Value : string) : integer;
function FormatSize(Size : Int64) : string;
function IsHarddrive(Drive: Char): Boolean;
function IsCDRom(Drive: Char): Boolean;
function IsFloppy(Drive: Char): Boolean;
function IsLocked(FileName: string): Boolean;

procedure RunWWW(WWWPath: string);
procedure ExecReg(var Path: string);
function ExecInf(var Path, Param: string): Cardinal;
procedure OpenFolder(Folder: String);                    
procedure ExploreFolder(Folder: String);
procedure File_Set_Date(const hFile: THandle; const iAge: Integer); // ## GC
function Get_File_Size(const FileName: String): Int64;
function Get_File_DateTime(const FileName: String): TDateTime;
function CalcFolderSize(const aRootPath: string): Int64;
procedure MakeDirectory(DirName: string);
function PollFileList(MaskedName: string; SubDir: Boolean): TStrings;
function SubDirList(Dir: string): TStrings;
procedure DeleteDir(aDir: string);
function ExtractRootPath(aPath : string) : string;
function CreateShortcut(LinkFileName, FilePath: string): Boolean;overload;
procedure DiskUnSpan(FileName: string);
function DiskSpan(Source, Target: string; DiskSize: Longint;
  MakeBatch: Boolean): Integer;
procedure Combine(FirstFile, SecondFile, ThirdFile, TargetFile: string);

//Misc
function GetFreeSpace(Drive: char): Int64;

function replace(source : string; fromm, too : string) : string;
function DosDateTimeToDateTime(wDosDate, wDosTime: Word): TDateTime;

function GetFileSize(const FileName: String): Int64;
function GetFileDate(const FileName: String): TDatetime;

implementation

function DosDateTimeToDateTime(wDosDate, wDosTime: Word): TDateTime;
var
  DosDateTime: Integer;
  DateTime:    TDateTime;
begin
  LongRec(DosDateTime).Hi := wDosDate;
  LongRec(DosDateTime).Lo := wDosTime;
  if DosDateTime <> 0 then
   begin
    try
      DateTime := FileDateToDateTime(DosDateTime)
      except
        DateTime := 0;
     end;
   end
  else
   begin
    DateTime := 0;
   end;
  Result := DateTime;
end;

procedure refreshicon;
begin
        Shlobj.SHChangeNotify( SHCNE_ASSOCCHANGED, SHCNF_FLUSH, nil, nil );
end;

function GetFreeSpace(Drive: char): Int64;
var i : Byte;
begin
  i := Ord(uppercase(Drive)[1])-64;
  Result := DiskFree(i);
end;


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
  SHGetSpecialFolderLocation({Application.Handle}0, CSIDL_PERSONAL, ItemIDList);
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

function NewTempPath: string;
var 
  i: Integer;
  k: String;
begin
  i := Gettickcount;
  while DirectoryExists(GrabTempPath + IntToStr(i)) do
    Inc(i);
  k := GrabTempPath + IntToStr(i) + '\';
  MakeDirectory(k);
  Result := k;
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

function FileListPath : string;
begin
  Result := GrabProgramPath+'FileList\'
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
    if (k[i] = '/') {or (k[i] = '-')} then //Mr April 4, 2002 - '-'
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

function RemoveFrontSlash(Input: string): string;
begin
   Result := Input;
   if (Length(Result) > 0) then
    if (Result[1] = '\') then
       Result := Copy(Result, 2, Length(Result) - 1);
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

function KtoSize(Value : string) : integer;
var k : string;
    r : real;
begin
    k := Replace(Value,'kb','');
    k := Replace(k,',','');
    k := Trim(k);
    r := StrToFloatDef(k,0);
    Result := Trunc(r*1000);
end;

function FormatSize(Size : Int64) : string;
begin
  Result := FormatFloat('###,###,###,###,##0', Size);
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


procedure RunWWW(WWWPath: string);
begin
  ShellExecute({Application.Handle}0, 'open', PChar(WWWPath), '',
    '', SW_SHOWNORMAL);
end;

procedure ExecReg(var Path: string);
var
  k: String;
begin
  k := '/s /y ' + Path;
  Shellexecute({Application.Handle}0, 'open', 'Regedit.exe',
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
  ShellExecute({Application.Handle}0, 'open', PChar(Folder), '', PChar(Folder),
    SW_SHOWNORMAL);
end;

procedure ExploreFolder(Folder: String);
begin
  ShellExecute({Application.Handle}0, 'explore', PChar(Folder), '', PChar(Folder),
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
  if fileexists(filename) then
  begin
  myFile := FindFirstFile(PChar(FileName), myFindData);
  if myFile <> INVALID_HANDLE_VALUE then
   begin
    Windows.FindClose(myFile);
    Result := Int64(myFindData.nFileSizeHigh) shl Int64(32) +
      Int64(myFindData.nFileSizeLow);
   end;
  end;
end;

function Get_File_DateTime(const FileName: String): TDateTime;
var
  myFile:     THandle;
  myFindData: TWin32FindData;
  Dat,Tim : Word;
begin
  Result := now;
  myFile := FindFirstFile(PChar(FileName), myFindData);
  if myFile <> INVALID_HANDLE_VALUE then
   begin
    Windows.FindClose(myFile);
    FileTimetoDosDateTime(myFindData.ftLastWriteTime,Dat,Tim);
    Result := DosDateTimeToDateTime(Dat,Tim);
   end;
end;

function GetFileDate(const FileName: String): TDatetime;
begin
  Get_File_DateTime(Filename);
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
end; {DeleteDir}

function ExtractRootPath(aPath : string) : string;
var k : string;
begin
  k := RemoveDrive(aPath);
  While Extractfilepath(Removeslash(k)) <> '' do
    k := Extractfilepath(Removeslash(k));
  Result := RemoveSlash(k);
end;

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




function replace(source : string; fromm, too : string) : string;
begin
        Result := AnsiReplaceText(source,fromm,too);
end;

function GetFileSize(const FileName: String): Int64;
var
  myFile: THandle;
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



end.
