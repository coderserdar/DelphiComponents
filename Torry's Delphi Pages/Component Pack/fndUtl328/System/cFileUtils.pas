{$INCLUDE ..\cDefines.inc}
unit cFileUtils;

{                                                                              }
{                       File utility functions v3.02                           }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cFileUtils.pas                    }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{ Revision history:                                                            }
{   01/06/2002  3.01  Created cFileUtils from cSysUtils.                       }
{   12/12/2002  3.02  Revision.                                                }
{                                                                              }

interface

uses
  { Delphi }
  {$IFDEF OS_MSWIN}
  Windows,
  {$ENDIF}
  SysUtils;



{                                                                              }
{ Path functions                                                               }
{                                                                              }
const
  PathSeperator = {$IFDEF OS_UNIX} '/'{$ENDIF}
                  {$IFDEF OS_MSWIN}'\'{$ENDIF};

function  PathHasDriveLetter(const Path: String): Boolean;
function  PathIsDriveLetter(const Path: String): Boolean;
function  PathIsDriveRoot(const Path: String): Boolean;
function  PathIsRoot(const Path: String): Boolean;
function  PathIsUNCPath(const Path: String): Boolean;
function  PathIsAbsolute(const Path: String): Boolean;
function  PathIsDirectory(const Path: String): Boolean;
function  PathInclSuffix(const Path: String;
          const PathSep: Char = PathSeperator): String;
function  PathExclSuffix(const Path: String;
          const PathSep: Char = PathSeperator): String;
procedure PathEnsureSuffix(var Path: String;
          const PathSep: Char = PathSeperator);
procedure PathEnsureNoSuffix(var Path: String;
          const PathSep: Char = PathSeperator);
function  PathCanonical(const Path: String;
          const PathSep: Char = PathSeperator): String;
function  PathExpand(const Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeperator): String;

function  PathLeftElement(const Path: String;
          const PathSep: Char = PathSeperator): String;
procedure PathSplitLeftElement(const Path: String;
          var LeftElement, RightPath: String;
          const PathSep: Char = PathSeperator);

procedure DecodeFilePath(const FilePath: String;
          var Path, FileName: String;
          const PathSep: Char = PathSeperator);

function  FileNameValid(const FileName: String): String;
function  FilePath(const FileName, Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeperator): String;

function  DirectoryExpand(const Path: String; const BasePath: String = '';
          const PathSep: Char = PathSeperator): String;

function  UnixPathToWinPath(const Path: String): String;
function  WinPathToUnixPath(const Path: String): String;



{                                                                              }
{ File operations                                                              }
{   MoveFile first attempts a rename, then a copy and delete.                  }
{                                                                              }
type
  EFileError = class(Exception);

function  GetFileSize(const FileName: String): Int64;
function  GetFileDateTime(const FileName: String): TDateTime;
function  GetFirstFileNameMatching(const FileMask: String): String;
function  DirEntryGetAttr(const FileName: String): Integer;
function  DirEntryIsDirectory(const FileName: String): Boolean;
function  FileHasAttr(const FileName: String; const Attr: Word): Boolean;
function  FileIsReadOnly(const FileName: String): Boolean;
procedure CopyFile(const FileName, DestName: String);
procedure MoveFile(const FileName, DestName: String);
function  DeleteFiles(const FileMask: String): Boolean;



{$IFDEF OS_MSWIN}
{                                                                              }
{ Logical Drive functions                                                      }
{                                                                              }
type
  TLogicalDriveType = (
      DriveRemovable,
      DriveFixed,
      DriveRemote,
      DriveCDRom,
      DriveRamDisk,
      DriveTypeUnknown);

function  DriveIsValid(const Drive: Char): Boolean;
function  DriveGetType(const Path: String): TLogicalDriveType;
function  DriveFreeSpace(const Path: String): Int64;
{$ENDIF}



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  cUtils,
  cStrings;



{                                                                              }
{ Path functions                                                               }
{                                                                              }
function PathHasDriveLetter(const Path: String): Boolean;
var P: PChar;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  P := Pointer(Path);
  if not (P^ in ['A'..'Z', 'a'..'z']) then
    exit;
  Inc(P);
  if P^ <> ':' then
    exit;
  Result := True;
end;

function PathIsDriveLetter(const Path: String): Boolean;
begin
  Result := (Length(Path) = 2) and PathHasDriveLetter(Path);
end;

function PathIsDriveRoot(const Path: String): Boolean;
begin
  Result := (Length(Path) = 3) and PathHasDriveLetter(Path) and
            (Path[3] = '\');
end;

function PathIsRoot(const Path: String): Boolean;
begin
  Result := ((Length(Path) = 1) and (Path[1] in csSlash)) or
            PathIsDriveRoot(Path);
end;

function PathIsUNCPath(const Path: String): Boolean;
var P: PChar;
begin
  Result := False;
  if Length(Path) < 2 then
    exit;
  P := Pointer(Path);
  if P^ <> '\' then
    exit;
  Inc(P);
  if P^ <> '\' then
    exit;
  Result := True;
end;

function PathIsAbsolute(const Path: String): Boolean;
begin
  if Path = '' then
    Result := False else
  if PathHasDriveLetter(Path) then
    Result := True else
  if PChar(Pointer(Path))^ in ['\', '/'] then
    Result := True else
    Result := False;
end;

function PathIsDirectory(const Path: String): Boolean;
var L: Integer;
    P: PChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := False else
  if (L = 2) and PathHasDriveLetter(Path) then
    Result := True else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      Result := P^ in csSlash;
    end;
end;

function PathInclSuffix(const Path: String; const PathSep: Char): String;
var L: Integer;
    P: PChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := '' else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      if P^ = PathSep then
        Result := Path else
        Result := Path + PathSep;
    end;
end;

procedure PathEnsureSuffix(var Path: String; const PathSep: Char);
begin
  Path := PathInclSuffix(Path, PathSep);
end;

procedure PathEnsureNoSuffix(var Path: String; const PathSep: Char);
begin
  Path := PathExclSuffix(Path, PathSep);
end;

function PathExclSuffix(const Path: String; const PathSep: Char): String;
var L: Integer;
    P: PChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := '' else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      if P^ = PathSep then
        Result := Copy(Path, 1, L - 1) else
        Result := Path;
    end;
end;

function PathCanonical(const Path: String; const PathSep: Char): String;
var L, M : Integer;
    I, J : Integer;
    P    : StringArray;
    Q    : PChar;
begin
  Result := Path;
  // \.\ references
  M := Length(Result);
  Repeat
    L := M;
    if L = 0 then
      exit;
    Result := StrReplace('\.\', '\', Result);
    Result := StrReplace('/./', '/', Result);
    M := Length(Result);
  Until L = M;
  // .\ prefix
  StrEnsureNoPrefix(Result, '.\');
  StrEnsureNoPrefix(Result, './');
  // \. suffix
  StrEnsureNoSuffix(Result, '\.');
  StrEnsureNoSuffix(Result, '/.');
  // ..
  if Pos('..', Result) > 0 then
    begin
      P := StrSplitChar(Result, PathSep);
      Repeat
        J := -1;
        For I := Length(P) - 1 downto 0 do
          if P[I] = '..' then
            begin
              J := I;
              break;
            end;
        if J = -1 then
          break;
        M := -1;
        For I := J - 1 downto 0 do
          if (P[I] = '') or ((I = 0) and PathHasDriveLetter(P[I])) then
            break else
          if P[I] <> '..' then
            begin
              M := I;
              break;
            end;
        if M = -1 then
          break;
        Remove(P, J, 1);
        Remove(P, M, 1);
      Until False;
      Result := StrJoinChar(P, PathSep);
    end;
  // \..\ prefix
  While StrMatchLeft(Result, '\..\') do
    Delete(Result, 1, 3);
  While StrMatchLeft(Result, '/../') do
    Delete(Result, 1, 3);
  if (Result = '\..') or (Result = '/..') then
    Result := '';
  L := Length(Result);
  if L = 0 then
    exit;
  // X:\..\ prefix
  Q := Pointer(Result);
  if Q^ in ['A'..'Z', 'a'..'z'] then
    begin
      if StrMatch(Result, ':\..\', 2) then
        Delete(Result, 4, 3) else
      if (L = 5) and StrMatch(Result, ':\..', 2) then
        begin
          SetLength(Result, 2);
          exit;
        end;
      L := Length(Result);
    end;
  // single dot
  Q := Pointer(Result);
  if L = 1 then
    begin
      if Q^ = '.' then
        Result := '';
      exit;
    end;
  // final dot
  Inc(Q, L - 2);
  if not (Q^ in ['.', '\', '/', ':']) then
    begin
      Inc(Q);
      if Q^ = '.' then
        Delete(Result, L, 1);
    end;
end;

function PathExpand(const Path: String; const BasePath: String;
    const PathSep: Char): String;
begin
  if Path = '' then
    Result := BasePath else
  if PathIsAbsolute(Path) then
    Result := Path else
    Result := PathInclSuffix(BasePath, PathSep) + Path;
  Result := PathCanonical(Result, PathSep);
end;

function PathLeftElement(const Path: String; const PathSep: Char): String;
var I: Integer;
begin
  I := PosChar(PathSep, Path);
  if I <= 0 then
    Result := Path else
    Result := Copy(Path, 1, I - 1);
end;

procedure PathSplitLeftElement(const Path: String;
    var LeftElement, RightPath: String; const PathSep: Char);
var I: Integer;
begin
  I := PosChar(PathSep, Path);
  if I <= 0 then
    begin
      LeftElement := Path;
      RightPath := '';
    end else
    begin
      LeftElement := Copy(Path, 1, I - 1);
      RightPath := CopyFrom(Path, I + 1);
    end;
end;

procedure DecodeFilePath(const FilePath: String; var Path, FileName: String;
    const PathSep: Char);
var I: Integer;
begin
  I := PosCharRev(PathSep, FilePath);
  if I <= 0 then
    begin
      Path := '';
      FileName := FilePath;
    end else
    begin
      Path := Copy(FilePath, 1, I);
      FileName := CopyFrom(FilePath, I + 1);
    end;
end;

function FileNameValid(const FileName: String): String;
begin
  Result := StrReplaceChar(['\', '/', ':', '>', '<', '*', '?'], '_', FileName);
  if Result = '.' then
    Result := '' else
  if Result = '..' then
    Result := '_';
end;

function FilePath(const FileName, Path: String; const BasePath: String;
    const PathSep: Char): String;
var P, F: String;
begin
  F := FileNameValid(FileName);
  if F = '' then
    begin
      Result := '';
      exit;
    end;
  P := PathExpand(Path, BasePath, PathSep);
  if P = '' then
    Result := F else
    Result := PathInclSuffix(P, PathSep) + F;
  End;

function DirectoryExpand(const Path: String; const BasePath: String;
    const PathSep: Char): String;
begin
  Result := PathExpand(PathInclSuffix(Path, PathSep),
      PathInclSuffix(BasePath), PathSep);
end;

function UnixPathToWinPath(const Path: String): String;
begin
  Result := StrReplaceChar('/', '\',
            StrReplaceChar(['\', ':', '<', '>', '|'], '_', Path));
end;

function WinPathToUnixPath(const Path: String): String;
begin
  Result := Path;
  if PathHasDriveLetter(Path) then
    begin
      // X: -> \X
      Result[2] := Result[1];
      Result[1] := '\';
    end else
  if StrMatchLeft(Path, '\\.\') then
    // \\.\ -> \
    Delete(Result, 1, 3) else
  if PathIsUncPath(Path) then
    // \\ -> \
    Delete(Result, 1, 1);
  Result := StrReplaceChar('\', '/',
            StrReplaceChar(['/', ':', '<', '>', '|'], '_', Result));
end;




{                                                                              }
{ File operations                                                              }
{                                                                              }
function GetFileSize(const FileName: String): Int64;
var SRec : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SRec) <> 0 then
    Result := -1 else
    begin
      {$IFDEF OS_MSWIN}
      {$WARNINGS OFF}
      Int64Rec(Result).Lo := SRec.FindData.nFileSizeLow;
      Int64Rec(Result).Hi := SRec.FindData.nFileSizeHigh;
      {$IFDEF DEBUG}{$WARNINGS ON}{$ENDIF}
      {$ELSE}
      Result := SRec.Size;
      {$ENDIF}
      FindClose(SRec);
    end;
end;

function GetFileDateTime(const FileName: String): TDateTime;
var Age : LongInt;
begin
  Age := FileAge(FileName);
  if Age = -1 then
    Result := 0
  else
    Result := FileDateToDateTime(Age);
end;

function GetFirstFileNameMatching(const FileMask: String): String;
var SRec : TSearchRec;
begin
  Result := '';
  if FindFirst(FileMask, faAnyFile, SRec) = 0 then
    try
      Repeat
        if SRec.Attr and faDirectory = 0 then
          begin
            Result := ExtractFilePath(FileMask) + SRec.Name;
            exit;
          end;
      Until FindNext(SRec) <> 0;
    finally
      FindClose(SRec);
    end;
end;

function DirEntryGetAttr(const FileName: String): Integer;
var SRec : TSearchRec;
begin
  if (FileName = '') or PathIsDriveLetter(FileName) then
    Result := -1 else
  if PathIsRoot(FileName) then
    Result := $0800 or faDirectory else
  if FindFirst(PathExclSuffix(FileName, '\'), faAnyFile, SRec) = 0 then
    begin
      Result := SRec.Attr;
      FindClose(SRec);
    end
  else
    Result := -1;
end;

function DirEntryIsDirectory(const FileName: String): Boolean;
var SRec : TSearchRec;
begin
  if (FileName = '') or PathIsDriveLetter(FileName) then
    Result := False else
  if PathIsRoot(FileName) then
    Result := True else
  if FindFirst(PathExclSuffix(FileName, '\'), faDirectory, SRec) = 0 then
    begin
      Result := SRec.Attr and faDirectory <> 0;
      FindClose(SRec);
    end
  else
    Result := False;
end;

{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM OFF}{$ENDIF}
function FileHasAttr(const FileName: String; const Attr: Word): Boolean;
var A : Integer;
begin
  A := FileGetAttr(FileName);
  Result := (A >= 0) and (A and Attr <> 0);
end;

function FileIsReadOnly(const FileName: String): Boolean;
begin
  Result := FileHasAttr(FileName, faReadOnly);
end;

procedure CopyFile(const FileName, DestName: String);
var
  CopyBuffer   : Pointer;
  BytesCopied  : Longint;
  Source, Dest : Integer;
  Destination  : TFileName;
const
  ChunkSize = 8192;
begin
  Destination := ExpandFileName(DestName);
  if FileHasAttr(Destination, faDirectory) then // if destination is a directory, append file name
    Destination := Destination + '\' + ExtractFileName(FileName);
  GetMem(CopyBuffer, ChunkSize);
  try
    Source := FileOpen(FileName, fmShareDenyWrite);
    if Source < 0 then
      raise EFileError.CreateFmt('Can not open file %s', [FileName]);
    try
      Dest := FileCreate(Destination);
      if Dest < 0 then
        raise EFileError.CreateFmt('Can not create file %s', [Destination]);
      try
        Repeat
          BytesCopied := FileRead(Source, CopyBuffer^, ChunkSize);
          if BytesCopied > 0 then
            FileWrite(Dest, CopyBuffer^, BytesCopied);
        Until BytesCopied < ChunkSize;
      finally
        FileClose(Dest);
      end;
    finally
      FileClose(Source);
    end;
  finally
    FreeMem(CopyBuffer, ChunkSize);
  end;
end;

procedure MoveFile(const FileName, DestName: String);
var Destination : String;
    Attr : Integer;
begin
  Destination := ExpandFileName(DestName);
  if not RenameFile(FileName, Destination) then
    begin
      Attr := FileGetAttr(FileName);
      if (Attr < 0) or (Attr and faReadOnly <> 0) then
        raise EFileError.Create(Format('Can not move file %s', [FileName]));
      CopyFile(FileName, Destination);
      DeleteFile(FileName);
    end;
end;

function DeleteFiles(const FileMask: String): Boolean;
var SRec : TSearchRec;
    Path : String;
begin
  Result := FindFirst(FileMask, faAnyFile, SRec) = 0;
  if not Result then
    exit;
  try
    Path := ExtractFilePath(FileMask);
    Repeat
      if (SRec.Name <> '') and (SRec.Name  <> '.') and (SRec.Name <> '..') and
        (SRec.Attr and (faVolumeID + faDirectory) = 0) then
      begin
        Result := DeleteFile(Path + SRec.Name);
        if not Result then
          break;
      end;
    Until FindNext(SRec) <> 0;
  finally
    FindClose(SRec);
  end;
end;
{$IFDEF DELPHI6_UP}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}



{$IFDEF OS_MSWIN}
{                                                                              }
{ Logical Drive functions                                                      }
{                                                                              }
function DriveIsValid(const Drive: Char): Boolean;
var D : Char;
begin
  D := UpCase(Drive);
  Result := D in ['A'..'Z'];
  if not Result then
    exit;
  Result := IsBitSet(GetLogicalDrives, Ord(D) - Ord('A'));
end;

function DriveGetType(const Path: String): TLogicalDriveType;
begin
  Case GetDriveType(PChar(Path)) of
    DRIVE_REMOVABLE : Result := DriveRemovable;
    DRIVE_FIXED     : Result := DriveFixed;
    DRIVE_REMOTE    : Result := DriveRemote;
    DRIVE_CDROM     : Result := DriveCDRom;
    DRIVE_RAMDISK   : Result := DriveRamDisk;
  else
    Result := DriveTypeUnknown;
  end;
end;

function DriveFreeSpace(const Path: String): Int64;
var D: Byte;
begin
  if PathHasDriveLetter(Path) then
    D := Ord(UpCase(PChar(Path)^)) - Ord('A') + 1 else
  if PathIsUNCPath(Path) then
    begin
      Result := -1;
      exit;
    end else
    D := 0;
  Result := DiskFree(D);
end;
{$ENDIF}



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
begin
  Assert(PathHasDriveLetter('C:'), 'PathHasDriveLetter');
  Assert(PathHasDriveLetter('C:\'), 'PathHasDriveLetter');
  Assert(not PathHasDriveLetter('\C\'), 'PathHasDriveLetter');
  Assert(not PathHasDriveLetter('::'), 'PathHasDriveLetter');

  Assert(PathIsAbsolute('\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('C:\'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('C:'), 'PathIsAbsolute');
  Assert(PathIsAbsolute('\C\..\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute(''), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\D'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('C\D\'), 'PathIsAbsolute');
  Assert(not PathIsAbsolute('..\'), 'PathIsAbsolute');

  Assert(PathIsDirectory('\'), 'PathIsDirectory');
  Assert(PathIsDirectory('\C\'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:\'), 'PathIsDirectory');
  Assert(PathIsDirectory('C:\D\'), 'PathIsDirectory');
  Assert(not PathIsDirectory(''), 'PathIsDirectory');
  Assert(not PathIsDirectory('D'), 'PathIsDirectory');
  Assert(not PathIsDirectory('C\D'), 'PathIsDirectory');

  Assert(PathInclSuffix('', '\') = '', 'PathInclSuffix');
  Assert(PathInclSuffix('C', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\', '\') = 'C\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\D', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffix('C\D\', '\') = 'C\D\', 'PathInclSuffix');
  Assert(PathInclSuffix('C:', '\') = 'C:\', 'PathInclSuffix');
  Assert(PathInclSuffix('C:\', '\') = 'C:\', 'PathInclSuffix');

  Assert(PathExclSuffix('', '\') = '', 'PathExclSuffix');
  Assert(PathExclSuffix('C', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffix('C\', '\') = 'C', 'PathExclSuffix');
  Assert(PathExclSuffix('C\D', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffix('C\D\', '\') = 'C\D', 'PathExclSuffix');
  Assert(PathExclSuffix('C:', '\') = 'C:', 'PathExclSuffix');
  Assert(PathExclSuffix('C:\', '\') = 'C:', 'PathExclSuffix');

  Assert(PathCanonical('', '\') = '', 'PathCanonical');
  Assert(PathCanonical('.', '\') = '', 'PathCanonical');
  Assert(PathCanonical('.\', '\') = '', 'PathCanonical');
  Assert(PathCanonical('..\', '\') = '..\', 'PathCanonical');
  Assert(PathCanonical('\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonical('\X\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonical('\..', '\') = '', 'PathCanonical');
  Assert(PathCanonical('X', '\') = 'X', 'PathCanonical');
  Assert(PathCanonical('\X', '\') = '\X', 'PathCanonical');
  Assert(PathCanonical('X.', '\') = 'X', 'PathCanonical');
  Assert(PathCanonical('.', '\') = '', 'PathCanonical');
  Assert(PathCanonical('\X.', '\') = '\X', 'PathCanonical');
  Assert(PathCanonical('\X.Y', '\') = '\X.Y', 'PathCanonical');
  Assert(PathCanonical('\X.Y\', '\') = '\X.Y\', 'PathCanonical');
  Assert(PathCanonical('\A\X..Y\', '\') = '\A\X..Y\', 'PathCanonical');
  Assert(PathCanonical('\A\.Y\', '\') = '\A\.Y\', 'PathCanonical');
  Assert(PathCanonical('\A\..Y\', '\') = '\A\..Y\', 'PathCanonical');
  Assert(PathCanonical('\A\Y..\', '\') = '\A\Y..\', 'PathCanonical');
  Assert(PathCanonical('\A\Y..', '\') = '\A\Y..', 'PathCanonical');
  Assert(PathCanonical('X', '\') = 'X', 'PathCanonical');
  Assert(PathCanonical('X\', '\') = 'X\', 'PathCanonical');
  Assert(PathCanonical('X\Y\..', '\') = 'X', 'PathCanonical');
  Assert(PathCanonical('X\Y\..\', '\') = 'X\', 'PathCanonical');
  Assert(PathCanonical('\X\Y\..', '\') = '\X', 'PathCanonical');
  Assert(PathCanonical('\X\Y\..\', '\') = '\X\', 'PathCanonical');
  Assert(PathCanonical('\X\Y\..\..', '\') = '', 'PathCanonical');
  Assert(PathCanonical('\X\Y\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonical('\A\.\.\X\.\Y\..\.\..\.\', '\') = '\A\', 'PathCanonical');
  Assert(PathCanonical('C:', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonical('C:\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonical('C:\A\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonical('C:\A\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonical('C:\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonical('C:\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonical('C:\A\..\..', '\') = 'C:', 'PathCanonical');
  Assert(PathCanonical('C:\A\..\..\', '\') = 'C:\', 'PathCanonical');
  Assert(PathCanonical('\A\B\..\C\D\..\', '\') = '\A\C\', 'PathCanonical');
  Assert(PathCanonical('\A\B\..\C\D\..\..\', '\') = '\A\', 'PathCanonical');
  Assert(PathCanonical('\A\B\..\C\D\..\..\..\', '\') = '\', 'PathCanonical');
  Assert(PathCanonical('\A\B\..\C\D\..\..\..\..\', '\') = '\', 'PathCanonical');

  Assert(PathExpand('', '', '\') = '', 'PathExpand');
  Assert(PathExpand('', '\', '\') = '\', 'PathExpand');
  Assert(PathExpand('', '\C', '\') = '\C', 'PathExpand');
  Assert(PathExpand('', '\C\', '\') = '\C\', 'PathExpand');
  Assert(PathExpand('..\', '\C\', '\') = '\', 'PathExpand');
  Assert(PathExpand('..', '\C\', '\') = '', 'PathExpand');
  Assert(PathExpand('\..', '\C\', '\') = '', 'PathExpand');
  Assert(PathExpand('\..\', '\C\', '\') = '\', 'PathExpand');
  Assert(PathExpand('A', '..\', '\') = '..\A', 'PathExpand');
  Assert(PathExpand('..\', '..\', '\') = '..\..\', 'PathExpand');
  Assert(PathExpand('\', '', '\') = '\', 'PathExpand');
  Assert(PathExpand('\', '\C', '\') = '\', 'PathExpand');
  Assert(PathExpand('\A', '\C\', '\') = '\A', 'PathExpand');
  Assert(PathExpand('\A\', '\C\', '\') = '\A\', 'PathExpand');
  Assert(PathExpand('\A\B', '\C', '\') = '\A\B', 'PathExpand');
  Assert(PathExpand('A\B', '\C', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('A\B', '\C', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('A\B', '\C\', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('A\B', '\C\', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('A\B', 'C\D', '\') = 'C\D\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B', 'C\D', '\') = 'C\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B', '\C\D', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('..\..\A\B', 'C\D', '\') = 'A\B', 'PathExpand');
  Assert(PathExpand('..\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpand('..\..\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpand('\..\A\B', '\C\D', '\') = '\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B', '\..\C\D', '\') = '\C\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B', '..\C\D', '\') = '..\C\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B', 'C:\C\D', '\') = 'C:\C\A\B', 'PathExpand');
  Assert(PathExpand('..\A\B\', 'C:\C\D', '\') = 'C:\C\A\B\', 'PathExpand');

  Assert(FilePath('C', '..\X\Y', 'A\B', '\') = 'A\X\Y\C', 'FilePath');
  Assert(FilePath('C', '\X\Y', 'A\B', '\') = '\X\Y\C', 'FilePath');
  Assert(FilePath('C', '', 'A\B', '\') = 'A\B\C', 'FilePath');
  Assert(FilePath('', '\X\Y', 'A\B', '\') = '', 'FilePath');
  Assert(FilePath('C', 'X\Y', 'A\B', '\') = 'A\B\X\Y\C', 'FilePath');
  Assert(FilePath('C', 'X\Y', '', '\') = 'X\Y\C', 'FilePath');

  Assert(DirectoryExpand('', '', '\') = '', 'DirectoryExpand');
  Assert(DirectoryExpand('', '\X', '\') = '\X\', 'DirectoryExpand');
  Assert(DirectoryExpand('\', '\X', '\') = '\', 'DirectoryExpand');
  Assert(DirectoryExpand('\A', '\X', '\') = '\A\', 'DirectoryExpand');
  Assert(DirectoryExpand('\A\', '\X', '\') = '\A\', 'DirectoryExpand');
  Assert(DirectoryExpand('\A\B', '\X', '\') = '\A\B\', 'DirectoryExpand');
  Assert(DirectoryExpand('A', '\X', '\') = '\X\A\', 'DirectoryExpand');
  Assert(DirectoryExpand('A\', '\X', '\') = '\X\A\', 'DirectoryExpand');
  Assert(DirectoryExpand('C:', '\X', '\') = 'C:\', 'DirectoryExpand');
  Assert(DirectoryExpand('C:\', '\X', '\') = 'C:\', 'DirectoryExpand');

  Assert(UnixPathToWinPath('/c/d.f') = '\c\d.f', 'UnixPathToWinPath');
  Assert(WinPathToUnixPath('\c\d.f') = '/c/d.f', 'WinPathToUnixPath');
end;



end.

