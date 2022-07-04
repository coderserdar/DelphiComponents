unit kmUtils;

interface

uses Windows,Classes,Forms,Graphics,SysUtils,StdCtrls,WinInet,Registry,Messages;

type
  TCallBack = procedure(Position,Size,sTime:LongInt); { export; }

  TEllipseType = (etNone, etEndEllipse, etPathEllipse);

  TellLabel = class(TLabel)
  private
    FEllipseType: TEllipseType;
    procedure SetEllipseType(const Value: TEllipseType);
    { Private declarations }
  protected
    { Protected declarations }
    function DoEllipse(s:string):string;
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property EllipseType: TEllipseType read FEllipseType write SetEllipseType;
  end;

var
  FCancelCopy: boolean;  // used by FastFileCopy to trigger cancel operation
  FCancelMsg: string;
  FMsgTitle: string;

{ External function timeGetTime }
function timeGetTime: DWORD; stdcall;
function timeGetTime; external 'winmm.dll' name 'timeGetTime';

function ConvertToLocalPath(const St: string): string;
function CreatePath(aPath:string): string;
function EncryptDecrypt(s:String):String;
function ExcludeLeadingSlash(const St: String): string;
function ExcludeTrailingBackslash(const St: String): string;
function GetCommonAppData(): string;
function GetCommonDesktop(): string;
function GetEnvVarValue(const VarName: string): string;
function GetProgramFilesDir(): string;
function GetSystemDir(): string;
function GetTempDir(): string;
function GetWindowsDir(): string;
function IncludeTrailingBackslash(const St: String): String;
function IncludeTrailingForwardslash(const St: String): String;
function LoadResourceToStream(Instance: hInst; ResName, ResType: PChar; Stream: TStream): Boolean;
function StrPas(const Str: PChar): string;
function VersionCheck(const newVer,oldVer: String): Shortint;
procedure FastFileCopy(const InFileName,OutFileName: string; CallBack: TCallBack; AskCancel: boolean);


implementation

const
  ALIGNSTYLE : array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WORDWRAPSTYLE : array[Boolean] of DWORD = (DT_SINGLELINE, DT_WORDBREAK);
  LAYOUTSTYLE : array[TTextLayout] of DWORD = (0,DT_VCENTER,DT_BOTTOM);
  ELLIPSSTYLE : array[TEllipseType] of DWORD = (0,DT_END_ELLIPSIS,DT_PATH_ELLIPSIS);
  ACCELSTYLE : array[Boolean] of DWORD = (DT_NOPREFIX,0);


{ TkmEllipseLabel }
function TellLabel.DoEllipse(s:string):string;
{ this function works with both network and internet paths }
var
  p:integer;
  endStr,tmpStr:string;
begin
  Result := s;
  if Canvas.TextWidth(s) > Width then begin
    { default to etEndEllipse }
    tmpStr := s;
    while Canvas.TextWidth(tmpStr + '...') > Width do begin
      SetLength(tmpStr,Length(tmpStr)-1);
      if Length(tmpStr) = 0 then exit;
      end;
    Result := tmpStr + '...';

    if FEllipseType = etEndEllipse then exit;

    { now do etPathEllipse... if fails, default will be etEndEllipse }
    tmpStr := s;
    p := LastDelimiter('/\',tmpStr);
    if p = 0 then exit;
    endStr := copy(tmpStr,p,Length(tmpStr)-p+1);
    SetLength(tmpStr,p-1);
    if Length(tmpStr) = 0 then exit;
    while Canvas.TextWidth(tmpStr + '...' + endStr) > Width do begin
      SetLength(tmpStr,Length(tmpStr)-1);
      if Length(tmpStr) = 0 then exit;
      end;
    Result := tmpStr + '...' + endStr;
    end;
end;

procedure TellLabel.Paint;
var
  R: TRect;
  DrawStyle: DWORD;
  eCaption: string;
begin
  R := GetClientRect;

  if not Transparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  end;

  Canvas.Brush.Style := bsClear;

  DrawStyle := ALIGNSTYLE[Alignment] or WORDWRAPSTYLE[WordWrap] or
    LAYOUTSTYLE[Layout] or ELLIPSSTYLE[FEllipseType] or ACCELSTYLE[ShowAccelChar] or DT_MODIFYSTRING;

  {$IFDEF DELPHI4_LVL}
  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);
  {$ENDIF}

  Canvas.Font := Font;

  eCaption := DoEllipse(Caption);
  if not Enabled then
  begin
    OffsetRect(R, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawTextEx(Canvas.Handle,PChar(eCaption),Length(eCaption),R, DrawStyle, nil);
    OffsetRect(R, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawTextEx(Canvas.Handle,PChar(eCaption),Length(eCaption),R, DrawStyle, nil);
  end
  else
    DrawTextEx(Canvas.Handle,PChar(eCaption),Length(eCaption),R, DrawStyle, nil);
end; {Paint}

{ ----------------------------------------------------------------- }

procedure TellLabel.SetEllipseType(const Value: TEllipseType);
begin
  if FEllipseType <> Value then
  begin
    FEllipseType := Value;
    Invalidate;
  end;
end; {SetEllipseType}

{ end TkmEllipseLabel }

{ ----------------------------------------------------------------- }

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end; {StrPas}

{ ----------------------------------------------------------------- }

function EncryptDecrypt(s:String):String;
begin
Result:= '';
if s = '' then Exit;
asm
mov eax,[Offset s]
@_Loop:
xor Byte Ptr [eax],23
inc eax
cmp Byte Ptr [eax],0
jne @_Loop
end;
Result:= s;
end; {EncryptDecrypt}

{ ----------------------------------------------------------------- }

function CreatePath(aPath:string): string;
var
  i: integer;
  sTmp: array [1..MAX_PATH] of char;
begin
  FillChar(sTmp,MAX_PATH,#0);
  for i := 1 to Length(aPath) do begin
    sTmp[i] := aPath[i];
    if (sTmp[i] = '\') or (i = Length(aPath)) then
      if not DirectoryExists(string(sTmp)) then
        CreateDirectory(@sTmp,nil);
    end;
end; {CreatePath}

{ ----------------------------------------------------------------- }

function IncludeTrailingBackslash(const St: string): string;
begin
 if (St <> '') and (St[Length(St)] <> '\') then
   Result := St + '\'
 else
   Result := St;
end; {IncludeTrailingBackslash}

{ ----------------------------------------------------------------- }

function IncludeTrailingForwardslash(const St: string): string;
begin
 if (St <> '') and (St[Length(St)] <> '/') then
   Result := St + '/'
 else
   Result := St;
end; {IncludeTrailingForwardslash}

{ ----------------------------------------------------------------- }

function ExcludeTrailingBackslash(const St: string): string;
begin
  Result := St;
  while (Result <> '') and (Result[Length(Result)] = '\') do
   SetLength(Result, Length(Result) - 1);
end; {ExcludeTrailingBackslash}

{ ----------------------------------------------------------------- }

function ExcludeLeadingSlash(const St: string): string;
var
  tStr: String;
begin
  Result := St;
  tStr := St;
  SetLength(tStr,1);
  if (tStr = '\') or (tStr = '/') then
    result := Copy(St,2,Length(St)-1);
end; {ExcludeLeadingSlash}

{ ----------------------------------------------------------------- }

function ConvertToLocalPath(const St: string): string;
begin
  Result := StringReplace(St,'/','\',[rfReplaceAll]);
end; {ConvertToLocalPath}

{ ----------------------------------------------------------------- }

function GetWindowsDir(): string;
var
  PC: array[0..MAX_PATH + 1] of Char;
begin
  GetWindowsDirectory(PC,MAX_PATH);
  Result := IncludeTrailingBackslash(StrPas(PC));
end; { GetWindowsDir }

{ ----------------------------------------------------------------- }

function GetSystemDir(): string;
{ returns path to System directory }
var
  PC: Array[0..MAX_PATH + 1] of Char;
begin
  GetSystemDirectory(PC,MAX_PATH);
  Result := IncludeTrailingBackslash(StrPas(PC));
end; { GetSystemDir }

{ ----------------------------------------------------------------- }

function GetTempDir(): string;
var
  PC: Array[0..MAX_PATH + 1] of Char;
  s: string;
begin
  GetTempPath(MAX_PATH, PC);
  s := IncludeTrailingBackslash(StrPas(PC));
  if not DirectoryExists(s) then begin
    s := ExtractFilePath(Application.ExeName);
    s := IncludeTrailingBackslash(s);
    s := s + 'Temp\';
    CreatePath(s);
    end;
  Result := s;
end; { GetTempDir }

{ ----------------------------------------------------------------- }

function GetProgramFilesDir(): string;
var
  PC:string;
  reg : TRegIniFile;
  sKey : string;
begin
  PC := '';
  reg := nil;
  try
  sKey := '\Software\Microsoft\Windows\CurrentVersion';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  PC := reg.ReadString(sKey,'ProgramFilesDir','');
  finally
  reg.Free;
  end;
  Result := IncludeTrailingBackslash(PC);
end; { GetProgramFilesDir }

{ ----------------------------------------------------------------- }

function GetCommonAppData(): string;
var
  PC:string;
  reg : TRegIniFile;
  sKey : string;
begin
  PC := '';
  reg := nil;
  try
  sKey := '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  PC := reg.ReadString(sKey,'Common AppData','');
  finally
  reg.Free;
  end;
  if PC = '' then
     PC := IncludeTrailingBackslash(GetEnvVarValue('ALLUSERSPROFILE')) + 'Application Data\';
  Result := IncludeTrailingBackslash(PC);
end; { GetCommonAppData }

{ ----------------------------------------------------------------- }

function GetCommonDesktop(): string;
var
  PC:string;
  reg : TRegIniFile;
  sKey : string;
begin
  PC := '';
  reg := nil;
  try
  sKey := '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  PC := reg.ReadString(sKey,'Common Desktop','');
  finally
  reg.Free;
  end;
  if PC = '' then
     PC := IncludeTrailingBackslash(GetEnvVarValue('ALLUSERSPROFILE')) + 'Desktop\';
  Result := IncludeTrailingBackslash(PC);
end; { GetCommonDesktop }

{ ----------------------------------------------------------------- }

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  { Get required buffer size (inc. terminal #0) }
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    { Read env var value into result string }
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName),PChar(Result),BufSize);
  end
  else
    { No such environment variable }
    Result := '';
end; { GetEnvVarValue }

{--- Here is the list of different variables you could use -----------
ALLUSERSPROFILE
APPDATA
CLIENTNAME
CommonProgramFiles
COMPUTERNAME
ComSpec
HOMEDRIVE
HOMEPATH
LOGONSERVER
NUMBER_OF_PROCESSORS
OS
Path
PATHEXT
PCToolsDir
PROCESSOR_ARCHITECTURE
PROCESSOR_IDENTIFIER
PROCESSOR_LEVEL
PROCESSOR_REVISION
ProgramFiles
SESSIONNAME
SystemDrive
SystemRoot
TEMP
TMP
USERDOMAIN
USERNAME
USERPROFILE
windir

{ ----------------------------------------------------------------- }

function LoadResourceToStream(Instance: hInst; ResName, ResType: PChar; Stream: TStream): Boolean;
var
  ResSrc: hRSrc;
  ResGlobal: hGlobal;
  ResAddr: Pointer;
  ResLength: DWord;
begin
  Result := False;
  if Stream = nil then Exit;

  ResSrc := FindResource(Instance, ResName, ResType);
  if ResSrc = 0 then Exit;
  ResGlobal := LoadResource(Instance, ResSrc);
  if ResGlobal = 0 then Exit;
  ResAddr := LockResource(ResGlobal);
  FreeResource(ResGlobal);
  if ResAddr = nil then Exit;

  ResLength := SizeOfResource(Instance, ResSrc);
  if ResLength = 0 then Exit;

  try
    Stream.WriteBuffer(ResAddr^, ResLength);
    Result := True;
  except
  end;
end; {LoadResourceToStream}

{ ----------------------------------------------------------------- }

function VersionCheck(const newVer,oldVer: String): ShortInt;
const
  MAX_SECTIONS = 4;
type
  TVerArray = array[1..MAX_SECTIONS] of string;
var
  i,vNew,vOld: integer;
  newVerStr,oldVerStr: TVerArray;

  procedure SplitVersion(Version:String; var VerArrayStr: TVerArray);
  var
    i,p: integer;
  begin
    Version := Version + '.';
    for i := 1 to MAX_SECTIONS do begin
      p := Pos('.', Version);
      if p = 0 then
        VerArrayStr[i] := '0' else
        VerArrayStr[i] := Copy(Version,1,p - 1);
      Version := Copy(Version,p + 1,Length(Version));
     end;
  end;

begin
  result := 0;
  SplitVersion(newVer,newVerStr);
  SplitVersion(oldVer,oldVerStr);

  for i := 1 to MAX_SECTIONS do
   begin
    vNew := StrToIntDef(newVerStr[i],-1);
    vOld := StrToIntDef(oldVerStr[i],-1);

    if (vNew = -1) or (vOld = -1) then begin
      result := StrComp(PChar(LowerCase(newVerStr[i])),
                        PChar(LowerCase(oldVerStr[i])));
      if result = 0 then Continue;
      exit;
     end
    else
     if vNew <> vOld then begin
       if vNew > vOld then
         result := 1 else
         result := -1;
       exit;
      end;
   end;
end; {VersionCheck}

{ ----------------------------------------------------------------- }

procedure FastFileCopy(const InFileName,OutFileName: string; CallBack: TCallBack; AskCancel: boolean);
const
  BufSize = 3 * 4 * 4096; { 48Kbytes gives me the best results }
type
  PBuffer = ^TBuffer;
  TBuffer = array[1..BufSize] of Byte;
var
  filePath: string;
  Size: DWORD;
  Buffer: PBuffer;
  infile, outfile: file;
  SizeDone,SizeFile,t1: LongInt;
begin
  if (InFileName <> OutFileName) then { file cannot be copied onto itself }
  begin
    buffer := nil;
    filePath := ExtractFilePath(OutFileName);
    if not (DirectoryExists(filePath)) then
      CreatePath(filePath);
    Assign(infile, InFileName);
    Reset(infile, 1);
    try
      SizeFile := FileSize(infile);
      Assign(outfile, OutFileName);
      Rewrite(outfile, 1);
      try
        SizeDone := 0;
        New(Buffer);
        t1 := timegettime();
        repeat
          BlockRead(infile, Buffer^, BufSize, Size);
          Inc(SizeDone, Size);
          BlockWrite(outfile, Buffer^, Size);
          if FCancelCopy then begin
            if AskCancel then begin
              MessageBeep(MB_ICONASTERISK);
              if MessageBox(Application.Handle,PChar(FCancelMsg),PChar(FMsgTitle),
                  MB_YESNO or MB_ICONQUESTION) = IDYES then begin
                Size := 0; { forces exit }
                end
              else FCancelCopy := false;
              end;
            end;
          CallBack(SizeDone,SizeFile,t1);
        until Size < BufSize;
        FileSetDate(TFileRec(outfile).Handle,
        FileGetDate(TFileRec(infile).Handle));
      finally
        if Buffer <> nil then
          Dispose(Buffer);
        CloseFile(outfile)
      end;
    finally
      CloseFile(infile);
    end;
  end;
end; {FastFileCopy}

end.
