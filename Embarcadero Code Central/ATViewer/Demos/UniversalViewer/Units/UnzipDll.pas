// This UnzipDll unit requires Unzip unit by Headcrash Industries.
// Copyright (c) 2006 Alexey Torgashin

unit UnzipDll;

interface

function UnzipSingle(const fn, dir: string; masks: array of PChar): Longint;
function UnzipAll(const fn, dir: string): Longint;
// PK_OK means OK

procedure UnzipInit;
procedure UnzipFree;


implementation

uses
  Windows, SysUtils, Unzip;

var
  UFuncs: USERFUNCTIONS;
  DCList: DCL;
  Wiz_SingleEntryUnzip: PROCWiz_SingleEntryUnzip = nil;

procedure CallbackMessage(ucsize, csiz, cfactor, mo, dy, yr, hh, mm: Longword; c: Byte; fname, meth: PChar; crc: Longword; fCrypt: Byte); stdcall;
begin
  //MessageBox(0, PChar(Format('%u%s%u  %u%s  %u-%u-%u  %u:%u  %s', [ucsize, '/', csiz, cfactor, '%', mo, dy, yr, hh, mm, fname])),
  //  Unzip32, MB_OK or MB_ICONINFORMATION);
end;

function CallbackPrint(buffer: PChar; size: Longword): EDllPrint; stdcall;
begin
  //MessageBox(0, buffer, Unzip32, MB_OK or MB_ICONINFORMATION);
  //Result := size;
  Result:= 0;
end;

function CallbackPassword(pwbuf: PChar; size: Longint; m, efn: PChar): EDllPassword; stdcall;
begin
  Result:= IZ_PW_NONE;
end;

function CallbackReplace(filename: PChar): EDllReplace; stdcall;
begin
  Result:= IDM_REPLACE_NONE;
end;

function CallbackService(efn: PChar; details: Longword): EDllService; stdcall;
begin
  Result:= UZ_ST_CONTINUE;
end;


function UnzipSingle(const fn, dir: string; masks: array of PChar): Longint;
var
  incl: array[1..20] of PChar;
  inclNum: integer;
  excl: PChar;
  n: integer;
begin
  if @Wiz_SingleEntryUnzip=nil then
    begin
    Result:= PK_ERR;
    MessageBox(0, 'Cannot load unzip32.dll', 'Error', MB_OK or MB_ICONERROR);
    Exit
    end;

  FillChar(UFuncs, SizeOf(UFuncs), 0);
  with UFuncs do
    begin
    print:= CallbackPrint;
    sound:= nil;
    replace:= CallbackReplace;
    password:= CallbackPassword;
    SendApplicationMessage:= CallbackMessage;
    ServCallBk:= CallbackService;
    end;

  FillChar(DCList, SizeOf(DCList), 0);
  with DCList do
    begin
    ExtractOnlyNewer := 0;
    SpaceToUnderscore := 0;
    PromptToOverwrite := 0;
    fQuiet := 2;
    ncflag := 0;
    ntflag := 0;
    nvflag := 0;
    nfflag := 0;
    nzflag := 0;
    ndflag := 1;
    noflag := 1;
    naflag := 0;
    nZIflag := 0;
    C_flag := 1;
    fPrivilege := 0;
    lpszZipFN := PChar(fn);
    lpszExtractDir := PChar(dir);
    end;

  inclNum:= 0;
  for n:= Low(masks) to High(masks) do
    if inclNum<High(incl) then
      begin
      Inc(inclNum);
      incl[inclNum]:= masks[n];
      end;
  excl:= nil;

  Result:= Wiz_SingleEntryUnzip(inclNum, incl[1], 0, excl, DCList, UFuncs);
end;
  

function UnzipAll(const fn, dir: string): Longint;
begin
  Result:= UnzipSingle(fn, dir, ['*.*']);
end;
    

function GetPluginFilename: string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buf, GetModuleFileName(hInstance, buf, SizeOf(buf)));
end;


var
  hLib: THandle = 0;

procedure UnzipInit;
begin
  hLib:= LoadLibrary(PChar(ExtractFilePath(GetPluginFilename)+Unzip32));
  if hLib<>0 then
    begin
    @Wiz_SingleEntryUnzip:= GetProcAddress(hLib, 'Wiz_SingleEntryUnzip');
    end;
end;

procedure UnzipFree;
begin
  FreeLibrary(hLib);
  @Wiz_SingleEntryUnzip:= nil;
  hLib:= 0;
end;

  
end.
