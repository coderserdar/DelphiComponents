{$I ATViewerOptions.inc}

unit ATxUtils;

interface

uses
  Windows, Graphics, Controls, Forms;

function IsWindowsVista: Boolean;

procedure SSetFont(AFont: TFont; S: string);
function SFontToString(AFont: TFont): string;

procedure FixFormFont(AFont: TFont);
procedure FixImageList32Bit(ImageList: TImageList);
procedure SetFormStyle(Form: TForm; Value: Boolean);
function GetFormOnTop(Form: TForm): Boolean;
procedure SetFormOnTop(Form: TForm; Value: Boolean);

function FGetAppDataPath: WideString;                  //%AppData% path
function SExpandVars(const S: WideString): WideString; //Expand env vars
function SExpanded(const S: WideString): Boolean;      //Are env vars expanded
procedure SSetEnv(const Name, Value: WideString);      //Set env var

function IsFilenameRemote(const fn: string): Boolean; //File on Network disk ( \\server\share\... )
function IsFilenameFixed(const fn: string): Boolean;  //File on fixed disk (C: D:..)
procedure SDecodeSearchW(var S: WideString);          //Prepare string for search: replace \n \r \0..

function FFileWriteStringA(const AFileName: WideString; const S: AnsiString): Boolean;
function FFileWriteStringW(const AFileName: WideString; const S: WideString): Boolean;
function FNumberName(const FN: WideString): WideString; //Generate number name by mask 'File%d.ext'
function FTempPath: WideString;
function FClipName(const Ext: WideString): WideString;      //Clipboard file name (in %Temp%)
function FPasteToFile: WideString;                      //Pastes clipboard to FClipName
function FSearchDir(const sname, sdir: string; var fn: string): boolean;

const
  WM_XBUTTONDOWN   = $020B;
  WM_XBUTTONUP     = $020C;
  WM_XBUTTONDBLCLK = $020D;

function FormatFileSize(const Size: Int64): string;
function FormatFileTime(const ft: TFileTime): string;


implementation

uses
  SysUtils,
  Classes, Consts, CommCtrl, ATxFProc, ATxSProc,
  TntClasses, Clipbrd, TntClipbrd,
  {$ifdef PNG}
  PngImage,
  {$endif}
  ATViewerMsg, ATxMsgProc;

//--------------------------------------------------------
function IsWindowsVista: Boolean;
begin
  Result:=
    (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion >= 6);
end;

//--------------------------------------------------------
{
Windows XP icons in Delphi
I want to display Windows XP compatible icons on tool bars in my Delphi application. How do I do that? 
http://discuss.joelonsoftware.com/default.asp?joel.3.8105.4
}
procedure FixImageList32Bit(ImageList: TImageList);
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
var
  TempList: TImageList;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and Assigned(ImageList) then
  begin
    TempList := TImageList.Create(nil);
    try
      TempList.Assign(ImageList);
      with ImageList do
      begin
        Handle := ImageList_Create(Width, Height, ILC_COLOR32 or Mask[Masked],
          0, AllocBy);
        if not HandleAllocated then
          raise EInvalidOperation.Create(SInvalidImageList);
      end;

      Imagelist.AddImages(TempList);
    finally
      FreeAndNil(TempList);
    end;
  end;
end;

//--------------------------------------------------------
{
Creating Windows Vista Ready Applications with Delphi
http://www.installationexcellence.com/articles/VistaWithDelphi/Index.html
}
procedure FixFormFont(AFont: TFont);
var
  LogFont: TLogFont;
begin
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
    AFont.Handle := CreateFontIndirect(LogFont)
  else
    AFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
end;

//--------------------------------------------------------
type
  TSHGetSpecialFolderPathA = function(hwndOwner: HWND; lpszPath: PAnsiChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
  TSHGetSpecialFolderPathW = function(hwndOwner: HWND; lpszPath: PWideChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;

const
  sAppData = '%AppData%';
  CSIDL_APPDATA = $001a;

function FGetAppDataPathA: string;
var
  Handle: THandle;
  Func: TSHGetSpecialFolderPathA;
  Buffer: array[0..MAX_PATH-1] of AnsiChar;
begin
  Result:= '';

  Handle := GetModuleHandle(kernel32);
  if Handle <> 0 then
    begin
    Func:= GetProcAddress(Handle, 'SHGetSpecialFolderPathA');
    if Assigned(Func) then
      begin
      if Func(0, Buffer, CSIDL_APPDATA, true) then
        Result:= AnsiString(Buffer);
      end;
    end;

  if Result = '' then
    Result:= SExpandVars(sAppData);
end;

function FGetAppDataPath: WideString;
begin
  Result:= FGetAppDataPathA;
end;

//--------------------------------------------------------
function SExpandVars(const S: WideString): WideString;
const
  BufSize = 4 * 1024;
var
  BufferA: array[0 .. BufSize - 1] of AnsiChar;
  BufferW: array[0 .. BufSize - 1] of WideChar;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(BufferW, SizeOf(BufferW), 0);
    ExpandEnvironmentStringsW(PWChar(S), BufferW, BufSize);
    Result := WideString(BufferW);
  end
  else
  begin
    FillChar(BufferA, SizeOf(BufferA), 0);
    ExpandEnvironmentStringsA(PAnsiChar(AnsiString(S)), BufferA, BufSize);
    Result := AnsiString(BufferA);
  end;
end;

function SExpanded(const S: WideString): Boolean;
begin
  Result := Pos('%', S) = 0;
end;

procedure SSetEnv(const Name, Value: WideString);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetEnvironmentVariableW(PWChar(Name), PWChar(Value))
  else
    SetEnvironmentVariableA(PAnsiChar(AnsiString(Name)), PAnsiChar(AnsiString(Value)));
end;

//--------------------------------------------------------
procedure SDecodeSearchW(var S: WideString);
const
  DecodeRec: array[1..5] of TStringDecodeRec =
    ((SFrom: '\n'; STo: #13),
     (SFrom: '\r'; STo: #10),
     (SFrom: '\t'; STo: #9),
     (SFrom: '\\'; STo: '\'),
     (SFrom: '\0'; STo: #0));
begin
  S := SDecodeW(S, DecodeRec);
end;

//--------------------------------------------------------
function IsFilenameRemote(const fn: string): Boolean;
begin
  Result := Copy(fn, 1, 2) = '\\';
end;

function IsFilenameFixed(const fn: string): Boolean;
var
  Dir: string;
begin
  Result := false;
  if Pos(':\', fn) = 2 then
  begin
    Dir:= Copy(fn, 1, 3);
    Result:= GetDriveType(PChar(Dir)) = DRIVE_FIXED;
  end;
end;

//--------------------------------------------------------
procedure SetFormStyle(Form: TForm; Value: Boolean);
const
  cNormalStyles = WS_BORDER or WS_DLGFRAME or WS_CAPTION or WS_THICKFRAME;
begin
  with Form do
  begin
    if Value then
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or cNormalStyles)
    else
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not cNormalStyles);

    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOSIZE or SWP_NOMOVE or SWP_FRAMECHANGED);
  end;
end;

function GetFormOnTop(Form: TForm): Boolean;
begin
  with Form do
    Result:= (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOPMOST) <> 0;
end;

procedure SetFormOnTop(Form: TForm; Value: Boolean);
const
  cTopMost: array[Boolean] of DWORD = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  with Form do
    SetWindowPos(Handle, cTopMost[Value], 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);
end;

//--------------------------------------------------------
function FFileWriteStringA(const AFileName: WideString; const S: AnsiString): Boolean;
begin
  if S = '' then
    begin Result := False; Exit end;

  try
    with TTntFileStream.Create(AFileName, fmCreate) do
      try
        WriteBuffer(S[1], Length(S));
        Result := True;
      finally
        Free;
      end;
  except
    Result := False;
  end;
end;

function FFileWriteStringW(const AFileName: WideString; const S: WideString): Boolean;
const
  Sig: WideChar = #$FEFF;
begin
  if S = '' then
    begin Result := False; Exit end;

  try
    with TTntFileStream.Create(AFileName, fmCreate) do
      try
        if S[1] <> Sig then
          WriteBuffer(Sig, 2);
        WriteBuffer(S[1], Length(S) * 2);
        Result := True;
      finally
        Free;
      end;
  except
    Result := False;
  end;
end;


//FN must contain '%d' mask
function FNumberName(const FN: WideString): WideString;
var
  N: Integer;
begin
  N := 0;
  repeat
    Inc(N);
    Result := SFormatWD(FN, [N]);
  until not IsFileExist(Result);
end;

function FTempPath: WideString;
var
  buf: array[0 .. MAX_PATH-1] of AnsiChar;
begin
  FillChar(buf, SizeOf(buf), 0);
  GetTempPathA(SizeOf(buf), buf);
  Result := AnsiString(buf);
end;


function FClipName(const Ext: WideString): WideString;
const
  cName = 'Clipboard';
begin
  Result := FTempPath + cName + Ext;
end;


function FPasteToFile: WideString;
var
  b: TBitmap;
  {$ifdef PNG}
  p: TPngObject;
  {$endif}
  //m: TMetafile;
  SA: AnsiString;
  SW: WideString;
begin
  Result := '';

  try
    if Clipboard.HasFormat(CF_BITMAP) then
    {$ifdef PNG}
    begin
      b := TBitmap.Create;
      p := TPngObject.Create;
      try
        b.Assign(Clipboard);
        p.Assign(b);
        Result := FClipName('.png');
        p.SaveToFile(Result);
      finally
        b.Free;
        p.Free;
      end;
    end
    {$else}
    begin
      b := TBitmap.Create;
      try
        b.Assign(Clipboard);
        Result := FClipName('.bmp');
        b.SaveToFile(Result);
      finally
        b.Free;
      end;
    end
    {$endif}
    (*
    else
    if Clipboard.HasFormat(CF_METAFILEPICT) then
    begin
      m := TMetafile.Create;
      try
        m.Assign(Clipboard);
        Result := FClipName('.wmf');
        m.SaveToFile(Result);
      finally
        m.Free;
      end;
    end
    *)
    else
    if Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      SW := TntClipboard.AsWideText;
      Result := FClipName('.txt');
      if not FFileWriteStringW(Result, SW) then
        raise Exception.Create('');
    end
    else
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      SA := AnsiString(Clipboard.AsText);
      Result := FClipName('.txt');
      if not FFileWriteStringA(Result, SA) then
        raise Exception.Create('');
    end
    else
      MsgWarning(MsgString(127));
  except
    MsgError(MsgString(128));
    Result := '';
  end;
end;

























//----------------------------------------
// 'Name,Size,Color,Style,Charset' --> Font

procedure SSetFont(AFont: TFont; S: string);

  function Item: string;
  var
    N: Integer;
  begin
    N := Pos(',', S);
    if N = 0 then N := MaxInt;
    Result := Copy(S, 1, N - 1);
    Delete(S, 1, N);
  end;

begin
  Assert(Assigned(AFont), 'Font not assigned');
  with AFont do
  begin
    Name := Item;
    Size := StrToIntDef(Item, Size);
    Color := StrToIntDef(Item, Color);
    Style := TFontStyles(byte(StrToIntDef(Item, byte(Style))));
    Charset := TFontCharset(StrToIntDef(Item, Charset));
  end;
end;


function SFontToString(AFont: TFont): string;
begin
  Assert(Assigned(AFont), 'Font not assigned');
  with AFont do
    Result := Format('%s,%d,%d,%d,%d', [Name, Size, Color, byte(Style), integer(CharSet)]);
end;

//-------------------------------------------------
function FormatFileSize(const Size: Int64): string;
const
  Kb = 1024;
  Mb = Kb*Kb;
  Gb = Kb*Kb*Kb;
begin
  if Size >= Gb then
    Result:= Format(MsgString(304), [Size div Gb])
  else
  if Size >= Mb then   
    Result:= Format(MsgString(303), [Size div Mb])
  else
  if Size >= Kb then   
    Result:= Format(MsgString(302), [Size div Kb])
  else
    Result:= Format(MsgString(301), [Size]);
end;

function FormatFileTime(const ft: TFileTime): string;
var
  lt: TFileTime;
  st: TSystemTime;
  Date, Time: string;
begin
  Result:= '';
  if FileTimeToLocalFileTime(ft, lt) and FileTimeToSystemTime(lt, st) then
    begin
    Date:= Format('%2.2d%s%2.2d%s%d',
      [st.wDay, DateSeparator, st.wMonth, DateSeparator, st.wYear]);

    Time:= Format('%2.2d%s%2.2d%s%2.2d',
      [st.wHour, TimeSeparator, st.wMinute, TimeSeparator, st.wSecond]);

    Result:= Date + ' ' + Time;
    end;
end;

//----------------------------------------
function FSearchDir(const sname, sdir: string; var fn: string): boolean;
var
  h: THandle;
  fd: TWin32FindData;
begin
  if not IsDirExist(sdir) then begin Result:= false; Exit end;

  h:= FindFirstFile(PChar(sdir+'\'+sname), fd);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then 
    begin fn:= sdir+'\'+fd.cFileName; Windows.FindClose(h); Exit end;

  h:= FindFirstFile(PChar(sdir+'\*.*'), fd);
  if h=INVALID_HANDLE_VALUE then Exit;
  
  repeat
    if ((fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0) and
      (fd.cFileName[0]<>'.') then
        begin
        Result:= FSearchDir(sname, sdir+'\'+fd.cFileName, fn);
        if Result then Break;
        end;

  until not FindNextFile(h, fd);

  Windows.FindClose(h);
end;




end.
