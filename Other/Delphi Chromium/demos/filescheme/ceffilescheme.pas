(*
 *                       Delphi Chromium Embedded
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *)

{$IFDEF FPC}
   {$MODE DELPHI}{$H+}
{$ENDIF}

unit ceffilescheme;
{$R 'ceffilescheme.res'}
{$WARN SYMBOL_PLATFORM OFF}

interface
uses ceflib, Classes;

type
  TFileScheme = class(TCefSchemeHandlerOwn)
  private
    FPath: string;
    FDataStream: TStream;
    FStatus: Integer;
    FStatusText: string;
    FMimeType: string;
  protected
    function ProcessRequest(const Request: ICefRequest; var redirectUrl: ustring;
      const callback: ICefSchemeHandlerCallback): Boolean; override;
    procedure GetResponseHeaders(const response: ICefResponse; var responseLength: Int64); override;
    function ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean; override;
    procedure Cancel; override;
  public
    constructor Create(SyncMainThread: Boolean; const scheme: ustring;
      const browser: ICefBrowser; const request: ICefRequest); override;
    destructor Destroy; override;
  end;

implementation
uses Windows, SysUtils, Registry, Contnrs;

function Escape(const str: ustring): string;
var
  p: PWideChar;
begin
  Result := '';
  p := PWideChar(str);
  while p^ <> #0 do
  begin
    if Ord(p^) > 255 then
      Result := Result + '\u' + IntToHex(Ord(p^), 4) else
      if (AnsiChar(p^) in ['\', '"']) then
        Result := Result + '\' + p^ else
        Result := Result + p^;
    inc(p);
  end;
end;

type
  TFileInfo = class
    name: ustring;
    size: Int64;
    date: TFileTime;
    attr: Integer;
    constructor Create(const aname: ustring; asize: Int64; adate: TFileTime; aattr: Integer);
    function DisplaySize: string;
    function DisplayDate: string;
    function Display: string;
  end;

  constructor TFileInfo.Create(const aname: ustring; asize: Int64; adate: TFileTime; aattr: Integer);
  begin
    name := aname;
    size := asize;
    date := adate;
    attr := aattr;
  end;

  function TFileInfo.DisplaySize: string;
  const
    s: array[0..5] of Int64 = (
      1024,
      1048576,
      1073741824,
      1099511627776,
      1125899906842624,
      1152921504606846976);
  begin
    if size < S[0] then Result := Format('%d B', [size]) else
    if size < S[1] then Result := Format('%0.1f kB', [size/S[0]]) else
    if size < S[2] then Result := Format('%0.1f MB', [size/S[1]]) else
    if size < S[3] then Result := Format('%0.1f GB', [size/S[2]]) else
    if size < S[4] then Result := Format('%0.1f TB', [size/S[3]]) else
    if size < S[5] then Result := Format('%0.1f PB', [size/S[4]]) else
      Result := 'big';
  end;

  function TFileInfo.DisplayDate: string;
  var
    local: TFileTime;
    sys: TSystemTime;
    dt: TDateTime;
  begin
    FileTimeToLocalFileTime(date, local);
    FileTimeToSystemTime(local, sys);
    with sys do
      dt := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    Result := DateTimeToStr(dt);
  end;

  function TFileInfo.Display: string;
  var
    d: Integer;
  begin
    if attr and faDirectory = faDirectory then
      d := 1 else d := 0;
    Result := Format('<script>addRow("%s","%0:s",%d,"%s","%s");</script>'#13#10, [Escape(name), d, DisplaySize, DisplayDate])
  end;

  function FileSortCompare(Item1, Item2: TFileInfo): Integer;
  begin
    if (Item1.attr and faDirectory = faDirectory) then
    begin
      if (Item2.attr and faDirectory = faDirectory) then
        Result := CompareText(Item1.name, Item2.name) else
        Result := -1;
    end else
      if (Item2.attr and faDirectory <> faDirectory) then
        Result := CompareText(Item1.name, Item2.name) else
        Result := 1;
  end;

function HTTPDecode(const AStr: ustring): rbstring;
var
  Sp, Rp, Cp: PAnsiChar;
  src: rbstring;
begin
  src := rbstring(AStr);
  SetLength(Result, Length(src));
  Sp := PAnsiChar(src);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Inc(Sp);
             if Sp^ = '%' then
               Rp^ := '%'
             else
             begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then
                 Rp^ := AnsiChar(StrToInt('$' + Char(Cp^) + Char(Sp^)))
               else
               begin
                 Result := '';
                 Exit;
               end;
             end;
           end;
    else
      Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

function ParseFileUrl(const url: ustring): ustring;
label
  error;
var
  pos: Integer;
  p: PWideChar;
begin
  pos := 0;
  p := PWideChar(url);
  while True do
  begin
    case pos of
      0: if p^ <> 'f' then goto error;
      1: if p^ <> 'i' then goto error;
      2: if p^ <> 'l' then goto error;
      3: if p^ <> 'e' then goto error;
      4: if p^ <> ':' then goto error;
      5: if p^ <> '/' then goto error;
      6: if p^ <> '/' then goto error;
      7: if p^ = '/' then
           begin
             Inc(p);
             {$IFDEF UNICODE}
               Exit(StringReplace(UTF8ToString(HTTPDecode(p)), '/', '\', [rfReplaceAll]));
             {$ELSE}
               Result := StringReplace(UTF8Decode(HTTPDecode(p)), '/', '\', [rfReplaceAll]);
               Exit;
             {$ENDIF}
           end else
             {$IFDEF UNICODE}
               Exit('\\' + StringReplace(UTF8ToString(HTTPDecode(p)), '/', '\', [rfReplaceAll]));
             {$ELSE}
               begin
                 Result := '\\' + StringReplace(UTF8Decode(HTTPDecode(p)), '/', '\', [rfReplaceAll]);
                 Exit;
               end;
             {$ENDIF}

    end;
    Inc(p);
    Inc(pos);
  end;
error:
  Result := '';
end;

{ TFileScheme }

procedure TFileScheme.Cancel;
begin
  inherited;

end;

constructor TFileScheme.Create(SyncMainThread: Boolean; const scheme: ustring;
  const browser: ICefBrowser; const request: ICefRequest);
begin
  inherited;
  FDataStream := nil;
end;

destructor TFileScheme.Destroy;
begin
  if FDataStream <> nil then
    FDataStream.Free;
  inherited;
end;

procedure TFileScheme.GetResponseHeaders(const response: ICefResponse;
  var responseLength: Int64);
begin
  response.Status := FStatus;
  response.StatusText := FStatusText;
  response.MimeType := FMimeType;
  responseLength := FDataStream.Size;
end;

function TFileScheme.ProcessRequest(const Request: ICefRequest;
  var redirectUrl: ustring; const callback: ICefSchemeHandlerCallback): Boolean;
var
  rec: TSearchRec;
  reg: TRegistry;
  Items: TObjectList;
  i: Integer;
  rc: TResourceStream;

  procedure OutPut(const str: string);
  {$IFDEF UNICODE}
  var
    rb: rbstring;
  {$ENDIF}
  begin
  {$IFDEF UNICODE}
    rb := rbstring(str);
    FDataStream.Write(rb[1], Length(rb))
  {$ELSE}
    FDataStream.Write(str[1], Length(str))
  {$ENDIF}
  end;

  procedure OutputUTF8(const str: string);
  var
    rb: rbstring;
  begin
  {$IFDEF UNICODE}
    rb := utf8string(str);
  {$ELSE}
    rb := UTF8Encode(str);
  {$ENDIF}
    FDataStream.Write(rb[1], Length(rb))
  end;

begin
  Result := True;

  FPath := ParseFileUrl(Request.Url);
  if FindFirst(FPath, 0, rec) = 0 then
  begin
    FStatus := 200;
    FStatusText := 'OK';
    FindClose(rec);

    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      if reg.OpenKey(ExtractFileExt(FPath), False) then
        FMimeType := reg.ReadString('Content Type') else
        if LowerCase(ExtractFileExt(FPath)) = '.pdf' then
          FMimeType := 'application/pdf';
    finally
      reg.Free;
    end;
    if FMimeType = '' then
      FMimeType := 'application/octet-stream';
    FDataStream := TFileStream.Create(FPath, fmOpenRead or fmShareDenyNone);
  end else
  if DirectoryExists(FPath) then
  begin
    FStatus := 200;
    FStatusText := 'OK';
    Items := TObjectList.Create(True);
    try
      FPath := IncludeTrailingPathDelimiter(FPath);
      FDataStream := TMemoryStream.Create;
      rc := TResourceStream.Create(HInstance, 'CEFFILESCHEMEDIR', RT_RCDATA);
      try
        rc.SaveToStream(FDataStream);
      finally
        rc.Free;
      end;

      OutPut(Format('<script>start("%s");</script>'#13#10, [escape(FPath)]));

      if FindFirst(FPath + '*.*', faAnyFile, rec) = 0 then
      begin
        repeat
          if rec.Name <> '.' then
            Items.Add(TFileInfo.Create(rec.Name, rec.Size, rec.FindData.ftLastWriteTime, rec.Attr));
        until FindNext(rec) <> 0;
        FindClose(rec);
      end;
      Items.Sort(@FileSortCompare);

      for i := 0 to Items.Count - 1 do
        OutPut(TFileInfo(Items[i]).Display);

      FDataStream.Seek(0, soFromBeginning);
      FMimeType := 'text/html';
    finally
      Items.Free;
    end;
  end else
  begin
    FStatus := 404;
    FStatusText := 'Not found';

    // error
    FDataStream := TMemoryStream.Create;

    OutputUTF8('<html><head><meta http-equiv="content-type" content="text/html; '+
      'charset=UTF-8"/></head><body><h1>'+ FPath+'</h1><h2>not found</h2></body></html>');
    FMimeType := 'text/html';
    FDataStream.Seek(0, soFromBeginning);
  end;

  callback.HeadersAvailable;
  callback.BytesAvailable;
end;

function TFileScheme.ReadResponse(DataOut: Pointer; BytesToRead: Integer;
      var BytesRead: Integer; const callback: ICefSchemeHandlerCallback): Boolean;
begin
  BytesRead := FDataStream.Read(DataOut^, BytesToRead);
  Result := True;
end;

end.
