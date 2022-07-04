///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMWebIniFile;

{$B-}

interface

uses INIFiles;

type
  TWebINIFile=class(TMemIniFile)
  private
    FString: string;
  public
    constructor Create(const FileName: string);
    procedure UpdateFile; override;
    function ReadSectionText(Section: string): string;
  end;
  
implementation

uses Windows, Classes, SysUtils, WinInet;

{ TWebINIFile }

constructor TWebINIFile.Create(const FileName: string);
var
  List: TStringList;
  hSession, hFile: HINTERNET;
  URL: array[0..1024] of char;
  Buf: array[0..1024] of byte; 
  ByteRead: DWORD;
begin
  inherited Create(''); // disable LoadValues from disk file
  FString:='';
  // check whether filename is ftp or http URL
  if Pos('TP:', UpperCase(FileName))>0 then
  begin // download file to Str as in other DM2003 units
    StrPCopy(URL, FileName);
    hSession:=InternetOpen('DM2003', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession<>nil then
    try
      hFile:=InternetOpenUrl(hSession, URL, nil, 0, INTERNET_FLAG_NO_UI, 0);
      if hFile<>nil then
      try
        repeat
          if not InternetReadFile(hFile, @Buf, SizeOf(Buf)-1, ByteRead) 
          then break;
          Buf[ByteRead]:=0; // make pchar
          FString:=FString+StrPas(PChar(@Buf));
        until ByteRead=0;
      finally
        InternetCloseHandle(hFile);
      end;
    finally
      InternetCloseHandle(hSession);
    end;
  end;
  if FString=''
  then Exit; // no data!
  List:=TStringList.Create; // parse FString
  try
    List.Text:=FString;
    SetStrings(List);
  finally
    List.Free;
  end;
end;

function TWebINIFile.ReadSectionText(Section: string): string;
var 
  List: TStringList;
  Reading: boolean; 
  S,S1,S2: string;
  I: integer;
begin
  Result:='';
  List:=TStringList.Create; // parse FString
  try
    List.Text:=FString;
    // as in TDMINIFile.ReadSection
    S2:='';
    Reading:=false;
    for I:=0 to List.Count-1 do
    begin
      S:=List[I]; 
      S1:=Trim(S);
      if Reading and (S1<>'') and (S1[1]='[') then Break; // next section?
      if Reading then S2:=S2+S+#13#10;
      if UpperCase(S1)='['+UpperCase(Section)+']' then Reading:=true;
    end;
    Result:=S2;
  finally
    List.Free;
  end;
end;

procedure TWebINIFile.UpdateFile;
begin
  // intentionally does nothing: TWebINIFile is always read-only
end;

end.
 