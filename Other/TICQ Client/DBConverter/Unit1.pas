unit Unit1;

interface

uses
  Windows, Messages, Classes, Controls, Forms,
  StdCtrls, ICQDb, ICQWorks, ComCtrls, SysUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    ICQDb1: TICQDb;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    StatusBar1: TStatusBar;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    ListView1: TListView;
    procedure ICQDb1Error(Sender: TObject; Reason: Word; ReasonStr: String);
    procedure Button1Click(Sender: TObject);
    procedure ICQDb1ParsingFinished(Sender: TObject);
    procedure ICQDb1ParsingStarted(Sender: TObject);
    procedure ICQDb1Progress(Sender: TObject; Progress: Byte);
    procedure ICQDb1ContactFound(Sender: TObject; UIN: Cardinal; NickName,
      FirstName, LastName, Email: String; Age, Gender: Byte;
      LastUpdate: String; LastUpdateStamp: Cardinal);
    procedure ICQDb1MessageFound(Sender: TObject; UIN: Cardinal;
      Incoming: Boolean; Msg, RecvTime: String; RecvTimeStamp: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure ICQDb1SelfInfoFound(Sender: TObject; UIN: Cardinal; NickName,
      FirstName, LastName, Email, Password: String; Age, Gender: Byte;
      LastUpdate: String; LastUpdateStamp: Cardinal);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

function GetIcqFiles(DbPath: String; var FList: TStringList): Boolean;
function GetMirandaFiles(var FList: TStringList): Boolean;

implementation

{$R *.dfm}

procedure TForm1.ICQDb1Error(Sender: TObject; Reason: Word; ReasonStr: String);
begin
  MessageBox(0, PChar('Error: ' + ReasonStr), 'Error', MB_ICONERROR);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;  
  if ListView1.Selected = nil then
  begin
    MessageBox(0, 'Please select database', 'Error', MB_ICONERROR);
    Exit;
  end;
  ICQDb1.DatFile := ListView1.Selected.Caption + '.dat';
  ICQDb1.StartParsing;
end;

procedure TForm1.ICQDb1ParsingFinished(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Parsing finished';
end;

procedure TForm1.ICQDb1ParsingStarted(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Parsing started';
  Application.ProcessMessages;
end;

procedure TForm1.ICQDb1Progress(Sender: TObject; Progress: Byte);
begin
  ProgressBar1.Position := Progress;
  Application.ProcessMessages;  
end;

procedure TForm1.ICQDb1ContactFound(Sender: TObject; UIN: Cardinal;
  NickName, FirstName, LastName, Email: String; Age, Gender: Byte;
  LastUpdate: String; LastUpdateStamp: Cardinal);
begin
  if NickName <> '' then
    Memo1.Lines.Add(IntToStr(UIN) + ' (' + NickName + ')')
  else
    Memo1.Lines.Add(IntToStr(UIN));
end;

procedure TForm1.ICQDb1MessageFound(Sender: TObject; UIN: Cardinal;
  Incoming: Boolean; Msg, RecvTime: String; RecvTimeStamp: Cardinal);
begin
  if Incoming then
    Memo2.Lines.Add('From ' + IntToStr(UIN) + ': ' + Msg)
  else
    Memo2.Lines.Add('To ' + IntToStr(UIN) + ': ' + Msg);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  RegKeyHandle: HKEY;
  StrBuffer: array[0..2047] of Char;
  DataType, BufSize: Integer;
  DbPaths: TStringList;
  i, n: Word;
  l: TStringList;
  item: TListItem;

  procedure QueryReg(Where: HKEY);
  begin
    if (RegOpenKey(Where, PChar('SOFTWARE\Mirabilis\ICQ\DefaultPrefs'), RegKeyHandle) = ERROR_SUCCESS) then
    begin
      if RegQueryValueEx(RegKeyHandle, PChar('99b Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
      if RegQueryValueEx(RegKeyHandle, PChar('2000a Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
      if RegQueryValueEx(RegKeyHandle, PChar('2000b Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
      if RegQueryValueEx(RegKeyHandle, PChar('2001a Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
      if RegQueryValueEx(RegKeyHandle, PChar('2002a Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
      if RegQueryValueEx(RegKeyHandle, PChar('2003a Database'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS then
         DbPaths.Add(Copy(StrBuffer, 0, BufSize));
    end;
    RegCloseKey(RegKeyHandle);
  end;
begin
  DbPaths := TStringList.Create;
  {Find Miranda-icq database files.}
  if GetMirandaFiles(DbPaths) then
    for i := 0 to DbPaths.Count - 1 do
    begin
      item := ListView1.Items.Add;
      item.Caption := DbPaths.Strings[i];
    end;
  DbPaths.Clear;
  {Find ICQ database files.}
  QueryReg(HKEY_LOCAL_MACHINE);
  QueryReg(HKEY_CURRENT_USER);
  if DbPaths.Count > 0 then
     for i := 0 to DbPaths.Count - 1 do
     begin
       l := TStringList.Create;
       GetIcqFiles(DbPaths.Strings[i],  l);
       if l.Count > 0 then
         for n := 0 to l.Count - 1 do
         begin
          item := ListView1.Items.Add;
          item.Caption := DbPaths.Strings[i] + '\' + l.Strings[n];
         end;
       l.Free;
     end;
  DbPaths.Free;
  if ListView1.Items.Count < 1 then
    MessageBox(Form1.Handle, 'Sorry no database files were found in your system, please set the paths manually', 'Error', MB_ICONERROR);
end;

function GetIcqFiles(DbPath: String; var FList: TStringList): Boolean;
var
  fd: TWin32FindData;
  hs: THandle;
begin
  FList.Clear;
  fd.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
  hs := FindFirstFile(PChar(DbPath + '\*.*'), fd);
  if hs <> INVALID_HANDLE_VALUE then
  begin
    repeat
      if AnsiLowerCase(Copy(fd.cFileName, LastDelimiter('.', fd.cFileName) + 1, Length(fd.cFileName) - LastDelimiter('.', fd.cFileName))) = 'dat' then
        FList.Add(Copy(fd.cFileName, 0, Pos('.', fd.cFileName) - 1));
    until not FindNextFile(hs, fd);
    Windows.FindClose(hs);
  end;
  Result := FList.Count > 0;
end;

function GetMirandaFiles(var FList: TStringList): Boolean;
var
  fd: TWin32FindData;
  hs: THandle;
  Path: String;
  RegKeyHandle: HKEY;
  StrBuffer: array[0..2048] of Char;
  DataType, BufSize: Integer;
begin
  Path := ''; Result := False;
  DataType := REG_SZ; BufSize := SizeOf(StrBuffer) - 1;
  if (RegOpenKey(HKEY_LOCAL_MACHINE, PChar('SOFTWARE\Miranda'), RegKeyHandle) = ERROR_SUCCESS) and
     (RegQueryValueEx(RegKeyHandle, PChar('Install_Dir'), nil, @DataType, PByte(@StrBuffer), @BufSize) = ERROR_SUCCESS) then
       Path := StrBuffer;
  RegCloseKey(RegKeyHandle);
  {Exit if there is no install_dir found}
  if Path = '' then
    Exit;

  fd.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
  hs := FindFirstFile(PChar(Path + '\*.*'), fd);
  if hs <> INVALID_HANDLE_VALUE then
  begin
    repeat
      if AnsiLowerCase(Copy(fd.cFileName, LastDelimiter('.', fd.cFileName) + 1, Length(fd.cFileName) - LastDelimiter('.', fd.cFileName))) = 'dat' then
      begin
        FList.Add(Path + '\' + Copy(fd.cFileName, 0, LastDelimiter('.', fd.cFileName) - 1));
        Result := True;
      end;
    until not FindNextFile(hs, fd);
    Windows.FindClose(hs);
  end;
end;

procedure TForm1.ICQDb1SelfInfoFound(Sender: TObject; UIN: Cardinal;
  NickName, FirstName, LastName, Email, Password: String; Age,
  Gender: Byte; LastUpdate: String; LastUpdateStamp: Cardinal);
begin
  Memo3.Lines.Add('UIN: ' + IntToStr(UIN));
  Memo3.Lines.Add('NickName: ' + NickName);
  Memo3.Lines.Add('FirstName: ' + FirstName);
  Memo3.Lines.Add('LastName: ' + LastName);
  Memo3.Lines.Add('Email: ' + Email);
  Memo3.Lines.Add('Password: ' + Password);
  Memo3.Lines.Add('Last update: ' + LastUpdate);
end;

end.
