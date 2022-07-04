unit fmblob;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IniFiles, Buttons, ExtCtrls, ComCtrls, SynEdit, SynMemo,
  ActnList, StdCtrls, DBCtrls;

type
  Tfblob = class(TForm)
    Panel2: TPanel;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    Status: TStatusBar;
    Panel1: TPanel;
    pahex: TPanel;
    hexa: TSynMemo;
    actions: TActionList;
    aHex: TAction;
    aText: TAction;
    parich: TPanel;
    rich: TDBMemo;
    ds: TDataSource;
    paimg: TPanel;
    img: TDBImage;
    SpeedButton1: TSpeedButton;
    aImg: TAction;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    aSave: TAction;
    aLoad: TAction;
    SaveD: TSaveDialog;
    OpenD: TOpenDialog;
    aRun: TAction;
    SpeedButton4: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure hexaGutterGetText(Sender: TObject; aLine: Integer; var aText: String);
    procedure aHexExecute(Sender: TObject);
    procedure aTextExecute(Sender: TObject);
    procedure aImgExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aLoadExecute(Sender: TObject);
    procedure aRunExecute(Sender: TObject);
  private
    { Private declarations }
    field: TBlobField;
    procedure LoadIni;
    procedure SaveIni;
    procedure ReadHex;
    function ShowBlob(blob: TBlobField): Boolean;
  public
    { Public declarations }
  end;

function blob_window(blob: TBlobField): Boolean;

implementation

uses
  udm, shellapi, fmext;

{$R *.dfm}

const
  hex_count = 32768;

function blob_window(blob: TBlobField): Boolean;
begin
  Result := false;
  if Assigned(blob) then
  with Tfblob.Create(Application) do
  Result := ShowBlob(blob);
end;

procedure Tfblob.FormCreate(Sender: TObject);
begin
  field := nil;
  LoadIni;
  pahex.Align := alClient;
  parich.Align := alClient;
  paimg.Align := alClient;
  aHex.Execute;
end;

procedure Tfblob.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveIni;
  Action := caFree;
end;

procedure Tfblob.LoadIni;
var
  f: TIniFile;
begin
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    Left := f.ReadInteger('BLOBWIN', 'left', (Screen.Width - Width) div 2);
    Top := f.ReadInteger('BLOBWIN', 'top', (Screen.Height - Height) div 2);
    Width := f.ReadInteger('BLOBWIN', 'width', Width);
    Height := f.ReadInteger('BLOBWIN', 'heigth', Height);
  finally
    f.Free;
  end;
end;

procedure Tfblob.SaveIni;
var
  f: TIniFile;
begin
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    f.WriteInteger('BLOBWIN', 'left', Left);
    f.WriteInteger('BLOBWIN', 'top', Top);
    f.WriteInteger('BLOBWIN', 'width', Width);
    f.WriteInteger('BLOBWIN', 'heigth', Height);
  finally
    f.Free;
  end;
end;

function Tfblob.ShowBlob(blob: TBlobField): Boolean;
begin
  field := blob;
  ds.DataSet := field.DataSet;
  Caption := ' Blob field: ' + field.FieldName;
  ReadHex;
  Result := ShowModal = mrOk;
end;

procedure Tfblob.ReadHex;
var
  i, n, j: Integer;
  c: AnsiChar;
  p: AnsiString;
  a: Array[0 .. 1] of AnsiChar;
  l, s, e: String;
begin
  hexa.Clear;
  n := field.BlobSize;
  aRun.Enabled := n > 0;
  Status.Panels[0].Text := '  blob size = ' + IntToStr(n) + ' bytes';
  if n < 1 then Exit;

  e := '';
  if n > hex_count then
  begin
    e := 'etc...';
    n := hex_count;
  end;

  p := AnsiString(field.AsString);

  l := '';
  s := '';
  j := 0;
  for i := 1 to n do
  begin
    c := p[i];
    BinToHex(@c, a, 1);
    l := l + String(a) + ' ';
    if ord(c) < 32 then
    s := s + '.'
    else s := s + Char(c);
    inc(j, 1);
    if j >= 16 then
    begin
      hexa.Lines.Add(l + '  ' + s);
      l := '';
      s := '';
      j := 0;
    end;
  end;

  if j > 0 then
  hexa.Lines.Add(str_fixlen(l, 16 * 3) + '  ' + s);
  hexa.Lines.Add(e);
end;

procedure Tfblob.hexaGutterGetText(Sender: TObject; aLine: Integer; var aText: String);
begin
  aText := IntToStr((aLine - 1) * 16);
end;

procedure Tfblob.aHexExecute(Sender: TObject);
begin
  parich.Hide;
  paimg.Hide;
  pahex.Show;
end;

procedure Tfblob.aTextExecute(Sender: TObject);
begin
  pahex.Hide;
  paimg.Hide;
  try
    try
      if rich.Visible then
      rich.DataField := field.FieldName;
    except
      rich.Hide;
      parich.Caption := 'No valid text.';
    end;
  finally
    parich.Show;
  end;
end;

procedure Tfblob.aImgExecute(Sender: TObject);
begin
  pahex.Hide;
  parich.Hide;
  try
    try
      if img.Visible then
      img.DataField := field.FieldName;
    except
      img.Hide;
      paimg.Caption := 'No valid bitmap.';
    end;
  finally
    paimg.Show;
  end;
end;

procedure Tfblob.aSaveExecute(Sender: TObject);
begin
  if SaveD.Execute then
  field.SaveToFile(SaveD.FileName);
end;

procedure Tfblob.aLoadExecute(Sender: TObject);
begin
  if OpenD.Execute then
  begin
    img.DataField := '';
    field.DataSet.Edit;
    field.LoadFromFile(OpenD.FileName);
    field.DataSet.Post;
    ReadHex;
  end;
end;

procedure Tfblob.aRunExecute(Sender: TObject);
var
  s: String;
begin
  if GetExtension(Self, s) then
  begin
    s := '.' + s;
    while pos('..', s) = 1 do
    Delete(s, 1, 1);
    s := get_work_folder_file('blob_tmp' + s);
    field.SaveToFile(s);
    Status.Panels[1].Text := 'Saved to: ' + s;
    ShellExecute(0, 'open', PChar(s), nil, nil, SW_NORMAL);
  end;
end;

end.
