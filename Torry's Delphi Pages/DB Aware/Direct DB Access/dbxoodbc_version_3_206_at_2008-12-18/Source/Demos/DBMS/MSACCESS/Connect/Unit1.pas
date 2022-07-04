unit Unit1;

interface

  { Delphi 6/7/2005/2006/2007 }

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, SqlExpr, ExtCtrls, StdCtrls,
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  dbx_access_connect;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    LUSER: TLabel;
    EUSER: TEdit;
    LPWD: TLabel;
    EPWD: TEdit;
    LDNS: TLabel;
    EDNS: TEdit;
    CDirectOdbc: TCheckBox;
    BConnect: TButton;
    BDisconnect: TButton;
    LAdd: TLabel;
    EAdditional: TEdit;
    LDB: TLabel;
    EMDBFILENAME: TEdit;
    lbl1: TLabel;
    btn_mdb_load: TButton;
    sh1: TShape;
    CUnicodeDriver: TCheckBox;
    OD: TOpenDialog;
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_mdb_loadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IF CompilerVersion >= 18.00}
  {$IF CompilerVersion >= 18.50}
  CUnicodeDriver.Checked := True;
  CUnicodeDriver.Enabled := False;
  {$ELSE}
  CUnicodeDriver.Checked := True;
  CUnicodeDriver.Enabled := True;
  {$IFEND}
  {$ELSE}
  CUnicodeDriver.Checked := False;
  CUnicodeDriver.Enabled := False;
  {$IFEND}
  EMDBFILENAME.Text := ExtractFilePath(ParamStr(0)) + 'dbdemos.mdb';
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.BConnectClick(Sender: TObject);
begin
  if (not CDirectOdbc.Enabled) and (EDNS.Text = '') then
    raise Exception.Create('It is necessary to set DNS (DataSource Name).');

(*

procedure AccessConnect(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = ''
);

procedure AccessConnectW(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = ''
);
*)

  if CUnicodeDriver.Checked then
    dbx_access_connect.AccessConnectW(SQLConnection,
      EMDBFILENAME.Text, EDNS.Text,
      CDirectOdbc.Enabled,
      EUser.Text <> '', EUser.Text, EAdditional.Text)
  else
    dbx_access_connect.AccessConnect(SQLConnection,
      EMDBFILENAME.Text, EDNS.Text,
      CDirectOdbc.Enabled,
      EUser.Text <> '', EUser.Text, EAdditional.Text);

end;

procedure TForm1.BDisconnectClick(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh1.Brush.Color := clRed;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    sh1.Brush.Color := clGray;
end;


procedure TForm1.btn_mdb_loadClick(Sender: TObject);
begin
  if OD.Execute then
  begin
    EMDBFILENAME.Text := OD.FileName;
    SQLConnection.Connected := False;
  end;
end;

end.
