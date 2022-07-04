unit Unit1;

interface

  { Delphi 6/7/2005/2006/2007 } //todo: 2009

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  {$IF CompilerVersion < 18.50}
  DBXpress,
  {$ELSE}
  DBXCommon,
  {$IFEND}
  Dialogs, DB, SqlExpr, StdCtrls,
  dbx_ora_connect, WideStrings, ExtCtrls;

const
  ora_tns_name = 'TNS_DBDEMOS';
  ora_user_name = 'scott'; // scott
  ora_user_pswd = 'tiger'; // tiger

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    TTNS: TLabel;
    ETNS: TEdit;
    LUSER: TLabel;
    EUSER: TEdit;
    LPWD: TLabel;
    EPWD: TEdit;
    LDNS: TLabel;
    EDNS: TEdit;
    CMSDriver: TCheckBox;
    CDirectOdbc: TCheckBox;
    BConnect: TButton;
    BDisconnect: TButton;
    LExample_oracle: TLabel;
    LExample_microsoft: TLabel;
    sh1: TShape;
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.BConnectClick(Sender: TObject);
begin

  if ETNS.Text = '' then
    raise Exception.Create('It is necessary to set TNS.'
      + #13#10'TNS must be indicated in a file like "C:\Oracle\9.2.0.6\network\ADMIN\tnsnames.ora"');


  dbx_ora_connect.OracleConnect(
    SQLConnection,
    {TNS=} ETNS.Text,
    {User=} EUSER.Text,
    {Password=} EPWD.Text,
    {MicrosoftDriver=} CMSDriver.Checked,
    {DirectOdbc=} CDirectOdbc.Checked,
    {LoginPrompt=} EUSER.Text = '',
    {DNS=}EDNS.Text
  );
end;

procedure TForm1.BDisconnectClick(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  bIsOracle, bIsMicrosoft: Boolean;
begin
  bIsOracle := IsPresentedOracleDriver();
  CMSDriver.Checked := not bIsOracle;
  bIsMicrosoft := IsPresentedMicrosoftOracleDriver();
  CDirectOdbc.Checked := bIsOracle or bIsMicrosoft;

  {TNS=} ETNS.Text := ora_tns_name;
  {User=} EUSER.Text := ora_user_name;
  {Password=} EPWD.Text := ora_user_pswd;
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

end.
