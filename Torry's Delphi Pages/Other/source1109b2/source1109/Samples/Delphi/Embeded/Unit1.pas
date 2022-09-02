Unit Unit1;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Db,
  Stdctrls,
  ExtCtrls,
  DBCtrls,
  Grids,
  DBGrids,
  fsllprot,
  fsdb,
  fsllcomp,
  fsllcomm,
  fslllgcy,
  fssrbase,
  fsllbase,
  fsdbbase,
  fslleng,
  fssrintm,
  fslogdlg,
  Spin,
  ComCtrls, fsserverclass;

Type
  TForm1 = Class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    DataSource1: TDataSource;
    FSSession1: TFSSession;
    FSDatabase1: TFSDatabase;
    FSTable1: TFSTable;
    FSClient1: TFSClient;
    Button2: TButton;
    Label4: TLabel;
    FSServer1: TFSServer;
    Edit4: TEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    ListBox1: TListBox;
    Memo1: TMemo;
    spdundel: TButton;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FSClient1ConnectionLost(aSource: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    procedure spdundelClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  Form1: TForm1;

Implementation

{$R *.DFM}
Uses fschangeport;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  fsSession1.Active := False;
  FSSession1.Passwords.Assign(Memo1.Lines);
  fsClient1.Active := False;
  ListBox1.Items.Clear;
  FSDatabase1.AliasName := Edit4.text;
  fsClient1.Active := True;
  If fsClient1.Active Then
    Begin
      FSDatabase1.Open;
      fsSession1.GetTableNames(FSDatabase1.DataBaseName, '', False, False, ListBox1.Items);
    End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  ListBox1.Items.Clear;
  fsSession1.Active := False;
  fsClient1.Active := False;
End;

Procedure TForm1.FSClient1ConnectionLost(aSource: TObject);
Resourcestring
  cMsg = 'The connection to the server has been lost!';
Begin
  MessageDlg(cMsg, mtError, [mbOk], 1);
  Try
    FSTable1.Close;
  Except
    //application.Terminate;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  edit4.text := ExtractFilePath(ParamStr(0));
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Begin
  FSTable1.Close;
  If ListBox1.ItemIndex >= 0 Then
    Begin
      FSTable1.TableName := ListBox1.Items[ListBox1.ItemIndex];
      FSTable1.Open;
      spdundel.enabled := FsTable1.Dictionary.EngineDeleteType <> edtNotUndelete;
    End;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  spdundel.enabled := FsTable1.Dictionary.EngineDeleteType <> edtNotUndelete;
End;

procedure TForm1.spdundelClick(Sender: TObject);
begin
  FSTable1.Undelete(true);
end;

End.

