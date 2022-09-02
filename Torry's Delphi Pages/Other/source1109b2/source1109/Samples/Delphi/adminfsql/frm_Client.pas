Unit frm_Client;

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
  Stdctrls,
  fsAdminPlug,
  fssrbase,
  fssrcfg,
  ExtCtrls,
  fslleng,
  fssrintm,
  fsllcomm,
  fssrcmd,
  fssrsec,
  fslllgcy,
  fsllbase,
  fsllcomp,
  fslllog,
  fssrbde, fsserverremoteclass;

Type
  TfrmClient = Class(TForm)
    btnUsers: TButton;
    btnConf: TButton;
    btnSingle: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edtName: TEdit;
    edtPasswd: TEdit;
    RemoteAdmin: TFSRemoteAdminServer;
    Transport: TFSParamConnect;
    Monitor: TFSMonitor;
    Handler: TFSHandler;
    ServerEngine: TFSRemoteServer;
    EventLog: TFSEventLog;
    Procedure FormShow(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure btnUsersClick(Sender: TObject);
    Procedure btnConfClick(Sender: TObject);
    Procedure btnSingleClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
  Public
    { Public declarations }
  End;

Var
  frmClient: TfrmClient;

Implementation

{$R *.DFM}

Uses frm_Users,
  frm_GenInfo,
  frm_Single;

Procedure TfrmClient.FormShow(Sender: TObject);
Begin
  Transport.Startup;
  ServerEngine.Startup;
End;

Procedure TfrmClient.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  ServerEngine.Shutdown;
  Transport.Shutdown;
End;

Procedure TfrmClient.btnUsersClick(Sender: TObject);
// batched user configuration
Var
  myPlugin: TfsRemoteAdminServer;
Begin
  // setup Admin plugin
  myPlugin := RemoteAdmin;
  myPlugin.Username := edtName.Text;
  myPlugin.Password := edtPasswd.Text;
  myPlugin.Start;
  With TfrmUsers.Create(Application) Do
    Begin
      Try
        // show user admin form
        Plugin := myPlugin;
        ShowModal;
      Finally
        Free;
      End;
    End;
  // shutdown plugin
  myPlugin.Close;
End;

Procedure TfrmClient.btnConfClick(Sender: TObject);
// batched config administration
Var
  myPlugin: TfsRemoteAdminServer;
Begin
  // setup Admin plugin
  myPlugin := RemoteAdmin;
  myPlugin.Username := edtName.Text;
  myPlugin.Password := edtPasswd.Text;
  myPlugin.Start;
  With TfrmGenInfo.Create(Application) Do
    Begin
      Try
        // show config admin form
        Plugin := myPlugin;
        ShowModal;
      Finally
        Free;
      End;
    End;
  // shutdown plugin
  myPlugin.Close;
End;

Procedure TfrmClient.btnSingleClick(Sender: TObject);
// one-2-one administration
Var
  myPlugin: TfsRemoteAdminServer;
Begin
  // setup Admin plugin
  myPlugin := RemoteAdmin;
  myPlugin.Username := edtName.Text;
  myPlugin.Password := edtPasswd.Text;
  myPlugin.Start;
  With TfrmSingle.Create(Application) Do
    Begin
      Try
        // show one-2-one admin form
        Plugin := myPlugin;
        ShowModal;
      Finally
        Free;
      End;
    End;
  // shutdown plugin
  myPlugin.Close;
End;

Procedure TfrmClient.FormCreate(Sender: TObject);
Begin
  EventLog.FileName := ExtractFilePath(Application.ExeName) + 'FSServer.log';
End;

End.

