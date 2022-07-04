program ScrShareSrv;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  UserList in 'UserList.pas' {frmUsers},
  NMMReceiverList in '..\..\Source\NMMReceiverList.pas',
  ThreadInfo in 'ThreadInfo.pas' {frmThreadInfo},
  NMMRCQueue in '..\..\..\Source\NMMRCQueue.pas',
  About in 'About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmUsers, frmUsers);
  Application.CreateForm(TfrmThreadInfo, frmThreadInfo);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
