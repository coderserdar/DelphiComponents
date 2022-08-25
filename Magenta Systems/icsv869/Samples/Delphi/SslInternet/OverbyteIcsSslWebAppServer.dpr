program OverbyteIcsSslWebAppServer;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslWebAppServerMain in 'OverbyteIcsSslWebAppServerMain.pas' {WebAppSrvForm},
  OverbyteIcsWebAppServerSessionData in '..\WebDemos\OverbyteIcsWebAppServerSessionData.pas',
  OverbyteIcsWebAppServerLogin in '..\WebDemos\OverbyteIcsWebAppServerLogin.pas',
//  OverbyteIcsWebAppServerUploads in '..\WebDemos\OverbyteIcsWebAppServerUploads.pas',
  OverbyteIcsWebAppServerUrlDefs in '..\WebDemos\OverbyteIcsWebAppServerUrlDefs.pas',
  OverbyteIcsWebAppServerHttpHandlerBase in '..\WebDemos\OverbyteIcsWebAppServerHttpHandlerBase.pas',
  OverbyteIcsWebAppServerDataModule in '..\WebDemos\OverbyteIcsWebAppServerDataModule.pas' {WebAppSrvDataModule: TDataModule},
  OverbyteIcsWebAppServerCounter in '..\WebDemos\OverbyteIcsWebAppServerCounter.pas',
  OverbyteIcsWebAppServerHomePage in '..\WebDemos\OverbyteIcsWebAppServerHomePage.pas',
  OverbyteIcsWebAppServerConfig in '..\WebDemos\OverbyteIcsWebAppServerConfig.pas',
  OverbyteIcsWebAppServerCounterView in '..\WebDemos\OverbyteIcsWebAppServerCounterView.pas',
  OverbyteIcsWebAppServerHead in '..\WebDemos\OverbyteIcsWebAppServerHead.pas',
  OverbyteIcsWebAppServerHelloWorld in '..\WebDemos\OverbyteIcsWebAppServerHelloWorld.pas',
  OverbyteIcsWebAppServerMailer in '..\WebDemos\OverbyteIcsWebAppServerMailer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWebAppSrvForm, WebAppSrvForm);
  Application.CreateForm(TWebAppSrvDataModule, WebAppSrvDataModule);
  Application.Run;
end.
