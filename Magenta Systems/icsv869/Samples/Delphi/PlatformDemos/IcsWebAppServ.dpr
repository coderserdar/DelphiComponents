program IcsWebAppServ;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND}
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  Fmx.Types,
  IcsWebAppServerMain in 'IcsWebAppServerMain.pas' {WebAppSrvForm},
  OverbyteIcsWebAppServerConfig in '..\WebDemos\OverbyteIcsWebAppServerConfig.pas',
  OverbyteIcsWebAppServerCounterView in '..\WebDemos\OverbyteIcsWebAppServerCounterView.pas',
  OverbyteIcsWebAppServerDataModule in '..\WebDemos\OverbyteIcsWebAppServerDataModule.pas' {WebAppSrvDataModule: TDataModule},
  OverbyteIcsWebAppServerHead in '..\WebDemos\OverbyteIcsWebAppServerHead.pas',
  OverbyteIcsWebAppServerHelloWorld in '..\WebDemos\OverbyteIcsWebAppServerHelloWorld.pas',
  OverbyteIcsWebAppServerHomePage in '..\WebDemos\OverbyteIcsWebAppServerHomePage.pas',
  OverbyteIcsWebAppServerHttpHandlerBase in '..\WebDemos\OverbyteIcsWebAppServerHttpHandlerBase.pas',
  OverbyteIcsWebAppServerLogin in '..\WebDemos\OverbyteIcsWebAppServerLogin.pas',
  OverbyteIcsWebAppServerSessionData in '..\WebDemos\OverbyteIcsWebAppServerSessionData.pas',
  OverbyteIcsWebAppServerUrlDefs in '..\WebDemos\OverbyteIcsWebAppServerUrlDefs.pas',
  Ics.Fmx.OverbyteIcsWebAppServerCounter in '..\WebDemos\Ics.Fmx.OverbyteIcsWebAppServerCounter.pas',
  Ics.Fmx.OverbyteIcsWebAppServerMailer in '..\WebDemos\Ics.Fmx.OverbyteIcsWebAppServerMailer.pas',
  OverbyteIcsFormDataDecoder in '..\..\..\Source\OverbyteIcsFormDataDecoder.pas';

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TWebAppSrvForm, WebAppSrvForm);
  Application.CreateForm(TWebAppSrvDataModule, WebAppSrvDataModule);
  Application.Run;
end.
