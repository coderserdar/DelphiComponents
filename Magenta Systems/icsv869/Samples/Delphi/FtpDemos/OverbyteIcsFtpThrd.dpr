program OverbyteIcsFtpThrd;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsFtpThrd1 in 'OverbyteIcsFtpThrd1.pas' {ThrdFtpForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TThrdFtpForm, ThrdFtpForm);
  Application.Run;
end.
