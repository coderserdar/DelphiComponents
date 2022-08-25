program OverbyteIcsFtpAsy;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsFtpAsy1 in 'OverbyteIcsFtpAsy1.pas' {FtpAsyncForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFtpAsyncForm, FtpAsyncForm);
  Application.Run;
end.
