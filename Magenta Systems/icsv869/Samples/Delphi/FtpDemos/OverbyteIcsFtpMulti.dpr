program OverbyteIcsFtpMulti;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsFtpMulti1 in 'OverbyteIcsFtpMulti1.pas' {FtpMultiForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFtpMultiForm, FtpMultiForm);
  Application.Run;
end.
