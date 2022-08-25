program OverbyteIcsFtpServ;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsFtpServ1 in 'OverbyteIcsFtpServ1.pas' {FtpServerForm},
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas';

{$R *.RES}

begin
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
