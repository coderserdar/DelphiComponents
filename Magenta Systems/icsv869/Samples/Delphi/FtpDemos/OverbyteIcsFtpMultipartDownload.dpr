program OverbyteIcsFtpMultipartDownload;

{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFtpMultipartDownload1 in 'OverbyteIcsFtpMultipartDownload1.pas' {MultipartFtpDownloadForm},
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMultipartFtpDownloadForm, MultipartFtpDownloadForm);
  Application.Run;
end.
