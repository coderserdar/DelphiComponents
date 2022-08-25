program OverbyteIcsHttpMultipartDownload;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsHttpMultipartDownload1 in 'OverbyteIcsHttpMultipartDownload1.pas' {MultipartHttpDownloadForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMultipartHttpDownloadForm, MultipartHttpDownloadForm);
  Application.Run;
end.
