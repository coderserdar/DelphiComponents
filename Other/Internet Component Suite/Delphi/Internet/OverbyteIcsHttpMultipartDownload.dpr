program OverbyteIcsHttpMultipartDownload;

uses
  Forms,
  OverbyteIcsHttpMultipartDownload1 in 'OverbyteIcsHttpMultipartDownload1.pas' {MultipartHttpDownloadForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMultipartHttpDownloadForm, MultipartHttpDownloadForm);
  Application.Run;
end.
