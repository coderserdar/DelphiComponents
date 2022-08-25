program OverbyteIcsFtpTst;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsFtpTst1 in 'OverbyteIcsFtpTst1.pas' {FtpReceiveForm},
  OverByteIcsFtpTst2 in 'OverByteIcsFtpTst2.pas' {DirectoryForm},
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas';

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
