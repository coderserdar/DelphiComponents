program OverbyteIcsFtpTstW;

uses
  Forms,
  OverbyteIcsFtpTstW1 in 'OverbyteIcsFtpTstW1.pas' {FtpReceiveForm},
  OverByteIcsFtpTstW2 in 'OverByteIcsFtpTstW2.pas' {DirectoryForm};

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
