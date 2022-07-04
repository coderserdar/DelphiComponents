program OverbyteIcsFtpTst;

uses
  Forms,
  OverbyteIcsFtpTst1 in 'OverbyteIcsFtpTst1.pas' {FtpReceiveForm},
  OverByteIcsFtpTst2 in 'OverByteIcsFtpTst2.pas' {DirectoryForm};

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
