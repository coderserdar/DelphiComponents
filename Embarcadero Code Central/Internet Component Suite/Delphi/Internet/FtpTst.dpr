program Ftptst;

uses
  Forms,
  FtpTst1 in 'FtpTst1.pas' {FtpReceiveForm},
  Ftptst2 in 'Ftptst2.pas' {DirectoryForm};

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
