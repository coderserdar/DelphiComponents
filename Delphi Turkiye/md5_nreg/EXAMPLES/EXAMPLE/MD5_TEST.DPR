program MD5_Test;

uses
  Forms,
  MD5T in 'MD5T.pas' {frmMD5Test};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMD5Test, frmMD5Test);
  Application.Run;
end.
