program md5test;

uses
  Forms,
  md5test1 in 'md5test1.pas' {MD5TestForm};

{$R *.RES}

begin
  Application.CreateForm(TMD5TestForm, MD5TestForm);
  Application.Run;
end.
