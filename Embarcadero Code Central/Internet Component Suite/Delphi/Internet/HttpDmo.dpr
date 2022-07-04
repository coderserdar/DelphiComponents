program HttpDmo;

uses
  Forms,
  HttpDmo1 in 'HttpDmo1.pas' {HttpToMemoForm};

{$R *.RES}

begin
  Application.CreateForm(THttpToMemoForm, HttpToMemoForm);
  Application.Run;
end.
