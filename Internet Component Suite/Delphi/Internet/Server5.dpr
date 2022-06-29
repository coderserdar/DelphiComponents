program server5;

uses
  Forms,
  Srv5 in 'Srv5.pas' {ServerForm};

{$R *.RES}

begin
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
