program CliDemo;

uses
  Forms,
  CliDemo1 in 'CliDemo1.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
