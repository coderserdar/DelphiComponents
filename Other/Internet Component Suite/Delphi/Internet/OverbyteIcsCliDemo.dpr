program OverbyteIcsCliDemo;

uses
  Forms,
  OverbyteIcsCliDemo1 in 'OverbyteIcsCliDemo1.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
