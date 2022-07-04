program SMScriptDemo;

uses
  Forms,
  Main in 'Main.pas' {frmPackage};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPackage, frmPackage);
  Application.Run;
end.
