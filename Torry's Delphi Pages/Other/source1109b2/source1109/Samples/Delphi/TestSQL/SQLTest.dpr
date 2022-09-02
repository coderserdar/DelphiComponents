program SQLTest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmTestSQL};

{Form1}

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTestSQL, frmTestSQL);
  Application.Run;
end.
