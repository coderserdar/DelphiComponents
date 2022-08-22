program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {DualListDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDualListDlg, DualListDlg);
  Application.Run;
end.
