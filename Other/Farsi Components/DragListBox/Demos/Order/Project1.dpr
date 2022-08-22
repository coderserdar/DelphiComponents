program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmOrderedList};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOrderedList, frmOrderedList);
  Application.Run;
end.
