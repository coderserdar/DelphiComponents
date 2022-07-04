program Biolife;

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  UnitDesign in 'UnitDesign.pas' {FormDesign};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormDesign, FormDesign);
  Application.Run;
end.

