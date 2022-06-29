program EanMainFmt;

uses
  Forms,
  EanDemo in 'EanDemo.pas' {Form1},
  EanAbout in 'Eanabout.pas' {PSoftAbout},
  EanRpt1 in 'EanRpt1.pas' {Rpt1},
  EanRpt2 in 'EanRpt2.pas' {Rpt2},
  EanRpt3 in 'EanRpt3.pas' {Rpt3},
  EanSpecs in '..\EanSpecs.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPSoftAbout, PSoftAbout);
  Application.CreateForm(TRpt1, Rpt1);
  Application.CreateForm(TRpt2, Rpt2);
  Application.CreateForm(TRpt3, Rpt3);
  Application.Run;
end.
