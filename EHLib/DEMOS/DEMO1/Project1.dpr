program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CustPrev in 'CustPrev.pas' {fCustomPreview},
  Unit2 in 'Unit2.pas' {Form2},
  DM1 in 'DM1.pas' {DataModule1: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfCustomPreview, fCustomPreview);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
