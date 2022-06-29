program ProtectingFields;

uses
  Forms,
  main in 'main.pas' {Form1},
  dtm_Sample in 'dtm_Sample.pas' {SampleData: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSampleData, SampleData);
  Application.Run;
end.
