program ProtectingRecords;

uses
  Forms,
  main in 'main.pas' {Form1},
  additional_info in 'additional_info.pas' {frmAdditionalInfo},
  dtm_Sample in 'dtm_Sample.pas' {SampleData: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
