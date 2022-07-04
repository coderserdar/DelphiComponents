program SXSkinConverter;

uses
  Forms,
  ConverterUnit in 'ConverterUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SXSkinConverter';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
