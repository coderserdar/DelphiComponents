program TBXConverter;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {ConverterForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TBX Converter';
  Application.CreateForm(TConverterForm, ConverterForm);
  Application.Run;
end.
