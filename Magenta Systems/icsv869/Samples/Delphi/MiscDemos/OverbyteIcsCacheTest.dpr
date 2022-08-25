program OverbyteIcsCacheTest;

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsCacheTest1 in 'OverbyteIcsCacheTest1.pas' {CacheTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCacheTestForm, CacheTestForm);
  Application.Run;
end.
