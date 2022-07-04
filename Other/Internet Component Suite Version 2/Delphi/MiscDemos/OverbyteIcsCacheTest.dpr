program OverbyteIcsCacheTest;

uses
  Forms,
  OverbyteIcsCacheTest1 in 'OverbyteIcsCacheTest1.pas' {CacheTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCacheTestForm, CacheTestForm);
  Application.Run;
end.
