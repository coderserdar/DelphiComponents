program OverByteIcsWndControlTest;

uses
  Forms,
  OverByteIcsWndControlTest1 in 'OverByteIcsWndControlTest1.pas' {AppBaseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppBaseForm, AppBaseForm);
  Application.Run;
end.
