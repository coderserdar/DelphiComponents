program OverbyteIcsRestDemo;

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsRestDemo1 in 'OverbyteIcsRestDemo1.pas' {RestDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRestDemoForm, RestDemoForm);
  Application.Run;
end.
