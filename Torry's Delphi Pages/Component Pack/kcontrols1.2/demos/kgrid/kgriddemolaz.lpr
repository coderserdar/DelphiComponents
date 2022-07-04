program kgriddemolaz;

uses
  Interfaces,
  Forms, LResources, kcontrolslaz, Printer4Lazarus, tachartlazaruspkg,
  Main in 'main.pas',
  Input in 'input.pas';

{$IFDEF WINDOWS}{$R kgriddemolaz.rc}{$ENDIF}

begin
  {$I kgriddemolaz.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TInputForm, InputForm);
  Application.Run;
end.
