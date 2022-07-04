///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

program WksDemo;

uses
  Forms,
  WksDemoU in 'WksDemoU.pas' {WKSDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Worksheet Demo';
  Application.CreateForm(TWKSDemoForm, WKSDemoForm);
  Application.Run;
end.
