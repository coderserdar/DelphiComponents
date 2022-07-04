///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

// The purpose of this application is to demonstrate how to add points
// into the TContainer component and visualize data stream in the real time
// using TPlot and TWorksheet components

program UpdateDemo;

uses
  Forms,
  UpdateDemoU in 'UpdateDemoU.pas' {Form1};

{$E EXE}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Update Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
