program Test_D7;

uses
  Forms,
  TestUI in 'TestUI.pas' {TestForm},
  ComparisonUI in 'ComparisonUI.pas' {ComparisonTestForm},
  PerformanceTest in 'PerformanceTest.pas',
  FunctionalTest in 'FunctionalTest.pas',
  MemoryLeakTest in 'MemoryLeakTest.pas',
  Collections in '..\Collections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TComparisonTestForm, ComparisonTestForm);
  Application.Run;
end.
