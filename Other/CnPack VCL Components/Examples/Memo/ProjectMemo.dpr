program ProjectMemo;

uses
  Forms,
  UnitMemo in 'UnitMemo.pas' {CnMemoForm},
  CnMemo in '..\..\Source\Graphics\CnMemo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCnMemoForm, CnMemoForm);
  Application.Run;
end.
