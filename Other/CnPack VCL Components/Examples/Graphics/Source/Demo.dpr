program Demo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CnPackͼ�������ʾ���� V0.11Alpha';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
