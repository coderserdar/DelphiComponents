program SegDemo;

uses
  Forms,
  SegMain in 'SegMain.pas' {SegForm},
  SegCfg in 'SegCfg.pas' {CfgForm},
  SegSel in 'SegSel.pas' {SelForm};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.Title := 'SegLHA Demo1';
  Application.CreateForm(TSegForm, SegForm);
  Application.CreateForm(TCfgForm, CfgForm);
  Application.CreateForm(TSelForm, SelForm);
  Application.Run;
end.
