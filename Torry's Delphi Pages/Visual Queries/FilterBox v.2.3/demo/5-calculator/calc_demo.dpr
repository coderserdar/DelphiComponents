program calc_demo;

uses
  Forms,
  calc_damo_main in 'calc_damo_main.pas' {psc_frm_setup_main},
  calc_demo_const in 'calc_demo_const.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tpsc_frm_setup_main, psc_frm_setup_main);
  Application.Run;
end.
