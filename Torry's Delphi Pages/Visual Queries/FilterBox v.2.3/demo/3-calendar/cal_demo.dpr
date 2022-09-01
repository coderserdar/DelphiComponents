program cal_demo;

uses
  Forms,
  cal_demo_main in 'cal_demo_main.pas' {cal_demo_frm_main},
  cal_demo_calpanel in 'cal_demo_calpanel.pas' {cal_demo_frm_calpanel},
  cal_demo_dateedit in 'cal_demo_dateedit.pas' {cal_demo_frm_datetime},
  cal_demo_monthbox in 'cal_demo_monthbox.pas' {cal_demo_frm_monthbox},
  cal_demo_calendar in 'cal_demo_calendar.pas' {cal_demo_frm_calendar},
  cal_demo_dateupdown in 'cal_demo_dateupdown.pas' {cal_demo_frm_pscdateupdown},
  cal_demo_const in 'cal_demo_const.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tcal_demo_frm_main, cal_demo_frm_main);
  Application.CreateForm(Tcal_demo_frm_calpanel, cal_demo_frm_calpanel);
  Application.CreateForm(Tcal_demo_frm_datetime, cal_demo_frm_datetime);
  Application.CreateForm(Tcal_demo_frm_monthbox, cal_demo_frm_monthbox);
  Application.CreateForm(Tcal_demo_frm_calendar, cal_demo_frm_calendar);
  Application.CreateForm(Tcal_demo_frm_pscdateupdown, cal_demo_frm_pscdateupdown);
  Application.Run;
end.
