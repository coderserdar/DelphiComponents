program M_dproj;

uses
  Forms,
  M_Dmain in 'M_DMAIN.PAS' {FormMain};

{$R *.RES}

begin
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
