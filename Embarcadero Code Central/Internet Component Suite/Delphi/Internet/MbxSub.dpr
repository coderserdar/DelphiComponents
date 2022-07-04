program MbxSub;

uses
  Forms,
  MbxSub1 in 'MbxSub1.pas' {AppBaseForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TAppBaseForm, AppBaseForm);
  Application.Run;
end.
