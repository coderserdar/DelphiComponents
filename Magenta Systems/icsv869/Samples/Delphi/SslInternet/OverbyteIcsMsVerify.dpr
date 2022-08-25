program OverbyteIcsMsVerify;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMsVerify1 in 'OverbyteIcsMsVerify1.pas' {MsVerifyForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMsVerifyForm, MsVerifyForm);
  Application.Run;
end.
