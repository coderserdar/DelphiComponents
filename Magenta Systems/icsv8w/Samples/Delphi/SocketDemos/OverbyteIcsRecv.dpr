program OverbyteIcsRecv;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsRecv1 in 'OverbyteIcsRecv1.pas' {RecvForm};

{$R *.RES}

begin
  Application.CreateForm(TRecvForm, RecvForm);
  Application.Run;
end.
