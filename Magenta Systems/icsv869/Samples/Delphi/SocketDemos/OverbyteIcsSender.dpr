program OverbyteIcsSender;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSender1 in 'OverbyteIcsSender1.pas' {SenderForm};

{$R *.RES}

begin
  Application.CreateForm(TSenderForm, SenderForm);
  Application.Run;
end.
