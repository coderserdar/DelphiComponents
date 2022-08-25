program OverbyteIcsMailRcv;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMailRcv1 in 'OverbyteIcsMailRcv1.pas' {POP3ExcercizerForm},
  OverbyteIcsMailRcv2 in 'OverbyteIcsMailRcv2.pas' {MessageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.
