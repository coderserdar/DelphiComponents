program OverbyteIcsSslMailRcv;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslMailRcv1 in 'OverbyteIcsSslMailRcv1.pas' {POP3ExcercizerForm},
  OverbyteIcsSslMailRcv2 in 'OverbyteIcsSslMailRcv2.pas' {MessageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.

