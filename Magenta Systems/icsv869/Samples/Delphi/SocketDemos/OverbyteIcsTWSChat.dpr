program OverbyteIcsTWSChat;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsTWSChat1 in 'OverbyteIcsTWSChat1.pas' {TWSChatForm};

{$R *.RES}

begin
  Application.CreateForm(TTWSChatForm, TWSChatForm);
  Application.Run;
end.
