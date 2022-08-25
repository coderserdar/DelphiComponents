program OverbyteIcsCliDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsCliDemo1 in 'OverbyteIcsCliDemo1.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
