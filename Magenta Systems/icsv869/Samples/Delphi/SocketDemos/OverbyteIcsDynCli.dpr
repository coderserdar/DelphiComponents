program OverbyteIcsDynCli;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsDynCli1 in 'OverbyteIcsDynCli1.pas' {DynCliForm};

{$R *.RES}

begin
  Application.CreateForm(TDynCliForm, DynCliForm);
  Application.Run;
end.
