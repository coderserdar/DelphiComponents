program OverbyteIcsBinCliDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsBinCliDemo1 in 'OverbyteIcsBinCliDemo1.pas' {BinClientForm};

{$R *.RES}

begin
  Application.CreateForm(TBinClientForm, BinClientForm);
  Application.Run;
end.
