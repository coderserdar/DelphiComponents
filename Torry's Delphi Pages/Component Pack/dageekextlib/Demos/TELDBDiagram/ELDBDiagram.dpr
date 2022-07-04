program ELDBDiagram;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  ItemPropsFrm in 'ItemPropsFrm.pas' {frmItemProps},
  LinkPropsFrm in 'LinkPropsFrm.pas' {frmLinkProps};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
