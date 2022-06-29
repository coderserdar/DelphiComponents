program RVEditD6;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CPFrm in 'CPFrm.pas' {frmCP},
  PropFrm in 'PropFrm.pas' {frmProp},
  ListFrm in 'ListFrm.pas' {frmList},
  PreviewFrm in 'PreviewFrm.pas' {frmPreview},
  RVUndoStr in 'RVUndoStr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'RichViewEdit Demo';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmCP, frmCP);
  Application.CreateForm(TfrmProp, frmProp);
  Application.CreateForm(TfrmList, frmList);
  Application.CreateForm(TfrmPreview, frmPreview);
  Application.Run;
end.
