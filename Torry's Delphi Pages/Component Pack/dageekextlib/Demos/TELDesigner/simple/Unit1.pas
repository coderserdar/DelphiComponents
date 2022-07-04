unit Unit1;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, ELDsgnr, StdCtrls;

type
    TfrmMain = class (TForm)
        ELDesigner1: TELDesigner;
        Button1: TButton;
        procedure FormShow (Sender: TObject);
        procedure Button1Click (Sender: TObject);
        procedure ELDesigner1ChangeSelection (Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    frmMain: TfrmMain;

implementation

uses Unit2, Unit3;

{$R *.dfm}

procedure TfrmMain.FormShow (Sender: TObject);
begin
    frmDesign.Show;

    ELDesigner1.DesignControl := frmDesign;
end;

procedure TfrmMain.Button1Click (Sender: TObject);
begin
    ELDesigner1.Active := not ELDesigner1.Active;
    if ELDesigner1.Active then
        frmInspector.FillObjList;
    frmInspector.Visible := ELDesigner1.Active;
end;

procedure TfrmMain.ELDesigner1ChangeSelection (Sender: TObject);
var
    i: integer;
begin
    frmInspector.ELPropertyInspector1.Clear;

    for i := 0 to ELDesigner1.SelectedControls.Count - 1 do
        frmInspector.ELPropertyInspector1.Add (ELDesigner1.SelectedControls [i]);

    if ELDesigner1.SelectedControls.Count <> 1 then
        frmInspector.RefreshObjList
    else
        frmInspector.RefreshObjList (ELDesigner1.SelectedControls [0]);
end;

end.

