unit Unit2;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Grids, ELPropInsp, ComCtrls, StdCtrls, ExtCtrls;

type
    TfrmInspector = class (TForm)
        StatusBar1: TStatusBar;
        ELPropertyInspector1: TELPropertyInspector;
        Panel1: TPanel;
        ComboBox1: TComboBox;
    Panel2: TPanel;
        procedure FormCreate (Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ELPropertyInspector1GetComponentNames(Sender: TObject;
      AClass: TComponentClass; AResult: TStrings);
    procedure ELPropertyInspector1GetComponent(Sender: TObject;
      const AComponentName: String; var AComponent: TComponent);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure FillObjList;
		procedure RefreshObjList (SelectedObj: TControl = nil);
    end;

var
    frmInspector: TfrmInspector;

implementation

uses Unit3, Unit1;

{$R *.dfm}

procedure TfrmInspector.FillObjList;
var
    i: integer;
begin
	ComboBox1.Clear;
	ComboBox1.Items.AddObject (Format ('%s: %s', [frmDesign.Name, frmDesign.ClassName]), frmDesign);

	for i := 0 to frmDesign.ComponentCount - 1 do
		ComboBox1.Items.AddObject (Format ('%s: %s', [frmDesign.Components [i].Name, frmDesign.Components [i].ClassName]), frmDesign.Components [i]);

end;

procedure TfrmInspector.FormCreate (Sender: TObject);
begin
    ComboBox1.Align := alTop;
    Panel1.AutoSize := True;
end;

procedure TfrmInspector.RefreshObjList (SelectedObj: TControl);
begin
	if SelectedObj = nil then
		ComboBox1.ItemIndex := -1
	else
		ComboBox1.ItemIndex := ComboBox1.Items.IndexOfObject (SelectedObj);
end;

procedure TfrmInspector.ComboBox1Change(Sender: TObject);
var
	ctrl : TControl;
begin
	if TComboBox(Sender).ItemIndex = -1 then Exit;
	ctrl := TControl(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);

	frmMain.ELDesigner1.SelectedControls.Clear;
	frmMain.ELDesigner1.SelectedControls.Add(ctrl);
//	ELPropertyInspector1.Clear;
end;

procedure TfrmInspector.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
	frmMain.ELDesigner1.Active := False;
end;

procedure TfrmInspector.ELPropertyInspector1GetComponentNames(
  Sender: TObject; AClass: TComponentClass; AResult: TStrings);
begin
	if AClass.ClassName = 'TPopupMenu' then
		begin
	Aresult.Add('PopupMenu1');
	Aresult.Add('PopupMenu2');
	end;
end;

procedure TfrmInspector.ELPropertyInspector1GetComponent(Sender: TObject;
  const AComponentName: String; var AComponent: TComponent);
begin
	if AComponentName = 'PopupMenu1' then
		AComponent := frmDesign.PopupMenu1
	else
		AComponent := frmDesign.PopupMenu2
end;

end.

