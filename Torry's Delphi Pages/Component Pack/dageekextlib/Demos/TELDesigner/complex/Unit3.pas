unit Unit3;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Grids, ELPropInsp, ComCtrls, StdCtrls, ExtCtrls;

type
    TForm3 = class (TForm)
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
    procedure FormShow(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure FillObjList;
		procedure RefreshObjList (SelectedObj: TControl = nil);
    end;

var
    Form3: TForm3;

implementation

uses Unit2, Unit1;

{$R *.dfm}

procedure TForm3.FillObjList;
var
	i: integer;
begin
	ComboBox1.Clear;
	ComboBox1.Items.AddObject (Format ('%s: %s', [Form2.Name, Form2.ClassName]), Form2);

	for i := 0 to Form2.ComponentCount - 1 do
		ComboBox1.Items.AddObject (Format ('%s: %s', [Form2.Components [i].Name, Form2.Components [i].ClassName]), Form2.Components [i]);

end;

procedure TForm3.FormCreate (Sender: TObject);
begin
    ComboBox1.Align := alTop;
    Panel1.AutoSize := True;
end;

procedure TForm3.RefreshObjList (SelectedObj: TControl);
begin
	if SelectedObj = nil then
		ComboBox1.ItemIndex := -1
	else
		ComboBox1.ItemIndex := ComboBox1.Items.IndexOfObject (SelectedObj);
end;

procedure TForm3.ComboBox1Change(Sender: TObject);
var
	ctrl : TControl;
begin
	if TComboBox(Sender).ItemIndex = -1 then Exit;
	ctrl := TControl(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]);

	Form1.ELDesigner1.SelectedControls.Clear;
	Form1.ELDesigner1.SelectedControls.Add(ctrl);
//	ELPropertyInspector1.Clear;
end;

procedure TForm3.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
	Form1.ELDesigner1.Active := False;
end;

procedure TForm3.ELPropertyInspector1GetComponentNames(
  Sender: TObject; AClass: TComponentClass; AResult: TStrings);
begin
	if AClass.ClassName = 'TPopupMenu' then
		begin
	Aresult.Add('PopupMenu1');
	Aresult.Add('PopupMenu2');
	end;
end;

procedure TForm3.ELPropertyInspector1GetComponent(Sender: TObject;
  const AComponentName: String; var AComponent: TComponent);
begin
	if AComponentName = 'PopupMenu1' then
		AComponent := Form2.PopupMenu1
	else
		AComponent := Form2.PopupMenu2
end;

procedure TForm3.FormShow(Sender: TObject);
begin
	FillObjList;
end;

end.

