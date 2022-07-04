unit FormGraph3D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Graph3D, StdCtrls, Buttons, FormAbout;

type
  TForm1 = class(TForm)
    G: TGraph3D;
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    ComboStyle: TComboBox;
    ListShape: TListBox;
    butShapeColor1: TPanel;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    chkAxis: TCheckBox;
    chkWireframe: TCheckBox;
    butAxisColor1: TPanel;
    butBackColor: TPanel;
    butAdd: TSpeedButton;
    butRemove: TSpeedButton;
    Label6: TLabel;
    butSHapeColor2: TPanel;
    Timer: TTimer;
    Label5: TLabel;
    butAxisColor2: TPanel;
    butZoomIn: TSpeedButton;
    butZoomOut: TSpeedButton;
    ColorDialog: TColorDialog;
    procedure FormPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure butZoomInClick(Sender: TObject);
    procedure butZoomOutClick(Sender: TObject);
    procedure chkAxisClick(Sender: TObject);
    procedure butBackColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butAddClick(Sender: TObject);
    procedure butRemoveClick(Sender: TObject);
    procedure butShapeColor1Click(Sender: TObject);
    procedure butSHapeColor2Click(Sender: TObject);
    procedure chkWireframeClick(Sender: TObject);
    procedure butAxisColor1Click(Sender: TObject);
    procedure butAxisColor2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormPaint(Sender: TObject);
begin
    Timer.Enabled:=true;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
    Timer.Enabled:=false;
    G.Render();
end;

procedure TForm1.butZoomInClick(Sender: TObject);
begin
    G.ZoomIn;
    G.Render();
end;

procedure TForm1.butZoomOutClick(Sender: TObject);
begin
    G.ZoomOut;
    G.Render();
end;

procedure TForm1.chkAxisClick(Sender: TObject);
begin
    G.PAxis:=chkAxis.Checked;
    G.Render();
end;

procedure TForm1.butBackColorClick(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butBackColor.Color:=ColorDialog.Color;
        G.PColorBackground:=ColorDialog.Color;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    chkAxis.Checked:=G.PAxis;
    butAxisColor1.Color:=G.PColorAxis1;
    butAxisColor2.Color:=G.PColorAxis2;
    butBackColor.Color:=G.PColorBackground;
    ComboStyle.ItemIndex:=7;
end;

procedure TForm1.butAddClick(Sender: TObject);
var s:string;
begin
    case ComboStyle.ItemIndex of
    0:s:='Cone(0,0,2,0,0,0,1)';
    1:s:='Cube(-1,-1,-1,1,1,1)';
    2:s:='Cylinder(0,0,0,1,1,1)';
    3:s:='Line(-2,-2,-2,2,2,2)';
    4:s:='Plane(-1,-1,1,1,-1,1,1,1,-1,-1,1,-1)';
    5:s:='Sphere(0,0,0,1)';
    6:s:='Triangle(0,0,0,2,0,0,2,2,0)';
    7:s:='ZYX(0.3*(X*X+Y*Y),-2,-2,2,2)';
    end;
    s:=InputBox('Expression','Type Shape''s Expression',s);
    ListShape.Items.Add(s);
    G.Add(s,butShapeColor1.Color,butShapeColor2.Color);
    G.Render();
end;

procedure TForm1.butRemoveClick(Sender: TObject);
var i:integer;
begin
    i:=0;
    while i<ListShape.Items.Count do
    if ListShape.Selected[i]then begin
        ListShape.Items.Delete(i);
        G.Remove(i);
    end
    else inc(i);
    G.Render();
end;

procedure TForm1.butShapeColor1Click(Sender: TObject);
begin
    if ColorDialog.Execute then butShapeColor1.Color:=ColorDialog.Color;
end;

procedure TForm1.butSHapeColor2Click(Sender: TObject);
begin
    if ColorDialog.Execute then butShapeColor2.Color:=ColorDialog.Color;
end;

procedure TForm1.chkWireframeClick(Sender: TObject);
begin
    G.PWireframe:=chkWireframe.Checked;
    G.Render();
end;

procedure TForm1.butAxisColor1Click(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butAxisColor1.Color:=ColorDialog.Color;
        G.PColorAxis1:=ColorDialog.Color;
    end;
end;

procedure TForm1.butAxisColor2Click(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butAxisColor2.Color:=ColorDialog.Color;
        G.PColorAxis2:=ColorDialog.Color;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
    Form2.ShowModal;
end;

end.
