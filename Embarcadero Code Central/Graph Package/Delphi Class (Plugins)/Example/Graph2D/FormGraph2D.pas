unit FormGraph2D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Graph, StdCtrls, Buttons, FormAbout;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    G: TGraph;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ComboStyle: TComboBox;
    ListShape: TListBox;
    GroupBox2: TGroupBox;
    chkAxis: TCheckBox;
    chkGrid: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    butShapeColor: TPanel;
    butAxisColor: TPanel;
    butBackColor: TPanel;
    butGridColor: TPanel;
    ColorDialog: TColorDialog;
    butAdd: TSpeedButton;
    butRemove: TSpeedButton;
    butZoomIn: TSpeedButton;
    butZoomOut: TSpeedButton;
    procedure butZoomInClick(Sender: TObject);
    procedure butZoomOutClick(Sender: TObject);
    procedure chkAxisClick(Sender: TObject);
    procedure chkGridClick(Sender: TObject);
    procedure butShapeColorClick(Sender: TObject);
    procedure butAxisColorClick(Sender: TObject);
    procedure butBackColorClick(Sender: TObject);
    procedure butGridColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butAddClick(Sender: TObject);
    procedure butRemoveClick(Sender: TObject);
    procedure ListShapeClick(Sender: TObject);
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

procedure TForm1.butZoomInClick(Sender: TObject);
begin
    G.POP:=G.POP+1;
    G.Render();
end;

procedure TForm1.butZoomOutClick(Sender: TObject);
begin
    G.POP:=G.POP-1;
    G.Render();
end;

procedure TForm1.chkAxisClick(Sender: TObject);
begin
    G.PAxisVisible:=chkAxis.Checked;
    G.Render();
end;

procedure TForm1.chkGridClick(Sender: TObject);
begin
    G.PGrid:=chkGrid.Checked;
    G.Render();
end;

procedure TForm1.butShapeColorClick(Sender: TObject);
begin
    if ColorDialog.Execute then butShapeColor.Color:=ColorDialog.Color;
end;

procedure TForm1.butAxisColorClick(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butAxisColor.Color:=ColorDialog.Color;
        G.PColorAxis:=ColorDialog.Color;
    end;
end;

procedure TForm1.butBackColorClick(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butBackColor.Color:=ColorDialog.Color;
        G.PColorBackground:=ColorDialog.Color;
    end;
end;

procedure TForm1.butGridColorClick(Sender: TObject);
begin
    if ColorDialog.Execute then begin
        butGridColor.Color:=ColorDialog.Color;
        G.PColorGrid:=ColorDialog.Color;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    chkAxis.Checked:=G.PAxisVisible;
    chkGrid.Checked:=G.PGrid;
    butAxisColor.Color:=G.PColorAxis;
    butBackColor.Color:=G.PColorBackground;
    butGridColor.Color:=G.PColorGrid;
    G.POx:=G.Width div 2;
    G.POy:=G.Height div 2;
    ComboStyle.ItemIndex:=9;
end;

procedure TForm1.butAddClick(Sender: TObject);
var s:string;
begin
    case ComboStyle.ItemIndex of
    0:s:='String(www.ngondn.net.tf,-3,3,10)';
    1:s:='Point(1,1)';
    2:s:='Line(1,1,1,-2,2)';
    3:s:='LineDraw(-1,-1,2,2)';
    4:s:='Ellipse(0,0,3,2)';
    5:s:='Rectangle(-3,-2,3,2)';
    6:s:='FillRectangle(-3,-2,3,2)';
    7:s:='Arc(0,0,5,5,0,90)';
    8:s:='Pie(0,0,5,5,0,90)';
    9:s:='YX(y=x^3,-2,2,0,0)';
    10:s:='XY(x=y^2,-2,2,0,0)';
    11:s:='RT(r=6*cos(2*t),-180,180,0,0)';
    12:s:='YXT(x=5*cos(t)^3_y=5*sin(t)^3,-10,10,0,0)';
    end;
    s:=InputBox('Expression','Type Shape''s Expression',s);
    ListShape.Items.Add(s);
    G.Add(s,butShapeColor.Color);
    G.UnSelectAll;
    G.Select(G.Count-1);
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

procedure TForm1.ListShapeClick(Sender: TObject);
var i:integer;
begin
    G.UnSelectAll;
    for i:=0 to ListShape.Items.Count-1 do
        if ListShape.Selected[i]then G.Select(i);
    G.Render();
end;

procedure TForm1.FormShow(Sender: TObject);
begin
    Form2.ShowModal;
end;

end.
