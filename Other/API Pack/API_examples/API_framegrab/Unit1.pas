unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_FrameGrab, StdCtrls, API_base;

type
  TForm1 = class(TForm)
    API_FrameGrab1: TAPI_FrameGrab;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button3: TButton;
    Button2: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button5: TButton;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_FrameGrab1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure API_FrameGrab1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    X1, X2, Y1, Y2: integer;      // mouse on application
    mX1, mX2, mY1, mY2: integer;  // mouse position on screen
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  api_graphics;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  titleheight: integer;
begin
  // get list of available drivers
  combobox1.Items:= api_framegrab1.GetDriverList;
  // apply zoom window defaults
  titleheight:= GetSystemMetrics(SM_CYCAPTION);
  x1:= shape1.Left;
  mX1:= x1 + left;
  y1:= shape1.Top;
  mY1:= y1 + top + titleheight;
  x2:= shape1.Left+shape1.width;
  mX2:= x2 + left;
  y2:= shape1.Top+shape1.Height;
  mY2:= y2 + top + titleheight;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
begin
  if not api_framegrab1.DriverOpen then exit;

  if api_framegrab1.CapInProgess then
  begin
    // stop capturing
    if not api_framegrab1.CapSingleFramesClose then
      messagedlg('Failed to stop capturing', mterror, [mbok], 0);
  end else
  begin
    // start capturing
    if not api_framegrab1.CapSingleFramesOpen then
      messagedlg('Failed to start capturing', mterror, [mbok], 0);
  end;

  // check for capturing frames
  button5.Enabled:= api_framegrab1.CapInProgess;
end;

//------------------------------------------------------------------------------
procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if combobox1.itemindex<0 then exit;

  // check if already open
  if api_framegrab1.DriverOpen then
  begin
    // stop capturing
    if api_framegrab1.CapInProgess then
      api_framegrab1.StopCapture;
    // close driver
    api_framegrab1.driveropen:=false;
  end;

  // check that driver is selected
  api_framegrab1.DriverIndex:= combobox1.itemindex;
  // open driver
  api_framegrab1.DriverOpen:= true;

  // get some properties
  checkbox1.enabled:= api_framegrab1.HasVideoOverlay;
  checkbox2.enabled:= api_framegrab1.DriverOpen;
  button1.Enabled:= api_framegrab1.DriverOpen;
  button3.Enabled:= api_framegrab1.HasDlgSource;
  button2.enabled:= api_framegrab1.HasDlgSource;
  button4.Enabled:= api_framegrab1.HasDlgDisplay;
end;

//------------------------------------------------------------------------------
procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // change overlay mode
  api_framegrab1.VideoOverlay := checkbox1.Checked;
end;

//------------------------------------------------------------------------------
procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  // change preview mode
  api_framegrab1.VideoPreview := checkbox2.checked;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button3Click(Sender: TObject);
begin
  api_framegrab1.DlgVSource;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button2Click(Sender: TObject);
begin
  api_framegrab1.DlgVFormat;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button4Click(Sender: TObject);
begin
  api_framegrab1.DlgVDisplay;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button5Click(Sender: TObject);
begin
  if api_framegrab1.GrabFrame then
  begin
    savedialog1.DefaultExt:='.bmp';
    savedialog1.HistoryList.Clear;
    savedialog1.FileName:= '';
    if savedialog1.Execute then
    begin
      api_framegrab1.SingleImageFile:= savedialog1.FileName;
      if api_framegrab1.SaveAsDIB then
      begin
        messagedlg('Frame saved', mtinformation, [mbok], 0);
      end else
      begin
        messagedlg('Failed to save frame', mterror, [mbok], 0);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  bmp: tbitmap;
begin
  // draw shape
  shape1.Left:= x1;
  shape1.Top:= y1;
  shape1.Width:= x2-x1;
  shape1.Height:= y2-y1;
  // strethc draw panel on right-bottom
  if (mx2>mx1) and (my2>my1) then
  begin
    bmp:= api_graphics.CaptureScreenRect(rect(mx1,my1,mx2,my2)); //(api_framegrab1);
    try
      // stretch draw
      paintbox1.Canvas.StretchDraw(paintbox1.canvas.ClipRect, bmp);
    finally
      bmp.free;
    end;
  end else
  begin
    // no area selected
    paintbox1.canvas.Brush.Color:= clnavy;
    paintbox1.Canvas.FillRect(paintbox1.canvas.ClipRect);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_FrameGrab1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  t: tpoint;
begin
  GetCursorPos(t);
  // store window top-left
  if button=mbleft then
  begin
    mX1:= t.x;
    x1:= x;
    mY1:= t.y;
    y1:= y;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_FrameGrab1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  t: tpoint;
begin
  GetCursorPos(t);
  // store window top-left
  if (x>x1) and (y>y1) then
  begin
    mX2:= t.X;
    X2:= x;
    mY2:= t.Y;
    Y2:= y;
  end else
  // reset zoom window
  begin
    mX1:= 0;
    x1:= 0;
    mX2:= 0;
    x2:= 0;
    mY1:= 0;
    y1:= 0;
    mY2:= 0;
    x2:= 0;
  end;
end;

end.
