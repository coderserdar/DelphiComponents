///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit PlotDemoU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DMPlot, AppEvnts, StdCtrls, ExtDlgs, DMContainer,
  ComCtrls, ColorGrd;

type
  TPlotDemoForm = class(TForm)
    Plot1: TPlot;
    ApplicationEvents1: TApplicationEvents;
    Panel1: TPanel;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Button1: TButton;
    Button2: TButton;
    Container1: TContainer;
    StatusBar1: TStatusBar;
    Button4: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    Button5: TButton;
    ProgressBar1: TProgressBar;
    ColorGrid1: TColorGrid;
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    function Plot1GetPoint(Sender: TObject; Point, Serie: Integer; var X,
      Y: Extended): Boolean;
    procedure Plot1Hint(Sender: TObject; const H: String);
    procedure Plot1Error(Sender: TObject; const H: String);
    procedure Button4Click(Sender: TObject);
    function Plot1PointClick(Sender: TObject; Point, Serie: Integer): Boolean;
    procedure ComboBox1Change(Sender: TObject);
    procedure Plot1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Container1Changed(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Container1Progress(Sender: TObject; P: Integer);
    procedure ColorGrid1Change(Sender: TObject);
  private
    { Private declarations }
    Parameters: TRealArray;
    History: TStringList;
  public
    { Public declarations }
  end;

var
  PlotDemoForm: TPlotDemoForm;

implementation

uses AxisDlg, SerieDlg {$ifndef dmmath}, Parser{$endif};

{$R *.dfm}

const
  strAssertionFailed='Assertion %s failed at '#13#10'%s'#13#10'Please '+
  'report this bug to the developer';

{$ifdef dmmath}
{$I DMMath.inc}  
{$else}
var
  Parser: TMathParser;

function Parse(S: string): extended;
begin
  Parse:=Parser.Parse(S);
end;
{$endif}

procedure TPlotDemoForm.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
var
  Code, Line: string;
  Position, Len: integer;  
begin
  if E is EAssertionFailed then 
  begin
    Len:=Length(E.Message);
    Position:=Pos('}', E.Message);
    if Position>0 then
    begin
      Code:=Copy(E.Message, 1, Position); 
      Line:=Copy(E.Message, Position+1, Len-Position);
      MessageDlg(Format(strAssertionFailed,[Code, Line]), mtError, [mbCancel], 0); 
    end else Application.ShowException(E);
  end else Application.ShowException(E);  
end;

procedure TPlotDemoForm.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    Plot1.Transparent:=true;
    Plot1.BorderStyle:=bsNone;
    Plot1.AutoMargins:=false;
  end;
end;

procedure TPlotDemoForm.Button2Click(Sender: TObject);
begin
  Plot1.CopyToClipboard;
end;

{in this example, we assume that 'X','Y' are the names of the X,Y columns
and X is the name of the parameter. In the real-world situation, we would 
initialize Parameters using TRealData's array. Of course, the only allowed 
parameters are x and y}
function TPlotDemoForm.Plot1GetPoint(Sender: TObject; Point,
  Serie: Integer; var X, Y: Extended): Boolean;
var
  S: array[0..2048] of char;  
begin
  // initialize Parameters[]
  if Plot1.Series[Serie].IsFunction 
  then Parameters[byte('x')-byte('a')+1]:=Plot1.Series[Serie].PMin+
    (Plot1.Series[Serie].PMax-Plot1.Series[Serie].PMin)*Point/
    (Plot1.Series[Serie].LastLine-Plot1.Series[Serie].FirstLine)
  else
  begin
    Parameters[byte('x')-byte('a')+1]:=X;
    Parameters[byte('y')-byte('a')+1]:=Y;
  end;
  // evaluate Serie expressions
  StrPCopy(S, Plot1.Series[Serie].XExpression);  
  if S<>'' 
  then X:=Parse(S);
  StrPCopy(S, Plot1.Series[Serie].YExpression);  
  if S<>'' 
  then Y:=Parse(S);
  // evaluate axes expressions
  Parameters[byte('x')-byte('a')+1]:=X;
  Parameters[byte('y')-byte('a')+1]:=Y;
  if Plot1.Series[Serie].XAxis=BottomAxis
  then StrPCopy(S, Plot1.XAxis.Expression)
  else StrPCopy(S, Plot1.XAxis2.Expression);
  if S<>'' 
  then X:=Parse(S);
  if Plot1.Series[Serie].YAxis=LeftAxis
  then StrPCopy(S, Plot1.YAxis.Expression)
  else StrPCopy(S, Plot1.YAxis2.Expression);
  if S<>'' 
  then Y:=Parse(S);
  Result:=true;
end;

procedure TPlotDemoForm.Plot1Hint(Sender: TObject; const H: String);
begin
  StatusBar1.Panels[0].Text:=H;
  StatusBar1.Update; 
end;

procedure TPlotDemoForm.Plot1Error(Sender: TObject; const H: String);
begin
  StatusBar1.Panels[1].Text:=H;
  windows.beep(1000,100);
end;

procedure TPlotDemoForm.Button4Click(Sender: TObject);
begin
  if Plot1.CanUnZoom 
  then Plot1.UndoZoom
  else beep;
end;

function TPlotDemoForm.Plot1PointClick(Sender: TObject; Point, 
  Serie: Integer): Boolean;
begin
  StatusBar1.Panels[1].Text:=Format('Point: %d; Serie: %d',[Point, Serie]);
  Result:=true;
end;

procedure TPlotDemoForm.ComboBox1Change(Sender: TObject);
begin
  Plot1.MouseMode:=TPlotMouseMode(Combobox1.itemindex)
end;

procedure TPlotDemoForm.Plot1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var 
  A: TAxis;
  I: integer;
begin
  A:=nil;
  if (Button=mbLeft) and (ssDouble in Shift) then
  {todo: calculate dialog box title!}
  begin
    if X<Plot1.LeftMargin then A:=Plot1.YAxis;
    if X>Plot1.Width-Plot1.RightMargin then A:=Plot1.YAxis2;
    if Y<Plot1.TopMargin then A:=Plot1.XAxis2;
    if Y>Plot1.Height-Plot1.BottomMargin then A:=Plot1.XAxis;
    if Assigned(A) then
    with TAxisPropsForm.Create(Application) do
    try
      ExpressionComboBox.Items.Assign(History);
      Execute(A);
      History.Assign(ExpressionComboBox.Items);
    finally
      Free;
    end
    else
    with TSeriePropsForm.Create(Application) do 
    try
      for I:=1 to MaxCols do
      XColumnComboBox.Items.Add('Column '+IntToStr(I));
      YColumnComboBox.Items.Assign(XColumnComboBox.Items);
      WorksheetComboBox.AddItem(Container1.Name, Container1);
      WorksheetComboBox.ItemIndex:=0;
      XExpressionComboBox.Items.Assign(History);
      YExpressionComboBox.Items.Assign(History);
      Execute(Plot1.ThisSerie);
      History.Assign(XExpressionComboBox.Items);
    finally
      Free;
    end
  end;
end;

procedure TPlotDemoForm.ApplicationEvents1Hint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:=Application.Hint;
end;

procedure TPlotDemoForm.FormCreate(Sender: TObject);
var
  I: integer;
  C: Char;
  AC: array[0..1] of char;
begin
{$ifndef dmmath}
  Parser.AddStdParams(@Parameters);
{$else}
  AC[1]:=#0;
  for C:='A' to 'Z' do
  begin
    AC[0]:=C;
    AddParserObject(@Parameters[byte(C)-byte('A')+1], @AC, 4);
  end;
{$endif}
  for I:=0 to Plot1.Series.Count-1 do
  ComboBox2.Items.Add(Plot1.Series[I].Text);
  ComboBox2.ItemIndex:=Plot1.SerieIndex;
  History:=TStringList.Create;
  CheckBox1.Checked:=false;
end;

procedure TPlotDemoForm.ComboBox2Change(Sender: TObject);
begin
  Plot1.SerieIndex:=ComboBox2.ItemIndex;
end;

procedure TPlotDemoForm.Container1Changed(Sender: TObject);
begin
  CheckBox1.Checked:=true;
end;

procedure TPlotDemoForm.Button5Click(Sender: TObject);
begin
  Plot1.Delete;
end;

procedure TPlotDemoForm.FormDestroy(Sender: TObject);
begin
  History.Free;
end;

procedure TPlotDemoForm.Container1Progress(Sender: TObject; P: Integer);
begin
  ProgressBar1.Position:=P;
  ProgressBar1.Visible:=(P>0) and (P<100);
  if Sender=Plot1 then Sleep(10);
end;

procedure TPlotDemoForm.ColorGrid1Change(Sender: TObject);
begin
  Plot1.Color:=ColorGrid1.ForegroundColor;
  Plot1.Transparent:=false;
end;

{$ifndef dmmath}
initialization
  Parser:=TMathParser.Create;
  Parser.Init(100);
  with Parser do
  begin
    AddGonio;
    AddLogic;
    AddMath;
    AddMisc;
    AddSpec;
  end;
finalization
  FreeAndNil(Parser);
{$endif}
end.
