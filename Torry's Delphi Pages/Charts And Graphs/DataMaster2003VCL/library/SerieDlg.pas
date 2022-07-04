///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit SerieDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, ImgList, Buttons, ExtCtrls, ColorGrd, DMPlot,
  DMComboBox;

type
  TSeriePropsForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    HelpBitBtn: TBitBtn;
    ImageList: TImageList;
    ColorDialog: TColorDialog;
    PageControl: TPageControl;
    DataTabSheet: TTabSheet;
    ViewTabSheet: TTabSheet;
    XGroupBox: TGroupBox;
    YGroupBox: TGroupBox;
    PointsCheckBox: TCheckBox;
    PointsPanel: TPanel;
    PointSizeLabel: TLabel;
    PointSizeSpinEdit: TSpinEdit;
    PointTypeComboBox: TComboBox;
    PointTypeLabel: TLabel;
    LinePanel: TPanel;
    LineWidthLabel: TLabel;
    LineTypeLabel: TLabel;
    LineWidthSpinEdit: TSpinEdit;
    LineTypeComboBox: TComboBox;
    LineCheckBox: TCheckBox;
    PreviewPaintBox: TPaintBox;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    XColumnComboBox: TComboBox;
    YColumnComboBox: TComboBox;
    YColumnLabel: TLabel;
    YExpressionLabel: TLabel;
    XColumnLabel: TLabel;
    XExpressionLabel: TLabel;
    DataPanel: TPanel;
    ParameterPanel: TPanel;
    PMinEdit: TEdit;
    PMaxEdit: TEdit;
    IntervalsSpinEdit: TSpinEdit;
    IntervalsLabel: TLabel;
    CXLabel: TLabel;
    FirstLineLabel: TLabel;
    FirstLineSpinEdit: TSpinEdit;
    LastLineLabel: TLabel;
    LastLineSpinEdit: TSpinEdit;
    InterleaveLabel: TLabel;
    InterleaveSpinEdit: TSpinEdit;
    WorksheetComboBox: TComboBox;
    WorksheetLabel: TLabel;
    DataRadioButton: TRadioButton;
    ParameterRadioButton: TRadioButton;
    ParameterLabel: TLabel;
    LineColorGrid: TColorGrid;
    LineColorBitBtn: TBitBtn;
    LineColorLabel: TLabel;
    PointsColorGrid: TColorGrid;
    PointsColorBitBtn: TBitBtn;
    PointsColorLabel: TLabel;
    XAxisRadioGroup: TRadioGroup;
    YAxisRadioGroup: TRadioGroup;
    VisibleCheckBox: TCheckBox;
    XExpressionComboBox: TExpressionComboBox;
    YExpressionComboBox: TExpressionComboBox;
    HTMLTextBitBtn: TBitBtn;
    XErrorBarColumnLabel: TLabel;
    XErrorBarColumnComboBox: TComboBox;
    YErrorBarColumnComboBox: TComboBox;
    YErrorBarColumnLabel: TLabel;
    XErrorBarColumnSpeedButton: TSpeedButton;
    YErrorBarColumnSpeedButton: TSpeedButton;
    LeaderPanel: TPanel;
    LeaderCheckBox: TCheckBox;
    LeaderSpinEdit: TSpinEdit;
    LeaderLabel: TLabel;
    AreaLabel: TLabel;
    AreaComboBox: TComboBox;
    procedure PointPanelCheckBoxClick(Sender: TObject);
    procedure PreviewPaintBoxPaint(Sender: TObject);
    procedure ColorGridChange(Sender: TObject);
    procedure PointsColorBitBtnClick(Sender: TObject);
    procedure LineColorBitBtnClick(Sender: TObject);
    procedure LineTypeComboBoxDrawItem(Control: TWinControl;
      Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure PointTypeComboBoxDrawItem(Control: TWinControl;
      Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure WorksheetComboBoxChange(Sender: TObject);
    procedure WorksheetComboBoxDropDown(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure HTMLTextBitBtnClick(Sender: TObject);
    procedure XErrorBarColumnSpeedButtonClick(Sender: TObject);
    procedure YErrorBarColumnSpeedButtonClick(Sender: TObject);
    procedure AreaComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect:
        TRect; State: TOwnerDrawState);
    procedure LeaderCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    PointColorBuf, LineColorBuf: TColor;
    FPlot: TPlot; 
    FSerie: TSerie;
  public
    { Public declarations }
    function Execute(Serie: TSerie): boolean;
  end;

var 
  SeriePropsForm: TSeriePropsForm;

implementation

uses TypInfo, TextDlg, DMContainer, HTextDlg, DMHTMLText;

{$R *.DFM}

{ TSeriePropsForm }

const
  fix=100; // combo Objects[] bugfix 
  {we can't remember -1 in TCustomComboBoxStrings.Objects because values
  are remembered by Windows, and this value has special meaning - CB_ERR!!!
  See implementation of TCustomComboBoxStrings.GetObject for details.}
  
{prior to Execute() call, following comboboxes MUST be initialized:
1) Columns combos - with valid column number and labels;
2) Worksheet combo: Clear, then AddObjects(<Window caption/Filename>,
   <Pointer to TContainer>) + ItemIndex set to serie's container (or -1);
3) Expressions combos - assigned expressions history list.}
function TSeriePropsForm.Execute(Serie: TSerie): boolean;

  function S12(S1, S2: string): string; // as in DMPlotEdit
  begin
    if S1<>''
    then S2:=S2+' {'+S1+'}';
    S12:=HTML2Text(S2);
  end;

var 
  BX, BY: boolean; 
  I: integer;
begin
  FPlot:=(Serie.Collection as TSeries).Plot; // save reference for PaintPoint()
  FSerie:=Serie; // for SetScaleBitBtnClick()
  // copy properties from serie
  VisibleCheckBox.Checked:=Serie.Visible;
  XColumnComboBox.ItemIndex:=Serie.XColumn-1; 
  YColumnComboBox.ItemIndex:=Serie.YColumn-1;
  XErrorBarColumnComboBox.ItemIndex:=Serie.XErrorColumn-1; 
  YErrorBarColumnComboBox.ItemIndex:=Serie.YErrorColumn-1; 
  XExpressionComboBox.Text:=Serie.XExpression;
  YExpressionComboBox.Text:=Serie.YExpression;
  XAxisRadioGroup.ItemIndex:=Ord(Serie.XAxis);
  YAxisRadioGroup.ItemIndex:=Ord(Serie.YAxis);
  if Assigned(Serie.Container) then
  begin
    FirstLineSpinEdit.MaxValue:=Serie.Container.Items.Count-1;
    LastLineSpinEdit.MaxValue:=Serie.Container.Items.Count-1;
  end else
  begin
    FirstLineSpinEdit.MaxValue:=-1; 
    LastLineSpinEdit.MaxValue:=-1;
  end;
  FirstLineSpinEdit.Value:=Serie.FirstLine;
  LastLineSpinEdit.Value:=Serie.LastLine;
  InterleaveSpinEdit.Value:=Serie.Interleave;
  PointsCheckBox.Checked:=Serie.PointVisible; // handler called implicitly!
  LineCheckBox.Checked:=Serie.LineVisible;
  CaptionEdit.Text:=Serie.Text;
  PointsColorGrid.ForegroundIndex:=-1;
  LineColorGrid.ForegroundIndex:=-1;
  PointColorBuf:=Serie.Brush.Color;
  LineColorBuf:=Serie.Pen.Color;
  PointSizeSpinEdit.Value:=Serie.PointSize;
  PointTypeComboBox.ItemIndex:=ord(Serie.PointType);
  LineWidthSpinEdit.Value:=Serie.Pen.Width;
  case Serie.Pen.Style of
    psSolid: LineTypeComboBox.ItemIndex:=0;
    psDash: LineTypeComboBox.ItemIndex:=1;
    psDot: LineTypeComboBox.ItemIndex:=2;
    psDashDot: LineTypeComboBox.ItemIndex:=3;
  else LineTypeComboBox.ItemIndex:=-1;
  end;
  PMinEdit.Text:=FloatToStrF(Serie.PMin, ffGeneral, 8, 4);
  PMaxEdit.Text:=FloatToStrF(Serie.PMax, ffGeneral, 8, 4);
  IntervalsSpinEdit.Value:=Serie.LastLine-Serie.FirstLine+1;
  ParameterRadioButton.Checked:=Serie.IsFunction;
  LeaderCheckBox.Checked:=Serie.IsRecording;
  LeaderSpinEdit.Value:=Serie.LeaderPosition;
  // prepare AreaComboBox
  AreaComboBox.Clear;
  AreaComboBox.Items.AddObject('Empty', TObject(abcNothing+fix));
  AreaComboBox.Items.AddObject(S12(FPlot.YAxis2.Title, 
    {FPlot.YAxis2.GetNamePath - always TAxis!}'Y Axis 2'), TObject(abcYAxis2+fix));
  AreaComboBox.Items.AddObject(S12(FPlot.XAxis2.Title, 
    'X Axis 2'), TObject(abcXAxis2+fix));
  AreaComboBox.Items.AddObject(S12(FPlot.YAxis.Title, 
    'Y Axis'), TObject(abcYAxis+fix));
  AreaComboBox.Items.AddObject(S12(FPlot.XAxis.Title, 
    'X Axis'), TObject(abcXAxis+fix));
  for I:=0 to FPlot.Series.Count-1 do
  if {(I<>Serie.Index) and (!!!}not FPlot.Series[I].Empty{)} then
  AreaComboBox.Items.AddObject(S12(FPlot.Series[I].Text, 'Series '+
    IntToStr(FPlot.Series[I].Index)), TObject(FPlot.Series[I].ID+fix));
  for I:=0 to AreaComboBox.Items.Count-1 do
  if integer(AreaComboBox.Items.Objects[I])-fix=Serie.AreaBorder then
  begin
    AreaComboBox.ItemIndex:=I;
    Break;
  end;
  if AreaComboBox.ItemIndex=-1 // fix invalid value
  then AreaComboBox.ItemIndex:=0;
  // show dialog
  Result:=ShowModal=mrOK;  
  if not Result then Exit; // cancelled
  // copy properties to serie
  Serie.IsRecording:=LeaderCheckBox.Checked; // before setting LeaderPosition!
  Serie.LeaderPosition:=LeaderSpinEdit.Value;
  Serie.AreaBorder:=integer(AreaComboBox.Items.Objects[AreaComboBox.ItemIndex])
    -fix;
  Serie.Visible:=VisibleCheckBox.Checked;
  Serie.IsFunction:=ParameterRadioButton.Checked;
  if ParameterRadioButton.Checked then
  begin
    Serie.FirstLine:=0;
    Serie.LastLine:=IntervalsSpinEdit.Value-1;
  end else
  begin
    Serie.FirstLine:=FirstLineSpinEdit.Value;
    Serie.LastLine:=LastLineSpinEdit.Value;
  end;
  Serie.XColumn:=XColumnComboBox.ItemIndex+1; 
  Serie.YColumn:=YColumnComboBox.ItemIndex+1;
  Serie.XErrorColumn:=XErrorBarColumnComboBox.ItemIndex+1; 
  Serie.YErrorColumn:=YErrorBarColumnComboBox.ItemIndex+1; 
  Serie.XExpression:=XExpressionComboBox.Text;
  Serie.YExpression:=YExpressionComboBox.Text;
  Serie.Interleave:=InterleaveSpinEdit.Value;
  with WorksheetComboBox do if (ItemIndex>=0)
  and (Items.Objects[ItemIndex] is TContainer)
  then Serie.Container:=Items.Objects[ItemIndex] as TContainer;
  Serie.PointVisible:=PointsCheckBox.Checked;
  Serie.LineVisible:=LineCheckBox.Checked;
  Serie.Text:=CaptionEdit.Text;
  Serie.Brush.Color:=PointColorBuf;
  Serie.Pen.Color:=LineColorBuf;
  Serie.PointSize:=PointSizeSpinEdit.Value;
  Serie.PointType:=TPointType(PointTypeComboBox.ItemIndex);
  Serie.Pen.Width:=LineWidthSpinEdit.Value;
  case LineTypeComboBox.ItemIndex of
    0: Serie.Pen.Style:=psSolid;
    1: Serie.Pen.Style:=psDash;
    2: Serie.Pen.Style:=psDot;
    3: Serie.Pen.Style:=psDashDot;
  end;
  Serie.PMin:=StrToFloat(PMinEdit.Text);
  Serie.PMax:=StrToFloat(PMaxEdit.Text);
  Serie.XAxis:=TXAxis(XAxisRadioGroup.ItemIndex);
  Serie.YAxis:=TYAxis(YAxisRadioGroup.ItemIndex);
  BX:=true; 
  BY:=true; // add expressions to historylist
  with XExpressionComboBox.Items do for I:=0 to Count-1 do
  begin
    if Strings[I]=XExpressionComboBox.Text then BX:=false;
    if Strings[I]=YExpressionComboBox.Text then BY:=false;
  end;
  with XExpressionComboBox do // result saved only in XExpressionComboBox!
  begin
    if BX and (Text<>'') 
    then Items.Insert(0, Text);
    if BY and (YExpressionComboBox.Text<>'')
    then Items.Insert(0, YExpressionComboBox.Text);
  end;      
end;

// data tab handlers

procedure TSeriePropsForm.RadioButtonClick(Sender: TObject);
begin
  DataPanel.Visible:=DataRadioButton.Checked;
  ParameterPanel.Visible:=ParameterRadioButton.Checked;
  XColumnComboBox.Enabled:=DataRadioButton.Checked;
  XColumnLabel.Enabled:=DataRadioButton.Checked;
  YColumnComboBox.Enabled:=DataRadioButton.Checked;
  YColumnLabel.Enabled:=DataRadioButton.Checked;
  XErrorBarColumnComboBox.Enabled:=DataRadioButton.Checked;
  XErrorBarColumnLabel.Enabled:=DataRadioButton.Checked;
  XErrorBarColumnSpeedButton.Enabled:=DataRadioButton.Checked;
  YErrorBarColumnComboBox.Enabled:=DataRadioButton.Checked;
  YErrorBarColumnLabel.Enabled:=DataRadioButton.Checked;
  YErrorBarColumnSpeedButton.Enabled:=DataRadioButton.Checked;
  LeaderCheckBox.Enabled:=DataRadioButton.Checked;
  LeaderPanel.Enabled:=DataRadioButton.Checked;
end;

procedure TSeriePropsForm.WorksheetComboBoxDropDown(Sender: TObject);
var 
  I, N: integer; // correct list size for long fnames (font must be fixed!)
begin
  N:=0; 
  with Sender as TComboBox do
  begin
    for I:=0 to Items.Count-1 do
    if N<length(Items[I])
    then N:=length(Items[I]);   // get max string length
    N:=Round(-N*Font.Height/1.5); 
    if N<Width 
    then N:=Width;
    SendMessage(Handle,CB_SETDROPPEDWIDTH,N,0); // do it!
  end;
end;

procedure TSeriePropsForm.WorksheetComboBoxChange(Sender: TObject);
begin
  with WorksheetComboBox do 
  if (ItemIndex>=0)
    and (Items.Objects[ItemIndex] is TContainer) then
  with (Items.Objects[ItemIndex] as TContainer) do
  begin
    FirstLineSpinEdit.MaxValue:=Items.Count-1;
    with FirstLineSpinEdit do 
    if Value>MaxValue 
    then Value:=MaxValue;
    LastLineSpinEdit.MaxValue:=Items.Count-1;
    with LastLineSpinEdit do 
    if Value>MaxValue 
    then Value:=MaxValue;
  end else
  begin
    FirstLineSpinEdit.MaxValue:=-1; 
    LastLineSpinEdit.MaxValue:=-1;
  end;
end;

// view tab handlers

procedure TSeriePropsForm.PointPanelCheckBoxClick(Sender: TObject);
var 
  I: integer; 
  PropInfo: PPropInfo;
begin
  for I:=0 to PointsPanel.ControlCount-1 do
  begin
    PropInfo:=GetPropInfo(PointsPanel.Controls[I].ClassInfo,'Enabled');
    if (PropInfo<>nil) 
    then SetOrdProp(PointsPanel.Controls[I],
      PropInfo, integer(PointsCheckBox.Checked));
  end;
  PreviewPaintBoxPaint(nil);
end;

procedure TSeriePropsForm.PreviewPaintBoxPaint(Sender: TObject);
begin
  with PreviewPaintBox do
  begin
    Canvas.Brush.Color:=(FSerie.Collection as TSeries).Plot.Color;
    Canvas.FillRect(Rect(0,0,Width,Height));
    Canvas.Pen.Assign(FSerie.Pen);
    Canvas.Pen.Color:=LineColorBuf;
    case LineTypeComboBox.ItemIndex of
      0: Canvas.Pen.Style:=psSolid;
      1: Canvas.Pen.Style:=psDash;
      2: Canvas.Pen.Style:=psDot;
      3: Canvas.Pen.Style:=psDashDot;
    end;
    Canvas.Pen.Width:=LineWidthSpinEdit.Value;
    Canvas.Brush.Style:=bsClear;
    Canvas.MoveTo(1, Height div 2);
    if LineCheckBox.Checked then Canvas.LineTo(Width-1, Height div 2);
    Canvas.Brush.Color:=PointColorBuf;
    Canvas.Brush.Style:=FSerie.Brush.Style;
    Canvas.Pen.Style:=psSolid;
    if PointsCheckBox.Checked 
    then DrawPoint(Canvas, Width div 2, Height div 2, PointSizeSpinEdit.Value,
      TPointType(PointTypeComboBox.ItemIndex)); 
  end;
end;

procedure TSeriePropsForm.ColorGridChange(Sender: TObject);
begin
  if Sender=PointsColorGrid 
  then PointColorBuf:=PointsColorGrid.ForegroundColor;
  if Sender=LineColorGrid 
  then LineColorBuf:=LineColorGrid.ForegroundColor;
  PreviewPaintBoxPaint(nil);
end;

procedure TSeriePropsForm.PointsColorBitBtnClick(Sender: TObject);
begin
  ColorDialog.Color:=PointColorBuf; 
  if not ColorDialog.Execute 
  then Exit;
  PointsColorGrid.ForegroundIndex:=-1; 
  PointColorBuf:=ColorDialog.Color;
  PreviewPaintBoxPaint(nil);
end;

procedure TSeriePropsForm.LineColorBitBtnClick(Sender: TObject);
begin
  ColorDialog.Color:=LineColorBuf; 
  if not ColorDialog.Execute 
  then Exit;
  LineColorGrid.ForegroundIndex:=-1; 
  LineColorBuf:=ColorDialog.Color;
  PreviewPaintBoxPaint(nil);
end;

procedure TSeriePropsForm.LineTypeComboBoxDrawItem(Control: TWinControl;
  Index: Integer; R: TRect; State: TOwnerDrawState);
var 
  PSBuf: TPenStyle;
begin
  with LineTypeComboBox do
  begin
    Canvas.FillRect(R); 
    PSBuf:=Canvas.Pen.Style;
    case Index of
      0: Canvas.Pen.Style:=psSolid;
      1: Canvas.Pen.Style:=psDash;
      2: Canvas.Pen.Style:=psDot;
      3: Canvas.Pen.Style:=psDashDot;
    end;
    Canvas.MoveTo(R.Left, R.Top+(R.Bottom-R.Top) div 2);
    Canvas.LineTo(R.Right, R.Top+(R.Bottom-R.Top) div 2);
    Canvas.Pen.Style:=PSBuf;
  end;
end;

procedure TSeriePropsForm.PointTypeComboBoxDrawItem(Control: TWinControl;
  Index: Integer; R: TRect; State: TOwnerDrawState);
begin
  with PointTypeComboBox do
  begin
    Canvas.FillRect(R);
    DrawPoint(Canvas, (R.Left+R.Right) div 2, R.Top+((R.Bottom-R.Top) div 2), 
      ((R.Bottom-R.Top) div 2)+2, TPointType(Index));
  end;
end;

procedure TSeriePropsForm.HTMLTextBitBtnClick(Sender: TObject);
var
  S: string;
begin
  S:=CaptionEdit.Text;
  HTMLTextForm:=THTMLTextForm.Create(Application);
  try
    HTMLTextForm.HelpBitBtn.OnClick:=HelpBitBtn.OnClick;
    if HTMLTextForm.Execute(S)
    then CaptionEdit.Text:=S;
  finally
    HTMLTextForm.Free;
  end;
end;

procedure TSeriePropsForm.XErrorBarColumnSpeedButtonClick(Sender: TObject);
begin
  XErrorBarColumnComboBox.ItemIndex:=-1;
end;

procedure TSeriePropsForm.YErrorBarColumnSpeedButtonClick(Sender: TObject);
begin
  YErrorBarColumnComboBox.ItemIndex:=-1;
end;

procedure TSeriePropsForm.AreaComboBoxDrawItem(Control: TWinControl; Index:
  Integer; Rect: TRect; State: TOwnerDrawState);

  procedure doPAx;
  begin
    AreaComboBox.Canvas.Pen.Style:=psSolid;
    AreaComboBox.Canvas.Pen.Color:=clBlack;
    AreaComboBox.Canvas.Pen.Width:=2;
    AreaComboBox.Canvas.Rectangle(Rect);
    AreaComboBox.Canvas.Pen.Color:=clRed;
  end;
  
var
  LegWidth: integer;
  Serie: TSerie;  
begin
// !! warning: initially tools are configured for correct state painting!!
  LegWidth:=AreaComboBox.Height*2; // legend area width
  // draw text portion (with state)
  AreaComboBox.Canvas.FillRect(Rect);
  AreaComboBox.Canvas.TextOut(Rect.Left+LegWidth, Rect.Top, 
    AreaComboBox.Items[Index]);
  AreaComboBox.Canvas.Brush.Color:=(FSerie.Collection as TSeries).Plot.Color;
  InflateRect(Rect, -1, -2);
  Rect.Right:=LegWidth-2; // legend area - prepare
  AreaComboBox.Canvas.FillRect(Rect);
  // draw legend - icons
  if (integer(AreaComboBox.Items.Objects[Index])-fix)=abcNothing then
  begin
    AreaComboBox.Canvas.Pen.Style:=psSolid;
    AreaComboBox.Canvas.Pen.Color:=clRed;
    AreaComboBox.Canvas.Pen.Width:=2;
    AreaComboBox.Canvas.MoveTo(Rect.Left+(Rect.Right-Rect.Left) div 2 
      - (Rect.Bottom-Rect.Top) div 2, Rect.Top);
    AreaComboBox.Canvas.LineTo(Rect.Left+(Rect.Right-Rect.Left) div 2 
      + (Rect.Bottom-Rect.Top) div 2, Rect.Bottom);
    AreaComboBox.Canvas.MoveTo(Rect.Left+(Rect.Right-Rect.Left) div 2 
      - (Rect.Bottom-Rect.Top) div 2, Rect.Bottom);
    AreaComboBox.Canvas.LineTo(Rect.Left+(Rect.Right-Rect.Left) div 2 
      + (Rect.Bottom-Rect.Top) div 2, Rect.Top);
  end; 
  if (integer(AreaComboBox.Items.Objects[Index])-fix)=abcYAxis then
  begin
    doPAx;
    AreaComboBox.Canvas.MoveTo(Rect.Left, Rect.Top);
    AreaComboBox.Canvas.LineTo(Rect.Left, Rect.Bottom-1);
  end; 
  if (integer(AreaComboBox.Items.Objects[Index])-fix)=abcYAxis2 then
  begin
    doPAx;
    AreaComboBox.Canvas.MoveTo(Rect.Right-1, Rect.Top);
    AreaComboBox.Canvas.LineTo(Rect.Right-1, Rect.Bottom-1);
  end; 
  if (integer(AreaComboBox.Items.Objects[Index])-fix)=abcXAxis then
  begin
    doPAx;
    AreaComboBox.Canvas.MoveTo(Rect.Left, Rect.Bottom-1);
    AreaComboBox.Canvas.LineTo(Rect.Right-1, Rect.Bottom-1);
  end; 
  if (integer(AreaComboBox.Items.Objects[Index])-fix)=abcXAxis2 then
  begin
    doPAx;
    AreaComboBox.Canvas.MoveTo(Rect.Left, Rect.Top);
    AreaComboBox.Canvas.LineTo(Rect.Right-1, Rect.Top);
  end; 
  // draw legend - series
  if (integer(AreaComboBox.Items.Objects[Index])-fix)>=0 then
  begin
    Serie:=TSerie(FPlot.Series.FindItemID(integer(
      AreaComboBox.Items.Objects[Index])-fix));
    if not Assigned(Serie)
    then Exit; // NEVER should be...
    AreaComboBox.Canvas.Pen:=Serie.Pen;
    AreaComboBox.Canvas.Brush.Style:=bsClear;
    AreaComboBox.Canvas.MoveTo(Rect.Left, 
      Rect.Top+(Rect.Bottom-Rect.Top) div 2);
    if Serie.LineVisible 
    then AreaComboBox.Canvas.LineTo(Rect.Right-1, 
      Rect.Top+(Rect.Bottom-Rect.Top) div 2);
    AreaComboBox.Canvas.Brush:=Serie.Brush;
    AreaComboBox.Canvas.Pen.Style:=psSolid;
    if Serie.PointVisible
    then DrawPoint(AreaComboBox.Canvas, Rect.Left+(Rect.Right-Rect.Left) div 2, 
      Rect.Top+(Rect.Bottom-Rect.Top) div 2, Serie.PointSize, Serie.PointType);
  end; 
end;

procedure TSeriePropsForm.LeaderCheckBoxClick(Sender: TObject);
begin
  LeaderSpinEdit.Enabled:=LeaderCheckBox.Checked;
  LeaderLabel.Enabled:=LeaderCheckBox.Checked;
  if LeaderCheckBox.Checked
  then LeaderSpinEdit.Value:=LastLineSpinEdit.Value
  else LeaderSpinEdit.Value:=LeaderSpinEdit.MinValue;
end;

end.

