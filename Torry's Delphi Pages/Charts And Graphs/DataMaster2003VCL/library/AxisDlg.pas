///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit AxisDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList, StdCtrls, Buttons, Spin, ExtCtrls, ColorGrd, DMPlot,
  DMFloatEdit, DMComboBox;

type
  TAxisPropsForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    HelpBitBtn: TBitBtn;
    ImageList: TImageList;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    PageControl: TPageControl;
    ScaleTabSheet: TTabSheet;
    LabelsTabSheet: TTabSheet;
    LineTabSheet: TTabSheet;
    ManualScalePanel: TPanel;
    ManualScaleRadioButton: TRadioButton;
    MinScaleLabel: TLabel;
    MaxScaleLabel: TLabel;
    AutoScalePanel: TPanel;
    MarginsLabel: TLabel;
    AutoScaleRadioButton: TRadioButton;
    ExpressionLabel: TLabel;
    FontBitBtn: TBitBtn;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    LineWidthLabel: TLabel;
    LineWidthSpinEdit: TSpinEdit;
    TicksGroupBox: TGroupBox;
    MajorTicksLabel: TLabel;
    MajorTicksSpinEdit: TSpinEdit;
    MinorTicksLabel: TLabel;
    MinorTicksSpinEdit: TSpinEdit;
    GridCheckBox: TCheckBox;
    LineColorGroupBox: TGroupBox;
    LineColorGrid: TColorGrid;
    LineColorBitBtn: TBitBtn;
    MarginsFloatEdit: TFloatEdit;
    MinScaleFloatEdit: TFloatEdit;
    MaxScaleFloatEdit: TFloatEdit;
    LabelsCheckBox: TCheckBox;
    VisibleCheckBox: TCheckBox;
    TicksRadioGroup: TRadioGroup;
    ExpressionComboBox: TExpressionComboBox;
    NumericFormatRadioButton: TRadioButton;
    DateTimeRadioButton: TRadioButton;
    NumericFormatPanel: TPanel;
    WidthLabel: TLabel;
    WidthSpinEdit: TSpinEdit;
    DigitsLabel: TLabel;
    DigitsSpinEdit: TSpinEdit;
    SampleLabel: TLabel;
    FormatRadioGroup: TRadioGroup;
    DateTimeFormatPanel: TPanel;
    DateTimeFormatLabel: TLabel;
    DateTimeFormatHelpLabel: TLabel;
    DateTimeFormatComboBox: TComboBox;
    SampleLabel1: TLabel;
    MinScaleDateTimePicker: TDateTimePicker;
    MaxScaleDateTimePicker: TDateTimePicker;
    HTMLTextBitBtn: TBitBtn;
    LinkedCheckBox: TCheckBox;
    ShowExpressionCheckBox: TCheckBox;
    InnerTicksCheckBox: TCheckBox;
    TickLengthLabel: TLabel;
    TickLengthFloatEdit: TFloatEdit;
    procedure LineColorGridChange(Sender: TObject);
    procedure LineColorBitBtnClick(Sender: TObject);
    procedure FontBitBtnClick(Sender: TObject);
    procedure FormatChange(Sender: TObject);
    procedure ScaleFloatEditChange(Sender: TObject);
    procedure LabelsCheckBoxClick(Sender: TObject);
    procedure TicksRadioGroupClick(Sender: TObject);
    procedure FormatRadioButtonClick(Sender: TObject);
    procedure DateTimeFormatHelpLabelMouseEnter(Sender: TObject);
    procedure DateTimeFormatHelpLabelMouseLeave(Sender: TObject);
    procedure DateTimeFormatComboBoxChange(Sender: TObject);
    procedure ScaleDateTimePickerChange(Sender: TObject);
    procedure HTMLTextBitBtnClick(Sender: TObject);
    procedure LinkedCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    FScaleIncrement: extended; // buffer set in Execute
    FPlotColorBuf: TColor; // used in HTMLTextBitBtnClick for preview painting
  public
    { Public declarations }
    function Execute(Axis: TAxis): boolean; // returns true if clicked OK
  end;

var 
  AxisPropsForm: TAxisPropsForm;

implementation

uses TextDlg, DateUtils, HTextDlg;

{$R *.DFM}

{ TAxisPropsForm }

function TAxisPropsForm.Execute(Axis: TAxis): boolean;
  function CheckScale: boolean; // true if scale is valid date/time
  var
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  begin
    Result:=false;
    DecodeDateTime(Axis.Min, AYear, AMonth, ADay, 
      AHour, AMinute, ASecond, AMilliSecond);
    if not IsValidDateTime(AYear, AMonth, ADay, 
      AHour, AMinute, ASecond, AMilliSecond)  
    then Exit;  
    DecodeDateTime(Axis.Max, AYear, AMonth, ADay, 
      AHour, AMinute, ASecond, AMilliSecond);
    if not IsValidDateTime(AYear, AMonth, ADay, 
      AHour, AMinute, ASecond, AMilliSecond)  
    then Exit;  
    Result:=true;
  end;
var 
  B: boolean; 
  I: integer;
begin
  // init FPlotColorBuf
  if Assigned(Axis.Plot)
  then FPlotColorBuf:=Axis.Plot.Color
  else FPlotColorBuf:=clBtnFace;
  // copy properties from axis
  MarginsFloatEdit.Value:=Axis.Margins; 
  MinScaleFloatEdit.Value:=Axis.Min;
  MaxScaleFloatEdit.Value:=Axis.Max;
  { note: if we would not check scale, DateTimePicker.DateTime:=Axis.Min
  on some values passed, but when DateTimePicker about to be displayed
  (in showmodal) it throws ECommonCalendarError which can't be processed.
  So in fact we try to invoke the exception where we can catch it. }
  try
    if CheckScale then
    begin
      MinScaleDateTimePicker.DateTime:=Axis.Min;
      MaxScaleDateTimePicker.DateTime:=Axis.Max;
    end else
      DateTimeFormatPanel.Font.Color:=clRed;
  except
    // Windows.Beep(1000, 200); for DEBUG only
    DateTimeFormatPanel.Font.Color:=clRed; 
  end;
  FScaleIncrement:=(Axis.Max-Axis.Min)/Axis.MajorTicks;
  if Axis.LogTicks then
  begin
    MinScaleFloatEdit.Multiply:=true;
    MaxScaleFloatEdit.Multiply:=true;
    MinScaleFloatEdit.Increment:=10;
    MaxScaleFloatEdit.Increment:=10;
  end else
  begin
    MinScaleFloatEdit.Multiply:=false;
    MaxScaleFloatEdit.Multiply:=false;
    MinScaleFloatEdit.Increment:=FScaleIncrement;
    MaxScaleFloatEdit.Increment:=FScaleIncrement;
  end;
  ExpressionComboBox.Text:=Axis.Expression;
  if Axis.SmartTicks // before format (see OnChange!)
  then TicksRadioGroup.ItemIndex:=1 else
    if Axis.LogTicks
    then TicksRadioGroup.ItemIndex:=2 
    else TicksRadioGroup.ItemIndex:=0;
  WidthSpinEdit.Value:=Axis.LabelWidth;
  DigitsSpinEdit.Value:=Axis.LabelDecimals;
  case Axis.labelType of
    ffGeneral: FormatRadioGroup.ItemIndex:=0;
    ffFixed: FormatRadioGroup.ItemIndex:=1;
    ffExponent: FormatRadioGroup.ItemIndex:=2;
  end;
  FormatChange(Self); // must be called AFTER setting format controls
  if Axis.LabelDateTime then
  begin
    DateTimeFormatComboBox.Text:=Axis.FormatString;
    DateTimeRadioButton.Checked:=true;
  end;  
  DateTimeFormatComboBoxChange(Self);
  AutoScaleRadioButton.Checked:=Axis.AutoScale;
  ManualScaleRadioButton.Checked:=not Axis.AutoScale; // else both may be off!
  CaptionEdit.Text:=Axis.Title;
  MajorTicksSpinEdit.Value:=Axis.MajorTicks;
  MinorTicksSpinEdit.Value:=Axis.MinorTicks;
  LineWidthSpinEdit.Value:=Axis.Pen.Width;
  GridCheckBox.Checked:=Axis.ShowGrid;
  LineColorGrid.ForegroundIndex:=-1;
  ColorDialog.Color:=Axis.Pen.Color;
  FontDialog.Font:=Axis.Font;
  LabelsCheckBox.Checked:=Axis.LabelVisible;
  VisibleCheckBox.Checked:=Axis.Visible;
  ShowExpressionCheckBox.Checked:=Axis.ShowExpression;
  LabelsCheckBoxClick(nil);
  LinkedCheckBox.Checked:=Axis.IsLinked;
  InnerTicksCheckBox.Checked:=Axis.InnerTicks;
  TickLengthFloatEdit.Value:=Axis.TickLength+1;
  Result:=ShowModal=mrOK; 
  if not Result then Exit; // cancelled
  // copy properties to axis
  Axis.TickLength:=TickLengthFloatEdit.Value-1;
  Axis.InnerTicks:=InnerTicksCheckBox.Checked;
  Axis.IsLinked:=LinkedCheckBox.Checked;
  Axis.ShowExpression:=ShowExpressionCheckBox.Checked;
  Axis.Margins:=MarginsFloatEdit.Value;   
  Axis.Expression:=ExpressionComboBox.Text;
  Axis.AutoScale:=AutoScaleRadioButton.Checked;
  Axis.LabelDateTime:=DateTimeRadioButton.Checked;
  if NumericFormatRadioButton.Checked then
  begin
    Axis.Min:=MinScaleFloatEdit.Value;
    Axis.Max:=MaxScaleFloatEdit.Value;
    Axis.LabelWidth:=WidthSpinEdit.Value;
    Axis.LabelDecimals:=DigitsSpinEdit.Value;
    case FormatRadioGroup.ItemIndex of
      0: Axis.labelType:=ffGeneral;
      1: Axis.labelType:=ffFixed;
      2: Axis.labelType:=ffExponent;
    end;
  end else
  begin
    Axis.Min:=MinScaleDateTimePicker.DateTime;
    Axis.Max:=MaxScaleDateTimePicker.DateTime;
    Axis.FormatString:=DateTimeFormatComboBox.Text;
  end;
  Axis.Title:=CaptionEdit.Text;
  Axis.MajorTicks:=MajorTicksSpinEdit.Value;
  Axis.MinorTicks:=MinorTicksSpinEdit.Value;
  Axis.Pen.Width:=LineWidthSpinEdit.Value;
  Axis.ShowGrid:=GridCheckBox.Checked;
  Axis.Pen.Color:=ColorDialog.Color;
  Axis.Font:=FontDialog.Font;
  Axis.LabelVisible:=LabelsCheckBox.Checked;
  case TicksRadioGroup.ItemIndex of
    0: 
      begin
        Axis.LogTicks:=false; 
        Axis.SmartTicks:=false; 
      end;
    1:
      Axis.SmartTicks:=true;
    2:
      Axis.LogTicks:=true;    
  end; 
  Axis.Visible:=VisibleCheckBox.Checked;
  B:=true; // check expression for duplication and add to the top of list
  with ExpressionComboBox.Items do 
  for I:=0 to Count-1 do
  if Strings[I]=ExpressionComboBox.Text 
  then B:=false;
  if B and (ExpressionComboBox.Text<>'')
  then ExpressionComboBox.Items.Insert(0, ExpressionComboBox.Text);
end;

procedure TAxisPropsForm.LineColorGridChange(Sender: TObject);
begin 
  ColorDialog.Color:=LineColorGrid.ForegroundColor; 
end;

procedure TAxisPropsForm.LineColorBitBtnClick(Sender: TObject);
begin 
  ColorDialog.Execute; 
  LineColorGrid.ForegroundIndex:=-1; 
end;

procedure TAxisPropsForm.FontBitBtnClick(Sender: TObject);
begin 
  FontDialog.Execute; 
end;

procedure TAxisPropsForm.FormatChange(Sender: TObject);
var
  F: TFloatFormat; 
begin
  (*
  case FormatRadioGroup.ItemIndex of // update scale editors
    0: MinScaleFloatEdit.FType:=ffGeneral;
    1: MinScaleFloatEdit.FType:=ffFixed;
    2: MinScaleFloatEdit.FType:=ffExponent;
  end;{case}
  MinScaleFloatEdit.MinWidth:=WidthSpinEdit.Value;
  MinScaleFloatEdit.Decimals:=DigitsSpinEdit.Value;
  MaxScaleFloatEdit.FType:=MinScaleFloatEdit.FType;
  MaxScaleFloatEdit.MinWidth:=MinScaleFloatEdit.MinWidth;
  MaxScaleFloatEdit.Decimals:=MinScaleFloatEdit.Decimals;
  with MaxScaleFloatEdit do 
  SampleLabel.Caption:=FloatToStrF((Value+
    MinScaleFloatEdit.Value)/2,FType,MinWidth,Decimals); // update sample
  Because changing format may cause parasitic rounding, we will NOT use it!
  *)
  case FormatRadioGroup.ItemIndex of 
    0: F:=ffGeneral;
    1: F:=ffFixed;
    2: F:=ffExponent;
  else
    F:=ffGeneral;  
  end;
  SampleLabel.Caption:=FloatToStrF((MinScaleFloatEdit.Value+
    MaxScaleFloatEdit.Value)/2, F, WidthSpinEdit.Value, DigitsSpinEdit.Value);
  if F=ffFixed // correct to avoid rounding
  then F:=ffGeneral;
  MinScaleFloatEdit.FType:=F;
  MaxScaleFloatEdit.FType:=F;
end;

procedure TAxisPropsForm.ScaleFloatEditChange(Sender: TObject);
begin
  if (Sender as TFloatEdit).Focused 
  then ManualScaleRadioButton.Checked:=true;
end;

procedure TAxisPropsForm.LabelsCheckBoxClick(Sender: TObject);
begin
  GridCheckBox.Enabled:=LabelsCheckBox.Checked;
  if not LabelsCheckBox.Checked 
  then GridCheckBox.Checked:=false;
end;

procedure TAxisPropsForm.TicksRadioGroupClick(Sender: TObject);
begin
  if TicksRadioGroup.ItemIndex=2 then
  begin
    MinorTicksSpinEdit.Value:=9;
    MinScaleFloatEdit.Multiply:=true;
    MaxScaleFloatEdit.Multiply:=true;
    MinScaleFloatEdit.Increment:=10;
    MaxScaleFloatEdit.Increment:=10;
  end else 
  begin
    MinScaleFloatEdit.Multiply:=false;
    MaxScaleFloatEdit.Multiply:=false;
    MinScaleFloatEdit.Increment:=FScaleIncrement;
    MaxScaleFloatEdit.Increment:=FScaleIncrement;
    MinorTicksSpinEdit.Value:=2;
  end;  
end;

procedure TAxisPropsForm.FormatRadioButtonClick(Sender: TObject);
begin
  NumericFormatPanel.Visible:=NumericFormatRadioButton.Checked;
  DateTimeFormatPanel.Visible:=DateTimeRadioButton.Checked;
  MinScaleDateTimePicker.Visible:=DateTimeRadioButton.Checked;
  MaxScaleDateTimePicker.Visible:=DateTimeRadioButton.Checked;
  MinScaleFloatEdit.Visible:=NumericFormatRadioButton.Checked;
  MaxScaleFloatEdit.Visible:=NumericFormatRadioButton.Checked;
end;

procedure TAxisPropsForm.DateTimeFormatHelpLabelMouseEnter(Sender: TObject);
begin
  DateTimeFormatHelpLabel.Font.Color:=clBlue;
end;

procedure TAxisPropsForm.DateTimeFormatHelpLabelMouseLeave(Sender: TObject);
begin
  DateTimeFormatHelpLabel.Font.Color:=clNavy;
end;

procedure TAxisPropsForm.DateTimeFormatComboBoxChange(Sender: TObject);
begin
  SampleLabel1.Caption:=FormatDateTime(DateTimeFormatComboBox.Text,
    (MinScaleDateTimePicker.DateTime+MaxScaleDateTimePicker.DateTime)/2);
end; // note: we use ^ DateTimePickers, not FloatEdits!

procedure TAxisPropsForm.ScaleDateTimePickerChange(Sender: TObject);
begin
  if (Sender as TDateTimePicker).Focused 
  then ManualScaleRadioButton.Checked:=true;
end;

procedure TAxisPropsForm.HTMLTextBitBtnClick(Sender: TObject);
var
  S: string;
begin
  S:=CaptionEdit.Text;
  HTMLTextForm:=THTMLTextForm.Create(Application);
  try
    HTMLTextForm.HelpBitBtn.OnClick:=HelpBitBtn.OnClick;
    if HTMLTextForm.Execute(S, FPlotColorBuf, FontDialog.Font)
    then CaptionEdit.Text:=S;
  finally
    HTMLTextForm.Free;
  end;
end;

procedure TAxisPropsForm.LinkedCheckBoxClick(Sender: TObject);
begin
  AutoScaleRadioButton.Enabled:=not LinkedCheckBox.Checked;
  AutoScalePanel.Enabled:=not LinkedCheckBox.Checked;
  ManualScaleRadioButton.Enabled:=not LinkedCheckBox.Checked;
  ManualScalePanel.Enabled:=not LinkedCheckBox.Checked;
  TicksGroupBox.Enabled:=not LinkedCheckBox.Checked;
  TicksRadioGroup.Enabled:=not LinkedCheckBox.Checked;
  GridCheckBox.Enabled:=not LinkedCheckBox.Checked;
  if LinkedCheckBox.Checked
  then GridCheckBox.Checked:=false;  
end;

end.
 