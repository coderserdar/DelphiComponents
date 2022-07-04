///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMPlot;

{$B-}

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms, Contnrs, DMContainer;

type
  TPlotHintEvent = procedure (Sender: TObject; const H: string) of object;
  TGetPointEvent = function (Sender: TObject; Point, Serie: integer; var X, Y: extended): Boolean of object;
  TPointClickEvent = function (Sender: TObject; Point, Serie: integer): Boolean of object;
  TPointType=(ptSquare, ptCircle, ptCross, ptXCross, ptAsterisk); // point shapes
  TPlotMouseMode=(pmNone, pmAutoZoom, pmZoom, pmRuler, pmUnZoom, pmSelect, 
    pmPointClick, pmPointEdit, pmPointDelete, pmTranslate, pmMargins, pmLabelEdit);
  TAxisKind=(axLeft, axRight, axTop, axBottom);
  TXAxis=(BottomAxis, TopAxis);
  TYAxis=(LeftAxis, RightAxis);
  TPlotCopyModes=(pcmPage, pcmPoints, pcmLines, pcmSerie, pcmLabel, pcmUseTabs);
  TPlotCopyFlags=set of TPlotCopyModes;
  TLabelKind=(lkText, lkLegend, lkArrow, lkBalloon, lkLine, lkRectangle, lkEllipse, lkVText);
  TLabelValues=array of TReal;
  TLabelCoordinates=(lcX1, lcY1, lcX2, lcY2); // for TPlotLabel.Scale
  TLabelPinMode=(lpmFrame, lpmAxis, lpmScale, lpmAxis2, lpmScale2);
  TLabelAlignment=(laTopLeft, laTopRight, laBottomLeft, laBottomRight, laCenter,
    laLeftCenter, laTopCenter, laRightCenter, laBottomCenter);

const
  DefLineVisible=true;
  DefXColumn=0;
  DefYColumn=0;
  DefFirstLine=0;
  DefLastLine=-1;
  DefPointSize=5;
  DefPointVisible=true;
  DefInterleave=1;
  DefPointType=ptSquare;
  DefIsFunction=false;
  DefMinorTicks=5;
  DefMajorTicks=10;
  DefLabelWidth=5;
  DefLabelDecimals=2;
  DefLabelType=ffGeneral;
  DefMouseMode=pmNone;
  DefBorderStyle=bsSingle;
  DefTitleMargin=$FFFF;
  DefLabelMargin=$FFFF;
  // TSerie.AreaBorder codes
  abcXAxis=-1;
  abcYAxis=-2;
  abcXAxis2=-3;
  abcYAxis2=-4;
  abcNothing=-5;

type
  TPlot=class;

  TAxis = class (TPersistent)
  private
    FPlot: TPlot;
    FFormat: TFormat;
    FMin: Extended;
    FMax: Extended;
    FMargins: Extended;
    FPen: TPen;
    FFont: TFont;
    FMajorTicks: Integer;
    FMinorTicks: Integer;
    FAutoScale: Boolean;
    FShowGrid: Boolean;
    FTitle: string;
    FExpression: string;
    FSmartTicks: Boolean;
    FLogTicks: Boolean;
    FLabelVisible: Boolean;
    FVisible: Boolean;
    FLength: Integer;
    FOrigin: Integer;
    FVertical: Boolean;
    FLabelDateTime: Boolean;
    FDateTimeFormat: string;
    FShowExpression: Boolean;
    FTitleMargin: Integer;
    FTitleRect: TRect;
    FIsLinked: Boolean;
    FInnerTicks: Boolean;
    FLabelMargin: Integer;
    FLabelsRect: TRect;
    FTickLength: Extended;
    procedure OnChanged(Sender: TObject);
    procedure InitLabels(var MajorTicksPositions: TLabelValues);
    procedure CalculateLabelsWidth(Canvas: TCanvas; var W, H: integer);
    function FormatLabel(Value: extended): string;
    function GetWidth: Integer;
    function GetDecimals: Integer;
    function GetFType: TFloatFormat;
    procedure SetMin(const Value: Extended);
    procedure SetMax(const Value: Extended);
    procedure SetPen(const Value: TPen);
    procedure SetFont(const Value: TFont);
    procedure SetMinorTicks(const Value: Integer);
    procedure SetMajorTicks(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetDecimals(const Value: Integer);
    procedure SetFType(const Value: TFloatFormat);
    procedure SetFormat(const Value: TFormat);
    procedure SetAutoScale(const Value: Boolean);
    procedure SetShowGrid(const Value: Boolean);
    procedure SetMargins(const Value: Extended);
    procedure SetTitle(const Value: string);
    procedure SetExpression(const Value: string);
    procedure SetSmartTicks(const Value: Boolean);
    procedure SetLogTicks(const Value: Boolean);
    procedure SetLabelVisible(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    function GetFormatString: string;
    procedure SetFormatString(Value: string);
    procedure SetLabelDateTime(Value: Boolean);
    procedure SetShowExpression(Value: Boolean);
    function AutoMarginT: Boolean;
    function AutoMarginL: Boolean;
    procedure SetTitleMargin(Value: Integer);
    procedure PaintTitleFrame(const Canvas: TCanvas);
    procedure SetIsLinked(Value: Boolean);
    procedure SetInnerTicks(Value: Boolean);
    procedure SetLabelMargin(Value: Integer);
    procedure PaintLabelsFrame(const Canvas: TCanvas);
    procedure SetTickLength(Value: Extended);
  public
    property Format: TFormat read FFormat write SetFormat;
    constructor Create(APlot: TPlot);
    destructor Destroy; override;
    procedure Assign(A: TPersistent); override;
    procedure Paint(const Canvas: TCanvas; const X,Y,Length,GridSize: integer; Kind: TAxisKind);
    function Scale(Value: TReal; Abs: boolean=true): Integer; overload;
    function Scale(Value: integer): TReal; overload;
    function InRange(const Value: extended): Boolean;
    function GetLinkedAxis: TAxis;
    property Plot: TPlot read FPlot;
  published
    property Min: Extended read FMin write SetMin;
    property Max: Extended read FMax write SetMax;
    property Pen: TPen read FPen write SetPen;
    property Font: TFont read FFont write SetFont;
    property MinorTicks: Integer read FMinorTicks write SetMinorTicks default DefMinorTicks;
    property MajorTicks: Integer read FMajorTicks write SetMajorTicks default DefMajorTicks;
    property LabelDateTime: Boolean read FLabelDateTime write SetLabelDateTime default false;
    property LabelWidth: Integer read GetWidth write SetWidth default DefLabelWidth;
    property LabelDecimals: Integer read GetDecimals write SetDecimals default DefLabelDecimals;
    property LabelType: TFloatFormat read GetFType write SetFType default DefLabelType;
    property FormatString: string read GetFormatString write SetFormatString stored FLabelDateTime;
    property AutoScale: Boolean read FAutoScale write SetAutoScale default false;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default false;
    property Margins: Extended read FMargins write SetMargins;
    property Title: string read FTitle write SetTitle;
    property Expression: string read FExpression write SetExpression;
    property SmartTicks: Boolean read FSmartTicks write SetSmartTicks default false;
    property LogTicks: Boolean read FLogTicks write SetLogTicks default false;
    property Visible: Boolean read FVisible write SetVisible default true;
    property LabelVisible: Boolean read FLabelVisible write SetLabelVisible default true;
    property ShowExpression: Boolean read FShowExpression write SetShowExpression default true;
    property TitleMargin: Integer read FTitleMargin write SetTitleMargin stored AutoMarginT;
    property IsLinked: Boolean read FIsLinked write SetIsLinked default false;
    property InnerTicks: Boolean read FInnerTicks write SetInnerTicks default false;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin stored AutoMarginL;
    property TickLength: Extended read FTickLength write SetTickLength;
  end;
  
  TSerie = class (TCollectionItem)
  private
    FText: string;
    FPointType: TPointType;
    FPointVisible: Boolean;
    FLineVisible: Boolean;
    FIsFunction: Boolean;
    FXColumn: Integer;
    FYColumn: Integer;
    FFirstLine: Integer;
    FLastLine: Integer;
    FInterleave: Integer;
    FPointSize: Integer;
    FPen: TPen;
    FBrush: TBrush;
    FXExpression: string;
    FYExpression: string;
    FContainer: TContainer;
    FShowBestFit: Boolean;
    FPMax: Extended;
    FPMin: Extended;
    FYAxis: TYAxis;
    FXAxis: TXAxis;
    FVisible: Boolean;
    FIsRecording: Boolean;
    FLeaderPosition: Integer;
    FXErrorColumn: Integer;
    FYErrorColumn: Integer;
    FAreaBorder: Integer;
    procedure OnChanged(Sender: TObject);
    procedure SetPointType(const Value: TPointType);
    procedure SetPointVisible(const Value: Boolean);
    procedure SetLineVisible(const Value: Boolean);
    procedure SetIsFunction(const Value: Boolean);
    procedure SetXColumn(const Value: Integer);
    procedure SetYColumn(const Value: Integer);
    procedure SetFirstLine(const Value: Integer);
    procedure SetLastLine(const Value: Integer);
    procedure SetPointSize(const Value: Integer);
    procedure SetInterleave(const Value: Integer);
    procedure SetPen(const Value: TPen);
    procedure SetBrush(const Value: TBrush);
    procedure SetXExpression(const Value: string);
    procedure SetYExpression(const Value: string);
    procedure SetContainer(const Value: TContainer);
    procedure SetText(const Value: string);
    procedure SetPMax(const Value: Extended);
    procedure SetPMin(const Value: Extended);
    procedure SetXAxis(const Value: TXAxis);
    procedure SetYAxis(const Value: TYAxis);
    function _XAxis: TAxis;
    function _YAxis: TAxis;
    procedure SetVisible(const Value: Boolean);
    procedure SetIsRecording(const Value: Boolean);
    procedure DrawLeader(const Canvas: TCanvas);
    procedure SetLeaderPosition(Value: Integer);
    procedure SetXErrorColumn(Value: Integer);
    procedure SetYErrorColumn(Value: Integer);
    procedure SetAreaBorder(Value: Integer);
    function GetAreaBorder: Integer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetClipboardFormat: Word;
    procedure CopyToClipboard;
    function PasteFromClipboard(DataOnly: boolean): Boolean;
    function Scale(var Min, Max: TRealPoint): Boolean;
    procedure Paint(const Canvas: TCanvas);
    procedure AddPoint;
    procedure ClearBlock;
    function Empty: Boolean;
    procedure PaintArea(const Canvas: TCanvas);
  published
    property PointType: TPointType read FPointType write SetPointType default DefPointType;
    property PointVisible: Boolean read FPointVisible write SetPointVisible default DefPointVisible;
    property PointSize: Integer read FPointSize write SetPointSize default DefPointSize;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default DefLineVisible;
    property FirstLine: Integer read FFirstLine write SetFirstLine default DefFirstLine;
    property LastLine: Integer read FLastLine write SetLastLine default DefLastLine;
    property Interleave: Integer read FInterleave write SetInterleave default DefInterleave;
    property ShowBestFit: Boolean read FShowBestFit write FShowBestFit default false;
    property IsFunction: Boolean read FIsFunction write SetIsFunction default DefIsFunction;
    property XColumn: Integer read FXColumn write SetXColumn default DefXColumn;
    property YColumn: Integer read FYColumn write SetYColumn default DefYColumn;
    property Pen: TPen read FPen write SetPen;
    property Brush: TBrush read FBrush write SetBrush;
    property Text: string read FText write SetText;
    property XExpression: string read FXExpression write SetXExpression;
    property YExpression: string read FYExpression write SetYExpression;
    property Container: TContainer read FContainer write SetContainer;
    property PMin: Extended read FPMin write SetPMin;
    property PMax: Extended read FPMax write SetPMax;
    property XAxis: TXAxis read FXAxis write SetXAxis default BottomAxis;
    property YAxis: TYAxis read FYAxis write SetYAxis default LeftAxis;
    property Visible: Boolean read FVisible write SetVisible default true;
    property IsRecording: Boolean read FIsRecording write SetIsRecording default false;
    property LeaderPosition: Integer read FLeaderPosition write SetLeaderPosition default DefLastLine;
    property XErrorColumn: Integer read FXErrorColumn write SetXErrorColumn default DefXColumn;
    property YErrorColumn: Integer read FYErrorColumn write SetYErrorColumn default DefYColumn;
    property AreaBorder: Integer read GetAreaBorder write SetAreaBorder default abcNothing;
  end;
  
  TPlotLabel = class (TCollectionItem)
  private
    FFont: TFont;
    FText: string;
    FPen: TPen;
    FBrush: TBrush;
    FVisible: Boolean;
    FX1: Extended;
    FX2: Extended;
    FY1: Extended;
    FY2: Extended;
    FLabelKind: TLabelKind;
    FSize: TRect;
    FPinModeX1: TLabelPinMode;
    FPinModeY1: TLabelPinMode;
    FPinModeX2: TLabelPinMode;
    FPinModeY2: TLabelPinMode;
    FTransparent: Boolean;
    FAlignment: TLabelAlignment;
    procedure OnChanged(Sender: TObject);
    function _XAxis(LabelCoordinate: TLabelCoordinates): TAxis;
    function _YAxis(LabelCoordinate: TLabelCoordinates): TAxis;
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
    procedure SetBrush(const Value: TBrush);
    procedure SetText(const Value: string);
    function GetXAxis: TXAxis;
    procedure SetXAxis(const Value: TXAxis);
    function GetYAxis: TYAxis;
    procedure SetYAxis(const Value: TYAxis);
    procedure SetVisible(const Value: Boolean);
    procedure SetX1(Value: Extended);
    procedure SetX2(Value: Extended);
    procedure SetY1(Value: Extended);
    procedure SetY2(Value: Extended);
    procedure SetLabelKind(Value: TLabelKind);
    procedure PaintMarkers(const Canvas: TCanvas);
    procedure _Move(Plt: TPlot);
    procedure SetPinModeX1(Value: TLabelPinMode);
    procedure SetPinModeY1(Value: TLabelPinMode);
    procedure SetPinModeX2(Value: TLabelPinMode);
    procedure SetPinModeY2(Value: TLabelPinMode);
    procedure SetTransparent(Value: Boolean);
    procedure SetAlignment(Value: TLabelAlignment);
  protected
    function GetDisplayName: string; override;
  public
    function Belong(iX, iY: integer): Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetClipboardFormat: Word;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure Paint(const Canvas: TCanvas; Width, Height: integer);
    function Scale(LabelCoordinate: TLabelCoordinates; Size: integer=0; Canvas: TCanvas=nil): Integer;
  published
    property LabelKind: TLabelKind read FLabelKind write SetLabelKind default lkText;
    property Pen: TPen read FPen write SetPen;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property XAxis: TXAxis read GetXAxis write SetXAxis stored False;
    property YAxis: TYAxis read GetYAxis write SetYAxis stored False;
    property Visible: Boolean read FVisible write SetVisible default true;
    property X1: Extended read FX1 write SetX1;
    property X2: Extended read FX2 write SetX2;
    property Y1: Extended read FY1 write SetY1;
    property Y2: Extended read FY2 write SetY2;
    property PinModeX1: TLabelPinMode read FPinModeX1 write SetPinModeX1;
    property PinModeY1: TLabelPinMode read FPinModeY1 write SetPinModeY1;
    property PinModeX2: TLabelPinMode read FPinModeX2 write SetPinModeX2;
    property PinModeY2: TLabelPinMode read FPinModeY2 write SetPinModeY2;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Alignment: TLabelAlignment read FAlignment write SetAlignment default laTopLeft;
  end;
  
  TSeries = class (TCollection)
  private
    FPlot: TPlot;
    function GetItem(Index: Integer): TSerie;
    procedure SetItem(Index: Integer; Value: TSerie);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(APlot: TPlot);
    function Add: TSerie;
    property Items[Index: Integer]: TSerie read GetItem write SetItem; default;
    property Plot: TPlot read FPlot;
  end;
  
  TLabels = class (TCollection)
  private
    FPlot: TPlot;
    FItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItem(Index: Integer): TPlotLabel;
    procedure SetItem(Index: Integer; Value: TPlotLabel);
    function GetSelectedLabel: TPlotLabel;
    procedure DrawSelected(const Canvas: TCanvas);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(APlot: TPlot);
    function Add: TPlotLabel;
    property Items[Index: Integer]: TPlotLabel read GetItem write SetItem; default;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Plot: TPlot read FPlot;
    property SelectedLabel: TPlotLabel read GetSelectedLabel;
  end;
  
  TPlotMouseModeInfo=record
    InProgress: boolean;
    XAxis, YAxis: TAxis;
    TransBuf: array of TRealPoint; // preview buffer (not allowed in variant part!!!)
    case TPlotMouseMode of
      pmAutoZoom, pmZoom, pmUnZoom:(
        ZoomX, ZoomY, ZoomXo, ZoomYo: integer;
        ZoomXAxis, ZoomYAxis: boolean);
      pmSelect:(
        SelX, SelY, SelXo, SelYo: integer);
      pmRuler:(
        RulerX, RulerY: integer; // ruler center coords
        RulerFi: TReal); // ruler angle
      pmPointEdit:(
        EditX, EditY, EditX1, EditY1, EditX2, EditY2, EditSer, EditPnt: integer);
      pmTranslate:(
        TransIX, TransIY, TransX1, TransX2, TransY1, TransY2: TReal; // keep old coordinates
        TransPointCount: integer;
        TransMode: (ptmL, ptmR, ptmB, ptmT, ptmTL, ptmTR, ptmBL, ptmBR, ptmMove));
      pmMargins:(
        MarginMode: (pmmTL, pmmTR, pmmBL, pmmBR, pmmXT, pmmYT, pmmX2T, pmmY2T, 
          pmmXL, pmmYL, pmmX2L, pmmY2L);
        MarginXo, MarginYo: integer); // keep initial mouse coordinates
      pmLabelEdit:(
        LEXo, LEYo, LEX, LEY: integer;
        Move1, Move2, Move3, Move4: boolean);  
      // other modes
  end;

  TGetAxisLabelEvent = procedure (Sender: TObject; Axis: TAxis; var Value: extended) of object;
  // we need to place event declaration after TAxis...
  
  TPlot = class (TPaintBox)
  private
    FTransparent: Boolean;
    FSerieIndex: Integer;
    FXAxis: TAxis;
    FYAxis: TAxis;
    FXAxis2: TAxis;
    FYAxis2: TAxis;
    FBorderStyle: TBorderStyle;
    FBrush: TBrush;
    FGetPoint: TGetPointEvent;
    FOnSelectionChange: TNotifyEvent;
    FPen: TPen;
    FOnError: TPlotHintEvent;
    FOnHint: TPlotHintEvent;
    FMouseMode: TPlotMouseMode;
    FOnPointClick: TPointClickEvent;
    FSeries: TSeries;
    FMargins: array [1..4] of integer;
    FAutoMargins: Boolean;
    FSelection: array[1..4] of extended;
    FSelectionVisible: Boolean;
    FPMInfo: TPlotMouseModeInfo;
    FClipPoints: Boolean;
    FZoomStack: TObjectStack;
    FLabels: TLabels;
    FOnGetAxisLabel: TGetAxisLabelEvent;
    FAreaSeries: Boolean;
    FAutoScaleLabels: Boolean;
    function GetThisSerie: TSerie;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBrush(const Value: TBrush);
    procedure SetMouseMode(const Value: TPlotMouseMode);
    procedure SetPen(const Value: TPen);
    procedure SetSerieIndex(const Value: Integer);
    procedure SetSeries(const Value: TSeries);
    procedure SetTransparent(const Value: Boolean);
    procedure SetXAxis(const Value: TAxis);
    procedure SetYAxis(const Value: TAxis);
    procedure SetXAxis2(const Value: TAxis);
    procedure SetYAxis2(const Value: TAxis);
    procedure Changed(Sender: TObject);
    function GetMargin(Index: Integer): Integer;
    procedure SetMargin(Index: Integer; const Value: Integer);
    procedure SetAutoMargins(const Value: Boolean);
    procedure ShowPlotHint(Value: string);
    function GetSelection(Index: Integer): Extended;
    procedure SetSelection(Index: Integer; const Value: Extended);
    procedure SetSelectionVisible(const Value: Boolean);
    procedure DrawSelection(Canvas: TCanvas);
    procedure DrawRuler(X, Y: integer);
    procedure DrawEdit(X, Y: integer);
    function BelongMarker(rX, rY: TReal; X, Y: integer): Boolean; overload;
    function BelongMarker(iX, iY, X, Y: integer): Boolean; overload;
    procedure DrawMargins(Canvas: TCanvas);
    procedure SetClipPoints(Value: Boolean);
    procedure SetLabels(Value: TLabels);
    procedure DeletePoint(Idx: integer; Container: TContainer);
    procedure SetAreaSeries(Value: Boolean);
    procedure SetAutoScaleLabels(Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    property SelectionVisible: Boolean read FSelectionVisible write SetSelectionVisible;
    property SelectionTop: Extended index 1 read GetSelection write SetSelection;
    property SelectionBottom: Extended index 2 read GetSelection write SetSelection;
    property SelectionLeft: Extended index 3 read GetSelection write SetSelection;
    property SelectionRight: Extended index 4 read GetSelection write SetSelection;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintCanvas(Canvas: TCanvas; Width, Height: integer);
    procedure SaveToMetaFile(WMF: TMetaFile);
    function CanUnZoom: Boolean;
    procedure UndoZoom;
    procedure SaveToFile(FileName: string);
    procedure Print(W, H: integer);
    procedure Delete;
    procedure CopyToClipboard(Flags: TPlotCopyFlags=[pcmPage, pcmPoints, pcmLines, pcmSerie, pcmLabel]);
    function GetPoint(Point, Serie: integer; var X, Y: TReal): Boolean;
  published
    property Pen: TPen read FPen write SetPen;
    property Brush: TBrush read FBrush write SetBrush;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default DefBorderStyle;
    property MouseMode: TPlotMouseMode read FMouseMode write SetMouseMode default DefMouseMode;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property XAxis: TAxis read FXAxis write SetXAxis;
    property YAxis: TAxis read FYAxis write SetYAxis;
    property XAxis2: TAxis read FXAxis2 write SetXAxis2;
    property YAxis2: TAxis read FYAxis2 write SetYAxis2;
    property Series: TSeries read FSeries write SetSeries;
    property SerieIndex: Integer read FSerieIndex write SetSerieIndex;
    property ThisSerie: TSerie read GetThisSerie stored false;
    property LeftMargin: Integer index 1 read GetMargin write SetMargin;
    property RightMargin: Integer index 2 read GetMargin write SetMargin;
    property TopMargin: Integer index 3 read GetMargin write SetMargin;
    property BottomMargin: Integer index 4 read GetMargin write SetMargin;
    property AutoMargins: Boolean read FAutoMargins write SetAutoMargins default true;
    property ClipPoints: Boolean read FClipPoints write SetClipPoints default true;
    property Labels: TLabels read FLabels write SetLabels;
    property AreaSeries: Boolean read FAreaSeries write SetAreaSeries default false;
    property AutoScaleLabels: Boolean read FAutoScaleLabels write SetAutoScaleLabels default false;
    property OnHint: TPlotHintEvent read FOnHint write FOnHint;
    property OnError: TPlotHintEvent read FOnError write FOnError;
    property OnGetPoint: TGetPointEvent read FGetPoint write FGetPoint;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPointClick: TPointClickEvent read FOnPointClick write FOnPointClick;
    property OnGetAxisLabel: TGetAxisLabelEvent read FOnGetAxisLabel write FOnGetAxisLabel;
  end;
  
procedure Register;
procedure DrawPoint(const Canvas: TCanvas; X, Y, psize: integer; PointType: TPointType);

resourcestring
  strPlotBusy='Busy...';
  strPlotPointError='Exception %s at point %d in series %d with message:'+CRLF+'%s';
  strPlotEditError='Unable to move points!'+CRLF+
    'Non-empty series or axis expression(s)'+CRLF+
    'disable point editor and translation tools';
  strPlotDataError='Editor and Translation tools require TRealData items!';
  strPlotTransError='Unable to translate empty or functional series!';
  strPlotTransSelError='Unable to translate empty selection!'+CRLF+
    'Please select plot area first.';
  strPlotScaleError='Invalid coordinate scaling: %s!';
  strPlotNoLabelText='<Label text undefined>';
  strInvalidDateTime='Bad date/time';
  strPlotLinkedAxisError='Exception %s in linked axis with message:'+CRLF+'%s';

implementation

uses Math, ClipBrd, Printers, Dialogs, DMHTMLText;

const
  PPI=85; // points per inch; => [w,px]=[px/"]/[pt/"]*[w,pt] - used for printing
  MSZ=3;  // selection handle halfsize (px)

procedure Register;
begin
  RegisterComponents('DM2003', [TPlot]);
end;

procedure CorRect(var x1, y1, x2, y2: integer);  // ensure: x1<x2, y1<y2
var
  i: integer;
begin
  if x1>x2 then
  begin
    i:=x1;
    x1:=x2;
    x2:=i;
  end;
  if y1>y2 then
  begin
    i:=y1;
    y1:=y2;
    y2:=i;
  end;
end;

// more trustworthy than Canvas.DrawFocusRect used in DM2000
procedure DrawDotRect(const Canvas: TCanvas; x1, y1, x2, y2: integer);
var
  A: array[1..5] of TPoint;
begin
  CorRect(x1, y1, x2, y2);
  A[1]:=Point(x1, y1);
  A[2]:=Point(x2, y1);
  A[3]:=Point(x2, y2);
  A[4]:=Point(x1, y2);
  A[5]:=A[1];
  with Canvas.Pen do
  begin
    Width:=1;
    Style:=psDot;
    Mode:=pmXor;
    Color:=clGray;
  end;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clSilver; 
  Canvas.Polyline(A);
end;

{used in TPlot.DrawTranslation and TPlot.MouseUp}
function Translate(x,x1,x2,x1o,x2o: TReal; Log: boolean): TReal;
begin
  if x=x2o then Result:=x2 else
    if Log
    then Result:=exp((ln(x1)+(ln(x)-ln(x1o))/(ln(x2o)-ln(x))*ln(x2))/(1+(ln(x)-ln(x1o))/(ln(x2o)-ln(x))))
    else Result:=(x1+(x-x1o)/(x2o-x)*x2)/(1+(x-x1o)/(x2o-x));
end;

{Called from TSerie.Paint(), may be used in legends etc. NOTICE: Canvas
painting tools must be properly assigned (that is, from the Serie Pen and
Brush or may be another objects) prior to calling DrawPoint()}
procedure DrawPoint(const Canvas: TCanvas; X, Y, psize: integer;
  PointType: TPointType);
var
  fix: integer;  
begin
  psize:=psize div 2; // calculate halfsize
  if Canvas.Pen.Width=1 // fix point shape bug
  then fix:=1
  else fix:=0; 
  if psize<1 // one pixel?
  then Canvas.Pixels[X,Y]:=Canvas.Brush.Color
  else
  case PointType of
    ptCircle:
      Canvas.Ellipse(X-psize, Y-psize, X+psize, Y+psize);
    ptSquare:
      Canvas.Rectangle(X-psize, Y-psize, X+psize, Y+psize);
    ptCross:
      begin
        Canvas.MoveTo(X,Y-psize);
        Canvas.LineTo(X,Y+psize+fix); {|}
        Canvas.MoveTo(X-psize,Y);
        Canvas.LineTo(X+psize+fix,Y); {_}
      end;
    ptXCross:
      begin
        Canvas.MoveTo(X-psize,Y-psize);
        Canvas.LineTo(X+psize+fix,Y+psize+fix);  {\}
        Canvas.MoveTo(X-psize,Y+psize);
        Canvas.LineTo(X+psize+fix,Y-psize-fix);  {/}
      end;
    ptAsterisk:
      begin
        Canvas.MoveTo(X,Y-psize);
        Canvas.LineTo(X,Y+psize+fix); {|}
        Canvas.MoveTo(X-psize,Y);
        Canvas.LineTo(X+psize+fix,Y); {_}
        Canvas.MoveTo(X-psize,Y-psize);
        Canvas.LineTo(X+psize+fix,Y+psize+fix); {\}
        Canvas.MoveTo(X-psize,Y+psize);
        Canvas.LineTo(X+psize+fix,Y-psize-fix); {/}
      end;
  end;{case}
end;

{---------- Class: TAxis ----------}
procedure TAxis.Assign(A: TPersistent);
begin
  if A is TAxis then
  begin
    AutoScale:=(A as TAxis).AutoScale;
    Expression:=(A as TAxis).Expression;
    Font.Assign((A as TAxis).Font);
    LabelDecimals:=(A as TAxis).LabelDecimals;
    LabelType:=(A as TAxis).LabelType;
    LabelVisible:=(A as TAxis).LabelVisible;
    LabelWidth:=(A as TAxis).LabelWidth;
    LogTicks:=(A as TAxis).LogTicks;
    MajorTicks:=(A as TAxis).MajorTicks;
    Margins:=(A as TAxis).Margins;
    Max:=(A as TAxis).Max;
    Min:=(A as TAxis).Min;
    MinorTicks:=(A as TAxis).MinorTicks;
    Pen.Assign((A as TAxis).Pen);
    ShowGrid:=(A as TAxis).ShowGrid;
    SmartTicks:=(A as TAxis).SmartTicks;
    Title:=(A as TAxis).Title;
    Visible:=(A as TAxis).Visible;
    ShowExpression:=(A as TAxis).ShowExpression;
    TitleMargin:=(A as TAxis).TitleMargin;
    IsLinked:=(A as TAxis).IsLinked;
    InnerTicks:=(A as TAxis).InnerTicks;
    LabelMargin:=(A as TAxis).LabelMargin;
    TickLength:=(A as TAxis).TickLength;
  end else inherited;
end;

constructor TAxis.Create(APlot: TPlot);
begin
  inherited Create;
  FPlot:=APlot;
  FFormat.Width:=DefLabelWidth;
  FFormat.Decimals:=DefLabelDecimals;
  FFormat.FType:=DefLabelType;
  FMin:=0;
  FMax:=10;
  FMajorTicks:=DefMajorTicks;
  FMinorTicks:=DefMinorTicks;
  FPen:=TPen.Create;
  FPen.OnChange:=OnChanged;
  FFont:=TFont.Create;
  FFont.OnChange:=OnChanged;
  FMargins:=0;
  FAutoScale:=false;
  FShowGrid:=false;
  FSmartTicks:=false;
  FVisible:=true;
  FLabelVisible:=true;
  FShowExpression:=true;
  FTitleMargin:=DefTitleMargin;
  FIsLinked:=false;
  FLabelMargin:=DefLabelMargin;
end;

destructor TAxis.Destroy;
begin
  if Assigned(FFont) then
  FFont.Free;
  if Assigned(FPen) then
  FPen.Free;
  inherited;
end;

function TAxis.GetDecimals: Integer;
begin
  Result:=FFormat.Decimals;
end;

function TAxis.GetFType: TFloatFormat;
begin
  Result:=FFormat.FType;
end;

function TAxis.GetWidth: Integer;
begin
  Result:=FFormat.Width;
end;

//: called when properties changed 
procedure TAxis.OnChanged(Sender: TObject);
begin
  if Assigned(FPlot)
  then FPlot.Changed(Sender);
end;

//: Note: negative result (-1000) PROBABLY means error! 
function TAxis.Scale(Value: TReal; Abs: boolean=true): Integer;
begin
  // return main axis scale in Linked mode for correct painting
  // WARNING!!! the code assumes that axis length is the same for both axes
  if FIsLinked then
  begin
    Assert(Assigned(FPlot));
    Result:=GetLinkedAxis.Scale(Value, Abs);
    Exit;
  end;
  // warning: possible range check exceptions eliminated!
  Result:=-1000; // default result on error
  try
    if FMin=FMax
    then Result:=FLength div 2
    else if FLogTicks
         then Result:=Round((ln(Value)-ln(FMin))/(ln(FMax)-ln(FMin))*FLength)
         else Result:=Round((Value-FMin)/(FMax-FMin)*FLength);
    if Abs then
      if FVertical
      then Result:=FOrigin-Result
      else Result:=FOrigin+Result;
  except
    on E: Exception do
    if E is ERangeError then
    begin
      if Assigned(FPlot.FOnError)
      then FPlot.FOnError(Self, SysUtils.Format(strPlotScaleError, [E.Message]));
    end else
    begin
      if Assigned(FPlot.FOnError)
      then FPlot.FOnError(Self, E.Message);
      Windows.Beep(1000, 50);
    end;
  end;
end;

//: Overloaded version performs reverse calculation. Works only in "absolute" mode 
function TAxis.Scale(Value: integer): TReal;
begin
  // WARNING!!! the code assumes that axis length is the same for both axes
  if FIsLinked then
  begin
    Assert(Assigned(FPlot));
    Result:=GetLinkedAxis.Scale(Value);
    Exit;
  end;
  if FLength<=0 then // may be really called BEFORE TAxis.Paint !!!
  begin
    Result:=(FMax+FMin)/2;
    Exit;
  end;
  if FVertical
  then Value:=FOrigin-Value
  else Value:=Value-FOrigin;
  if FLogTicks
  then Result:=exp(ln(FMin)+(ln(FMax)-ln(FMin))*Value/FLength)
  else Result:=FMin+(FMax-FMin)*Value/FLength;
end;

procedure TAxis.Paint(const Canvas: TCanvas; const X,Y,Length,GridSize: integer; Kind: TAxisKind);
var
  _TickLength, _TextHeight, _LabelMargin, _TitleMargin: Integer;
  MajorTicksPositions: TLabelValues;
  MinorTicksPositions: array of integer;
  
  procedure PaintTop;
  var
    I, J, Xo, Yo, W, H, YL: integer;
    S: string;
  label
    DrawTitle;
  begin
    // paint axis
    Canvas.MoveTo(X,Y);
    Canvas.LineTo(X+Length, Y);
    if FLabelVisible then
    begin
      // paint ticks and grid
      if FShowGrid and (not FIsLinked)
      then Yo:=Y+GridSize
      else Yo:=Y;
      // calculate tick labels position
      if FInnerTicks
      then YL:=Y-_TextHeight
      else YL:=Y-_TickLength-_TextHeight;
      if AutoMarginL
      then YL:=Y-_LabelMargin
      else FLabelMargin:=Y-YL;
      // process FMin=FMax separately
      if (FMin=FMax) and (not FIsLinked) then
      begin
        Xo:=Scale(FMin, false); // relative tick position
        Canvas.MoveTo(X+Xo, Yo);
        if FInnerTicks and FShowGrid
        then Canvas.LineTo(X+Xo, Y)
        else Canvas.LineTo(X+Xo, Y-_TickLength);
        S:=FormatLabel(FMin);
        Xo:=Xo-(Canvas.TextWidth(S) div 2);
        Canvas.TextOut(X+Xo, YL, S);
        goto DrawTitle;
      end;
      // minor ticks before first major tick
      I:=High(MinorTicksPositions);
      Xo:=2*Scale(MajorTicksPositions[0], false)-Scale(MajorTicksPositions[1], false);
      while (I>=0) and (MinorTicksPositions[I]+Xo>0) and
        (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Xo>Length))) do
      begin
        Canvas.MoveTo(Xo+MinorTicksPositions[I]+X, Y);
        Canvas.LineTo(Xo+MinorTicksPositions[I]+X, Y-(_TickLength div 2));
        Dec(I);
      end;
      // major ticks and minor ticks between first and last major ticks
      for I:=0 to High(MajorTicksPositions) do
      begin
        Xo:=X+Scale(MajorTicksPositions[I], false); // absolute major tick position
        if (Xo>=X) and (Xo<=X+FLength) then
        begin
          Canvas.MoveTo(Xo, Yo);
          if FInnerTicks and FShowGrid
          then Canvas.LineTo(Xo, Y)
          else Canvas.LineTo(Xo, Y-_TickLength);
          S:=FormatLabel(MajorTicksPositions[I]);
          Canvas.TextOut(Xo-(Canvas.TextWidth(S) div 2), YL, S);
        end;
        if I<High(MajorTicksPositions) then
        for J:=0 to High(MinorTicksPositions) do
        if (Xo+MinorTicksPositions[J]>=X) and (Xo+MinorTicksPositions[J]<=X+FLength) then
        begin
          Canvas.MoveTo(Xo+MinorTicksPositions[J], Y);
          Canvas.LineTo(Xo+MinorTicksPositions[J], Y-(_TickLength div 2));
        end;
      end;
      // minor ticks after last major tick
      I:=0;
      Xo:=Scale(MajorTicksPositions[High(MajorTicksPositions)], false);
      while (I<=High(MinorTicksPositions)) and (Xo+MinorTicksPositions[I]<Length)
        and (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Xo<0))) do
      begin
        Canvas.MoveTo(Xo+MinorTicksPositions[I]+X, Y);
        Canvas.LineTo(Xo+MinorTicksPositions[I]+X, Y-(_TickLength div 2));
        Inc(I);
      end;
    end;
    // axis title
  DrawTitle:
    S:=FTitle;
    if S<>'' then
    begin
      if FInnerTicks then
        if FLabelVisible
        then Yo:=Y-2*_TextHeight
        else Yo:=Y-_TextHeight
      else
        if FLabelVisible
        then Yo:=Y-2*_TextHeight-_TickLength
        else Yo:=Y-_TextHeight;
      Yo:=Yo-1; // 1-PIXEL MARGIN!
      if FShowExpression and (FExpression<>'')
      then S:=S+' {'+FExpression+'}';
      HTMLCalculateSize(Canvas, S, W, H);
      Canvas.Font:=Font; // restore font after HTMLCalculateSize
      if AutoMarginT
      then Yo:=Y-_TitleMargin
      else FTitleMargin:=Y-Yo;
      HTMLTextOut(Canvas, S, X+(Length div 2)-(W div 2), Yo, false);
    end;
  end;
  
  procedure PaintRight;
  var
    I, J, Xo, Yo, LW, MaxLW, W, H, XM: integer;
    S: string;
  label
    DrawTitle;
  begin
    MaxLW:=0; // used to find maximal label width to indent axis title
    // paint axis
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X, Y-Length);
    if FLabelVisible then
    begin
      // paint ticks and grid
      if FShowGrid and (not FIsLinked)
      then Xo:=X-GridSize
      else Xo:=X;
      // calculate tick labels position
      if FInnerTicks
      then XM:=2 // 2-PIXEL MARGIN!
      else XM:=2+_TickLength;
      if AutoMarginL
      then XM:=_LabelMargin
      else FLabelMargin:=XM;
      // process FMin=FMax separately
      if (FMin=FMax) and (not FIsLinked) then
      begin
        Yo:=Scale(FMin, false); // relative tick position
        if FInnerTicks and FShowGrid
        then Canvas.MoveTo(X, Y-Yo)
        else Canvas.MoveTo(X+_TickLength, Y-Yo);
        Canvas.LineTo(Xo, Y-Yo);
        S:=FormatLabel(FMin);
        Canvas.TextOut(X+XM, Y-Yo-(_TextHeight div 2), S);
        // Exit; <- error!
        MaxLW:=Canvas.TextWidth(S);
        goto DrawTitle;
      end;
      // minor ticks before first major tick
      I:=High(MinorTicksPositions);
      Yo:=2*Scale(MajorTicksPositions[0], false)-Scale(MajorTicksPositions[1], false);
      while (I>=0) and (MinorTicksPositions[I]+Yo>0) and
        (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Yo>Length))) do
      begin
        Canvas.MoveTo(X, Y-Yo-MinorTicksPositions[I]);
        Canvas.LineTo(X+(_TickLength div 2), Y-Yo-MinorTicksPositions[I]);
        Dec(I);
      end;
      // major ticks and minor ticks between first and last major ticks
      for I:=0 to High(MajorTicksPositions) do
      begin
        Yo:=Y-Scale(MajorTicksPositions[I], false); // absolute major tick position
        if (Yo<=Y) and (Yo>=Y-FLength) then
        begin
          if FInnerTicks and FShowGrid
          then Canvas.MoveTo(X, Yo)
          else Canvas.MoveTo(X+_TickLength, Yo);
          Canvas.LineTo(Xo, Yo);
          S:=FormatLabel(MajorTicksPositions[I]);
          LW:=Canvas.TextWidth(S);
          if MaxLW<LW then MaxLW:=LW; // correct axis title indent
          Canvas.TextOut(X+XM, Yo-(_TextHeight div 2), S);
        end;
        if I<High(MajorTicksPositions) then
        for J:=0 to High(MinorTicksPositions) do
        if (Yo-MinorTicksPositions[J]<=Y) and (Yo-MinorTicksPositions[J]>=Y-FLength) then
        begin
          Canvas.MoveTo(X, Yo-MinorTicksPositions[J]);
          Canvas.LineTo(X+(_TickLength div 2), Yo-MinorTicksPositions[J]);
        end;
      end;
      // minor ticks after last major tick
      I:=0;
      Yo:=Scale(MajorTicksPositions[High(MajorTicksPositions)], false);
      while (I<=High(MinorTicksPositions)) and (Yo+MinorTicksPositions[I]<Length)
        and (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Yo<0))) do
      begin
        Canvas.MoveTo(X, Y-Yo-MinorTicksPositions[I]);
        Canvas.LineTo(X+(_TickLength div 2), Y-Yo-MinorTicksPositions[I]);
        Inc(I);
      end;
    end;
  DrawTitle:
    // axis title
    S:=FTitle;
    if S<>'' then
    begin
      {if FFormat.FType=ffGeneral
      then MaxLW:=MaxLW+4;add gap}
      if FLabelVisible then
        if FInnerTicks
        then Xo:=X+MaxLW
        else Xo:=X+MaxLW+_TickLength
      else
        Xo:=X;
      Xo:=Xo+2; // 2-PIXEL MARGIN!
      if FShowExpression and (FExpression<>'')
      then S:=S+' {'+FExpression+'}';
      HTMLCalculateSize(Canvas, S, W, H);
      Canvas.Font:=Font; // restore font after HTMLCalculateSize
      if AutoMarginT
      then Xo:=X+_TitleMargin
      else FTitleMargin:=Xo-X;
      HTMLTextOut(Canvas, S, Xo, Y-(Length div 2)+(W div 2), true);
    end;
  end;
  
  procedure PaintLeft;
  var
    I, J, Xo, Yo, LW, MaxLW, W, H, XM: integer;
    S: string;
  label
    DrawTitle;
  begin
    MaxLW:=0; // used to find maximal label width to indent axis title
    // paint axis
    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X, Y-Length);
    if FLabelVisible then
    begin
      // paint ticks and grid
      if FShowGrid and (not FIsLinked)
      then Xo:=X+GridSize
      else Xo:=X;
      // calculate tick labels position
      if FInnerTicks
      then XM:=1 // 1-PIXEL MARGIN!
      else XM:=1+_TickLength;
      if AutoMarginL
      then XM:=_LabelMargin
      else FLabelMargin:=XM;
      // process FMin=FMax separately
      if (FMin=FMax) and (not FIsLinked) then
      begin
        Yo:=Scale(FMin, false); // relative tick position
        if FInnerTicks and FShowGrid
        then Canvas.MoveTo(X, Y-Yo)
        else Canvas.MoveTo(X-_TickLength, Y-Yo);
        Canvas.LineTo(Xo, Y-Yo);
        S:=FormatLabel(FMin);
        MaxLW:=Canvas.TextWidth(S);
        Xo:=MaxLW+XM;
        Canvas.TextOut(X-Xo, Y-Yo-(_TextHeight div 2), S);
        goto DrawTitle;
      end;
      // minor ticks before first major tick
      I:=High(MinorTicksPositions);
      Yo:=2*Scale(MajorTicksPositions[0], false)-Scale(MajorTicksPositions[1], false);
      while (I>=0) and (MinorTicksPositions[I]+Yo>0) and
        (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Yo>Length))) do
      begin
        Canvas.MoveTo(X, Y-Yo-MinorTicksPositions[I]);
        Canvas.LineTo(X-(_TickLength div 2), Y-Yo-MinorTicksPositions[I]);
        Dec(I);
      end;
      // major ticks and minor ticks between first and last major ticks
      for I:=0 to High(MajorTicksPositions) do
      begin
        Yo:=Y-Scale(MajorTicksPositions[I], false); // absolute major tick position
        if (Yo<=Y) and (Yo>=Y-FLength) then
        begin
          if FInnerTicks and FShowGrid
          then Canvas.MoveTo(X, Yo)
          else Canvas.MoveTo(X-_TickLength, Yo);
          Canvas.LineTo(Xo, Yo);
          S:=FormatLabel(MajorTicksPositions[I]);
          LW:=Canvas.TextWidth(S);
          if MaxLW<LW then MaxLW:=LW; // correct axis title indent
          Canvas.TextOut(X-LW-XM, Yo-(_TextHeight div 2), S);
        end;
        if I<High(MajorTicksPositions) then
        for J:=0 to High(MinorTicksPositions) do
        if (Yo-MinorTicksPositions[J]<=Y) and (Yo-MinorTicksPositions[J]>=Y-FLength) then
        begin
          Canvas.MoveTo(X, Yo-MinorTicksPositions[J]);
          Canvas.LineTo(X-(_TickLength div 2), Yo-MinorTicksPositions[J]);
        end;
      end;
      // minor ticks after last major tick
      I:=0;
      Yo:=Scale(MajorTicksPositions[High(MajorTicksPositions)], false);
      while (I<=High(MinorTicksPositions)) and (Yo+MinorTicksPositions[I]<Length)
        and (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Yo<0))) do
      begin
        Canvas.MoveTo(X, Y-Yo-MinorTicksPositions[I]);
        Canvas.LineTo(X-(_TickLength div 2), Y-Yo-MinorTicksPositions[I]);
        Inc(I);
      end;
    end;
    // axis title
  DrawTitle:
    S:=FTitle;
    if S<>'' then
    begin
      {if FFormat.FType=ffGeneral
      then MaxLW:=MaxLW+2;add gap}
      if FLabelVisible then
        if FInnerTicks
        then Xo:=X-MaxLW-_TextHeight
        else Xo:=X-MaxLW-_TickLength-_TextHeight
      else
        Xo:=X-_TextHeight;
      Xo:=Xo-1; // 1-PIXEL MARGIN!
      if FShowExpression and (FExpression<>'')
      then S:=S+' {'+FExpression+'}';
      HTMLCalculateSize(Canvas, S, W, H);
      Canvas.Font:=Font; // restore font after HTMLCalculateSize
      if AutoMarginT
      then Xo:=X-_TitleMargin
      else FTitleMargin:=X-Xo;
      HTMLTextOut(Canvas, S, Xo, Y-(Length div 2)+(W div 2), true);
    end;
  end;
  
  procedure PaintBottom;
  var
    I, J, Xo, Yo, W, H, YL: integer;
    S: string;
  label
    DrawTitle;
  begin
    // paint axis
    Canvas.MoveTo(X,Y);
    Canvas.LineTo(X+Length, Y);
    if FLabelVisible then
    begin
      // paint ticks and grid
      if FShowGrid and (not FIsLinked)
      then Yo:=Y-GridSize
      else Yo:=Y;
      // calculate tick labels position
      if FInnerTicks
      then YL:=Y
      else YL:=Y+_TickLength;
      if AutoMarginL
      then YL:=Y+_LabelMargin
      else FLabelMargin:=YL-Y;
      // process FMin=FMax separately
      if (FMin=FMax) and (not FIsLinked) then
      begin
        Xo:=Scale(FMin, false); // relative tick position
        Canvas.MoveTo(X+Xo, Yo);
        if FInnerTicks and FShowGrid
        then Canvas.LineTo(X+Xo, Y)
        else Canvas.LineTo(X+Xo, Y+_TickLength);
        S:=FormatLabel(FMin);
        Xo:=Xo-(Canvas.TextWidth(S) div 2);
        Canvas.TextOut(X+Xo, YL, S);
        goto DrawTitle;
      end;
      // minor ticks before first major tick
      I:=High(MinorTicksPositions);
      Xo:=2*Scale(MajorTicksPositions[0], false)-Scale(MajorTicksPositions[1], false);
      while (I>=0) and (MinorTicksPositions[I]+Xo>0) and
        (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Xo>Length))) do
      begin
        Canvas.MoveTo(Xo+MinorTicksPositions[I]+X, Y);
        Canvas.LineTo(Xo+MinorTicksPositions[I]+X, Y+(_TickLength div 2));
        Dec(I);
      end;
      // major ticks and minor ticks between first and last major ticks
      for I:=0 to High(MajorTicksPositions) do
      begin
        Xo:=X+Scale(MajorTicksPositions[I], false); // absolute major tick position
        if (Xo>=X) and (Xo<=X+FLength) then
        begin
          Canvas.MoveTo(Xo, Yo);
          if FInnerTicks and FShowGrid
          then Canvas.LineTo(Xo, Y)
          else Canvas.LineTo(Xo, Y+_TickLength);
          S:=FormatLabel(MajorTicksPositions[I]);
          Canvas.TextOut(Xo-(Canvas.TextWidth(S) div 2), YL, S);
        end;
        if I<High(MajorTicksPositions) then
        for J:=0 to High(MinorTicksPositions) do
        if (Xo+MinorTicksPositions[J]>=X) and (Xo+MinorTicksPositions[J]<=X+FLength) then
        begin
          Canvas.MoveTo(Xo+MinorTicksPositions[J], Y);
          Canvas.LineTo(Xo+MinorTicksPositions[J], Y+(_TickLength div 2));
        end;
      end;
      // minor ticks after last major tick
      I:=0;
      Xo:=Scale(MajorTicksPositions[High(MajorTicksPositions)], false);
      while (I<=High(MinorTicksPositions)) and (Xo+MinorTicksPositions[I]<Length)
        and (not (FLogTicks and (FMin>FMax) and (MinorTicksPositions[I]+Xo<0))) do
      begin
        Canvas.MoveTo(Xo+MinorTicksPositions[I]+X, Y);
        Canvas.LineTo(Xo+MinorTicksPositions[I]+X, Y+(_TickLength div 2));
        Inc(I);
      end;
    end;
    // axis title
  DrawTitle:
    S:=FTitle;
    if S<>'' then
    begin
      if FLabelVisible then
      begin
        Yo:=Y+_TextHeight-1; // 1-PIXEL MARGIN!
        if not FInnerTicks
        then Yo:=Yo+_TickLength;
      end else
        Yo:=Y-1; // 1-PIXEL MARGIN!
      if FShowExpression and (FExpression<>'')
      then S:=S+' {'+FExpression+'}';
      HTMLCalculateSize(Canvas, S, W, H);
      Canvas.Font:=Font; // restore font after HTMLCalculateSize
      if AutoMarginT
      then Yo:=Y+_TitleMargin
      else FTitleMargin:=Yo-Y;
      HTMLTextOut(Canvas, S, X+(Length div 2)-(W div 2), Yo, false);
    end;
  end;
  
  procedure MakeMinorTicks;
  var
    q: TReal;
    I: integer;
  begin
    Assert(High(MajorTicksPositions)>0, '{C131BF2F-6F5F-4F18-8719-DEF0CF1345D6}');
    SetLength(MinorTicksPositions, FMinorTicks-1);
    try
      if FLogTicks // calculate SCALED (!) distance between 2 major ticks
      then q:=(ln(10)-ln(1))/(ln(FMax)-ln(FMin))*Length
      else q:=(MajorTicksPositions[1]-MajorTicksPositions[0])/(FMax-FMin)*Length;
      for I:=0 to FMinorTicks-2 do
      if FLogTicks
      then MinorTicksPositions[I]:=Round(q*ln((9*(I+1)/(FMinorTicks))+1)/ln(10))
      else MinorTicksPositions[I]:=Round(q/FMinorTicks*(I+1));
    except
      on E: Exception do
      begin
        SetLength(MinorTicksPositions, 0);
        if Assigned(FPlot.FOnError)
        then FPlot.FOnError(Self, E.Message);
      end;
    end;
  end;
  
begin
  // remember parameters for use in Scale() method
  FLength:=Length;
  FVertical:=Kind in [axLeft, axRight];
  if FVertical
  then FOrigin:=Y
  else FOrigin:=X;
  if not FVisible then Exit; // notice: FLength, FOrigin still valid!
  // calculate major tick positions (initialize MajorTicksPositions[])
  InitLabels(MajorTicksPositions);
  // calculate minor tick positions (initialize MinorTicksPositions[])
  if not FIsLinked then
  begin
    Assert(FMinorTicks>0, '{B10F14EF-DF6E-4766-B257-76384F007A89}');
    if FMin<>FMax then MakeMinorTicks;
  end;
  // prepare canvas drawing tools
  Canvas.Pen:=Pen;
  if Canvas=Printer.Canvas then
  Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
  Canvas.Font:=Font;
  Canvas.Brush.Style:=bsClear;
  { Warning!!! independent _TextHeight, _TickLength calculations also
  performed in Plot.PaintCanvas (CalculateMargins) and MUST BE IN SYNC!!! }
  // calculate height of labels and title
  _TextHeight:=Canvas.TextHeight('H');
  // calculate major tick length (minor ticks length 2 times shorter)
  _TickLength:=Round(_TextHeight*(1+FTickLength));
  if FInnerTicks
  then _TickLength:=-_TickLength;
  // correct labels and title margins
  // Warning! 1-2 pixel margins are not corrected, they are only for display
  _LabelMargin:=FLabelMargin;
  _TitleMargin:=FTitleMargin;
  if AutoMarginL and (Canvas=Printer.Canvas)
  then _LabelMargin:=Round(_LabelMargin*Canvas.Font.PixelsPerInch/PPI);
  if AutoMarginT and (Canvas=Printer.Canvas)
  then _TitleMargin:=Round(_TitleMargin*Canvas.Font.PixelsPerInch/PPI);
  // make main paint axis job
  case Kind of
    axLeft: PaintLeft;
    axRight: PaintRight;
    axTop: PaintTop;
    axBottom: PaintBottom;
  end;
end;

procedure TAxis.SetAutoScale(const Value: Boolean);
begin
  if FAutoScale<>Value then
  begin
    FAutoScale:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetDecimals(const Value: Integer);
begin
  if FFormat.Decimals<>Value then
  begin
    FFormat.Decimals:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetExpression(const Value: string);
begin
  if FExpression<>Value then
  begin
    FExpression:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetFont(const Value: TFont);
begin
  if Assigned(FFont) then
  FFont.Assign(Value);
end;

procedure TAxis.SetFormat(const Value: TFormat);
begin
  if (FFormat.Width<>Value.Width) or (FFormat.Decimals<>Value.Decimals) or
     (FFormat.FType<>Value.FType) then
  begin
    FFormat:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetFType(const Value: TFloatFormat);
begin
  if FFormat.FType<>Value then
  begin
    FFormat.FType:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetLabelVisible(const Value: Boolean);
begin
  if FLabelVisible<>Value then
  begin
    FLabelVisible:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetLogTicks(const Value: Boolean);
begin
  if FLogTicks<>Value then
  begin
    FLogTicks:=Value;
    if Value then
    begin
      if FMin<=0
      then FMin:=1;
      if FMax<=0
      then FMax:=10;
      FSmartTicks:=false; // SmartTicks and LogTicks modes are mutually exclusive!
    end;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetMajorTicks(const Value: Integer);
begin
  if FMajorTicks<>Value then
  begin
    FMajorTicks:=Value;
    if Value<=0 then FMajorTicks:=1;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetMargins(const Value: Extended);
begin
  if FMargins<>Value then
  begin
    FMargins:=Value;
    if Value<0 then FMargins:=0;
    if Value>0.3 then FMargins:=0.3;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetMax(const Value: Extended);
begin
  if FMax<>Value then
  begin
    if FLogTicks then
    begin
      if Value>0 then FMax:=Value;
      if Value<0 then FMax:=-Value;
      if Value=0 then FMax:=10;
    end
    else FMax:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetMin(const Value: Extended);
begin
  if FMin<>Value then
  begin
    if FLogTicks then
    begin
      if Value>0 then FMin:=Value;
      if Value<0 then FMin:=-Value;
      if Value=0 then FMin:=1;
    end
    else FMin:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetMinorTicks(const Value: Integer);
begin
  if FMinorTicks<>Value then
  begin
    FMinorTicks:=Value;
    if Value<=0 then FMinorTicks:=1;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetPen(const Value: TPen);
begin
  if Assigned(FPen) then
  FPen.Assign(Value);
end;

procedure TAxis.SetShowGrid(const Value: Boolean);
begin
  if FShowGrid<>Value then
  begin
    FShowGrid:=Value;
    OnChanged(Self);
  end;
end;

//: SmartTicks and LogTicks modes are mutually exclusive! 
procedure TAxis.SetSmartTicks(const Value: Boolean);
begin
  if FSmartTicks<>Value then
  begin
    FSmartTicks:=Value;
    if Value then FLogTicks:=false;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetTitle(const Value: string);
begin
  if FTitle<>Value then
  begin
    FTitle:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetVisible(const Value: Boolean);
begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetWidth(const Value: Integer);
begin
  if FFormat.Width<>Value then
  begin
    FFormat.Width:=Value;
    OnChanged(Self);
  end;
end;

function TAxis.InRange(const Value: extended): Boolean;
var
  A: Extended;
  B: Extended;
begin
  if FIsLinked then
  begin
    Assert(Assigned(FPlot));
    Result:=GetLinkedAxis.InRange(Value);
    Exit;
  end;
  A:=FMin;
  B:=FMax;
  if A=B then // bugfix: sometimes points clipped if all coordinates
  begin       // are the same (rounding problem as in MakeSmartTicks)
    Result:=Abs(A-Value)<1e-10; // uncertainity for extended ~ 1e-16
    Exit;
  end;
  if B<A then
  begin
    B:=FMin;
    A:=FMax;
  end;
  Result:=(A<=Value) and (Value<=B);
end;

function TAxis.GetFormatString: string;
var
  C: Char;
begin
  if LabelDateTime then
  begin
    Result:=FDateTimeFormat;
    Exit;
  end;
  case FFormat.FType of
    ffGeneral: C:='g';
    ffFixed: C:='f';
  else
    C:='e';
  end;
  Result:=IntToStr(FFormat.Width)+'.'+IntToStr(FFormat.Decimals)+C;
end;

procedure TAxis.SetFormatString(Value: string);
var
  C: Char;
  I: Integer;
begin
  if LabelDateTime then
  begin
    if FDateTimeFormat<>Value then
    begin
      FDateTimeFormat:=Value;
      OnChanged(Self);
    end;
    Exit;
  end;
  if GetFormatString<>Value then
  begin
    C:=Value[Length(Value)];
    case UpCase(C) of
      'G': FFormat.FType:=ffGeneral;
      'F': FFormat.FType:=ffFixed;
      else
        FFormat.FType:=ffExponent;
    end;
    I:=Pos('.', Value);
    FFormat.Width:=StrToIntDef(Copy(Value, 1, I-1), 5);
    FFormat.Decimals:=StrToIntDef(Copy(Value, I+1, Length(Value)-I-1), 3);
    OnChanged(Self);
  end;
end;

//: Returns FALSE if label margin calculated automatically 
function TAxis.AutoMarginL: Boolean;
begin
  if Assigned(FPlot)
  then AutoMarginL:=not FPlot.FAutoMargins
  else AutoMarginL:=true;
  // fix situation when no value was remembered
  if Result and (FLabelMargin=DefLabelMargin)
  then AutoMarginL:=false;
end;

//: Returns FALSE if title margin calculated automatically 
function TAxis.AutoMarginT: Boolean;
begin
  if Assigned(FPlot)
  then AutoMarginT:=not FPlot.FAutoMargins
  else AutoMarginT:=true;
  // fix situation when no value was remembered
  if Result and (FTitleMargin=DefTitleMargin)
  then AutoMarginT:=false;
end;

//: Returns maximal label width and height needed to calculate axes margins 
procedure TAxis.CalculateLabelsWidth(Canvas: TCanvas; var W, H: integer);
var
  LabelValues: TLabelValues;
  I: Integer;
  S: string;
begin
  InitLabels(LabelValues);
  Canvas.Font:=Font;
  W:=0;
  H:=Canvas.TextHeight('H');
  for I:=0 to High(LabelValues) do
  begin
    S:=FormatLabel(LabelValues[I]){+'W'}; // W gives too large gap!!!
    W:=Math.Max(Canvas.TextWidth(S), W);
  end;
end;

//: Safely formats label as number or date/time and process IsLinked 
function TAxis.FormatLabel(Value: extended): string;
begin
  // process IsLinked
  if FIsLinked and Assigned(FPlot) and Assigned(FPlot.FOnGetAxisLabel) then
  try
    FPlot.FOnGetAxisLabel(FPlot, Self, Value);
  except
    on E: Exception do
    begin
      if Assigned(FPlot.FOnError)
      then FPlot.FOnError(FPlot, SysUtils.Format(strPlotLinkedAxisError,
        [E.ClassName, E.Message]));
    end;
  end;
  // process date/time
  if LabelDateTime then
  try
    Result:=FormatDateTime(FDateTimeFormat, Value);
  except
    Result:=strInvalidDateTime;
  end else
    Result:=FloatToStrF(Value, FFormat.FType, FFormat.Width, FFormat.Decimals);
end;

function TAxis.GetLinkedAxis: TAxis;
begin
  Result:=nil;
  if (not FIsLinked) or (not Assigned(FPlot))
  then Exit;
  // calculate paired (main) axis
  if Self=FPlot.XAxis
  then Result:=FPlot.XAxis2;
  if Self=FPlot.XAxis2
  then Result:=FPlot.XAxis;
  if Self=FPlot.YAxis
  then Result:=FPlot.YAxis2;
  if Self=FPlot.YAxis2
  then Result:=FPlot.YAxis;
end;

//: Calculates major ticks positions (Labels values) 
procedure TAxis.InitLabels(var MajorTicksPositions: TLabelValues);
  
  // calculates exponential part of any positive float
  function CalcN(Range: extended): integer; // range>0, CalcN - any integer
  var
    X: extended;
  begin
    X:=ln(Range)/ln(10);
    if X>=0 then Result:=Trunc(X) else Result:=Trunc(X)-1;
  end;
  
  {if use logarithmic scale, actual number of major ticks determined ONLY
  by axis range, while FMajorTicks just IGNORED! However, number of MINOR
  ticks always correct}
  procedure MakeMajorTicks;
  var
    I, RMin, RMax: integer;
  begin
    if FLogTicks then
    begin
      RMin:=CalcN(FMin);
      RMax:=CalcN(FMax);
      if RMax=Rmin then // ensure that number of major ticks >1
      begin
        RMax:=Rmin+1;
        RMin:=Rmin-1;
      end;
      if RMax>Rmin then
      begin
        if RMax-Rmin>1 // ensure that number of major ticks >1
        then Inc(RMin);
        SetLength(MajorTicksPositions, RMax-Rmin+1);
        for I:=RMin to RMax do
        MajorTicksPositions[I-RMin]:=exp(ln(10)*I);
      end;
      if RMax<RMin then
      begin
        if Rmin-RMax>1 // ensure that number of major ticks >1
        then Inc(RMax);
        SetLength(MajorTicksPositions, RMin-Rmax+1);
        for I:=RMax to RMin do
        MajorTicksPositions[I-RMax]:=exp(ln(10)*I);
      end;
    end else
    begin
      SetLength(MajorTicksPositions, FMajorTicks+1);
      for I:=0 to High(MajorTicksPositions) do
      MajorTicksPositions[I]:=FMin+(FMax-FMin)/FMajorTicks*I;
    end;
  end;
  
  procedure MakeSmartTicks;
  const
    Deltas: array[1..9] of extended=(4,3,2,1,0.5,0.25,0.3,0.125,0.1);
  var
    a,d,v,v1,v2: TReal;
    I,N: integer;
  begin // MakeSmartTicks
    a:=trunc((FMax-FMin)/exp(ln(10)*CalcN(Abs(FMax-FMin))))/FMajorTicks;
    d:=10; N:=1;
    for I:=1 to 9 do
    if Abs(Abs(a)-Deltas[I])<d then
    begin
      d:=Abs(Abs(a)-Deltas[I]);
      N:=I;
    end;
    d:=Deltas[N]*exp(ln(10)*CalcN(Abs(FMax-FMin)))*(FMax-FMin)/Abs(FMax-FMin);
    v1:=(Round(FMin/d))*d;
    v2:=(Round(FMax/d))*d;
    v:=v1;
    I:=1;
    repeat
      if InRange(V) then
      begin
        SetLength(MajorTicksPositions, I);
        MajorTicksPositions[I-1]:=v;
        if Abs(v)<Abs(d)/1e10 // correct "bad" zero (e.g. 0.12345e-16)
        then MajorTicksPositions[I-1]:=0;
        Inc(I);
      end;
      v:=v+d;
    until ((v>v2) and (d>0)) or ((v<v2) and (d<0)) or (I>1000{bugfix!});
  end;
  
begin
  // check linked axis
  if FIsLinked then
  begin
    Assert(Assigned(FPlot));
    GetLinkedAxis.InitLabels(MajorTicksPositions);
    Exit;
  end;
  // check ticks
  Assert(FMajorTicks>0, '{D1BC06CA-DDAB-4003-8E0A-E6092024F032}');
  if FLogTicks then
  Assert((FMin>0) and (FMax>0), '{3F12756E-C2EA-4EE4-94F1-F95174D24879}');
  if FSmartTicks and (FMin<>FMax) // check FMin<>FMax (error in CalcN)
  then MakeSmartTicks
  else MakeMajorTicks;
  // if axis scale is about Max(|extended|), SmartTicks crashes a bit faster
  // this results in further Assert() failure in MakeMinorTicks
  if High(MajorTicksPositions)=-1
  then MakeMajorTicks;
end;

//: draw frame around axis tick labels in pmMargins 
procedure TAxis.PaintLabelsFrame(const Canvas: TCanvas);
var
  W, H: Integer;
begin
  FLabelsRect:=Rect(0,0,0,0);
  if (not FLabelVisible) or (not FVisible)
  then Exit; // invisible or no labels - nothing to do!
  Assert(Assigned(FPlot));
  CalculateLabelsWidth(Canvas, W, H);
  // calculate and save title frame
  if Self=FPlot.XAxis
  then FLabelsRect:=Rect(
    Point(FPlot.LeftMargin-W div 2,
      FPlot.Height-FPlot.BottomMargin+FLabelMargin),
    Point(FPlot.Width-FPlot.RightMargin+W div 2,
      FPlot.Height-FPlot.BottomMargin+FLabelMargin+H));
  if Self=FPlot.XAxis2
  then FLabelsRect:=Rect(
    Point(FPlot.LeftMargin-W div 2,
      FPlot.TopMargin-FLabelMargin),
    Point(FPlot.Width-FPlot.RightMargin+W div 2,
      FPlot.TopMargin-FLabelMargin+H));
  if Self=FPlot.YAxis
  then FLabelsRect:=Rect(
    Point(FPlot.LeftMargin-W-FLabelMargin,
      FPlot.TopMargin-H div 2),
    Point(FPlot.LeftMargin-FLabelMargin,
      FPlot.Height-FPlot.BottomMargin+H div 2));
  if Self=FPlot.YAxis2
  then FLabelsRect:=Rect(
    Point(FPlot.Width-FPlot.RightMargin+FLabelMargin,
      FPlot.TopMargin-H div 2),
    Point(FPlot.Width-FPlot.RightMargin+FLabelMargin+W,
      FPlot.Height-FPlot.BottomMargin+H div 2));
  // draw frame rectangle
  Canvas.Polyline([FLabelsRect.TopLeft,
    Point(FLabelsRect.Right,FLabelsRect.Top),
    FLabelsRect.BottomRight,
    Point(FLabelsRect.Left,FLabelsRect.Bottom),
    FLabelsRect.TopLeft]);
end;

//: draw frame around axis title in pmMargins 
procedure TAxis.PaintTitleFrame(const Canvas: TCanvas);
var
  S: string;
  W, H: Integer;
begin
  FTitleRect:=Rect(0,0,0,0);
  S:=FTitle;
  if (S='') or (not FVisible)
  then Exit; // invisible or no title - nothing to do!
  if FShowExpression and (FExpression<>'')
  then S:=S+' {'+FExpression+'}'; // as in Paint()!
  Canvas.Font:=Font; // font size used in HTMLCalculateSize
  HTMLCalculateSize(Canvas, S, W, H);
  W:=(W div 2)+2;
  Canvas.Brush.Style:=bsSolid; // restore after HTMLCalculateSize
  Assert(Assigned(FPlot));
  // calculate and save title frame
  if Self=FPlot.XAxis
  then FTitleRect:=Rect(
    Point(FPlot.LeftMargin+((FPlot.Width-FPlot.RightMargin-
      FPlot.LeftMargin) div 2)-W,
      FPlot.Height-FPlot.BottomMargin+FTitleMargin),
    Point(FPlot.LeftMargin+((FPlot.Width-FPlot.RightMargin-
      FPlot.LeftMargin) div 2)+W,
      FPlot.Height-FPlot.BottomMargin+H+FTitleMargin));
  if Self=FPlot.XAxis2
  then FTitleRect:=Rect(
    Point(FPlot.LeftMargin+((FPlot.Width-FPlot.RightMargin-
      FPlot.LeftMargin) div 2)-W, FPlot.TopMargin-FTitleMargin),
    Point(FPlot.LeftMargin+((FPlot.Width-FPlot.RightMargin-
      FPlot.LeftMargin) div 2)+W, FPlot.TopMargin-FTitleMargin+H));
  if Self=FPlot.YAxis
  then FTitleRect:=Rect(
    Point(FPlot.LeftMargin-FTitleMargin, FPlot.TopMargin+
      ((FPlot.Height-FPlot.TopMargin-FPlot.BottomMargin) div 2)-W),
    Point(FPlot.LeftMargin-FTitleMargin+H, FPlot.TopMargin+
      ((FPlot.Height-FPlot.TopMargin-FPlot.BottomMargin) div 2)+W));
  if Self=FPlot.YAxis2
  then FTitleRect:=Rect(
    Point(FPlot.Width-FPlot.RightMargin+FTitleMargin, FPlot.TopMargin+
      ((FPlot.Height-FPlot.TopMargin-FPlot.BottomMargin) div 2)-W),
    Point(FPlot.Width-FPlot.RightMargin+FTitleMargin+H, FPlot.TopMargin+
      ((FPlot.Height-FPlot.TopMargin-FPlot.BottomMargin) div 2)+W));
  // draw frame rectangle
  Canvas.Polyline([FTitleRect.TopLeft,
    Point(FTitleRect.Right,FTitleRect.Top),
    FTitleRect.BottomRight,
    Point(FTitleRect.Left,FTitleRect.Bottom),
    FTitleRect.TopLeft]);
end;

procedure TAxis.SetInnerTicks(Value: Boolean);
begin
  if FInnerTicks <> Value then
  begin
    FInnerTicks := Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetIsLinked(Value: Boolean);
begin
  if FIsLinked<>Value then
  begin
    if not Assigned(FPlot) // nothing to do if no another axis!
    then Exit;
    FIsLinked:=Value;
    if Value  // only one axis in pair may be linked
    then GetLinkedAxis.IsLinked:=false;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetLabelDateTime(Value: Boolean);
begin
  if FLabelDateTime<>Value then
  begin
    FLabelDateTime:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetLabelMargin(Value: Integer);
begin
  if FLabelMargin <> Value then
  begin
    FLabelMargin := Value;
    if AutoMarginL
    then OnChanged(Self);
  end;
end;

procedure TAxis.SetShowExpression(Value: Boolean);
begin
  if FShowExpression<>Value then
  begin
    FShowExpression:=Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetTickLength(Value: Extended);
begin
  // must be >1!
  if Value<-1
  then Exit;
  if FTickLength <> Value then
  begin
    FTickLength := Value;
    OnChanged(Self);
  end;
end;

procedure TAxis.SetTitleMargin(Value: Integer);
begin
  if (FTitleMargin<>Value) {and AutoMargin} then
  begin
    FTitleMargin:=Value;
    if AutoMarginT
    then OnChanged(Self);
  end;
end;

type
  TSerieClipboardData=record
    Container: TContainer;
    IsFunction,PointVisible,LineVisible,ShowBestFit,Visible,IsRecording: boolean;
    Interleave,FirstLine,LastLine,XColumn,YColumn,PointSize,PenWidth,
      LeaderPosition,XErrorColumn,YErrorColumn,AreaBorder: integer;
    Text: shortstring;
    XExpression,YExpression: array[0..2048] of char;
    PointType: TPointType;
    PenColor, BrushColor: TColor;
    BrushStyle: TBrushStyle;
    PenStyle: TPenStyle;
    PMax,PMin: TReal;
    XAxis: TXAxis;
    YAxis: TYAxis;
  end;

{---------- Class: TSerie ----------}
procedure TSerie.Assign(Source: TPersistent);
begin
  if Source is TSerie then
  begin
    Text:=TSerie(Source).Text;
    PointType:=(Source as TSerie).PointType;
    PointVisible:=(Source as TSerie).PointVisible;
    LineVisible:=(Source as TSerie).LineVisible;
    XColumn:=(Source as TSerie).XColumn;
    YColumn:=(Source as TSerie).YColumn;
    FirstLine:=(Source as TSerie).FirstLine;
    LastLine:=(Source as TSerie).LastLine;
    PointSize:=(Source as TSerie).PointSize;
    Interleave:=(Source as TSerie).Interleave;
    ShowBestFit:=(Source as TSerie).ShowBestFit;
    Pen.Assign((Source as TSerie).Pen);
    Brush.Assign((Source as TSerie).Brush);
    Container:=(Source as TSerie).Container;
    XExpression:=(Source as TSerie).XExpression;
    YExpression:=(Source as TSerie).YExpression;
    IsFunction:=(Source as TSerie).IsFunction;
    PMin:=(Source as TSerie).PMin;
    PMax:=(Source as TSerie).PMax;
    XAxis:=(Source as TSerie).XAxis;
    YAxis:=(Source as TSerie).YAxis;
    Visible:=(Source as TSerie).Visible;
    IsRecording:=(Source as TSerie).IsRecording;
    LeaderPosition:=(Source as TSerie).LeaderPosition;
    XErrorColumn:=(Source as TSerie).XErrorColumn;
    YErrorColumn:=(Source as TSerie).YErrorColumn;
    // AreaBorder:=(Source as TSerie).AreaBorder; see QA0112082008
    if (Source as TSerie).AreaBorder<0
    then AreaBorder:=(Source as TSerie).AreaBorder
    else AreaBorder:=abcNothing;
  end else inherited Assign(Source);
end;

procedure TSerie.ClearBlock;
begin
  FXColumn:=DefXColumn;
  FYColumn:=DefYColumn;
  FXErrorColumn:=DefXColumn;
  FYErrorColumn:=DefYColumn;
  FFirstLine:=DefFirstLine;
  FLastLine:=DefLastLine;
  Changed(false);
end;

procedure TSerie.CopyToClipboard;
var
  Data: Pointer;
  HData: THandle;
  SD: TSerieClipboardData;
begin
  SD.Container:=Container;
  SD.IsFunction:=IsFunction;
  SD.PointVisible:=PointVisible;
  SD.LineVisible:=LineVisible;
  SD.ShowBestFit:=ShowBestFit;
  SD.Interleave:=Interleave;
  SD.FirstLine:=FirstLine;
  SD.LastLine:=LastLine;
  SD.XColumn:=XColumn;
  SD.YColumn:=YColumn;
  SD.PointSize:=PointSize;
  SD.PenWidth:=Pen.Width;
  SD.Text:=Text;
  StrPCopy(SD.XExpression,XExpression);
  StrPCopy(SD.YExpression,YExpression);
  SD.PointType:=PointType;
  SD.PenColor:=Pen.Color;
  SD.BrushColor:=Brush.Color;
  SD.BrushStyle:=Brush.Style;
  SD.PenStyle:=Pen.Style;
  SD.XAxis:=XAxis;
  SD.YAxis:=YAxis;
  SD.PMax:=PMax;
  SD.PMin:=PMin;
  SD.Visible:=Visible;
  SD.IsRecording:=IsRecording;
  SD.LeaderPosition:=LeaderPosition;
  SD.XErrorColumn:=XErrorColumn;
  SD.YErrorColumn:=YErrorColumn;
  SD.AreaBorder:=AreaBorder;
  ClipBoard.Open;
  try
    HData:=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, SizeOf(SD));
    try
      Data:=GlobalLock(HData);
      try
        Move(SD, Data^, SizeOf(SD));
        SetClipboardData(GetClipboardFormat, HData);
      finally
        GlobalUnlock(HData);
      end;
    except
      GlobalFree(HData); raise;
    end;
  finally
    ClipBoard.Close;
  end;
end;

constructor TSerie.Create(Collection: TCollection);
begin
  inherited;
  FPointType:=DefPointType;
  FPointVisible:=DefPointVisible;
  FLineVisible:=DefLineVisible;
  FIsFunction:=DefIsFunction;
  FXColumn:=DefXColumn;
  FYColumn:=DefYColumn;
  FFirstLine:=DefFirstLine;
  FLastLine:=DefLastLine;
  FPointSize:=DefPointSize;
  FInterleave:=DefInterleave;
  FPen:=TPen.Create;
  FPen.OnChange:=OnChanged;
  FBrush:=TBrush.Create;
  FBrush.OnChange:=OnChanged;
  FXExpression:='';
  FYExpression:='';
  FXAxis:=BottomAxis;
  FYAxis:=LeftAxis;
  FContainer:=nil;
  FVisible:=true;
  FLeaderPosition:=DefLastLine;
  FXErrorColumn:=DefXColumn;
  FYErrorColumn:=DefYColumn;
  FAreaBorder:=abcNothing;
end;

destructor TSerie.Destroy;
var
  I: Integer;
begin
  // check AreaBorders of other series
  if Assigned(Collection) then
  for I:=0 to Collection.Count-1 do
  if (Collection.Items[I] as TSerie).AreaBorder=ID
  then (Collection.Items[I] as TSerie).AreaBorder:=abcNothing;
  // destroy drawing objects
  if Assigned(FPen) then
  FPen.Free;
  if Assigned(FBrush) then
  FBrush.Free;
  inherited;
end;

function TSerie.Empty: Boolean;
begin
  if FIsFunction
  then Result:=(FFirstLine<DefFirstLine) or (FLastLine=DefLastLine) or
    (FLastLine<FFirstLine) or (FXExpression='') or (FYExpression='')
  else Result:=(FXColumn<=DefXColumn) or (FYColumn<=DefYColumn) or
    (FFirstLine<DefFirstLine) or (FLastLine=DefLastLine) or
    (FLastLine<FFirstLine) or (not Assigned(FContainer));
end;

class function TSerie.GetClipboardFormat: Word;
begin
  Result:=RegisterClipboardFormat('TSerie Object');
end;

function TSerie.GetDisplayName: string;
begin
  Result:=Text;
  if Result=''
  then Result:=inherited GetDisplayName;
end;

//: called when properties changed 
procedure TSerie.OnChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TSerie.Paint(const Canvas: TCanvas);
var
  I, i1, i2: Integer;
  NumPoints: Integer;
  psize: Integer;
  P: TRealPoint;
  PointCache: array of TPoint;
  Plt: TPlot;
  D: TData;
  PointIndexCache: array of Integer;
  V: Extended;
begin
  if Empty or (not (FLineVisible or FPointVisible))
    or (not FVisible) then Exit; // nothing to do!
  // calculate point coordinates and fill PointCache
  NumPoints:=0; // number of points in PointCache
  Plt:=(Collection as TSeries).Plot;
  SetLength(PointCache, FLastLine-FFirstLine+1);
  SetLength(PointIndexCache, FLastLine-FFirstLine+1); // used to paint error bars
  for I:=0 to FLastLine-FFirstLine do
  if (((I mod FInterleave)=0) or FIsFunction) and
     Plt.GetPoint(I, Index, P.X, P.Y) and
     ((_XAxis.InRange(P.X) and _YAxis.InRange(P.Y)) or (not Plt.FClipPoints)) then
  begin
    PointCache[NumPoints].X:=_XAxis.Scale(P.X, true);
    PointCache[NumPoints].Y:=_YAxis.Scale(P.Y, true);
    PointIndexCache[NumPoints]:=FFirstLine+I; // remember index of data item
    Inc(NumPoints);
  end;
  // initialize canvas plotting tools
  Canvas.Pen:=FPen;
  if Canvas=Printer.Canvas then
  Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
  Canvas.Brush.Style:=bsClear; // transparent dashes
  // calculate actual point size
  psize:=FPointSize;
  if Canvas=Printer.Canvas
  then psize:=Round(psize*Canvas.Font.PixelsPerInch/PPI);
  // paint lines
  if FLineVisible
  // then Canvas.Polyline(Slice(PointCache, NumPoints)); <- compiler error...
  then Polyline(Canvas.Handle, PointCache[0], NumPoints);
  Canvas.Pen.Style:=psSolid; // points (and their contours) are always solid
  Canvas.Brush:=FBrush;
  // paint error bars
  if ((FXErrorColumn<>DefXColumn) or (FYErrorColumn<>DefYColumn))
    and Assigned(FContainer) and (not FIsFunction)
    and (FContainer.DataType=dtRealData) then
  for I:=0 to NumPoints-1 do
  begin
    D:=TData(FContainer.Items[PointIndexCache[I]]);
    if not Plt.GetPoint(PointIndexCache[I]-FFirstLine,
      Index, P.X, P.Y) // get float coordinates
    then Continue; // bad coordinates - no painting (really already checked)
    if D is TRealData then
    with (D as TRealData) do
    begin
      if (FXErrorColumn<>DefXColumn) and (FXErrorColumn<=Size) then // |-y-|
      begin
        Assert(FXErrorColumn>0); // just additional check...
        V:=GetItem(FXErrorColumn);
        {todo: in logticks mode, if point coordinate positive BUT with -V it
        become negative, TAxis.Scale() call will invoke OnError! should we
        process this situation there or add some check HERE?!}
        if not (_XAxis.FLogTicks and ((P.X-V<=0) or (P.X+V<=0))) then
        if V<>0 then // zero value means no error bar
        begin
          i1:=_XAxis.Scale(P.X-V, true);
          i2:=_XAxis.Scale(P.X+V, true);
          Canvas.MoveTo(i1, PointCache[I].Y);
          Canvas.LineTo(i2, PointCache[I].Y);
          Canvas.MoveTo(i1, PointCache[I].Y-psize);
          Canvas.LineTo(i1, PointCache[I].Y+psize);
          Canvas.MoveTo(i2, PointCache[I].Y-psize);
          Canvas.LineTo(i2, PointCache[I].Y+psize);
        end;
      end;
      if (FYErrorColumn<>DefYColumn) and (FYErrorColumn<=Size) then   // ___
      begin                                                           //  |
        Assert(FYErrorColumn>0);                                      //  x
        V:=GetItem(FYErrorColumn);                                    //  |
        if not (_YAxis.FLogTicks and ((P.Y-V<=0) or (P.Y+V<=0))) then // ---
        if V<>0 then // zero value means no error bar
        begin
          i1:=_YAxis.Scale(P.Y-V, true);
          i2:=_YAxis.Scale(P.Y+V, true);
          Canvas.MoveTo(PointCache[I].X, i1);
          Canvas.LineTo(PointCache[I].X, i2);
          Canvas.MoveTo(PointCache[I].X-psize, i1);
          Canvas.LineTo(PointCache[I].X+psize, i1);
          Canvas.MoveTo(PointCache[I].X-psize, i2);
          Canvas.LineTo(PointCache[I].X+psize, i2);
        end;
      end;
    end;
  end;
  // paint points
  if FPointVisible then
  for I:=0 to NumPoints-1 do
  DrawPoint(Canvas, PointCache[I].X, PointCache[I].Y, psize, FPointType);
  // Finalize(PointCache); //
end;

function TSerie.PasteFromClipboard(DataOnly: boolean): Boolean;
var
  HData: THandle;
  SD: TSerieClipboardData;
  Data: Pointer;
  I: Integer;
  J: Integer;
begin
  Result:=false;
  if Clipboard.HasFormat(GetClipboardFormat) then
  begin
    Clipboard.Open;
    try
      HData:=GetClipboardData(GetClipboardFormat);
      if HData=0 then Exit;
      Data:=GlobalLock(HData);
      if Data=nil then Exit;
      try
        SD:=TSerieClipboardData(Data^);
        Container:=nil;
        for I:=0 to Screen.FormCount-1 do
        begin
          for J:=0 to Screen.Forms[I].ComponentCount-1 do
          if Screen.Forms[I].Components[J]=SD.Container then
          begin
            Container:=SD.Container;
            Result:=true;
            Break;
          end;
          if Assigned(Container) then Break;
        end;
        IsFunction:=SD.IsFunction;
        Interleave:=SD.Interleave;
        FirstLine:=SD.FirstLine;
        LastLine:=SD.LastLine;
        XColumn:=SD.XColumn;
        YColumn:=SD.YColumn;
        XExpression:=StrPas(SD.XExpression);
        YExpression:=StrPas(SD.YExpression);
        Text:=SD.Text;
        XAxis:=SD.XAxis;
        YAxis:=SD.YAxis;
        PMax:=SD.PMax;
        PMin:=SD.PMin;
        XErrorColumn:=SD.XErrorColumn;
        YErrorColumn:=SD.YErrorColumn;
        // AreaBorder:=SD.AreaBorder; see QA0112082008
        if SD.AreaBorder<0
        then AreaBorder:=SD.AreaBorder
        else AreaBorder:=abcNothing;
        if not DataOnly then
        begin
          PointVisible:=SD.PointVisible;
          LineVisible:=SD.LineVisible;
          ShowBestFit:=SD.ShowBestFit;
          PointSize:=SD.PointSize;
          Pen.Width:=SD.PenWidth;
          PointType:=SD.PointType;
          Pen.Color:=SD.PenColor;
          Brush.Color:=SD.BrushColor;
          Brush.Style:=SD.BrushStyle;
          Pen.Style:=SD.PenStyle;
          Visible:=SD.Visible;
          IsRecording:=SD.IsRecording;
          LeaderPosition:=SD.LeaderPosition;
        end;
      finally
        GlobalUnlock(HData);
      end;
    finally
      Clipboard.Close;
    end;
  end;
end;

function TSerie.Scale(var Min, Max: TRealPoint): Boolean;
var
  I: Integer;
  X: TReal;
  Y: TReal;
  First: Boolean;
begin
  Result:=false;
  First:=true;
  Min.X:=0;
  Max.X:=0;
  Min.Y:=0;
  Max.Y:=0;
  if Empty or (not (FLineVisible or FPointVisible)) then Exit; // nothing to do!
  for I:=0 to FLastLine-FFirstLine do
  if (((I mod FInterleave)=0) or FIsFunction) and
     (Collection as TSeries).Plot.GetPoint(I, Index, X, Y) then
  if First then
  begin
    First:=false;
    Min.X:=X;
    Min.Y:=Y;
    Max.X:=X;
    Max.Y:=Y;
  end else
  begin
    Min.X:=Math.Min(Min.X, X);
    Min.Y:=Math.Min(Min.Y, Y);
    Max.X:=Math.Max(Max.X, X);
    Max.Y:=Math.Max(Max.Y, Y);
  end;
  Result:=true;
end;

procedure TSerie.SetBrush(const Value: TBrush);
begin
  if Assigned(FBrush) then
  FBrush.Assign(Value);
end;

procedure TSerie.SetContainer(const Value: TContainer);
begin
  if FContainer<>Value then
  begin
    FContainer:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetFirstLine(const Value: Integer);
begin
  if FFirstLine<>Value then
  begin
    FFirstLine:=Value;
    if (FLeaderPosition<>DefLastLine) and FIsRecording then
    begin
      if FLeaderPosition<FFirstLine
      then FLeaderPosition:=FFirstLine;
      if FLeaderPosition>FLastLine
      then FLeaderPosition:=FLastLine;
    end;
    Changed(false);
  end;
end;

procedure TSerie.SetInterleave(const Value: Integer);
begin
  if (FInterleave<>Value) and (Value>0) then
  begin
    FInterleave:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetIsFunction(const Value: Boolean);
begin
  if FIsFunction<>Value then
  begin
    FIsFunction:=Value;
    FXColumn:=0;
    FYColumn:=0;
    if Value
    then FIsRecording:=false;
    Changed(false);
  end;
end;

procedure TSerie.SetLastLine(const Value: Integer);
begin
  if FLastLine<>Value then
  begin
    FLastLine:=Value;
    if (FLeaderPosition<>DefLastLine) and FIsRecording then
    begin
      if FLeaderPosition<FFirstLine
      then FLeaderPosition:=FFirstLine;
      if FLeaderPosition>FLastLine
      then FLeaderPosition:=FLastLine;
    end;
    Changed(false);
  end;
end;

procedure TSerie.SetLineVisible(const Value: Boolean);
begin
  if FLineVisible<>Value then
  begin
    FLineVisible:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetPen(const Value: TPen);
begin
  if Assigned(FPen) then
  FPen.Assign(Value);
end;

procedure TSerie.SetPMax(const Value: Extended);
begin
  if FPMax<>Value then
  begin
    FPMax:=Value;
    if FIsFunction
    then Changed(false);
  end;
end;

procedure TSerie.SetPMin(const Value: Extended);
begin
  if FPMin<>Value then
  begin
    FPMin:=Value;
    if FIsFunction
    then Changed(false);
  end;
end;

procedure TSerie.SetPointSize(const Value: Integer);
begin
  if FPointSize<>Value then
  begin
    FPointSize:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetPointType(const Value: TPointType);
begin
  if FPointType<>Value then
  begin
    FPointType:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetPointVisible(const Value: Boolean);
begin
  if FPointVisible<>Value then
  begin
    FPointVisible:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetText(const Value: string);
begin
  if FText<>Value then
  begin
    FText:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetXAxis(const Value: TXAxis);
begin
  if FXAxis<>Value then
  begin
    FXAxis:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetYAxis(const Value: TYAxis);
begin
  if FYAxis<>Value then
  begin
    FYAxis:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetXColumn(const Value: Integer);
begin
  if FXColumn<>Value then
  begin
    FXColumn:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetXExpression(const Value: string);
begin
  if FXExpression<>Value then
  begin
    FXExpression:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetYColumn(const Value: Integer);
begin
  if FYColumn<>Value then
  begin
    FYColumn:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetYExpression(const Value: string);
begin
  if FYExpression<>Value then
  begin
    FYExpression:=Value;
    Changed(false);
  end;
end;

function TSerie._XAxis: TAxis;
begin
  Assert(Assigned(Collection));
  if FXAxis=TopAxis
  then _XAxis:=(Collection as TSeries).Plot.XAxis2
  else _XAxis:=(Collection as TSeries).Plot.XAxis;
  // correct for linked axis
  if Result.IsLinked
  then Result:=Result.GetLinkedAxis;
end;

function TSerie._YAxis: TAxis;
begin
  Assert(Assigned(Collection));
  if FYAxis=RightAxis
  then _YAxis:=(Collection as TSeries).Plot.YAxis2
  else _YAxis:=(Collection as TSeries).Plot.YAxis;
  // correct for linked axis
  if Result.IsLinked
  then Result:=Result.GetLinkedAxis;
end;

procedure TSerie.AddPoint;
var
  NotFirst: Boolean;
  P: TPlot;
  X, Y, X0, Y0: TReal;
  iX, iY, iX0, iY0: Integer;
begin
  if (not (FLineVisible or FPointVisible)) or (not FVisible) or IsFunction
    or (Container.Items.Count=0)
  then Exit; // nothing to do!
  P:=(Collection as TSeries).Plot;
  if FIsRecording
  then DrawLeader(P.Canvas); // hide old leader
  NotFirst:=(Container.Items.Count>1) and (not Empty);
  if NotFirst then
  NotFirst:=P.GetPoint(Container.Items.Count-2-FFirstLine, Index, X0, Y0);
  FLastLine:=Container.Items.Count-1;
  FLeaderPosition:=FLastLine;
  P.Canvas.Pen:=FPen;
  P.Canvas.Brush:=FBrush;
  if P.GetPoint(Container.Items.Count-1-FFirstLine, Index, X, Y) and
    ( (_XAxis.InRange(X) and _YAxis.InRange(Y)) or (not P.FClipPoints) ) then
  begin
    iX:=_XAxis.Scale(X);
    iY:=_YAxis.Scale(Y);
    // paint new line n<->n-1
    if FLineVisible and NotFirst then
    begin
      iX0:=_XAxis.Scale(X0);
      iY0:=_YAxis.Scale(Y0);
      P.Canvas.Brush.Style:=bsClear; // transparent dashes as in Paint()
      P.Canvas.Polyline([Point(iX, iY), Point(iX0, iY0)]);
      P.Canvas.Pen.Style:=psSolid; // points (and their contours) are always solid
      P.Canvas.Brush:=FBrush;
      if FPointVisible //  draw previous point over line!
      then DrawPoint(P.Canvas, iX0, iY0, FPointSize, FPointType);
    end;
    // paint new point
    if FPointVisible
    then DrawPoint(P.Canvas, iX, iY, FPointSize, FPointType);
  end;
  if FIsRecording
  then DrawLeader(P.Canvas); // show new leader
end;

procedure TSerie.SetVisible(const Value: Boolean);
begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetIsRecording(const Value: Boolean);
begin
  if (FIsRecording<>Value) and (not FIsFunction) then
  begin
    FIsRecording:=Value;
    if Value
    then FLeaderPosition:=FLastLine; // compatibility!
    DrawLeader((Collection as TSeries).Plot.Canvas);
    if not Value
    then FLeaderPosition:=DefLastLine; // else default will not work!
  end;
end;

//: used to draw a cross at the FLeaderPosition point of recording serie 
procedure TSerie.DrawLeader(const Canvas: TCanvas);
var
  RP: TRealPoint;
  P: TPoint;
begin
  if (not (Empty or IsFunction)) and FVisible and (FLeaderPosition<>DefLastLine) and
     (Collection as TSeries).Plot.GetPoint(FLeaderPosition{FLastLine}-FFirstLine,
     Index, RP.X, RP.Y) and _XAxis.InRange(RP.X) and _YAxis.InRange(RP.Y) then
  begin
    P.X:=_XAxis.Scale(RP.X, true);
    P.Y:=_YAxis.Scale(RP.Y, true);
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Mode:=pmXor;
    Canvas.Pen.Color:=clGray;
    Canvas.MoveTo(P.X, P.Y-2*FPointSize);
    Canvas.LineTo(P.X, P.Y+2*FPointSize);
    Canvas.MoveTo(P.X-2*FPointSize, P.Y);
    Canvas.LineTo(P.X+2*FPointSize, P.Y);
  end;
end;

//: Note: Serie Index written to the stream instead of ID (for correct loading) 
function TSerie.GetAreaBorder: Integer;
begin
  Result:=abcNothing;
  if FAreaBorder<0 then // NOT serie
  begin
    Result:=FAreaBorder;
    Exit;
  end;
  // now process serie reference
  if not Assigned((Collection as TSeries).Plot)
  then Exit; // plot component must exist!
  if csWriting in (Collection as TSeries).Plot.ComponentState then
  begin
    if Assigned(Collection.FindItemID(FAreaBorder))
    then Result:=Collection.FindItemID(FAreaBorder).Index;
  end else
    Result:=FAreaBorder;
end;

procedure TSerie.PaintArea(const Canvas: TCanvas);
var
  I, NumPoints, MaxPoints: Integer;
  P, P1, P2: TRealPoint;
  PointCache: array of TPoint;
  Plt: TPlot;
  Se: TSerie;
begin
  // area for empty series not displayed
  if Empty
  then Exit;
  // check FAreaBorder
  if {(}FAreaBorder<=abcNothing{) or (FAreaBorder=ID)!!!}
  then Exit; // wrong object reference!
  Se:=nil; // avoid warning
  if FAreaBorder>=0 then // serie?
  begin
    Se:=Collection.FindItemID(FAreaBorder) as TSerie;
    if (not Assigned(Se)) or Se.Empty
    then Exit; // wrong serie object!
  end;
  // initialize vars
  MaxPoints:=FLastLine-FFirstLine+1; // as in Paint()
  if FAreaBorder<0
  then MaxPoints:=MaxPoints+2;
  if (FAreaBorder>=0) and (FAreaBorder<>ID){!!!} then
  MaxPoints:=MaxPoints+Se.FLastLine-Se.FFirstLine+1;
  NumPoints:=0; // number of points in PointCache
  SetLength(PointCache, MaxPoints);
  Plt:=(Collection as TSeries).Plot;
  P1.X:=_XAxis.Min; // initialize P1,2 for better safety
  P2.X:=_XAxis.Max;
  P1.Y:=_YAxis.Min;
  P2.Y:=_YAxis.Max;
  // fill point cache - main series - as in Paint()
  for I:=0 to FLastLine-FFirstLine do
  if (((I mod FInterleave)=0) or FIsFunction) and
     Plt.GetPoint(I, Index, P.X, P.Y) and
     ((_XAxis.InRange(P.X) and _YAxis.InRange(P.Y)) or (not Plt.FClipPoints)) then
  begin
    PointCache[NumPoints].X:=_XAxis.Scale(P.X, true);
    PointCache[NumPoints].Y:=_YAxis.Scale(P.Y, true);
    if NumPoints=0 // remember 1st point (for axis object)
    then P1:=P;
    Inc(NumPoints);
  end;
  P2:=P; // remember last point (for axis object)
  // add points from AreaBorder - series
  if (FAreaBorder>=0) and (FAreaBorder<>ID){!!!} then
  with Se do
  for I:=FLastLine-FFirstLine downto 0 do
  // reverse order!!! we assume that both series are sorted similarly
  if (((I mod FInterleave)=0) or FIsFunction) and
     Plt.GetPoint(I, Index, P.X, P.Y) and
     ((_XAxis.InRange(P.X) and _YAxis.InRange(P.Y)) or (not Plt.FClipPoints)) then
  begin
    PointCache[NumPoints].X:=_XAxis.Scale(P.X, true);
    PointCache[NumPoints].Y:=_YAxis.Scale(P.Y, true);
    Inc(NumPoints);
  end;
  // add points from AreaBorder - XAxis
  if FAreaBorder=abcXAxis then
  begin
    PointCache[NumPoints].X:=_XAxis.Scale(P2.X, true);
    //PointCache[NumPoints].Y:=Plt.Height-Plt.BottomMargin;
    // need to use another method for correct printing!
    PointCache[NumPoints].Y:=Plt.YAxis.FOrigin; // not so safe... but works
    Inc(NumPoints);
    PointCache[NumPoints].X:=_XAxis.Scale(P1.X, true);
    PointCache[NumPoints].Y:=PointCache[NumPoints-1].Y;
    Inc(NumPoints);
  end;
  // add points from AreaBorder - XAxis2
  if FAreaBorder=abcXAxis2 then
  begin
    PointCache[NumPoints].X:=_XAxis.Scale(P2.X, true);
    //PointCache[NumPoints].Y:=Plt.TopMargin;
    PointCache[NumPoints].Y:=Plt.YAxis.FOrigin-Plt.YAxis.FLength;
    Inc(NumPoints);
    PointCache[NumPoints].X:=_XAxis.Scale(P1.X, true);
    PointCache[NumPoints].Y:=PointCache[NumPoints-1].Y;
    Inc(NumPoints);
  end;
  // add points from AreaBorder - YAxis
  if FAreaBorder=abcYAxis then
  begin
    //PointCache[NumPoints].X:=Plt.LeftMargin;
    PointCache[NumPoints].X:=Plt.XAxis.FOrigin;
    PointCache[NumPoints].Y:=_YAxis.Scale(P2.Y, true);
    Inc(NumPoints);
    PointCache[NumPoints].X:=PointCache[NumPoints-1].X;
    PointCache[NumPoints].Y:=_YAxis.Scale(P1.Y, true);
    Inc(NumPoints);
  end;
  // add points from AreaBorder - YAxis2
  if FAreaBorder=abcYAxis2 then
  begin
    //PointCache[NumPoints].X:=Plt.Width-Plt.RightMargin;
    PointCache[NumPoints].X:=Plt.XAxis.FOrigin+Plt.XAxis.FLength;
    PointCache[NumPoints].Y:=_YAxis.Scale(P2.Y, true);
    Inc(NumPoints);
    PointCache[NumPoints].X:=PointCache[NumPoints-1].X;
    PointCache[NumPoints].Y:=_YAxis.Scale(P1.Y, true);
    Inc(NumPoints);
  end;
  // draw (use main series painting tools)
  Canvas.Pen:=FPen;
  Canvas.Brush:=FBrush;
  Polygon(Canvas.Handle, PointCache[0], NumPoints);
end;

procedure TSerie.SetAreaBorder(Value: Integer);
begin
  if (FAreaBorder<>Value) and (Value>=abcNothing) {and (Value<Collection.Count)} then
  begin                                           { ^ streaming! }
    FAreaBorder:=Value;
    Changed(false);
  end;
end;

procedure TSerie.SetLeaderPosition(Value: Integer);
begin
  if (FLeaderPosition<>Value) and (Value>=FFirstLine)
    and (Value<=FLastLine) and FIsRecording then
  begin
    DrawLeader((Collection as TSeries).Plot.Canvas); // hide
    FLeaderPosition:=Value;
    DrawLeader((Collection as TSeries).Plot.Canvas); // show
  end;
end;

procedure TSerie.SetXErrorColumn(Value: Integer);
begin
  if FXErrorColumn <> Value then
  begin
    FXErrorColumn := Value;
    Changed(false);
  end;
end;

procedure TSerie.SetYErrorColumn(Value: Integer);
begin
  if FYErrorColumn <> Value then
  begin
    FYErrorColumn := Value;
    Changed(false);
  end;
end;

{---------- Class: TSeries ----------}
function TSeries.Add: TSerie;
begin
  Result:=TSerie(inherited Add);
end;

constructor TSeries.Create(APlot: TPlot);
begin
  inherited Create(TSerie);
  FPlot:=APlot;
end;

function TSeries.GetItem(Index: Integer): TSerie;
begin
  Result:=TSerie(inherited GetItem(Index));
end;

function TSeries.GetOwner: TPersistent;
begin
  Result:=FPlot;
end;

procedure TSeries.SetItem(Index: Integer; Value: TSerie);
begin
  inherited SetItem(Index, Value);
end;

procedure TSeries.Update(Item: TCollectionItem);
begin
  if Assigned(FPlot) then
  begin
  // FPlot.FSelectionVisible:=false; // hide selection to fix range check bug
  // ^ this line produce weird side effect in Plot.Delete - after first point
  // delete, series block changes and this method called. Then, since selection
  // turns off, all points until end will be deleted!!!
    if Assigned(Item)
    then FPlot.Changed(Item)
    else FPlot.Changed(Self);
  end;
end;

type
  TUndoZoomCoordinates = class (TObject)
  public
    OldY2: TReal;
    OldY1: TReal;
    OldX2: TReal;
    OldX1: TReal;
    YAxis: TAxis;
    XAxis: TAxis;
    Old2X1: TReal;
    Old2X2: TReal;
    Old2Y1: TReal;
    Old2Y2: TReal;
  end;
  
{---------- Class: TLabels ----------}
function TLabels.Add: TPlotLabel;
begin
  Result:=TPlotLabel(inherited Add);
end;

constructor TLabels.Create(APlot: TPlot);
begin
  inherited Create(TPlotLabel);
  FPlot:=APlot;
  FItemIndex:=-1;
end;

function TLabels.GetItem(Index: Integer): TPlotLabel;
begin
  Result:=TPlotLabel(inherited GetItem(Index));
end;

function TLabels.GetOwner: TPersistent;
begin
  Result:=FPlot;
end;

procedure TLabels.SetItem(Index: Integer; Value: TPlotLabel);
begin
  inherited SetItem(Index, Value);
end;

//: Notice: FItemIndex:=-1! 
procedure TLabels.Update(Item: TCollectionItem);
begin
  if Assigned(FPlot) then
  begin
    FItemIndex:=-1; // plot will redraw anyway!
    if Assigned(Item)
    then FPlot.Changed(Item)
    else FPlot.Changed(Self);
  end;
end;

//: Draws markers on the selected label (if any) 
procedure TLabels.DrawSelected(const Canvas: TCanvas);
begin
  if (Canvas<>Printer.Canvas) and Assigned(SelectedLabel)
  then SelectedLabel.PaintMarkers(Canvas);
end;

function TLabels.GetSelectedLabel: TPlotLabel;
begin
  if (FItemIndex>=0) and (FItemIndex<Count)
  then Result:=Items[FItemIndex]
  else Result:=nil;
end;

//: Changes index of the selected label 
procedure TLabels.SetItemIndex(Value: Integer);
begin
  if FPlot.FMouseMode<>pmLabelEdit
  then Exit;
  if FItemIndex <> Value then
  begin
    DrawSelected(FPlot.Canvas);
    FItemIndex := Value;
    DrawSelected(FPlot.Canvas);
  end;
end;

type
  TPlotLabelClipboardData=record
    Kind: TLabelKind;
    Visible, Transparent: boolean;
    Text, FontName: shortstring;
    PenColor, BrushColor, FontColor: TColor;
    BrushStyle: TBrushStyle;
    PenStyle: TPenStyle;
    FontSize, PenWidth: integer;
    FontStyle: TFontStyles;
    X1, X2, Y1, Y2: TReal;
    PinModeX1, PinModeY1, PinModeX2, PinModeY2: TLabelPinMode;
    Alignment: TLabelAlignment;
  end;

{---------- Class: TPlotLabel ----------}
procedure TPlotLabel.SetFont(const Value: TFont);
begin
  if Assigned(FFont) then
  FFont.Assign(Value);
end;

procedure TPlotLabel.Assign(Source: TPersistent);
begin
  if Source is TPlotLabel then
  begin
    LabelKind:=(Source as TPlotLabel).LabelKind;
    Pen.Assign((Source as TPlotLabel).Pen);
    Brush.Assign((Source as TPlotLabel).Brush);
    Font.Assign((Source as TPlotLabel).Font);
    Text:=TPlotLabel(Source).Text;
    Visible:=(Source as TPlotLabel).Visible;
    PinModeX1:=(Source as TPlotLabel).PinModeX1;
    PinModeY1:=(Source as TPlotLabel).PinModeY1;
    PinModeX2:=(Source as TPlotLabel).PinModeX2;
    PinModeY2:=(Source as TPlotLabel).PinModeY2;
    X1:=(Source as TPlotLabel).X1;
    X2:=(Source as TPlotLabel).X2;
    Y1:=(Source as TPlotLabel).Y1;
    Y2:=(Source as TPlotLabel).Y2;
    Transparent:=(Source as TPlotLabel).Transparent;
    Alignment:=(Source as TPlotLabel).Alignment;
  end else inherited Assign(Source);
end;

procedure TPlotLabel.CopyToClipboard;
var
  Data: Pointer;
  HData: THandle;
  LD: TPlotLabelClipboardData;
begin
  LD.Kind:=LabelKind;
  LD.Visible:=Visible;
  LD.Text:=Text;
  LD.FontName:=Font.Name;
  LD.PenColor:=Pen.Color;
  LD.BrushColor:=Brush.Color;
  LD.FontColor:=Font.Color;
  LD.BrushStyle:=Brush.Style;
  LD.PenStyle:=Pen.Style;
  LD.PenWidth:=Pen.Width;
  LD.FontSize:=Font.Size;
  LD.FontStyle:=Font.Style;
  LD.X1:=X1;
  LD.X2:=X2;
  LD.Y1:=Y1;
  LD.Y2:=Y2;
  LD.Transparent:=Transparent;
  LD.PinModeX1:=PinModeX1;
  LD.PinModeY1:=PinModeY1;
  LD.PinModeX2:=PinModeX2;
  LD.PinModeY2:=PinModeY2;
  LD.Alignment:=Alignment;
  ClipBoard.Open;
  try
    HData:=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, SizeOf(LD));
    try
      Data:=GlobalLock(HData);
      try
        Move(LD, Data^, SizeOf(LD));
        SetClipboardData(GetClipboardFormat, HData);
      finally
        GlobalUnlock(HData);
      end;
    except
      GlobalFree(HData); raise;
    end;
  finally
    ClipBoard.Close;
  end;
end;

constructor TPlotLabel.Create(Collection: TCollection);
begin
  inherited;
  FLabelKind:=lkText;
  FVisible:=true;
  FX1:=0;
  FY1:=0;
  FX2:=0;
  FY2:=0;
  FPen:=TPen.Create;
  FPen.OnChange:=OnChanged;
  FBrush:=TBrush.Create;
  FBrush.OnChange:=OnChanged;
  FFont:=TFont.Create;
  FFont.OnChange:=OnChanged;
  FSize:=Rect(0, 0, 0, 0);
  FPinModeX1:=lpmFrame;
  FPinModeY1:=lpmFrame;
  FPinModeX2:=lpmFrame;
  FPinModeY2:=lpmFrame;
  FTransparent:=false;
  FAlignment:=laTopLeft;
  // if (Collection is TLabels) and Assigned((Collection as TLabels).Plot.ThisSerie) then
  // begin
  //   FXAxis:=(Collection as TLabels).Plot.ThisSerie.XAxis;
  //   FYAxis:=(Collection as TLabels).Plot.ThisSerie.YAxis;
  // end;
  // wrong!!! see QA0127082008
end;

destructor TPlotLabel.Destroy;
begin
  if Assigned(FPen) then
  FPen.Free;
  if Assigned(FBrush) then
  FBrush.Free;
  if Assigned(FFont) then
  FFont.Free;
  inherited;
end;

class function TPlotLabel.GetClipboardFormat: Word;
begin
  Result:=RegisterClipboardFormat('TPlotLabel Object');
end;

function TPlotLabel.GetDisplayName: string;
begin
  Result:=Text;
  if Result=''
  then Result:=inherited GetDisplayName;
end;

//: called when font/pen/brush properties changed 
procedure TPlotLabel.OnChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TPlotLabel.Paint(const Canvas: TCanvas; Width, Height: integer);
var
  Txt: string;
  
  procedure PaintArrow;
  var
    Angle, Angle2: extended;
    iX1, iY1, iX2, iY2, dsize: integer;
    Arrow: array[0..2] of TPoint;
    X, Y, mX, mY, TxtW, TxtH: integer;
  const
    DashLength=10; // px
    DashAngle=Pi/10;
  
    function YX(X: integer): integer;
    begin
      Assert(iX1<>iX2);
      YX:=Round(iY1+(iY2-iY1)/(iX2-iX1)*(X-iX1));
    end;
  
    function XY(Y: integer): integer;
    begin
      Assert(iY1<>iY2);
      XY:=Round(iX1+(iX2-iX1)/(iY2-iY1)*(Y-iY1));
    end;
  
  begin
    // first paint arrow line
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    Canvas.Font:=FFont;
    dsize:=DashLength*Canvas.Pen.Width;
    if Canvas=Printer.Canvas then // correct pen width and dash size for print
    begin
      Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
      dsize:=Round(dsize*Canvas.Font.PixelsPerInch/PPI);
    end;
    iX1:=Scale(lcX1, Width, Canvas); // get int positions of arrow start and end
    iY1:=Scale(lcY1, Height, Canvas);
    iX2:=Scale(lcX2, Width, Canvas);
    iY2:=Scale(lcY2, Height, Canvas);
    if Txt<>'' then
    begin
      // calculate FSize by Txt
      HTMLCalculateSize(Canvas, Txt, TxtW, TxtH);
      Canvas.Font:=FFont; // restore font after HTMLCalculateSize
      mX:=Round(Canvas.TextWidth('W')/5); // calculate margins size
      mY:=Round(Canvas.TextHeight('H')/10);
      X:=iX1-(TxtW div 2)-2*mX;
      Y:=iY1-(TxtH div 2)-2*mY;
      FSize:=Rect(X, Y, X+TxtW+4*mX, Y+TxtH+4*mY); // save label area
      if fsItalic in Canvas.Font.Style // correction (?!)
      then FSize.Right:=FSize.Right+mX*2;
      {We could use here ExcludeClipRect or something like this.
      However, it seems more reliable to calculate arrow line clipping
      in transparent mode (QA0130082008) by hands.}
      // clip region - change 1st point (iX1, iY1)
      if FTransparent then
      begin
        if iX1=iX2 then
        begin
          if iY2>FSize.Bottom
          then iY1:=FSize.Bottom;
          if iY2<FSize.Top
          then iY1:=FSize.Top;
        end else
        if (iX1<iX2) and
          (FSize.Top<=YX(FSize.Right)) and (YX(FSize.Right)<=FSize.Bottom) then
        begin
          iY1:=YX(FSize.Right);
          iX1:=FSize.Right;
        end else
        if (iX2<iX1) and
          (FSize.Top<=YX(FSize.Left)) and (YX(FSize.Left)<=FSize.Bottom) then
        begin
          iY1:=YX(FSize.Left);
          iX1:=FSize.Left;
        end else
        if (iY1>iY2) and
          (FSize.Left<=XY(FSize.Top)) and (XY(FSize.Top)<=FSize.Right) then
        begin
          iX1:=XY(FSize.Top);
          iY1:=FSize.Top;
        end else
        if (iY1<iY2) and
          (FSize.Left<=XY(FSize.Bottom)) and (XY(FSize.Bottom)<=FSize.Right) then
        begin
          iX1:=XY(FSize.Bottom);
          iY1:=FSize.Bottom;
        end;
      end;
    end else
    begin // avoid compiler warning (these values never will be used!)
      X:=0; Y:=0; mX:=0; mY:=0;
    end;
    Canvas.Brush.Style:=bsClear; // paint only dashes
    Canvas.MoveTo(iX1, iY1);
    Canvas.LineTo(iX2, iY2);
    if FLabelKind=lkArrow then // paint arrow
    begin
      if iX2>iX1
      then Angle2:=0
      else Angle2:=Pi;
      if iX2<>iX1
      then Angle:=Angle2-arctan((iY1-iY2)/(iX2-iX1))
      else Angle:=-Pi/2*Sign(iY1-iY2);
      Arrow[0]:=Point(iX2-Round(dsize*cos(Angle-DashAngle)),
        iY2-Round(dsize*sin(Angle-DashAngle)));
      Arrow[1]:=Point(iX2, iY2);
      Arrow[2]:=Point(iX2-Round(dsize*cos(Angle+DashAngle)),
        iY2-Round(dsize*sin(Angle+DashAngle)));
      Canvas.Brush:=FBrush;
      Canvas.Brush.Style:=bsSolid;
      Canvas.Pen.Style:=psSolid;
      Canvas.Polygon(Arrow);
      Canvas.Pen.Style:=FPen.Style;
    end;
    // then paint text as in PaintText
    if Txt<>'' then
    begin
      Canvas.Brush:=FBrush;
      if not FTransparent
      then Canvas.Rectangle(FSize);
      HTMLTextOut(Canvas, Txt, X+2*mX, Y+2*mY, false);
    end;
  end;
  
  procedure CalcAlignment(var alX, alY: integer); // FSize must be initialized!
  begin
    alX:=0; alY:=0;
    case FAlignment of
      laTopRight: alX:=FSize.Left-FSize.Right;
      laBottomLeft: alY:=FSize.Top-FSize.Bottom;
      laBottomRight:
      begin
        alX:=FSize.Left-FSize.Right; alY:=FSize.Top-FSize.Bottom;
      end;
      laCenter:
      begin
        alX:=(FSize.Left-FSize.Right) div 2; alY:=(FSize.Top-FSize.Bottom) div 2;
      end;
      laLeftCenter: alY:=(FSize.Top-FSize.Bottom) div 2;
      laTopCenter: alX:=(FSize.Left-FSize.Right) div 2;
      laRightCenter:
      begin
        alX:=FSize.Left-FSize.Right; alY:=(FSize.Top-FSize.Bottom) div 2;
      end;
      laBottomCenter:
      begin
        alX:=(FSize.Left-FSize.Right) div 2; alY:=FSize.Top-FSize.Bottom;
      end;
    end;
  end;
  
  procedure PaintText;
  var
    X, Y, mX, mY, TxtW, TxtH, alX, alY: integer;
  begin
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    Canvas.Font:=FFont;
    if Canvas=Printer.Canvas // correct pen width for print
    then Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
    HTMLCalculateSize(Canvas, Txt, TxtW, TxtH);
    Canvas.Font:=FFont; // restore font after HTMLCalculateSize
    mX:=Round(Canvas.TextWidth('W')/5); // calculate margins size
    mY:=Round(Canvas.TextHeight('H')/10);
    X:=Scale(lcX1, Width, Canvas); // calculate position
    Y:=Scale(lcY1, Height, Canvas);
    if FLabelKind=lkText then
    begin
      FSize:=Rect(X, Y, X+TxtW+4*mX, Y+TxtH+4*mY); // save label area
      if fsItalic in Canvas.Font.Style // correction (?!)
      then FSize.Right:=FSize.Right+mX*2;
    end else
      FSize:=Rect(X, Y, X+TxtH+4*mY, Y+TxtW+5*mX);
    CalcAlignment(alX, alY); // calculate alignment
    // draw label elements
    Canvas.Brush:=FBrush; // restore brush after HTMLCalculateSize (bsClear!)
    OffsetRect(FSize, alX, alY);
    if not FTransparent
    then Canvas.Rectangle(FSize);
    if FLabelKind=lkText
    then HTMLTextOut(Canvas, Txt, X+2*mX+alX, Y+2*mY+alY, false)
    else HTMLTextOut(Canvas, Txt, X+3*mY+alX, Y+TxtW+2*mX+alY, true);
  end;
  
  procedure PaintLegend;
  var
    I, N, X, Y, Yy, mX, mY, TxtW, TxtH, LegW, psize, W1, H1, II, alX, alY: integer;
    Plt: TPlot;
    SL: TStringList;
  begin
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    Canvas.Font:=FFont;
    if Canvas=Printer.Canvas // correct pen width for print
    then Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
    LegW:=Canvas.TextHeight('H')*3; // width of series legend (line+symbol)
    SL:=TStringList.Create;
    try
      SL.CommaText:=ProcessLegendAction(Txt, plaRemove, '');
      if Txt='' // actually never should be! checked in Paint()
      then begin TxtW:=0; TxtH:=0; end
      else HTMLCalculateSize(Canvas, Txt, TxtW, TxtH);
      Canvas.Font:=FFont; // restore font after HTMLCalculateSize
      TxtW:=TxtW-LegW; // correct width to compare label text with series text
      mX:=Round(Canvas.TextWidth('W')/5); // calculate margins size
      mY:=Round(Canvas.TextHeight('H')/10);
      X:=Scale(lcX1, Width, Canvas); // calculate position
      Y:=Scale(lcY1, Height, Canvas);
      Assert(Assigned(Collection));
      Plt:=(Collection as TLabels).Plot;
      Assert(Assigned(Plt));
      N:=0; // calculate number of series and actual width
      for I:=0 to Plt.Series.Count-1 do
      if (not Plt.Series[I].Empty) and Plt.Series[I].Visible then
      begin
        if (SL.Count>0) and (SL.IndexOfName(IntToStr(I))=-1)
        then Continue; // series not listed in custom order
        Inc(N);
        if SL.Values[IntToStr(I)]<>'' // use alternative series text!
        then HTMLCalculateSize(Canvas, SL.Values[IntToStr(I)], W1, H1)
        else HTMLCalculateSize(Canvas, Plt.Series[I].Text, W1, H1);
        Canvas.Font:=FFont; // restore font after HTMLCalculateSize
        TxtW:=Max(W1, TxtW); // H1 disregarded!
      end;
      H1:=Canvas.TextHeight('H'); // height of series text (single line only!!!)
      if (Txt='') and (N=0) then
      // Exit;  warning!!! nothing to display. Legend MAY disappear!
      begin // in this case, we need to paint something
        Txt:=strPlotNoLabelText;
        TxtW:=Canvas.TextWidth(strPlotNoLabelText);
        LegW:=0;
        TxtH:=H1+3*mY;
      end;
      // now paint legend
      FSize:=Rect(X, Y, LegW+X+TxtW+4*mX, Y+TxtH+(H1+mY)*N+{4}5*mY); // save
      if fsItalic in Canvas.Font.Style // correction (?!)
      then FSize.Right:=FSize.Right+mX*2;
      CalcAlignment(alX, alY); // calculate alignment
      OffsetRect(FSize, alX, alY); // move FSize
      X:=X+alX; Y:=Y+alY; // move base point
      Canvas.Brush:=FBrush; // restore brush after HTMLCalculateSize (bsClear!)
      if not FTransparent
      then Canvas.Rectangle(FSize);
      HTMLTextOut(Canvas, Txt, X+2*mX, Y+2*mY, false);
      Canvas.Font:=FFont; // restore font after HTMLTextOut
      // paint series (text and legend)
      N:=0; // counts vertical shift
      I:=H1; H1:=TxtH; TxtH:=I; // copy to avoid changes in the rest of code
      II:=0; // counts tested series
      while ((SL.Count=0) and (II<Plt.Series.Count))
        or ((SL.Count>0) and (II<SL.Count)) do
      begin
        if SL.Count=0
        then I:=II // no custom order
        else I:=StrToIntDef(SL.Names[II], -1); // use custom order
        if (I<0) or (I>=Plt.Series.Count) or // I valid?
          (not ((not Plt.Series[I].Empty) and Plt.Series[I].Visible)) then
        begin
          Inc(II);
          Continue; // series is not visible, or wrong custom order index
        end;
        Yy:=H1+Y+2*mY+N*(TxtH+mY); // top of series
        if SL.Values[IntToStr(I)]<>'' // use alternative series text!
        then HTMLTextOut(Canvas, SL.Values[IntToStr(I)], LegW+X+2*mX, Yy, false)
        else HTMLTextOut(Canvas, Plt.Series[I].Text, LegW+X+2*mX, Yy, false);
        Canvas.Font:=FFont; // restore font after HTMLTextOut
        // paint axes state
        with Canvas do
        begin
          Pen.Color:=Font.Color;
          Pen.Style:=psSolid;
          Pen.Mode:=pmCopy;
          Pen.Width:=1;
        end;
        if Canvas=Printer.Canvas // correct pen width for print
        then Canvas.Pen.Width:=Round(Canvas.Font.PixelsPerInch/PPI);
        if (Plt.Series[I].XAxis=BottomAxis) and (Plt.Series[I].YAxis=LeftAxis)
        then Canvas.PolyLine([Point(X+mX, Yy+1), Point(X+mX, Yy+TxtH-1),
          Point(X+mX+LegW, Yy+TxtH-1)]);
        if (Plt.Series[I].XAxis=BottomAxis) and (Plt.Series[I].YAxis=RightAxis)
        then Canvas.PolyLine([Point(X+mX, Yy+TxtH-1), Point(X+mX+LegW, Yy+TxtH-1),
          Point(X+mX+LegW, Yy+1)]);
        if (Plt.Series[I].XAxis=TopAxis) and (Plt.Series[I].YAxis=LeftAxis)
        then Canvas.PolyLine([Point(X+mX, Yy+TxtH-1), Point(X+mX, Yy+1),
          Point(X+mX+LegW, Yy+1)]);
        if (Plt.Series[I].XAxis=TopAxis) and (Plt.Series[I].YAxis=RightAxis)
        then Canvas.PolyLine([Point(X+mX, Yy+1), Point(X+mX+LegW, Yy+1),
          Point(X+mX+LegW, Yy+TxtH-1)]);
        // paint line and point
        Canvas.Pen:=Plt.Series[I].Pen;
        // correct line width and point size
        psize:=Plt.Series[I].PointSize;
        if Canvas=Printer.Canvas then
        begin
          psize:=Round(psize*Canvas.Font.PixelsPerInch/PPI);
          Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
        end;
        Yy:=Yy+((TxtH+mY) div 2);
        Canvas.MoveTo(X+mX, Yy);
        if Plt.Series[I].LineVisible
        then Canvas.LineTo(X+LegW+mX, Yy);
        Canvas.Brush:=Plt.Series[I].Brush;
        Canvas.Pen.Style:=psSolid;
        if Plt.Series[I].PointVisible
        // then DrawPoint(Canvas, X+(LegW div 2)+mX, Yy, TxtH div 2,
        // here we'll use "actual" point size instead of equal as commented above
        then DrawPoint(Canvas, X+(LegW div 2)+mX, Yy, psize,
          Plt.Series[I].PointType);
        Inc(N);
        Inc(II);
      end;
    finally
      SL.Free;
    end;
  end;
  
  procedure PaintBalloon;
  var
    I, X, Y, Xx, Yy, Xc, Yc, mX, mY, R, Rv, d1, d2, d3, TxtW, TxtH, alX, alY: integer;
    AP: array[0..31] of TPoint;
  
    function GT1: boolean;
    begin
      Result:=Yy<=(FSize.Bottom-FSize.Top)/(FSize.Right-FSize.Left)*(Xx-Xc)+Yc;
    end;
  
    function GT2: boolean;
    begin
      Result:=Yy<=(FSize.Bottom-FSize.Top)/(FSize.Right-FSize.Left)*(Xc-Xx)+Yc;
    end;
  
  begin
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    Canvas.Font:=FFont;
    if Canvas=Printer.Canvas // correct pen width for print
    then Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
    HTMLCalculateSize(Canvas, Txt, TxtW, TxtH);
    Canvas.Font:=FFont; // restore font after HTMLCalculateSize
    mX:=Round(Canvas.TextWidth('W')/5); // calculate margins size
    mY:=Round(Canvas.TextHeight('H')/10);
    X:=Scale(lcX1, Width, Canvas); // position of block (as for text)
    Y:=Scale(lcY1, Height, Canvas);
    Xx:=Scale(lcX2, Width, Canvas); // position of pointer
    Yy:=Scale(lcY2, Height, Canvas);
    // paint block background and stem
    ///Canvas.RoundRect(X, Y, X+TxtW+4*mX, Y+(TxtH+mY)*SL.Count+2*mY, TxtH, TxtH);
    FSize:=Rect(X, Y, X+TxtW+4*mX, Y+TxtH+4*mY); // save label area
    if fsItalic in Canvas.Font.Style // correction (?!)
    then FSize.Right:=FSize.Right+mX*2;
    CalcAlignment(alX, alY); // calculate alignment
    OffsetRect(FSize, alX, alY); // move FSize
    X:=X+alX; Y:=Y+alY; // move base point
    Xc:=(FSize.Left+FSize.Right) div 2; // position of center of block
    Yc:=(FSize.Top+FSize.Bottom) div 2;
    // corner roundings (corners formed by 5 points and 2x2 extra dashes)
    R:=(Canvas.TextHeight('H') div 2)-2; // corner rounding radius
    if FSize.Bottom-FSize.Top>4*R // size of vert. stem root
    then Rv:=R
    else Rv:=(FSize.Bottom-FSize.Top) div 4;
    if FSize.Top+R>=Yc-Rv // correct rounding errors
    then Rv:=Yc-FSize.Top-R-1;
    d1:=Round(R*(1-cos(Pi/8)));
    d2:=Round(R*(1-sin(Pi/4)));
    d3:=Round(R*sin(Pi/8));
    // top left corner
    for I:=0 to 7 do // fill
    AP[I]:=FSize.TopLeft;
    AP[0]:=Point(AP[0].X, AP[0].Y+R); // correct
    AP[1]:=Point(AP[1].X+d1, AP[1].Y+R-d3);
    AP[2]:=Point(AP[2].X+d2, AP[2].Y+d2);
    AP[3]:=Point(AP[3].X+R-d3, AP[3].Y+d1);
    AP[4]:=Point(AP[4].X+R, AP[4].Y);
    AP[5]:=Point(Xc-R, FSize.Top);
    AP[6]:=Point(Xc, FSize.Top);
    AP[7]:=Point(Xc+R, FSize.Top);
    // top right corner
    for I:=8 to 15 do // fill
    AP[I]:=Point(FSize.Right, FSize.Top);
    AP[8]:=Point(AP[8].X-R, AP[8].Y); // correct
    AP[9]:=Point(AP[9].X-R+d3, AP[9].Y+d1);
    AP[10]:=Point(AP[10].X-d2, AP[10].Y+d2);
    AP[11]:=Point(AP[11].X-d1, AP[11].Y+R-d3);
    AP[12]:=Point(AP[12].X, AP[12].Y+R);
    AP[13]:=Point(FSize.Right, Yc-Rv);
    AP[14]:=Point(FSize.Right, Yc);
    AP[15]:=Point(FSize.Right, Yc+Rv);
    // bottom right corner
    for I:=16 to 23 do // fill
    AP[I]:=FSize.BottomRight;
    AP[16]:=Point(AP[16].X, AP[16].Y-R); // correct
    AP[17]:=Point(AP[17].X-d1, AP[17].Y-R+d3);
    AP[18]:=Point(AP[18].X-d2, AP[18].Y-d2);
    AP[19]:=Point(AP[19].X-R+d3, AP[19].Y-d1);
    AP[20]:=Point(AP[20].X-R, AP[20].Y);
    AP[21]:=Point(Xc+R, FSize.Bottom);
    AP[22]:=Point(Xc, FSize.Bottom);
    AP[23]:=Point(Xc-R, FSize.Bottom);
    // bottom left corner
    for I:=24 to 31 do // fill
    AP[I]:=Point(FSize.Left, FSize.Bottom);
    AP[24]:=Point(AP[24].X+R, AP[24].Y); // correct
    AP[25]:=Point(AP[25].X+R-d3, AP[25].Y-d1);
    AP[26]:=Point(AP[26].X+d2, AP[26].Y-d2);
    AP[27]:=Point(AP[27].X+d1, AP[27].Y-R+d3);
    AP[28]:=Point(AP[28].X, AP[28].Y-R);
    AP[29]:=Point(FSize.Left, Yc+Rv);
    AP[30]:=Point(FSize.Left, Yc);
    AP[31]:=Point(FSize.Left, Yc-Rv);
    // now decide what side of balloon should be used for stem root
    // (and modify stem root points)
    if GT1 and GT2
    then AP[6]:=Point(Xx, Yy);
    if (not GT1) and (not GT2)
    then AP[22]:=Point(Xx, Yy);
    if (not GT2) and GT1
    then AP[14]:=Point(Xx, Yy);
    if (not GT1) and GT2
    then AP[30]:=Point(Xx, Yy);
    Canvas.Brush:=FBrush; // restore brush after HTMLCalculateSize (bsClear!)
    if FTransparent
    then Canvas.Brush.Style:=bsClear;
    Canvas.Polygon(AP);
    // paint text
    HTMLTextOut(Canvas, Txt, X+2*mX, Y+2*mY, false);
  end;
  
  procedure PaintRectangle;
  var
    TxtW, TxtH, mX, mY: integer;
  begin
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    if Canvas=Printer.Canvas // correct pen width for print
    then Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
    FSize.Left:=Scale(lcX1, Width, Canvas);
    FSize.Top:=Scale(lcY1, Height, Canvas);
    FSize.Right:=Scale(lcX2, Width, Canvas);
    FSize.Bottom:=Scale(lcY2, Height, Canvas);
    CorRect(FSize.Left, FSize.Top, FSize.Right, FSize.Bottom);
    if FTransparent
    then Canvas.Brush.Style:=bsClear;
    Canvas.Rectangle(FSize);
    if FText<>'' then
    begin
      Canvas.Font:=FFont;
      HTMLCalculateSize(Canvas, FText, TxtW, TxtH);
      Canvas.Font:=FFont; // restore font after HTMLCalculateSize
      mX:=Round(Canvas.TextWidth('W')/4); // calculate margins size
      mY:=Round(Canvas.TextHeight('H')/8);
      case FAlignment of
        laTopLeft: HTMLTextOut(Canvas, FText, FSize.Left+mX, FSize.Top+mY, false);
        laTopRight: HTMLTextOut(Canvas, FText, FSize.Right-mX-TxtW,
          FSize.Top+mY, false);
        laBottomLeft: HTMLTextOut(Canvas, FText, FSize.Left+mX,
          FSize.Bottom-mY-TxtH, false);
        laBottomRight: HTMLTextOut(Canvas, FText, FSize.Right-mX-TxtW,
          FSize.Bottom-mY-TxtH, false);
        laCenter: HTMLTextOut(Canvas, FText, ((FSize.Left+FSize.Right) div 2)
          - TxtW div 2, ((FSize.Top+FSize.Bottom) div 2) - TxtH div 2, false);
        laLeftCenter: HTMLTextOut(Canvas, FText, FSize.Left+mX,
          ((FSize.Top+FSize.Bottom) div 2) - TxtH div 2, false);
        laTopCenter: HTMLTextOut(Canvas, FText, ((FSize.Left+FSize.Right) div 2)
          - TxtW div 2, FSize.Top+mY, false);
        laRightCenter: HTMLTextOut(Canvas, FText, FSize.Right-mX-TxtW,
          ((FSize.Top+FSize.Bottom) div 2) - TxtH div 2, false);
        laBottomCenter: HTMLTextOut(Canvas, FText, ((FSize.Left+FSize.Right) div 2)
          - TxtW div 2, FSize.Bottom-mY-TxtH, false);
      end;
    end;
  end;
  
  procedure PaintEllipse;
  var
    TxtW, TxtH: integer;
  begin
    Canvas.Pen:=FPen;
    Canvas.Brush:=FBrush;
    if Canvas=Printer.Canvas // correct pen width for print
    then Canvas.Pen.Width:=Round(Canvas.Pen.Width*Canvas.Font.PixelsPerInch/PPI);
    FSize.Left:=Scale(lcX1, Width, Canvas);
    FSize.Top:=Scale(lcY1, Height, Canvas);
    FSize.Right:=Scale(lcX2, Width, Canvas);
    FSize.Bottom:=Scale(lcY2, Height, Canvas);
    CorRect(FSize.Left, FSize.Top, FSize.Right, FSize.Bottom);
    if FTransparent
    then Canvas.Brush.Style:=bsClear;
    Canvas.Ellipse(FSize);
    if FText<>'' then
    begin
      Canvas.Font:=FFont;
      HTMLCalculateSize(Canvas, FText, TxtW, TxtH);
      Canvas.Font:=FFont; // restore font after HTMLCalculateSize
      HTMLTextOut(Canvas, FText, ((FSize.Left+FSize.Right) div 2) - TxtW div 2,
        ((FSize.Top+FSize.Bottom) div 2) - TxtH div 2, false);
    end;
  end;
  
begin
  FSize:=Rect(0, 0, 0, 0); // clear label area
  Txt:=FText;
  if ((Txt='') or (HTML2Text(Txt)='')) and (FLabelKind in [lkText, lkVText, lkBalloon])
  then Txt:=strPlotNoLabelText; // always paint something in ballon and text
  // paint label (and initialize label area)
  if FVisible then
  case FLabelKind of
    lkText, lkVText: PaintText;
    lkLegend: PaintLegend;
    lkArrow, lkLine: PaintArrow;
    lkBalloon: PaintBalloon;
    lkRectangle: PaintRectangle;
    lkEllipse: PaintEllipse;
    // todo: other label types
  end;
end;

procedure TPlotLabel.PasteFromClipboard;
var
  HData: THandle;
  LD: TPlotLabelClipboardData;
  Data: Pointer;
begin
  if Clipboard.HasFormat(GetClipboardFormat) then
  begin
    Clipboard.Open;
    try
      HData:=GetClipboardData(GetClipboardFormat);
      if HData=0 then Exit;
      Data:=GlobalLock(HData);
      if Data=nil then Exit;
      try
        LD:=TPlotLabelClipboardData(Data^);
        LabelKind:=LD.Kind;
        Visible:=LD.Visible;
        Text:=LD.Text;
        Font.Name:=LD.FontName;
        Pen.Color:=LD.PenColor;
        Pen.Width:=LD.PenWidth;
        Brush.Color:=LD.BrushColor;
        Font.Color:=LD.FontColor;
        Brush.Style:=LD.BrushStyle;
        Pen.Style:=LD.PenStyle;
        Font.Size:=LD.FontSize;
        Font.Style:=LD.FontStyle;
        Transparent:=LD.Transparent;
        Alignment:=LD.Alignment;
        PinModeX1:=LD.PinModeX1;
        PinModeY1:=LD.PinModeY1;
        PinModeX2:=LD.PinModeX2;
        PinModeY2:=LD.PinModeY2;
        X1:=LD.X1;
        X2:=LD.X2;
        Y1:=LD.Y1;
        Y2:=LD.Y2;
      finally
        GlobalUnlock(HData);
      end;
    finally
      Clipboard.Close;
    end;
  end;
end;

procedure TPlotLabel.SetBrush(const Value: TBrush);
begin
  if Assigned(FBrush) then
  FBrush.Assign(Value);
end;

procedure TPlotLabel.SetPen(const Value: TPen);
begin
  if Assigned(FPen) then
  FPen.Assign(Value);
end;

procedure TPlotLabel.SetText(const Value: string);
begin
  if FText<>Value then
  begin
    FText:=Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetXAxis(const Value: TXAxis);
begin
  if Value=BottomAxis then
  begin
    if PinModeX1=lpmScale2
    then PinModeX1:=lpmScale;
    if PinModeX1=lpmAxis2
    then PinModeX1:=lpmAxis;
    if PinModeX2=lpmScale2
    then PinModeX2:=lpmScale;
    if PinModeX2=lpmAxis2
    then PinModeX2:=lpmAxis;
  end else
  begin
    if PinModeX1=lpmScale
    then PinModeX1:=lpmScale2;
    if PinModeX1=lpmAxis
    then PinModeX1:=lpmAxis2;
    if PinModeX2=lpmScale
    then PinModeX2:=lpmScale2;
    if PinModeX2=lpmAxis
    then PinModeX2:=lpmAxis2;
  end;
end;

procedure TPlotLabel.SetYAxis(const Value: TYAxis);
begin
  if Value=LeftAxis then
  begin
    if PinModeY1=lpmScale2
    then PinModeY1:=lpmScale;
    if PinModeY1=lpmAxis2
    then PinModeY1:=lpmAxis;
    if PinModeY2=lpmScale2
    then PinModeY2:=lpmScale;
    if PinModeY2=lpmAxis2
    then PinModeY2:=lpmAxis;
  end else
  begin
    if PinModeY1=lpmScale
    then PinModeY1:=lpmScale2;
    if PinModeY1=lpmAxis
    then PinModeY1:=lpmAxis2;
    if PinModeY2=lpmScale
    then PinModeY2:=lpmScale2;
    if PinModeY2=lpmAxis
    then PinModeY2:=lpmAxis2;
  end;
end;

function TPlotLabel._XAxis(LabelCoordinate: TLabelCoordinates): TAxis;
begin
  Result:=nil;
  if (Collection is TLabels) then
  begin
    Assert(Assigned((Collection as TLabels).Plot));
    if LabelCoordinate=lcX1 then
    begin
      if FPinModeX1=lpmScale
      then Result:=(Collection as TLabels).Plot.XAxis;
      if FPinModeX1=lpmScale2
      then Result:=(Collection as TLabels).Plot.XAxis2;
    end;
    if LabelCoordinate=lcX2 then
    begin
      if FPinModeX2=lpmScale
      then Result:=(Collection as TLabels).Plot.XAxis;
      if FPinModeX2=lpmScale2
      then Result:=(Collection as TLabels).Plot.XAxis2;
    end;
  end;
  // correct for linked axis
  if Assigned(Result) and Result.IsLinked
  then Result:=Result.GetLinkedAxis;
  // safecheck
  Assert(Assigned(Result));
end;

function TPlotLabel._YAxis(LabelCoordinate: TLabelCoordinates): TAxis;
begin
  Result:=nil;
  if (Collection is TLabels) then
  begin
    Assert(Assigned((Collection as TLabels).Plot));
    if LabelCoordinate=lcY1 then
    begin
      if FPinModeY1=lpmScale
      then Result:=(Collection as TLabels).Plot.YAxis;
      if FPinModeY1=lpmScale2
      then Result:=(Collection as TLabels).Plot.YAxis2;
    end;
    if LabelCoordinate=lcY2 then
    begin
      if FPinModeY2=lpmScale
      then Result:=(Collection as TLabels).Plot.YAxis;
      if FPinModeY2=lpmScale2
      then Result:=(Collection as TLabels).Plot.YAxis2;
    end;
  end;
  // correct for linked axis
  if Assigned(Result) and Result.IsLinked
  then Result:=Result.GetLinkedAxis;
  // safecheck
  Assert(Assigned(Result));
end;

procedure TPlotLabel.SetVisible(const Value: Boolean);
begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    Changed(false);
  end;
end;

//: Returns true if given integer coordinates belong to the label area 
function TPlotLabel.Belong(iX, iY: integer): Boolean;
var
  R: TRect;
  Plt: TPlot;
  ix1, ix2, iy1, iy2: Integer;
begin
  Result:=false;
  Assert(Assigned(Collection));
  Plt:=(Collection as TLabels).Plot;
  Assert(Assigned(Plt));
  if (FSize.Left<>0) or (FSize.Right<>0) or (FSize.Top<>0) or (FSize.Bottom<>0) then
  begin // check if block area initialized
    R:=FSize;
    if Plt.FPMInfo.InProgress then
    begin
      R.Left:=R.Left+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Right:=R.Right+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Top:=R.Top+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
      R.Bottom:=R.Bottom+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
    Result:=PtInRect(R, Point(iX, iY));
  end;
  if FLabelKind in [lkArrow, lkLine] then // check the arrow line
  begin
    if Result // block area OK!
    then Exit;
    // now check points along the line
    ix1:=Scale(lcX1);
    ix2:=Scale(lcX2);
    iy1:=Scale(lcY1);
    iy2:=Scale(lcY2);
    if (ix1=ix2) and (iy1=iy2) then
    begin // zero-length arrow requires special processing!
      Result:=InRange(ix1, iX-MSZ, iX+MSZ) and InRange(iy1, iY-MSZ, iY+MSZ);
      Exit;
    end;
    if ix1<>ix2 then
    begin {/}
      Result:=InRange(iX, Min(ix1, ix2)-MSZ, Max(ix1, ix2)+MSZ)
        and InRange(iY, Min(iy1, iy2)-MSZ, Max(iy1, iy2)+MSZ);
      if not Result
      then Exit; // X out of range!
      Result:=Abs(Round( (ix2-ix1)/Sqrt(Sqr(ix2-ix1)+Sqr(iy2-iy1)) *
        (iy1+(iy2-iy1)/(ix2-ix1)*(iX-ix1)-iY) ))<MSZ;
    end else {|}
      Result:=(Abs(ix1-iX)<MSZ) and InRange(iY, Min(iy1, iy2), Max(iy1, iy2));
  end;
  if FLabelKind=lkBalloon
  then Result:=Result or Plt.BelongMarker(Scale(lcX2), Scale(lcY2), iX, iY);
  // todo: TPlot->Belong for balloon label stem body?
  if FLabelKind in [lkRectangle, lkEllipse] then
  begin // process case of zero-width or zero-height FSize, when PtInRect fails
    if R.Left=R.Right
    then Result:=InRange(iX, R.Left-MSZ, R.Left+MSZ) and
      InRange(iY, Min(R.Top, R.Bottom), Max(R.Top, R.Bottom));
    if R.Top=R.Bottom
    then Result:=InRange(iY, R.Top-MSZ, R.Top+MSZ) and
      InRange(iX, Min(R.Left, R.Right), Max(R.Left, R.Right));
  end;
  // todo: other label types
end;

function TPlotLabel.GetXAxis: TXAxis;
begin
  if FPinModeX1 in [lpmAxis2, lpmScale2]
  then Result:=TopAxis
  else Result:=BottomAxis;
end;

function TPlotLabel.GetYAxis: TYAxis;
begin
  if FPinModeY1 in [lpmAxis2, lpmScale2]
  then Result:=RightAxis
  else Result:=LeftAxis;
end;

//: Paint label markers 
procedure TPlotLabel.PaintMarkers(const Canvas: TCanvas);
var
  R: TRect;
  Plt: TPlot;
  iX, iY: Integer;
  
  procedure StretchR(xc, yc: TLabelCoordinates; var R: TRect);
  var
    iX, iY: integer;
  begin
    iX:=Scale(xc);
    iY:=Scale(yc);
    if R.Left=iX
    then R.Left:=R.Left+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo)
    else R.Right:=R.Right+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
    if R.Top=iY
    then R.Top:=R.Top+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo)
    else R.Bottom:=R.Bottom+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
  end;
  
begin
  if not FVisible
  then Exit; // cannot select invisible label
  with Canvas do // initialize drawing tools
  begin
    Pen.Width:=1;
    Pen.Style:=psDash;
    Pen.Mode:=pmXor;
    Pen.Color:=clGray;
    Brush.Color:=clGray;
  end;
  Assert(Assigned(Collection));
  Plt:=(Collection as TLabels).Plot;
  Assert(Assigned(Plt));
  // todo: other label types
  if FLabelKind in [lkRectangle, lkEllipse] then
  begin
    R:=FSize;
    // modify FSize rectangle if it is moved or stretched
    if Plt.FPMInfo.InProgress and (not Plt.FPMInfo.Move1)
      and (not Plt.FPMInfo.Move2) and (not Plt.FPMInfo.Move3)
      and (not Plt.FPMInfo.Move4) then
    begin
      R.Left:=R.Left+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Right:=R.Right+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Top:=R.Top+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
      R.Bottom:=R.Bottom+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
    if Plt.FPMInfo.InProgress and Plt.FPMInfo.Move1
    then StretchR(lcX1, lcY1, R);
    if Plt.FPMInfo.InProgress and Plt.FPMInfo.Move2
    then StretchR(lcX2, lcY2, R);
    if Plt.FPMInfo.InProgress and Plt.FPMInfo.Move3
    then StretchR(lcX1, lcY2, R);
    if Plt.FPMInfo.InProgress and Plt.FPMInfo.Move4
    then StretchR(lcX2, lcY1, R);
    // correct frame size (QA0118012009)
    R.Bottom:=R.Bottom-1;
    R.Right:=R.Right-1;
    // draw frame
    Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
    R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
    // draw handles
    Canvas.Rectangle(R.Left-MSZ, R.Top-MSZ, R.Left+MSZ, R.Top+MSZ);
    Canvas.Rectangle(R.Right-MSZ, R.Top-MSZ, R.Right+MSZ, R.Top+MSZ);
    Canvas.Rectangle(R.Right-MSZ, R.Bottom-MSZ, R.Right+MSZ, R.Bottom+MSZ);
    Canvas.Rectangle(R.Left-MSZ, R.Bottom-MSZ, R.Left+MSZ, R.Bottom+MSZ);
    Exit; // skip code for old label types
  end;
  if (FSize.Left<>0) or (FSize.Right<>0) or (FSize.Top<>0) or (FSize.Bottom<>0)
    and (not (Plt.FPMInfo.Move2 and Plt.FPMInfo.InProgress)) then
  begin // draw block markers if block area initialized
    R:=FSize;
    if Plt.FPMInfo.InProgress and (not Plt.FPMInfo.Move2) then
    begin
      R.Left:=R.Left+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Right:=R.Right+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      R.Top:=R.Top+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
      R.Bottom:=R.Bottom+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
    if FLabelKind in [lkText, lkVText, lkLegend] then
    begin // correct frame size (QA0118012009)
      R.Bottom:=R.Bottom-1;
      R.Right:=R.Right-1;
    end;
    // frame
    Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
    R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
    // handles
    Canvas.Rectangle(R.Left-MSZ, R.Top-MSZ, R.Left+MSZ, R.Top+MSZ);
    Canvas.Rectangle(R.Right-MSZ, R.Top-MSZ, R.Right+MSZ, R.Top+MSZ);
    Canvas.Rectangle(R.Right-MSZ, R.Bottom-MSZ, R.Right+MSZ, R.Bottom+MSZ);
    Canvas.Rectangle(R.Left-MSZ, R.Bottom-MSZ, R.Left+MSZ, R.Bottom+MSZ);
  end;
  if FLabelKind in [lkArrow, lkLine] then // additional markers and line
    if Plt.FPMInfo.InProgress then
    begin
      if Plt.FPMInfo.Move1 then // process start move mode
      begin
        iX:=Scale(lcX1);
        iY:=Scale(lcY1);
        iX:=iX+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
        iY:=iY+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
        Canvas.MoveTo(iX, iY);
        iX:=Scale(lcX2);
        iY:=Scale(lcY2);
        Canvas.LineTo(iX, iY);
        Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
      end;
      if Plt.FPMInfo.Move2 then // process end move mode
      begin
        iX:=Scale(lcX1);
        iY:=Scale(lcY1);
        Canvas.MoveTo(iX, iY);
        iX:=Scale(lcX2);
        iY:=Scale(lcY2);
        iX:=iX+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
        iY:=iY+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
        Canvas.LineTo(iX, iY);
        Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
      end;
      if (not Plt.FPMInfo.Move1) and (not Plt.FPMInfo.Move2) then
      begin // move label as a whole
        iX:=Scale(lcX1);
        iY:=Scale(lcY1);
        iX:=iX+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
        iY:=iY+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
        Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
        Canvas.MoveTo(iX, iY);
        iX:=Scale(lcX2);
        iY:=Scale(lcY2);
        iX:=iX+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
        iY:=iY+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
        Canvas.LineTo(iX, iY);
        Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
      end;
    end else
    begin
      iX:=Scale(lcX1);
      iY:=Scale(lcY1);
      Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
      Canvas.MoveTo(iX, iY);
      iX:=Scale(lcX2);
      iY:=Scale(lcY2);
      Canvas.LineTo(iX, iY);
      Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ);
    end;
  if FLabelKind=lkBalloon then
  begin
    iX:=Scale(lcX2); // position of pointer
    iY:=Scale(lcY2);
    if (Plt.FPMInfo.InProgress) and (Plt.FPMInfo.Move2) then
    begin
      iX:=iX+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      iY:=iY+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
    Canvas.Rectangle(iX-MSZ, iY-MSZ, iX+MSZ, iY+MSZ); // point marker
    Canvas.MoveTo(iX, iY); // stem preview
    Canvas.LineTo((R.Left+R.Right) div 2, (R.Top+R.Bottom) div 2);
  end;
end;

function TPlotLabel.Scale(LabelCoordinate: TLabelCoordinates; Size: integer=0; Canvas: TCanvas=nil): Integer;
var
  Plt: TPlot;
  _LeftMargin, _RightMargin, _TopMargin, _BottomMargin: Integer;
  
  function FixPrint(V: extended): Integer;
  begin
    if Canvas=Printer.Canvas
    then Result:=Round(V*Canvas.Font.PixelsPerInch/PPI)
    else Result:=Round(V);
  end;
  
begin
  Result:=0; // default
  Assert(Assigned(Collection));
  Plt:=(Collection as TLabels).Plot;
  Assert(Assigned(Plt));
  // Correct margins for printing - only in fixed margins mode!
  // because in automargins mode calculated margins already corrected.
  if (not Plt.AutoMargins) and (Canvas=Printer.Canvas) then
  begin
    _LeftMargin:=Round(Plt.LeftMargin*Canvas.Font.PixelsPerInch/PPI);
    _RightMargin:=Round(Plt.RightMargin*Canvas.Font.PixelsPerInch/PPI);
    _TopMargin:=Round(Plt.TopMargin*Canvas.Font.PixelsPerInch/PPI);
    _BottomMargin:=Round(Plt.BottomMargin*Canvas.Font.PixelsPerInch/PPI);
  end else
  begin
    _LeftMargin:=Plt.LeftMargin;
    _RightMargin:=Plt.RightMargin;
    _TopMargin:=Plt.TopMargin;
    _BottomMargin:=Plt.BottomMargin;
  end;
  // initialize default Size
  if Size=0 then
    if LabelCoordinate in [lcX1, lcX2]
    then Size:=Plt.Width
    else  Size:=Plt.Height;
  // calculate Scale result
  case LabelCoordinate of
    lcX1:
    case FPinModeX1 of
      lpmFrame:
        Result:=_LeftMargin+Round((Size-_LeftMargin-_RightMargin)*FX1);
      lpmAxis:
        Result:=FixPrint(FX1)+_LeftMargin;
      lpmAxis2:
        Result:=FixPrint(FX1)+(Size-_RightMargin);
      lpmScale, lpmScale2:
        Result:=_XAxis(LabelCoordinate).Scale(FX1);
    end;
    lcX2:
    case FPinModeX2 of
      lpmFrame:
        Result:=_LeftMargin+Round((Size-_LeftMargin-_RightMargin)*FX2);
      lpmAxis:
        Result:=FixPrint(FX2)+_LeftMargin;
      lpmAxis2:
        Result:=FixPrint(FX2)+(Size-_RightMargin);
      lpmScale, lpmScale2:
        Result:=_XAxis(LabelCoordinate).Scale(FX2);
    end;
    lcY1:
    case FPinModeY1 of
      lpmFrame:
        Result:=_TopMargin+Round((Size-_TopMargin-_BottomMargin)*FY1);
      lpmAxis:
        Result:=FixPrint(FY1)+(Size-_BottomMargin);
      lpmAxis2:
        Result:=FixPrint(FY1)+_TopMargin;
      lpmScale, lpmScale2:
        Result:=_YAxis(LabelCoordinate).Scale(FY1);
    end;
    lcY2:
    case FPinModeY2 of
      lpmFrame:
        Result:=_TopMargin+Round((Size-_TopMargin-_BottomMargin)*FY2);
      lpmAxis:
        Result:=FixPrint(FY2)+(Size-_BottomMargin);
      lpmAxis2:
        Result:=FixPrint(FY2)+_TopMargin;
      lpmScale, lpmScale2:
        Result:=_YAxis(LabelCoordinate).Scale(FY2);
    end;
  end;
end;

procedure TPlotLabel.SetAlignment(Value: TLabelAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetLabelKind(Value: TLabelKind);
begin
  if FLabelKind <> Value then
  begin
    FLabelKind := Value;
    // todo: other label types
    case Value of
      lkText, lkVText, lkLegend:
      begin
        {F}PinModeX1:=lpmFrame;
        {F}PinModeY1:=lpmFrame;
        {F}PinModeX2:=lpmFrame;
        {F}PinModeY2:=lpmFrame;
      end;
      lkArrow, lkLine, lkRectangle, lkEllipse:
      begin
        {F}PinModeX1:=lpmScale;
        {F}PinModeY1:=lpmScale;
        {F}PinModeX2:=lpmScale;
        {F}PinModeY2:=lpmScale;
      end;
      lkBalloon:
      begin
        {F}PinModeX1:=lpmFrame;
        {F}PinModeY1:=lpmFrame;
        {F}PinModeX2:=lpmScale;
        {F}PinModeY2:=lpmScale;
      end;
    end;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetPinModeX1(Value: TLabelPinMode);
var
  X: Integer;
  B: Boolean;
  Plt: TPlot;
begin
  if FPinModeX1 <> Value then
  begin
    Plt:=nil;
    if Assigned(Collection)
    then Plt:=(Collection as TLabels).Plot;
    B:=Assigned(Plt);
    if B
    then B:=not (csReading in (Collection as TLabels).Plot.ComponentState);
    if B
    then X:=Scale(lcX1) // remember integer coordinates
    else X:=0; // avoid compiler warning
    FPinModeX1 := Value;
    if B then // restore integer coordinates as in Scale()
    case Value of
      lpmFrame:
        if (Plt.Width-Plt.LeftMargin-Plt.RightMargin)<>0
        then X1:=(X-Plt.LeftMargin)/(Plt.Width-Plt.LeftMargin-Plt.RightMargin);
      lpmAxis:
        X1:=X-Plt.LeftMargin;
      lpmAxis2:
        X1:=X-(Plt.Width-Plt.RightMargin);
      lpmScale, lpmScale2:
        X1:=_XAxis(lcX1).Scale(X);
    end;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetPinModeX2(Value: TLabelPinMode);
var
  X: Integer;
  B: Boolean;
  Plt: TPlot;
begin
  if FPinModeX2 <> Value then
  begin
    Plt:=nil;
    if Assigned(Collection)
    then Plt:=(Collection as TLabels).Plot;
    B:=Assigned(Plt);
    if B
    then B:=not (csReading in (Collection as TLabels).Plot.ComponentState);
    if B
    then X:=Scale(lcX2) // remember integer coordinates
    else X:=0; // avoid compiler warning
    FPinModeX2 := Value;
    if B then // restore integer coordinates as in Scale()
    case Value of
      lpmFrame:
        if (Plt.Width-Plt.LeftMargin-Plt.RightMargin)<>0
        then X2:=(X-Plt.LeftMargin)/(Plt.Width-Plt.LeftMargin-Plt.RightMargin);
      lpmAxis:
        X2:=X-Plt.LeftMargin;
      lpmAxis2:
        X2:=X-(Plt.Width-Plt.RightMargin);
      lpmScale, lpmScale2:
        X2:=_XAxis(lcX2).Scale(X);
    end;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetPinModeY1(Value: TLabelPinMode);
var
  Y: Integer;
  B: Boolean;
  Plt: TPlot;
begin
  if FPinModeY1 <> Value then
  begin
    Plt:=nil;
    if Assigned(Collection)
    then Plt:=(Collection as TLabels).Plot;
    B:=Assigned(Plt);
    if B
    then B:=not (csReading in (Collection as TLabels).Plot.ComponentState);
    if B
    then Y:=Scale(lcY1) // remember integer coordinates
    else Y:=0; // avoid compiler warning
    FPinModeY1 := Value;
    if B then // restore integer coordinates as in Scale()
    case Value of
      lpmFrame:
        if (Plt.Height-Plt.TopMargin-Plt.BottomMargin)<>0
        then Y1:=(Y-Plt.TopMargin)/(Plt.Height-Plt.TopMargin-Plt.BottomMargin);
      lpmAxis:
        Y1:=Y-(Plt.Height-Plt.BottomMargin);
      lpmAxis2:
        Y1:=Y-Plt.TopMargin;
      lpmScale, lpmScale2:
        Y1:=_YAxis(lcY1).Scale(Y);
    end;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetPinModeY2(Value: TLabelPinMode);
var
  Y: Integer;
  B: Boolean;
  Plt: TPlot;
begin
  if FPinModeY2 <> Value then
  begin
    Plt:=nil;
    if Assigned(Collection)
    then Plt:=(Collection as TLabels).Plot;
    B:=Assigned(Plt);
    if B
    then B:=not (csReading in (Collection as TLabels).Plot.ComponentState);
    if B
    then Y:=Scale(lcY2) // remember integer coordinates
    else Y:=0; // avoid compiler warning
    FPinModeY2 := Value;
    if B then // restore integer coordinates as in Scale()
    case Value of
      lpmFrame:
        if (Plt.Height-Plt.TopMargin-Plt.BottomMargin)<>0
        then Y2:=(Y-Plt.TopMargin)/(Plt.Height-Plt.TopMargin-Plt.BottomMargin);
      lpmAxis:
        Y2:=Y-(Plt.Height-Plt.BottomMargin);
      lpmAxis2:
        Y2:=Y-Plt.TopMargin;
      lpmScale, lpmScale2:
        Y2:=_YAxis(lcY2).Scale(Y);
    end;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetX1(Value: Extended);
begin
  if FX1 <> Value then
  begin
    FX1 := Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetX2(Value: Extended);
begin
  if FX2 <> Value then
  begin
    FX2 := Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetY1(Value: Extended);
begin
  if FY1 <> Value then
  begin
    FY1 := Value;
    Changed(false);
  end;
end;

procedure TPlotLabel.SetY2(Value: Extended);
begin
  if FY2 <> Value then
  begin
    FY2 := Value;
    Changed(false);
  end;
end;

//: invoked from Plot.MouseUp 
procedure TPlotLabel._Move(Plt: TPlot);
  
  procedure MoveX1;
  begin
    case FPinModeX1 of
      lpmFrame:
        X1:=X1+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo)/
          (Plt.Width-Plt.LeftMargin-Plt.RightMargin);
      lpmAxis, lpmAxis2:
        X1:=X1+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      lpmScale, lpmScale2:
        { done: in this mode, marker and final label positions are not the same!
        beware of logticks problem when using dX, dY!!!
        X1:=_XAxis.Scale(Plt.FPMInfo.LEX); <- with parasitic displacement;
        This version fails in log mode: X1:=X1+dX;}
        X1:=_XAxis(lcX1).Scale(Scale(lcX1)+Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
    end;
  end;
  
  procedure MoveX2;
  begin
    case FPinModeX2 of
      lpmFrame:
        X2:=X2+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo)/
          (Plt.Width-Plt.LeftMargin-Plt.RightMargin);
      lpmAxis, lpmAxis2:
        X2:=X2+(Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
      lpmScale, lpmScale2:
        //X2:=_XAxis.Scale(Plt.FPMInfo.LEX);
        X2:=_XAxis(lcX2).Scale(Scale(lcX2)+Plt.FPMInfo.LEX-Plt.FPMInfo.LEXo);
    end;
  end;
  
  procedure MoveY1;
  begin
    case FPinModeY1 of
      lpmFrame:
        Y1:=Y1+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo)/
          (Plt.Height-Plt.TopMargin-Plt.BottomMargin);
      lpmAxis, lpmAxis2:
        Y1:=Y1+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
      lpmScale, lpmScale2:
        //Y1:=_YAxis.Scale(Plt.FPMInfo.LEY);
        Y1:=_YAxis(lcY1).Scale(Scale(lcY1)+Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
  end;
  
  procedure MoveY2;
  begin
    case FPinModeY2 of
      lpmFrame:
        Y2:=Y2+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo)/
          (Plt.Height-Plt.TopMargin-Plt.BottomMargin);
      lpmAxis, lpmAxis2:
        Y2:=Y2+(Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
      lpmScale, lpmScale2:
        //Y2:=_YAxis.Scale(Plt.FPMInfo.LEY);
        Y2:=_YAxis(lcY2).Scale(Scale(lcY2)+Plt.FPMInfo.LEY-Plt.FPMInfo.LEYo);
    end;
  end;
  
  procedure Move1;
  begin
    MoveX1;
    MoveY1;
  end;
  
  procedure Move2;
  begin
    MoveX2;
    MoveY2;
  end;
  
begin
  Assert(Assigned(Plt));
  if (Plt.FPMInfo.LEX=Plt.FPMInfo.LEXo) and (Plt.FPMInfo.LEY=Plt.FPMInfo.LEYo)
  then Exit; // nothing to do - mouse not moved
  case FLabelKind of
    lkText, lkVText, lkLegend:
      Move1;
    lkArrow, lkLine:
    begin
      if Plt.FPMInfo.Move1
      then Move1; // only arrow start moved
      if Plt.FPMInfo.Move2
      then Move2; // only arrow end moved
      if (not Plt.FPMInfo.Move1) and (not Plt.FPMInfo.Move2) then
      begin // moved both ends (line)
        Move1;
        Move2;
      end;
    end;
    lkBalloon:
      if Plt.FPMInfo.Move2
      then Move2 // point marker moved
      else Move1; // block moved
    lkRectangle, lkEllipse:
    begin
      if Plt.FPMInfo.Move1
      then Move1;
      if Plt.FPMInfo.Move2
      then Move2;
      if Plt.FPMInfo.Move3 then
      begin
        MoveX1;
        MoveY2;
      end;
      if Plt.FPMInfo.Move4 then
      begin
        MoveX2;
        MoveY1;
      end;
      if (not Plt.FPMInfo.Move1) and (not Plt.FPMInfo.Move2) and
        (not Plt.FPMInfo.Move3) and (not Plt.FPMInfo.Move4) then
      begin // moved whole rectangle
        Move1;
        Move2;
      end;
    end;
    // todo: other label types
  end;
end;

{---------- Class: TPlot ----------}
procedure TPlot.Changed(Sender: TObject);
begin
  FPMInfo.InProgress:=false; // abort any mouse operation
  Invalidate;
end;

constructor TPlot.Create(AOwner: TComponent);
begin
  inherited;
  FZoomStack:=TObjectStack.Create;
  FPen:=TPen.Create;
  FPen.OnChange:=Changed;
  FBrush:=TBrush.Create;
  FBrush.OnChange:=Changed;
  FXAxis:=TAxis.Create(Self);
  FYAxis:=TAxis.Create(Self);
  FXAxis2:=TAxis.Create(Self);
  FYAxis2:=TAxis.Create(Self);
  FSeries:=TSeries.Create(Self);
  FLabels:=TLabels.Create(Self);
  FAutoMargins:=true;
  FTransparent:=true;
  FMouseMode:=DefMouseMode;
  FSerieIndex:=-1;
  FBorderStyle:=DefBorderStyle;
  FPMInfo.InProgress:=false;
  FClipPoints:=true;
  FAreaSeries:=false;
  FAutoScaleLabels:=false;
end;

destructor TPlot.Destroy;
begin
  if Assigned(FPen)
  then FPen.Free;
  if Assigned(FBrush)
  then FBrush.Free;
  if Assigned(FXAxis)
  then FXAxis.Free;
  if Assigned(FYAxis)
  then FYAxis.Free;
  if Assigned(FXAxis2)
  then FXAxis2.Free;
  if Assigned(FYAxis2)
  then FYAxis2.Free;
  if Assigned(FSeries)
  then FSeries.Free;
  if Assigned(Flabels)
  then Flabels.Free;
  if Assigned(FZoomStack)
  then FZoomStack.Free;
  inherited;
end;

function TPlot.GetMargin(Index: Integer): Integer;
begin
  Result:=FMargins[Index];
end;

function TPlot.GetPoint(Point, Serie: integer; var X, Y: TReal): Boolean;
var
  D: TData;
  S: TSerie;
  X1, Y1: Extended;
begin
  Result:=false; // fail by default!
  try
    S:=FSeries[Serie];
    if S.Empty or (not InRange(Point, 0, S.FLastLine-S.FFirstLine))
    then Exit;
    if not S.FIsFunction then
    begin
      D:=TData(S.FContainer.Items[S.FFirstLine+Point]);
      if Assigned(D) and (D is TRealData) then
      begin
        X:=(D as TRealData).GetItem(S.FXColumn);
        Y:=(D as TRealData).GetItem(S.FYColumn);
        Result:=true;
      end;
    end;
    if Assigned(FGetPoint) then
  {$ifdef doublefloat}
    begin
      X1:=X; Y1:=Y;
      Result:=FGetPoint(Self, Point, Serie, X1, Y1);
      X:=X1; Y:=Y1;
    end;
  {$else}
    Result:=FGetPoint(Self, Point, Serie, X, Y);
  {$endif}
  except
    on E: Exception do
    begin
      if Assigned(FOnError)
      then FOnError(Self, Format(strPlotPointError,
      [E.ClassName, Point, Serie, E.Message]));
      Result:=false;
    end;
  end;
end;

function TPlot.GetThisSerie: TSerie;
begin
  if (SerieIndex>=0) and (SerieIndex<Series.Count)
  then Result:=Series[SerieIndex]
  else Result:=nil;
end;

procedure TPlot.Paint;
begin
  PaintCanvas(Canvas, Width, Height);
end;

procedure TPlot.PaintCanvas(Canvas: TCanvas; Width, Height: integer);
var
  MiniXAxes, MiniYAxes: Boolean;
  
  procedure CalculateMargins;
  var
    W, H: array[1..4] of integer;
    A, B, DefMargin: integer;
  const
    _DefMargin=3;
  begin // CalculateMargins
    // correct DefMargin
    DefMargin:=_DefMargin;
    if Canvas=Printer.Canvas
    then DefMargin:=Round(DefMargin*Canvas.Font.PixelsPerInch/PPI);
    // calculate label size for all axes
    YAxis.CalculateLabelsWidth(Canvas, W[1], H[1]); // left
    YAxis2.CalculateLabelsWidth(Canvas, W[2], H[2]); // right
    XAxis2.CalculateLabelsWidth(Canvas, W[3], H[3]); // top
    XAxis.CalculateLabelsWidth(Canvas, W[4], H[4]); // bottom
    // calculate bottom margin
    if XAxis.FVisible then
    begin
      if XAxis.FLabelVisible
      then
        if XAxis.FInnerTicks
        then FMargins[4]:=DefMargin+H[4]
        else FMargins[4]:=DefMargin+H[4]+Round((1+XAxis.TickLength)*H[4])
      else FMargins[4]:=DefMargin;
      if XAxis.FTitle<>''
      then FMargins[4]:=FMargins[4]+H[4];
      if (FMargins[4]=DefMargin) and
        ((YAxis.Visible and YAxis.LabelVisible) or
        (YAxis2.Visible and YAxis2.LabelVisible))
      then FMargins[4]:=Math.Max(FMargins[4], Math.Max(H[1], H[2]) div 2);
    end else FMargins[4]:=Math.Max(H[1], H[2]) div 2;
    // calculate top margin
    if XAxis2.FVisible then
    begin
      if XAxis2.FLabelVisible
      then
        if XAxis2.FInnerTicks
        then FMargins[3]:=DefMargin+H[3]
        else FMargins[3]:=DefMargin+H[3]+Round((1+XAxis2.TickLength)*H[3])
      else FMargins[3]:=DefMargin;
      if XAxis2.FTitle<>''
      then FMargins[3]:=FMargins[3]+H[3];
      if (FMargins[3]=DefMargin) and
        ((YAxis.Visible and YAxis.LabelVisible) or
        (YAxis2.Visible and YAxis2.LabelVisible))
      then FMargins[3]:=Math.Max(FMargins[3], Math.Max(H[1], H[2]) div 2);
    end else FMargins[3]:=Math.Max(H[1], H[2]) div 2;
    // calculate left margin
    if YAxis.FVisible then
    begin
      if XAxis.FVisible and XAxis.FLabelVisible
      then A:=W[4] div 2
      else A:=DefMargin;
      if XAxis2.FVisible and XAxis2.FLabelVisible
      then B:=W[3] div 2
      else B:=DefMargin;
      A:=Math.Max(A, B);
      if YAxis.FLabelVisible then
        if YAxis.FInnerTicks
        then B:=DefMargin+W[1]
        else B:=DefMargin+W[1]+Round((1+YAxis.TickLength)*H[1])
      else B:=DefMargin;
      FMargins[1]:=Math.Max(A, B);
      if YAxis.FTitle<>'' then
        if YAxis.FLabelVisible
        then FMargins[1]:=FMargins[1]+H[1]
        else FMargins[1]:=Math.Max(FMargins[1], DefMargin+H[1]);
    end else FMargins[1]:=Math.Max(W[3], W[4]) div 2;
    // calculate right margin
    if YAxis2.FVisible then
    begin
      if XAxis.FVisible and XAxis.FLabelVisible
      then A:=W[4] div 2
      else A:=DefMargin;
      if XAxis2.FVisible and XAxis2.FLabelVisible
      then B:=W[3] div 2
      else B:=DefMargin;
      A:=Math.Max(A, B);
      if YAxis2.FLabelVisible then
        if YAxis2.FInnerTicks
        then B:=DefMargin+W[2]
        else B:=DefMargin+W[2]+Round((1+YAxis2.TickLength)*H[2])
      else B:=DefMargin;
      FMargins[2]:=Math.Max(A, B);
      if YAxis2.FTitle<>'' then
        if YAxis2.FLabelVisible
        then FMargins[2]:=FMargins[2]+H[2]
        else FMargins[2]:=Math.Max(FMargins[2], DefMargin+H[2]);
    end else FMargins[2]:=Math.Max(W[3], W[4]) div 2;
  end;
  
  procedure PaintBackground;
  var
    R: TRect;
  begin
    R:=GetClientRect;
    Canvas.Brush.Color:=Color;
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(R);
  end;
  
  procedure PaintBorder;
  var
    AP: array[1..5] of TPoint;
  begin
    AP[1]:=Point(0, 0);
    AP[2]:=Point(Width-1, 0);
    AP[3]:=Point(Width-1, Height-1);
    AP[4]:=Point(0, Height-1);
    AP[5]:=AP[1];
    Canvas.Pen.Width:=1;
    Canvas.Pen.Color:=clBlack;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Mode:=pmCopy;
    Canvas.Polyline(AP);
  end;
  
  procedure CalculateScale;
  type
    TAutoScaleInfo=record
      Scaled: boolean;
      Min: TReal;
      Max: TReal;
    end;
  
    procedure ProcessAutoScaleInfo(Min, Max: TReal; var ASI: TAutoScaleInfo);
    begin
      if ASI.Scaled then
      begin
        ASI.Min:=Math.Min(ASI.Min, Min);
        ASI.Max:=Math.Max(ASI.Max, Max);
      end else
      begin
        ASI.Min:=Min;
        ASI.Max:=Max;
        ASI.Scaled:=true;
      end;
    end;
  
    procedure UpdateAxis(const A: TAxis; ASI: TAutoScaleInfo);
    begin
      if ASI.Scaled then
      if A.FLogTicks then
      begin
        if ASI.Min>0 then A.FMin:=ASI.Min*(1-A.FMargins);
        if ASI.Max>0 then A.FMax:=ASI.Max*(1+A.FMargins);
      end else
      begin
        A.FMin:=ASI.Min-(ASI.Max-ASI.Min)*A.FMargins;
        A.FMax:=ASI.Max+(ASI.Max-ASI.Min)*A.FMargins;
      end;
    end;
  
  var
    I: integer;
    Min, Max: TRealPoint;
    X,Y,X2,Y2: TAutoScaleInfo;
  
    // note: unlike series, label may affect only one (x or y) coordinate!
    // warning: this procedure uses global variables from var block above.
    procedure ProcessLabel(L: TPlotLabel);
    begin
      // x coordinates
      if (L.PinModeX1=lpmScale) and XAxis.AutoScale then
      begin
        ProcessAutoScaleInfo(L.X1, L.X1, X);
        UpdateAxis(XAxis, X);
      end;
      if (L.PinModeX1=lpmScale2) and XAxis2.AutoScale then
      begin
        ProcessAutoScaleInfo(L.X1, L.X1, X2);
        UpdateAxis(XAxis2, X2);
      end;
      if (L.PinModeX2=lpmScale) and XAxis.AutoScale and
        (not (L.LabelKind in [lkText, lkLegend, lkVText])) then
      begin
        ProcessAutoScaleInfo(L.X2, L.X2, X);
        UpdateAxis(XAxis, X);
      end;
      if (L.PinModeX2=lpmScale2) and XAxis2.AutoScale and
        (not (L.LabelKind in [lkText, lkLegend, lkVText])) then
      begin
        ProcessAutoScaleInfo(L.X2, L.X2, X2);
        UpdateAxis(XAxis2, X2);
      end;
      // y coordinates
      if (L.PinModeY1=lpmScale) and YAxis.AutoScale then
      begin
        ProcessAutoScaleInfo(L.Y1, L.Y1, Y);
        UpdateAxis(YAxis, Y);
      end;
      if (L.PinModeY1=lpmScale2) and YAxis2.AutoScale then
      begin
        ProcessAutoScaleInfo(L.Y1, L.Y1, Y2);
        UpdateAxis(YAxis2, Y2);
      end;
      if (L.PinModeY2=lpmScale) and YAxis.AutoScale and
        (not (L.LabelKind in [lkText, lkLegend, lkVText])) then
      begin
        ProcessAutoScaleInfo(L.Y2, L.Y2, Y);
        UpdateAxis(YAxis, Y);
      end;
      if (L.PinModeY2=lpmScale2) and YAxis2.AutoScale and
        (not (L.LabelKind in [lkText, lkLegend, lkVText])) then
      begin
        ProcessAutoScaleInfo(L.Y2, L.Y2, Y2);
        UpdateAxis(YAxis2, Y2);
      end;
    end;
  
  begin
    X.Scaled:=false;
    X2.Scaled:=false;
    Y.Scaled:=false;
    Y2.Scaled:=false;
    for I:=0 to FSeries.Count-1 do
    if (FSeries[I]._XAxis.FAutoScale or FSeries[I]._YAxis.FAutoScale) {!!!}
      and FSeries[I].Visible and FSeries[I].Scale(Min, Max) then
    begin
      // process X
      if (FSeries[I]._XAxis=XAxis) and XAxis.FAutoScale
      then ProcessAutoScaleInfo(Min.X, Max.X, X);
      if (FSeries[I]._XAxis=XAxis2) and XAxis2.FAutoScale
      then ProcessAutoScaleInfo(Min.X, Max.X, X2);
      // process Y
      if (FSeries[I]._YAxis=YAxis) and YAxis.FAutoScale
      then ProcessAutoScaleInfo(Min.Y, Max.Y, Y);
      if (FSeries[I]._YAxis=YAxis2) and YAxis2.FAutoScale
      then ProcessAutoScaleInfo(Min.Y, Max.Y, Y2);
      // update axes if scale information available
      UpdateAxis(XAxis, X);
      UpdateAxis(XAxis2, X2);
      UpdateAxis(YAxis, Y);
      UpdateAxis(YAxis2, Y2);
    end;
    if FAutoScaleLabels then
    for I:=0 to FLabels.Count-1 do
    if FLabels[I].Visible
    then ProcessLabel(FLabels[I]);
  end;
  
  procedure PaintSeries;
  var
    I: integer;
  begin
    if FAreaSeries then
    for I:=0 to FSeries.Count-1 do
    begin
      FSeries[I].PaintArea(Canvas);
      FSeries[I].Paint(Canvas);
    end else
    begin
      for I:=0 to FSeries.Count-1 do // paint areas first!!!
      FSeries[I].PaintArea(Canvas);
      for I:=0 to FSeries.Count-1 do // then curves (above areas)
      FSeries[I].Paint(Canvas);
    end;
  end;
  
  procedure PaintLeaders;
  var
    I: integer;
  begin
    for I:=0 to FSeries.Count-1 do
    if FSeries[I].FIsRecording
    then FSeries[I].DrawLeader(Canvas);
  end;
  
  procedure PaintLabels;
  var
    I: integer;
  begin
    for I:=0 to FLabels.Count-1 do
    FLabels[I].Paint(Canvas, Width, Height);
  end;
  
  procedure PaintAxes;
  var
    XL, YL, X2L, Y2L: boolean;
  
    procedure doPaintAxes;
    var
      _LeftMargin, _RightMargin, _TopMargin, _BottomMargin: integer;
    begin
      // Correct margins for printing - only in fixed margins mode!
      // because in automargins mode calculated margins already corrected.
      if (not FAutoMargins) and (Canvas=Printer.Canvas) then
      begin
        _LeftMargin:=Round(LeftMargin*Canvas.Font.PixelsPerInch/PPI);
        _RightMargin:=Round(RightMargin*Canvas.Font.PixelsPerInch/PPI);
        _TopMargin:=Round(TopMargin*Canvas.Font.PixelsPerInch/PPI);
        _BottomMargin:=Round(BottomMargin*Canvas.Font.PixelsPerInch/PPI);
      end else
      begin
        _LeftMargin:=LeftMargin;
        _RightMargin:=RightMargin;
        _TopMargin:=TopMargin;
        _BottomMargin:=BottomMargin;
      end;
      // paint axes
      XAxis.Paint(Canvas, _LeftMargin, Height-_BottomMargin-1,
        Width-_LeftMargin-_RightMargin-1, Height-_BottomMargin-_TopMargin-1, axBottom);
      YAxis.Paint(Canvas, _LeftMargin, Height-_BottomMargin-1,
        Height-_BottomMargin-_TopMargin-1, Width-_LeftMargin-_RightMargin, axLeft);
      XAxis2.Paint(Canvas, _LeftMargin, _TopMargin,
        Width-_LeftMargin-_RightMargin-1, Height-_BottomMargin-_TopMargin-1, axTop);
      YAxis2.Paint(Canvas, Width-_RightMargin-1, Height-_BottomMargin-1,
        Height-_BottomMargin-_TopMargin-1, Width-_LeftMargin-_RightMargin, axRight);
    end;
  
  begin
    if FAutoMargins then
    begin
      CalculateMargins;
      if Canvas=Printer.Canvas then
      begin // label autohiding intentionally not implementeded for printouts
        MiniXAxes:=false;
        MiniYAxes:=false;
      end else
      begin
        MiniXAxes:=BottomMargin+TopMargin>Height div 2;
        MiniYAxes:=LeftMargin+RightMargin>Width div 2;
      end;
      XL:=XAxis.FLabelVisible;
      X2L:=XAxis2.FLabelVisible;
      YL:=YAxis.FLabelVisible;
      Y2L:=YAxis2.FLabelVisible;
      if MiniXAxes then
      begin
        XAxis.FLabelVisible:=false;
        XAxis2.FLabelVisible:=false;
      end;
      if MiniYAxes then
      begin
        YAxis.FLabelVisible:=false;
        YAxis2.FLabelVisible:=false;
      end;
      try
        if MiniXAxes or MiniYAxes
        then CalculateMargins; // once again!
        doPaintAxes;
      finally // use FLabelVisible fields - no side effect
        if MiniXAxes then
        begin
          XAxis.FLabelVisible:=XL;
          XAxis2.FLabelVisible:=X2L;
        end;
        if MiniYAxes then
        begin
          YAxis.FLabelVisible:=YL;
          YAxis2.FLabelVisible:=Y2L;
        end;
      end;
    end else
    begin
      MiniXAxes:=false;
      MiniYAxes:=false;
      doPaintAxes;
    end;
  end;
  
  {$ifdef PaintAxes2}
  procedure PaintAxes2; // uses global MiniX,YAxes set in PaintAxes()
  begin // as in doPaintAxes!
    if Canvas=Printer.Canvas  // WARNING: print support intentionally unsupported
    then Exit;
    if not MiniXAxes then
    begin
      if (not FClipPoints) or XAxis.FInnerTicks or XAxis.FShowGrid
      then XAxis.Paint(Canvas, LeftMargin, Height-BottomMargin-1,
        Width-LeftMargin-RightMargin-1, Height-BottomMargin-TopMargin-1, axBottom);
      if (not FClipPoints) or XAxis2.FInnerTicks or XAxis2.FShowGrid
      then XAxis2.Paint(Canvas, LeftMargin, TopMargin,
        Width-LeftMargin-RightMargin-1, Height-BottomMargin-TopMargin-1, axTop);
    end;
    if not MiniYAxes then
    begin
      if (not FClipPoints) or YAxis.FInnerTicks or YAxis.FShowGrid
      then YAxis.Paint(Canvas, LeftMargin, Height-BottomMargin-1,
        Height-BottomMargin-TopMargin-1, Width-LeftMargin-RightMargin, axLeft);
      if (not FClipPoints) or YAxis2.FInnerTicks or YAxis2.FShowGrid
      then YAxis2.Paint(Canvas, Width-RightMargin-1, Height-BottomMargin-1,
        Height-BottomMargin-TopMargin-1, Width-LeftMargin-RightMargin, axRight);
    end;
  end;
  {$endif}
  
begin
  // PaintCanvas potentially long!
  Screen.Cursor:=crHourGlass;
  ShowPlotHint(strPlotBusy);
  try
    // paint background
    if not (FTransparent or (Canvas=Printer.Canvas))
    then PaintBackground;
    // paint border (if any)
    {$ifdef PrintBorder}
    if (FBorderStyle=bsSingle)
    {$else}
    if (FBorderStyle=bsSingle) and (Canvas<>Printer.Canvas)
    {$endif}
    then PaintBorder;
    // calculate scale and margins (after scale!!!)
    CalculateScale;
    // paint axes
    PaintAxes;
    // paint series
    PaintSeries;
    {$ifdef PaintAxes2}
    // repaint axes if their elements are hidden by areas
    PaintAxes2;
    {$endif}
    // paint labels - above series and axes
    PaintLabels;
    // allow user painting
    Canvas.Font:=Font;
    Canvas.Pen:=Pen;
    Canvas.Brush:=Brush;
    if Assigned(OnPaint)
    then OnPaint(Self);
    // draw leaders and Selection (or Margins) frame after all
    PaintLeaders;
    if SelectionVisible
    then DrawSelection(Canvas);
    if FMouseMode=pmMargins
    then DrawMargins(Canvas);
    if FMouseMode=pmLabelEdit
    then FLabels.DrawSelected(Canvas);
  finally
    // hide hint and restore cursor
    ShowPlotHint(' ');
    Screen.Cursor:=crDefault;
  end;
end;

procedure TPlot.SaveToMetaFile(WMF: TMetaFile);
var
  WMFC: TMetafileCanvas;
begin
  WMF.Width:=Width;
  WMF.Height:=Height;
  WMFC:=TMetafileCanvas.Create(WMF, 0);
  try
    PaintCanvas(WMFC, Width, Height);
  finally
    WMFC.Free;
  end;
end;

procedure TPlot.SetAutoMargins(const Value: Boolean);
begin
  if FAutoMargins<>Value then
  begin
    FAutoMargins:=Value;
    if Value then Changed(Self);
  end;
end;

procedure TPlot.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle<>Value then
  begin
    FBorderStyle:=Value;
    {todo : TPlot->borderStyle: future versions may use optimization - redraw only frame!}
    Changed(Self);
  end;
end;

procedure TPlot.SetBrush(const Value: TBrush);
begin
  if Assigned(FBrush) then
  FBrush.Assign(Value);
end;

procedure TPlot.SetMargin(Index: Integer; const Value: Integer);
begin
  if FMargins[Index]<>Value then
  begin
    FMargins[Index]:=Value;
    Changed(Self);
  end;
end;

procedure TPlot.SetMouseMode(const Value: TPlotMouseMode);
begin
  if FMouseMode=Value
  then Exit;
  if (FMouseMode=pmMargins) or (Value=pmMargins)
  then DrawMargins(Canvas); // hide or show
  if ((Value=pmTranslate) or (FMouseMode=pmTranslate)) and SelectionVisible then
  begin
    DrawSelection(Canvas);
    FMouseMode:=Value;
    DrawSelection(Canvas);
    Exit;
  end;
  FLabels.ItemIndex:=-1; // clear selected label when mode changed
  FMouseMode:=Value;
end;

procedure TPlot.SetPen(const Value: TPen);
begin
  if Assigned(FPen) then
  FPen.Assign(Value);
end;

procedure TPlot.SetSerieIndex(const Value: Integer);
var
  ix1, ix2, iy1, iy2: Integer;
begin
  if (Value<>FSerieIndex) and (Value>-2) and (Value<Series.Count) then
  begin
    if FSelectionVisible then DrawSelection(Canvas); // hide
    // get integer coordinates of selection
    if Assigned(ThisSerie) then
    begin
      ix1:=ThisSerie._XAxis.Scale(SelectionLeft);
      ix2:=ThisSerie._XAxis.Scale(SelectionRight);
      iy1:=ThisSerie._YAxis.Scale(SelectionBottom);
      iy2:=ThisSerie._YAxis.Scale(SelectionTop);
    end else
    begin
      ix1:=XAxis.Scale(XAxis.Min);
      ix2:=XAxis.Scale(XAxis.Max);
      iy1:=YAxis.Scale(YAxis.Min);
      iy2:=YAxis.Scale(YAxis.Max);
    end;
    FSerieIndex:=Value; // change ThisSerie
    // update real selection coordinates to prevent range checks
    if Assigned(ThisSerie) then
    begin
      FSelection[3]:=ThisSerie._XAxis.Scale(ix1); {SelectionLeft}
      FSelection[4]:=ThisSerie._XAxis.Scale(ix2); {SelectionRight}
      FSelection[2]:=ThisSerie._YAxis.Scale(iy1); {SelectionBottom}
      FSelection[1]:=ThisSerie._YAxis.Scale(iy2); {SelectionTop}
    end else
    begin
      FSelection[3]:=XAxis.Min;
      FSelection[4]:=XAxis.Max;
      FSelection[2]:=YAxis.Min;
      FSelection[1]:=YAxis.Max;
    end;
    if FSelectionVisible then DrawSelection(Canvas); // show
  end;
end;

procedure TPlot.SetSeries(const Value: TSeries);
begin
  if Assigned(FSeries) then
  FSeries.Assign(Value);
end;

procedure TPlot.SetTransparent(const Value: Boolean);
begin
  if FTransparent<>Value then
  begin
    FTransparent:=Value;
    Changed(Self);
  end;
end;

procedure TPlot.SetXAxis(const Value: TAxis);
begin
  if Assigned(FXAxis) then
  FXAxis.Assign(Value);
end;

procedure TPlot.SetYAxis(const Value: TAxis);
begin
  if Assigned(FYAxis) then
  FYAxis.Assign(Value);
end;

procedure TPlot.SetXAxis2(const Value: TAxis);
begin
  if Assigned(FXAxis2) then
  FXAxis2.Assign(Value);
end;

procedure TPlot.SetYAxis2(const Value: TAxis);
begin
  if Assigned(FYAxis2) then
  FYAxis2.Assign(Value);
end;

procedure TPlot.ShowPlotHint(Value: string);
begin
  if Assigned(FOnHint)
  then FOnHint(Self, Value);
end;

function TPlot.GetSelection(Index: Integer): Extended;
begin
  Result:=FSelection[Index];
end;

procedure TPlot.SetSelection(Index: Integer; const Value: Extended);
begin
  if FSelection[Index]<>Value then
  begin
    // Changed(Self);
    if FSelectionVisible then DrawSelection(Canvas); // hide
    FSelection[Index]:=Value;
    if FSelectionVisible then DrawSelection(Canvas); // show
  end;
end;

procedure TPlot.SetSelectionVisible(const Value: Boolean);
begin
  if FSelectionVisible<>Value then
  begin
    FSelectionVisible:=Value;
    DrawSelection(Canvas); // not Changed(Self); !!!
  end;
end;

procedure TPlot.DrawSelection(Canvas: TCanvas);
var
  A: array[1..5] of TPoint;
  Buf: array of TPoint;
  I, X1, Y1, X2, Y2: Integer;
  rX, rY: TReal;
begin
  // code below depends on ThisSerie
  if not Assigned(ThisSerie) then Exit;
  // check plot selection
  if ThisSerie._XAxis.LogTicks then
  begin
    if FSelection[3]<=0 // left
    then FSelection[3]:=1;
    if FSelection[4]<=0 // right
    then FSelection[4]:=1;
  end;
  if ThisSerie._YAxis.LogTicks then
  begin
    if FSelection[1]<=0 // top
    then FSelection[1]:=1;
    if FSelection[2]<=0 // bottom
    then FSelection[2]:=1;
  end;
  // draw selection frame
  A[1].X:=ThisSerie._XAxis.Scale(SelectionLeft, true);
  A[1].Y:=ThisSerie._YAxis.Scale(SelectionTop, true);
  A[3].X:=ThisSerie._XAxis.Scale(SelectionRight, true);
  A[3].Y:=ThisSerie._YAxis.Scale(SelectionBottom, true);
  CorRect(A[1].X, A[1].Y, A[3].X, A[3].Y);
  A[2]:=Point(A[3].X, A[1].Y);
  A[4]:=Point(A[1].X, A[3].Y);
  A[5]:=A[1];
  // setup drawing tools
  with Canvas.Pen do
  begin
    Width:=1;
    Style:=psDashDotDot;
    Mode:=pmXor;
    Color:=clGray;
  end;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clSilver;
  Canvas.Polyline(A);
  if FMouseMode<>pmTranslate
  then Exit;
  // draw Translation markers
  Canvas.Pen.Width:=2;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=clSilver;
  // draw markers
  X1:=ThisSerie._XAxis.Scale(SelectionLeft, true);
  X2:=ThisSerie._XAxis.Scale(SelectionRight, true);
  Y1:=ThisSerie._YAxis.Scale(SelectionTop, true);
  Y2:=ThisSerie._YAxis.Scale(SelectionBottom, true);
  Canvas.Rectangle(X1-MSZ, Y1-MSZ, X1+MSZ, Y1+MSZ);
  Canvas.Rectangle(X1-MSZ, ((Y1+Y2) div 2)-MSZ, X1+MSZ, ((Y1+Y2) div 2)+MSZ);
  Canvas.Rectangle(X1-MSZ, Y2-MSZ, X1+MSZ, Y2+MSZ);
  Canvas.Rectangle(((X1+X2) div 2)-MSZ, Y1-MSZ, ((X1+X2) div 2)+MSZ, Y1+MSZ);
  Canvas.Rectangle(((X1+X2) div 2)-MSZ, Y2-MSZ, ((X1+X2) div 2)+MSZ, Y2+MSZ);
  Canvas.Rectangle(X2-MSZ, Y1-MSZ, X2+MSZ, Y1+MSZ);
  Canvas.Rectangle(X2-MSZ, ((Y1+Y2) div 2)-MSZ, X2+MSZ, ((Y1+Y2) div 2)+MSZ);
  Canvas.Rectangle(X2-MSZ, Y2-MSZ, X2+MSZ, Y2+MSZ);
  // draw translation curve preview
  // code below depends on FPMInfo!!!
  if FPMInfo.InProgress and (FPMInfo.TransPointCount>0) then
  begin
    SetLength(Buf, FPMInfo.TransPointCount);
    for I:=0 to FPMInfo.TransPointCount-1 do
    begin
      rX:=Translate(FPMInfo.TransBuf[I].X, SelectionLeft, SelectionRight,
        FPMInfo.TransX1, FPMInfo.TransX2, FPMInfo.XAxis.LogTicks);
      rY:=Translate(FPMInfo.TransBuf[I].Y, SelectionBottom, SelectionTop,
        FPMInfo.TransY1, FPMInfo.TransY2, FPMInfo.YAxis.LogTicks);
      Buf[I].X:=FPMInfo.XAxis.Scale(rX, true);
      Buf[I].Y:=FPMInfo.YAxis.Scale(rY, true);
    end;
    Canvas.PolyLine(Buf);
  end;
end;

procedure TPlot.MouseDown(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer);
  
  procedure doPointClick;
  var
    rx, ry: TReal;
    Ser, Pnt, ix, iy: integer;
  begin
    Screen.Cursor:=crHourGlass;
    ShowPlotHint(strPlotBusy); // search for point potentially may be long
    try
      for Ser:=0 to FSeries.Count-1 do // scan series
      with FSeries[Ser] do
      begin
        if not FVisible then Continue; // can't click HIDDEN series!
        if (Empty{$ifdef DisableSelFunSer} or IsFunction{$endif}) then Continue;
        for Pnt:=0 to FLastLine-FFirstLine do // scan points
        begin
          if not ( GetPoint(Pnt, Ser, rx, ry) and
            (_XAxis.InRange(rx) and _YAxis.InRange(ry)) or (not FClipPoints) )
          then Continue;
          ix:=_XAxis.Scale(rx, true);
          iy:=_YAxis.Scale(ry, true);
          if (abs(x-ix)>FPointSize div 2) or (abs(y-iy)>FPointSize div 2)
          then Continue; // click too far from point
          if Assigned(FOnPointClick) then // POINT FOUND!
            if not FOnPointClick(Self, Pnt+FFirstLine, Ser)
            then Continue; // cancel delete or edit point!
          {$ifndef DisableSelFunSer}
          if IsFunction and (FMouseMode<>pmPointClick) then Continue;
          {$endif}
          // delete clicked point
          if FMouseMode=pmPointDelete then
          begin
            DeletePoint(Pnt+FSeries[Ser].FirstLine, FSeries[Ser].Container);
            FSeries[Ser].Container.Modified:=true;
            Exit;
          end;{PointDelete}
          if MouseMode=pmPointEdit then
          begin
            if (FXExpression<>'') or (FYExpression<>'')
              or (_XAxis.FExpression<>'') or (_YAxis.FExpression<>'') then
            begin
              Screen.Cursor:=crDefault;
              ShowPlotHint(' ');
              MessageDlg(strPlotEditError, mtError, [mbCancel], 0);
              Screen.Cursor:=crHourGlass;
              ShowPlotHint(strPlotBusy);
              Break; // goto next serie!
            end;
            FPMInfo.InProgress:=true; // note: axes NOT saved!!!
            // save exact point coordinates, not X,Y!
            FPMInfo.EditX:=ix; // save exact coordinates, not X,Y!
            FPMInfo.EditY:=iy;
            FPMInfo.EditSer:=Ser;
            FPMInfo.EditPnt:=Pnt+FFirstLine; // use to change point!
            // save coordinates of previous point if any
            if (Pnt>0) and GetPoint(Pnt-1, Ser, rx, ry) then
            begin
              FPMInfo.EditX1:=_XAxis.Scale(rx, true);
              FPMInfo.EditY1:=_YAxis.Scale(ry, true);
            end else
            begin
              FPMInfo.EditX1:=0;
              FPMInfo.EditY1:=0;
            end;
            // save coordinates of next point if any
            if (Pnt<FLastLine-FFirstLine) and GetPoint(Pnt+1, Ser, rx, ry) then
            begin
              FPMInfo.EditX2:=_XAxis.Scale(rx, true);
              FPMInfo.EditY2:=_YAxis.Scale(ry, true);
            end else
            begin
              FPMInfo.EditX2:=0;
              FPMInfo.EditY2:=0;
            end;
            DrawEdit(iX, iY); // show "rubber thread"
          end;{PointEdit}
          Break;
        end;{points cycle}
        if FPMInfo.InProgress then Break; // prevent capture of >1 point
      end;{series cycle}
    finally
      Screen.Cursor:=crDefault;
      ShowPlotHint(' ');
    end;
  end;
  
  procedure doTranslate;
  var
    rX, rY, LR2, TB2: TReal;
    I: integer;
  begin
    // checks
    if ThisSerie.Empty or ThisSerie.IsFunction then
    begin
  {    if Assigned(FOnError)
      then FOnError(Self, strPlotTransError);
      Beep;}
      MessageDlg(strPlotTransError, mtError, [mbCancel], 0);
      Exit;
    end;
    with ThisSerie do
    if (FXExpression<>'') or (FYExpression<>'')
      or (_XAxis.FExpression<>'') or (_YAxis.FExpression<>'') then
    begin
      MessageDlg(strPlotEditError, mtError, [mbCancel], 0);
      Exit;
    end;
    FPMInfo.XAxis:=ThisSerie._XAxis;
    FPMInfo.YAxis:=ThisSerie._YAxis;
    if (SelectionLeft=SelectionRight) or (SelectionTop=SelectionBottom) then
    begin
      MessageDlg(strPlotTransSelError, mtError, [mbCancel], 0);
      Exit;
    end;
    rX:=FPMInfo.XAxis.Scale(X);
    rY:=FPMInfo.YAxis.Scale(Y);
    if FPMInfo.XAxis.LogTicks // calculate integer halfsize
    then LR2:=exp((ln(SelectionLeft)+ln(SelectionRight))/2)
    else LR2:=(SelectionLeft+SelectionRight)/2;
    if FPMInfo.YAxis.LogTicks
    then TB2:=exp((ln(SelectionTop)+ln(SelectionBottom))/2)
    else TB2:=(SelectionTop+SelectionBottom)/2;
    // calculate FPMInfo.TransMode
    if BelongMarker(SelectionLeft,SelectionTop,X,Y)
    then FPMInfo.TransMode:=ptmTL else
    if BelongMarker(SelectionRight,SelectionTop,X,Y)
    then FPMInfo.TransMode:=ptmTR else
    if BelongMarker(LR2,SelectionTop,X,Y)
    then FPMInfo.TransMode:=ptmT else
    if BelongMarker(LR2,SelectionBottom,X,Y)
    then FPMInfo.TransMode:=ptmB else
    if BelongMarker(SelectionLeft,SelectionBottom,X,Y)
    then FPMInfo.TransMode:=ptmBL else
    if BelongMarker(SelectionRight,SelectionBottom,X,Y)
    then FPMInfo.TransMode:=ptmBR else
    if BelongMarker(SelectionLeft,TB2,X,Y)
    then FPMInfo.TransMode:=ptmL else
    if BelongMarker(SelectionRight,TB2,X,Y)
    then FPMInfo.TransMode:=ptmR else
    if (rX>SelectionLeft) and (rX<SelectionRight) and (rY<SelectionTop) and
    (rY>SelectionBottom) then FPMInfo.TransMode:=ptmMove else Exit;
    // remember selection and mouse position
    FPMInfo.InProgress:=true;
    FPMInfo.TransIX:=rX;
    FPMInfo.TransIY:=rY;
    FPMInfo.TransX1:=SelectionLeft;
    FPMInfo.TransX2:=SelectionRight;
    FPMInfo.TransY1:=SelectionBottom;
    FPMInfo.TransY2:=SelectionTop;
    // fill translation preview buffer
    Screen.Cursor:=crHourGlass;
    ShowPlotHint(strPlotBusy);
    with ThisSerie do
    try
      SetLength(FPMInfo.TransBuf, LastLine-FirstLine+1);
      FPMInfo.TransPointCount:=0;
      for I:=FirstLine to LastLine do
      begin
        if LastLine<>FirstLine
        then Container.ShowProgress(Round((I-FirstLine)/(LastLine-FirstLine)*100));
        GetPoint(I-FirstLine, Index, rX, rY);
        if (FPMInfo.TransX1<=rX) and (FPMInfo.TransX2>=rX) and
        (FPMInfo.TransY1<=rY) and (FPMInfo.TransY2>=rY) then
        begin
          with FPMInfo.TransBuf[FPMInfo.TransPointCount] do
          begin
            X:=rX;
            Y:=rY;
          end;
          Inc(FPMInfo.TransPointCount);
        end;
      end;
    finally
      Screen.Cursor:=crDefault;
      ShowPlotHint(' ');
    end;
  end;
  
  procedure doLabelEdit;
  var
    I, N: integer;
  begin
    N:=-1; // find index of clicked label
    for I:=FLabels.Count-1 downto 0 do // reverse order!
    if FLabels[I].Visible and FLabels[I].Belong(X, Y) then
    begin
      N:=I;
      Break;
    end;
    if N<>FLabels.ItemIndex then
    begin
      FLabels.ItemIndex:=N; // select another label
      // Exit; if uncommented, user needs to click label twice - for selection,
      // then once more to start moving
    end;
    if N<0 // no selected labels
    then Exit;
    // start moving label
    FPMInfo.InProgress:=true;
    FPMInfo.LEX:=X;
    FPMInfo.LEXo:=X;
    FPMInfo.LEY:=Y;
    FPMInfo.LEYo:=Y;
    // this code is to be changed (see global QA0121012009)
    if FLabels[N].XAxis=BottomAxis
    then FPMInfo.XAxis:=FXAxis
    else FPMInfo.XAxis:=FXAxis2;
    if FLabels[N].YAxis=LeftAxis
    then FPMInfo.YAxis:=FYAxis
    else FPMInfo.YAxis:=FYAxis2;
    /////
    FPMInfo.Move1:=false;
    FPMInfo.Move2:=false;
    FPMInfo.Move3:=false;
    FPMInfo.Move4:=false;
    if FLabels[N].FLabelKind in [lkArrow, lkLine, lkRectangle, lkEllipse] then
    begin // check whether we should move individual arrow ends
      FPMInfo.Move1:=BelongMarker(FLabels[N].Scale(lcX1), FLabels[N].Scale(lcY1), X, Y);
      FPMInfo.Move2:=BelongMarker(FLabels[N].Scale(lcX2), FLabels[N].Scale(lcY2), X, Y);
      if FPMInfo.Move1 and FPMInfo.Move2 // prevent zero-length arrow
      then FPMInfo.Move1:=false;
    end;
    if FLabels[N].FLabelKind in [lkRectangle, lkEllipse] then
    begin // process additional handles...
      if FPMInfo.Move1 or FPMInfo.Move2
      then Exit;
      FPMInfo.Move3:=BelongMarker(FLabels[N].Scale(lcX1), FLabels[N].Scale(lcY2), X, Y);
      if FPMInfo.Move3
      then Exit;
      FPMInfo.Move4:=BelongMarker(FLabels[N].Scale(lcX2), FLabels[N].Scale(lcY1), X, Y);
    end;
    if FLabels[N].FLabelKind=lkBalloon then
    begin // check whether we should move balloon point marker
      FPMInfo.Move2:=BelongMarker(FLabels[N].Scale(lcX2), FLabels[N].Scale(lcY2), X, Y);
    end;
    // todo: other label types
  end;
  
begin
  inherited;
  if not (ssLeft in Shift) or (ssDouble in Shift)
  then Exit; // only single left mouse button clicks!
  // pmPointClick, pmPointEdit, pmPointDelete
  if FMouseMode in [pmPointClick, pmPointEdit, pmPointDelete]
  then doPointClick;
  // pmLabelEdit
  if (FMouseMode=pmLabelEdit) and (Shift=[ssLeft])
  then doLabelEdit;
  if not Assigned(ThisSerie) then Exit; // check modes that require ThisSerie
  // pmAutoZoom, pmZoom, pmUnZoom
  if ((ssShift in Shift) and (FMouseMode=pmAutoZoom)) or
     (FMouseMode=pmZoom) or (FMouseMode=pmUnZoom) then
  begin
    FPMInfo.InProgress:=true;
    FPMInfo.XAxis:=ThisSerie._XAxis;
    FPMInfo.YAxis:=ThisSerie._YAxis;
    FPMInfo.ZoomX:=X;
    FPMInfo.ZoomY:=Y;
    FPMInfo.ZoomXo:=X;
    FPMInfo.ZoomYo:=Y;
    // "alternative" modes - zoom only one axis
    FPMInfo.ZoomXAxis:=(FMouseMode in [pmZoom, pmUnZoom]) and (ssCtrl in Shift);
    FPMInfo.ZoomYAxis:=(FMouseMode in [pmZoom, pmUnZoom]) and (ssShift in Shift);
    if FPMInfo.ZoomXAxis then
    begin
      FPMInfo.ZoomYo:=TopMargin;
      FPMInfo.ZoomY:=Height-BottomMargin;
    end;
    if FPMInfo.ZoomYAxis then
    begin
      FPMInfo.ZoomXo:=LeftMargin;
      FPMInfo.ZoomX:=Width-RightMargin;
    end;
  end;
  // pmSelect
  if FMouseMode=pmSelect then
  begin
    SelectionVisible:=false; // hide previous selection if any
    FPMInfo.InProgress:=true;
    FPMInfo.XAxis:=ThisSerie._XAxis;
    FPMInfo.YAxis:=ThisSerie._YAxis;
    FPMInfo.SelX:=X;
    FPMInfo.SelY:=Y;
    FPMInfo.SelXo:=X;
    FPMInfo.SelYo:=Y;
  end;
  // pmRuler
  if FMouseMode=pmRuler then
  begin
    FPMInfo.InProgress:=true;
    FPMInfo.XAxis:=ThisSerie._XAxis;
    FPMInfo.YAxis:=ThisSerie._YAxis;
    FPMInfo.RulerX:=X;
    FPMInfo.RulerY:=Y;
    FPMInfo.RulerFi:=0;
    DrawRuler(X, Y);
  end;
  // pmTranslate
  if (FMouseMode=pmTranslate) and FSelectionVisible
  then doTranslate;
  // pmMargins
  if FMouseMode=pmMargins then
  begin
    FPMInfo.InProgress:=true;
    FPMInfo.MarginXo:=X; // save coordinates (only for title)
    FPMInfo.MarginYo:=Y; // note: we could check axis and remember only one value!
    // title rectangles
    if PtInRect(FXAxis.FTitleRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmXT else
    if PtInRect(FYAxis.FTitleRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmYT else
    if PtInRect(FXAxis2.FTitleRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmX2T else
    if PtInRect(FYAxis2.FTitleRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmY2T else
    // THEN handles
    if BelongMarker(LeftMargin, TopMargin, X, Y)
    then FPMInfo.MarginMode:=pmmTL else
    if BelongMarker(Width-RightMargin, TopMargin, X, Y)
    then FPMInfo.MarginMode:=pmmTR else
    if BelongMarker(LeftMargin, Height-BottomMargin, X, Y)
    then FPMInfo.MarginMode:=pmmBL else
    if BelongMarker(Width-RightMargin, Height-BottomMargin, X, Y)
    then FPMInfo.MarginMode:=pmmBR else
    // and after all - labels rectangles
    if PtInRect(FXAxis.FLabelsRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmXL else
    if PtInRect(FYAxis.FLabelsRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmYL else
    if PtInRect(FXAxis2.FLabelsRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmX2L else
    if PtInRect(FYAxis2.FLabelsRect, Point(X,Y))
    then FPMInfo.MarginMode:=pmmY2L else FPMInfo.InProgress:=false;
  end;
  {todo: MouseDown for other mouse modes}
end;

procedure TPlot.MouseMove(Shift: TShiftState; X,Y: Integer);
  
  {notice: called even if MouseMode=pmNone!}
  procedure ShowCoordinates;
  var
    Sx, Sy: string[128];
  begin
    if (FMouseMode=pmLabelEdit) and (FPMInfo.InProgress) then
    begin // this code is to be changed (see global QA0121012009)
      if (FPMInfo.XAxis=nil) or (FPMInfo.YAxis=nil)
      then Exit;
      with FPMInfo.XAxis do
      Sx:=FloatToStrF(Scale(X), LabelType, LabelWidth, LabelDecimals);
      with FPMInfo.YAxis do
      Sy:=FloatToStrF(Scale(Y), LabelType, LabelWidth, LabelDecimals);
      ShowPlotHint(Format('X: %s Y: %s', [Sx, Sy]));
      Exit;
    end;
    if not Assigned(ThisSerie) then Exit;
    with ThisSerie._XAxis do
    Sx:=FloatToStrF(Scale(X), LabelType, LabelWidth, LabelDecimals);
    with ThisSerie._YAxis do
    Sy:=FloatToStrF(Scale(Y), LabelType, LabelWidth, LabelDecimals);
    ShowPlotHint(Format('X: %s Y: %s', [Sx, Sy]));
  end;
  
  procedure doRuler;
  var
    a, rX, Ry, MinX, MinY, MaxX, MaxY: TReal;
    S: string[128];
  begin
    if not FPMInfo.InProgress then Exit;
    Assert(Assigned(FPMInfo.XAxis));
    Assert(Assigned(FPMInfo.YAxis));
    DrawRuler(FPMInfo.RulerX, FPMInfo.RulerY); // hide
    FPMInfo.RulerY:=Y;
    if (ssAlt in Shift)
    then FPMInfo.RulerFi:=(FPMInfo.RulerX-X)/Width*Pi
    else FPMInfo.RulerX:=X;
    DrawRuler(FPMInfo.RulerX, FPMInfo.RulerY); // show
    with FPMInfo do // make ruler hint
    if XAxis.LogTicks or YAxis.LogTicks then
    begin
      S:='%7.4g*X+%-7.4g';
      rX:=XAxis.Scale(X);
      rY:=YAxis.Scale(Y);
      MinX:=XAxis.Min;
      MaxX:=XAxis.Max;
      MinY:=YAxis.Min;
      MaxY:=YAxis.Max;
      if YAxis.LogTicks then
      begin
        Assert(MinY>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        Assert(MaxY>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        Assert(rY>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        MinY:=ln(MinY)/ln(10);
        MaxY:=ln(MaxY)/ln(10);
        rY:=ln(rY)/ln(10);
        S:='10^(%7.4g*X+%-7.4g)';
      end;
      if XAxis.LogTicks then
      begin
        Assert(MinX>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        Assert(MaxX>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        Assert(rX>0, '{22234499-40BD-4FE4-B57C-D41861C78C54}');
        MinX:=ln(MinX)/ln(10);
        MaxX:=ln(MaxX)/ln(10);
        rX:=ln(rX)/ln(10);
        if YAxis.LogTicks
        then S:='10^(%7.4g*lg(X)+%-7.4g)'
        else S:='%7.4g*lg(X)+%-7.4g';
      end;
      a:=sin(RulerFi)/cos(RulerFi)*(MaxY-MinY)/
        (Height-TopMargin-BottomMargin)/
        (MaxX-MinX)*(Width-LeftMargin-RightMargin);
      S:=Format(S, [a, rY-rX*a]);
    end else
    begin
      a:=sin(RulerFi)/cos(RulerFi)*(YAxis.Max-YAxis.Min)/
        (Height-TopMargin-BottomMargin)/
        (XAxis.Max-XAxis.Min)*(Width-LeftMargin-RightMargin);
      rX:=XAxis.Scale(X);
      rY:=YAxis.Scale(Y);
      S:=Format('%7.4g*X+%-7.4g',[a,rY-rX*a]);
    end;
    if Pos('+-', S)<>0
    then System.Delete(S, Pos('+-', S), 1); // delete extra "+"
    ShowPlotHint(S);
  end;
  
  {called even if not FPMInfo.InProgress!}
  procedure CheckCursor;
  var
    L, T, R, B: integer;
  begin
    if (FMouseMode=pmTranslate) and FSelectionVisible and Assigned(ThisSerie) then
    begin
      L:=ThisSerie._XAxis.Scale(SelectionLeft, true);
      R:=ThisSerie._XAxis.Scale(SelectionRight, true);
      T:=ThisSerie._YAxis.Scale(SelectionTop, true);
      B:=ThisSerie._YAxis.Scale(SelectionBottom, true);
      if BelongMarker(L, T, X, Y)
      then Screen.Cursor:=crSizeNWSE else
      if BelongMarker(R, T, X, Y)
      then Screen.Cursor:=crSizeNESW else
      if BelongMarker((L+R) div 2, T ,X, Y) or
        BelongMarker((L+R) div 2, B, X, Y)
      then Screen.Cursor:=crSizeNS else
      if BelongMarker(L, B, X, Y)
      then Screen.Cursor:=crSizeNESW else
      if BelongMarker(R, B, X, Y)
      then Screen.Cursor:=crSizeNWSE else
      if BelongMarker(L, (T+B) div 2, X, Y) or
        BelongMarker(R, (T+B) div 2, X, Y)
      then Screen.Cursor:=crSizeWE else
      if (X>L) and (X<R) and
        (Y>T) and (Y<B)
      then Screen.Cursor:=crSize else Screen.Cursor:=crDefault;
    end;
    if FMouseMode=pmMargins then
    begin
      if PtInRect(FXAxis.FTitleRect, Point(X,Y)) // as in MouseDown
      then Screen.Cursor:=crSizeNS else
      if PtInRect(FYAxis.FTitleRect, Point(X,Y))
      then Screen.Cursor:=crSizeWE else
      if PtInRect(FXAxis2.FTitleRect, Point(X,Y))
      then Screen.Cursor:=crSizeNS else
      if PtInRect(FYAxis2.FTitleRect, Point(X,Y))
      then Screen.Cursor:=crSizeWE else
      if BelongMarker(LeftMargin, TopMargin, X, Y)
      then Screen.Cursor:=crSizeNWSE else
      if BelongMarker(Width-RightMargin, TopMargin, X, Y)
      then Screen.Cursor:=crSizeNESW else
      if BelongMarker(LeftMargin, Height-BottomMargin, X, Y)
      then Screen.Cursor:=crSizeNESW else
      if BelongMarker(Width-RightMargin, Height-BottomMargin, X, Y)
      then Screen.Cursor:=crSizeNWSE else
      if PtInRect(FXAxis.FLabelsRect, Point(X,Y))
      then Screen.Cursor:=crSizeNS else
      if PtInRect(FYAxis.FLabelsRect, Point(X,Y))
      then Screen.Cursor:=crSizeWE else
      if PtInRect(FXAxis2.FLabelsRect, Point(X,Y))
      then Screen.Cursor:=crSizeNS else
      if PtInRect(FYAxis2.FLabelsRect, Point(X,Y))
      then Screen.Cursor:=crSizeWE else Screen.Cursor:=crDefault;
    end;
    if FMouseMode=pmLabelEdit then
      if Assigned(FLabels.SelectedLabel) and FLabels.SelectedLabel.Belong(X, Y)
      then Screen.Cursor:=crSize
      else Screen.Cursor:=crDefault;
  end;
  
  procedure doTranslate;
  var
    rX, rY, rXo, rYo: TReal;
  begin
    DrawSelection(Canvas); // hide previous selection
    rXo:=FPMInfo.TransIX;
    rYo:=FPMInfo.TransIY;
    rX:=FPMInfo.XAxis.Scale(X);
    rY:=FPMInfo.YAxis.Scale(Y);
    // show hints
    with FPMInfo do
    if TransMode=ptmMove
    then ShowPlotHint('dX : '+FloatToStrF(SelectionLeft-TransX1,
      XAxis.LabelType, XAxis.LabelWidth, XAxis.LabelDecimals)+
      ' dY : '+FloatToStrF(SelectionBottom-TransY1,
      YAxis.LabelType, YAxis.LabelWidth, YAxis.LabelDecimals))
    else ShowPlotHint('*X : '+
      FloatToStrF(Abs((SelectionRight-SelectionLeft)/(TransX2-TransX1)),
      ffFixed, 7, 4)+' *Y : '+FloatToStrF(Abs((SelectionTop-SelectionBottom)
      /(TransY2-TransY1)), ffFixed, 7, 4));
    // update selection
    case FPMInfo.TransMode of
      ptmMove:
        begin
          if FPMInfo.YAxis.LogTicks then
          begin
            FSelection[1]:=exp(ln(FSelection[1])+(ln(rY)-ln(rYo))); // top
            FSelection[2]:=exp(ln(FSelection[2])+(ln(rY)-ln(rYo))); // bottom
          end else
          begin
            FSelection[1]:=FSelection[1]+(rY-rYo); // top
            FSelection[2]:=FSelection[2]+(rY-rYo); // bottom
          end;
          if FPMInfo.XAxis.LogTicks then
          begin
            FSelection[3]:=exp(ln(FSelection[3])+(ln(rX)-ln(rXo))); // left
            FSelection[4]:=exp(ln(FSelection[4])+(ln(rX)-ln(rXo))); // right
          end else
          begin
            FSelection[3]:=FSelection[3]+(rX-rXo); // left
            FSelection[4]:=FSelection[4]+(rX-rXo); // right
          end;
        end;
      ptmT:
        if FPMInfo.YAxis.LogTicks
        then FSelection[1]:=exp(ln(FSelection[1])+(ln(rY)-ln(rYo)))
        else FSelection[1]:=FSelection[1]+(rY-rYo);
      ptmB:
        if FPMInfo.YAxis.LogTicks
        then FSelection[2]:=exp(ln(FSelection[2])+(ln(rY)-ln(rYo)))
        else FSelection[2]:=FSelection[2]+(rY-rYo);
      ptmL:
        if FPMInfo.XAxis.LogTicks
        then FSelection[3]:=exp(ln(FSelection[3])+(ln(rX)-ln(rXo)))
        else FSelection[3]:=FSelection[3]+(rX-rXo);
      ptmR:
        if FPMInfo.XAxis.LogTicks
        then FSelection[4]:=exp(ln(FSelection[4])+(ln(rX)-ln(rXo)))
        else FSelection[4]:=FSelection[4]+(rX-rXo);
      ptmTL:
        begin
          if FPMInfo.YAxis.LogTicks
          then FSelection[1]:=exp(ln(FSelection[1])+(ln(rY)-ln(rYo)))
          else FSelection[1]:=FSelection[1]+(rY-rYo);
          if FPMInfo.XAxis.LogTicks
          then FSelection[3]:=exp(ln(FSelection[3])+(ln(rX)-ln(rXo)))
          else FSelection[3]:=FSelection[3]+(rX-rXo);
        end;
      ptmTR:
        begin
          if FPMInfo.YAxis.LogTicks
          then FSelection[1]:=exp(ln(FSelection[1])+(ln(rY)-ln(rYo)))
          else FSelection[1]:=FSelection[1]+(rY-rYo);
          if FPMInfo.XAxis.LogTicks
          then FSelection[4]:=exp(ln(FSelection[4])+(ln(rX)-ln(rXo)))
          else FSelection[4]:=FSelection[4]+(rX-rXo);
        end;
      ptmBL:
        begin
          if FPMInfo.YAxis.LogTicks
          then FSelection[2]:=exp(ln(FSelection[2])+(ln(rY)-ln(rYo)))
          else FSelection[2]:=FSelection[2]+(rY-rYo);
          if FPMInfo.XAxis.LogTicks
          then FSelection[3]:=exp(ln(FSelection[3])+(ln(rX)-ln(rXo)))
          else FSelection[3]:=FSelection[3]+(rX-rXo);
        end;
      ptmBR:
        begin
          if FPMInfo.YAxis.LogTicks
          then FSelection[2]:=exp(ln(FSelection[2])+(ln(rY)-ln(rYo)))
          else FSelection[2]:=FSelection[2]+(rY-rYo);
          if FPMInfo.XAxis.LogTicks
          then FSelection[4]:=exp(ln(FSelection[4])+(ln(rX)-ln(rXo)))
          else FSelection[4]:=FSelection[4]+(rX-rXo);
        end;
    end;{case}
    DrawSelection(Canvas); // show selection (and preview!) at new position
    FPMInfo.TransIX:=rX; // remember position to use in next MouseMove()
    FPMInfo.TransIY:=rY;
    CheckCursor;
  end;
  
  procedure doMargins;
  begin
    DrawMargins(Canvas); // hide
    case FPMInfo.MarginMode of
      // handles
      pmmTL:
        begin
          FMargins[1]:=X; // left
          FMargins[3]:=Y; // top
          ShowPlotHint('X: '+IntToStr(FMargins[1])+' Y: '+IntToStr(FMargins[3]));
        end;
      pmmTR:
        begin
          FMargins[2]:=Width-X; // right
          FMargins[3]:=Y; // top
          ShowPlotHint('X: '+IntToStr(FMargins[2])+' Y: '+IntToStr(FMargins[3]));
        end;
      pmmBL:
        begin
          FMargins[1]:=X; // left
          FMargins[4]:=Height-Y; // bottom
          ShowPlotHint('X: '+IntToStr(FMargins[1])+' Y: '+IntToStr(FMargins[4]));
        end;
      pmmBR:
        begin
          FMargins[2]:=Width-X; // right
          FMargins[4]:=Height-Y; // bottom
          ShowPlotHint('X: '+IntToStr(FMargins[2])+' Y: '+IntToStr(FMargins[4]));
        end;
      // axis titles
      pmmXT: // X axis
        begin
          FXAxis.FTitleMargin:=FXAxis.FTitleMargin-(FPMInfo.MarginYo-Y);
          ShowPlotHint('Y: '+IntToStr(FXAxis.FTitleMargin));
          FPMInfo.MarginYo:=Y;
        end;
      pmmX2T: // X2 axis
        begin
          FXAxis2.FTitleMargin:=FXAxis2.FTitleMargin+(FPMInfo.MarginYo-Y);
          ShowPlotHint('Y: '+IntToStr(FXAxis2.FTitleMargin));
          FPMInfo.MarginYo:=Y;
        end;
      pmmYT: // Y axis
        begin
          FYAxis.FTitleMargin:=FYAxis.FTitleMargin+(FPMInfo.MarginXo-X);
          ShowPlotHint('X: '+IntToStr(FYAxis.FTitleMargin));
          FPMInfo.MarginXo:=X;
        end;
      pmmY2T: // Y2 axis
        begin
          FYAxis2.FTitleMargin:=FYAxis2.FTitleMargin-(FPMInfo.MarginXo-X);
          ShowPlotHint('X: '+IntToStr(FYAxis2.FTitleMargin));
          FPMInfo.MarginXo:=X;
        end;
      // axis labels
      pmmXL: // X axis
        begin
          FXAxis.FLabelMargin:=FXAxis.FLabelMargin-(FPMInfo.MarginYo-Y);
          ShowPlotHint('Y: '+IntToStr(FXAxis.FLabelMargin));
          FPMInfo.MarginYo:=Y;
        end;
      pmmX2L: // X2 axis
        begin
          FXAxis2.FLabelMargin:=FXAxis2.FLabelMargin+(FPMInfo.MarginYo-Y);
          ShowPlotHint('Y: '+IntToStr(FXAxis2.FLabelMargin));
          FPMInfo.MarginYo:=Y;
        end;
      pmmYL: // Y axis
        begin
          FYAxis.FLabelMargin:=FYAxis.FLabelMargin+(FPMInfo.MarginXo-X);
          ShowPlotHint('X: '+IntToStr(FYAxis.FLabelMargin));
          FPMInfo.MarginXo:=X;
        end;
      pmmY2L: // Y2 axis
        begin
          FYAxis2.FLabelMargin:=FYAxis2.FLabelMargin-(FPMInfo.MarginXo-X);
          ShowPlotHint('X: '+IntToStr(FYAxis2.FLabelMargin));
          FPMInfo.MarginXo:=X;
        end;
    end;
    DrawMargins(Canvas); // show
    CheckCursor;
  end;
  
begin
  inherited;
  // check whether coordinates are in control range
  if not (InRange(X, 0, Width-1) and InRange(Y, 0, Height-1))
  then Exit;
  // check whether mouse tool mode in progress
  if not FPMInfo.InProgress then
  begin
    ShowCoordinates;
    CheckCursor;
    Exit;
  end;
  // pmAutoZoom, pmZoom, pmUnZoom
  if FMouseMode in [pmAutoZoom, pmZoom, pmUnZoom] then
  with FPMInfo do
  begin
    ShowCoordinates;
    DrawDotRect(Canvas, ZoomXo, ZoomYo, ZoomX, ZoomY);
    if not ZoomXAxis then ZoomY:=Y;
    if not ZoomYAxis then ZoomX:=X;
    DrawDotRect(Canvas, ZoomXo, ZoomYo, ZoomX, ZoomY);
  end;
  // pmSelect
  if FMouseMode=pmSelect then
  with FPMInfo do
  begin
    ShowCoordinates;
    DrawDotRect(Canvas, SelXo, SelYo, SelX, SelY);
    SelY:=Y;
    SelX:=X;
    DrawDotRect(Canvas, SelXo, SelYo, SelX, SelY);
  end;
  // pmRuler
  if FMouseMode=pmRuler
  then doRuler;
  // pmPointEdit
  if FMouseMode=pmPointEdit then
  begin
    DrawEdit(X, Y);
    ShowCoordinates;
  end;
  // pmTranslate
  if FMouseMode=pmTranslate
  then doTranslate;
  // pmMargins
  if FMouseMode=pmMargins
  then doMargins;
  // pmLabelEdit
  if FMouseMode=pmLabelEdit then
  begin
    FLabels.DrawSelected(Canvas); // hide
    FPMInfo.LEX:=X;
    FPMInfo.LEY:=Y;
    FLabels.DrawSelected(Canvas); // show
    CheckCursor;
    ShowCoordinates;
  end;
  {todo: MouseMove for other mouse modes}
end;

procedure TPlot.MouseUp(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer);
  
  {todo: TPlot->doZoom: implement abort zoom operation with ESC?}
  procedure doZoom;
  var
    rx1, rx2, ry1, ry2, r2x1, r2x2, r2y1, r2y2: TReal;
    UZ: TUndoZoomCoordinates;
    Both: boolean;
  begin
    Both:=not (ssCtrl in Shift); // rescale both axes by default!
    with FPMInfo do
    begin
      // calculate zoom rectangle (X,Y NOT used!)
      CorRect(ZoomXo, ZoomYo, ZoomX, ZoomY);
      if ((ZoomX-ZoomXo<3)) or ((ZoomY-ZoomYo<3)) then Exit;
      if Both then
      begin // X,YAxis not used!
        rx1:=FXAxis.Scale(ZoomXo);
        rx2:=FXAxis.Scale(ZoomX);
        ry1:=FYAxis.Scale(ZoomYo);
        ry2:=FYAxis.Scale(ZoomY);
        FXAxis.AutoScale:=false;
        FYAxis.AutoScale:=false;
        r2x1:=FXAxis2.Scale(ZoomXo);
        r2x2:=FXAxis2.Scale(ZoomX);
        r2y1:=FYAxis2.Scale(ZoomYo);
        r2y2:=FYAxis2.Scale(ZoomY);
        FXAxis2.AutoScale:=false;
        FYAxis2.AutoScale:=false;
      end else
      begin
        rx1:=XAxis.Scale(ZoomXo);
        rx2:=XAxis.Scale(ZoomX);
        ry1:=YAxis.Scale(ZoomYo);
        ry2:=YAxis.Scale(ZoomY);
        r2x1:=0; // avoid compiler warning
        r2x2:=0;
        r2y1:=0;
        r2y2:=0;
        XAxis.AutoScale:=false;
        YAxis.AutoScale:=false;
      end;
      // save coordinates for Undo Zoom
      UZ:=TUndoZoomCoordinates.Create;
      if Both then
      begin
        UZ.OldX1:=FXAxis.Min;
        UZ.OldX2:=FXAxis.Max;
        UZ.OldY1:=FYAxis.Min;
        UZ.OldY2:=FYAxis.Max;
        UZ.Old2X1:=FXAxis2.Min;
        UZ.Old2X2:=FXAxis2.Max;
        UZ.Old2Y1:=FYAxis2.Min;
        UZ.Old2Y2:=FYAxis2.Max;
        UZ.XAxis:=nil;
        UZ.YAxis:=nil;
      end else
      begin
        UZ.OldX1:=XAxis.Min;
        UZ.OldX2:=XAxis.Max;
        UZ.OldY1:=YAxis.Min;
        UZ.OldY2:=YAxis.Max;
        UZ.XAxis:=XAxis;
        UZ.YAxis:=YAxis;
      end;
      FZoomStack.Push(UZ);
      // set new axes ranges
      if ((ssAlt in Shift) and (FMouseMode=pmAutoZoom))
         or (FMouseMode=pmUnZoom) then
      begin
        if Both then // WARNING: with >FPMInfo<!!! FAxis<=>plot, Axis<=>FPMinfo
        begin // UNzoom
          if FXAxis.LogTicks then // note: in log mode, we use /3 correction factor
          begin
            FXAxis.Min:=exp(ln(FXAxis.Min)-
              (ln(FXAxis.Max)-ln(FXAxis.Min))*(ln(rx1)-ln(FXAxis.Min))/3/(ln(rx2)-ln(rx1)));
            FXAxis.Max:=exp(ln(FXAxis.Max)+
              (ln(FXAxis.Max)-ln(FXAxis.Min))*(ln(FXAxis.Max)-ln(rx2))/3/(ln(rx2)-ln(rx1)));
          end else
          begin
            FXAxis.Min:=FXAxis.Min-(FXAxis.Max-FXAxis.Min)*(rx1-FXAxis.Min)/(rx2-rx1);
            FXAxis.Max:=FXAxis.Max+(FXAxis.Max-FXAxis.Min)*(FXAxis.Max-rx2)/(rx2-rx1);
          end;
          if FYAxis.LogTicks then
          begin
            FYAxis.Min:=exp(ln(FYAxis.Min)-
              (ln(FYAxis.Max)-ln(YAxis.Min))*(ln(ry2)-ln(FYAxis.Min))/3/(ln(ry1)-ln(ry2)));
            FYAxis.Max:=exp(ln(FYAxis.Max)+
              (ln(FYAxis.Max)-ln(YAxis.Min))*(ln(FYAxis.Max)-ln(ry1))/3/(ln(ry1)-ln(ry2)));
          end else
          begin
            FYAxis.Min:=FYAxis.Min-(FYAxis.Max-YAxis.Min)*(ry2-FYAxis.Min)/(ry1-ry2);
            FYAxis.Max:=FYAxis.Max+(FYAxis.Max-YAxis.Min)*(FYAxis.Max-ry1)/(ry1-ry2);
          end;
          if FXAxis2.LogTicks then
          begin
            FXAxis2.Min:=exp(ln(FXAxis2.Min)-
              (ln(FXAxis2.Max)-ln(FXAxis2.Min))*(ln(r2x1)-ln(FXAxis2.Min))/3/(ln(r2x2)-ln(r2x1)));
            FXAxis2.Max:=exp(ln(FXAxis2.Max)+
              (ln(FXAxis2.Max)-ln(FXAxis2.Min))*(ln(FXAxis2.Max)-ln(r2x2))/3/(ln(r2x2)-ln(r2x1)));
          end else
          begin
            FXAxis2.Min:=FXAxis2.Min-(FXAxis2.Max-FXAxis2.Min)*(r2x1-FXAxis2.Min)/(r2x2-r2x1);
            FXAxis2.Max:=FXAxis2.Max+(FXAxis2.Max-FXAxis2.Min)*(FXAxis2.Max-r2x2)/(r2x2-r2x1);
          end;
          if FYAxis2.LogTicks then
          begin
            FYAxis2.Min:=exp(ln(FYAxis2.Min)-
              (ln(FYAxis2.Max)-ln(YAxis2.Min))*(ln(r2y2)-ln(FYAxis2.Min))/3/(ln(r2y1)-ln(r2y2)));
            FYAxis2.Max:=exp(ln(FYAxis2.Max)+
              (ln(FYAxis2.Max)-ln(YAxis2.Min))*(ln(FYAxis2.Max)-ln(r2y1))/3/(ln(r2y1)-ln(r2y2)));
          end else
          begin
            FYAxis2.Min:=FYAxis2.Min-(FYAxis2.Max-YAxis2.Min)*(r2y2-FYAxis2.Min)/(r2y1-r2y2);
            FYAxis2.Max:=FYAxis2.Max+(FYAxis2.Max-YAxis2.Min)*(FYAxis2.Max-r2y1)/(r2y1-r2y2);
          end;
        end else
        begin
          if XAxis.LogTicks then
          begin
            XAxis.Min:=exp(ln(XAxis.Min)-
              (ln(XAxis.Max)-ln(XAxis.Min))*(ln(rx1)-ln(XAxis.Min))/3/(ln(rx2)-ln(rx1)));
            XAxis.Max:=exp(ln(XAxis.Max)+
              (ln(XAxis.Max)-ln(XAxis.Min))*(ln(XAxis.Max)-ln(rx2))/3/(ln(rx2)-ln(rx1)));
          end else
          begin
            XAxis.Min:=XAxis.Min-(XAxis.Max-XAxis.Min)*(rx1-XAxis.Min)/(rx2-rx1);
            XAxis.Max:=XAxis.Max+(XAxis.Max-XAxis.Min)*(XAxis.Max-rx2)/(rx2-rx1);
          end;
          if YAxis.LogTicks then
          begin
            YAxis.Min:=exp(ln(YAxis.Min)-
              (ln(YAxis.Max)-ln(YAxis.Min))*(ln(ry2)-ln(YAxis.Min))/3/(ln(ry1)-ln(ry2)));
            YAxis.Max:=exp(ln(YAxis.Max)+
              (ln(YAxis.Max)-ln(YAxis.Min))*(ln(YAxis.Max)-ln(ry1))/3/(ln(ry1)-ln(ry2)));
          end else
          begin
            YAxis.Min:=YAxis.Min-(YAxis.Max-YAxis.Min)*(ry2-YAxis.Min)/(ry1-ry2);
            YAxis.Max:=YAxis.Max+(YAxis.Max-YAxis.Min)*(YAxis.Max-ry1)/(ry1-ry2);
          end;
        end;
      end else
      begin // zoom
        if Both then
        begin
          FXAxis.Min:=rx1;
          FXAxis.Max:=rx2;
          FYAxis.Min:=ry2;
          FYAxis.Max:=ry1;
          FXAxis2.Min:=r2x1;
          FXAxis2.Max:=r2x2;
          FYAxis2.Min:=r2y2;
          FYAxis2.Max:=r2y1;
        end else
        begin
          XAxis.Min:=rx1;
          XAxis.Max:=rx2;
          YAxis.Min:=ry2;
          YAxis.Max:=ry1;
        end;
      end;
    end;
  end;
  
  procedure doSelect;
  begin
    with FPMInfo do
    begin
      DrawDotRect(Canvas, SelXo, SelYo, SelX, SelY);
      CorRect(SelXo, SelYo, SelX, SelY);
      if ((SelX-SelXo<3)) or ((SelY-SelYo<3)) then Exit;
      FSelection[3]:=XAxis.Scale(SelXo); // left
      FSelection[4]:=XAxis.Scale(SelX); // right
      FSelection[1]:=YAxis.Scale(SelYo); // top
      FSelection[2]:=YAxis.Scale(SelY); // bottom
    end;
    // since we hide selection in MouseDown, show it again!
    SelectionVisible:=true;
    if Assigned(FOnSelectionChange) // invoke handler AFTER all
    then FOnSelectionChange(Self);
  end;
  
  procedure doPointEdit;
  var
    rX, rY: TReal;
    D: TData;
  begin
    Invalidate; // erase lost "rubber thread" and old moved point
    rX:=FSeries[FPMInfo.EditSer]._XAxis.Scale(X);
    rY:=FSeries[FPMInfo.EditSer]._YAxis.Scale(Y);
    D:=FSeries[FPMInfo.EditSer].Container.Items[FPMInfo.EditPnt];
    if D is TRealData then
    begin
      (D as TRealData).RData[FSeries[FPMInfo.EditSer].FXColumn]:=rX;
      (D as TRealData).RData[FSeries[FPMInfo.EditSer].FYColumn]:=rY;
      FSeries[FPMInfo.EditSer].Container.Modified:=true;
    end
    else MessageDlg(strPlotDataError, mtError, [mbCancel], 0);
  end;
  
  procedure doTranslate;
  var
    a, rX, rY: TReal;
    M: boolean;
    I: integer;
    D: TData;
  begin
    // order selection
    if SelectionLeft{3}>SelectionRight{4} then
    begin
      a:=SelectionLeft; FSelection[3]:=SelectionRight; FSelection[4]:=a;
    end;
    if SelectionBottom{2}>SelectionTop{1} then
    begin
      a:=SelectionBottom; FSelection[2]:=SelectionTop; FSelection[1]:=a;
    end;
    // fire event
    if Assigned(FOnSelectionChange)
    then FOnSelectionChange(Self);
    // move points
    M:=false;
    with ThisSerie, FPMInfo do
    try
      Screen.Cursor:=crHourGlass;
      ShowPlotHint(strPlotBusy);
      for I:=FirstLine to LastLine do
      begin
        if LastLine<>FirstLine
        then Container.ShowProgress(Round((I-FirstLine)/(LastLine-FirstLine)*100));
        GetPoint(I-FirstLine, Index, rX, rY);
        if (TransX1<=rX) and (TransX2>=rX) and (TransY1<=rY) and (TransY2>=rY)
        then begin
          rX:=Translate(rX, SelectionLeft, SelectionRight, TransX1, TransX2, XAxis.LogTicks);
          rY:=Translate(rY, SelectionBottom, SelectionTop, TransY1, TransY2, YAxis.LogTicks);
          D:=Container.Items[I];
          if D is TRealData then
          begin
            (D as TRealData).RData[FXColumn]:=rX;
            (D as TRealData).RData[FYColumn]:=rY;
          end else
          begin
            if Assigned(FOnError)
            then FOnError(Self, strPlotDataError);
            Break;
          end;
          M:=true;
        end;
      end;
      if M then
      begin
        Invalidate;{!}
        Container.Modified:=true;
      end;
    finally
      Screen.Cursor:=crDefault;
      ShowPlotHint(' ');
    end;
  end;
  
begin
  inherited;
  if FPMInfo.InProgress
  then FPMInfo.InProgress:=false
  else Exit;
  // pmAutoZoom, pmZoom, pmUnZoom
  if FMouseMode in [pmAutoZoom, pmZoom, pmUnZoom]
  then doZoom;
  // pmSelect
  if FMouseMode=pmSelect
  then doSelect;
  // pmRuler
  if FMouseMode=pmRuler
  then DrawRuler(FPMInfo.RulerX, FPMInfo.RulerY); // just hide ruler!
  // pmPointEdit
  if FMouseMode=pmPointEdit
  then doPointEdit;
  // pmTranslate
  if FMouseMode=pmTranslate
  then doTranslate;
  // pmMargins
  if FMouseMode=pmMargins then
  begin
    DrawMargins(Canvas);
    FAutoMargins:=false;
    Invalidate;
  end;
  // pmLabelEdit
  if (FMouseMode=pmLabelEdit) and Assigned(FLabels.SelectedLabel)
  then FLabels.SelectedLabel._Move(Self);
  {todo: MouseUp for other mouse modes}
end;

function TPlot.CanUnZoom: Boolean;
begin
  Result:=FZoomStack.Count>0;
end;

//: notice: we restore XAxis, YAxis saved with OldXX coordinates! 
procedure TPlot.UndoZoom;
begin
  if FZoomStack.Count>0 then
  with FZoomStack.Pop as TUndoZoomCoordinates do
  begin
    if Assigned(XAxis) and Assigned(YAxis) then
    begin
      XAxis.Min:=OldX1;
      XAxis.Max:=OldX2;
      YAxis.Min:=OldY1;
      YAxis.Max:=OldY2;
    end else
    begin
      FXAxis.Min:=OldX1;
      FXAxis.Max:=OldX2;
      FYAxis.Min:=OldY1;
      FYAxis.Max:=OldY2;
      FXAxis2.Min:=Old2X1;
      FXAxis2.Max:=Old2X2;
      FYAxis2.Min:=Old2Y1;
      FYAxis2.Max:=Old2Y2;
    end;
    Free;
  end;
end;

//: WARNING: DrawRuler() must be called only from MouseMove/MouseUp! 
procedure TPlot.DrawRuler(X, Y: integer);
  
  const
    WFactor=40; // size of ruler relatively to plot
    LFactor=3;
  var
    A: array[0..4] of TPoint;
    Fi, Len: TReal;
  
begin
  with FPMInfo do
  begin
    if RulerFi>Pi/2.1
    then RulerFi:=Pi/2.1;  // correct rotation angle
    if RulerFi<-Pi/2.1
    then RulerFi:=-Pi/2.1;
    Fi:=arctan(Height/WFactor/Width*LFactor);
    Len:=sqrt(sqr(Width/LFactor)+sqr(Height/WFactor));
    A[0]:=Point(Round(X-Len*cos(RulerFi+Fi)), Round(Y+Len*sin(RulerFi+Fi)));
    A[1]:=Point(Round(X-Len*cos(RulerFi-Fi)), Round(Y+Len*sin(RulerFi-Fi)));
    A[2]:=Point(Round(X+Len*cos(RulerFi+Fi)), Round(Y-Len*sin(RulerFi+Fi)));
    A[3]:=Point(Round(X+Len*cos(RulerFi-Fi)), Round(Y-Len*sin(RulerFi-Fi)));
    A[4]:=A[0];
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Color:=clSilver;
    Canvas.Pen.Mode:=pmXor;
    Canvas.PolyLine(A);
    Canvas.Pen.Color:=clGray;
    Canvas.Pen.Mode:=pmNotXor;
    Canvas.MoveTo(X-Round(Len*cos(RulerFi)), Y+Round(Len*sin(RulerFi)));
    Canvas.LineTo(X+Round(Len*cos(RulerFi)), Y-Round(Len*sin(RulerFi)));
  end;
end;

//: draws "rubber thread" for point editor tool 
procedure TPlot.DrawEdit(X, Y: integer);
begin
  Canvas.Pen.Mode:=pmXor;
  Canvas.Pen.Style:=psDot;
  Canvas.Pen.Width:=1;
  Canvas.Pen.Color:=clGray;
  with FPMInfo do
  begin
    if (EditX1<>0) and (EditY1<>0) then
    begin
      if (X<>EditX) or (Y<>EditY) // hide previous line if it is not the same
      then Canvas.PolyLine([Point(EditX1,EditY1), Point(EditX,EditY)]);
      Canvas.PolyLine([Point(EditX1,EditY1), Point(X,Y)]);
    end;
    if (EditX2<>0) and (EditY2<>0) then
    begin
      if (X<>EditX) or (Y<>EditY)
      then Canvas.PolyLine([Point(EditX2,EditY2), Point(EditX,EditY)]);
      Canvas.PolyLine([Point(EditX2,EditY2), Point(X,Y)]);
    end;
    EditX:=X;
    EditY:=Y;
  end;
end;

function TPlot.BelongMarker(rX, rY: TReal; X, Y: integer): Boolean;
var
  iX, iY: Integer;
begin
  iX:=FPMInfo.XAxis.Scale(rX, true);
  iY:=FPMInfo.YAxis.Scale(rY, true);
  Result:=(X>=iX-MSZ) and (X<=iX+MSZ) and (Y>=iY-MSZ) and (Y<=iY+MSZ);
end;

//: draws Margins frame in MouseMode=pmMargins 
procedure TPlot.DrawMargins(Canvas: TCanvas);
begin
  if Canvas=Printer.Canvas  // frame cannot be printed
  then Exit;
  with Canvas do
  begin
    Pen.Width:=1;
    Pen.Style:=psDash;
    Pen.Mode:=pmXor;
    Pen.Color:=clGray;
    Brush.Style:=bsSolid;
    Brush.Color:=clSilver;
  end;
  // frame
  Canvas.Polyline([Point(LeftMargin, TopMargin),
    Point(Width-RightMargin-1, TopMargin),
    Point(Width-RightMargin-1, Height-BottomMargin-1),
    Point(LeftMargin, Height-BottomMargin-1),
    Point(LeftMargin, TopMargin)]);
  // axis title frames
    XAxis.PaintTitleFrame(Canvas);
    YAxis.PaintTitleFrame(Canvas);
    XAxis2.PaintTitleFrame(Canvas);
    YAxis2.PaintTitleFrame(Canvas);
  // axis labels frames
    XAxis.PaintLabelsFrame(Canvas);
    YAxis.PaintLabelsFrame(Canvas);
    XAxis2.PaintLabelsFrame(Canvas);
    YAxis2.PaintLabelsFrame(Canvas);
  // handles
  Canvas.Rectangle(LeftMargin-MSZ, TopMargin-MSZ,
    LeftMargin+MSZ, TopMargin+MSZ);
  Canvas.Rectangle(Width-RightMargin-1-MSZ, TopMargin-MSZ,
    Width-RightMargin-1+MSZ, TopMargin+MSZ);
  Canvas.Rectangle(Width-RightMargin-1-MSZ, Height-BottomMargin-1-MSZ,
    Width-RightMargin-1+MSZ, Height-BottomMargin-1+MSZ);
  Canvas.Rectangle(LeftMargin-MSZ, Height-BottomMargin-1-MSZ,
    LeftMargin+MSZ, Height-BottomMargin-1+MSZ);
end;

function TPlot.BelongMarker(iX, iY, X, Y: integer): Boolean;
begin
  Result:=InRange(X, iX-MSZ, iX+MSZ) and InRange(Y, iY-MSZ, iY+MSZ);
end;

procedure TPlot.SaveToFile(FileName: string);
var
  WMF: TMetafile;
begin
  WMF:=TMetafile.Create;
  try
    SaveToMetafile(WMF);
    WMF.SaveToFile(FileName);
  finally
    WMF.Free;
  end;
end;

procedure TPlot.Print(W, H: integer);
begin
  PaintCanvas(Printer.Canvas, W, H);
end;

procedure TPlot.Delete;
var
  K, P, DP: Integer;
  X, Y: TReal;
  M: Boolean;
begin
  M:=false;
  with ThisSerie do
  try
    if Empty then Exit;
    Screen.Cursor:=crHourGlass;
    K:=FirstLine;
    P:=0;
    DP:=LastLine-FirstLine;
    while K<=LastLine do
    begin
      if DP>0
      then Container.ShowProgress(Round(P/DP*100));
      Inc(P);
      GetPoint(K-FirstLine, SerieIndex, X, Y);
      if ((SelectionLeft<=X) and (SelectionRight>=X) and
        (SelectionBottom<=Y) and (SelectionTop>=Y))
        or (not SelectionVisible) then
      begin
        DeletePoint(K, Container);
        M:=true;
      end else Inc(K);
    end;{while}
  finally
    Screen.Cursor:=crDefault;
    if M
    then Container.Modified:=true; // only if data really were modified
  end;
end;

procedure TPlot.CopyToClipboard(Flags: TPlotCopyFlags=[pcmPage, pcmPoints, pcmLines, pcmSerie, pcmLabel]);
var
  WMF: TMetafile;
  I: Integer;
  J: Integer;
  S: TStringList;
  X: TReal;
  Y: TReal;
  Tab: Char;
  D: TData;
  Data: Pointer;
  HData: THandle;
  MS: TMemoryStream;
  ss: string;
begin
  if Flags-[pcmUseTabs]=[]
  then Exit; // nothing to do!
  if pcmUseTabs in Flags // delimiter for pcmPoints
  then Tab:=#9
  else Tab:=' ';
  ClipBoard.Open;
  try
    // copy page
    if pcmPage in Flags then
    begin
      WMF:=TMetafile.Create;
      try
        SaveToMetafile(WMF);
        Clipboard.Assign(WMF);
      finally
        WMF.Free;
      end;
    end;
    // copy Serie (may be empty!)
    if (pcmSerie in Flags) and Assigned(ThisSerie)
    then ThisSerie.CopyToClipboard;
    // copy selected label (if any)
    if (pcmLabel in Flags) and Assigned(FLabels.SelectedLabel)
    then FLabels.SelectedLabel.CopyToClipboard;
    // copy Points or/and Lines
    if ((pcmPoints in Flags) or (pcmLines in Flags)) and Assigned(ThisSerie) then
    with ThisSerie do
    begin
      if Empty
      then Exit;
      if IsFunction then
        if pcmPoints in Flags then
        begin
          Screen.Cursor:=crHourGlass;
          S:=TStringList.Create;
          try
            for I:=FirstLine to LastLine do
            begin
              if Assigned(Container) and (LastLine<>FirstLine)
              then Container.ShowProgress(Round((I-FirstLine)/(LastLine-FirstLine)*100));
              GetPoint(I-FirstLine, SerieIndex, X, Y);
              if ((SelectionLeft<=X) and (SelectionRight>=X) and
                (SelectionBottom<=Y) and (SelectionTop>=Y))
                or (not SelectionVisible) then // if invisible - copy ALL points!
              S.Add(FloatToStr(X)+Tab+FloatToStr(Y));
            end;
            Clipboard.AsText:=S.Text;
          finally
            S.Free;
            Screen.Cursor:=crDefault;
          end;
          Exit;
        end else Exit;
      Screen.Cursor:=crHourGlass;
      S:=TStringList.Create;
      MS:=TMemoryStream.Create; // for pcmLines only
      J:=0;
      MS.Write(J, SizeOf(J)); // set size counter
      try
        Assert(Assigned(Container));
        for I:=FirstLine to LastLine do
        begin
          if LastLine<>FirstLine
          then Container.ShowProgress(Round((I-FirstLine)/(LastLine-FirstLine)*100));
          D:=Container.Items[I]; // get data item for pcmLines
          GetPoint(I-FirstLine, SerieIndex, X, Y); // get point for pcmPoints
          if ((SelectionLeft<=X) and (SelectionRight>=X) and
            (SelectionBottom<=Y) and (SelectionTop>=Y))
            or (not SelectionVisible) then // if invisible - copy ALL points!
          begin
            // copy as text
            if pcmPoints in Flags
            then S.Add(FloatToStr(X)+Tab+FloatToStr(Y))
            else
              if D is TRealData then
              begin
                ss:='';
                for J:=1 to TRealData(D).Size do
                ss:=ss+TRealData(D).GetItemText(J)+Tab;
                S.Add(ss);
              end
              else S.Add(D.Data);
            // copy as TRealData array
            if (pcmLines in Flags) and (D is TRealData) then
            with D as TRealData do
            begin
              J:=Size;
              MS.Write(J, SizeOf(J));
              for J:=1 to Size do
              begin
                X:=RData[J];
                MS.Write(X, SizeOf(X));
              end;
              integer(MS.Memory^):=integer(MS.Memory^)+1;
            end;
          end;{if}
        end;{for}
        Clipboard.AsText:=S.Text;
        if integer(MS.Memory^)>0 then
        begin
          HData:=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, MS.Size);
          try
            Data:=GlobalLock(HData);
            try
              Move(MS.Memory^, Data^, MS.Size);
              SetClipboardData(TRealData.GetClipboardFormat, HData);
            finally
              GlobalUnlock(HData);
            end;
          except
            GlobalFree(HData); raise;
          end;
        end;
      finally
        S.Free;
        MS.Free;
        Screen.Cursor:=crDefault;
      end;
    end;{with}
  finally
    ClipBoard.Close;
  end;
end;

//: Deletes point and corrects related series 
procedure TPlot.DeletePoint(Idx: integer; Container: TContainer);
var
  SI, FI, CI: Integer;
  Ser2: TSerie;
begin
  for FI:=0 to Screen.FormCount-1 do // try to correct blocks in related series
  for CI:=0 to Screen.Forms[FI].ComponentCount-1 do
  if Screen.Forms[FI].Components[CI] is TPlot then
  for SI:=0 to (Screen.Forms[FI].Components[CI] as TPlot).FSeries.Count-1 do
  begin
    Ser2:=(Screen.Forms[FI].Components[CI] as TPlot).FSeries[SI];
    if (Ser2.Container=Container)
      and (not Ser2.Empty) and (not Ser2.IsFunction) then
    begin
      if Idx<=Ser2.LastLine
      then Ser2.LastLine:=Ser2.LastLine-1;
      if Idx<Ser2.FirstLine
      then Ser2.FirstLine:=Ser2.FirstLine-1;
    end;
  end;
  with Container do // free data and delete item
  begin
    TData(Items[Idx]).Free;
    Items.Delete(Idx);
  end;
end;

procedure TPlot.SetAreaSeries(Value: Boolean);
begin
  if FAreaSeries <> Value then
  begin
    FAreaSeries := Value;
    Changed(Self);
  end;
end;

procedure TPlot.SetAutoScaleLabels(Value: Boolean);
begin
  if FAutoScaleLabels <> Value then
  begin
    FAutoScaleLabels := Value;
    Changed(Self);
  end;
end;

procedure TPlot.SetClipPoints(Value: Boolean);
begin
  if FClipPoints<>Value then
  begin
    FClipPoints:=Value;
    Changed(Self);
  end;
end;

procedure TPlot.SetLabels(Value: TLabels);
begin
  if Assigned(FLabels) then
  FLabels.Assign(Value);
end;

end.
