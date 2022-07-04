{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingSurface;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SCDECommon, SCDEConsts, SCDEControl, SCDEBitmap, SCDrawingCommons, SCXML;

type
  TSCDeShape = class;
  TSCDePoint = class;
  TSCDeShapeBase = class;
  TSCDeContainer = class;
  TSCDeLayer = class;
  TSCDeSurface = class;

  TSCDeShapeEvent = procedure(Sender: TObject; Shape: TSCDeShapeBase) of object;
  TSCDeSaveLoadEvent = procedure(Sender: TObject; Root: TSCDomElement) of object;

  TSCDeSurfaceClient = class
  private
    FSender: TSCDeSurface;
    FOnActiveLayerChange: TNotifyEvent;
    FOnInsertLayer: TSCDeShapeEvent;
    FOnInsertShape: TSCDeShapeEvent;
    FOnRemoveLayer: TSCDeShapeEvent;
    FOnRemoveShape: TSCDeShapeEvent;
    FOnLayerChange: TSCDeShapeEvent;
    FOnClearShape: TSCDeShapeEvent;
    FOnFramePropsChange: TNotifyEvent;
    FOnGridPropsChange: TNotifyEvent;
    FOnLayerPropsChange: TNotifyEvent;
    FOnZoom: TNotifyEvent;
    FOnBeforeLoadDOM: TSCDeSaveLoadEvent;
    FOnAfterLoadDOM: TSCDeSaveLoadEvent;
    FOnBeforeSaveDOM: TSCDeSaveLoadEvent;
    FOnAfterSaveDOM: TSCDeSaveLoadEvent;
  protected
    procedure DoActiveLayerChange; dynamic;
    procedure DoInsertLayer(Shape: TSCDeShapeBase); dynamic;
    procedure DoInsertShape(Shape: TSCDeShapeBase); dynamic;
    procedure DoRemoveLayer(Shape: TSCDeShapeBase); dynamic;
    procedure DoRemoveShape(Shape: TSCDeShapeBase); dynamic;
    procedure DoLayerChange(Shape: TSCDeShapeBase); dynamic;
    procedure DoShapeCleared(Shape: TSCDeShapeBase); dynamic;
    procedure DoFramePropsChange; dynamic;
    procedure DoGridPropsChange; dynamic;
    procedure DoLayerPropsChange; dynamic;
    procedure DoZoom; dynamic;
    procedure DoBeforeLoadDOM(Root: TSCDomElement); dynamic;
    procedure DoAfterLoadDOM(Root: TSCDomElement); dynamic;
    procedure DoBeforeSaveDOM(Root: TSCDomElement); dynamic;
    procedure DoAfterSaveDOM(Root: TSCDomElement); dynamic;
  public
    constructor Create(ASender: TSCDeSurface); virtual;
    destructor Destroy; override;

    property Sender: TSCDeSurface read FSender;
    property OnActiveLayerChange: TNotifyEvent read FOnActiveLayerChange write FOnActiveLayerChange;
    property OnInsertLayer: TSCDeShapeEvent read FOnInsertLayer write FOnInsertLayer;
    property OnInsertShape: TSCDeShapeEvent read FOnInsertShape write FOnInsertShape;
    property OnRemoveLayer: TSCDeShapeEvent read FOnRemoveLayer write FOnRemoveLayer;
    property OnRemoveShape: TSCDeShapeEvent read FOnRemoveShape write FOnRemoveShape;
    property OnLayerChange: TSCDeShapeEvent read FOnLayerChange write FOnLayerChange;
    property OnClearShape: TSCDeShapeEvent read FOnClearShape write FOnClearShape;
    property OnFramePropsChange: TNotifyEvent read FOnFramePropsChange write FOnFramePropsChange;
    property OnGridPropsChange: TNotifyEvent read FOnGridPropsChange write FOnGridPropsChange;
    property OnLayerPropsChange: TNotifyEvent read FOnLayerPropsChange write FOnLayerPropsChange;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnBeforeLoadDOM: TSCDeSaveLoadEvent read FOnBeforeLoadDOM write FOnBeforeLoadDOM;
    property OnAfterLoadDOM: TSCDeSaveLoadEvent read FOnAfterLoadDOM write FOnAfterLoadDOM;
    property OnBeforeSaveDOM: TSCDeSaveLoadEvent read FOnBeforeSaveDOM write FOnBeforeSaveDOM;
    property OnAfterSaveDOM: TSCDeSaveLoadEvent read FOnAfterSaveDOM write FOnAfterSaveDOM;
  end;

  TSCDePointControl = class(TPersistent)
  private
    Fx: Double;
    Fy: Double;
    FDestroying: Boolean;
    FOwner: TSCDePoint;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    procedure Setx(Value: Double);
    procedure Sety(Value: Double);
    function  GetValue: TDoublePoint;
    procedure SetValue(Value: TDoublePoint);
    function  GetInUpdate: Boolean;
  protected
    function  GetOwner: TPersistent; override;

    procedure Destroying;
    procedure Changing;
    procedure Changed;
  public
    constructor Create; virtual;
    procedure  BeforeDestruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure SetPosition(Ax, Ay: Double);

    procedure BeginUpdate;
    procedure EndUpdate;

    property InDestroy: Boolean read FDestroying;
    property InUpdate: Boolean read GetInUpdate;
  published
    property x: Double read Fx write Setx;
    property y: Double read Fy write Sety;
    property Point: TDoublePoint read GetValue write SetValue;
  end;

  TSCDePositionEvent = procedure(Sender: TObject; var x, y: Double) of object;

  TSCDePointControlClass = class of TSCDePointControl;

  TSCDePoint = class(TPersistent)
  private
    Fx: Double;
    Fy: Double;
    FControl_1: TSCDePointControl;
    FControl_2: TSCDePointControl;
    FDestroying: Boolean;
    FOwner: TSCDeShapeBase;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FOnChanging: TSCDePositionEvent;
    function  GetIndex: Integer;
    procedure Setx(Value: Double);
    procedure Sety(Value: Double);
    function  GetValue: TDoublePoint;
    procedure SetValue(Value: TDoublePoint);
    procedure SetControl_1(C: TSCDePointControl);
    procedure SetControl_2(C: TSCDePointControl);
    function  GetInUpdate: Boolean;

    procedure RemoveControl(C: TSCDePointControl);

    procedure DoControlAdded(C: TSCDePointControl);
    procedure DoControlRemoved(C: TSCDePointControl);
    procedure DoControlChanging(C: TSCDePointControl);
    procedure DoControlChanged(C: TSCDePointControl);
  protected
    function  GetOwner: TPersistent; override;
    function  GetControlClass: TSCDePointControlClass; dynamic;

    function  CanInsertControl(C: TSCDePointControl): Boolean; virtual;

    procedure ControlAdded(C: TSCDePointControl); dynamic;
    procedure ControlRemoved(C: TSCDePointControl); dynamic;
    procedure ControlChanging(C: TSCDePointControl); dynamic;
    procedure ControlChanged(C: TSCDePointControl); dynamic;

    procedure StartAction;
    procedure EndAction;

    procedure NotifyUndo;

    procedure Destroying;
    procedure Changing;
    procedure Changed;

    property OnChanging: TSCDePositionEvent read FOnChanging write FOnChanging;
  public
    constructor Create(AOwner: TSCDeShapeBase);
    procedure  BeforeDestruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure SetPosition(Ax, Ay: Double);

    function  NewControl(x, y: Double): TSCDePointControl;
    function  ControlAt(P: TDoublePoint; Fuzz: Double): Integer; virtual;
    procedure ClearControls;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Index: Integer read GetIndex;
    property InDestroy: Boolean read FDestroying;
    property InUpdate: Boolean read GetInUpdate;
  published
    property x: Double read Fx write Setx;
    property y: Double read Fy write Sety;
    property Point: TDoublePoint read GetValue write SetValue;
    property Control_1: TSCDePointControl read FControl_1 write SetControl_1;
    property Control_2: TSCDePointControl read FControl_2 write SetControl_2;
  end;

  TSCDeControl = class(TPersistent)
  private
    Fx: Double;
    Fy: Double;
    FDestroying: Boolean;
    FOwner: TSCDeShapeBase;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FOnChanging: TSCDePositionEvent;
    function  GetIndex: Integer;
    procedure Setx(Value: Double);
    procedure Sety(Value: Double);
    function  GetValue: TDoublePoint;
    procedure SetValue(Value: TDoublePoint);
    function  GetInUpdate: Boolean;
  protected
    function  GetOwner: TPersistent; override;

    procedure StartAction;
    procedure EndAction;

    procedure NotifyUndo;

    procedure Destroying;
    procedure Changing;
    procedure Changed;

    property OnChanging: TSCDePositionEvent read FOnChanging write FOnChanging;
  public
    constructor Create(AOwner: TSCDeShapeBase);
    procedure  BeforeDestruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure SetPosition(Ax, Ay: Double);

    procedure BeginUpdate;
    procedure EndUpdate;

    property Index: Integer read GetIndex;
    property InDestroy: Boolean read FDestroying;
    property InUpdate: Boolean read GetInUpdate;
    property Point: TDoublePoint read GetValue write SetValue;
  published
    property x: Double read Fx write Setx;
    property y: Double read Fy write Sety;
  end;

  TSCDeShapePen = class(TPersistent)
  private
    FColor: TColor;
    FMode: TPenMode;
    FStyle: TPenStyle;
    FWidth: Integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TSCDeShapeBase;
    procedure SetColor(Value: TColor);
    procedure SetMode(Value: TPenMode);
    procedure SetStyle(Value: TPenStyle);
    procedure SetWidth(Value: Integer);
  protected
    function GetOwner: TPersistent; override;
    procedure SetOwner(AOwner: TSCDeShapeBase);

    procedure Changing;
    procedure Changed;

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;

    procedure AssignRec(const Source: TSCDePenRec);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadFromXML(Node: TSCDomElement); dynamic;
    procedure SaveToXML(Node: TSCDomElement); dynamic;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Mode: TPenMode read FMode write SetMode default pmCopy;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property Width: Integer read FWidth write SetWidth default 1;
  end;

  TSCDeShapeBrush = class(TPersistent)
  private
    FColor: TColor;
    FStyle: TBrushStyle;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TSCDeShapeBase;
    function  GetColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetStyle(Value: TBrushStyle);
  protected
    function GetOwner: TPersistent; override;
    procedure SetOwner(AOwner: TSCDeShapeBase);

    procedure Changing;
    procedure Changed;

    procedure ShapeColorChanged;

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;

    procedure AssignRec(const Source: TSCDeBrushRec);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadFromXML(Node: TSCDomElement); dynamic;
    procedure SaveToXML(Node: TSCDomElement); dynamic;
  published
    property Color: TColor read GetColor write SetColor default clLime;
    property Style: TBrushStyle read FStyle write SetStyle default bsSolid;
  end;

  TSCDeShapeGradient = class(TPersistent)
  private
    FColorBegin: TColor;
    FColorEnd: TColor;
    FStyle: TSCDeShapeGradientStyle;
    FShift: TSCDeGradientShift;
    FRotation: TSCDeGradientRotation;
    FReverse: Boolean;
    FPattern: TBitmap;
    FUpdateCount: Integer;
    FDirty: Boolean;
    FNeedsUpdate: Boolean;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TSCDeShapeBase;
    function  GetColorBegin: TColor;
    procedure SetColorBegin(Value: TColor);
    procedure SetColorEnd(Value: TColor);
    procedure SetStyle(Value: TSCDeShapeGradientStyle);
    procedure SetShift(Value: TSCDeGradientShift);
    procedure SetRotation(Value: TSCDeGradientRotation);
    procedure SetReverse(Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
    procedure SetOwner(AOwner: TSCDeShapeBase);

    procedure Changing;
    procedure Changed;

    procedure ShapeColorChanged;
    procedure UpdatePattern; virtual;

    property Pattern: TBitmap read FPattern;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AssignRec(const Source: TSCDeShapeGradienthRec);
    procedure Assign(Source: TPersistent); override;

    procedure Paint(ACanvas: TCanvas; ARect: TRect);
    function  CopyPatternTo(ABitmap: TBitmap): Boolean;
    procedure InvalidatePattern;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure LoadFromXML(Node: TSCDomElement); dynamic;
    procedure SaveToXML(Node: TSCDomElement); dynamic;
  published
    property ColorBegin: TColor read GetColorBegin write SetColorBegin default clLime;
    property ColorEnd: TColor read FColorEnd write SetColorEnd default clWhite;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property Rotation: TSCDeGradientRotation read FRotation write SetRotation default 0;
    property Shift: TSCDeGradientShift read FShift write SetShift default 0;
    property Style: TSCDeShapeGradientStyle read FStyle write SetStyle default scdgsNone;
  end;

  TSCDeShapeFont = class(TPersistent)
  private
    FCharset: TFontCharset;
    FColor: TColor;
    FName: TFontName;
    FSize: Integer;
    FStyle: TFontStyles;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TSCDeShapeBase;
    procedure SetCharset(Value: TFontCharset);
    procedure SetColor(Value: TColor);
    procedure SetName(Value: TFontName);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
  protected
    function GetOwner: TPersistent; override;
    procedure SetOwner(AOwner: TSCDeShapeBase);

    procedure Changing;
    procedure Changed;

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;

    procedure AssignRec(const Source: TSCDeShapeFontRec);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadFromXML(Node: TSCDomElement); dynamic;
    procedure SaveToXML(Node: TSCDomElement); dynamic;
  published
    property Charset: TFontCharset read FCharset write SetCharset default DEFAULT_CHARSET;
    property Color: TColor read FColor write SetColor default clWindowText;
    property Name: TFontName read FName write SetName;
    property Size: Integer read FSize write SetSize default 8;
    property Style: TFontStyles read FStyle write SetStyle default [];
  end;

  TSCDeSelectionData = record
    LineColor: TColor;
    LineStyle: TPenStyle;
    LineMode: TPenMode;
    CtrlInColor: TColor;
    CtrlOutColor: TColor;
    PointInColor: TColor;
    PointOutColor: TColor;
    PointSize: Integer;
    SelectionType: TSCDeSelectionType;
  end;
  PSCDeSelectionRec = ^TSCDeSelectionData;

  TSCDeDragPoint = record
    PartIndex: Integer;
    SubIndex: Integer;
    PointType: TSCDePointType;
    Offset: TDoublePoint;
  end;

  TSCDeShapeNotifier = class(TObject)
  private
    FShapes: TList;
    FOnChanging: TSCDeShapeEvent;
    FOnChange: TSCDeShapeEvent;
    FOnDestroy: TSCDeShapeEvent;

    procedure Changing(S: TSCDeShapeBase);
    procedure Changed(S: TSCDeShapeBase);
    procedure Destroyed(S: TSCDeShapeBase);

    function  GetCount: Integer;
    function  GetShape(Index: Integer): TSCDeShapeBase;
  public
    destructor Destroy; override;

    procedure UnregisterAll;
    procedure RegisterShape(S: TSCDeShapeBase);
    procedure UnregisterShape(S: TSCDeShapeBase);

    property Count: Integer read GetCount;
    property Shapes[Index: Integer]: TSCDeShapeBase read GetShape; default;
    property OnChanging: TSCDeShapeEvent read FOnChanging write FOnChanging;
    property OnChange: TSCDeShapeEvent read FOnChange write FOnChange;
    property OnDestroy: TSCDeShapeEvent read FOnDestroy write FOnDestroy;
  end;

  TSCDeShapeBase = class(TPersistent)
  private
    FID: DWord;
    FTempID: DWord;
    FName: String;
    FData: TObject;
    FText: String;
    FColor: TColor;
    FCaption: TCaption;
    FVisible: Boolean;
    FLocked: Boolean;
    FDestroying: Boolean;
    FPoints: TList;
    FControls: TList;
    FShapeStyle: TSCDeShapeStyles;
    FBrush: TSCDeShapeBrush;
    FPen: TSCDeShapePen;
    FFont: TSCDeShapeFont;
    FPicture: TPicture;
    FPictureStyle: TSCDeShapePictureStyle;
    FSelectionList: TList;
    FOwner: TSCDeContainer;
    FControlUpdate: Cardinal;
    FControlChange: Cardinal;
    FPointUpdate: Cardinal;
    FPointChange: Cardinal;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FNotifyList: TList;
    FSurface: TSCDeSurface;
    FNotifyLock: Integer;
    FGradient: TSCDeShapeGradient;
    procedure SetColor(Value: TColor);
    procedure SetBrush(Value: TSCDeShapeBrush);
    procedure SetFont(Value: TSCDeShapeFont);
    procedure SetPen(Value: TSCDeShapePen);
    procedure SetPicture(Value: TPicture);
    procedure SetPictureStyle(Value: TSCDeShapePictureStyle);
    procedure SetLocked(Value: Boolean);
    procedure SetName(const Value: String);
    procedure SetText(const Value: String);
    procedure SetVisible(Value: Boolean);
    procedure SetGradient(Value: TSCDeShapeGradient);

    function  GetControl(Index: Integer): TSCDeControl;
    function  GetControlCount: Integer;
    function  GetSelection(Index: Integer): TSCDePoint;
    function  GetSelectionCount: Integer;
    function  GetNeedsUpdate: Boolean;
    function  GetInUpdate: Boolean;
    function  GetInControlUpdate: Boolean;
    function  GetInPointUpdate: Boolean;
    function  GetNotifyLocked: Boolean;

    procedure InsertControl(C: TSCDeControl);
    procedure RemoveControl(C: TSCDeControl);

    procedure InsertPoint(P: TSCDePoint);
    procedure RemovePoint(P: TSCDePoint);

    procedure DoControlAdded(C: TSCDeControl);
    procedure DoControlRemoved(C: TSCDeControl);
    procedure DoControlChanging(C: TSCDeControl);
    procedure DoControlChanged(C: TSCDeControl);

    procedure DoPointAdded(P: TSCDePoint);
    procedure DoPointRemoved(P: TSCDePoint);
    procedure DoPointChanging(P: TSCDePoint);
    procedure DoPointChanged(P: TSCDePoint);

    procedure DoBrushChanging(Sender: TObject);
    procedure DoFontChanging(Sender: TObject);
    procedure DoPenChanging(Sender: TObject);
    procedure DoPictureChanging(Sender: TObject);
    procedure DoPictureStyleChanging;
    procedure DoGradientChanging(Sender: TObject);
    procedure DoBrushChanged(Sender: TObject);
    procedure DoFontChanged(Sender: TObject);
    procedure DoPenChanged(Sender: TObject);
    procedure DoPictureChanged(Sender: TObject);
    procedure DoPictureStyleChanged;
    procedure DoGradientChanged(Sender: TObject);

    procedure DoControlIsChanging(Sender: TObject; var x, y: Double);
    procedure DoPointIsChanging(Sender: TObject; var x, y: Double);

    procedure NotifyChanging;
    procedure NotifyChange;
    procedure NotifyDestroy;

    procedure ReleaseGradient;

    procedure RegisterNotifier(Notifier: TSCDeShapeNotifier);
    procedure UnregisterNotifier(Notifier: TSCDeShapeNotifier);
  protected
    procedure Destroying;
    function  GetOwner: TPersistent; override;

    procedure StoreID;
    procedure RestoreID;

    procedure SetSurface(ASurface: TSCDeSurface); virtual;
    function  GetGradient: TSCDeShapeGradient; virtual;

    function  CanInsertControl(C: TSCDeControl): Boolean; virtual;
    function  CanInsertPoint(P: TSCDePoint): Boolean; virtual;

    procedure StartAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;
    procedure EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;

    procedure NotifyUndo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;

    procedure ControlAdded(C: TSCDeControl); virtual;
    procedure ControlRemoved(C: TSCDeControl); virtual;
    procedure ControlChanging(C: TSCDeControl); virtual;
    procedure ControlChanged(C: TSCDeControl); virtual;

    procedure PointAdded(P: TSCDePoint); virtual;
    procedure PointRemoved(P: TSCDePoint); virtual;
    procedure PointChanging(P: TSCDePoint); virtual;
    procedure PointChanged(P: TSCDePoint); virtual;

    procedure ControlIsChanging(C: TSCDeControl; var x, y: Double); virtual;
    procedure PointIsChanging(P: TSCDePoint; var x, y: Double); virtual;

    procedure BrushChanging(Sender: TObject); virtual;
    procedure FontChanging(Sender: TObject); virtual;
    procedure PenChanging(Sender: TObject); virtual;
    procedure PictureChanging(Sender: TObject); virtual;
    procedure PictureStyleChanging; virtual;
    procedure GradientChanging(Sender: TObject); virtual;
    procedure BrushChanged(Sender: TObject); virtual;
    procedure FontChanged(Sender: TObject); virtual;
    procedure PenChanged(Sender: TObject); virtual;
    procedure PictureChanged(Sender: TObject); virtual;
    procedure PictureStyleChanged; virtual;
    procedure GradientChanged(Sender: TObject); virtual;

    procedure SelectionChanged; dynamic;

    procedure SetCaption(const Value: TCaption); virtual;
    procedure SetShapeStyle(Value: TSCDeShapeStyles);

    procedure BeforeSetShapeStyle(var Value: TSCDeShapeStyles); dynamic;
    procedure ShapeStyleChanged; dynamic;

    function  GetIndex: Integer; virtual;
    function  GetPoint(Index: Integer): TSCDePoint; virtual;
    function  GetPointCount: Integer; virtual;

    function  GetPointValue(Index: Integer): TDoublePoint; virtual;
    procedure SetPointValue(Index: Integer; Value: TDoublePoint); virtual;

    function  GetGravityCenter: TDoublePoint; virtual;

    procedure ClearPoints;
    procedure ClearControls;

    procedure Refine(Zoom: Double); dynamic;

    function  GetSurface: TSCDeSurface; virtual;
    function  GetCreationClicks: Integer; dynamic;

    procedure Changing;
    procedure Changed(Force: Boolean = False);
    procedure DoChanging; virtual;
    procedure DoChanged(Force: Boolean = False); virtual;

    function  DrawRect: TRect; virtual;
    procedure ArrangeBoundsRect(var R: TDoubleRect); virtual;

    procedure DrawPoint(C: TCanvas; R: TRect; InColor, OutColor: TColor); virtual;

    procedure UpdateRegion; virtual;
    procedure SetRegion(const Rgn: HRgn); virtual;
    function  GetRegion: HRgn; virtual;

    function  GetPointArray: TDPointArray; dynamic;

    procedure Add(P: TSCDePoint); overload; dynamic;
    function  Add(x, y: Double): TSCDePoint; overload; dynamic;
    procedure Insert(Index: Integer; P: TSCDePoint); overload; dynamic;
    function  ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint; overload; dynamic;
    procedure Delete(Index: Integer); dynamic;
    procedure Remove(P: TSCDePoint); dynamic;
    procedure Extract(P: TSCDePoint); dynamic;
    procedure Clear; dynamic;

    procedure LoadPictureFromXML(Node: TSCDomElement); dynamic;
    procedure SavePictureToXML(Node: TSCDomElement); dynamic;

    function  GetProperty(Parent: TSCDomElement; const AName: WideString): WideString;
    function  AddProperty(Parent: TSCDomElement; const AName,
      AValue: WideString): TSCDomElement;

    procedure BeginNotifyLock;
    procedure EndNotifyLock;

    procedure CreateGradient;

    procedure DrawPicture(C: TCanvas; R: TRect);

    property Region: HRgn read GetRegion;
    property NotifyLocked: Boolean read GetNotifyLocked;
    property Brush: TSCDeShapeBrush read FBrush write SetBrush;
    property Font: TSCDeShapeFont read FFont write SetFont;
    property Pen: TSCDeShapePen read FPen write SetPen;
    property Picture: TPicture read FPicture write SetPicture;
    property PictureStyle: TSCDeShapePictureStyle read FPictureStyle write SetPictureStyle default scdspStretch;
    property Caption: TCaption read FCaption write SetCaption;
    property Gradient: TSCDeShapeGradient read GetGradient write SetGradient;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); virtual;
    procedure  BeforeDestruction; override;
    destructor Destroy; override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); virtual;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); virtual;

    procedure PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
      R: TDoubleRect); virtual;
    procedure DrawSelectionIn(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; R: TDoubleRect; Zoom: Double); virtual;

    procedure RecalculateBounds(Force: Boolean); dynamic;
    procedure Refresh(Force: Boolean = False); dynamic;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure BeginPointUpdate;
    procedure EndPointUpdate;

    procedure BeginControlUpdate;
    procedure EndControlUpdate;

    function  HasPen: Boolean;
    function  HasBrush: Boolean;
    function  HasGradient: Boolean;
    function  HasPicture: Boolean;

    function  GetShapeType: TSCShapeType; virtual;

    function  GetBounds: TDoubleRect; virtual;
    function  GetDefaultBounds: TDoubleRect; dynamic;
    procedure SetBounds(R: TDoubleRect); overload;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Double); overload; dynamic;

    procedure SetPenRec(const P: TSCDePenRec);
    procedure SetBrushRec(const B: TSCDeBrushRec);
    procedure SetFontRec(const F: TSCDeShapeFontRec);
    procedure SetGradientRec(const G: TSCDeShapeGradienthRec);

    procedure MoveBy(X, Y: Double); dynamic;
    procedure MoveTo(X, Y: Double);
    procedure Resize(NewWidth, NewHeight: Double); dynamic;
    procedure ResizeBy(DifX, DifY: Double);
    procedure Rotate(Angle: Double); overload;
    procedure Rotate(GravityCenter: TDoublePoint; Angle: Double); overload; dynamic;
    procedure OffsetPosition(Index: Integer; Offset: TDoublePoint); dynamic;

    function  IsParent(S: TSCDeShapeBase): Boolean;
    function  InRect(const R: TDoubleRect): Boolean; virtual;

    function  GetLayer: TSCDeLayer;

    function  IndexOf(P: TSCDePoint): Integer; virtual;
    function  Exists(P: TSCDePoint): Boolean;
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; virtual;
    function  PointAt(P: TDoublePoint; Fuzz: Double): Integer; virtual;
    function  IsOnLine(P: TDoublePoint): Boolean; virtual;

    function  PointControlAt(P: TDoublePoint; Fuzz: Double;
      IsSelected: Boolean; var PointIndex: Integer): Integer; virtual;
                                          
    function  IndexOfControl(C: TSCDeControl): Integer; virtual;
    function  ControlExists(C: TSCDeControl): Boolean;
    function  ControlAt(P: TDoublePoint; Fuzz: Double): Integer; virtual;

    function  HitTest(P: TDoublePoint; Fuzz, PointSize: Double;
      Selected, Editing: Boolean; Shift: TShiftState): TSCDeShapeHit; virtual;

    procedure SelectAll;
    procedure ClearSelection;
    procedure UpdateSelection;

    procedure Select(P: TSCDePoint);
    procedure AddSelection(P: TSCDePoint);
    procedure AddSelections(A: array of TSCDePoint);
    procedure RemoveSelection(P: TSCDePoint);
    procedure RemoveSelections(A: array of TSCDePoint);

    function  IsSelected(P: TSCDePoint): Boolean; overload;
    function  IsSelected(Index: Integer): Boolean; overload;
    procedure GetSelections(L: TList);

    procedure LoadBoundsFromXML(Node: TSCDomElement); dynamic;
    procedure SaveBoundsToXML(Node: TSCDomElement); dynamic;

    procedure LoadFromXML(Node: TSCDomElement); dynamic;
    procedure SaveToXML(Node: TSCDomElement); dynamic;

    procedure LoadFromFile(const FileName: String); dynamic;
    procedure SaveToFile(const FileName: String); dynamic;

    function ShapeByID(AID: DWord): TSCDeShapeBase; dynamic;

    property ID: DWord read FID;
    property Surface: TSCDeSurface read FSurface;
    property Index: Integer read GetIndex;
    property UpdateCount: Cardinal read FUpdateCount;
    property NeedsUpdate: Boolean read GetNeedsUpdate;
    property InControlUpdate: Boolean read GetInControlUpdate;
    property InPointUpdate: Boolean read GetInPointUpdate;
    property InUpdate: Boolean read GetInUpdate;
    property InDestroy: Boolean read FDestroying;
    property Owner: TSCDeContainer read FOwner;
    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TSCDeControl read GetControl;
    property PointCount: Integer read GetPointCount;
    property Points[Index: Integer]: TSCDePoint read GetPoint; default;
    property PointValue[Index: Integer]: TDoublePoint read GetPointValue write SetPointValue;
    property Data: TObject read FData write FData;
    property SelectionCount: Integer read GetSelectionCount;
    property Selections[Index: Integer]: TSCDePoint read GetSelection;
    property ShapeStyle: TSCDeShapeStyles read FShapeStyle;
  published
    property Locked: Boolean read FLocked write SetLocked default False;
    property Name: String read FName write SetName;
    property Text: String read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TSCDeShapeClass = class of TSCDeShapeBase;

  TSCDeShape = class(TSCDeShapeBase)
  protected
    procedure Add(P: TSCDePoint); overload; override;
    function  Add(x, y: Double): TSCDePoint; overload; override;
    procedure Insert(Index: Integer; P: TSCDePoint); override;
    procedure Delete(Index: Integer); override;
    procedure Remove(P: TSCDePoint); override;
    procedure Extract(P: TSCDePoint); override;
    procedure Clear; override;
  public
    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
  end;

  TSCDeContainer = class(TSCDeShapeBase)
  private
    FItems: TList;
    FBounds: TDoubleRect;
    function  GetItemCount: Integer;
    function  GetItem(Index: Integer): TSCDeShapeBase;

    procedure InsertShape(S: TSCDeShapeBase);
    procedure RemoveShape(S: TSCDeShapeBase);

    procedure DoShapeChanging(S: TSCDeShapeBase);
    procedure DoShapeChanged(S: TSCDeShapeBase);
    procedure DoShapeInserted(S: TSCDeShapeBase);
    procedure DoShapeRemoved(S: TSCDeShapeBase);
    procedure DoShapeCleared(S: TSCDeShapeBase);
  protected
    procedure SetSurface(ASurface: TSCDeSurface); override;
    procedure DoChanged(Force: Boolean); override;

    function  CanInsertShape(S: TSCDeShapeBase): Boolean; virtual;

    function  GetPointValue(Index: Integer): TDoublePoint; override;
    procedure SetPointValue(Index: Integer; Value: TDoublePoint); override;

    procedure ShapeChanging(S: TSCDeShapeBase); dynamic;
    procedure ShapeChanged(S: TSCDeShapeBase); dynamic;
    procedure ShapeInserted(S: TSCDeShapeBase); dynamic;
    procedure ShapeRemoved(S: TSCDeShapeBase); dynamic;
    procedure ShapeCleared(S: TSCDeShapeBase); dynamic;
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;
    destructor Destroy; override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;

    procedure PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
      R: TDoubleRect); override;
    procedure DrawSelectionIn(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; R: TDoubleRect;
      Zoom: Double); override;

    function  GetBounds: TDoubleRect; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Double); overload; override;
    procedure RecalculateBounds(Force: Boolean); override;

    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;
    function  InRect(const R: TDoubleRect): Boolean; override;
    function  IsOnLine(P: TDoublePoint): Boolean; override;

    procedure Add(S: TSCDeShapeBase); reintroduce;
    procedure Insert(Index: Integer; S: TSCDeShapeBase); reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    procedure Remove(S: TSCDeShapeBase); reintroduce;
    function  Extract(S: TSCDeShapeBase): Boolean; reintroduce;
    function  IndexOf(S: TSCDeShapeBase): Integer; reintroduce;
    function  Exists(S: TSCDeShapeBase): Boolean; reintroduce;
    procedure Clear; reintroduce; dynamic;

    procedure BringForward(S: TSCDeShapeBase);
    procedure SendBackward(S: TSCDeShapeBase);
    procedure BringToFront(S: TSCDeShapeBase);
    procedure SendToBack(S: TSCDeShapeBase);

    function  ShapeAtPos(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean): TSCDeShapeBase; virtual;
    function  PointAt(P: TDoublePoint; Fuzz: Double): Integer; override;
    function  ControlAt(P: TDoublePoint; Fuzz: Double): Integer; override;

    procedure LoadBoundsFromXML(Node: TSCDomElement); override;
    procedure SaveBoundsToXML(Node: TSCDomElement); override;

    function  ShapeByID(AID: DWord): TSCDeShapeBase; override;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TSCDeShapeBase read GetItem; default;
  end;

  TSCDePackage = class(TSCDeContainer)
  public
    constructor Create(ASurface: TSCDeSurface; AOwner: TSCDeContainer); override;

    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;

    procedure Paint(C: TCanvas; X, Y: Integer; Zoom: Double); override;
    procedure PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
      R: TDoubleRect); override;

    procedure LoadFromXML(Node: TSCDomElement); override;
    procedure SaveToXML(Node: TSCDomElement); override;

    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile(const FileName: String); override;
  published
    property Brush;
    property Picture;
  end;

  TSCDeGroup = class(TSCDeContainer)
  public
    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;

    function  PointOnShape(P: TDoublePoint; Fuzz: Double;
      Editing: Boolean = True): Boolean; override;
  end;

  TSCDeLayer = class(TSCDeContainer)
  protected
    function  GetIndex: Integer; override;

    function  GetPointValue(Index: Integer): TDoublePoint; override;
    procedure SetPointValue(Index: Integer; Value: TDoublePoint); override;

    procedure StartAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); override;
    procedure EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); override;

    procedure NotifyUndo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); override;

    procedure ShapeInserted(S: TSCDeShapeBase); override;
    procedure ShapeRemoved(S: TSCDeShapeBase); override;
    procedure ShapeCleared(S: TSCDeShapeBase); override;
    procedure DoChanged(Force: Boolean); override;

    function  GetOwner: TPersistent; override;
    function  GetSurface: TSCDeSurface; override;
  public
    constructor Create(AOwner: TSCDeSurface); reintroduce; virtual;
    destructor Destroy; override;

    procedure DrawSelection(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; Zoom: Double); override;
    procedure DrawSelectionIn(C: TCanvas; X, Y: Double; const DragPoint: TSCDeDragPoint;
      const Data: TSCDeSelectionData; R: TDoubleRect; Zoom: Double); override;

    procedure LoadBoundsFromXML(Node: TSCDomElement); override;
    procedure SaveBoundsToXML(Node: TSCDomElement); override;

    procedure OffsetPosition(Index: Integer; Offset: TDoublePoint); override;

    procedure MoveBy(X, Y: Double); override;
    procedure Resize(NewWidth, NewHeight: Double); override;
    procedure RecalculateBounds(Force: Boolean); override;

    function  PointAt(P: TDoublePoint; Fuzz: Double): Integer; override;
    function  ControlAt(P: TDoublePoint; Fuzz: Double): Integer; override;
  end;

  TSCDeLayerClass = class of TSCDeLayer;

  TSCDeScrollbar = class(TSCDeControlScrollbar)
  private
    function GetScrollPos: Integer;
  protected
    function GetSensitivity: Integer; override;
    property Sensitivity default 120;
  public
    constructor Create(AOwner: TSCDeCustomControl; AKind: TSCDeScrollbarKind); override;
    property ScrollPos: Integer read GetScrollPos;
  published
    property Background;
    property ButtonColors;
    property Icons;
    property SlideLine;
    property Thumb;
    property Track default False;
  end;

  TSCDeScrollbars = class(TSCDeControlScrollbars);

  TSCDeBorderProps = class(TSCDeControlBorderProps)
  public
    constructor Create(AOwner: TSCDeCustomControl); override;
  published
    property Border default scdcb3DLowered;
    property Color default clWindow;
    property ExDraw;
    property FlatColor;
    property FlatInnerColor;
    property InnerBorder;
    property Width;
  end;

  TSCDeLayerProperties = class;

  TSCDeCheckersProperties = class(TPersistent)
  private
    FColorEven: TColor;
    FColorOdd: TColor;
    FWidth: Integer;
    FHeight: Integer;
    FOwner: TSCDeLayerProperties;
    procedure SetColorEven(Value: TColor);
    procedure SetColorOdd(Value: TColor);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
  protected
    procedure Changed;
    function  GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TSCDeLayerProperties); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property ColorEven: TColor read FColorEven write SetColorEven default clWhite;
    property ColorOdd: TColor read FColorOdd write SetColorOdd default clSilver;
    property Width: Integer read FWidth write SetWidth default 20;
    property Height: Integer read FHeight write SetHeight default 20;
  end;

  TSCDeLayerProperties = class(TPersistent)
  private
    FColor: TColor;
    FWidth: Integer;
    FHeight: Integer;
    FOwner: TSCDeSurface;
    FCheckers: TSCDeCheckersProperties;
    FTransparency: TSCDeLayerTransparency;
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetCheckers(Value: TSCDeCheckersProperties);
    procedure SetTransparency(Value: TSCDeLayerTransparency);
  protected
    procedure Changed(Scrollbars: Boolean = False);
    procedure CheckersChanged;
  public
    constructor Create(AOwner: TSCDeSurface);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(AWidth, AHeight: Integer);
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Width: Integer read FWidth write SetWidth default 400;
    property Height: Integer read FHeight write SetHeight default 400;
    property Checkers: TSCDeCheckersProperties read FCheckers write SetCheckers;
    property Transparency: TSCDeLayerTransparency read FTransparency
      write SetTransparency default scltNone;
  end;

  TSCDeGridProperties = class(TPersistent)
  private
    FX: Integer;
    FY: Integer;
    FColor: TColor;
    FGridType: TSCDeGridType;
    FGroupColor: TColor;
    FGroupCount: Word;
    FOwner: TSCDeSurface;
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetGridType(Value: TSCDeGridType);
    procedure SetGroupColor(Value: TColor);
    procedure SetGroupCount(Value: Word);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TSCDeSurface);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $00A6E4FF;
    property GridType: TSCDeGridType read FGridType write SetGridType default scgtLine;
    property GroupColor: TColor read FGroupColor write SetGroupColor default $003CC5FF;
    property GroupCount: Word read FGroupCount write SetGroupCount default 5;
    property X: Integer read FX write SetX default 20;
    property Y: Integer read FY write SetY default 20;
  end;

  TSCDeFrameProperties = class(TPersistent)
  private
    FColor: TColor;
    FShadowColor: TColor;
    FShadowOffset: Integer;
    FVisible: Boolean;
    FOwner: TSCDeSurface;
    procedure SetColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowOffset(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Changed(Force: Boolean = False);
  public
    constructor Create(AOwner: TSCDeSurface);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clNone;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 3;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TSCDeHitTest = record
    Test: TDoublePoint;
    Shape: TSCDeShapeBase;
    HitType: TSCDeHitType;
    State: TSCDeShapeHitState;
    PartIndex: Integer;
    SubIndex: Integer;
  end;

  TSCDeSurface = class(TSCDeCustomScrollControl)
  private
    FLayers: TList;
    FForced: Boolean;
    FZoom: Word;
    FInLoad: Boolean;
    FBufferedPaint: Boolean;
    FActiveLayer: TSCDeLayer;
    FClientBuffer: TSCDeBitmap;
    FLayerBuffer: TSCDeBitmap;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FUpdatingBuffer: Integer;
    FReupdateBuffers: Boolean;
    FHorizontalPos: Integer;
    FVerticalPos: Integer;
    FShowScrollbars: Boolean;
    FCreatingWnd: Integer;
    FScrollbarUpdate: Integer;
    FScrollbarChanging: Integer;
    FScrollPosChanging: Integer;
    FFrameProperties: TSCDeFrameProperties;
    FGridProperties: TSCDeGridProperties;
    FLayerProperties: TSCDeLayerProperties;
    FClients: TList;
    FOnActiveLayerChange: TNotifyEvent;
    FOnInsertLayer: TSCDeShapeEvent;
    FOnInsertShape: TSCDeShapeEvent;
    FOnRemoveLayer: TSCDeShapeEvent;
    FOnRemoveShape: TSCDeShapeEvent;
    FOnLayerChange: TSCDeShapeEvent;
    FOnClearShape: TSCDeShapeEvent;
    FOnFramePropsChange: TNotifyEvent;
    FOnGridPropsChange: TNotifyEvent;
    FOnLayerPropsChange: TNotifyEvent;
    FOnZoom: TNotifyEvent;
    FOnBeforeLoadDOM: TSCDeSaveLoadEvent;
    FOnAfterLoadDOM: TSCDeSaveLoadEvent;
    FOnBeforeSaveDOM: TSCDeSaveLoadEvent;
    FOnAfterSaveDOM: TSCDeSaveLoadEvent;
    procedure SetZoom(Value: Word);
    procedure SetActiveLayer(Value: TSCDeLayer);
    procedure SetBufferedPaint(Value: Boolean);
    procedure SetGridProperties(Value: TSCDeGridProperties);
    procedure SetLayerProperties(Value: TSCDeLayerProperties);
    function  GetLayer(Index: Integer): TSCDeLayer;
    function  GetLayerCount: Integer;
    procedure SetHorizontalPos(Value: Integer);
    procedure SetVerticalPos(Value: Integer);
    procedure SetShowScrollbars(Value: Boolean);
    procedure SetFrameProperties(const Value: TSCDeFrameProperties);
    function  GetClientCount: Integer;
    function  GetClient(Index: Integer): TSCDeSurfaceClient;

    function  ReArrangeActiveLayer(Ly: TSCDeLayer): TSCDeLayer;
    procedure ActiveLayerChanged;

    procedure FramePropsChanged;
    procedure GridPropsChanged;
    procedure LayerPropsChanged(Scrollbars: Boolean);

    procedure UpdateLayerBuffer;
    procedure RefreshBuffers(WithLayer, LockLayer: Boolean);

    function  GetNeedsUpdate: Boolean;

    function  GetLayerCanvas: TCanvas;
    function  GetClientCanvas: TCanvas;

    procedure BeforeLoadDOM(Root: TSCDomElement);
    procedure AfterLoadDOM(Root: TSCDomElement);
    procedure BeforeSaveDOM(Root: TSCDomElement);
    procedure AfterSaveDOM(Root: TSCDomElement);

    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    procedure Paint; override;
    procedure BeforePaint(DC: HDC); virtual;
    procedure AfterPaint(DC: HDC); virtual;
    procedure CreateWnd; override;
    procedure Loaded; override;

    procedure UnRegisterAllClients;

    procedure DoPictureListChanged; override;
    procedure DoPictureChanged; override;
    function  GetPictureRect(P: TPicture): TRect; override;

    procedure UpdateScrollBars; virtual;
    procedure DoScrollerPositionChanged(Kind: TSCDeScrollbarKind); override;

    procedure ResetHittest(var Hit: TSCDeHitTest);
    procedure UpdateHitTestPos(var Hit: TSCDeHitTest; P: TDoublePoint);

    procedure SetDirectBufferedPaint(Value: Boolean);
    procedure UpdatePaintBuffer; dynamic;

    procedure IndentChanged; override;

    function  GetActiveLayer: TSCDeLayer;

    function  GetPaintRect: TRect; virtual;
    function  GetClientRect: TRect; override;
    function  GetLayerRect: TDoubleRect; virtual;
    function  GetNominalViewRect: TDoubleRect; dynamic;
    function  GetViewRect: TDoubleRect; dynamic;

    procedure InsertLayer(L: TSCDeLayer);
    procedure RemoveLayer(L: TSCDeLayer);

    procedure Changed(Force: Boolean = True);

    procedure StartAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;
    procedure EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;

    procedure NotifyUndo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); virtual;

    function  HasClipboardData: Boolean; dynamic;
    procedure CopyToClipboard; dynamic;
    procedure CutToClipboard; dynamic;
    procedure PasteFromClipboard; dynamic;

    procedure ShapeInserted(S: TSCDeShapeBase);
    procedure ShapeRemoved(S: TSCDeShapeBase);
    procedure ShapeCleared(S: TSCDeShapeBase);
    procedure LayerInserted(L: TSCDeLayer);
    procedure LayerChanged(L: TSCDeLayer);
    procedure LayerRemoved(L: TSCDeLayer);

    procedure ValidateLayerOffset(var AHorz, AVert: Integer);
    procedure LayerBounds(var CR: TRect; var R: TDoubleRect); virtual;

    procedure DoActiveLayerChanged; virtual;

    procedure DoShapeInserted(S: TSCDeShapeBase); virtual;
    procedure DoShapeRemoved(S: TSCDeShapeBase); virtual;
    procedure DoLayerInserted(L: TSCDeLayer); virtual;
    procedure DoLayerChanged(L: TSCDeLayer); virtual;
    procedure DoLayerRemoved(L: TSCDeLayer); virtual;
    procedure DoShapeCleared(S: TSCDeShapeBase); virtual;

    procedure RefineSelections; dynamic;

    procedure DrawSelection(C: TCanvas); virtual;
    procedure DrawLayerFrame(C: TCanvas); virtual;
    procedure BeforeLayerPaint(C: TCanvas); virtual;
    procedure AfterLayerPaint(C: TCanvas); virtual;
    procedure BeforeUpdateBuffers(C: TCanvas); virtual;
    procedure AfterUpdateBuffers(C: TCanvas); virtual;

    function  GetControlScrollbarsClass: TSCDeControlCustomScrollbarsClass; override;
    function  GetScrollbarClass: TSCDeCustomControlScrollbarClass; override;
    function  GetBorderPropsClass: TSCDeControlBorderPropsClass; override;

    procedure DoBeforeLoadDOM(Root: TSCDomElement); dynamic;
    procedure DoAfterLoadDOM(Root: TSCDomElement); dynamic;
    procedure DoBeforeSaveDOM(Root: TSCDomElement); dynamic;
    procedure DoAfterSaveDOM(Root: TSCDomElement); dynamic;

    function  GetSelectionBounds: TDoubleRect; dynamic;

    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TSCDeSurfaceClient read GetClient;

    property ParentColor default False;
    property Color default clGray;
    property Border default scdcb3DLowered;
    property Indent default 20;
    property InLoad: Boolean read FInLoad;
    property Layers[Index: Integer]: TSCDeLayer read GetLayer;
    property Grid: TSCDeGridProperties read FGridProperties write SetGridProperties;
    property Layer: TSCDeLayerProperties read FLayerProperties write SetLayerProperties;
    property Frame: TSCDeFrameProperties read FFrameProperties write SetFrameProperties;
    property LayerCanvas: TCanvas read GetLayerCanvas;
    property ClientCanvas: TCanvas read GetClientCanvas;
    property BufferedPaint: Boolean read FBufferedPaint write SetBufferedPaint default False;
    property ActiveLayer: TSCDeLayer read FActiveLayer write SetActiveLayer;
    property HorizontalPos: Integer read FHorizontalPos write SetHorizontalPos default 0;
    property VerticalPos: Integer read FVerticalPos write SetVerticalPos default 0;
    property ShowScrollbars: Boolean read FShowScrollbars write SetShowScrollbars default True;
    property Zoom: Word read FZoom write SetZoom default 100;
    property OnActiveLayerChange: TNotifyEvent read FOnActiveLayerChange write FOnActiveLayerChange;
    property OnInsertLayer: TSCDeShapeEvent read FOnInsertLayer write FOnInsertLayer;
    property OnInsertShape: TSCDeShapeEvent read FOnInsertShape write FOnInsertShape;
    property OnRemoveLayer: TSCDeShapeEvent read FOnRemoveLayer write FOnRemoveLayer;
    property OnRemoveShape: TSCDeShapeEvent read FOnRemoveShape write FOnRemoveShape;
    property OnLayerChange: TSCDeShapeEvent read FOnLayerChange write FOnLayerChange;
    property OnClearShape: TSCDeShapeEvent read FOnClearShape write FOnClearShape;
    property OnFramePropsChange: TNotifyEvent read FOnFramePropsChange write FOnFramePropsChange;
    property OnGridPropsChange: TNotifyEvent read FOnGridPropsChange write FOnGridPropsChange;
    property OnLayerPropsChange: TNotifyEvent read FOnLayerPropsChange write FOnLayerPropsChange;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnBeforeLoadDOM: TSCDeSaveLoadEvent read FOnBeforeLoadDOM write FOnBeforeLoadDOM;
    property OnAfterLoadDOM: TSCDeSaveLoadEvent read FOnAfterLoadDOM write FOnAfterLoadDOM;
    property OnBeforeSaveDOM: TSCDeSaveLoadEvent read FOnBeforeSaveDOM write FOnBeforeSaveDOM;
    property OnAfterSaveDOM: TSCDeSaveLoadEvent read FOnAfterSaveDOM write FOnAfterSaveDOM;
  public
    constructor Create(AOwner: TComponent); override;
    procedure  BeforeDestruction; override;
    destructor Destroy; override;

    procedure RegisterClient(Client: TSCDeSurfaceClient);
    procedure UnRegisterClient(Client: TSCDeSurfaceClient);

    procedure Add(L: TSCDeLayer);
    procedure Insert(Index: Integer; L: TSCDeLayer);
    procedure Delete(Index: Integer);
    procedure Remove(L: TSCDeLayer);
    procedure Clear;
    function  IndexOf(L: TSCDeLayer): Integer;
    function  Exists(S: TSCDeShapeBase): Boolean;

    function  HandleActiveLayer: TSCDeLayer;

    function  Pack(A: array of TSCDeShapeBase): TSCDePackage; dynamic;
    procedure Unpack(P: TSCDePackage); dynamic;

    function  Group(A: array of TSCDeShapeBase): TSCDeGroup; dynamic;
    procedure Ungroup(G: TSCDeGroup); dynamic;

    procedure BringToFront(L: TSCDeLayer);
    procedure SendToBack(L: TSCDeLayer);

    procedure BeginUpdate;
    procedure EndUpdate;

    function  ScreenToWorld(P: TDoublePoint): TDoublePoint; dynamic;
    function  ShapeAtPos(P: TDoublePoint; Fuzz: Byte): TSCDeShapeBase;
    function  HitTest(P: TDoublePoint): TSCDeHitTest; dynamic;

    function  GetZoom: Double;
    procedure ZoomRect(R: TDoubleRect); dynamic;

    procedure SetLayerOffset(AHorz, AVert: Integer);

    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile(const FileName: String); override;

    property PaintRect: TRect read GetPaintRect;
    property LayerCount: Integer read GetLayerCount;
    property LayerRect: TDoubleRect read GetLayerRect;
    property UpdateCount: Cardinal read FUpdateCount;
    property NeedsUpdate: Boolean read GetNeedsUpdate;
  end;

  TSCDeCustomViewer = class(TSCDeSurface)
  private
    FDownShape: TSCDeShapeBase;
    FHotShape: TSCDeShapeBase;
    FOnShapeDown: TSCDeShapeEvent;
    FOnShapeMove: TSCDeShapeEvent;
    FOnShapeUp: TSCDeShapeEvent;
    FOnShapeClick: TSCDeShapeEvent;
  protected
    procedure DoShapeRemoved(S: TSCDeShapeBase); override;
    procedure DoLayerRemoved(L: TSCDeLayer); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    property HotShape: TSCDeShapeBase read FHotShape;
    property DownShape: TSCDeShapeBase read FDownShape;

    property OnShapeDown: TSCDeShapeEvent read FOnShapeDown write FOnShapeDown;
    property OnShapeMove: TSCDeShapeEvent read FOnShapeMove write FOnShapeMove;
    property OnShapeUp: TSCDeShapeEvent read FOnShapeUp write FOnShapeUp;
    property OnShapeClick: TSCDeShapeEvent read FOnShapeClick write FOnShapeClick;
  end;

  TSCDeViewer = class(TSCDeCustomViewer)
  public
    property Layers;
    property HorizontalPos;
    property VerticalPos;
    property InLoad;
    property HotShape;
    property DownShape;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderProps;
    property Color nodefault;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Frame;
    property Grid;
    property Indent;
    property Layer;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property Picture;
    property PictureProps;
    property PopupMenu;
    property Scrollbars;
    property ShowHint;
    property ShowScrollbars;
    property TabOrder;
    property TabStop;
    property Visible;
    property Zoom;
    property OnBeforeLoadDOM;
    property OnAfterLoadDOM;
    property OnBeforeSaveDOM;
    property OnAfterSaveDOM;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFramePropsChange;
    property OnGetSiteInfo;
    property OnGridPropsChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayerPropsChange;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnScroll;
    property OnScrollChangeEvent;
    property OnShapeDown;
    property OnShapeUp;
    property OnShapeClick;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnZoom;
  end;

  TSCDeRegister = class
  public
    class function  RegisterShape(AShape: TSCDeShapeClass): Boolean;
    class procedure UnRegisterShape(AShape: TSCDeShapeClass);
    class function  FindShape(const AShapeName: string): TSCDeShapeClass;
  end;

implementation

var
  ClassList: TStringList;
  
type
  TSCFakePicture = class(TPicture);

  TPictureWriter = class(TWriter)
  public
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
  end;

  TPictureReader = class(TReader)
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream; BufSize: Integer);

    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
  end;

{ TPictureWriter }

procedure TPictureWriter.DefineBinaryProperty(const Name: string; ReadData,
  WriteData: TStreamProc; HasData: Boolean);
begin
  if HasData and Assigned(WriteData) then
    WriteBinary(WriteData);
end;

{ TPictureReader }

constructor TPictureReader.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FStream := Stream;
end;

procedure TPictureReader.DefineBinaryProperty(const Name: string; ReadData,
  WriteData: TStreamProc; HasData: Boolean);
var
  Count: Longint;
  Stream: TMemoryStream;
begin
  if Assigned(ReadData) and (ReadValue = vaBinary) then
  begin
    Stream := TMemoryStream.Create;
    try
      Read(Count, SizeOf(Longint));
      Stream.SetSize(Count);
      Read(Stream.Memory^, Count);
      ReadData(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

{ TSCDePointControl }

procedure TSCDePointControl.Assign(Source: TPersistent);
begin
  if Source is TSCDePointControl then
  begin
    with TSCDePointControl(Source) do
    begin
      Self.Fx := x;
      Self.Fy := y;
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDePointControl.BeforeDestruction;
begin
  Destroying;
  inherited BeforeDestruction;
end;

procedure TSCDePointControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDePointControl.Changed;
begin
  if not FDestroying then
  begin
    if FUpdateCount > 0 then
    begin
      Inc(FChangeCount);
      Exit;
    end;

    FChangeCount := 0;
    if FOwner <> nil then FOwner.DoControlChanged(Self);
  end;
end;

constructor TSCDePointControl.Create;
begin
  inherited Create;
  Fx := 0.0;
  Fy := 0.0;
end;

destructor TSCDePointControl.Destroy;
begin
  if FOwner <> nil then FOwner.RemoveControl(Self);
  inherited Destroy;
end;

procedure TSCDePointControl.Destroying;
begin
  FDestroying := True;
end;

procedure TSCDePointControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FChangeCount > 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TSCDePointControl.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDePointControl.GetValue: TDoublePoint;
begin
  Result := TSCDeUtils.Point(Fx, Fy);
end;

function TSCDePointControl.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDePointControl.SetPosition(Ax, Ay: Double);
begin
  if (Fx <> Ax) or (Ay <> Fy) then
  begin
    Changing;
    Fx := Ax;
    Fy := Ay;
    Changed;
  end;
end;

procedure TSCDePointControl.SetValue(Value: TDoublePoint);
begin
  SetPosition(Value.x, Value.y);
end;

procedure TSCDePointControl.Setx(Value: Double);
begin
  SetPosition(Value, Fy);
end;

procedure TSCDePointControl.Sety(Value: Double);
begin
  SetPosition(Fx, Value);
end;

procedure TSCDePointControl.Changing;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.DoControlChanging(Self);
end;

{ TSCDePoint }

procedure TSCDePoint.Assign(Source: TPersistent);
begin
  if Source is TSCDePoint then
  begin
    with TSCDePoint(Source) do
    begin
      Self.Fx := x;
      Self.Fy := y;
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDePoint.BeforeDestruction;
begin
  Destroying;
  inherited BeforeDestruction;
end;

procedure TSCDePoint.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDePoint.Changed;
begin
  if not FDestroying then
  begin
    if FUpdateCount > 0 then
    begin
      Inc(FChangeCount);
      Exit;
    end;

    FChangeCount := 0;
    if FOwner <> nil then FOwner.DoPointChanged(Self);
  end;
end;

procedure TSCDePoint.ClearControls;
begin
  if FControl_1 <> nil then FreeAndNil(FControl_1);
  if FControl_2 <> nil then FreeAndNil(FControl_2);
end;

procedure TSCDePoint.ControlAdded(C: TSCDePointControl);
begin
  //
end;

procedure TSCDePoint.ControlChanged(C: TSCDePointControl);
begin
  //
end;

procedure TSCDePoint.ControlRemoved(C: TSCDePointControl);
begin
  //
end;

constructor TSCDePoint.Create(AOwner: TSCDeShapeBase);
begin
  inherited Create;
  if AOwner <> nil then AOwner.InsertPoint(Self);
  Fx := 0.0;
  Fy := 0.0;
end;

destructor TSCDePoint.Destroy;
begin
  ClearControls;
  if FOwner <> nil then FOwner.RemovePoint(Self);
  inherited Destroy;
end;

procedure TSCDePoint.Destroying;
begin
  FDestroying := True;
end;

procedure TSCDePoint.DoControlAdded(C: TSCDePointControl);
begin
  ControlAdded(C);
  Changed;
end;

procedure TSCDePoint.DoControlChanged(C: TSCDePointControl);
begin
  ControlChanged(C);
  Changed;
end;

procedure TSCDePoint.DoControlRemoved(C: TSCDePointControl);
begin
  ControlRemoved(C);
  Changed;
end;

procedure TSCDePoint.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FChangeCount > 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TSCDePoint.GetControlClass: TSCDePointControlClass;
begin
  Result := TSCDePointControl;
end;

function TSCDePoint.GetIndex: Integer;
begin
  Result := -1;
  if FOwner <> nil then Result := FOwner.IndexOf(Self);
end;

function TSCDePoint.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDePoint.GetValue: TDoublePoint;
begin
  Result := TSCDeUtils.Point(Fx, Fy);
end;

function TSCDePoint.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TSCDePoint.NewControl(x, y: Double): TSCDePointControl;
begin
  Result := GetControlClass.Create;
  Result.SetPosition(x, y);
end;

procedure TSCDePoint.RemoveControl(C: TSCDePointControl);
begin
  if (C <> nil) and (C.FOwner = Self) then
  begin
    C.FOwner := nil;

    if C = FControl_1 then FControl_1 := nil;
    if C = FControl_2 then FControl_2 := nil;

    DoControlRemoved(C);
  end;
end;

procedure TSCDePoint.SetPosition(Ax, Ay: Double);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, Ax, Ay);

  if (Fx <> Ax) or (Ay <> Fy) then
  begin
    StartAction;
    NotifyUndo;

    Changing;
    Fx := Ax;
    Fy := Ay;
    Changed;

    EndAction;
  end;
end;

procedure TSCDePoint.SetValue(Value: TDoublePoint);
begin
  SetPosition(Value.x, Value.y);
end;

procedure TSCDePoint.Setx(Value: Double);
begin
  SetPosition(Value, Fy);
end;

procedure TSCDePoint.Sety(Value: Double);
begin
  SetPosition(Fx, Value);
end;

function TSCDePoint.ControlAt(P: TDoublePoint; Fuzz: Double): Integer;
var
  Pt: TPoint;
  R: TDoubleRect;
  Po: TSCDePointControl;
begin
  Result := -1;

  if FControl_1 <> nil then
  begin
    Po := FControl_1;

    Pt.x := TSCDeUtils.Round(Fx + Po.x);
    Pt.y := TSCDeUtils.Round(Fy + Po.y);

    if (Pt.x = P.x) and (Pt.y = P.y) then
    begin
      Result := 0;
      Exit;
    end;

    if Fuzz > 0.0 then
    begin
      R.Left := Pt.x - (Fuzz / 2);
      R.Top  := Pt.y - (Fuzz / 2);

      R.Right  := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      if TSCDeUtils.PtInRect(R, P) then
      begin
        Result := 0;
        Exit;
      end;
    end;
  end;

  if FControl_2 <> nil then
  begin
    Po := FControl_2;

    Pt.x := TSCDeUtils.Round(Fx + Po.x);
    Pt.y := TSCDeUtils.Round(Fy + Po.y);

    if (Pt.x = P.x) and (Pt.y = P.y) then
    begin
      Result := 1;
      Exit;
    end;

    if Fuzz > 0.0 then
    begin
      R.Left := Pt.x - (Fuzz / 2);
      R.Top  := Pt.y - (Fuzz / 2);

      R.Right  := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      if TSCDeUtils.PtInRect(R, P) then
      begin
        Result := 1;
        Exit;
      end;
    end;
  end;
end;

function TSCDePoint.CanInsertControl(C: TSCDePointControl): Boolean;
begin
  Result := (C <> nil) and (C.FOwner <> Self);
end;

procedure TSCDePoint.SetControl_1(C: TSCDePointControl);
begin
  if (C <> FControl_1) and CanInsertControl(C) then
  begin
    StartAction;
    
    NotifyUndo;
    Changing;

    if C.FOwner <> nil then C.FOwner.RemoveControl(C);

    C.FOwner := Self;
    FControl_1 := C;

    DoControlAdded(C);
    EndAction;
  end;
end;

procedure TSCDePoint.SetControl_2(C: TSCDePointControl);
begin
  if (C <> FControl_2) and CanInsertControl(C) then
  begin
    if C.FOwner <> nil then C.FOwner.RemoveControl(C);

    C.FOwner := Self;
    FControl_2 := C;

    DoControlAdded(C);
  end;
end;

procedure TSCDePoint.ControlChanging(C: TSCDePointControl);
begin
  //
end;

procedure TSCDePoint.DoControlChanging(C: TSCDePointControl);
begin
  ControlChanging(C);
  Changing;
end;

procedure TSCDePoint.Changing;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.DoPointChanging(Self);
end;

procedure TSCDePoint.NotifyUndo;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.NotifyUndo(FOwner, scacSizing, '');
end;

procedure TSCDePoint.EndAction;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.EndAction(FOwner, scacSizing, '');
end;

procedure TSCDePoint.StartAction;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.StartAction(FOwner, scacSizing, '');
end;

{ TSCDeControl }

procedure TSCDeControl.Assign(Source: TPersistent);
begin
  if Source is TSCDeControl then
  begin
    with TSCDeControl(Source) do
    begin
      Self.Fx := x;
      Self.Fy := y;
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeControl.BeforeDestruction;
begin
  Destroying;
  inherited BeforeDestruction;
end;

procedure TSCDeControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeControl.Changed;
begin
  if not FDestroying then
  begin
    if FUpdateCount > 0 then
    begin
      Inc(FChangeCount);
      Exit;
    end;

    FChangeCount := 0;
    if FOwner <> nil then FOwner.DoControlChanged(Self);
  end;
end;

constructor TSCDeControl.Create(AOwner: TSCDeShapeBase);
begin
  inherited Create;
  if AOwner <> nil then AOwner.InsertControl(Self);
  Fx := 0.0;
  Fy := 0.0;
end;

destructor TSCDeControl.Destroy;
begin
  if FOwner <> nil then FOwner.RemoveControl(Self);
  inherited Destroy;
end;

procedure TSCDeControl.Destroying;
begin
  FDestroying := True;
end;

procedure TSCDeControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FChangeCount > 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TSCDeControl.GetIndex: Integer;
begin
  Result := -1;
  if FOwner <> nil then Result := FOwner.IndexOfControl(Self);
end;

function TSCDeControl.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeControl.GetValue: TDoublePoint;
begin
  Result := TSCDeUtils.Point(Fx, Fy);
end;

function TSCDeControl.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDeControl.SetPosition(Ax, Ay: Double);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, Ax, Ay);

  if (Fx <> Ax) or (Ay <> Fy) then
  begin
    StartAction;
    NotifyUndo;

    Changing;
    Fx := Ax;
    Fy := Ay;
    Changed;

    EndAction;
  end;
end;

procedure TSCDeControl.SetValue(Value: TDoublePoint);
begin
  SetPosition(Value.x, Value.y);
end;

procedure TSCDeControl.Setx(Value: Double);
begin
  SetPosition(Value, Fy);
end;

procedure TSCDeControl.Sety(Value: Double);
begin
  SetPosition(Fx, Value);
end;

procedure TSCDeControl.Changing;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.DoControlChanging(Self);
end;

procedure TSCDeControl.NotifyUndo;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.NotifyUndo(FOwner, scacSizing, '');
end;

procedure TSCDeControl.EndAction;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.EndAction(FOwner, scacSizing, '');
end;

procedure TSCDeControl.StartAction;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.StartAction(FOwner, scacSizing, '');
end;

{ TSCDeShapeBase }

procedure TSCDeShapeBase.BeginPointUpdate;
begin
  Inc(FPointUpdate);
end;

procedure TSCDeShapeBase.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeShapeBase.Changed(Force: Boolean);
begin
  if not FDestroying then
  begin
    try
      if FUpdateCount > 0 then
      begin
        Inc(FChangeCount);
        Exit;
      end;

      if FVisible or Force then
      begin
        FChangeCount := 0;
        FControlChange := 0;
        FPointChange := 0;

        UpdateRegion;

        DoChanged(Force);
        if FOwner <> nil then FOwner.DoShapeChanged(Self);
      end;
    finally
      NotifyChange;
    end;
  end;
end;

procedure TSCDeShapeBase.ClearPoints;
var
  I: Integer;
  P: TSCDePoint;
begin
  if FPoints.Count > 0 then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    BeginUpdate;
    try
      Inc(FChangeCount);

      for I := FPoints.Count-1 downto 0 do
      begin
        P := TSCDePoint(FPoints[I]);
        try
          P.FOwner := nil;
          FPoints.Delete(I);
        finally
          P.Free;
        end;
      end;
    finally
      EndUpdate;
    end;
    
    EndAction(Self, scacSizing, '');
  end;
end;

constructor TSCDeShapeBase.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  FID := TSCDeUtils.GenerateGlobalUniqueID;
  FTempID := FID;
  FColor := clLime;

  inherited Create;
  SetSurface(ASurface);

  FPoints := TList.Create;
  FControls := TList.Create;
  FSelectionList := TList.Create;

  FVisible := True;
  FPictureStyle := scdspStretch;
  FShapeStyle := FShapeStyle + [scssUsesPen, scssCanHottrack];

  FBrush := TSCDeShapeBrush.Create;
  FBrush.SetOwner(Self);
  FBrush.OnChanging := DoBrushChanging;
  FBrush.OnChange := DoBrushChanged;

  FFont := TSCDeShapeFont.Create;
  FFont.SetOwner(Self);
  FFont.OnChanging := DoFontChanging;
  FFont.OnChange := DoFontChanged;

  FPen := TSCDeShapePen.Create;
  FPen.SetOwner(Self);
  FPen.OnChanging := DoPenChanging;
  FPen.OnChange := DoPenChanged;

  FPicture := TPicture.Create;
  FPicture.OnChange := DoPictureChanged;

  if AOwner <> nil then AOwner.InsertShape(Self);
end;

destructor TSCDeShapeBase.Destroy;
begin
  ClearPoints;
  ClearControls;
  FreeAndNil(FPoints);
  FreeAndNil(FControls);
  FreeAndNil(FSelectionList);

  FBrush.OnChange := nil;
  FreeAndNil(FBrush);

  FFont.OnChange := nil;
  FreeAndNil(FFont);

  FPen.OnChange := nil;
  FreeAndNil(FPen);

  FPicture.OnChange := nil;
  FreeAndNil(FPicture);

  ReleaseGradient;
  inherited Destroy;
end;

procedure TSCDeShapeBase.Destroying;
begin
  FDestroying := True;
end;

procedure TSCDeShapeBase.DoChanged(Force: Boolean);
begin
  //
end;

procedure TSCDeShapeBase.DoPointChanged(P: TSCDePoint);
begin
  Inc(FPointChange);
  PointChanged(P);
  Changed;
end;

procedure TSCDeShapeBase.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
begin
  //
end;

procedure TSCDeShapeBase.EndPointUpdate;
begin
  if FPointUpdate > 0 then
  begin
    Dec(FPointUpdate);
    if (FPointUpdate = 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

procedure TSCDeShapeBase.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FChangeCount > 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TSCDeShapeBase.GetBounds: TDoubleRect;
var
  I: Integer;
  P: TSCDePoint;
begin
  Result := TSCDeUtils.Rect(0, 0, 0, 0);

  if FPoints.Count > 0 then
  begin
    P := TSCDePoint(FPoints[0]);

    Result.Left := P.x;
    Result.Right := P.x;
    Result.Top := P.y;
    Result.Bottom := P.y;

    for I := 1 to FPoints.Count-1 do
    begin
      P := TSCDePoint(FPoints[I]);

      if P.x < Result.Left then Result.Left := P.x;
      if P.x > Result.Right then Result.Right := P.x;
      if P.y < Result.Top then Result.Top := P.y;
      if P.y > Result.Bottom then Result.Bottom := P.y;
    end;
  end;
end;

function TSCDeShapeBase.GetSurface: TSCDeSurface;
var
  O: TSCDeContainer;
begin
  Result := FSurface;

  if Result = nil then
  begin
    O := FOwner;
    
    while O <> nil do
    begin
      if O is TSCDeLayer then
      begin
        Result := TSCDeLayer(O).Surface;
        Exit;
      end;

      O := O.FOwner;
    end;
  end;
end;

procedure TSCDeShapeBase.GradientChanged(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.GradientChanging(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.CreateGradient;
begin
  if FGradient = nil then
  begin
    FGradient := TSCDeShapeGradient.Create;
    FGradient.SetOwner(Self);

    FGradient.OnChanging := DoGradientChanging;
    FGradient.OnChange := DoGradientChanged;
  end;
end;

function TSCDeShapeBase.GetIndex: Integer;
begin
  Result := -1;
  if FOwner <> nil then Result := FOwner.IndexOf(Self);
end;

function TSCDeShapeBase.GetInPointUpdate: Boolean;
begin
  Result := FPointUpdate > 0;
end;

function TSCDeShapeBase.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeShapeBase.GetPoint(Index: Integer): TSCDePoint;
begin
  Result := nil;
  if (Index > -1) and (Index < FPoints.Count) then
    Result := TSCDePoint(FPoints[Index])
end;

function TSCDeShapeBase.GetPointCount: Integer;
begin
  Result := FPoints.Count;
end;

function TSCDeShapeBase.IndexOf(P: TSCDePoint): Integer;
begin
  Result := -1;
  if (P <> nil) and (P.FOwner = Self) then
    Result := FPoints.IndexOf(P);
end;

procedure TSCDeShapeBase.InsertPoint(P: TSCDePoint);
begin
  if (P <> nil) and (P.FOwner <> Self) and CanInsertPoint(P) then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    if P.FOwner <> nil then P.FOwner.RemovePoint(P);
    P.FOwner := Self;
    FPoints.Add(P);

    P.OnChanging := DoPointIsChanging;

    DoPointAdded(P);
    Changed;

    EndAction(Self, scacSizing, '');
  end;
end;

function TSCDeShapeBase.IsParent(S: TSCDeShapeBase): Boolean;
var
  O: TSCDeShapeBase;
begin
  Result := False;
  if (S <> nil) and (S <> Self) and (S.GetSurface = Self.GetSurface) then
  begin
    O := Self.Owner;
    while O <> nil do
    begin
      if O = S then
      begin
        Result := True;
        Exit;
      end;

      O := O.Owner;
    end;
  end;
end;

procedure TSCDeShapeBase.MoveBy(X, Y: Double);
var
  I: Integer;
  P: TSCDePoint;
begin
  if not InPointUpdate and (PointCount > 0) and ((X <> 0) or (Y <> 0)) then
  begin
    NotifyChanging;
    StartAction(Self, scacSizing, '');

    BeginNotifyLock;
    try
      BeginUpdate;
      try
        BeginPointUpdate;
        try
          NotifyUndo(Self, scacSizing, '');

          for I := 0 to FPoints.Count-1 do
          begin
            P := TSCDePoint(FPoints[I]);
            P.SetPosition(P.x + X, P.y + Y);
          end;
        finally
          EndPointUpdate;
        end;
      finally
        EndUpdate;
      end;
    finally
      EndNotifyLock;
    end;

    EndAction(Self, scacSizing, '');
  end;
end;

procedure TSCDeShapeBase.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
begin
  //
end;

procedure TSCDeShapeBase.PointAdded(P: TSCDePoint);
begin
  //
end;

procedure TSCDeShapeBase.PointChanged(P: TSCDePoint);
begin
  //
end;

function TSCDeShapeBase.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R: TDoubleRect;
begin
  if Fuzz < 0.0 then Fuzz := 0.0;

  R := GetBounds;
  TSCDeUtils.InflateRect(R, Fuzz, Fuzz);

  Result := not TSCDeUtils.IsRectEmpty(R) and
    TSCDeUtils.PtInRect(R, P);
end;

procedure TSCDeShapeBase.PointRemoved(P: TSCDePoint);
begin
  //
end;

procedure TSCDeShapeBase.RemovePoint(P: TSCDePoint);
begin
  if (P <> nil) and (P.FOwner = Self) then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    P.OnChanging := nil;
    P.FOwner := nil;
    FPoints.Remove(P);

    DoPointRemoved(P);
    Changed;

    EndAction(Self, scacSizing, '');
  end;
end;

procedure TSCDeShapeBase.SetBounds(ALeft, ATop, AWidth, AHeight: Double);
var
  R: TDoubleRect;
begin
  if AWidth < 0 then
  begin
    AWidth := Abs(AWidth);
    ALeft := ALeft - AWidth;
  end;

  if AHeight < 0 then
  begin
    AHeight := Abs(AHeight);
    ATop := ATop + AHeight;
  end;

  if PointCount > 0 then
  begin
    R := GetBounds;

    if (ALeft <> R.Left) or (ATop <> R.Top) or
      (AWidth <> R.Right - R.Left) or (AHeight <> R.Bottom - R.Top) then
    begin
      StartAction(Self, scacSizing, '');
      NotifyChanging;

      BeginNotifyLock;
      try
        BeginUpdate;
        try
          NotifyUndo(Self, scacSizing, '');

          MoveBy(ALeft - R.Left, ATop - R.Top);
          Resize(AWidth, AHeight);
        finally
          EndUpdate;
        end;
      finally
        EndNotifyLock;
      end;

      EndAction(Self, scacSizing, '');
    end;
  end;
end;

procedure TSCDeShapeBase.SetBounds(R: TDoubleRect);
begin
  if R.Left > R.Right then TSCDeUtils.Swap(R.Left, R.Right);
  if R.Top > R.Bottom then TSCDeUtils.Swap(R.Top, R.Bottom);

  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
end;

procedure TSCDeShapeBase.SetName(const Value: String);
begin
  if FName <> Value then
  begin
    StartAction(Self, scacChanging, 'name');
    NotifyUndo(Self, scacChanging, 'name');

    FName := Value;

    EndAction(Self, scacChanging, 'name');
  end;
end;

procedure TSCDeShapeBase.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    StartAction(Self, scacChanging, 'visible');
    NotifyUndo(Self, scacChanging, 'visible');

    FVisible := Value;
    Changed(True);

    if FSurface <> nil then
      FSurface.RefineSelections;

    EndAction(Self, scacChanging, 'visible');
  end;
end;

function TSCDeShapeBase.GetShapeType: TSCShapeType;
begin
  Result := sctyRectangle;
end;

procedure TSCDeShapeBase.AddSelection(P: TSCDePoint);
begin
  if (P <> nil) and (P.FOwner = Self) and
    (FSelectionList.IndexOf(P) = -1) then
  begin
    FSelectionList.Add(P);
    SelectionChanged;
  end;
end;

procedure TSCDeShapeBase.AddSelections(A: array of TSCDePoint);
var
  I: Integer;
  P: TSCDePoint;
  ListChanged: Boolean;
begin
  if Length(A) > 0 then
  begin
    ListChanged := False;

    for I := Low(A) to High(A) do
    begin
      P := A[I];
      if (P <> nil) and (P.FOwner = Self) and
        (FSelectionList.IndexOf(P) = -1) then
      begin
        FSelectionList.Add(P);
        ListChanged := True;
      end;
    end;

    if ListChanged then SelectionChanged;
  end;
end;

procedure TSCDeShapeBase.ClearSelection;
begin
  if FSelectionList.Count > 0 then
  begin
    FSelectionList.Clear;
    SelectionChanged;
  end;
end;

function TSCDeShapeBase.GetSelection(Index: Integer): TSCDePoint;
begin
  Result := TSCDePoint(FSelectionList[Index]);
end;

function TSCDeShapeBase.GetSelectionCount: Integer;
begin
  Result := FSelectionList.Count;
end;

procedure TSCDeShapeBase.GetSelections(L: TList);
var
  I: Integer;
begin
  L.Clear;
  for I := 0 to FSelectionList.Count-1 do
    L.Add(FSelectionList[I]);
end;

function TSCDeShapeBase.IsSelected(P: TSCDePoint): Boolean;
begin
  Result := not Self.Locked and (P <> nil) and
    (FSelectionList.IndexOf(P) > -1);
end;

procedure TSCDeShapeBase.RemoveSelection(P: TSCDePoint);
var
  Index: Integer;
begin
  if (P <> nil) and (P.FOwner = Self) then
  begin
    Index := FSelectionList.IndexOf(P);

    if Index > -1 then
    begin
      FSelectionList.Delete(Index);
      SelectionChanged;
    end;
  end;
end;

procedure TSCDeShapeBase.RemoveSelections(A: array of TSCDePoint);
var
  P: TSCDePoint;
  I, Index: Integer;
  ListChanged: Boolean;
begin
  if Length(A) > 0 then
  begin
    ListChanged := False;

    for I := Low(A) to High(A) do
    begin
      P := A[I];
      if (P <> nil) and (P.FOwner = Self) then
      begin
        Index := FSelectionList.IndexOf(P);

        if Index > -1 then
        begin
          FSelectionList.Delete(Index);
          ListChanged := True;
        end;
      end;
    end;

    if ListChanged then SelectionChanged;
  end;
end;

procedure TSCDeShapeBase.SelectAll;
var
  I: Integer;
begin
  FSelectionList.Clear;

  for I := 0 to FPoints.Count-1 do
    FSelectionList.Add(TSCDePoint(FPoints[I]));

  SelectionChanged;
end;

procedure TSCDeShapeBase.UpdateSelection;
var
  I, Cnt: Integer;
begin
  Cnt := FSelectionList.Count;
  if Cnt > 0 then
  begin
    for I := FSelectionList.Count-1 downto 0 do
      if not Exists(TSCDePoint(FSelectionList[I])) then
        FSelectionList.Delete(I);

    if Cnt <> FSelectionList.Count then
      SelectionChanged;
  end;
end;

procedure TSCDeShapeBase.DoPointRemoved(P: TSCDePoint);
begin
  Inc(FPointChange);

  FSelectionList.Remove(P);
  PointRemoved(P);
end;

procedure TSCDeShapeBase.SelectionChanged;
begin
  Changed;
end;

function TSCDeShapeBase.Exists(P: TSCDePoint): Boolean;
begin
  Result := IndexOf(P) > -1;
end;

procedure TSCDeShapeBase.SetLocked(Value: Boolean);
begin
  if FLocked <> Value then
  begin
    FLocked := Value;
    Changed;

    if FSurface <> nil then
      FSurface.RefineSelections;
  end;
end;

function TSCDeShapeBase.GetRegion: HRgn;
begin
  Result := 0;
end;

procedure TSCDeShapeBase.UpdateRegion;
begin
  //
end;

procedure TSCDeShapeBase.Resize(NewWidth, NewHeight: Double);
var
  I: Integer;
  P: TSCDePoint;
  R: TDoubleRect;
  W, H, X, Y, ZoomX, ZoomY: Double;
begin
  if not InPointUpdate and (PointCount > 1) then
  begin
    R := GetBounds;

    W := R.Right - R.Left;
    if W < 0.0 then W := 0.0;

    H := R.Bottom - R.Top;
    if H < 0.0 then H := 0.0;

    if NewWidth < 0.0 then NewWidth := W;
    if NewHeight < 0.0 then NewHeight := H;

    if (W <> NewWidth) or (H <> NewHeight) then
    begin
      ZoomX := 1.0;
      if W <> 0 then ZoomX := NewWidth / W;

      ZoomY := 1.0;
      if H <> 0 then ZoomY := NewHeight / H;

      StartAction(Self, scacSizing, '');
      NotifyChanging;

      BeginNotifyLock;
      try
        BeginUpdate;
        try
          BeginPointUpdate;
          try
            NotifyUndo(Self, scacSizing, '');

            for I := 0 to PointCount-1 do
            begin
              P := Points[I];

              X := R.Left + (ZoomX * (P.x - R.Left));
              Y := R.Top + (ZoomY * (P.y - R.Top));

              P.SetPosition(X, Y);
            end;
          finally
            EndPointUpdate;
          end;
        finally
          EndUpdate;
        end;
      finally
        EndNotifyLock;
      end;

      EndAction(Self, scacSizing, '');
    end;
  end;
end;

procedure TSCDeShapeBase.ResizeBy(DifX, DifY: Double);
var
  R: TDoubleRect;
  NewWidth, NewHeight: Double;
begin
  if not InPointUpdate and ((DifX <> 0) or (DifY <> 0)) then
  begin
    R := GetBounds;

    NewWidth := R.Right - R.Left;
    if NewWidth < 0 then NewWidth := 0;

    NewHeight := R.Bottom - R.Top;
    if NewHeight < 0 then NewHeight := 0;

    NewWidth  := NewWidth + DifX;
    NewHeight := NewHeight + DifY;

    Self.Resize(NewWidth, NewHeight);
  end;
end;

procedure TSCDeShapeBase.DoPointAdded(P: TSCDePoint);
begin
  Inc(FPointChange);
  PointAdded(P);
end;

procedure TSCDeShapeBase.Rotate(Angle: Double);
var
  R: TDoubleRect;
  P: TDoublePoint;
begin
  R := GetBounds;
  if R.Right < R.Left then TSCDeUtils.Swap(R.Left, R.Right);
  if R.Bottom < R.Top then TSCDeUtils.Swap(R.Bottom, R.Top);

  P := TSCDeUtils.Point(R.Left + (R.Right - R.Left) / 2,
    R.Top + (R.Bottom - R.Top) / 2);

  Rotate(P, Angle);  
end;

procedure TSCDeShapeBase.Rotate(GravityCenter: TDoublePoint; Angle: Double);
begin
  //
end;

function TSCDeShapeBase.InRect(const R: TDoubleRect): Boolean;
var
  I: Integer;
  P: TSCDePoint;
  DP1, DP2: TDoublePoint;
begin
  Result := False;

  if not TSCDeUtils.IsRectEmpty(R) and (FPoints.Count > 0) then
  begin
    for I := 0 to FPoints.Count-1 do
    begin
      P := TSCDePoint(FPoints[I]);

      DP1 := P.GetValue;
      Result := TSCDeUtils.PtInRect(R, DP1);

      if Result then Exit;

      if I < FPoints.Count-1 then
      begin
        P := TSCDePoint(FPoints[I + 1]);
        DP2 := P.GetValue;

        Result := TSCDeUtils.DoLineIntersectRect(DP1, DP2, R);
        if Result then Exit;
      end;
    end;
  end;
end;

procedure TSCDeShapeBase.DrawSelectionIn(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  R: TDoubleRect; Zoom: Double);
begin
  if not TSCDeUtils.IsRectEmpty(R) and Self.InRect(R) then
    Self.DrawSelection(C, X, Y, DragPoint, Data, Zoom);
end;

procedure TSCDeShapeBase.PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
  R: TDoubleRect);
var
  DR: TDoubleRect;
begin
  if not TSCDeUtils.IsRectEmpty(R) then
  begin
    DR := GetBounds;
    if Self.InRect(R) or TSCDeUtils.IntersectRect(DR, DR, R) then
      Self.Paint(C, X, Y, Zoom);
  end;
end;

function TSCDeShapeBase.PointAt(P: TDoublePoint; Fuzz: Double): Integer;
var
  I: Integer;
  Pt: TPoint;
  R: TDoubleRect;
  Po: TSCDePoint;
begin
  Result := -1;
  for I := PointCount-1 downto 0 do
  begin
    Po := Points[I];

    Pt.x := TSCDeUtils.Round(Po.x);
    Pt.y := TSCDeUtils.Round(Po.y);

    if (Pt.x = P.x) and (Pt.y = P.y) then
    begin
      Result := I;
      Exit;
    end;

    if Fuzz > 0 then
    begin
      R.Left := Pt.x - (Fuzz / 2);
      R.Top  := Pt.y - (Fuzz / 2);

      R.Right  := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      if TSCDeUtils.PtInRect(R, P) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

procedure TSCDeShapeBase.SetRegion(const Rgn: HRgn);
begin
  //
end;

function TSCDeShapeBase.DrawRect: TRect;
var
  O: TSCDeShapeBase;
begin
  Result := Rect(0, 0, 0, 0);

  O := Self;
  while O <> nil do
  begin
    if O is TSCDeLayer then
    begin
      if TSCDeLayer(O).Surface <> nil then
        with TSCDeLayer(O).Surface do
          Result := Rect(0, 0, Layer.Width, Layer.Height);

      Exit;
    end;

    O := O.Owner;
  end;
end;

procedure TSCDeShapeBase.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    StartAction(Self, scacChanging, 'text');
    NotifyUndo(Self, scacChanging, 'text');

    FText := Value;
    if scssUsesLabel in ShapeStyle then
      Changed;

    EndAction(Self, scacChanging, 'text');
  end;
end;

function TSCDeShapeBase.GetNeedsUpdate: Boolean;
begin
  Result := FChangeCount > 0;
end;

procedure TSCDeShapeBase.MoveTo(X, Y: Double);
var
  R: TDoubleRect;
begin
  R := GetBounds;
  if (R.Left <> X) or (R.Top <> Y) then
    MoveBy(X - R.Left, Y - R.Top);
end;

procedure TSCDeShapeBase.OffsetPosition(Index: Integer; Offset: TDoublePoint);
begin
  //
end;

function TSCDeShapeBase.IsSelected(Index: Integer): Boolean;
var
  I: Integer;
  P: TSCDePoint;
begin
  Result := False;
  if not Self.Locked and (Index > -1) and (Index < PointCount) then
    for I := 0 to FSelectionList.Count-1 do
    begin
      P := TSCDePoint(FSelectionList[I]);

      Result := P.Index = Index;
      if Result then
        Exit;
    end;
end;

function TSCDeShapeBase.GetCreationClicks: Integer;
begin
  Result := 0;
end;

function TSCDeShapeBase.GetLayer: TSCDeLayer;
var
  O: TSCDeShapeBase;
begin
  Result := nil;

  O := Self.Owner;
  while O <> nil do
  begin
    if O is TSCDeLayer then
    begin
      Result := TSCDeLayer(O);
      Exit;
    end;

    O := O.Owner;
  end;
end;

function TSCDeShapeBase.GetPointValue(Index: Integer): TDoublePoint;
begin
  Result := TSCDeUtils.Point(0, 0);
  if (Index > -1) and (Index < FPoints.Count) then
    Result := TSCDePoint(FPoints[Index]).GetValue;
end;

procedure TSCDeShapeBase.SetPointValue(Index: Integer;
  Value: TDoublePoint);
begin
  if (Index > -1) and (Index < FPoints.Count) then
    TSCDePoint(FPoints[Index]).SetValue(Value);
end;

procedure TSCDeShapeBase.SetShapeStyle(Value: TSCDeShapeStyles);
begin
  BeforeSetShapeStyle(Value);

  if FShapeStyle <> Value then
  begin
    FShapeStyle := Value;

    if not (scssUsesGradient in FShapeStyle) then
      ReleaseGradient
    else CreateGradient;

    ShapeStyleChanged;
  end;
end;

procedure TSCDeShapeBase.ShapeStyleChanged;
begin
  Changed;
end;

procedure TSCDeShapeBase.BeforeSetShapeStyle(var Value: TSCDeShapeStyles);
begin
  //
end;

function TSCDeShapeBase.IsOnLine(P: TDoublePoint): Boolean;
var
  I: Integer;
  Pt: TSCDePoint;
  Pts: TDPointArray;
  P1, P2: TDoublePoint;
begin
  Result := False;

  if PointCount > 1 then
  begin
    SetLength(Pts, PointCount + 1);

    Pt := TSCDePoint(Points[0]);

    P1.x := Pt.x;
    P1.y := Pt.y;

    Pts[PointCount] := P1;

    for I := 1 to PointCount-1 do
    begin
      Pt := Points[I];

      P2.x := Pt.x;
      P2.y := Pt.y;

      if (P1.x = P2.x) and (P1.y = P2.y) then
        Continue;

      Result := TSCDeUtils.NearLine(P, P1, P2, 2);
      if Result then Exit;

      P1 := P2;
      Pts[I] := P2;
    end;
  end;
end;

function TSCDeShapeBase.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDeShapeBase.BeforeDestruction;
begin
  Destroying;
  if FOwner <> nil then FOwner.RemoveShape(Self);

  NotifyDestroy;
  inherited BeforeDestruction;
end;

procedure TSCDeShapeBase.PictureChanged(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.SetBrush(Value: TSCDeShapeBrush);
begin
  FBrush.Assign(Value);
end;

procedure TSCDeShapeBase.SetPen(Value: TSCDeShapePen);
begin
  FPen.Assign(Value);
end;

procedure TSCDeShapeBase.SetPicture(Value: TPicture);
begin
  DoPictureChanging(FPicture);
  FPicture.Assign(Value);
end;

procedure TSCDeShapeBase.BrushChanged(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.DoBrushChanged(Sender: TObject);
begin
  BrushChanged(Sender);
  if scssUsesBrush in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.DoPenChanged(Sender: TObject);
begin
  PenChanged(Sender);
  if scssUsesPen in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.DoPictureChanged(Sender: TObject);
begin
  PictureChanged(Sender);
  if scssUsesPicture in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.PenChanged(Sender: TObject);
begin
  //
end;

function TSCDeShapeBase.GetDefaultBounds: TDoubleRect;
begin
  Result := TSCDeUtils.Rect(0, 0, 0, 0);
end;

procedure TSCDeShapeBase.SetFont(Value: TSCDeShapeFont);
begin
  FFont.Assign(Value);
end;

procedure TSCDeShapeBase.DoFontChanged(Sender: TObject);
begin
  FontChanged(Sender);
  if scssUsesFont in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.FontChanged(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.SetBrushRec(const B: TSCDeBrushRec);
begin
  FBrush.AssignRec(B);
end;

procedure TSCDeShapeBase.SetFontRec(const F: TSCDeShapeFontRec);
begin
  FFont.AssignRec(F);
end;

procedure TSCDeShapeBase.SetGradient(Value: TSCDeShapeGradient);
var
  AGradient: TSCDeShapeGradient;
begin
  AGradient := GetGradient;

  if AGradient <> nil then
  begin
    DoGradientChanging(FPicture);
    AGradient.Assign(Value);
  end;
end;

procedure TSCDeShapeBase.SetGradientRec(const G: TSCDeShapeGradienthRec);
var
  AGradient: TSCDeShapeGradient;
begin
  AGradient := GetGradient;
  if AGradient <> nil then AGradient.AssignRec(G);
end;

procedure TSCDeShapeBase.SetPenRec(const P: TSCDePenRec);
begin
  FPen.AssignRec(P);
end;

procedure TSCDeShapeBase.ClearControls;
var
  I: Integer;
  C: TSCDeControl;
begin
  if FControls.Count > 0 then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    BeginUpdate;
    try
      Inc(FChangeCount);

      for I := FControls.Count-1 downto 0 do
      begin
        C := TSCDeControl(FControls[I]);
        try
          C.FOwner := nil;
          FControls.Delete(I);
        finally
          C.Free;
        end;
      end;
    finally
      EndUpdate;
    end;

    EndAction(Self, scacSizing, '');
  end;
end;

procedure TSCDeShapeBase.ControlAdded(C: TSCDeControl);
begin
  //
end;

function TSCDeShapeBase.ControlAt(P: TDoublePoint; Fuzz: Double): Integer;
var
  I: Integer;
  Pt: TPoint;
  R: TDoubleRect;
  Po: TSCDeControl;
begin
  Result := -1;
  for I := ControlCount-1 downto 0 do
  begin
    Po := Controls[I];

    Pt.x := TSCDeUtils.Round(Po.x);
    Pt.y := TSCDeUtils.Round(Po.y);

    if (Pt.x = P.x) and (Pt.y = P.y) then
    begin
      Result := I;
      Exit;
    end;

    if Fuzz > 0.0 then
    begin
      R.Left := Pt.x - (Fuzz / 2);
      R.Top  := Pt.y - (Fuzz / 2);

      R.Right  := R.Left + Fuzz;
      R.Bottom := R.Top + Fuzz;

      if TSCDeUtils.PtInRect(R, P) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

procedure TSCDeShapeBase.ControlChanged(C: TSCDeControl);
begin
  //
end;

function TSCDeShapeBase.ControlExists(C: TSCDeControl): Boolean;
begin
  Result := IndexOfControl(C) > -1;
end;

procedure TSCDeShapeBase.ControlRemoved(C: TSCDeControl);
begin
  //
end;

procedure TSCDeShapeBase.DoControlAdded(C: TSCDeControl);
begin
  Inc(FControlChange);
  ControlAdded(C);
end;

procedure TSCDeShapeBase.DoControlChanged(C: TSCDeControl);
begin
  Inc(FControlChange);
  ControlChanged(C);
  Changed;
end;

procedure TSCDeShapeBase.DoControlRemoved(C: TSCDeControl);
begin
  Inc(FControlChange);
  ControlRemoved(C);
end;

function TSCDeShapeBase.GetControl(Index: Integer): TSCDeControl;
begin
  Result := TSCDeControl(FControls[Index]);
end;

function TSCDeShapeBase.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

function TSCDeShapeBase.IndexOfControl(C: TSCDeControl): Integer;
begin
  Result := -1;
  if (C <> nil) and (C.FOwner = Self) then
    Result := FControls.IndexOf(C);
end;

procedure TSCDeShapeBase.InsertControl(C: TSCDeControl);
begin
  if (C <> nil) and (C.FOwner <> Self) and CanInsertControl(C) then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    if C.FOwner <> nil then C.FOwner.RemoveControl(C);
    C.FOwner := Self;
    FControls.Add(C);

    C.OnChanging := DoControlIsChanging;

    DoControlAdded(C);
    Changed;

    EndAction(Self, scacSizing, '');
  end;
end;

procedure TSCDeShapeBase.RemoveControl(C: TSCDeControl);
begin
  if (C <> nil) and (C.FOwner = Self) then
  begin
    StartAction(Self, scacSizing, '');
    NotifyUndo(Self, scacSizing, '');

    C.OnChanging := nil;
    C.FOwner := nil;
    FControls.Remove(C);

    DoControlRemoved(C);
    Changed;

    EndAction(Self, scacSizing, '');
  end;
end;

procedure TSCDeShapeBase.BeginControlUpdate;
begin
  Inc(FControlUpdate);
end;

procedure TSCDeShapeBase.EndControlUpdate;
begin
  if FControlUpdate > 0 then
  begin
    Dec(FControlUpdate);
    if (FControlUpdate = 0) and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TSCDeShapeBase.GetInControlUpdate: Boolean;
begin
  Result := FControlUpdate > 0;
end;

procedure TSCDeShapeBase.Add(P: TSCDePoint);
begin
  //
end;

function TSCDeShapeBase.Add(x, y: Double): TSCDePoint;
begin
  Result := nil;
end;

procedure TSCDeShapeBase.Clear;
begin
  //
end;

procedure TSCDeShapeBase.Delete(Index: Integer);
begin
  //
end;

procedure TSCDeShapeBase.Extract(P: TSCDePoint);
begin
  //
end;

procedure TSCDeShapeBase.Insert(Index: Integer; P: TSCDePoint);
begin
  //
end;

procedure TSCDeShapeBase.Remove(P: TSCDePoint);
begin
  //
end;

function TSCDeShapeBase.ApproxInsert(P: TDoublePoint; Fuzz: Double): TSCDePoint;
begin
  Result := nil;
end;

procedure TSCDeShapeBase.Select(P: TSCDePoint);
begin
  if P = nil then
    ClearSelection
  else
  if (P.FOwner = Self) and not ((FSelectionList.Count = 1) and (FSelectionList[0] = P)) then
  begin
    FSelectionList.Clear;
    FSelectionList.Add(P);

    SelectionChanged;
  end;
end;

function TSCDeShapeBase.GetPointArray: TDPointArray;
var
  I: Integer;
  P: TSCDePoint;
begin
  SetLength(Result, PointCount);

  for I := 0 to PointCount-1 do
  begin
    P := Points[I];

    Result[I].x := P.x;
    Result[I].y := P.y;
  end;
end;

procedure TSCDeShapeBase.Refresh(Force: Boolean);
begin
  Changed(Force);
end;

procedure TSCDeShapeBase.RecalculateBounds(Force: Boolean);
begin
  //
end;

function TSCDeShapeBase.CanInsertControl(C: TSCDeControl): Boolean;
begin
  Result := (C <> nil) and (C.FOwner <> Self);
end;

function TSCDeShapeBase.CanInsertPoint(P: TSCDePoint): Boolean;
begin
  Result := (P <> nil) and (P.FOwner <> Self);
end;

function TSCDeShapeBase.PointControlAt(P: TDoublePoint; Fuzz: Double;
  IsSelected: Boolean; var PointIndex: Integer): Integer;
begin
  Result := -1;
  PointIndex := -1;
end;

procedure TSCDeShapeBase.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    StartAction(Self, scacChanging, 'caption');
    NotifyUndo(Self, scacChanging, 'caption');

    FCaption := Value;
    if scssUsesCaption in ShapeStyle then
      Changed;

    EndAction(Self, scacChanging, 'caption');
  end;
end;

function TSCDeShapeBase.HasPicture: Boolean;
begin
  Result := (scssUsesPicture in ShapeStyle) and (Picture.Graphic <> nil) and
    not Picture.Graphic.Empty and (Picture.Width > 0) and (Picture.Height > 0);
end;

function TSCDeShapeBase.HitTest(P: TDoublePoint; Fuzz, PointSize: Double;
  Selected, Editing: Boolean; Shift: TShiftState): TSCDeShapeHit;
begin
  Result.Part := scspNone;
  Result.PartIndex := -1;
  Result.SubIndex := -1;

  if Fuzz < 0.0 then Fuzz := 0.0;
  if PointSize < 1.0 then PointSize := 1.0;

  if Editing and (scssUsesControls in FShapeStyle) then
  begin
    Result.PartIndex := ControlAt(P, PointSize);

    if Result.PartIndex > -1 then
    begin
      Result.Part := scspControl;
      Exit;
    end;
  end;

  if Selected and Editing and (scssUsesPointControls in FShapeStyle) then
  begin
    Result.PartIndex := PointControlAt(P, PointSize, Selected, Result.SubIndex);

    if Result.PartIndex > -1 then
    begin
      Result.Part := scspPointControl;
      Exit;
    end;
  end;

  if Editing then
  begin
    Result.PartIndex := PointAt(P, PointSize);

    if Result.PartIndex > -1 then
    begin
      Result.Part := scspPoint;
      Exit;
    end;
  end;

  if PointOnShape(P, Fuzz, Editing) then
  begin
    Result.Part := scspShape;
    Exit;
  end;
end;

function TSCDeShapeBase.GetGradient: TSCDeShapeGradient;
begin
  Result := FGradient;
end;

function TSCDeShapeBase.GetGravityCenter: TDoublePoint;
begin
  Result := TSCDeUtils.Point(0, 0);
end;

procedure TSCDeShapeBase.ArrangeBoundsRect(var R: TDoubleRect);
begin
  if R.Left > R.Right then TSCDeUtils.Swap(R.Left, R.Right);
  if R.Top > R.Bottom then TSCDeUtils.Swap(R.Top, R.Bottom);
end;

procedure TSCDeShapeBase.Refine(Zoom: Double);
begin
  //
end;

procedure TSCDeShapeBase.DrawPoint(C: TCanvas; R: TRect; InColor,
  OutColor: TColor);
begin
  if not IsRectEmpty(R) and ((OutColor <> clNone) or (InColor <> clNone)) then
    with C do
    begin
      Pen.Width := 1;
      Pen.Mode  := pmCopy;

      if OutColor <> clNone then
      begin
        Pen.Color := OutColor;
        Pen.Style := psSolid;
      end else
      if InColor = clNone then
        Pen.Style := psClear
      else begin
        Pen.Color := InColor;
        Pen.Style := psSolid;
      end;

      if InColor = clNone then
        Brush.Style := bsClear
      else begin
        Brush.Color := InColor;
        Brush.Style := bsSolid;
      end;

      C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
    end;
end;

procedure TSCDeShapeBase.RegisterNotifier(Notifier: TSCDeShapeNotifier);
begin
  if (Notifier <> nil) and not FDestroying then
  begin
    if FNotifyList = nil then FNotifyList := TList.Create;
    FNotifyList.Add(Notifier);
  end;
end;

procedure TSCDeShapeBase.ReleaseGradient;
begin
  if FGradient <> nil then
  begin
    FGradient.OnChanging := nil;
    FGradient.OnChange := nil;

    FreeAndNil(FGradient);
  end;
end;

procedure TSCDeShapeBase.UnregisterNotifier(Notifier: TSCDeShapeNotifier);
begin
  if (Notifier <> nil) and (FNotifyList <> nil) then
  begin
    FNotifyList.Remove(Notifier);
    if FNotifyList.Count = 0 then FreeAndNil(FNotifyList);
  end;
end;

procedure TSCDeShapeBase.NotifyChange;
var
  I: Integer;
  N: TSCDeShapeNotifier;
begin
  I := 0;
  while (FNotifyList <> nil) and (I < FNotifyList.Count) do
  begin
    N := TSCDeShapeNotifier(FNotifyList[I]);
    Inc(I);

    N.Changed(Self);
  end;
end;

procedure TSCDeShapeBase.NotifyDestroy;
var
  I: Integer;
  N: TSCDeShapeNotifier;
begin
  while (FNotifyList <> nil) and (FNotifyList.Count > 0) do
  begin
    I := FNotifyList.Count-1;

    N := TSCDeShapeNotifier(FNotifyList[I]);
    FNotifyList.Delete(I);
    
    if FNotifyList.Count = 0 then FreeAndNil(FNotifyList);

    N.Destroyed(Self);
  end;
end;

procedure TSCDeShapeBase.SetSurface(ASurface: TSCDeSurface);
begin
  FSurface := ASurface;
end;

procedure TSCDeShapeBase.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
  AGradient: TSCDeShapeGradient;
begin
  FID := Trunc(scdStrToFloatDef(Trim(Node.GetAttribute('id')), FID));

  SetLocked(Boolean(StrToIntDef(GetProperty(Node, 'locked'), Integer(False))));
  SetVisible(Boolean(StrToIntDef(GetProperty(Node, 'visible'), Integer(True))));

  SetText(GetProperty(Node, 'text'));
  SetName(GetProperty(Node, 'name'));

  if (scssUsesPicture in FShapeStyle) then
  begin
    SetPictureStyle(TSCDeShapePictureStyle(StrToIntDef(GetProperty(Node, 'picture_style'),
      Integer(scdspStretch))));

    Prop := Node.ElementByName('picture');
    if Prop <> nil then Self.LoadPictureFromXML(Node);
  end;

  if (scssUsesGradient in FShapeStyle) then
  begin
    AGradient := Self.GetGradient;

    if AGradient <> nil then
    begin
      Prop := Node.ElementByName('gradient');
      if Prop <> nil then Self.Gradient.LoadFromXML(Prop);
    end;
  end;
end;

procedure TSCDeShapeBase.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
  AGradient: TSCDeShapeGradient;
begin
  Node.SetAttribute('id', IntToStr(FID));

  AddProperty(Node, 'visible', IntToStr(Integer(Visible)));
  AddProperty(Node, 'locked', IntToStr(Integer(Locked)));

  if Name <> '' then AddProperty(Node, 'name', Name);
  if Text <> '' then AddProperty(Node, 'text', Text);

  if (scssUsesPicture in FShapeStyle) then
  begin
    AddProperty(Node, 'picture_style', IntToStr(Integer(FPictureStyle)));
    Self.SavePictureToXML(Node);
  end;

  if (scssUsesGradient in FShapeStyle) then
  begin
    AGradient := Self.GetGradient;

    if AGradient <> nil then
    begin
      Prop := TSCDomElement.Create(Node);
      Prop.Name := 'gradient';
      Node.AddElement(Prop);

      AGradient.SaveToXML(Prop);
    end;
  end;
end;

procedure TSCDeShapeBase.DoPointChanging(P: TSCDePoint);
begin
  PointChanging(P);
  Changing;
end;

procedure TSCDeShapeBase.PointChanging(P: TSCDePoint);
begin
  //
end;

procedure TSCDeShapeBase.Changing;
begin
  if not FDestroying then
  begin
    DoChanging;
    if FOwner <> nil then FOwner.DoShapeChanging(Self);
    NotifyChanging;
  end;
end;

procedure TSCDeShapeBase.DoChanging;
begin
  //
end;

procedure TSCDeShapeBase.DoBrushChanging(Sender: TObject);
begin
  StartAction(Self, scacChangingBrush, '');
  NotifyUndo(Self, scacChangingBrush, '');

  BrushChanging(Sender);
  if scssUsesBrush in FShapeStyle then
    Changing;

  EndAction(Self, scacChangingBrush, '');
end;

procedure TSCDeShapeBase.DoFontChanging(Sender: TObject);
begin
  StartAction(Self, scacChangingFont, '');
  NotifyUndo(Self, scacChangingFont, '');

  FontChanging(Sender);
  if scssUsesFont in FShapeStyle then
    Changing;

  EndAction(Self, scacChangingFont, '');
end;

procedure TSCDeShapeBase.DoGradientChanged(Sender: TObject);
begin
  GradientChanged(Sender);
  if scssUsesGradient in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.DoGradientChanging(Sender: TObject);
begin
  StartAction(Self, scacChangingGradient, '');
  NotifyUndo(Self, scacChangingGradient, '');

  GradientChanging(Sender);
  if scssUsesGradient in FShapeStyle then
    Changing;

  EndAction(Self, scacChangingGradient, '');
end;

procedure TSCDeShapeBase.DoPenChanging(Sender: TObject);
begin
  StartAction(Self, scacChangingPen, '');
  NotifyUndo(Self, scacChangingPen, '');

  PenChanging(Sender);
  if scssUsesPen in FShapeStyle then
    Changing;

  EndAction(Self, scacChangingPen, '');
end;

procedure TSCDeShapeBase.BrushChanging(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.FontChanging(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.PenChanging(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.DoPictureChanging(Sender: TObject);
begin
  StartAction(Self, scacChangingPicture, '');
  NotifyUndo(Self, scacChangingPicture, '');

  PictureChanging(Sender);
  if scssUsesPicture in FShapeStyle then
    Changing;

  EndAction(Self, scacChangingPicture, '');
end;

procedure TSCDeShapeBase.PictureChanging(Sender: TObject);
begin
  //
end;

procedure TSCDeShapeBase.DoControlChanging(C: TSCDeControl);
begin
  ControlChanging(C);
  Changing;
end;

procedure TSCDeShapeBase.ControlChanging(C: TSCDeControl);
begin
  //
end;

procedure TSCDeShapeBase.NotifyChanging;
var
  I: Integer;
  N: TSCDeShapeNotifier;
begin
  I := 0;
  while (FNotifyList <> nil) and (I < FNotifyList.Count) do
  begin
    N := TSCDeShapeNotifier(FNotifyList[I]);
    Inc(I);

    N.Changing(Self);
  end;
end;

procedure TSCDeShapeBase.NotifyUndo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);
begin
  if (S <> nil) and (FOwner <> nil) and (FNotifyLock = 0) then
    FOwner.NotifyUndo(S, Action, PropName);
end;

procedure TSCDeShapeBase.LoadPictureFromXML(Node: TSCDomElement);
var
  C: Byte;
  SL: TStringList;
  S, Hex: String;
  CData: TSCDomCData;
  Prop: TSCDomElement;
  Stream: TMemoryStream;
  Reader: TPictureReader;
  I, Cnt, P, Ln, ReadLen: Integer;
begin
  if FPicture <> nil then
  begin
    Changing;
    BeginUpdate;
    try
      FPicture.Graphic := nil;
      Prop := Node.ElementByName('picture');

      if Prop <> nil then
      begin
        CData := nil;
        for I := 0 to Prop.ChildNodeCount-1 do
          if Prop.ChildNodes[I] is TSCDomCData then
          begin
            CData := TSCDomCData(Prop.ChildNodes[I]);
            Break;
          end;

        if CData <> nil then
        begin
          SL := TStringList.Create;

          S := '';
          try
            SL.Text := CData.Text;
            for I := 0 to SL.Count-1 do
              S := S + SL[I];
          finally
            SL.Free;
          end;

          Ln := Length(S);
          if (Ln = 0) or Odd(Ln) then
            Exit;

          Stream := TMemoryStream.Create;
          try
            Cnt := 0;
            Ln := Ln div 2;

            SetLength(Hex, 3);
            Hex[1] := '$';

            ReadLen := SizeOf(Byte);

            while Cnt < Ln do
            begin
              Inc(Cnt);
              P := 2*(Cnt-1) + 1;

              Hex[2] := S[P];
              Hex[3] := S[P + 1];

              C := StrToInt(Hex);
              Stream.Write(C, ReadLen);
            end;

            if Stream.Size > 0 then
            begin
              Stream.Position := 0;
              
              Reader := TPictureReader.Create(Stream, 1024);
              try
                TSCFakePicture(FPicture).DefineProperties(Reader);
              finally
                Reader.Free;
              end;
            end;
          finally
            Stream.Free;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeShapeBase.SavePictureToXML(Node: TSCDomElement);
var
  C: Byte;
  S, Hex: String;
  CData: TSCDomCData;
  Prop: TSCDomElement;
  Stream: TMemoryStream;
  Writer: TPictureWriter;
  ReadLen, P, Cnt, Size, ModCnt: Integer;
begin
  if (FPicture <> nil) and (FPicture.Graphic <> nil) and
    not FPicture.Graphic.Empty then
  begin
    Stream := TMemoryStream.Create;
    try
      Writer := TPictureWriter.Create(Stream, 1024);
      try
        TSCFakePicture(FPicture).DefineProperties(Writer);
        Writer.FlushBuffer;

        if Stream.Size > 0 then
        begin
          Prop := TSCDomElement.Create(Node);
          Prop.Name := 'picture';
          Prop.AddAttribute('class', FPicture.Graphic.ClassName);

          Node.AddElement(Prop);

          CData := TSCDomCData.Create(Prop);
          Prop.AddNode(CData);

          Size := Stream.Size + (Stream.Size div 40);
          if Size > 0 then
          begin
            Inc(Size);

            SetLength(S, 2*Size);

            S[1] := #13;
            S[2] := #10;

            Cnt := 1;
            Stream.Position := 0;
            ReadLen := SizeOf(Byte);

            ModCnt := 0;
            while Stream.Read(C, ReadLen) > 0 do
            begin
              Inc(Cnt);
              P := 2*(Cnt-1) + 1;

              Hex := IntToHex(C, 2);
              S[P] := Hex[1];
              S[P + 1] := Hex[2];

              Inc(ModCnt);
              if ModCnt = 40 then
              begin
                ModCnt := 0;

                Inc(Cnt);
                P := 2*(Cnt-1) + 1;

                S[P] := #13;
                S[P + 1] := #10;
              end;
            end;

            CData.Text := S;
          end;
        end;
      finally
        Writer.Free;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

function TSCDeShapeBase.GetProperty(Parent: TSCDomElement;
  const AName: WideString): WideString;
var
  I: Integer;
  Prop: TSCDomElement;
begin
  Result := '';

  if Parent <> nil then
  begin
    Prop := Parent.ElementByName(AName);

    if Prop <> nil then
      for I := 0 to Prop.ChildNodeCount-1 do
        if Prop.ChildNodes[I] is TSCDomText then
        begin
          Result := Trim(TSCDomText(Prop.ChildNodes[I]).Text);

          if (Result <> '') and (Result[1] = #13) then System.Delete(Result, 1, 1);
          if (Result <> '') and (Result[1] = #10) then System.Delete(Result, 1, 1);

          Result := Trim(Result);

          Break;
        end;
  end;
end;

function TSCDeShapeBase.AddProperty(Parent: TSCDomElement; const AName,
  AValue: WideString): TSCDomElement;
var
  Val: TSCDomText;
begin
  Result := TSCDomElement.Create(Parent);
  Result.Name := AName;
  Parent.AddElement(Result);

  Val := TSCDomText.Create(Result);
  Val.Text := AValue;
  Result.AddNode(Val);
end;

procedure TSCDeShapeBase.ControlIsChanging(C: TSCDeControl; var x,
  y: Double);
begin
  //
end;

procedure TSCDeShapeBase.DoControlIsChanging(Sender: TObject; var x,
  y: Double);
begin
  ControlIsChanging(TSCDeControl(Sender), x, y);
end;

procedure TSCDeShapeBase.DoPointIsChanging(Sender: TObject; var x,
  y: Double);
begin
  PointIsChanging(TSCDePoint(Sender), x, y);
end;

procedure TSCDeShapeBase.PointIsChanging(P: TSCDePoint; var x, y: Double);
begin
  //
end;

function TSCDeShapeBase.GetNotifyLocked: Boolean;
begin
  Result := FNotifyLock > 0;
end;

procedure TSCDeShapeBase.BeginNotifyLock;
begin
  Inc(FNotifyLock);
end;

procedure TSCDeShapeBase.EndNotifyLock;
begin
  if FNotifyLock > 0 then
    Dec(FNotifyLock);
end;

procedure TSCDeShapeBase.LoadBoundsFromXML(Node: TSCDomElement);
var
  S: String;
  I: Integer;
  Pt: TSCDePoint;
  C: TSCDeControl;
  P: TDoublePoint;
  Prop, SubProp, CtrlProp: TSCDomElement;
begin
  BeginUpdate;
  try
    BeginPointUpdate;
    try
      ClearPoints;

      Prop := Node.ElementByName('points');
      if Prop <> nil then
        for I := 0 to Prop.ChildNodeCount-1 do
          if (Prop.ChildNodes[I].Name = 'point') and (Prop.ChildNodes[I] is TSCDomElement) then
          begin
            P.x := 0;
            P.y := 0;

            SubProp := TSCDomElement(Prop.ChildNodes[I]);

            S := GetProperty(SubProp, 'x');
            if S <> '' then P.x := StrToFloat(S);

            S := GetProperty(SubProp, 'y');
            if S <> '' then P.y := StrToFloat(S);

            Pt := Self.Add(P.x, P.y);

            if (Pt <> nil) and (scssUsesPointControls in FShapeStyle) then
            begin
              CtrlProp := SubProp.ElementByName('control1');
              if CtrlProp <> nil then
              begin
                P.x := 0;
                P.y := 0;

                S := GetProperty(CtrlProp, 'x');
                if S <> '' then P.x := StrToFloat(S);

                S := GetProperty(CtrlProp, 'y');
                if S <> '' then P.y := StrToFloat(S);

                if (P.x <> 0.0) or (P.y <> 0.0) then
                begin
                  Pt.Control_1 := TSCDePointControl.Create;
                  Pt.Control_1.SetPosition(P.x, P.y); 
                end;
              end;

              CtrlProp := SubProp.ElementByName('control2');
              if CtrlProp <> nil then
              begin
                P.x := 0;
                P.y := 0;

                S := GetProperty(CtrlProp, 'x');
                if S <> '' then P.x := StrToFloat(S);

                S := GetProperty(CtrlProp, 'y');
                if S <> '' then P.y := StrToFloat(S);

                if (P.x <> 0.0) or (P.y <> 0.0) then
                begin
                  Pt.Control_2 := TSCDePointControl.Create;
                  Pt.Control_2.SetPosition(P.x, P.y);
                end;
              end;
            end;
          end;
    finally
      EndPointUpdate;
    end;

    if scssUsesControls in FShapeStyle then
    begin
      BeginControlUpdate;
      try
        ClearControls;

        Prop := Node.ElementByName('controls');
        if Prop <> nil then
          for I := 0 to Prop.ChildNodeCount-1 do
            if (Prop.ChildNodes[I].Name = 'control') and (Prop.ChildNodes[I] is TSCDomElement) then
            begin
              P.x := 0;
              P.y := 0;

              SubProp := TSCDomElement(Prop.ChildNodes[I]);

              S := GetProperty(SubProp, 'x');
              if S <> '' then P.x := StrToFloat(S);

              S := GetProperty(SubProp, 'y');
              if S <> '' then P.y := StrToFloat(S);

              C := TSCDeControl.Create(Self);
              C.SetPosition(P.x, P.y);
            end;
      finally
        EndPointUpdate;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeShapeBase.SaveBoundsToXML(Node: TSCDomElement);
var
  I: Integer;
  P: TSCDePoint;
  C: TSCDeControl;
  Prop, SubProp,
  CtrlProp: TSCDomElement;
begin
  if PointCount > 0 then
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := 'points';
    Node.AddElement(Prop);

    for I := 0 to PointCount-1 do
    begin
      P := Points[I];

      SubProp := TSCDomElement.Create(Prop);
      SubProp.Name := 'point';
      Prop.AddElement(SubProp);

      AddProperty(SubProp, 'x', FloatToStr(P.x));
      AddProperty(SubProp, 'y', FloatToStr(P.y));

      if scssUsesPointControls in FShapeStyle then
      begin
        if P.Control_1 <> nil then
        begin
          CtrlProp := TSCDomElement.Create(SubProp);
          CtrlProp.Name := 'control1';
          SubProp.AddElement(CtrlProp);

          AddProperty(CtrlProp, 'x', FloatToStr(P.Control_1.x));
          AddProperty(CtrlProp, 'y', FloatToStr(P.Control_1.y));
        end;

        if P.Control_2 <> nil then
        begin
          CtrlProp := TSCDomElement.Create(SubProp);
          CtrlProp.Name := 'control2';
          SubProp.AddElement(CtrlProp);

          AddProperty(CtrlProp, 'x', FloatToStr(P.Control_2.x));
          AddProperty(CtrlProp, 'y', FloatToStr(P.Control_2.y));
        end;
      end;
    end;
  end;

  if (ControlCount > 0) and (scssUsesControls in FShapeStyle) then
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := 'controls';
    Node.AddElement(Prop);

    for I := 0 to ControlCount-1 do
    begin
      C := Controls[I];

      SubProp := TSCDomElement.Create(Prop);
      SubProp.Name := 'control';
      Prop.AddElement(SubProp);

      AddProperty(SubProp, 'x', FloatToStr(C.x));
      AddProperty(SubProp, 'y', FloatToStr(C.y));
    end;
  end;
end;

function TSCDeShapeBase.ShapeByID(AID: DWord): TSCDeShapeBase;
begin
  Result := nil;
end;

procedure TSCDeShapeBase.LoadFromFile(const FileName: String);
var
  P: TSCDomParser;
  Doc: TSCDomDocument;
  Root: TSCDomElement;
begin
  StartAction(Self, scacChanging, '');
  BeginUpdate;
  try
    NotifyUndo(Self, scacChanging, '');

    BeginNotifyLock;
    try
      Clear;

      if FileExists(FileName) then
      begin
        P := TSCDomParser.Create(nil);
        try
          Doc := P.ParseFile(FileName);
          try
            if Doc <> nil then
            begin
              Root := Doc.ElementByName('element');
              if Root <> nil then Self.LoadFromXML(Root);
            end;
          finally
            if Doc <> nil then
              Doc.Free;
          end;
        finally
          P.Free;
        end;
      end;
    finally
      EndNotifyLock;
    end;
  finally
    EndUpdate;
  end;

  EndAction(Self, scacChanging, '');
end;

procedure TSCDeShapeBase.SaveToFile(const FileName: String);
var
  Doc: TSCDomDocument;
  Root: TSCDomElement;
begin
  if FileName <> '' then
  begin
    Doc := TSCDomDocument.Create(nil);
    try
      Root := TSCDomElement.Create(Doc);
      Root.Name := 'element';
      Root.AddAttribute('version', '1.0');

      Doc.AddElement(Root);
      Self.SaveToXML(Root);

      Doc.SaveToFile(FileName);
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSCDeShapeBase.RestoreID;
begin
  FID := FTempID;
end;

procedure TSCDeShapeBase.StoreID;
begin
  FTempID := FID;
end;

procedure TSCDeShapeBase.EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
  const PropName: String);
begin
  if (S <> nil) and (FOwner <> nil) and (FNotifyLock = 0) then
    FOwner.EndAction(S, Action, PropName);
end;

procedure TSCDeShapeBase.StartAction(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);
begin
  if (S <> nil) and (FOwner <> nil) and (FNotifyLock = 0) then
    FOwner.StartAction(S, Action, PropName);
end;

procedure TSCDeShapeBase.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FBrush <> nil then FBrush.ShapeColorChanged;
    if FGradient <> nil then FGradient.ShapeColorChanged;
  end;
end;

function TSCDeShapeBase.HasBrush: Boolean;
begin
  Result := (scssUsesBrush in ShapeStyle) and (Brush <> nil) and
    (Brush.Color <> clNone) and (Brush.Style <> bsClear);
end;

function TSCDeShapeBase.HasGradient: Boolean;
begin
  Result := (scssUsesGradient in ShapeStyle) and
    (FGradient <> nil) and (FGradient.ColorBegin <> clNone) and
    (FGradient.ColorEnd <> clNone) and (FGradient.Style <> scdgsNone);
end;

function TSCDeShapeBase.HasPen: Boolean;
begin
  Result := (scssUsesPen in ShapeStyle) and (Pen <> nil) and
    (Pen.Width > 0) and (Pen.Color <> clNone) and (Pen.Style <> psClear);
end;

procedure TSCDeShapeBase.SetPictureStyle(Value: TSCDeShapePictureStyle);
begin
  if FPictureStyle <> Value then
  begin
    DoPictureStyleChanging;
    FPictureStyle := Value;
    DoPictureStyleChanged;
  end;
end;

procedure TSCDeShapeBase.DrawPicture(C: TCanvas; R: TRect);
var
  I, J, L, T, W, H, XCnt, YCnt: Integer;
begin
  if not IsRectEmpty(R) and Self.HasPicture then
  begin
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;

    case FPictureStyle of
      scdspCenter:
      begin
        L := R.Left + ((W - Picture.Width) div 2);
        T := R.Top + ((H - Picture.Height) div 2);

        C.Draw(L, T, Picture.Graphic);
      end;
      scdspDefault:
        C.Draw(R.Left, R.Top, Picture.Graphic);
      scdspStretch:
        C.StretchDraw(R, Picture.Graphic);
      scdspTiled:
      begin
        XCnt := W div Picture.Width;
        if (W mod Picture.Width) <> 0 then
          Inc(XCnt);

        YCnt := H div Picture.Height;
        if (H mod Picture.Height) <> 0 then
          Inc(YCnt);

        for I := 0 to XCnt-1 do
        begin
          L := R.Left + I*Picture.Width;
          for J := 0 to YCnt-1 do
          begin
            T := R.Top + J*Picture.Height;
            C.Draw(L, T, Picture.Graphic);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSCDeShapeBase.DoPictureStyleChanged;
begin
  PictureStyleChanged;
  if scssUsesPicture in FShapeStyle then
    Changed;
end;

procedure TSCDeShapeBase.DoPictureStyleChanging;
begin
  StartAction(Self, scacChanging, 'picture_style');
  NotifyUndo(Self, scacChanging, 'picture_style');

  PictureStyleChanging;
  if scssUsesPicture in FShapeStyle then
    Changing;

  EndAction(Self, scacChanging, 'picture_style');
end;

procedure TSCDeShapeBase.PictureStyleChanged;
begin
  //
end;

procedure TSCDeShapeBase.PictureStyleChanging;
begin
  //
end;

{ TSCDeShapePen }

procedure TSCDeShapePen.Assign(Source: TPersistent);
begin
  if Source is TSCDeShapePen then
  begin
    Changing;
    with TSCDeShapePen(Source) do
    begin
      Self.FColor := Color;
      Self.FMode  := Mode;
      Self.FStyle := Style;
      Self.FWidth := Width;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeShapePen.AssignRec(const Source: TSCDePenRec);
begin
  Changing;
  with TSCDePenRec(Source) do
  begin
    Self.FColor := Color;
    Self.FMode  := Mode;
    Self.FStyle := Style;
    Self.FWidth := Width;
  end;
  Changed;
end;

procedure TSCDeShapePen.AssignTo(Dest: TPersistent);
begin
  if Dest is TPen then
  begin
    with TPen(Dest) do
    begin
      Color := Self.Color;
      Mode := Self.Mode;
      Style := Self.Style;
      Width := Self.Width;

      if Self.Color = clNone then Style := psClear;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSCDeShapePen.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSCDeShapePen.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

constructor TSCDeShapePen.Create;
begin
  inherited Create;
  FColor := clBlack;
  FMode  := pmCopy;
  FStyle := psSolid;
  FWidth := 1;
end;

function TSCDeShapePen.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeShapePen.LoadFromXML(Node: TSCDomElement);

  function GetProperty(Parent: TSCDomElement; AName: String): String;
  var
    I: Integer;
    Prop: TSCDomElement;
  begin
    Result := '';

    if Parent <> nil then
    begin
      Prop := Parent.ElementByName(AName);

      if Prop <> nil then
        for I := 0 to Prop.ChildNodeCount-1 do
          if Prop.ChildNodes[I] is TSCDomText then
          begin
            Result := Trim(TSCDomText(Prop.ChildNodes[I]).Text);

            if (Result <> '') and (Result[1] = #13) then Delete(Result, 1, 1);
            if (Result <> '') and (Result[1] = #10) then Delete(Result, 1, 1);

            Result := Trim(Result);

            Break;
          end;
    end;
  end;

var
  S: String;
begin
  Changing;
  try
    S := GetProperty(Node, 'color');
    if S <> '' then FColor := StringToColor(S);

    S := GetProperty(Node, 'mode');
    FMode := TPenMode(StrToIntDef(S, Integer(pmCopy)));

    S := GetProperty(Node, 'style');
    FStyle := TPenStyle(StrToIntDef(S, Integer(psSolid)));

    S := GetProperty(Node, 'width');
    FWidth := StrToIntDef(S, 1);
  finally
    Changed;
  end;
end;

procedure TSCDeShapePen.SaveToXML(Node: TSCDomElement);

  procedure AddProperty(const AName, AValue: String);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := AName;
    Node.AddElement(Prop);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

begin
  AddProperty('color', ColorToString(FColor));
  AddProperty('mode',  IntToStr(Integer(FMode)));
  AddProperty('style', IntToStr(Integer(FStyle)));
  AddProperty('width', IntToStr(Integer(FWidth)));
end;

procedure TSCDeShapePen.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    Changing;

    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeShapePen.SetMode(Value: TPenMode);
begin
  if FMode <> Value then
  begin
    Changing;

    FMode := Value;
    Changed;
  end;
end;

procedure TSCDeShapePen.SetOwner(AOwner: TSCDeShapeBase);
begin
  FOwner := AOwner;
end;

procedure TSCDeShapePen.SetStyle(Value: TPenStyle);
begin
  if FStyle <> Value then
  begin
    Changing;

    FStyle := Value;
    Changed;
  end;
end;

procedure TSCDeShapePen.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    Changing;

    FWidth := Value;
    Changed;
  end;
end;

{ TSCDeShapeBrush }

procedure TSCDeShapeBrush.Assign(Source: TPersistent);
begin
  if Source is TSCDeShapeBrush then
  begin
    Changing;
    with TSCDeShapeBrush(Source) do
    begin
      Self.FColor := Color;
      Self.FStyle := Style;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeShapeBrush.AssignRec(const Source: TSCDeBrushRec);
begin
  Changing;
  with TSCDeBrushRec(Source) do
  begin
    Self.Color := Color;
    Self.FStyle := Style;
  end;
  Changed;
end;

procedure TSCDeShapeBrush.AssignTo(Dest: TPersistent);
begin
  if Dest is TBrush then
  begin
    with TBrush(Dest) do
    begin
      Color := Self.Color;
      Style := Self.Style;

      if Self.Color = clNone then
        Style := bsClear;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSCDeShapeBrush.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSCDeShapeBrush.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

constructor TSCDeShapeBrush.Create;
begin
  inherited Create;
  FColor := clLime;
  FStyle := bsSolid;
end;

function TSCDeShapeBrush.GetColor: TColor;
begin
  Result := FColor;
  if FOwner <> nil then Result := FOwner.FColor;
end;

function TSCDeShapeBrush.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeShapeBrush.LoadFromXML(Node: TSCDomElement);

  function GetProperty(Parent: TSCDomElement; AName: String): String;
  var
    I: Integer;
    Prop: TSCDomElement;
  begin
    Result := '';

    if Parent <> nil then
    begin
      Prop := Parent.ElementByName(AName);

      if Prop <> nil then
        for I := 0 to Prop.ChildNodeCount-1 do
          if Prop.ChildNodes[I] is TSCDomText then
          begin
            Result := Trim(TSCDomText(Prop.ChildNodes[I]).Text);

            if (Result <> '') and (Result[1] = #13) then Delete(Result, 1, 1);
            if (Result <> '') and (Result[1] = #10) then Delete(Result, 1, 1);

            Result := Trim(Result);

            Break;
          end;
    end;
  end;

var
  S: String;
begin
  Changing;
  try
    S := GetProperty(Node, 'color');
    if S <> '' then
    begin
      FColor := StringToColor(S);
      if FOwner <> nil then
        FOwner.FColor := FColor;
    end;

    S := GetProperty(Node, 'style');
    FStyle := TBrushStyle(StrToIntDef(S, Integer(bsSolid)));
  finally
    Changed;
  end;
end;

procedure TSCDeShapeBrush.SaveToXML(Node: TSCDomElement);

  procedure AddProperty(const AName, AValue: String);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := AName;
    Node.AddElement(Prop);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

begin
  AddProperty('color', ColorToString(GetColor));
  AddProperty('style', IntToStr(Integer(FStyle)));
end;

procedure TSCDeShapeBrush.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    Changing;

    FColor := Value;
    if FOwner <> nil then
      FOwner.SetColor(Value);

    Changed;
  end;
end;

procedure TSCDeShapeBrush.SetOwner(AOwner: TSCDeShapeBase);
begin
  FOwner := AOwner;
end;

procedure TSCDeShapeBrush.SetStyle(Value: TBrushStyle);
begin
  if FStyle <> Value then
  begin
    Changing;

    FStyle := Value;
    Changed;
  end;
end;

procedure TSCDeShapeBrush.ShapeColorChanged;
begin
  if FOwner <> nil then
    Self.FColor := FOwner.FColor;
end;

{ TSCDeShape }

procedure TSCDeShape.Add(P: TSCDePoint);
begin
  InsertPoint(P);
end;

function TSCDeShape.Add(x, y: Double): TSCDePoint;
begin
  Result := TSCDePoint.Create(nil);
  Result.x := x;
  Result.y := y;

  Add(Result);
end;

procedure TSCDeShape.Clear;
begin
  ClearPoints;
end;

procedure TSCDeShape.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < FPoints.Count) then
    TSCDePoint(FPoints[Index]).Free;
end;

procedure TSCDeShape.Extract(P: TSCDePoint);
begin
  RemovePoint(P);
end;

procedure TSCDeShape.Insert(Index: Integer; P: TSCDePoint);
begin
  if (P <> nil) and (P.FOwner <> Self) and
    (Index > -1) and (Index <= FPoints.Count) then
  begin
    BeginUpdate;
    try
      InsertPoint(P);
      FPoints.Move(FPoints.Count-1, Index);
    finally
      EndUpdate;
    end;
  end;
end;

function TSCDeShape.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  R, R2: TDoubleRect;
begin
  if (Brush.Style <> bsClear) and (Brush.Color <> clNone) then
    Result := inherited PointOnShape(P, Fuzz)
  else begin
    Result := False;

    R := GetBounds;
    if not TSCDeUtils.IsRectEmpty(R) then
    begin
      R2 := R;
      TSCDeUtils.InflateRect(R2, Fuzz, Fuzz);

      Result := not TSCDeUtils.IsRectEmpty(R) and
        TSCDeUtils.PtInRect(R, P);

      if Result then
      begin
        R2 := R;
        TSCDeUtils.InflateRect(R2, -Fuzz, -Fuzz);

        Result := TSCDeUtils.IsRectEmpty(R) or
          not TSCDeUtils.PtInRect(R, P);
      end;
    end;  
  end;
end;

procedure TSCDeShape.Remove(P: TSCDePoint);
begin
  if (P <> nil) and (P.FOwner = Self) then P.Free;
end;

{ TSCDeContainer }

procedure TSCDeContainer.Add(S: TSCDeShapeBase);
begin
  InsertShape(S);
end;

procedure TSCDeContainer.BringToFront(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (S.FOwner = Self) then
  begin
    Index := S.Index;

    if Index < FItems.Count-1 then
    begin
      StartAction(S, scacBringingFront, '');
      NotifyUndo(S, scacBringingFront, '');

      Changing;

      FItems.Move(Index, FItems.Count-1);
      Changed;

      EndAction(S, scacBringingFront, '');
    end;
  end;
end;

procedure TSCDeContainer.Clear;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if (ItemCount > 0) or (PointCount > 0) then
  begin
    StartAction(Self, scacClearing, '');
    NotifyUndo(Self, scacClearing, '');

    BeginUpdate;
    try
      Inc(FChangeCount);

      BeginNotifyLock;
      try
        for I := FItems.Count-1 downto 0 do
        begin
          S := TSCDeShapeBase(FItems[I]);
          try
            S.FOwner := nil;
            FItems.Delete(I);
          finally
            S.Free;
          end;
        end;

        ClearPoints;
      finally
        EndNotifyLock;
      end;
    finally
      EndUpdate;
      DoShapeCleared(Self);
    end;

    EndAction(Self, scacClearing, '');
  end;
end;

constructor TSCDeContainer.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  FItems := TList.Create;
  FBounds := TSCDeUtils.Rect(0, 0, 0, 0);
  SetShapeStyle(ShapeStyle + [scssIsContainer] - [scssUsesPen]);
end;

procedure TSCDeContainer.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < FItems.Count) then
    TSCDeShapeBase(FItems[Index]).Free;
end;

destructor TSCDeContainer.Destroy;
begin
  Destroying;
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSCDeContainer.GetItem(Index: Integer): TSCDeShapeBase;
begin
  Result := TSCDeShapeBase(FItems[Index]);
end;

function TSCDeContainer.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TSCDeContainer.IndexOf(S: TSCDeShapeBase): Integer;
begin
  Result := FItems.IndexOf(S);
end;

procedure TSCDeContainer.Insert(Index: Integer; S: TSCDeShapeBase);
begin
  if (S <> nil) and (S.FOwner <> Self) and (S <> Self) and
    (Index > -1) and (Index <= FItems.Count) and not (S is TSCDeLayer) and
    not IsParent(S) and CanInsertShape(S) then
  begin
    StartAction(Self, scacInserting, '');
    NotifyUndo(Self, scacInserting, '');

    if S.FOwner <> nil then S.FOwner.RemoveShape(S);

    S.FOwner := Self;
    S.SetSurface(Self.Surface);

    FItems.Insert(FItems.Count-1, S);

    DoShapeInserted(S);
    EndAction(Self, scacInserting, '');
  end;
end;

procedure TSCDeContainer.InsertShape(S: TSCDeShapeBase);
begin
  if (S <> nil) and (S <> Self) and (S.FOwner <> Self) and
    not (S is TSCDeLayer) and not IsParent(S) and CanInsertShape(S) then
  begin
    StartAction(Self, scacInserting, '');
    NotifyUndo(Self, scacInserting, '');

    if S.FOwner <> nil then S.FOwner.RemoveShape(S);

    S.FOwner := Self;
    S.SetSurface(Self.Surface);

    FItems.Add(S);

    EndAction(Self, scacInserting, '');
    StartAction(Self, scacInserted, '');

    DoShapeInserted(S);
    NotifyUndo(S, scacInserted, '');

    EndAction(Self, scacInserted, '');
  end;
end;

procedure TSCDeContainer.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  I: Integer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
begin
  if FItems.Count > 0 then
  begin
    R := TSCDeUtils.Rect(DrawRect);

    for I := 0 to FItems.Count-1 do
    begin
      S := TSCDeShapeBase(FItems[I]);
      if S.Visible and not S.InDestroy and S.InRect(R) then
        S.Paint(C, X, Y, Zoom);
    end;
  end;
end;

function TSCDeContainer.ShapeAtPos(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): TSCDeShapeBase;
var
  I: Integer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
begin
  Result := nil;
  for I := 0 to FItems.Count-1 do
  begin
    S := TSCDeShapeBase(FItems[I]);

    if S.Visible and not S.InDestroy then
    begin
      R := S.GetBounds;

      if not TSCDeUtils.IsRectEmpty(R) and TSCDeUtils.PtInRect(R, P) and
        ((S is TSCDeContainer) or S.PointOnShape(P, Fuzz, Editing)) then
      begin
        Result := S;
        Exit;
      end;
    end;
  end;
end;

procedure TSCDeContainer.Remove(S: TSCDeShapeBase);
begin
  if (S <> nil) and (S.FOwner = Self) then S.Free;
end;

procedure TSCDeContainer.RemoveShape(S: TSCDeShapeBase);
begin
  if (S <> nil) and (S.FOwner = Self) then
  begin
    StartAction(Self, scacRemoving, '');

    NotifyUndo(Self, scacRemoving, '');
    NotifyUndo(S, scacRemoved, '');

    S.FOwner := nil;
    FItems.Remove(S);

    S.SetSurface(nil);

    EndAction(Self, scacRemoving, '');

    StartAction(S, scacRemoved, '');
    DoShapeRemoved(S);
    EndAction(S, scacRemoved, '');
  end;
end;

procedure TSCDeContainer.SendToBack(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (S.FOwner = Self) then
  begin
    Index := S.Index;

    if Index > 0 then
    begin
      StartAction(S, scacSendingBack, '');
      NotifyUndo(S, scacSendingBack, '');

      Changing;

      FItems.Move(Index, 0);
      Changed;

      EndAction(S, scacSendingBack, '');
    end;
  end;
end;

procedure TSCDeContainer.ShapeChanged(S: TSCDeShapeBase);
begin
  //
end;

procedure TSCDeContainer.ShapeRemoved(S: TSCDeShapeBase);
begin
  if FOwner <> nil then FOwner.ShapeRemoved(S);
end;

function TSCDeContainer.Exists(S: TSCDeShapeBase): Boolean;
var
  I: Integer;
begin
  Result := False;
  if S <> nil then
    for I := 0 to FItems.Count-1 do
    begin
      Result := FItems[I] = S;
      if not Result and (TSCDeShapeBase(FItems[I]) is TSCDeContainer) then
        Result := TSCDeContainer(FItems[I]).Exists(S);

      if Result then Exit;
    end;
end;

procedure TSCDeContainer.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
var
  R, PR: TRect;
  DR: TDoubleRect;
  InCl, OutCl: TColor;
  OX, OY, AX, AY: Integer;
  HasPoint, HasLine: Boolean;
begin
  HasLine := (Data.LineColor <> clNone) and
    (Data.SelectionType in [scdsMoving, scdsSizing]);

  HasPoint := (Data.PointSize > 1) and ((Data.PointInColor <> clNone) or
    (Data.PointOutColor <> clNone));

  if HasLine or HasPoint then
  begin
    DR := GetBounds;

    with DragPoint do
      case DragPoint.PartIndex of
        0:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        1:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Top := DR.Top + Offset.y;
        end;
        2:
        begin
          DR.Right := DR.Right + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
        3:
        begin
          DR.Left := DR.Left + Offset.x;
          DR.Bottom := DR.Bottom + Offset.y;
        end;
      end;

    TSCDeUtils.ZoomRect(DR, Zoom);
    TSCDeUtils.OffsetRect(DR, X, Y);

    if DR.Right < DR.Left then TSCDeUtils.Swap(DR.Left, DR.Right);
    if DR.Bottom < DR.Top then TSCDeUtils.Swap(DR.Top, DR.Bottom);

    R  := TSCDeUtils.Rect(DR);

    if HasLine then
    begin
      with C do
      begin
        Brush.Style := bsClear;

        Pen.Style := Data.LineStyle;
        Pen.Mode  := Data.LineMode;
        Pen.Color := Data.LineColor;
        Pen.Width := 1;
      end;

      if Data.LineColor <> clNone then
        C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;

    if HasPoint then
    begin
      InCl := Data.PointInColor;
      OutCl := Data.PointOutColor;

      if (OutCl = clNone) and (InCl <> clNone) then
        OutCl := InCl;

      OX := -(Data.PointSize div 2);
      OY := OX;

      AX := R.Left;
      AY := R.Top;

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      DrawPoint(C, PR, InCl, OutCl);

      AX := R.Right;
      AY := R.Top;

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      DrawPoint(C, PR, InCl, OutCl);

      AX := R.Right;
      AY := R.Bottom;

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      DrawPoint(C, PR, InCl, OutCl);

      AX := R.Left;
      AY := R.Bottom;

      PR := Rect(AX, AY, AX + Data.PointSize, AY + Data.PointSize);
      OffsetRect(PR, OX, OY);

      DrawPoint(C, PR, InCl, OutCl);
    end;
  end;
end;

function TSCDeContainer.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

procedure TSCDeContainer.MoveBy(X, Y: Double);
begin
  // 
end;

procedure TSCDeContainer.ShapeInserted(S: TSCDeShapeBase);
begin
  //
end;

function TSCDeContainer.Extract(S: TSCDeShapeBase): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (S <> nil) and (S.FOwner = Self) then
  begin
    Index := FItems.IndexOf(S);
    if Index > -1 then
    begin
      RemoveShape(S);
      Result := True;
    end;
  end;
end;

procedure TSCDeContainer.Resize(NewWidth, NewHeight: Double);
begin
  //
end;

function TSCDeContainer.InRect(const R: TDoubleRect): Boolean;
var
  I: Integer;
begin
  Result := False;

  if (FItems <> nil) and not TSCDeUtils.IsRectEmpty(R) then
    for I := 0 to FItems.Count-1 do
    begin
      Result := TSCDeShapeBase(FItems[I]).InRect(R);
      if Result then Exit;
    end;
end;

procedure TSCDeContainer.PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
  R: TDoubleRect);
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if (FItems.Count > 0) and not TSCDeUtils.IsRectEmpty(R) then
    for I := 0 to FItems.Count-1 do
    begin
      S := TSCDeShapeBase(FItems[I]);
      if S.Visible and not S.InDestroy and S.InRect(R) then
        S.PaintIn(C, X, Y, Zoom, R);
    end;
end;

procedure TSCDeContainer.DrawSelectionIn(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  R: TDoubleRect; Zoom: Double);
begin
  if (FItems.Count > 0) and not TSCDeUtils.IsRectEmpty(R) and Self.InRect(R) then
    DrawSelection(C, X, Y, DragPoint, Data, Zoom);
end;

function TSCDeContainer.PointAt(P: TDoublePoint; Fuzz: Double): Integer;
var
  R, PR: TDoubleRect;
  OX, OY, AX, AY: Double;
begin
  Result := -1;

  if FItems.Count > 0 then
  begin
    R := GetBounds;
    if Fuzz < 1.0 then Fuzz := 1.0;

    OX := -(Fuzz / 2);
    OY := OX;

    AX := R.Left;
    AY := R.Top;

    PR := TSCDeUtils.Rect(AX, AY, AX + Fuzz, AY + Fuzz);
    TSCDeUtils.OffsetRect(PR, OX, OY);

    if TSCDeUtils.PtInRect(PR, P) then
    begin
      Result := 0;
      Exit;
    end;

    AX := R.Right;
    AY := R.Top;

    PR := TSCDeUtils.Rect(AX, AY, AX + Fuzz, AY + Fuzz);
    TSCDeUtils.OffsetRect(PR, OX, OY);

    if TSCDeUtils.PtInRect(PR, P) then
    begin
      Result := 1;
      Exit;
    end;

    AX := R.Right;
    AY := R.Bottom;

    PR := TSCDeUtils.Rect(AX, AY, AX + Fuzz, AY + Fuzz);
    TSCDeUtils.OffsetRect(PR, OX, OY);

    if TSCDeUtils.PtInRect(PR, P) then
    begin
      Result := 2;
      Exit;
    end;

    AX := R.Left;
    AY := R.Bottom;

    PR := TSCDeUtils.Rect(AX, AY, AX + Fuzz, AY + Fuzz);
    TSCDeUtils.OffsetRect(PR, OX, OY);

    if TSCDeUtils.PtInRect(PR, P) then
    begin
      Result := 3;
      Exit;
    end;
  end;
end;

procedure TSCDeContainer.DoShapeChanged(S: TSCDeShapeBase);
begin
  RecalculateBounds(False);

  Changed;
  ShapeChanged(S);
end;

procedure TSCDeContainer.DoShapeInserted(S: TSCDeShapeBase);
begin
  RecalculateBounds(False);

  ShapeInserted(S);
  if FOwner <> nil then FOwner.DoShapeInserted(S);
  Changed;
end;

procedure TSCDeContainer.DoShapeRemoved(S: TSCDeShapeBase);
begin
  RecalculateBounds(False);

  ShapeRemoved(S);
  if FOwner <> nil then FOwner.DoShapeRemoved(S);
  Changed;
end;

procedure TSCDeContainer.RecalculateBounds(Force: Boolean);
var
  I: Integer;
  PR: TDoubleRect;
begin
  if Force or not (InUpdate or InPointUpdate) then
  begin
    FBounds.Right := FBounds.Left;
    FBounds.Bottom := FBounds.Top;

    if FItems.Count > 0 then
    begin
      FBounds := TSCDeShapeBase(FItems[0]).GetBounds;

      for I := 1 to FItems.Count-1 do
      begin
        PR := TSCDeShapeBase(FItems[I]).GetBounds;

        if PR.Left < FBounds.Left then FBounds.Left := PR.Left;
        if PR.Top < FBounds.Top then FBounds.Top := PR.Top;
        if PR.Right > FBounds.Right then FBounds.Right := PR.Right;
        if PR.Bottom > FBounds.Bottom then FBounds.Bottom := PR.Bottom;
      end;
    end;
  end;  
end;

function TSCDeContainer.GetPointValue(Index: Integer): TDoublePoint;
var
  R: TDoubleRect;
begin
  Result := TSCDeUtils.Point(0, 0);
  if (Index > -1) and (Index < 4) then
  begin
    R := GetBounds;
    case Index of
      0: Result := R.TopLeft;
      1: Result := TSCDeUtils.Point(R.Right, R.Top);
      2: Result := R.BottomRight;
      3: Result := TSCDeUtils.Point(R.Left, R.Bottom);
    end;
  end;
end;

procedure TSCDeContainer.SetPointValue(Index: Integer;
  Value: TDoublePoint);
var
  R: TDoubleRect;
begin
  if (Index > -1) and (Index < 4) then
  begin
    R := Self.GetBounds;

    case Index of
      0: R.TopLeft := Value;
      1:
      begin
        R.Right := Value.x;
        R.Top := Value.y;
      end;
      2: R.BottomRight := Value;
      3:
      begin
        R.Left := Value.x;
        R.Bottom := Value.y;
      end;
    end;

    if R.Left > R.Right then TSCDeUtils.Swap(R.Left, R.Right);
    if R.Top > R.Bottom then TSCDeUtils.Swap(R.Top, R.Bottom);

    Self.SetBounds(R);
  end;
end;

procedure TSCDeContainer.SetBounds(ALeft, ATop, AWidth, AHeight: Double);
var
  R: TDoubleRect;
  Offset: TDoublePoint;
begin
  if AWidth < 0 then
  begin
    AWidth := Abs(AWidth);
    ALeft := ALeft - AWidth;
  end;

  if AHeight < 0 then
  begin
    AHeight := Abs(AHeight);
    ATop := ATop + AHeight;
  end;

  R := GetBounds;

  if (R.Left <> ALeft) or (R.Top <> ATop) or
    (R.Right - R.Left <> AWidth) or (R.Bottom - R.Top <> AHeight) then
  begin
    BeginUpdate;
    try
      Offset.x := ALeft - R.Left;
      Offset.y := ATop - R.Top;

      if (Offset.x <> 0) or (offset.y <> 0) then
      begin
        MoveBy(Offset.x, Offset.y);
        RecalculateBounds(True);
      end;

      Resize(AWidth, AHeight);
    finally
      EndUpdate;
    end;
  end;
end;

function TSCDeContainer.IsOnLine(P: TDoublePoint): Boolean;
begin
  Result := False;
end;

procedure TSCDeContainer.DoChanged(Force: Boolean);
begin
  RecalculateBounds(False);
end;

function TSCDeContainer.ControlAt(P: TDoublePoint; Fuzz: Double): Integer;
begin
  Result := -1;
end;

function TSCDeContainer.CanInsertShape(S: TSCDeShapeBase): Boolean;
begin
  Result := (S <> nil) and (S <> Self) and (S.FOwner <> Self) and
    not (S is TSCDeLayer);
end;

procedure TSCDeContainer.SetSurface(ASurface: TSCDeSurface);
var
  I: Integer;
begin
  inherited SetSurface(ASurface);

  if FItems <> nil then
    for I := 0 to FItems.Count-1 do
      TSCDeShapeBase(FItems[I]).SetSurface(ASurface);
end;

procedure TSCDeContainer.BringForward(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (S.FOwner = Self) then
  begin
    Index := S.Index;

    if Index < FItems.Count-1 then
    begin
      StartAction(S, scacBringingForward, '');
      NotifyUndo(S, scacBringingForward, '');

      Changing;

      FItems.Move(Index, Index + 1);
      Changed;

      EndAction(S, scacBringingForward, '');
    end;
  end;
end;

procedure TSCDeContainer.SendBackward(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (S.FOwner = Self) then
  begin
    Index := S.Index;

    if Index > 0 then
    begin
      StartAction(S, scacSendingBackward, '');
      NotifyUndo(S, scacSendingBackward, '');

      Changing;

      FItems.Move(Index, Index - 1);
      Changed;

      EndAction(S, scacSendingBackward, '');
    end;
  end;
end;

procedure TSCDeContainer.DoShapeChanging(S: TSCDeShapeBase);
begin
  ShapeChanging(S);
  Changing;
end;

procedure TSCDeContainer.ShapeChanging(S: TSCDeShapeBase);
begin
  //
end;

procedure TSCDeContainer.LoadBoundsFromXML(Node: TSCDomElement);
var
  S: String;
  R: TDoubleRect;
  Prop: TSCDomElement;
begin
  BeginUpdate;
  try
    R := TSCDeUtils.Rect(0, 0, 0, 0);

    Prop := Node.ElementByName('rect');
    if Prop <> nil then
    begin
      S := GetProperty(Prop, 'left');
      if S <> '' then R.Left := StrToFloat(S);

      S := GetProperty(Prop, 'top');
      if S <> '' then R.Top := StrToFloat(S);

      S := GetProperty(Prop, 'right');
      if S <> '' then R.Right := StrToFloat(S);

      S := GetProperty(Prop, 'bottom');
      if S <> '' then R.Bottom := StrToFloat(S);

      if R.Right < R.Left then R.Right := R.Left;
      if R.Bottom < R.Top then R.Bottom := R.Top;
    end;

    Self.SetBounds(R);
  finally
    EndUpdate;
  end;
end;

procedure TSCDeContainer.SaveBoundsToXML(Node: TSCDomElement);
var
  R: TDoubleRect;
  Prop: TSCDomElement;
begin
  R := GetBounds;

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'rect';
  Node.AddElement(Prop);

  AddProperty(Prop, 'left',   FloatToStr(R.Left));
  AddProperty(Prop, 'top',    FloatToStr(R.Top));
  AddProperty(Prop, 'right',  FloatToStr(R.Right));
  AddProperty(Prop, 'bottom', FloatToStr(R.Bottom));
end;

function TSCDeContainer.ShapeByID(AID: DWord): TSCDeShapeBase;
var
  I: Integer;
  Child: TSCDeShapeBase;
begin
  Result := nil;
  for I := 0 to FItems.Count-1 do
  begin
    Child := TSCDeShapeBase(FItems[I]);

    if Child.ID = AID then
    begin
      Result := Child;
      Exit;
    end;

    if Child is TSCDeContainer then
    begin
      Result := Child.ShapeByID(AID);
      if Result <> nil then
        Exit;
    end;
  end;
end;

procedure TSCDeContainer.DoShapeCleared(S: TSCDeShapeBase);
begin
  ShapeCleared(S);
  if FOwner <> nil then FOwner.DoShapeCleared(S);
  Changed;
end;

procedure TSCDeContainer.ShapeCleared(S: TSCDeShapeBase);
begin
  if FOwner <> nil then FOwner.ShapeCleared(S);
end;

{ TSCDePackage }

constructor TSCDePackage.Create(ASurface: TSCDeSurface;
  AOwner: TSCDeContainer);
begin
  inherited Create(ASurface, AOwner);
  SetShapeStyle(ShapeStyle + [scssUsesBrush, scssUsesPicture] - [scssUsesPen]);
end;

procedure TSCDePackage.LoadFromFile(const FileName: String);

  procedure LoadFromDom(Root: TSCDeContainer; RootElm: TSCDomElement);
  var
    I: Integer;
    S: TSCDeShapeBase;
    SC: TSCDeShapeClass;
    Attr: TSCDomAttribute;
    Elms, Elm: TSCDomElement;
  begin
    if scssIsContainer in Root.ShapeStyle then
    begin
      Elms := RootElm.ElementByName('elements');

      if Elms <> nil then
        for I := 0 to Elms.ChildNodeCount-1 do
          if (Elms.ChildNodes[I].Name = 'element') and (Elms.ChildNodes[I] is TSCDomElement) then
          begin
            Elm := TSCDomElement(Elms.ChildNodes[I]);

            Attr := Elm.AttributeByName('class');
            if Attr <> nil then
            begin
              SC := TSCDeRegister.FindShape(Attr.Value);

              if SC <> nil then
              begin
                S := SC.Create(nil, nil);

                S.StoreID;
                try
                  S.LoadFromXML(Elm);
                finally
                  S.RestoreID;
                end;

                Root.Add(S);

                if S is TSCDeContainer then
                  LoadFromDom(TSCDeContainer(S), Elm);
              end;
            end;
          end;
    end;
  end;

var
  P: TSCDomParser;
  Doc: TSCDomDocument;
  Root: TSCDomElement;
begin
  StartAction(Self, scacChanging, '');
  BeginUpdate;
  try
    NotifyUndo(Self, scacChanging, '');

    BeginNotifyLock;
    try
      Clear;

      if FileExists(FileName) then
      begin
        P := TSCDomParser.Create(nil);
        try
          Doc := P.ParseFile(FileName);
          try
            if Doc <> nil then
            begin
              Root := Doc.ElementByName('package');

              if Root <> nil then
              begin
                Self.StoreID;
                try
                  Self.LoadFromXML(Root);
                  LoadFromDom(Self, Root);
                finally
                  Self.RestoreID;
                end;
              end;
            end;
          finally
            if Doc <> nil then
              Doc.Free;
          end;
        finally
          P.Free;
        end;
      end;
    finally
      EndNotifyLock;
    end;
  finally
    EndUpdate;
  end;

  EndAction(Self, scacChanging, '');
end;

procedure TSCDePackage.LoadFromXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  Changing;
  BeginUpdate;
  try
    inherited LoadFromXML(Node);

    Prop := Node.ElementByName('brush');
    if Prop <> nil then Brush.LoadFromXML(Prop);
  finally
    EndUpdate;
  end;
end;

procedure TSCDePackage.MoveBy(X, Y: Double);
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if ItemCount > 0 then
  begin
    BeginUpdate;
    try
      for I := 0 to ItemCount-1 do
      begin
        S := Items[I];
        S.MoveBy(X, Y);
      end;

      RecalculateBounds(True);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDePackage.Paint(C: TCanvas; X, Y: Integer; Zoom: Double);
var
  R: TRect;
  Rgn: HRGN;
  SR: Integer;
  DR: TDoubleRect;
begin
  DR := GetBounds;
  TSCDeUtils.ZoomRect(DR, Zoom);

  R := TSCDeUtils.Rect(DR);

  if Self.HasBrush and ((R.Left <> R.Right) or (R.Top <> R.Bottom)) then
  begin
    Brush.AssignTo(C.Brush);
    if Brush.Color = clNone then C.Brush.Style := bsClear;

    OffsetRect(R, X, Y);

    if (Brush.Color <> clNone) and (Brush.Style <> bsClear) then
      C.FillRect(R);
  end;

  if not IsRectEmpty(R) and Self.HasPicture then
  begin
    Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    try
      SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
      DeleteObject(Rgn);
      Rgn := 0;

      if SR <> NULLREGION then
        Self.DrawPicture(C, R);
    finally
      SelectClipRgn(C.Handle, 0);
      if Rgn <> 0 then
        DeleteObject(Rgn);
    end;
  end;

  inherited Paint(C, X, Y, Zoom);
end;

procedure TSCDePackage.PaintIn(C: TCanvas; X, Y: Integer; Zoom: Double;
  R: TDoubleRect);
var
  NR: TRect;
  Rgn: HRGN;
  SR: Integer;
  DR: TDoubleRect;
begin
  if not TSCDeUtils.IsRectEmpty(R) then
  begin
    DR := GetBounds;
    TSCDeUtils.ZoomRect(DR, Zoom);

    NR := TSCDeUtils.Rect(DR);

    if (Self.InRect(R) or TSCDeUtils.IntersectRect(DR, DR, R)) and
      Self.HasBrush and ((NR.Left <> NR.Right) or (NR.Top <> NR.Bottom)) then
    begin
      Brush.AssignTo(C.Brush);
      if Brush.Color = clNone then C.Brush.Style := bsClear;

      OffsetRect(NR, X, Y);

      if (Brush.Color <> clNone) and (Brush.Style <> bsClear) then
        C.FillRect(NR);
    end;

    if not IsRectEmpty(NR) and Self.HasPicture then
    begin
      Rgn := CreateRectRgn(NR.Left, NR.Top, NR.Right, NR.Bottom);
      try
        SR := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
        DeleteObject(Rgn);
        Rgn := 0;

        if SR <> NULLREGION then
          Self.DrawPicture(C, NR);
      finally
        SelectClipRgn(C.Handle, 0);
        if Rgn <> 0 then
          DeleteObject(Rgn);
      end;
    end;
  end;

  inherited PaintIn(C, X, Y, Zoom, R);
end;

procedure TSCDePackage.Resize(NewWidth, NewHeight: Double);
var
  I: Integer;
  CR, R: TDoubleRect;
  S: TSCDeShapeBase;
  W, H, L, T, ZoomX, ZoomY: Double;
begin
  if not InPointUpdate and (ItemCount > 0) then
  begin
    CR := GetBounds;

    W := CR.Right - CR.Left;
    if W < 0.0 then W := 0.0;

    H := CR.Bottom - CR.Top;
    if H < 0.0 then H := 0.0;

    if NewWidth < 0.01 then NewWidth := 0.01;
    if NewHeight < 0.01 then NewHeight := 0.01;

    if (W <> NewWidth) or (H <> NewHeight) then
    begin
      ZoomX := 1.0;
      if W <> 0.0 then ZoomX := NewWidth / W;

      ZoomY := 1.0;
      if H <> 0.0 then ZoomY := NewHeight / H;

      if (ZoomX <> 1.0) or (ZoomY <> 1.0) then
      begin
        BeginUpdate;
        try
          for I := 0 to ItemCount-1 do
          begin
            S := Items[I];
            R := S.GetBounds;

            W := R.Right - R.Left;
            if W < 0 then W := 0;

            W := W * ZoomX;

            H := R.Bottom - R.Top;
            if H < 0 then H := 0;

            H := H * ZoomY;

            L := CR.Left + (ZoomX * (R.Left - CR.Left));
            T := CR.Top + (ZoomY * (R.Top - CR.Top));

            S.SetBounds(L, T, W, H);
          end;

          RecalculateBounds(True);
        finally
          EndUpdate;
        end;
      end;  
    end;
  end;
end;

procedure TSCDePackage.SaveToFile(const FileName: String);

  procedure SaveElementsOf(S: TSCDeShapeBase; Root: TSCDomElement);
  var
    I, J: Integer;
    C: TSCDeContainer;
    SS: TSCDeShapeBase;
    Attr: TSCDomAttribute;
    Elms, Elm: TSCDomElement;
  begin
    if (S is TSCDeContainer) and (scssIsContainer in S.ShapeStyle) then
    begin
      C := TSCDeContainer(S);

      if C.ItemCount > 0 then
      begin
        Elms := TSCDomElement.Create(Root);
        Elms.Name := 'elements';

        Root.AddElement(Elms);

        for I := 0 to C.ItemCount-1 do
        begin
          SS := C.Items[I];

          Elm := TSCDomElement.Create(Elms);
          Elm.Name := 'element';
          Elm.AddAttribute('class', SS.ClassName);

          Elms.AddElement(Elm);

          SS.SaveToXML(Elm);

          for J := Elm.AttributeCount-1 downto 0 do
          begin
            Attr := Elm.Attributes[J];
            if (LowerCase(Attr.Name) = 'id') then
              Elm.DeleteAttribute(J);
          end;

          SaveElementsOf(SS, Elm);
        end;
      end;
    end;
  end;

var
  I: Integer;
  Doc: TSCDomDocument;
  Root: TSCDomElement;
  Node: TSCDomTreeNode;
  Attr: TSCDomAttribute;
begin
  if FileName <> '' then
  begin
    Doc := TSCDomDocument.Create(nil);
    try
      Root := TSCDomElement.Create(Doc);
      Root.Name := 'package';
      Root.AddAttribute('version', '1.0');

      Doc.AddElement(Root);

      Self.SaveToXML(Root);

      for I := Root.AttributeCount-1 downto 0 do
      begin
        Attr := Root.Attributes[I];
        if (LowerCase(Attr.Name) = 'id') then
          Root.DeleteAttribute(I);
      end;

      for I := Root.ChildNodeCount-1 downto 0 do
      begin
        Node := Root.ChildNodes[I];
        if (Node is TSCDomElement) and
          ((LowerCase(Node.Name) = 'visible') or
          (LowerCase(Node.Name) = 'locked')) then
           Root.DeleteNode(I);
      end;

      SaveElementsOf(Self, Root);

      Doc.SaveToFile(FileName);
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSCDePackage.SaveToXML(Node: TSCDomElement);
var
  Prop: TSCDomElement;
begin
  inherited SaveToXML(Node);

  Prop := TSCDomElement.Create(Node);
  Prop.Name := 'brush';
  Node.AddElement(Prop);

  Brush.SaveToXML(Prop);
end;

{ TSCDeGroup }

procedure TSCDeGroup.MoveBy(X, Y: Double);
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if ItemCount > 0 then
  begin
    BeginUpdate;
    try
      for I := 0 to ItemCount-1 do
      begin
        S := Items[I];
        S.MoveBy(X, Y);
      end;

      RecalculateBounds(True);
    finally
      EndUpdate;
    end;
  end;
end;

function TSCDeGroup.PointOnShape(P: TDoublePoint; Fuzz: Double;
  Editing: Boolean): Boolean;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  Result := False;
  for I := 0 to ItemCount-1 do
  begin
    S := Items[I];
    
    Result := S.PointOnShape(P, Fuzz, Editing);
    if Result then
      Exit;
  end;
end;

procedure TSCDeGroup.Resize(NewWidth, NewHeight: Double);
var
  I: Integer;
  CR, R: TDoubleRect;
  S: TSCDeShapeBase;
  W, H, L, T, ZoomX, ZoomY: Double;
begin
  if not InPointUpdate and (ItemCount > 0) then
  begin
    CR := GetBounds;

    W := CR.Right - CR.Left;
    if W < 0.0 then W := 0.0;

    H := CR.Bottom - CR.Top;
    if H < 0.0 then H := 0.0;

    if NewWidth < 0.01 then NewWidth := 0.01;
    if NewHeight < 0.01 then NewHeight := 0.01;

    if (W <> NewWidth) or (H <> NewHeight) then
    begin
      ZoomX := 1.0;
      if W <> 0.0 then ZoomX := NewWidth / W;

      ZoomY := 1.0;
      if H <> 0.0 then ZoomY := NewHeight / H;

      if (ZoomX <> 1.0) or (ZoomY <> 1.0) then
      begin
        BeginUpdate;
        try
          for I := 0 to ItemCount-1 do
          begin
            S := Items[I];
            R := S.GetBounds;

            W := R.Right - R.Left;
            if W < 0 then W := 0;

            W := W * ZoomX;

            H := R.Bottom - R.Top;
            if H < 0 then H := 0;

            H := H * ZoomY;

            L := CR.Left + (ZoomX * (R.Left - CR.Left));
            T := CR.Top + (ZoomY * (R.Top - CR.Top));

            S.SetBounds(L, T, W, H);
          end;

          RecalculateBounds(True);
        finally
          EndUpdate;
        end;
      end;  
    end;
  end;
end;

{ TSCDeLayer }

function TSCDeLayer.ControlAt(P: TDoublePoint; Fuzz: Double): Integer;
begin
  Result := -1;
end;

constructor TSCDeLayer.Create(AOwner: TSCDeSurface);
begin
  inherited Create(AOwner, nil);
  if AOwner <> nil then AOwner.InsertLayer(Self);
end;

destructor TSCDeLayer.Destroy;
begin
  Destroying;
  if FSurface <> nil then FSurface.RemoveLayer(Self);
  inherited Destroy;
end;

procedure TSCDeLayer.DoChanged(Force: Boolean);
begin
  if not InDestroy and (Force or Visible) and (FSurface <> nil) then
    FSurface.LayerChanged(Self);
end;

procedure TSCDeLayer.DrawSelection(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  Zoom: Double);
begin
  //
end;

procedure TSCDeLayer.DrawSelectionIn(C: TCanvas; X, Y: Double;
  const DragPoint: TSCDeDragPoint; const Data: TSCDeSelectionData;
  R: TDoubleRect; Zoom: Double);
begin
  //
end;

procedure TSCDeLayer.EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
  const PropName: String);
begin
  if (S <> nil) and (FSurface <> nil) and (FNotifyLock = 0) then
    FSurface.EndAction(S, Action, PropName);
end;

function TSCDeLayer.GetIndex: Integer;
begin
  Result := -1;
  if FSurface <> nil then Result := FSurface.IndexOf(Self);
end;

function TSCDeLayer.GetOwner: TPersistent;
begin
  Result := FSurface;
end;

function TSCDeLayer.GetPointValue(Index: Integer): TDoublePoint;
begin
  Result := TSCDeUtils.Point(0, 0);
end;

function TSCDeLayer.GetSurface: TSCDeSurface;
begin
  Result := FSurface;
end;

procedure TSCDeLayer.LoadBoundsFromXML(Node: TSCDomElement);
begin
  //
end;

procedure TSCDeLayer.MoveBy(X, Y: Double);
begin
  //
end;

procedure TSCDeLayer.NotifyUndo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);
begin
  if (S <> nil) and (FSurface <> nil) and (FNotifyLock = 0) then
    FSurface.NotifyUndo(S, Action, PropName);
end;

procedure TSCDeLayer.OffsetPosition(Index: Integer; Offset: TDoublePoint);
begin
  //
end;

function TSCDeLayer.PointAt(P: TDoublePoint; Fuzz: Double): Integer;
begin
  Result := -1;
end;

procedure TSCDeLayer.RecalculateBounds(Force: Boolean);
begin
  //
end;

procedure TSCDeLayer.Resize(NewWidth, NewHeight: Double);
begin
  //
end;

procedure TSCDeLayer.SaveBoundsToXML(Node: TSCDomElement);
begin
  //
end;

procedure TSCDeLayer.SetPointValue(Index: Integer; Value: TDoublePoint);
begin
  //
end;

procedure TSCDeLayer.ShapeCleared(S: TSCDeShapeBase);
begin
  inherited ShapeCleared(S);
  if FSurface <> nil then FSurface.ShapeCleared(S);
end;

procedure TSCDeLayer.ShapeInserted(S: TSCDeShapeBase);
begin
  inherited ShapeInserted(S);
  if FSurface <> nil then FSurface.ShapeInserted(S);
end;

procedure TSCDeLayer.ShapeRemoved(S: TSCDeShapeBase);
begin
  inherited ShapeRemoved(S);
  if FSurface <> nil then FSurface.ShapeRemoved(S);
end;

procedure TSCDeLayer.StartAction(S: TSCDeShapeBase; Action: TSCDeAction;
  const PropName: String);
begin
  if (S <> nil) and (FSurface <> nil) and (FNotifyLock = 0) then
    FSurface.StartAction(S, Action, PropName);
end;

{ TSCDeScrollbar }

constructor TSCDeScrollbar.Create(AOwner: TSCDeCustomControl;
  AKind: TSCDeScrollbarKind);
begin
  inherited Create(AOwner, AKind);
  Sensitivity := 120;
  Track := False;
end;

function TSCDeScrollbar.GetScrollPos: Integer;
begin
  Result := 0;
  if Visible then Result := Position;
end;

function TSCDeScrollbar.GetSensitivity: Integer;
begin
  Result := -1;
  if not Track then Result := inherited GetSensitivity;
end;

{ TSCDeBorderProps }

constructor TSCDeBorderProps.Create(AOwner: TSCDeCustomControl);
begin
  inherited Create(AOwner);
  Border := scdcb3DLowered;
end;

{ TSCDeLayerProperties }

procedure TSCDeLayerProperties.Assign(Source: TPersistent);
begin
  if Source is TSCDeLayerProperties then
  begin
    with TSCDeLayerProperties(Source) do
    begin
      Self.FColor := Color;
      Self.FWidth := Width;
      Self.FHeight := Height;
      Self.FTransparency := Transparency;
      Self.FCheckers.Assign(Checkers);
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeLayerProperties.Changed(Scrollbars: Boolean);
begin
  if FOwner <> nil then FOwner.LayerPropsChanged(Scrollbars);
end;

procedure TSCDeLayerProperties.CheckersChanged;
begin
  if FTransparency = scltChecked then
    Changed;
end;

constructor TSCDeLayerProperties.Create(AOwner: TSCDeSurface);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clWhite;
  FWidth := 400;
  FHeight := 400;
  FTransparency := scltNone;
  FCheckers := TSCDeCheckersProperties.Create(Self);
end;

destructor TSCDeLayerProperties.Destroy;
begin
  FreeAndNil(FCheckers);
  inherited Destroy;
end;

procedure TSCDeLayerProperties.SetBounds(AWidth, AHeight: Integer);
begin
  if AHeight < 0 then AHeight := 0;
  if AWidth < 0 then AWidth := 0;

  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FHeight := AHeight;
    FWidth := AWidth;
    
    Changed;
  end;
end;

procedure TSCDeLayerProperties.SetCheckers(Value: TSCDeCheckersProperties);
begin
  FCheckers.Assign(Value);
end;

procedure TSCDeLayerProperties.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeLayerProperties.SetHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TSCDeLayerProperties.SetTransparency(Value: TSCDeLayerTransparency);
begin
  if FTransparency <> Value then
  begin
    FTransparency := Value;
    Changed;
  end;
end;

procedure TSCDeLayerProperties.SetWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TSCDeGridProperties }

procedure TSCDeGridProperties.Assign(Source: TPersistent);
begin
  if Source is TSCDeGridProperties then
  begin
    with TSCDeGridProperties(Source) do
    begin
      Self.FGridType := GridType;
      Self.FColor := Color;
      Self.FGroupColor := GroupColor;
      Self.FX := X;
      Self.FY := Y;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeGridProperties.Changed;
begin
  if FOwner <> nil then FOwner.GridPropsChanged;
end;

constructor TSCDeGridProperties.Create(AOwner: TSCDeSurface);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := $00A6E4FF;
  FGridType := scgtLine;
  FGroupColor := $003CC5FF;
  FGroupCount := 5;
  FX := 20;
  FY := 20;
end;

procedure TSCDeGridProperties.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FGridType <> scgtNone then
      Changed;
  end;
end;

procedure TSCDeGridProperties.SetGridType(Value: TSCDeGridType);
begin
  if FGridType <> Value then
  begin
    FGridType := Value;
    Changed;
  end;
end;

procedure TSCDeGridProperties.SetGroupColor(Value: TColor);
begin
  if FGroupColor <> Value then
  begin
    FGroupColor := Value;
    if FGridType = scgtLine then
      Changed;
  end;
end;

procedure TSCDeGridProperties.SetGroupCount(Value: Word);
begin
  if Value < 2 then Value := 2;

  if FGroupCount <> Value then
  begin
    FGroupCount := Value;
    if FGridType = scgtLine then
      Changed;
  end;
end;

procedure TSCDeGridProperties.SetX(Value: Integer);
begin
  if Value < 2 then Value := 2;

  if FX <> Value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TSCDeGridProperties.SetY(Value: Integer);
begin
  if Value < 2 then Value := 2;

  if FY <> Value then
  begin
    FY := Value;
    Changed;
  end;
end;

{ TSCDeFrameProperties }

procedure TSCDeFrameProperties.Assign(Source: TPersistent);
var
  Sp: TSCDeFrameProperties;
begin
  if Source is TSCDeFrameProperties then
  begin
    Sp := TSCDeFrameProperties(Source);

    with Sp do
    begin
      Self.FColor := Color;
      Self.FShadowColor := ShadowColor;
      Self.FShadowOffset := ShadowOffset;
      Self.FVisible := Visible;
    end;

    Changed(Self.Visible <> Sp.Visible);
  end else
    inherited Assign(Source);
end;

procedure TSCDeFrameProperties.Changed(Force: Boolean);
begin
  if (Force or FVisible) and (FOwner <> nil) then
    FOwner.FramePropsChanged;
end;

constructor TSCDeFrameProperties.Create(AOwner: TSCDeSurface);
begin
  inherited Create;
  FOwner := AOwner;

  FColor := clNone;
  FShadowColor := clBlack;
  FShadowOffset := 3;
  FVisible := True;
end;

procedure TSCDeFrameProperties.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeFrameProperties.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    if FShadowOffset > 0 then
      Changed;
  end;
end;

procedure TSCDeFrameProperties.SetShadowOffset(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    if FShadowColor <> clNone then
      Changed;
  end;
end;

procedure TSCDeFrameProperties.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

{ TSCDeSurface }

procedure TSCDeSurface.Add(L: TSCDeLayer);
begin
  InsertLayer(L);
end;

procedure TSCDeSurface.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeSurface.BringToFront(L: TSCDeLayer);
var
  Index: Integer;
begin
  if (L <> nil) and (L.Surface = Self) then
  begin
    Index := L.Index;

    if Index < FLayers.Count - 1 then
    begin
      FLayers.Move(Index, FLayers.Count);
      LayerChanged(nil);
    end;
  end;
end;

procedure TSCDeSurface.Clear;
begin
  BeginUpdate;
  try
    while FLayers.Count > 0 do
      TObject(FLayers[FLayers.Count-1]).Free;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeSurface.CMColorChanged(var Message: TMessage);
begin
  inherited;
  RefreshBuffers(True, False);
end;

constructor TSCDeSurface.Create(AOwner: TComponent);
begin
  FClients := TList.Create;

  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks];

  FShowScrollbars := True;
  FZoom := 100;

  FClientBuffer := TSCDeBitmap.Create;
  FLayerBuffer := TSCDeBitmap.Create;

  FLayers := TList.Create;

  FFrameProperties := TSCDeFrameProperties.Create(Self);
  FGridProperties := TSCDeGridProperties.Create(Self);
  FLayerProperties := TSCDeLayerProperties.Create(Self);

  SetBounds(Left, Top, 400, 350);
  ParentColor := False;
  Color := clGray;
  Border := scdcb3DLowered;
  Indent := 20;
  ClickFocus := True;

  RefreshBuffers(True, False);
end;

procedure TSCDeSurface.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < FLayers.Count) then
    TSCDeLayer(FLayers[Index]).Free;
end;

destructor TSCDeSurface.Destroy;
begin
  Destroying;

  UnRegisterAllClients;

  Clear;
  FreeAndNil(FLayers);

  FreeAndNil(FClientBuffer);
  FreeAndNil(FLayerBuffer);

  FreeAndNil(FFrameProperties);
  FreeAndNil(FGridProperties);
  FreeAndNil(FLayerProperties);
  inherited Destroy;
end;

procedure TSCDeSurface.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FChangeCount > 0) and (FUpdateCount = 0) then
      Changed(FForced);
  end;
end;

function TSCDeSurface.GetBorderPropsClass: TSCDeControlBorderPropsClass;
begin
  Result := TSCDeBorderProps;
end;

function TSCDeSurface.GetControlScrollbarsClass: TSCDeControlCustomScrollbarsClass;
begin
  Result := TSCDeScrollbars;
end;

function TSCDeSurface.GetLayer(Index: Integer): TSCDeLayer;
begin
  Result := TSCDeLayer(FLayers[Index]);
end;

function TSCDeSurface.GetScrollbarClass: TSCDeCustomControlScrollbarClass;
begin
  Result := TSCDeScrollbar;
end;

function TSCDeSurface.IndexOf(L: TSCDeLayer): Integer;
begin
  Result := FLayers.IndexOf(L);
end;

procedure TSCDeSurface.Insert(Index: Integer; L: TSCDeLayer);
begin
  if (L <> nil) and (Index > -1) and (Index <= FLayers.Count) and
    ((L.Surface <> Self) or (FLayers.IndexOf(L) = -1)) then
  begin
    BeginUpdate;
    try
      InsertLayer(L);
      FLayers.Move(FLayers.Count-1, Index);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeSurface.InsertLayer(L: TSCDeLayer);
begin
  if (L <> nil) and ((L.Surface <> Self) or (FLayers.IndexOf(L) = -1)) then
  begin
    if L.Surface <> nil then L.Surface.RemoveLayer(L);
    L.FSurface := Self;
    FLayers.Add(L);

    LayerInserted(L);

    if (FActiveLayer = nil) and (FLayers.Count > 0) then
      SetActiveLayer(TSCDeLayer(FLayers[0]));
  end;
end;

procedure TSCDeSurface.LayerChanged(L: TSCDeLayer);
var
  I: Integer;
  SetActive: Boolean;
begin
  SetActive := (FActiveLayer = L) or
    (FActiveLayer = nil) or FActiveLayer.Locked;

  Changed(True);
  DoLayerChanged(L);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoLayerChange(L);

  if Assigned(FOnLayerChange) then
    FOnLayerChange(Self, L);

  if SetActive then
    SetActiveLayer(GetActiveLayer);
end;

procedure TSCDeSurface.Paint;
var
  CR, R: TRect;
  I, J, XCount,
  YCount, Size, W, H: Integer;
begin
  CR := ClientRect;

  W := CR.Right - CR.Left;
  H := CR.Bottom - CR.Top;

  if (FClientBuffer.Width <> W) or (FClientBuffer.Height <> H) then
    RefreshBuffers(False, True);

  Size := 200;

  XCount := W div Size;
  YCount := H div Size;

  R := Rect(0, 0, Size, Size);
  OffsetRect(R, CR.Left, CR.Top);

  for I := 0 to YCount do
  begin
    R := Rect(0, 0, Size, Size);
    OffsetRect(R, CR.Left, CR.Top + I*Size);

    if R.Bottom > CR.Bottom then
    begin
      R.Bottom := CR.Bottom;
      if IsRectEmpty(R) then
        Exit;
    end;

    if R.Top >= CR.Bottom then
      Exit;

    for J := 0 to XCount do
    begin
      if R.Right > CR.Right then
      begin
        R.Right := CR.Right;
        if IsRectEmpty(R) then
          Break;
      end;

      if R.Left >= CR.Right then
        Break;

      W := R.Right - R.Left;
      H := R.Bottom - R.Top;

      BitBlt(Canvas.Handle, R.Left, R.Top, W, H, FClientBuffer.Canvas.Handle,
        R.Left - CR.Left, R.Top - CR.Top, SRCCOPY);

      OffsetRect(R, Size, 0);
    end;
  end;
end;

function TSCDeSurface.ShapeAtPos(P: TDoublePoint; Fuzz: Byte): TSCDeShapeBase;
var
  I: Integer;
  L: TSCDeLayer;
begin
  Result := nil;
  for I := FLayers.Count-1 downto 0 do
  begin
    L := TSCDeLayer(FLayers[I]);
    if L.Visible then
    begin
      Result := L.ShapeAtPos(P, Fuzz, True);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure TSCDeSurface.Remove(L: TSCDeLayer);
begin
  if (L <> nil) and (L.Surface = Self) then L.Free;
end;

procedure TSCDeSurface.RemoveLayer(L: TSCDeLayer);
var
  I: Integer;
  AL: TSCDeLayer;
begin
  if (L <> nil) and (L.Surface = Self) then
  begin
    L.FSurface := nil;
    FLayers.Remove(L);

    LayerRemoved(L);

    if L = FActiveLayer then
    begin
      AL := nil;
      if FLayers.Count > 0 then
        for I := 0 to FLayers.Count-1 do
        begin
          L := TSCDeLayer(FLayers[I]);
          if L.Visible and not L.InDestroy then
          begin
            AL := L;
            Break;
          end;
        end;

      SetActiveLayer(AL);  
    end;
  end;
end;

procedure TSCDeSurface.SendToBack(L: TSCDeLayer);
var
  Index: Integer;
begin
  if (L <> nil) and (L.Surface = Self) then
  begin
    Index := L.Index;

    if Index > 0 then
    begin
      FLayers.Move(Index, 0);
      LayerChanged(nil);
    end;
  end;
end;

procedure TSCDeSurface.SetGridProperties(Value: TSCDeGridProperties);
begin
  FGridProperties.Assign(Value);
end;

procedure TSCDeSurface.SetLayerProperties(Value: TSCDeLayerProperties);
begin
  FLayerProperties.Assign(Value);
end;

procedure TSCDeSurface.ShapeRemoved(S: TSCDeShapeBase);
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
    DoShapeRemoved(S);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoRemoveShape(S);

  if Assigned(FOnRemoveShape) then FOnRemoveShape(Self, S);
end;

procedure TSCDeSurface.UpdateLayerBuffer;
var
  DC: HDC;
  Z: Double;
  CR, IR: TRect;
  Ly: TSCDeLayer;
  R, R2, R3: TDoubleRect;
  C, Cl, BCl, CCl, LCl, GCl: TColor;
  I, J, L, T, W, H, Hrz, Vrt, Gx, Gy, Ox, Oy: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  BeforeLayerPaint(FLayerBuffer.Canvas);

  Inc(FUpdatingBuffer);
  try
    LayerBounds(CR, R);

    Z := GetZoom;

    if not TSCDeUtils.IsRectEmpty(R) then
    begin
      R3 := R;
      OffsetRect(CR, -CR.Left, -CR.Top);

      if R3.Right > CR.Right then R3.Right := CR.Right + 1;
      if R3.Bottom > CR.Bottom then R3.Bottom := CR.Bottom + 1;

      Ox := 0;
      Oy := 0;

      if R.Left < 0.0 then
      begin
        Ox := TSCDeUtils.Round(R.Left);
        R.Left := 0.0;
      end;

      if R.Top < 0.0 then
      begin
        Oy := TSCDeUtils.Round(R.Top);
        R.Top := 0.0;
      end;

      if R.Right > CR.Right then R.Right := CR.Right + 1;
      if R.Bottom > CR.Bottom then R.Bottom := CR.Bottom + 1;

      TSCDeUtils.OffsetRect(R, -R.Left, -R.Top);

      if R.Right < 0.0 then R.Right := 0.0;
      if R.Bottom < 0.0 then R.Bottom := 0.0;

      IR := TSCDeUtils.Rect(R);

      FLayerBuffer.SetBounds(IR.Right, IR.Bottom);

      FLayerBuffer.Canvas.Lock;
      try
        C := Self.Color;
        if C = clNone then C := clGray;

        with FLayerBuffer do
        begin
          BCl := FLayerProperties.Color;
          CCl := FLayerProperties.Checkers.ColorOdd;

          if (BCl = clNone) or (FLayerProperties.Transparency = scltFull) then
            BCl := C
          else if FLayerProperties.Transparency = scltChecked then
          begin
            BCl := FLayerProperties.Checkers.ColorEven;
            if BCl = clNone then BCl := C;

            if CCl = clNone then CCl := BCl;
          end;

          with Canvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := BCl;

            FillRect(IR);
          end;

          with FLayerProperties.Checkers do
          begin
            W := Round(Z * Width);
            H := Round(Z * Height);
          end;

          if (FLayerProperties.Transparency = scltChecked) and
            (CCl <> BCl) and (W > 1) and (H > 1) then
          begin
            with Canvas do
            begin
              Brush.Style := bsSolid;
              Brush.Color := CCl;
            end;

            Hrz := TSCDeUtils.Round((R3.Right - R3.Left) / W);
            Vrt := TSCDeUtils.Round((R3.Bottom - R3.Top) / H);

            for I := 0 to Hrz do
            begin
              L := Ox + I*W;

              if (L >= R.Left) and (L < R.Right) then
                for J := 0 to Vrt do
                begin
                  T := Oy + J*H;

                  if (Odd(J) <> Odd(I)) and (T >= R.Top) and (T < R.Bottom) then
                  begin
                    IR := Rect(L, T, L + W, T + H);
                    Canvas.FillRect(IR);
                  end;
                end;
            end;
          end;

          DrawPicture(FLayerBuffer.Canvas);

          Gx := TSCDeUtils.Round(Z * FGridProperties.X);
          Gy := TSCDeUtils.Round(Z * FGridProperties.Y);

          if ((Gx > 1) and (Gy > 1)) or ((Z < 1.0) and (Gx > 0) and (Gy > 0)) then
          begin
            Gx := FGridProperties.X;
            Gy := FGridProperties.Y;

            Hrz := TSCDeUtils.Round((R3.Right - R3.Left) / (Z * Gx));
            Vrt := TSCDeUtils.Round((R3.Bottom - R3.Top) / (Z * Gy));

            if FGridProperties.GridType = scgtDot then
            begin
              DC := Canvas.Handle;

              for I := 0 to Hrz do
              begin
                L := Ox + TSCDeUtils.Round(I*Z*Gx);
                if (L < R.Left) or (L > R.Right) then
                  Continue;

                for J := 0 to Vrt do
                begin
                  T := Oy + TSCDeUtils.Round(J*Z*Gy);
                  if (T >= R.Top) and (T < R.Bottom) then
                    PatBlt(DC, L, T, 1, 1, PATINVERT);
                end;
              end;
            end else
            if FGridProperties.GridType = scgtLine then
            begin
              LCl := FGridProperties.Color;
              GCl := FGridProperties.GroupColor;

              if (LCl <> BCl) and ((LCl <> clNone) or (GCl <> clNone)) then
              begin
                IR := TSCDeUtils.Rect(R);

                Cl := GCl;
                if Cl = clNone then Cl := LCl;

                with Canvas do
                begin
                  Brush.Style := bsClear;

                  Pen.Width := 1;
                  Pen.Color := Cl;
                  Pen.Mode  := pmCopy;
                  Pen.Style := psSolid;

                  Rectangle(IR.Left, IR.Top, IR.Right, IR.Bottom);
                end;

                IR := TSCDeUtils.Rect(R);

                for I := 0 to Hrz do
                begin
                  L := Ox + TSCDeUtils.Round(I*Z*Gx);

                  Cl := LCl;
                  if FGridProperties.GroupColor <> clNone then
                  begin
                    if (I mod FGridProperties.GroupCount) = 0 then
                    begin
                      Cl := GCl;
                      if Cl = clNone then
                        Cl := GCl;
                    end;

                    Canvas.Pen.Color := Cl;
                  end;

                  if (Cl <> clNone) and (L >= IR.Left) and (L < IR.Right) then
                    with Canvas do
                    begin
                      MoveTo(L, IR.Top);
                      LineTo(L, IR.Bottom);
                    end;
                end;

                for I := 0 to Vrt do
                begin
                  T := Oy + TSCDeUtils.Round(I*Z*Gy);

                  Cl := LCl;
                  if FGridProperties.GroupColor <> clNone then
                  begin
                    if (I mod FGridProperties.GroupCount) = 0 then
                    begin
                      Cl := GCl;
                      if Cl = clNone then
                        Cl := GCl;
                    end;

                    Canvas.Pen.Color := Cl;
                  end;

                  if (Cl <> clNone) and (T >= IR.Top) and (T < IR.Bottom) then
                    with Canvas do
                    begin
                      MoveTo(IR.Left, T);
                      LineTo(IR.Right, T);
                    end;
                end;
              end;
            end;
          end;

          R2  := GetLayerRect;

          for I := 0 to FLayers.Count-1 do
          begin
            Ly := TSCDeLayer(FLayers[I]);

            if Ly.Visible and not Ly.InDestroy then
              Ly.PaintIn(Canvas, Ox, Oy, Z, R2);
          end;
        end;
      finally
        FLayerBuffer.Canvas.Unlock;
      end;

      AfterLayerPaint(FLayerBuffer.Canvas);
    end;
  finally
    Dec(FUpdatingBuffer);
  end;
end;

procedure TSCDeSurface.UpdateScrollBars;
var
  H, W: Integer;
  CR, IR: TRect;
  R: TDoubleRect;
  HorzChanged, VertChanged: Boolean;
  SiHOld, SiHNew, SiVOld, SiVNew: TSCDeScrollInfo;
  HorzIsVisible, VertIsVisible, HorzWasVisible, VertWasVisible: Boolean;
begin
  if (FScrollbarUpdate = 0) and (FCreatingWnd = 0) and
    HandleAllocated and not (csLoading in ComponentState) then
  begin
    SiHOld := Self.GetScrollbarInfo(scdskHorizontal);
    HorzWasVisible := SiHOld.Visible and (SiHOld.Page <= SiHOld.Max);

    SiVOld := Self.GetScrollbarInfo(scdskVertical);
    VertWasVisible := SiVOld.Visible and (SiVOld.Page <= SiVOld.Max);

    SiHNew := SiHOld;

    SiHNew.Page := 1;
    SiHNew.Min  := 0;
    SiHNew.Max  := 0;
    SiHNew.Pos  := 0;

    SiVNew := SiVOld;

    SiVNew.Page := 1;
    SiVNew.Min  := 0;
    SiVNew.Max  := 0;
    SiVNew.Pos  := 0;

    if FShowScrollbars then
    begin
      // Calculate Bounds
      LayerBounds(CR, R);
      IR := TSCDeUtils.Rect(R);

      CR := GetPaintRect;
      OffsetRect(CR, -CR.Left, -CR.Top);

      H := GetHorzScrollbarHeight;
      W := GetVertScrollbarWidth;

      if HorzWasVisible then
      begin
        Inc(CR.Bottom, H);
        if CR.Bottom < CR.Top then CR.Bottom := CR.Top;
      end;

      if VertWasVisible then
      begin
        Inc(CR.Right, W);
        if CR.Right < CR.Left then CR.Right := CR.Left;
      end;

      InflateRect(CR, -Indent, -Indent);

      OffsetRect(CR, -CR.Left, -CR.Top);
      OffsetRect(IR, -IR.Left, -IR.Top);

      // Calculate Horz Scrollbar
      HorzIsVisible := IR.Right > CR.Right;
      if HorzIsVisible then Dec(CR.Bottom, H);

      // Calculate Horz Scrollbar
      VertIsVisible := IR.Bottom > CR.Bottom;
      if VertIsVisible then Dec(CR.Right, W);

      if not HorzIsVisible and VertIsVisible then
      begin
        HorzIsVisible := IR.Right > CR.Right;
        if HorzIsVisible then Dec(CR.Bottom, H);
      end;

      // Recalculate Horz Scrollbar
      if HorzIsVisible then
      begin
        SiHNew.Page := CR.Right;
        SiHNew.Min  := 0;
        SiHNew.Max  := IR.Right;
      end;

      if SiHNew.Max < 0 then SiHNew.Max := 0;
      SiHNew.Pos := -FHorizontalPos;

      SiHNew.SmallChange := 10;
      SiHNew.LargeChange := SiHNew.Page div 10;

      if SiHNew.LargeChange < 2*SiHNew.SmallChange then
        SiHNew.LargeChange := 2*SiHNew.SmallChange;

      // Recalculate Vert Scrollbar
      if VertIsVisible then
      begin
        SiVNew.Page := CR.Bottom;
        SiVNew.Min  := 0;
        SiVNew.Max  := IR.Bottom;
      end;

      if SiVNew.Max < 0 then SiVNew.Max := 0;
      SiVNew.Pos := -FVerticalPos;

      SiVNew.SmallChange := 10;
      SiVNew.LargeChange := SiVNew.Page div 10;

      if SiVNew.LargeChange < 2*SiVNew.SmallChange then
        SiVNew.LargeChange := 2*SiVNew.SmallChange;
    end;

    HorzChanged := (SiHNew.Min <> SiHOld.Min) or (SiHNew.Max <> SiHOld.Max) or
      (SiHNew.Page <> SiHOld.Page) or (SiHNew.Pos <> SiHOld.Pos);
    HorzIsVisible := HorzChanged and (SiHNew.Page <= SiHNew.Max);

    VertChanged := (SiVNew.Min <> SiVOld.Min) or (SiVNew.Max <> SiVOld.Max) or
      (SiVNew.Page <> SiVOld.Page) or (SiVNew.Pos <> SiVOld.Pos);
    VertIsVisible := VertChanged and (SiVNew.Page <= SiVNew.Max);

    SiHNew.Visible := HorzIsVisible;
    SiVNew.Visible := VertIsVisible;

    Inc(FScrollbarUpdate);
    try
      // Set Horz Scrollbar
      if HorzChanged then
      begin
        Self.SetScrollbarInfo(scdskHorizontal, SiHNew);

        if Integer(SiHNew.Page) > SiHNew.Max then
          SetHorizontalPos(SiHNew.Min);
      end;

      // Set Vert Scrollbar
      if VertChanged then
      begin
        Self.SetScrollbarInfo(scdskVertical, SiVNew);

        if Integer(SiVNew.Page) > SiVNew.Max then
          SetVerticalPos(SiVNew.Min);
      end;
    finally
      Dec(FScrollbarUpdate);
    end;
  end;
end;

procedure TSCDeSurface.WMSize(var Message: TWMSize);
begin
  inherited;
  BeginUpdate;
  try
    UpdateScrollBars;
    ValidateLayerOffset(FHorizontalPos, FVerticalPos);

    RefreshBuffers(True, False);
  finally
    EndUpdate;
  end;
end;

function TSCDeSurface.Exists(S: TSCDeShapeBase): Boolean;
var
  I: Integer;
  L: TSCDeLayer;
begin
  Result := False;
  if S <> nil then
  begin
    for I := 0 to FLayers.Count-1 do
    begin
      Result := FLayers[I] = S;
      if Result then
        Exit;
    end;

    for I := 0 to FLayers.Count-1 do
    begin
      L := TSCDeLayer(FLayers[I]);
      Result := L.Exists(S);

      if Result then
        Exit;
    end;
  end;
end;

procedure TSCDeSurface.AfterUpdateBuffers(C: TCanvas);
begin
  //
end;

procedure TSCDeSurface.AfterLayerPaint(C: TCanvas);
begin
  //
end;

procedure TSCDeSurface.SetFrameProperties(const Value: TSCDeFrameProperties);
begin
  FFrameProperties.Assign(Value);
end;

procedure TSCDeSurface.DrawSelection(C: TCanvas);
begin
  //
end;

procedure TSCDeSurface.DrawLayerFrame(C: TCanvas);
var
  CR, IR: TRect;
  R, R2: TDoubleRect;
begin
  if (C <> nil) and FFrameProperties.Visible then
  begin
    LayerBounds(CR, R);

    if not (IsRectEmpty(CR) or TSCDeUtils.IsRectEmpty(R))then
    begin
      if (FFrameProperties.ShadowOffset > 0) and (FFrameProperties.ShadowColor <> clNone) then
        with C do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FFrameProperties.ShadowColor;

          // Draw right shadow
          if R.Right < CR.Right then
          begin
            R2 := R;

            R2.Left := R2.Right;
            R2.Right := R2.Right + FFrameProperties.ShadowOffset;
            TSCDeUtils.OffsetRect(R2, 0, FFrameProperties.ShadowOffset);

            IR := TSCDeUtils.Rect(R2);

            FillRect(IR);
          end;

          // Draw bottom shadow
          if R.Bottom < CR.Bottom then
          begin
            R2 := R;

            R2.Top := R2.Bottom;
            R2.Bottom := R2.Bottom + FFrameProperties.ShadowOffset;
            TSCDeUtils.OffsetRect(R2, FFrameProperties.ShadowOffset, 0);

            IR := TSCDeUtils.Rect(R2);

            FillRect(IR);
          end;  
        end;

      if FFrameProperties.Color <> clNone then
        with C do
        begin
          Brush.Style := bsClear;

          Pen.Width := 1;
          Pen.Color := FFrameProperties.Color;
          Pen.Mode  := pmCopy;
          Pen.Style := psSolid;

          IR := TSCDeUtils.Rect(R);

          if (IR.Top >= CR.Top) and (IR.Top < CR.Bottom) then
          begin
            MoveTo(IR.Left, IR.Top);
            LineTo(IR.Right, IR.Top);
          end;

          if (IR.Bottom >= CR.Top) and (IR.Bottom < CR.Bottom) then
          begin
            MoveTo(IR.Left, IR.Bottom);
            LineTo(IR.Right, IR.Bottom);
          end;

          if (IR.Left >= CR.Left) and (IR.Left < CR.Right) then
          begin
            MoveTo(IR.Left, IR.Top);
            LineTo(IR.Left, IR.Bottom);
          end;

          if (IR.Right >= CR.Left) and (IR.Right < CR.Right) then
          begin
            MoveTo(IR.Right, IR.Top);
            LineTo(IR.Right, IR.Bottom);
          end;
        end;
    end;
  end;
end;

procedure TSCDeSurface.DoLayerChanged(L: TSCDeLayer);
begin
  //
end;

procedure TSCDeSurface.DoShapeRemoved(S: TSCDeShapeBase);
begin
  //
end;

function TSCDeSurface.GetLayerCount: Integer;
begin
  Result := FLayers.Count;
end;

procedure TSCDeSurface.ShapeInserted(S: TSCDeShapeBase);
var
  I: Integer;
begin
  DoShapeInserted(S);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoInsertShape(S);

  if Assigned(FOnInsertShape) then FOnInsertShape(Self, S);
end;

procedure TSCDeSurface.DoShapeInserted(S: TSCDeShapeBase);
begin
  //
end;

procedure TSCDeSurface.Changed(Force: Boolean);
var
  Forced: Boolean;
  OldHorz, OldVert: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    FForced := FForced or Force;

    if FUpdateCount > 0 then
    begin
      Inc(FChangeCount);
      Exit;
    end;

    Forced := FForced;
    FForced := False;

    FChangeCount := 0;

    OldHorz := FHorizontalPos;
    OldVert := FVerticalPos;

    ValidateLayerOffset(FHorizontalPos, FVerticalPos);

    Forced := Forced or (OldHorz <> FHorizontalPos) or (OldVert <> FVerticalPos);

    UpdateScrollBars;
    RefreshBuffers(Forced, False);
  end;
end;

function TSCDeSurface.Pack(A: array of TSCDeShapeBase): TSCDePackage;
var
  I: Integer;
  L, PL: TList;
  Ly: TSCDeLayer;
  P: TSCDePackage;
  S: TSCDeShapeBase;
begin
  Result := nil;

  if (Length(A) > 0) and (LayerCount > 0) then
  begin
    L := TList.Create;
    try
      PL := TList.Create;
      try
        Ly := nil;
        for I := Low(A) to High(A) do
        begin
          S := A[I];
          if (S <> nil) and not (S is TSCDeLayer) and (S.FOwner is TSCDeLayer) and
            (S.GetSurface = Self) and (L.IndexOf(S) = -1) then
          begin
            L.Add(S);
            if S.Owner <> nil then PL.Add(S.Owner);

            if Ly = nil then
            begin
              Ly := S.GetLayer;
              if Ly.InDestroy then Ly := nil;
            end;
          end;
        end;

        if Ly = nil then Ly := GetActiveLayer;

        if (Ly <> nil) and (L.Count > 0) then
        begin
          PL.Add(Ly);

          BeginUpdate;
          try
            for I := 0 to PL.Count-1 do
              TSCDeShapeBase(PL[I]).BeginNotifyLock;

            try
              P := TSCDePackage.Create(Self, nil);

              StartAction(P, scacPacking, '');
              NotifyUndo(P, scacPacking, '');

              P.BeginUpdate;
              try
                for I := 0 to L.Count-1 do
                  P.Add(TSCDeShapeBase(L[I]));
              finally
                P.EndUpdate;
              end;

              Ly.Add(P);
              Result := P;

              EndAction(P, scacPacking, '');

              StartAction(P, scacPacked, '');
              NotifyUndo(P, scacPacked, '');
              EndAction(P, scacPacked, '');
            finally
              for I := 0 to PL.Count-1 do
                TSCDeShapeBase(PL[I]).EndNotifyLock;
            end;
          finally
            EndUpdate;
          end;
        end;
      finally
        PL.Free;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDeSurface.Unpack(P: TSCDePackage);
var
  C: TSCDeContainer;
begin
  if (P <> nil) and (P.GetSurface = Self) then
  begin
    C := P.Owner;

    if C is TSCDeLayer then
    begin
      StartAction(P, scacUnpacking, '');
      C.BeginUpdate;
      try
        C.BeginNotifyLock;
        try
          NotifyUndo(P, scacUnpacking, '');

          P.BeginNotifyLock;
          P.BeginUpdate;
          try
            while P.ItemCount > 0 do
              C.Add(P.Items[0]);
          finally
            P.Free;
          end;
        finally
          C.EndNotifyLock;
        end;
      finally
        C.EndUpdate;
      end;

      EndAction(P, scacUnpacking, '');
    end;
  end;
end;

function TSCDeSurface.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;

  with Result do
  begin
    if Right < Left then Right := Left;
    if Bottom < Top then Bottom := Top;
  end;
end;

procedure TSCDeSurface.RefreshBuffers(WithLayer, LockLayer: Boolean);
var
  C: TColor;
  R: TDoubleRect;
  CR, SR, IR: TRect;
begin
  if (FLayerProperties <> nil) and not (InUpdate or (ComponentState*[csDestroying, csLoading] <> [])) then
  begin
    if FUpdatingBuffer > 0 then
    begin
      FReupdateBuffers := True;
      Exit;
    end;

    LayerBounds(CR, R);

    Inc(FUpdatingBuffer);
    try
      BeforeUpdateBuffers(FClientBuffer.Canvas);

      SR := CR;
      OffsetRect(CR, -CR.Left, -CR.Top);

      if CR.Right < 0 then CR.Right := 0;
      if CR.Bottom < 0 then CR.Bottom := 0;

      C := Self.Color;
      if C = clNone then C := clGray;

      FClientBuffer.Canvas.Lock;
      try
        FClientBuffer.SetBounds(CR.Right, CR.Bottom);

        with FClientBuffer.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := C;

          FillRect(CR);
        end;

        if not TSCDeUtils.IsRectEmpty(R) then
        begin
          if R.Left < 0.0 then R.Left := 0.0;
          if R.Top < 0.0 then R.Top := 0.0;

          if R.Right > CR.Right then R.Right := CR.Right + 1;
          if R.Bottom > CR.Bottom then R.Bottom := CR.Bottom + 1;

          IR := TSCDeUtils.Rect(R);
          
          TSCDeUtils.OffsetRect(R, -R.Left, -R.Top);

          if R.Right < 0.0 then R.Right := 0.0;
          if R.Bottom < 0.0 then R.Bottom := 0.0;

          if not LockLayer and (WithLayer or (R.Right <> FLayerBuffer.Width) or
            (R.Bottom <> FLayerBuffer.Height)) then
            UpdateLayerBuffer;

          if IntersectRect(SR, IR, CR) then
          begin
            FClientBuffer.Canvas.Draw(SR.Left, SR.Top, FLayerBuffer);
            DrawLayerFrame(FClientBuffer.Canvas);
          end;  
        end;  
      finally
        FClientBuffer.Canvas.Unlock;
      end;
    finally
      Dec(FUpdatingBuffer);
    end;

    if FReupdateBuffers then
    begin
      FReupdateBuffers := False;
      RefreshBuffers(True, False);
      
      Exit;
    end;

    DrawSelection(FClientBuffer.Canvas);
    AfterUpdateBuffers(FClientBuffer.Canvas);

    Invalidate;
  end;
end;

function TSCDeSurface.GetLayerRect: TDoubleRect;
begin
  Result := TSCDeUtils.Rect(0, 0, FLayerProperties.Width,
    FLayerProperties.Height);
end;

function TSCDeSurface.HitTest(P: TDoublePoint): TSCDeHitTest;
var
  Z: Double;
  CR, IR: TRect;
  L: TSCDeLayer;
  I, J: Integer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
begin
  ResetHittest(Result);

  CR := ClientRect;

  with Result do
  begin
    Test.x := P.x - CR.Left;
    Test.y := P.y - CR.Top;
  end;

  Z := GetZoom;

  R := TSCDeUtils.Rect(CR);
  if Z < 1.0 then TSCDeUtils.ZoomRect(R, 1/Z);

  if not IsRectEmpty(CR) and TSCDeUtils.PtInRect(R, P) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if IntersectRect(IR, IR, CR) and TSCDeUtils.PtInRect(R, P) then
    begin
      Result.HitType := schtLayer;

      P.x := P.x - R.Left;
      P.y := P.y - R.Top;

      for I := 0 to LayerCount-1 do
      begin
        L := Layers[I];
      
        if L.Visible and not L.InDestroy then
          for J := 0 to L.ItemCount-1 do
          begin
            S := L.Items[J];

            if S.Visible and not S.InDestroy and S.PointOnShape(P, 0, False) then
            begin
              with Result do
              begin
                HitType := schtShape;
                Shape := S;
              end;

              Exit;
            end;
          end;
      end;
    end;
  end;
end;

function TSCDeSurface.GetNeedsUpdate: Boolean;
begin
  Result := FChangeCount > 0;
end;

function TSCDeSurface.GetClientCanvas: TCanvas;
begin
  Result := nil;
  if FClientBuffer <> nil then Result := FClientBuffer.Canvas;
end;

function TSCDeSurface.GetLayerCanvas: TCanvas;
begin
  Result := nil;
  if FLayerBuffer <> nil then Result := FLayerBuffer.Canvas;
end;

procedure TSCDeSurface.DoLayerRemoved(L: TSCDeLayer);
begin
  //
end;

procedure TSCDeSurface.LayerRemoved(L: TSCDeLayer);
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) and (L <> nil) then
  begin
    Changed(True);
    DoLayerRemoved(L);

    for I := 0 to FClients.Count-1 do
      TSCDeSurfaceClient(FClients[I]).DoRemoveLayer(L);

    if Assigned(FOnRemoveLayer) then FOnRemoveLayer(Self, L);
    if FActiveLayer = L then
      SetActiveLayer(ReArrangeActiveLayer(L));
  end;
end;

procedure TSCDeSurface.DoLayerInserted(L: TSCDeLayer);
begin
  //
end;

procedure TSCDeSurface.LayerInserted(L: TSCDeLayer);
var
  I: Integer;
begin
  Changed(True);
  DoLayerInserted(L);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoInsertLayer(L);

  if Assigned(FOnInsertLayer) then
    FOnInsertLayer(Self, L);
end;

procedure TSCDeSurface.LayerBounds(var CR: TRect; var R: TDoubleRect);
var
  Z, X, Y, W, H: Double;
begin
  CR := ClientRect;

  if CR.Right < CR.Left then CR.Right := CR.Left;
  if CR.Bottom < CR.Top then CR.Bottom := CR.Top;

  R := TSCDeUtils.EmptyRect;

  if not IsRectEmpty(CR) then
  begin
    W := FLayerProperties.Width;
    H := FLayerProperties.Height;

    if W < 0.0 then W := 0.0;
    if H < 0.0 then H := 0.0;

    Z := GetZoom;

    W := W * Z;
    H := H * Z;

    R := TSCDeUtils.Rect(0.0, 0.0, W, H);

    if (W > 0.0) or (H > 0.0) then
    begin
      X := Indent;
      Y := Indent;

      if W < CR.Right - CR.Left then
      begin
        X := CR.Left + ((CR.Right - CR.Left) - R.Right) / 2;
        if X < Indent then X := Indent;
      end;

      if H < CR.Bottom - CR.Top then
      begin
        Y := CR.Top + ((CR.Bottom - CR.Top) - R.Bottom) / 2;
        if Y < Indent then Y := Indent;
      end;

      TSCDeUtils.OffsetRect(R, X + FHorizontalPos, Y + FVerticalPos);
    end;
  end;
end;

procedure TSCDeSurface.IndentChanged;
begin
  Changed(True);
  inherited IndentChanged;
end;

procedure TSCDeSurface.Loaded;
begin
  inherited Loaded;

  BeginUpdate;
  try
    RefreshBuffers(True, False);
    UpdateScrollBars;
  finally
    EndUpdate;
  end;
end;

procedure TSCDeSurface.BeforeLayerPaint(C: TCanvas);
begin
  //
end;

procedure TSCDeSurface.BeforeUpdateBuffers(C: TCanvas);
begin
  //
end;

procedure TSCDeSurface.UpdatePaintBuffer;
begin
  //
end;

procedure TSCDeSurface.SetBufferedPaint(Value: Boolean);
begin
  if FBufferedPaint <> Value then
  begin
    FBufferedPaint := Value;
    UpdatePaintBuffer;
  end;
end;

procedure TSCDeSurface.AfterPaint(DC: HDC);
begin
  //
end;

procedure TSCDeSurface.BeforePaint(DC: HDC);
begin
  //
end;

procedure TSCDeSurface.WMPaint(var Message: TWMPaint);
begin
  BeforePaint(Message.DC);
  try
    if not (csDestroying in ComponentState) then
      UpdatePaintBuffer;

    inherited;
  finally
    AfterPaint(Message.DC);
  end;
end;

procedure TSCDeSurface.SetDirectBufferedPaint(Value: Boolean);
begin
  FBufferedPaint := Value;
end;

procedure TSCDeSurface.ResetHittest(var Hit: TSCDeHitTest);
begin
  with Hit do
  begin
    Shape := nil;
    Test.x := 0;
    Test.y := 0;
    HitType := schtNone;
    State := schsNone;
    PartIndex := -1;
    SubIndex := -1;
  end;
end;

procedure TSCDeSurface.UpdateHitTestPos(var Hit: TSCDeHitTest; P: TDoublePoint);
begin
  with Hit do
  begin
    Test.x := P.x;
    Test.y := P.y;
  end;
end;

procedure TSCDeSurface.SetActiveLayer(Value: TSCDeLayer);
begin
  if (Value = nil) or (Value.Surface = Self) then
  begin
    if Value <> nil then
      Value := ReArrangeActiveLayer(Value);

    if FActiveLayer <> Value then
    begin
      FActiveLayer := Value;
      ActiveLayerChanged;
    end;
  end;
end;

function TSCDeSurface.GetActiveLayer: TSCDeLayer;
begin
  Result := ReArrangeActiveLayer(FActiveLayer);
end;

function TSCDeSurface.Group(A: array of TSCDeShapeBase): TSCDeGroup;
var
  L, PL: TList;
  I: Integer;
  G: TSCDeGroup;
  Ly: TSCDeLayer;
  S: TSCDeShapeBase;
begin
  Result := nil;

  if (Length(A) > 0) and (LayerCount > 0) then
  begin
    L := TList.Create;
    try
      PL := TList.Create;
      try
        Ly := nil;
        for I := Low(A) to High(A) do
        begin
          S := A[I];
          if (S <> nil) and not (S is TSCDeLayer) and (S.FOwner is TSCDeLayer) and
            (S.GetSurface = Self) and (L.IndexOf(S) = -1) then
          begin
            L.Add(S);
            PL.Add(S);

            if Ly = nil then
            begin
              Ly := S.GetLayer;
              if Ly.InDestroy then Ly := nil;
            end;
          end;
        end;

        if Ly = nil then Ly := GetActiveLayer;

        if (Ly <> nil) and (L.Count > 0) then
        begin
          PL.Add(Ly);
          
          BeginUpdate;
          try
            for I := 0 to PL.Count-1 do
              TSCDeShapeBase(PL[I]).BeginNotifyLock;

            try
              G := TSCDeGroup.Create(Self, nil);

              StartAction(G, scacGrouping, '');
              NotifyUndo(G, scacGrouping, '');

              G.BeginUpdate;
              try
                for I := 0 to L.Count-1 do
                  G.Add(TSCDeShapeBase(L[I]));
              finally
                G.EndUpdate;
              end;

              Ly.Add(G);
              Result := G;

              EndAction(G, scacGrouping, '');

              StartAction(G, scacGrouped, '');
              NotifyUndo(G, scacGrouped, '');
              EndAction(G, scacGrouped, '');
            finally
              for I := 0 to PL.Count-1 do
                TSCDeShapeBase(PL[I]).EndNotifyLock;
            end;
          finally
            EndUpdate;
          end;
        end;
      finally
        PL.Free;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDeSurface.Ungroup(G: TSCDeGroup);
var
  C: TSCDeContainer;
begin
  if (G <> nil) and (G.GetSurface = Self) then
  begin
    C := G.Owner;
    if C is TSCDeLayer then
    begin
      StartAction(G, scacUngrouping, '');
      C.BeginUpdate;
      try
        C.BeginNotifyLock;
        try
          NotifyUndo(G, scacUngrouping, '');

          G.BeginNotifyLock;
          G.BeginUpdate;
          try
            while G.ItemCount > 0 do
              C.Add(G.Items[0]);
          finally
            G.Free;
          end;
        finally
          C.EndNotifyLock;
        end;
      finally
        C.EndUpdate;
      end;

      EndAction(G, scacUngrouping, '');
    end;
  end;
end;

procedure TSCDeSurface.SetZoom(Value: Word);
var
  I: Integer;
  R: TDoubleRect;
  OldValue: DWord;
  Z, HPos, VPos: Double;
begin
  if Value < 1 then Value := 1;
  if Value > 10000 then Value := 10000;

  if FZoom <> Value then
  begin
    Z := FZoom/100;
    R := GetNominalViewRect;

    HPos := (FHorizontalPos + R.Left)/Z;
    VPos := (FVerticalPos + R.Top)/Z;

    OldValue := FZoom;
    FZoom := Value;

    BeginUpdate;
    try
      R := GetNominalViewRect;

      if HPos > 0.0 then
        HPos := 0.0
      else
        HPos := -R.Left + HPos*(FZoom/OldValue);

      if VPos > 0.0 then
        VPos := 0.0
      else
        VPos := -R.Top + VPos*(FZoom/OldValue);

      SetLayerOffset(Trunc(HPos), Trunc(VPos));

      UpdateScrollBars;
      RefreshBuffers(True, False);
    finally
      EndUpdate;

      for I := 0 to FClients.Count-1 do
        TSCDeSurfaceClient(FClients[I]).DoZoom;

      if Assigned(FOnZoom) then FOnZoom(Self);
    end;
  end;
end;

function TSCDeSurface.ScreenToWorld(P: TDoublePoint): TDoublePoint;
var
  Z: Double;
  CR: TRect;
  R: TDoubleRect;
begin
  LayerBounds(CR, R);

  Z := GetZoom;

  Result := P;

  Result.x := ((Result.x - R.Left)/Z) + R.Left;
  Result.y := ((Result.y - R.Top)/Z) + R.Top;
end;

function TSCDeSurface.GetZoom: Double;
begin
  Result := FZoom / 100;
  if Result < 0.01 then Result := 0.01;
end;

procedure TSCDeSurface.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  inherited;
  Message.Result := 1;
end;

procedure TSCDeSurface.SetHorizontalPos(Value: Integer);
begin
  SetLayerOffset(Value, FVerticalPos);
end;

procedure TSCDeSurface.SetVerticalPos(Value: Integer);
begin
  SetLayerOffset(FHorizontalPos, Value);
end;

function TSCDeSurface.GetPaintRect: TRect;
begin
  Result := ClientRect;
end;

procedure TSCDeSurface.SetLayerOffset(AHorz, AVert: Integer);
var
  OldHorz, OldVert: Integer;
begin
  Inc(FScrollPosChanging);
  try
    if AHorz > 0 then AHorz := 0;
    if AVert > 0 then AVert := 0;

    OldHorz := FHorizontalPos;
    FHorizontalPos := AHorz;

    OldVert := FVerticalPos;
    FVerticalPos := AVert;

    ValidateLayerOffset(FHorizontalPos, FVerticalPos);

    if (OldHorz <> FHorizontalPos) or (OldVert <> FVerticalPos) then
      RefreshBuffers(True, False);

    if (FScrollbarChanging = 0) and (FScrollbarUpdate = 0) then
    begin
      TSCDeScrollbar(ScrollbarHorz).Position := -FHorizontalPos;
      TSCDeScrollbar(ScrollbarVert).Position := -FVerticalPos;
    end;
  finally
    Dec(FScrollPosChanging);
  end;
end;

procedure TSCDeSurface.ValidateLayerOffset(var AHorz, AVert: Integer);
var
  CR, IR: TRect;
  R: TDoubleRect;
begin
  if AHorz > 0 then AHorz := 0;
  if AVert > 0 then AVert := 0;

  if (AHorz <> 0) or (AVert <> 0) then
  begin
    LayerBounds(CR, R);

    CR := GetPaintRect;
    IR := TSCDeUtils.Rect(R);

    InflateRect(CR, -Indent, -Indent);

    OffsetRect(CR, -CR.Left, -CR.Top);
    OffsetRect(IR, -IR.Left, -IR.Top);

    if CR.Right >= IR.Right then
      AHorz := 0
    else if Abs(AHorz) > IR.Right - CR.Right then
      AHorz := -(IR.Right - CR.Right);

    if CR.Bottom >= IR.Bottom then
      AVert := 0
    else if Abs(AVert) > IR.Bottom - CR.Bottom then
      AVert := -(IR.Bottom - CR.Bottom);
  end;
end;

procedure TSCDeSurface.SetShowScrollbars(Value: Boolean);
begin
  if FShowScrollbars <> Value then
  begin
    FShowScrollbars := Value;
    UpdateScrollBars;
  end;
end;

procedure TSCDeSurface.CreateWnd;
begin
  Inc(FCreatingWnd);
  try
    inherited CreateWnd;
  finally
    Dec(FCreatingWnd);
  end;

  UpdateScrollbars;
end;

procedure TSCDeSurface.DoScrollerPositionChanged(Kind: TSCDeScrollbarKind);
var
  Sb: TSCDeScrollbar;
begin
  if (FScrollPosChanging = 0) and (FScrollbarUpdate = 0) then
  begin
    Inc(FScrollbarChanging);
    try
      if Kind = scdskHorizontal then
      begin
        Sb := TSCDeScrollbar(ScrollbarHorz);
        SetLayerOffset(-Sb.Position, FVerticalPos);
      end else
      begin
        Sb := TSCDeScrollbar(ScrollbarVert);
        SetLayerOffset(FHorizontalPos, -Sb.Position);
      end;
    finally
      Dec(FScrollbarChanging);
    end;
  end;  
end;

procedure TSCDeSurface.NotifyUndo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);
begin
  //
end;

procedure TSCDeSurface.LoadFromFile(const FileName: String);

  procedure LoadFromDom(Root: TSCDeContainer; RootElm: TSCDomElement);
  var
    I: Integer;
    S: TSCDeShapeBase;
    SC: TSCDeShapeClass;
    Attr: TSCDomAttribute;
    Elms, Elm: TSCDomElement;
  begin
    if scssIsContainer in Root.ShapeStyle then
    begin
      Elms := RootElm.ElementByName('elements');

      if Elms <> nil then
        for I := 0 to Elms.ChildNodeCount-1 do
          if (Elms.ChildNodes[I].Name = 'element') and (Elms.ChildNodes[I] is TSCDomElement) then
          begin
            Elm := TSCDomElement(Elms.ChildNodes[I]);

            Attr := Elm.AttributeByName('class');
            if Attr <> nil then
            begin
              SC := TSCDeRegister.FindShape(Attr.Value);

              if SC <> nil then
              begin
                S := SC.Create(nil, nil);
                S.LoadFromXML(Elm);

                Root.Add(S);

                if S is TSCDeContainer then
                  LoadFromDom(TSCDeContainer(S), Elm);
              end;
            end;
          end;
    end;
  end;

var
  I: Integer;
  P: TSCDomParser;
  ClassRef: TClass;
  S: TSCDeShapeBase;
  SC: TSCDeShapeClass;
  LC: TSCDeLayerClass;
  Doc: TSCDomDocument;
  Attr: TSCDomAttribute;
  Elm, Root: TSCDomElement;
begin
  if FInLoad then
    Exit;

  FInLoad := True;
  try
    BeginUpdate;
    try
      Clear;

      if FileExists(FileName) then
      begin
        P := TSCDomParser.Create(nil);
        try
          Doc := P.ParseFile(FileName);

          if Doc <> nil then
          begin
            try
              Root := Doc.ElementByName('surface');

              if Root <> nil then
              begin
                BeforeLoadDOM(Root);
                try
                  for I := 0 to Root.ChildNodeCount-1 do
                    if (Root.ChildNodes[I].Name = 'layer') and (Root.ChildNodes[I] is TSCDomElement) then
                    begin
                      Elm := TSCDomElement(Root.ChildNodes[I]);

                      LC := TSCDeLayer;

                      Attr := Elm.AttributeByName('class');
                      if Attr <> nil then
                      begin
                        SC := TSCDeRegister.FindShape(Attr.Value);

                        if SC <> nil then
                        begin
                          ClassRef := SC;
                          while ClassRef <> nil do
                          begin
                            if ClassRef = TSCDeLayer then
                            begin
                              LC := TSCDeLayerClass(SC);
                              Break;
                            end;

                            ClassRef := ClassRef.ClassParent;
                          end;
                        end;
                      end;

                      if LC = nil then LC := TSCDeLayer;

                      S := LC.Create(Self);
                      S.LoadFromXML(Elm);

                      LoadFromDom(TSCDeLayer(S), Elm);
                    end;
                finally
                  AfterLoadDOM(Root);
                end;
              end;
            finally
              Doc.Free;
            end;
          end;
        finally
          P.Free;
        end;
      end;
    finally
      EndUpdate;
    end;
  finally
    FInLoad := False;
  end;
end;

procedure TSCDeSurface.SaveToFile(const FileName: String);

  procedure SaveElementsOf(S: TSCDeShapeBase; Root: TSCDomElement);
  var
    I: Integer;
    C: TSCDeContainer;
    SS: TSCDeShapeBase;
    Elms, Elm: TSCDomElement;
  begin
    if (S is TSCDeContainer) and (scssIsContainer in S.ShapeStyle) then
    begin
      C := TSCDeContainer(S);

      if C.ItemCount > 0 then
      begin
        Elms := TSCDomElement.Create(Root);
        Elms.Name := 'elements';

        Root.AddElement(Elms);

        for I := 0 to C.ItemCount-1 do
        begin
          SS := C.Items[I];

          Elm := TSCDomElement.Create(Elms);
          Elm.Name := 'element';
          Elm.AddAttribute('class', SS.ClassName);

          Elms.AddElement(Elm);

          SS.SaveToXML(Elm);
          SaveElementsOf(SS, Elm);
        end;
      end;
    end;
  end;

var
  I: Integer;
  S: TSCDeShapeBase;
  Doc: TSCDomDocument;
  Elm, Root: TSCDomElement;
begin
  if FileName <> '' then
  begin
    Doc := TSCDomDocument.Create(nil);
    try
      Root := TSCDomElement.Create(Doc);
      Root.Name := 'surface';
      Root.AddAttribute('version', '1.0');

      Doc.AddElement(Root);

      BeforeSaveDOM(Root);
      try
        for I := 0 to FLayers.Count-1 do
        begin
          S := TSCDeShapeBase(FLayers[I]);

          Elm := TSCDomElement.Create(Root);
          Elm.Name := 'layer';
          if S.ClassType <> TSCDeLayer then Elm.AddAttribute('class', S.ClassName);

          Root.AddElement(Elm);

          S.SaveToXML(Elm);
          SaveElementsOf(S, Elm);
        end;
      finally
        AfterSaveDOM(Root);
      end;

      Doc.SaveToFile(FileName);
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSCDeSurface.GridPropsChanged;
var
  I: Integer;
begin
  Changed(True);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoGridPropsChange;

  if Assigned(FOnGridPropsChange) then FOnGridPropsChange(Self);
end;

procedure TSCDeSurface.LayerPropsChanged(Scrollbars: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    Changed(True);
    if Scrollbars then UpdateScrollBars;
  finally
    EndUpdate;
  end;

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoLayerPropsChange;

  if Assigned(FOnLayerPropsChange) then
    FOnLayerPropsChange(Self);
end;

procedure TSCDeSurface.FramePropsChanged;
var
  I: Integer;
begin
  Changed(False);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoFramePropsChange;

  if Assigned(FOnFramePropsChange) then FOnFramePropsChange(Self);
end;

procedure TSCDeSurface.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TSCDeSurface.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TSCDeSurface.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

procedure TSCDeSurface.CopyToClipboard;
begin
  //
end;

procedure TSCDeSurface.CutToClipboard;
begin
  //
end;

procedure TSCDeSurface.PasteFromClipboard;
begin
  //
end;

function TSCDeSurface.HasClipboardData: Boolean;
begin
  Result := False;
end;

procedure TSCDeSurface.BeforeDestruction;
begin
  Destroying;
  inherited BeforeDestruction;
end;

procedure TSCDeSurface.WMHScroll(var Message: TWMHScroll);
var
  SI: TScrollInfo;
  Sb: TSCDeScrollbar;
begin
  if not (csDesigning in ComponentState) and (ScrollbarHorz <> nil) then
  begin
    Sb := TSCDeScrollbar(ScrollbarHorz);

    SI.cbSize := SizeOf(SI);
    SI.fMask  := SIF_ALL;

    GetScrollInfo(Self.Handle, SB_HORZ, SI);

    case Message.ScrollCode of
      SB_LINEUP:
        SetHorizontalPos(FHorizontalPos + Sb.SmallChange);
      SB_LINEDOWN:
        SetHorizontalPos(FHorizontalPos - Sb.SmallChange);
      SB_PAGEUP:
        SetHorizontalPos(FHorizontalPos + Sb.LargeChange);
      SB_PAGEDOWN:
        SetHorizontalPos(FHorizontalPos - Sb.LargeChange);
      SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if SI.nTrackPos <= SI.nMin then
          SetHorizontalPos(SI.nMin)
        else
        if SI.nTrackPos >= SI.nMax then
          SetHorizontalPos(SI.nMax)
        else
          SetHorizontalPos(SI.nTrackPos);
      end;
      SB_TOP:
        SetHorizontalPos(SI.nMin);
      SB_BOTTOM:
        SetHorizontalPos(SI.nMax);
    end;
  end;
end;

procedure TSCDeSurface.WMMouseWheel(var Message: TWMMouseWheel);
var
  Sb: TSCDeScrollbar;
begin
  if not (csDesigning in ComponentState) and (ScrollbarHorz <> nil) then
  begin
    Sb := TSCDeScrollbar(ScrollbarHorz);

    if Message.WheelDelta < 0 then
      SetVerticalPos(FVerticalPos - 3*Sb.SmallChange)
    else
    if Message.WheelDelta > 0 then
      SetVerticalPos(FVerticalPos + 3*Sb.SmallChange);
  end;
end;

procedure TSCDeSurface.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
  Sb: TSCDeScrollbar;
begin
  if not (csDesigning in ComponentState) and (ScrollbarVert <> nil) then
  begin
    Sb := TSCDeScrollbar(ScrollbarVert);

    SI.cbSize := SizeOf(SI);
    SI.fMask  := SIF_ALL;

    GetScrollInfo(Self.Handle, SB_HORZ, SI);

    case Message.ScrollCode of
      SB_LINEUP:
        SetVerticalPos(FVerticalPos + Sb.SmallChange);
      SB_LINEDOWN:
        SetVerticalPos(FVerticalPos - Sb.SmallChange);
      SB_PAGEUP:
        SetVerticalPos(FVerticalPos + Sb.LargeChange);
      SB_PAGEDOWN:
        SetVerticalPos(FVerticalPos - Sb.LargeChange);
      SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if SI.nTrackPos <= SI.nMin then
          SetVerticalPos(SI.nMin)
        else
        if SI.nTrackPos >= SI.nMax then
          SetVerticalPos(SI.nMax)
        else
          SetVerticalPos(SI.nTrackPos);
      end;
      SB_TOP:
        SetVerticalPos(SI.nMin);
      SB_BOTTOM:
        SetVerticalPos(SI.nMax);
    end;
  end;
end;

procedure TSCDeSurface.DoPictureChanged;
begin
  Changed(True);
end;

procedure TSCDeSurface.DoPictureListChanged;
begin
  Changed(True);
end;

function TSCDeSurface.GetPictureRect(P: TPicture): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if ShowPicture and (FLayerBuffer <> nil) then
  begin
    Result := Rect(0, 0, FLayerBuffer.Width, FLayerBuffer.Height);
    InflateRect(Result, -PictureIndent, -PictureTopIndent);
  end;
end;

procedure TSCDeSurface.ZoomRect(R: TDoubleRect);
var
  CR, IR: TRect;
  DR: TDoubleRect;
  P: TDoublePoint;
  OldZ, Z, Z1, Z2: Double;
begin
  IR := TSCDeUtils.Rect(R);

  if not IsRectEmpty(IR) then
  begin
    LayerBounds(CR, DR);

    if not (IsRectEmpty(CR) or EqualRect(IR, CR)) then
    begin
      Z1 := (CR.Right - CR.Left)/(IR.Right - IR.Left);
      Z2 := (CR.Bottom - CR.Top)/(IR.Bottom - IR.Top);

      Z := Z1;
      if Z2 > Z then Z := Z2;

      OldZ := GetZoom;

      if (Z > 0.0) and (Z <> OldZ) then
      begin
        BeginUpdate;
        try
          SetZoom(TSCDeUtils.Round(Z*100));

          Z := GetZoom;
          DR := GetNominalViewRect;

          P.x := -(DR.Left + (Z*R.Left));
          P.y := -(DR.Top + (Z*R.Top));

          SetLayerOffset(TSCDeUtils.Round(P.x), TSCDeUtils.Round(P.y));
        finally
          EndUpdate;
        end;
      end;
    end;
  end;
end;

function TSCDeSurface.GetViewRect: TDoubleRect;
var
  CR: TRect;
  Z: Double;
begin
  LayerBounds(CR, Result);

  Z := GetZoom;
  TSCDeUtils.ZoomRect(Result, 1/Z);

  Result.Right := Result.Left + (CR.Right - CR.Left);
  Result.Bottom := Result.Top + (CR.Bottom - CR.Top);

  TSCDeUtils.OffsetRect(Result, -2*Result.Left, -2*Result.Top);
end;

function TSCDeSurface.GetNominalViewRect: TDoubleRect;
var
  CR: TRect;
begin
  LayerBounds(CR, Result);
  TSCDeUtils.OffsetRect(Result, -FHorizontalPos, -FVerticalPos);
end;

procedure TSCDeSurface.AfterLoadDOM(Root: TSCDomElement);
var
  I: Integer;
begin
  DoAfterLoadDOM(Root);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoAfterLoadDOM(Root);

  if Assigned(OnAfterLoadDOM) then
    OnAfterLoadDOM(Self, Root);
end;

procedure TSCDeSurface.AfterSaveDOM(Root: TSCDomElement);
var
  I: Integer;
begin
  DoAfterSaveDOM(Root);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoAfterSaveDOM(Root);

  if Assigned(OnAfterSaveDOM) then
    OnAfterSaveDOM(Self, Root);
end;

procedure TSCDeSurface.BeforeLoadDOM(Root: TSCDomElement);
var
  I: Integer;
begin
  DoBeforeLoadDOM(Root);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoBeforeLoadDOM(Root);

  if Assigned(OnBeforeLoadDOM) then
    OnBeforeLoadDOM(Self, Root);
end;

procedure TSCDeSurface.BeforeSaveDOM(Root: TSCDomElement);
var
  I: Integer;
begin
  DoBeforeSaveDOM(Root);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoBeforeSaveDOM(Root);

  if Assigned(OnBeforeSaveDOM) then
    OnBeforeSaveDOM(Self, Root);
end;

procedure TSCDeSurface.DoAfterLoadDOM(Root: TSCDomElement);
begin
  //
end;

procedure TSCDeSurface.DoAfterSaveDOM(Root: TSCDomElement);
begin
  //
end;

procedure TSCDeSurface.DoBeforeLoadDOM(Root: TSCDomElement);
begin
  //
end;

procedure TSCDeSurface.DoBeforeSaveDOM(Root: TSCDomElement);
begin
  //
end;

function TSCDeSurface.GetSelectionBounds: TDoubleRect;
begin
  Result := TSCDeUtils.Rect(0, 0, 0, 0);
end;

function TSCDeSurface.HandleActiveLayer: TSCDeLayer;
begin
  Result := GetActiveLayer;
  if (Result = nil) and (FLayers.Count = 0) then
  begin
    Result := TSCDeLayer.Create(Self);
    Self.SetActiveLayer(Result);
  end;
end;

function TSCDeSurface.GetClient(Index: Integer): TSCDeSurfaceClient;
begin
  Result := TSCDeSurfaceClient(FClients[Index]);
end;

function TSCDeSurface.GetClientCount: Integer;
begin
  Result := FClients.Count;
end;

procedure TSCDeSurface.UnRegisterAllClients;
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  for I := FClients.Count-1 downto 0 do
  begin
    C := TSCDeSurfaceClient(FClients[I]);
    FClients.Delete(I);
    C.FSender := nil;
  end;
end;

procedure TSCDeSurface.UnRegisterClient(Client: TSCDeSurfaceClient);
begin
  if (Client <> nil) and (Client.FSender = Self) then
  begin
    FClients.Remove(Client);
    Client.FSender := nil;
  end;
end;

procedure TSCDeSurface.RegisterClient(Client: TSCDeSurfaceClient);
begin
  if (Client <> nil) and (Client.FSender <> Self) then
  begin
    if Client.FSender <> nil then
      Client.FSender.UnRegisterClient(Client);

    FClients.Add(Client);
    Client.FSender := Self;
  end;
end;

procedure TSCDeSurface.DoActiveLayerChanged;
begin
  //
end;

procedure TSCDeSurface.ActiveLayerChanged;
var
  I: Integer;
begin
  DoActiveLayerChanged;

  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      TSCDeSurfaceClient(FClients[I]).DoActiveLayerChange;

  if Assigned(FOnActiveLayerChange) then
    FOnActiveLayerChange(Self);
end;

procedure TSCDeSurface.RefineSelections;
begin
  //
end;

procedure TSCDeSurface.ShapeCleared(S: TSCDeShapeBase);
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
    DoShapeCleared(S);

  for I := 0 to FClients.Count-1 do
    TSCDeSurfaceClient(FClients[I]).DoShapeCleared(S);

  if Assigned(FOnClearShape) then FOnClearShape(Self, S);
end;

procedure TSCDeSurface.DoShapeCleared(S: TSCDeShapeBase);
begin
  //
end;

procedure TSCDeSurface.EndAction(S: TSCDeShapeBase; Action: TSCDeAction;
  const PropName: String);
begin
  //
end;

procedure TSCDeSurface.StartAction(S: TSCDeShapeBase; Action: TSCDeAction;
  const PropName: String);
begin
  //
end;

function TSCDeSurface.ReArrangeActiveLayer(Ly: TSCDeLayer): TSCDeLayer;
var
  I, AI: Integer;
  L: TSCDeLayer;
begin
  Result := Ly;
  if Self.IndexOf(Ly) = -1 then
    Result := nil;

  if (Result = nil) or not Result.Visible or
    Result.Locked or Result.InDestroy then
  begin
    AI := -1;
    if Result <> nil then
      AI := Self.IndexOf(Result);

    Result := nil;
    if FLayers.Count > 0 then
    begin
      for I := AI + 1 to FLayers.Count-1 do
      begin
        L := TSCDeLayer(FLayers[I]);
        if L.Visible and not (L.InDestroy or L.Locked) then
        begin
          Result := L;
          Break;
        end;
      end;

      if Result = nil then
        for I := AI - 1 downto 0 do
        begin
          L := TSCDeLayer(FLayers[I]);
          if L.Visible and not (L.InDestroy or L.Locked) then
          begin
            Result := L;
            Break;
          end;
        end;

      if Result = nil then
        for I := FLayers.Count-1 downto 0 do
        begin
          L := TSCDeLayer(FLayers[I]);
          if not L.InDestroy and L.Locked then
          begin
            Result := L;
            Break;
          end;
        end;
    end;
  end;
end;

{ TSCDeCheckersProperties }

procedure TSCDeCheckersProperties.Assign(Source: TPersistent);
begin
  if Source is TSCDeCheckersProperties then
  begin
    with TSCDeCheckersProperties(Source) do
    begin
      Self.FColorEven := ColorEven;
      Self.FColorOdd := ColorOdd;
      Self.FWidth := Width;
      Self.FHeight := Height;
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeCheckersProperties.Changed;
begin
  if FOwner <> nil then FOwner.CheckersChanged;
end;

constructor TSCDeCheckersProperties.Create(AOwner: TSCDeLayerProperties);
begin
  inherited Create;
  FOwner := AOwner;
  FColorEven := clWhite;
  FColorOdd := clSilver;
  FWidth := 20;
  FHeight := 20;
end;

function TSCDeCheckersProperties.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeCheckersProperties.SetColorEven(Value: TColor);
begin
  if FColorEven <> Value then
  begin
    FColorEven := Value;
    Changed;
  end;
end;

procedure TSCDeCheckersProperties.SetColorOdd(Value: TColor);
begin
  if FColorOdd <> Value then
  begin
    FColorOdd := Value;
    Changed;
  end;
end;

procedure TSCDeCheckersProperties.SetHeight(Value: Integer);
begin
  if Value < 2 then Value := 2;

  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TSCDeCheckersProperties.SetWidth(Value: Integer);
begin
  if Value < 2 then Value := 2;

  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;


procedure RadialCentral(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 362;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 180 downto 0 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    for X := 180 downto 0 do
    begin
      rX := 361 - X;
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcXs[Y]))];
      Row1[X] := pRGB^;
      Row1[rX] := pRGB^;
      Row2[X] := pRGB^;
      Row2[rX] := pRGB^;
    end;
  end;
end;

procedure RadialTop(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX, rY: Integer;
  pRGB: PRGBQuad;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  rY := 0;
  for Y := 180 downto 0 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    rX := 181;
    for X := 180 downto 0 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row[X] := pRGB^;
      Row[rX] := pRGB^;
      Inc(rX);
    end;
    Inc(rY);
  end;
end;

procedure RadialBottom(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 362;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 180 downto 0 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    rX := 181;
    for X := 180 downto 0 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row[X] := pRGB^;
      Row[rX]:= pRGB^;
      Inc(rX);
    end;
  end;
end;

procedure RadialLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rY: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 362;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
      Row1[X] := pRGB^;
      Row2[X] := pRGB^;
    end;
    Dec(rY);
  end;
end;

procedure RadialRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 362;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 0 to 180 do
  begin
    Row1 := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row2 := PRGBQuadArray(Pattern.ScanLine[361-Y]);
    for X := 0 to 180 do
    begin
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcXs[Y]))];
      Row1[X] := pRGB^;
      Row2[X] := pRGB^;
    end;
  end;
end;

procedure RadialTopLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
end;

procedure RadialTopRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX, rY: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  rX :=0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
    Dec(rY);
  end;
end;

procedure RadialBottomLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rY: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  for X := 180 downto 0 do
    PreCalcXs[X] := X * X;

  rY := 180;
  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[rY];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
    Dec(rY);
  end;
end;

procedure RadialBottomRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y, rX: Integer;
  Row: PRGBQuadArray;
  PreCalcY: Integer;
  PreCalcXs: array[0..180] of Integer;
begin
  Pattern.Width := 181;
  Pattern.Height := 181;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 0 to 180 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    PreCalcY := PreCalcXs[Y];
    for X := 0 to 180 do
      Row[X] := Colors[Round(Sqrt(PreCalcXs[X] + PreCalcY))];
  end;
end;

procedure LinearHorizontal(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 1;
  Row := PRGBQuadArray(Pattern.ScanLine[0]);
  for X := 0 to 255 do
    Row[X] := Colors[X];
end;

procedure LinearVertical(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 1;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row[0] := Colors[Y];
  end;
end;

procedure ReflectedHorizontal(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 1;
  Pattern.Height := 512;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    Row[0] := Colors[255 - Y];
    Row := PRGBQuadArray(Pattern.ScanLine[511 - Y]);
    Row[0] := Colors[255 - Y];
  end;
end;

procedure ReflectedVertical(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 512;
  Pattern.Height := 1;
  Row := PRGBQuadArray(Pattern.ScanLine[0]);
  for X := 0 to 255 do
  begin
    Row[X] := Colors[255 - X];
    Row[511 - X] := Colors[255 - X];
  end;
end;

procedure DiagonalLinearForward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 128;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[X + Y];
  end;
end;

procedure DiagonalLinearBackward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 128;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[127 + (Y - X)];
  end;
end;

procedure DiagonalReflectedForward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X + Y < 255 then
        Row[X] := Colors[255 - (X + Y)]
      else
        Row[X] := Colors[(Y + X) - 255];
  end;
end;

procedure DiagonalReflectedBackward(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 255 do
      if X > Y then
        Row[X] := Colors[X - Y]
      else
        Row[X] := Colors[Y - X];
  end;
end;

procedure ArrowLeft(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 129;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[255 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[Y - X];
  end;
end;

procedure ArrowRight(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 129;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[(X - Y) + 127];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 128 do
      Row[X] := Colors[(X + Y) - 128];
  end;
end;

procedure ArrowUp(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row[X] := Colors[X - Y];
  end;
end;

procedure ArrowDown(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 129;
  for Y := 0 to 128 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[127 + (Y - X)];
    for X := 128 to 255 do
      Row[X] := Colors[(X + Y) - 128];
  end;
end;

procedure Diamond(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[255 - (X + Y)];
    for X := 128 to 255 do
      Row[X] := Colors[X - Y];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[Y - X];
    for X := 128 to 255 do
      Row[X] := Colors[(X + Y) - 255];
  end;
end;

procedure Butterfly(const Colors: TSCDeGradientColors; Pattern: TBitmap);
var
  X, Y: Integer;
  Row: PRGBQuadArray;
begin
  Pattern.Width := 256;
  Pattern.Height := 256;
  for Y := 0 to 127 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[(X - Y) + 128];
    for X := 128 to 255 do
      Row[X] := Colors[383 - (X + Y)];
  end;
  for Y := 128 to 255 do
  begin
    Row := PRGBQuadArray(Pattern.ScanLine[Y]);
    for X := 0 to 127 do
      Row[X] := Colors[(X + Y) - 128];
    for X := 128 to 255 do
      Row[X] := Colors[128 + (Y - X)];
  end;
end;

{ TSCDeShapeGradient }

type
  TSCDePatternBuilder = procedure(const Colors: TSCDeGradientColors; Pattern: TBitmap);

const
  PatternBuilder: array[TSCDeShapeGradientStyle] of TSCDePatternBuilder = (
    nil, RadialCentral, RadialTop, RadialBottom, RadialLeft, RadialRight,
    RadialTopLeft, RadialTopRight, RadialBottomLeft, RadialBottomRight,
    LinearHorizontal, LinearVertical, ReflectedHorizontal, ReflectedVertical,
    DiagonalLinearForward, DiagonalLinearBackward, DiagonalReflectedForward,
    DiagonalReflectedBackward, ArrowLeft, ArrowRight, ArrowUp, ArrowDown,
    Diamond, Butterfly);

constructor TSCDeShapeGradient.Create;
begin
  inherited Create;
  FColorBegin := clLime;
  FColorEnd := clWhite;
  FStyle := scdgsNone;
  FShift := 0;
  FRotation := 0;
  FReverse := False;
  FNeedsUpdate := True;

  FPattern := TBitmap.Create;
  FPattern.PixelFormat := pf32bit;

  UpdatePattern;
end;

destructor TSCDeShapeGradient.Destroy;
begin
  FreeAndNil(FPattern);
  inherited Destroy;
end;

procedure TSCDeShapeGradient.Paint(ACanvas: TCanvas; ARect: TRect);
begin
 if not (FDirty or (ACanvas = nil) or (FStyle = scdgsNone) or
   (FColorEnd = clNone) or (FColorBegin = clNone) or IsRectEmpty(ARect)) then
 begin
   UpdatePattern;
   ACanvas.StretchDraw(ARect, Pattern);
 end;
end;

procedure TSCDeShapeGradient.Assign(Source: TPersistent);
begin
  if Source is TSCDeShapeGradient then
  begin
    Changing;
    with TSCDeShapeGradient(Source) do
    begin
      Self.FColorBegin := ColorBegin;
      Self.FColorEnd := ColorEnd;
      Self.FReverse := Reverse;
      Self.FRotation := Rotation;
      Self.FShift := Shift;
      Self.FStyle := Style;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeShapeGradient.AssignRec(const Source: TSCDeShapeGradienthRec);
begin
  Changing;
  with TSCDeShapeGradienthRec(Source) do
  begin
    Self.FColorBegin := ColorBegin;
    Self.FColorEnd := ColorEnd;
    Self.FReverse := Reverse;
    Self.FRotation := Rotation;
    Self.FShift := Shift;
    Self.FStyle := Style;
  end;
  Changed;
end;

procedure TSCDeShapeGradient.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeShapeGradient.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      UpdatePattern;
  end;
end;

function TSCDeShapeGradient.GetColorBegin: TColor;
begin
  Result := FColorBegin;
  if FOwner <> nil then
    Result := FOwner.FColor;
end;

function TSCDeShapeGradient.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeShapeGradient.Changed;
begin
  FNeedsUpdate := True;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSCDeShapeGradient.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

function TSCDeShapeGradient.CopyPatternTo(ABitmap: TBitmap): Boolean;
begin
  Result := False;
  if not FDirty and (FUpdateCount = 0) and Assigned(ABitmap) then
  begin
    Result := True;

    UpdatePattern;
    ABitmap.Assign(Self.Pattern);
  end;
end;

procedure TSCDeShapeGradient.InvalidatePattern;
begin
  UpdatePattern;
end;

procedure TSCDeShapeGradient.LoadFromXML(Node: TSCDomElement);

  function GetProperty(Parent: TSCDomElement; AName: String): String;
  var
    I: Integer;
    Prop: TSCDomElement;
  begin
    Result := '';

    if Parent <> nil then
    begin
      Prop := Parent.ElementByName(AName);

      if Prop <> nil then
        for I := 0 to Prop.ChildNodeCount-1 do
          if Prop.ChildNodes[I] is TSCDomText then
          begin
            Result := Trim(TSCDomText(Prop.ChildNodes[I]).Text);

            if (Result <> '') and (Result[1] = #13) then Delete(Result, 1, 1);
            if (Result <> '') and (Result[1] = #10) then Delete(Result, 1, 1);

            Result := Trim(Result);

            Break;
          end;
    end;
  end;

var
  S: String;
  I: Integer;
begin
  Changing;
  try
    S := GetProperty(Node, 'colorbegin');
    if S <> '' then
    begin
      FColorBegin := StringToColor(S);
      if FOwner <> nil then
        FOwner.FColor := FColorBegin;
    end;

    S := GetProperty(Node, 'colorend');
    if S <> '' then FColorEnd := StringToColor(S);

    S := GetProperty(Node, 'reverse');
    FReverse := Boolean(StrToIntDef(S, Integer(False)));

    S := GetProperty(Node, 'rotation');
    I := StrToIntDef(S, 0);

    if I > 100 then I := 100;
    if I < -100 then I := -100;

    FRotation := TSCDeGradientRotation(I);

    S := GetProperty(Node, 'shift');
    I := StrToIntDef(S, 0);

    if I > 100 then I := 100;
    if I < -100 then I := -100;

    FShift := TSCDeGradientShift(I);

    S := GetProperty(Node, 'style');
    FStyle := TSCDeShapeGradientStyle(StrToIntDef(S, Integer(scdgsNone)));

    FNeedsUpdate := True;
    UpdatePattern;
  finally
    Changed;
  end;
end;

procedure TSCDeShapeGradient.SaveToXML(Node: TSCDomElement);

  procedure AddProperty(const AName, AValue: String);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := AName;
    Node.AddElement(Prop);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

begin
  AddProperty('colorbegin', ColorToString(FColorBegin));
  AddProperty('colorend', ColorToString(FColorEnd));
  AddProperty('reverse', IntToStr(Integer(FReverse)));
  AddProperty('rotation', IntToStr(FRotation));
  AddProperty('shift', IntToStr(FShift));
  AddProperty('style', IntToStr(Integer(FStyle)));
end;

procedure TSCDeShapeGradient.SetColorBegin(Value: TColor);
begin
  if GetColorBegin <> Value then
  begin
    Changing;

    FColorBegin := Value;
    if FOwner <> nil then
      FOwner.SetColor(Value);

    Changed;
  end;
end;

procedure TSCDeShapeGradient.SetColorEnd(Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    Changing;

    FColorEnd := Value;
    Changed;
  end;
end;

procedure TSCDeShapeGradient.SetStyle(Value: TSCDeShapeGradientStyle);
begin
  if FStyle <> Value then
  begin
    Changing;

    FStyle := Value;
    Changed;
  end;
end;

procedure TSCDeShapeGradient.SetShift(Value: TSCDeGradientShift);
begin
  if Value < Low(TSCDeGradientShift) then
    Value := Low(TSCDeGradientShift)
  else if Value > High(TSCDeGradientShift) then
    Value := High(TSCDeGradientShift);

  if FShift <> Value then
  begin
    Changing;

    FShift := Value;
    Changed;
  end;
end;

procedure TSCDeShapeGradient.SetRotation(Value: TSCDeGradientRotation);
begin
  if Value < Low(TSCDeGradientRotation) then
    Value := Low(TSCDeGradientRotation)
  else if Value > High(TSCDeGradientRotation) then
    Value := High(TSCDeGradientRotation);

  if FRotation <> Value then
  begin
    Changing;

    FRotation := Value;
    Changed;
  end;
end;

procedure TSCDeShapeGradient.SetReverse(Value: Boolean);
begin
  if FReverse <> Value then
  begin
    Changing;

    FReverse := Value;
    Changed;
  end;
end;

procedure TSCDeShapeGradient.UpdatePattern;
var
  RGB1, RGB2: TRGBQuad;
  Colors: TSCDeGradientColors;
  dRed, dGreen, dBlue: Integer;
  RGBColor1, RGBColor2: TColor;
  Index, rIndex, M, rM: Integer;
begin
  if not FNeedsUpdate then
    Exit;

  FNeedsUpdate := False;

  if Reverse then
  begin
    RGBColor1 := ColorToRGB(ColorEnd);
    RGBColor2 := ColorToRGB(ColorBegin);
  end
  else
  begin
    RGBColor1 := ColorToRGB(ColorBegin);
    RGBColor2 := ColorToRGB(ColorEnd);
  end;

  RGB1.rgbRed := GetRValue(RGBColor1);
  RGB1.rgbGreen := GetGValue(RGBColor1);
  RGB1.rgbBlue := GetBValue(RGBColor1);
  RGB1.rgbReserved := 0;

  RGB2.rgbRed := GetRValue(RGBColor2);
  RGB2.rgbGreen := GetGValue(RGBColor2);
  RGB2.rgbBlue := GetBValue(RGBColor2);
  RGB2.rgbReserved := 0;

  if Shift > 0 then
  begin
    RGB1.rgbRed := Byte(RGB1.rgbRed + MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB1.rgbGreen := Byte(RGB1.rgbGreen + MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB1.rgbBlue := Byte(RGB1.rgbBlue + MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
  end
  else if Shift < 0 then
  begin
    RGB2.rgbRed := Byte(RGB2.rgbRed + MulDiv(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB2.rgbGreen := Byte(RGB2.rgbGreen + MulDiv(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB2.rgbBlue := Byte(RGB2.rgbBlue + MulDiv(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
  end;

  dRed := RGB2.rgbRed - RGB1.rgbRed;
  dGreen := RGB2.rgbGreen - RGB1.rgbGreen;
  dBlue := RGB2.rgbBlue - RGB1.rgbBlue;

  M := MulDiv(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div 255;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div 255;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div 255;
      end
  else if M > 0 then
  begin
    M := 255 - M;
    for Index := 0 to M - 1 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div M;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div M;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div M;
      end;
    for Index := M to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB1.rgbRed + ((rIndex) * dRed) div (rM);
        rgbGreen := RGB1.rgbGreen + ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB1.rgbBlue + ((rIndex) * dBlue) div (rM);
      end;
  end else
  if M < 0 then
  begin
    M := -M;
    for Index := 0 to M do
      with Colors[Index] do
      begin
        rgbRed := RGB2.rgbRed - (Index * dRed) div M;
        rgbGreen := RGB2.rgbGreen - (Index * dGreen) div M;
        rgbBlue := RGB2.rgbBlue - (Index * dBlue) div M;
      end;
    for Index := M + 1 to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB2.rgbRed - ((rIndex) * dRed) div (rM);
        rgbGreen := RGB2.rgbGreen - ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB2.rgbBlue - ((rIndex) * dBlue) div (rM);
      end;
  end;

  FDirty := True;
  try
    if @PatternBuilder[Style] <> nil then
      PatternBuilder[Style](Colors, Pattern)
    else begin
      Pattern.Width := 2;
      Pattern.Height := 2;
      Pattern.Canvas.Pixels[0, 0] := RGBColor1;
      Pattern.Canvas.Pixels[0, 1] := RGBColor2;
      Pattern.Canvas.Pixels[1, 0] := RGBColor2;
      Pattern.Canvas.Pixels[1, 1] := RGBColor1;
    end;
  finally
    FDirty := False;
  end;
end;

procedure TSCDeShapeGradient.SetOwner(AOwner: TSCDeShapeBase);
begin
  FOwner := AOwner;
end;

procedure TSCDeShapeGradient.ShapeColorChanged;
begin
  FNeedsUpdate := True;
  UpdatePattern;
end;

{ TSCDeShapeFont }

procedure TSCDeShapeFont.Assign(Source: TPersistent);
begin
  if Source is TSCDeShapeFont then
  begin
    Changing;
    with TSCDeShapeFont(Source) do
    begin
      Self.FCharset := Charset;
      Self.FColor  := Color;
      Self.FName  := Name;
      Self.FSize  := Size;
      Self.FStyle := Style;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeShapeFont.AssignRec(const Source: TSCDeShapeFontRec);
begin
  Changing;
  with TSCDeShapeFontRec(Source) do
  begin
    Self.FCharset := Charset;
    Self.FColor  := Color;
    Self.FName  := Name;
    Self.FSize  := Size;
    Self.FStyle := Style;
  end;
  Changed;
end;

procedure TSCDeShapeFont.AssignTo(Dest: TPersistent);
begin
  if Dest is TFont then
  begin
    with TFont(Dest) do
    begin
      Charset := Self.Charset;
      Color := Self.Color;
      Name := Self.Name;
      Size := Self.Size;
      Style := Self.Style;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TSCDeShapeFont.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSCDeShapeFont.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

constructor TSCDeShapeFont.Create;
begin
  inherited Create;
  FCharset := DEFAULT_CHARSET;
  FColor := clWindowText;
  FName := 'MS Sans Serif';
  FSize := 8;
  FStyle := [];
end;

function TSCDeShapeFont.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeShapeFont.LoadFromXML(Node: TSCDomElement);

  function GetProperty(Parent: TSCDomElement; AName: String): String;
  var
    I: Integer;
    Prop: TSCDomElement;
  begin
    Result := '';

    if Parent <> nil then
    begin
      Prop := Parent.ElementByName(AName);

      if Prop <> nil then
        for I := 0 to Prop.ChildNodeCount-1 do
          if Prop.ChildNodes[I] is TSCDomText then
          begin
            Result := Trim(TSCDomText(Prop.ChildNodes[I]).Text);

            if (Result <> '') and (Result[1] = #13) then Delete(Result, 1, 1);
            if (Result <> '') and (Result[1] = #10) then Delete(Result, 1, 1);

            Result := Trim(Result);

            Break;
          end;
    end;
  end;

var
  S: String;
  I: Integer;
begin
  Changing;
  try
    S := GetProperty(Node, 'charset');
    FCharset := StrToIntDef(S, DEFAULT_CHARSET);

    S := GetProperty(Node, 'color');
    if S <> '' then FColor := StringToColor(S);

    S := GetProperty(Node, 'color');
    FName := S;

    S := GetProperty(Node, 'size');
    FSize := StrToIntDef(S, 8);

    S := GetProperty(Node, 'style');
    I := StrToIntDef(S, 0);

    FStyle := [];

    if I and 1 = 1 then Include(FStyle, fsBold);
    if I and 2 = 2 then Include(FStyle, fsItalic);
    if I and 4 = 4 then Include(FStyle, fsUnderline);
    if I and 8 = 8 then Include(FStyle, fsStrikeOut);
  finally
    Changed;
  end;
end;

procedure TSCDeShapeFont.SaveToXML(Node: TSCDomElement);

  procedure AddProperty(const AName, AValue: String);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := TSCDomElement.Create(Node);
    Prop.Name := AName;
    Node.AddElement(Prop);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

var
  I: Integer;
begin
  AddProperty('charset', IntToStr(FCharset));
  AddProperty('color', ColorToString(FColor));
  AddProperty('name', FName);
  AddProperty('size', IntToStr(FSize));

  I := 0;
  if fsBold in FStyle then Inc(I, 1);
  if fsItalic in FStyle then Inc(I, 2);
  if fsUnderline in FStyle then Inc(I, 4);
  if fsStrikeOut in FStyle then Inc(I, 8);

  AddProperty('style', IntToStr(I));
end;

procedure TSCDeShapeFont.SetCharset(Value: TFontCharset);
begin
  if FCharset <> Value then
  begin
    Changing;

    FCharset := Value;
    Changed;
  end;
end;

procedure TSCDeShapeFont.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    Changing;

    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeShapeFont.SetName(Value: TFontName);
begin
  if FName <> Value then
  begin
    Changing;

    FName := Value;
    Changed;
  end;
end;

procedure TSCDeShapeFont.SetOwner(AOwner: TSCDeShapeBase);
begin
  FOwner := AOwner;
end;

procedure TSCDeShapeFont.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    Changing;

    FSize := Value;
    Changed;
  end;
end;

procedure TSCDeShapeFont.SetStyle(Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    Changing;

    FStyle := Value;
    Changed;
  end;
end;

{ TSCDeShapeNotifier }

procedure TSCDeShapeNotifier.Changed(S: TSCDeShapeBase);
begin
  if Assigned(FOnChange) and (S <> nil) and
    (FShapes <> nil) and (FShapes.IndexOf(S) > -1)  then
    FOnChange(Self, S);
end;

procedure TSCDeShapeNotifier.Changing(S: TSCDeShapeBase);
begin
  if Assigned(FOnChanging) and (S <> nil) and
    (FShapes <> nil) and (FShapes.IndexOf(S) > -1)  then
    FOnChanging(Self, S);
end;

destructor TSCDeShapeNotifier.Destroy;
begin
  UnregisterAll;
  inherited Destroy;
end;

procedure TSCDeShapeNotifier.Destroyed(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (FShapes <> nil) then
  begin
    Index := FShapes.IndexOf(S);

    if Index > -1 then
    begin
      FShapes.Delete(Index);
      if FShapes.Count = 0 then FreeAndNil(FShapes);

      S.UnregisterNotifier(Self);

      if Assigned(FOnDestroy) then FOnDestroy(Self, S);
    end;
  end;
end;

function TSCDeShapeNotifier.GetCount: Integer;
begin
  Result := 0;
  if FShapes <> nil then Result := FShapes.Count;
end;

function TSCDeShapeNotifier.GetShape(Index: Integer): TSCDeShapeBase;
begin
  Result := nil;
  if FShapes <> nil then Result := FShapes[Index];
end;

procedure TSCDeShapeNotifier.RegisterShape(S: TSCDeShapeBase);
begin
  if S <> nil then
  begin
    if FShapes = nil then FShapes := TList.Create;

    if FShapes.IndexOf(S) = -1 then
    begin
      FShapes.Add(S);
      S.RegisterNotifier(Self);
    end;
  end;
end;

procedure TSCDeShapeNotifier.UnregisterAll;
var
  S: TSCDeShapeBase;
begin
  if FShapes <> nil then
  begin
    while FShapes.Count > 0 do
    begin
      S := FShapes[FShapes.Count-1];
      FShapes.Delete(FShapes.Count-1);

      S.UnregisterNotifier(Self);
    end;

    FreeAndNil(FShapes);
  end;
end;

procedure TSCDeShapeNotifier.UnregisterShape(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (FShapes <> nil) then
  begin
    Index := FShapes.IndexOf(S);

    if Index > -1 then
    begin
      FShapes.Delete(Index);
      S.UnregisterNotifier(Self);
    end;
  end;
end;

{ TSCDeRegister }

class function TSCDeRegister.FindShape(const AShapeName: string): TSCDeShapeClass;
var
  Index: Integer;
begin
  Result := nil;

  if ClassList <> nil then
  begin
    Index := ClassList.IndexOf(AShapeName);
    if Index > -1 then Result := TSCDeShapeClass(ClassList.Objects[Index]);
  end;
end;

class function TSCDeRegister.RegisterShape(AShape: TSCDeShapeClass): Boolean;
var
  AClassName: string;
begin
  Result := False;
  if ClassList = nil then ClassList := TStringList.Create;

  AClassName := AShape.ClassName;

  if ClassList.IndexOf(AClassName) = -1 then
  begin
    ClassList.AddObject(AClassName, TObject(AShape));
    Result := True;
  end;
end;

class procedure TSCDeRegister.UnRegisterShape(AShape: TSCDeShapeClass);
var
  Index: Integer;
  AClassName: string;
begin
  if ClassList <> nil then
  begin
    AClassName := AShape.ClassName;

    Index := ClassList.IndexOf(AClassName);
    if Index > -1 then ClassList.Delete(Index);
  end;
end;

{ TSCDeCustomViewer }

procedure TSCDeCustomViewer.DoLayerRemoved(L: TSCDeLayer);
begin
  if (FHotShape <> nil) and not Exists(FHotShape) then FHotShape := nil;
  if (FDownShape <> nil) and not Exists(FDownShape) then FDownShape := nil;
end;

procedure TSCDeCustomViewer.DoShapeRemoved(S: TSCDeShapeBase);
begin
  if (FHotShape <> nil) and (S = FHotShape) then FHotShape := nil;
  if (FDownShape <> nil) and (S = FDownShape) then FDownShape := nil;
end;

procedure TSCDeCustomViewer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Hit: TSCDeHitTest;
  OldHot: TSCDeShapeBase;
begin
  inherited MouseDown(Button, Shift, X, Y);

  Hit := Self.HitTest(TSCDeUtils.Point(X, Y));

  OldHot := FHotShape;

  FHotShape := Hit.Shape;
  FDownShape := Hit.Shape;

  if Assigned(FOnShapeDown) then FOnShapeDown(Self, FDownShape);
  if (FHotShape <> OldHot) and Assigned(FOnShapeMove) then
    FOnShapeMove(Self, FHotShape);
end;

procedure TSCDeCustomViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Hit: TSCDeHitTest;
  OldHot: TSCDeShapeBase;
begin
  inherited MouseMove(Shift, X, Y);

  OldHot := FHotShape;

  Hit := Self.HitTest(TSCDeUtils.Point(X, Y));
  FHotShape := Hit.Shape;

  if (FDownShape = nil) and (OldHot <> FHotShape) and Assigned(FOnShapeMove) then
    FOnShapeMove(Self, FHotShape);
end;

procedure TSCDeCustomViewer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Hit: TSCDeHitTest;
  OldDown, OldHot: TSCDeShapeBase;
begin
  inherited MouseUp(Button, Shift, X, Y);

  OldDown := FDownShape;
  FDownShape := nil;

  if (OldDown <> nil) and Assigned(FOnShapeUp) then
    FOnShapeUp(Self, OldDown);

  OldHot := FHotShape;

  Hit := Self.HitTest(TSCDeUtils.Point(X, Y));
  FHotShape := Hit.Shape;

  if (FDownShape = nil) and (OldHot <> FHotShape) and Assigned(FOnShapeMove) then
    FOnShapeMove(Self, FHotShape);
end;

{ TSCDeSurfaceClient }

constructor TSCDeSurfaceClient.Create(ASender: TSCDeSurface);
begin
  inherited Create;
  if ASender <> nil then ASender.RegisterClient(Self);
end;

destructor TSCDeSurfaceClient.Destroy;
begin
  if FSender <> nil then FSender.UnRegisterClient(Self);
  inherited Destroy;
end;

procedure TSCDeSurfaceClient.DoActiveLayerChange;
begin
  if Assigned(OnActiveLayerChange) then
    OnActiveLayerChange(FSender);
end;

procedure TSCDeSurfaceClient.DoAfterLoadDOM(Root: TSCDomElement);
begin
  if Assigned(OnAfterLoadDOM) then
    OnAfterLoadDOM(FSender, Root);
end;

procedure TSCDeSurfaceClient.DoAfterSaveDOM(Root: TSCDomElement);
begin
  if Assigned(OnAfterSaveDOM) then
    OnAfterSaveDOM(FSender, Root);
end;

procedure TSCDeSurfaceClient.DoBeforeLoadDOM(Root: TSCDomElement);
begin
  if Assigned(OnBeforeLoadDOM) then
    OnBeforeLoadDOM(FSender, Root);
end;

procedure TSCDeSurfaceClient.DoBeforeSaveDOM(Root: TSCDomElement);
begin
  if Assigned(OnBeforeSaveDOM) then
    OnBeforeSaveDOM(FSender, Root);
end;

procedure TSCDeSurfaceClient.DoFramePropsChange;
begin
  if Assigned(OnFramePropsChange) then
    OnFramePropsChange(FSender);
end;

procedure TSCDeSurfaceClient.DoGridPropsChange;
begin
  if Assigned(OnGridPropsChange) then
    OnGridPropsChange(FSender);
end;

procedure TSCDeSurfaceClient.DoInsertLayer(Shape: TSCDeShapeBase);
begin
  if Assigned(OnInsertLayer) then
    OnInsertLayer(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoInsertShape(Shape: TSCDeShapeBase);
begin
  if Assigned(OnInsertShape) then
    OnInsertShape(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoLayerChange(Shape: TSCDeShapeBase);
begin
  if Assigned(OnLayerChange) then
    OnLayerChange(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoLayerPropsChange;
begin
  if Assigned(OnLayerPropsChange) then
    OnLayerPropsChange(FSender);
end;

procedure TSCDeSurfaceClient.DoRemoveLayer(Shape: TSCDeShapeBase);
begin
  if Assigned(OnRemoveLayer) then
    OnRemoveLayer(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoRemoveShape(Shape: TSCDeShapeBase);
begin
  if Assigned(OnRemoveShape) then
    OnRemoveShape(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoShapeCleared(Shape: TSCDeShapeBase);
begin
  if Assigned(OnClearShape) then
    OnClearShape(FSender, Shape);
end;

procedure TSCDeSurfaceClient.DoZoom;
begin
  if Assigned(OnZoom) then
    OnZoom(FSender);
end;

initialization
  ClassList := TStringList.Create;

  TSCDeRegister.RegisterShape(TSCDePackage);
  TSCDeRegister.RegisterShape(TSCDeGroup);
  TSCDeRegister.RegisterShape(TSCDeLayer);

finalization
  FreeAndNil(ClassList);

end.
