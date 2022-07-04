{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingEditor;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SCDECommon, SCDEConsts, SCDEBitmap, SCDEControl, SCDrawingCommons,
  SCDrawingSurface, SCDrawingShapes, SCXML;

type
  TSCDeCustomEditor = class;

  TSCDeEditorClient = class(TSCDeSurfaceClient)
  private
    FOnClipboardChange: TNotifyEvent;
    FOnEditStateChange: TNotifyEvent;
    FOnEndNewShape: TSCDeShapeEvent;
    FOnNewShape: TSCDeShapeEvent;
    FOnNewShapeClass: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnDefBrushChange: TNotifyEvent;
    FOnDefFontChange: TNotifyEvent;
    FOnDefPenChange: TNotifyEvent;
    FOnGuideChange: TNotifyEvent;
  protected
    procedure DoClipboardChange; dynamic;
    procedure DoEditStateChange; dynamic;
    procedure DoEndNewShape(Shape: TSCDeShapeBase); dynamic;
    procedure DoNewShape(Shape: TSCDeShapeBase); dynamic;
    procedure DoNewShapeClass; dynamic;
    procedure DoSelectionChange; dynamic;
    procedure DoDefBrushChange; dynamic;
    procedure DoDefFontChange; dynamic;
    procedure DoDefPenChange; dynamic;
    procedure DoGuideChange; dynamic;
  public
    property OnClipboardChange: TNotifyEvent read FOnClipboardChange write FOnClipboardChange;
    property OnEditStateChange: TNotifyEvent read FOnEditStateChange write FOnEditStateChange;
    property OnEndNewShape: TSCDeShapeEvent read FOnEndNewShape write FOnEndNewShape;
    property OnNewShape: TSCDeShapeEvent read FOnNewShape write FOnNewShape;
    property OnNewShapeClass: TNotifyEvent read FOnNewShapeClass write FOnNewShapeClass;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnDefBrushChange: TNotifyEvent read FOnDefBrushChange write FOnDefBrushChange;
    property OnDefFontChange: TNotifyEvent read FOnDefFontChange write FOnDefFontChange;
    property OnDefPenChange: TNotifyEvent read FOnDefPenChange write FOnDefPenChange;
    property OnGuideChange: TNotifyEvent read FOnGuideChange write FOnGuideChange;
  end;

  TSCDeToolPaintData = record
    LineColor: TColor;
    LineStyle: TPenStyle;
    LineMode: TPenMode;
  end;
  PSCDeToolPaintData = ^TSCDeToolPaintData;

  TSCDeToolBase = class(TPersistent)
  private
    FPoints: TList;
    FCursor: TCursor;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FOwner: TSCDeCustomEditor;
    function  GetCount: Integer;
    function  GetInUpdate: Boolean;
    function  GetPoint(Index: Integer): TDoublePoint;
    procedure SetPoint(Index: Integer; P: TDoublePoint);
  protected
    function  GetOwner: TPersistent; override;

    procedure Changed;
    procedure DoChanged; dynamic;
  public
    constructor Create(AOwner: TSCDeCustomEditor); virtual;
    procedure  Assign(Source: TPersistent); override;
    destructor Destroy; override;

    procedure Process; dynamic;
    procedure Paint(C: TCanvas; X, Y: Double;
      const Data: TSCDeToolPaintData; Zoom: Double); virtual;

    procedure Add(const P: TDoublePoint); dynamic;
    procedure Delete(Index: Integer); dynamic;
    procedure Clear; dynamic;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TSCDeCustomEditor read FOwner;
    property InUpdate: Boolean read GetInUpdate;
    property Count: Integer read GetCount;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property Points[Index: Integer]: TDoublePoint read GetPoint write SetPoint;
  end;

  TSCDeSelectionProps = class(TPersistent)
  private
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    FLineMode: TPenMode;
    FCtrlInColor: TColor;
    FCtrlOutColor: TColor;
    FPointInColor: TColor;
    FPointOutColor: TColor;
    FPointSize: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLineColor(Value: TColor);
    procedure SetLineStyle(Value: TPenStyle);
    procedure SetLineMode(Value: TPenMode);
    procedure SetCtrlInColor(Value: TColor);
    procedure SetCtrlOutColor(Value: TColor);
    procedure SetPointInColor(Value: TColor);
    procedure SetPointOutColor(Value: TColor);
    procedure SetPointSize(Value: Integer);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default $FFA800;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psSolid;
    property LineMode: TPenMode read FLineMode write SetLineMode default pmCopy;
    property CtrlInColor: TColor read FCtrlInColor write SetCtrlInColor default clYellow;
    property CtrlOutColor: TColor read FCtrlOutColor write SetCtrlOutColor default $FFA800;
    property PointInColor: TColor read FPointInColor write SetPointInColor default clWhite;
    property PointOutColor: TColor read FPointOutColor write SetPointOutColor default $FFA800;
    property PointSize: Integer read FPointSize write SetPointSize default 5;
  end;

  TSCDeHottrackProps = class(TSCDeSelectionProps)
  public
    constructor Create; override;
  published
    property LineColor default clNone;
    property LineStyle default psClear;
    property LineMode;
    property CtrlInColor default clRed;
    property CtrlOutColor default clRed;
    property PointInColor default clRed;
    property PointOutColor default clRed;
    property PointSize;
  end;

  TSCDeMoveSizeProps = class(TPersistent)
  private
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    FLineMode: TPenMode;
    FOnChange: TNotifyEvent;
    procedure SetLineColor(Value: TColor);
    procedure SetLineStyle(Value: TPenStyle);
    procedure SetLineMode(Value: TPenMode);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default clFuchsia;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psDot;
    property LineMode: TPenMode read FLineMode write SetLineMode default pmCopy;
  end;

  TSCDeSelectionRectProperties = class(TPersistent)
  private
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    FLineMode: TPenMode;
    FOwner: TSCDeCustomEditor;
    FOnChange: TNotifyEvent;
    procedure SetLineColor(Value: TColor);
    procedure SetLineStyle(Value: TPenStyle);
    procedure SetLineMode(Value: TPenMode);

    procedure Changed;
  protected
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TSCDeCustomEditor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psDot;
    property LineMode: TPenMode read FLineMode write SetLineMode default pmNotXor;
  end;

  TSCDeGuides = class;

  TSCDeGuide = class(TPersistent)
  private
    FPosition: Double;
    FGuideType: TSCDeGuideType;
    FOwner: TSCDeGuides;
    FDestroying: Boolean;
    function  GetIndex: Integer;
    procedure SetPosition(Value: Double);
    procedure SetGuideType(Value: TSCDeGuideType);
    procedure Changed;
  protected
    function  GetOwner: TPersistent; override;
    procedure Destroying;
  public
    constructor Create(AOwner: TSCDeGuides); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Owner: TSCDeGuides read FOwner;
    property Index: Integer read GetIndex;
    property InDestroy: Boolean read FDestroying;
    property Position: Double read FPosition write SetPosition;
    property GuideType: TSCDeGuideType read FGuideType write SetGuideType default scgtHorizontal;
  end;

  TSCDeGuides = class(TPersistent)
  private
    FItems: TList;
    FOwner: TSCDeCustomEditor;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FDestroying: Boolean;
    function  GetCount: Integer;
    function  GetItem(Index: Integer): TSCDeGuide;
    function  GetInUpdate: Boolean;
  protected
    function  GetOwner: TPersistent; override;
    procedure Changed(G: TSCDeGuide);
    procedure Destroying;

    procedure Insert(G: TSCDeGuide);
    procedure Remove(G: TSCDeGuide);
  public
    constructor Create(AOwner: TSCDeCustomEditor); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function  Add: TSCDeGuide; overload;
    procedure Add(G: TSCDeGuide); overload;
    procedure Delete(Index: Integer);
    function  IndexOf(G: TSCDeGuide): Integer;
    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    property InUpdate: Boolean read GetInUpdate;
    property InDestroy: Boolean read FDestroying;
    property Owner: TSCDeCustomEditor read FOwner;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSCDeGuide read GetItem; default;
  end;

  TSCDeGuideProperties = class(TPersistent)
  private
    FColor: TColor;
    FStyle: TPenStyle;
    FMode: TPenMode;
    FOwner: TSCDeCustomEditor;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetStyle(Value: TPenStyle);
    procedure SetMode(Value: TPenMode);

    procedure Changed;
  protected
    function GetOwner: TPersistent; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TSCDeCustomEditor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property Mode: TPenMode read FMode write SetMode default pmCopy;
  end;

  TSCDeRulers = class;

  TSCDeRuler = class(TPersistent)
  private
    FOwner: TSCDeRulers;
    FOffset: Integer;
    FPosition: Integer;
    FRulerType: TSCDeRulerType;
    function  GetStep(Zoom: Double): Double;
    procedure DrawHorzRuler(C: TCanvas; R: TRect; Zoom: Double);
    procedure DrawVertRuler(C: TCanvas; R: TRect; Zoom: Double);
  protected
    procedure SetOffset(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetRulerType(Value: TSCDeRulerType);

    procedure Paint(C: TCanvas; R: TRect; Zoom: Double); virtual;
  public
    constructor Create(AOwner: TSCDeRulers); virtual;

    property Offset: Integer read FOffset;
    property Position: Integer read FPosition;
    property RulerType: TSCDeRulerType read FRulerType;
  end;

  TSCDeRulers = class(TPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FGuideColor: TColor;
    FMarkColor: TColor;
    FMode: TSCDeRulerMode;
    FShowNumbers: Boolean;
    FSize: Word;
    FVisible: Boolean;
    FHorzRuler: TSCDeRuler;
    FVertRuler: TSCDeRuler;
    FForced: Boolean;
    FUpdateCount: Cardinal;
    FChangeCount: Cardinal;
    FOwner: TSCDeCustomEditor;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetGuideColor(Value: TColor);
    procedure SetMarkColor(Value: TColor);
    procedure SetMode(Value: TSCDeRulerMode);
    procedure SetShowNumbers(Value: Boolean);
    procedure SetSize(Value: Word);
    procedure SetVisible(Value: Boolean);
    function  GetInUpdate: Boolean;
    procedure FontChanged(Sender: TObject);
  protected
    function  GetOwner: TPersistent; override;
    procedure Changed(Force: Boolean = False);

    property HorzRuler: TSCDeRuler read FHorzRuler;
    property VertRuler: TSCDeRuler read FVertRuler;
  public
    constructor Create(AOwner: TSCDeCustomEditor); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property InUpdate: Boolean read GetInUpdate;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property GuideColor: TColor read FGuideColor write SetGuideColor default clBlue;
    property MarkColor: TColor read FMarkColor write SetMarkColor default clWindowText;
    property Mode: TSCDeRulerMode read FMode write SetMode default scrmPixel;
    property ShowNumbers: Boolean read FShowNumbers write SetShowNumbers default True;
    property Size: Word read FSize write SetSize default 25;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TSCDeCustomEditor = class(TSCDeSurface)
  private
    FHitFuzz: Byte;
    FUndoLock: Integer;
    FGuides: TSCDeGuides;
    FControlKeys: TSCDeShiftKeys;
    FDefaultBrush: TSCDeShapeBrush;
    FDefaultPen: TSCDeShapePen;
    FDefaultFont: TSCDeShapeFont;
    FEditOptions: TSCDeEditOptions;
    FSelectionList: TList;
    FNewShape: TSCDeShapeBase;
    FHotShape: TSCDeShapeBase;
    FEditState: TSCDeEditState;
    FPaintBuffer: TSCDeBitmap;
    FHitRec: TSCDeHitTest;
    FMoveRec: TSCDeHitTest;
    FRulers: TSCDeRulers;
    FCursorPos: TPoint;
    FPaintNeeded: Boolean;
    FPaintCount: Integer;
    FDefaultCursor: TCursor;
    FUpdatingCursor: Integer;
    FActionStartRec: TSCDeHitTest;
    FNewShapeClass: TSCDeShapeClass;
    FSelectionRectKeys: TSCDeShiftKeys;
    FMultiSelectKeys: TSCDeShiftKeys;
    FMouseSense: Byte;
    FKeyMoveSizeStep: Double;
    FCurrentTool: TSCDeToolBase;
    FGuideProperties: TSCDeGuideProperties;
    FCursorGuideProperties: TSCDeGuideProperties;
    FHottrackProps: TSCDeHottrackProps;
    FMoveSizeProps: TSCDeMoveSizeProps;
    FSelectionProps: TSCDeSelectionProps;
    FSelectionRectProperties: TSCDeSelectionRectProperties;
    FTempShape: TSCDeShapeBase;
    FNewShapeNotifier: TSCDeShapeNotifier;
    FHasClipboardData: Boolean;
    FNextClipViewer: HWND;
    FClipViewerHandle: HWND;
    FInUndo: Boolean;
    FInRedo: Boolean;
    FUndoGroup: TSCDomElement;
    FRedoGroup: TSCDomElement;
    FUndoGrouping: Cardinal;
    FRedoGrouping: Cardinal;
    FUndoLimit: Integer;
    FUndoBuffer: TSCDomDocument;
    FRedoBuffer: TSCDomDocument;
    FModified: Boolean;
    FOnEditStateChange: TNotifyEvent;
    FOnEndNewShape: TSCDeShapeEvent;
    FOnNewShape: TSCDeShapeEvent;
    FOnNewShapeClass: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnDefBrushChange: TNotifyEvent;
    FOnDefFontChange: TNotifyEvent;
    FOnDefPenChange: TNotifyEvent;
    FOnGuideChange: TNotifyEvent;
    FOnClipboardChange: TNotifyEvent;

    procedure SetControlKeys(Value: TSCDeShiftKeys);
    procedure SetCursorGuideProperties(Value: TSCDeGuideProperties);
    procedure SetDefaultBrush(Value: TSCDeShapeBrush);
    procedure SetDefaultPen(Value: TSCDeShapePen);
    procedure SetDefaultFont(Value: TSCDeShapeFont);
    procedure SetEditOptions(Value: TSCDeEditOptions);
    procedure SetRulers(Value: TSCDeRulers);
    procedure SetSelectionRectKeys(Value: TSCDeShiftKeys);
    procedure SetMultiSelectKeys(Value: TSCDeShiftKeys);
    procedure SetGuideProperties(Value: TSCDeGuideProperties);
    procedure SetHottrackProps(Value: TSCDeHottrackProps);
    procedure SetMoveSizeProps(Value: TSCDeMoveSizeProps);
    procedure SetSelectionProps(Value: TSCDeSelectionProps);
    procedure SetSelectionRectProperties(Value: TSCDeSelectionRectProperties);
    function  GetSelectionCount: Integer;
    function  GetSelection(Index: Integer): TSCDeShapeBase;
    procedure SetMouseSense(Value: Byte);
    procedure SetModified(Value: Boolean);
    procedure SetUndoLimit(Value: Integer);
    procedure SetKeyMoveSizeStep(Value: Double);

    procedure DefaultBrushChanged(Sender: TObject);
    procedure DefaultPenChanged(Sender: TObject);
    procedure DefaultFontChanged(Sender: TObject);

    function  CreateNewShape: TSCDeShapeBase;
    procedure EndNewShape;

    procedure SetHotShape(S: TSCDeShapeBase);
    procedure UpdateHottrack;

    procedure ExecuteHitAction(MouseAction: TSCDeEditState;
      OldState, NewState: TSCDeHitTest; Shift: TShiftState);

    procedure CursorGuidePropertiesChanged(Sender: TObject);
    procedure GuidePropertiesChanged(Sender: TObject);
    procedure HottrackPropsChanged(Sender: TObject);
    procedure SelectionPropsChanged(Sender: TObject);
    procedure SelectionRectPropertiesChanged(Sender: TObject);

    procedure InitiateUndoBuffer;
    procedure InitiateRedoBuffer;

    procedure NewShapeDestroyed(Sender: TObject; S: TSCDeShapeBase);

    procedure UpdateClipboardData;
    function  GetClipboardText: WideString;

    procedure CreateUndo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String);
    procedure CreateRedo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String);

    procedure ClearRedo;
    procedure BeginRedoGroup;
    procedure EndRedoGroup;

    procedure EditStateChanged;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Message: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
  protected
    procedure DestroyWindowHandle; override;
    procedure CreateWnd; override;
    procedure Loaded; override;

    procedure CheckUndoBufferLength;
    procedure NotifyUndo(S: TSCDeShapeBase; Action: TSCDeAction;
      const PropName: String); override;

    procedure Paint; override;
    procedure UpdatePaintBuffer; override;

    procedure AfterUpdateBuffers(C: TCanvas); override;
    procedure MouseInControlChanged; override;
    procedure FocusChanged; override;

    procedure UpdateCursorPos;
    function  GetCurrentCursor(const P: TDoublePoint; var Cr: TCursor): Boolean; dynamic;
    procedure UpdateCursor(X, Y: Double; UseXY: Boolean = False); dynamic;

    function  GetPaintRect: TRect; override;
    function  GetNominalViewRect: TDoubleRect; override;
    function  GetViewRect: TDoubleRect; override;

    procedure GuideChanged(G: TSCDeGuide);
    procedure DoGuideChanged(G: TSCDeGuide); dynamic;

    procedure SelectionChanged;
    procedure DoSelectionChanged; virtual;
    procedure RefineSelections; override;

    procedure DoShapeRemoved(S: TSCDeShapeBase); override;
    procedure DoLayerRemoved(L: TSCDeLayer); override;

    procedure DrawNewGuide(C: TCanvas); virtual;
    procedure DrawGuideDrag(C: TCanvas); virtual;
    procedure DrawGuides(C: TCanvas); virtual;
    procedure DrawRulers(C: TCanvas); virtual;
    procedure DrawNewCreation(C: TCanvas); virtual;
    procedure DrawHottrack(C: TCanvas); virtual;
    procedure DrawSelection(C: TCanvas); override;
    procedure DrawSelectionRect(C: TCanvas); virtual;
    procedure DrawSelectionMove(C: TCanvas); virtual;
    procedure DrawSelectionSize(C: TCanvas); virtual;
    procedure DrawCursorGuide(C: TCanvas); virtual;

    procedure HotShapeChanged; virtual;
    procedure SetEditState(Value: TSCDeEditState);

    procedure DoEditStateChanged; virtual;

    procedure InvalidateRulers;
    procedure RulersChanged(Sender: TObject; Forced: Boolean);
    procedure DoRulersChanged(Sender: TObject; Forced: Boolean); dynamic;

    procedure LayerBounds(var CR: TRect; var R: TDoubleRect); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure ApplyControlMove(S: TSCDeShapeBase; CtrlIndex: Integer;
      StartPoint, EndPoint: TDoublePoint; Shift: TShiftState); dynamic;
    procedure ApplySizing(S: TSCDeShapeBase; PointIndex, SubIndex: Integer;
      StartPoint, EndPoint: TDoublePoint; Shift: TShiftState); dynamic;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    property BufferedPaint default True;
    property InUndo: Boolean read FInUndo;
    property InRedo: Boolean read FInRedo;
    property ControlKeys: TSCDeShiftKeys read FControlKeys write SetControlKeys default [scskAlt];
    property CursorGuide: TSCDeGuideProperties read FCursorGuideProperties write SetCursorGuideProperties;
    property DefaultBrush: TSCDeShapeBrush read FDefaultBrush write SetDefaultBrush;
    property DefaultPen: TSCDeShapePen read FDefaultPen write SetDefaultPen;
    property DefaultFont: TSCDeShapeFont read FDefaultFont write SetDefaultFont;
    property EditOptions: TSCDeEditOptions read FEditOptions write SetEditOptions default DeDefaultEditOptions;
    property Guides: TSCDeGuides read FGuides;
    property Guide: TSCDeGuideProperties read FGuideProperties write SetGuideProperties;
    property HitFuzz: Byte read FHitFuzz write FHitFuzz default 2;
    property Hottrack: TSCDeHottrackProps read FHottrackProps write SetHottrackProps;
    property MoveSize: TSCDeMoveSizeProps read FMoveSizeProps write SetMoveSizeProps;
    property MultiSelectKeys: TSCDeShiftKeys read FMultiSelectKeys write SetMultiSelectKeys default [scskCtrl];
    property SelectionRectKeys: TSCDeShiftKeys read FSelectionRectKeys write SetSelectionRectKeys default [scskShift];
    property Selection: TSCDeSelectionProps read FSelectionProps write SetSelectionProps;
    property SelectionRect: TSCDeSelectionRectProperties read FSelectionRectProperties write SetSelectionRectProperties;
    property EditState: TSCDeEditState read FEditState;
    property HotShape: TSCDeShapeBase read FHotShape;
    property KeyMoveSizeStep: Double read FKeyMoveSizeStep write SetKeyMoveSizeStep;
    property SelectionCount: Integer read GetSelectionCount;
    property Selections[Index: Integer]: TSCDeShapeBase read GetSelection;
    property MouseSense: Byte read FMouseSense write SetMouseSense default 1;
    property NewShapeClass: TSCDeShapeClass read FNewShapeClass;
    property CurrentTool: TSCDeToolBase read FCurrentTool;
    property Rulers: TSCDeRulers read FRulers write SetRulers;
    property Modified: Boolean read FModified write SetModified default False;
    property UndoLimit: Integer read FUndoLimit write SetUndoLimit default -1;
    property OnClipboardChange: TNotifyEvent read FOnClipboardChange write FOnClipboardChange;
    property OnEditStateChange: TNotifyEvent read FOnEditStateChange write FOnEditStateChange;
    property OnEndNewShape: TSCDeShapeEvent read FOnEndNewShape write FOnEndNewShape;
    property OnNewShape: TSCDeShapeEvent read FOnNewShape write FOnNewShape;
    property OnNewShapeClass: TNotifyEvent read FOnNewShapeClass write FOnNewShapeClass;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnDefBrushChange: TNotifyEvent read FOnDefBrushChange write FOnDefBrushChange;
    property OnDefFontChange: TNotifyEvent read FOnDefFontChange write FOnDefFontChange;
    property OnDefPenChange: TNotifyEvent read FOnDefPenChange write FOnDefPenChange;
    property OnGuideChange: TNotifyEvent read FOnGuideChange write FOnGuideChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginPointAdd;
    procedure EndPointAdd;

    procedure BeginPointRemove;
    procedure EndPointRemove;

    function  GuideTest(P: TDoublePoint; WithState: TSCDeEditState;
      Shift: TShiftState): TSCDeHitTest; virtual;
    function  HitCase(P: TDoublePoint; WithState: TSCDeEditState;
      Shift: TShiftState): TSCDeHitTest; virtual;
    function  HitTest(P: TDoublePoint): TSCDeHitTest; override;

    procedure SetPanView;
    procedure SetSelecting;

    procedure SetNewShapeClass(ShapeClass: TSCDeShapeClass);
    procedure CancelNewShape;
    procedure RestartNewShape;

    procedure SetCurrentTool(ATool: TSCDeToolBase);

    procedure SelectAll;
    procedure ClearSelection;
    procedure UpdateSelection;
    procedure DeleteSelection;

    procedure LoadFromFile(const FileName: String); override;

    function  CanSelect(S: TSCDeShapeBase): Boolean;
    procedure SelectIn(R: TDoubleRect; FullSatisfy: Boolean = False);
    procedure Select(S: TSCDeShapeBase);
    procedure SelectList(A: array of TSCDeShapeBase);
    procedure AddSelection(S: TSCDeShapeBase);
    procedure AddSelections(A: array of TSCDeShapeBase);
    procedure RemoveSelection(S: TSCDeShapeBase);
    procedure RemoveSelections(A: array of TSCDeShapeBase);

    procedure BringForward;
    procedure BringToFront; overload;
    procedure SendBackward;
    procedure SendToBack; overload;

    function  IsSelected(S: TSCDeShapeBase): Boolean;
    procedure GetSelections(L: TList);
    function  GetSelectionBounds: TDoubleRect; override;

    function  Pack(A: array of TSCDeShapeBase): TSCDePackage; override;
    function  Group(A: array of TSCDeShapeBase): TSCDeGroup; override;

    procedure GroupSelection;
    procedure UngroupSelection;
    procedure PackSelection;
    procedure UnpackSelection;

    procedure Lock(A: array of TSCDeShapeBase);
    procedure LockSelection;

    procedure BeginUndoLock;
    procedure EndUndoLock;

    procedure BeginUndoGroup;
    procedure EndUndoGroup;
    function  CanUndo: Boolean;
    function  CanRedo: Boolean;
    procedure ClearUndo;
    procedure Undo;
    procedure Redo;

    procedure InitiateClipViewer;
    procedure ReleaseClipViewer;

    function  HasClipboardData: Boolean; override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    procedure PasteFromClipboard; override;
  end;

  TSCDeEditor = class(TSCDeCustomEditor)
  public
    property Guides;
    property Layers;
    property EditState;
    property SelectionCount;
    property Selections;
    property NewShapeClass;
    property CurrentTool;
    property HorizontalPos;
    property VerticalPos;
    property ActiveLayer;
    property Modified;
    property InLoad;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderProps;
    property Color nodefault;
    property Constraints;
    property ControlKeys;
    property CursorGuide;
    property DefaultBrush;
    property DefaultPen;
    property DefaultFont;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditOptions;
    property Enabled;
    property Frame;
    property Grid;
    property Guide;
    property HitFuzz;
    property Hottrack;
    property Indent;
    property KeyMoveSizeStep;
    property Layer;
    property MouseSense;
    property MoveSize;
    property MultiSelectKeys;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property Picture;
    property PictureProps;
    property PopupMenu;
    property Rulers;
    property Scrollbars;
    property SelectionRectKeys;
    property Selection;
    property SelectionRect;
    property ShowHint;
    property ShowScrollbars;
    property TabOrder;
    property TabStop;
    property UndoLimit;
    property Visible;
    property Zoom;
    property OnActiveLayerChange;
    property OnBeforeLoadDOM;
    property OnAfterLoadDOM;
    property OnBeforeSaveDOM;
    property OnAfterSaveDOM;
    property OnCanResize;
    property OnClearShape;
    property OnChange;
    property OnClick;
    property OnClipboardChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDefBrushChange;
    property OnDefFontChange;
    property OnDefPenChange;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEditStateChange;
    property OnEndDock;
    property OnEndDrag;
    property OnEndNewShape;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGuideChange;
    property OnInsertLayer;
    property OnInsertShape;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewShape;
    property OnNewShapeClass;
    property OnRemoveLayer;
    property OnRemoveShape;
    property OnResize;
    property OnScroll;
    property OnScrollChangeEvent;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnZoom;
  end;

implementation

uses Clipbrd, Math;

type
  TSCDeFakeShape = class(TSCDeShapeBase);
  TSCDeFakeBrush = class(TSCDeShapeBrush);
  TSCDeFakePen = class(TSCDeShapePen);
  TSCDeFakeFont = class(TSCDeShapeFont);

{ TSCDeToolBase }

procedure TSCDeToolBase.Add(const P: TDoublePoint);
var
  Pp: PDoublePoint;
begin
  New(Pp);

  Pp^.x := P.x;
  Pp^.y := P.y;

  FPoints.Add(Pp);
  Changed;
end;

procedure TSCDeToolBase.Assign(Source: TPersistent);
var
  I: Integer;
  P: TDoublePoint;
  Pp: PDoublePoint;
begin
  if Source is TSCDeToolBase then
  begin
    BeginUpdate;
    try
      Clear;

      with TSCDeToolBase(Source) do
      begin
        for I := 0 to Count-1 do
        begin
          P := Points[I];

          New(Pp);

          Pp^.x := P.x;
          Pp^.y := P.y;

          Self.FPoints.Add(Pp);
        end;
      end;
    finally
      Changed;
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TSCDeToolBase.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeToolBase.Changed;
begin
  if FUpdateCount > 0 then
  begin
    Inc(FChangeCount);
    Exit;
  end;

  FChangeCount := 0;
  DoChanged;
end;

procedure TSCDeToolBase.Clear;
var
  Pp: PDoublePoint;
begin
  while FPoints.Count > 0 do
  begin
    Pp := FPoints[FPoints.Count-1];

    FPoints.Delete(FPoints.Count-1);
    Dispose(Pp);
  end;
end;

constructor TSCDeToolBase.Create(AOwner: TSCDeCustomEditor);
begin
  inherited Create;
  FPoints := TList.Create;
  FCursor := crDefault;
  if AOwner <> nil then AOwner.SetCurrentTool(Self);
end;

procedure TSCDeToolBase.Delete(Index: Integer);
var
  Pp: PDoublePoint;
begin
  Pp := FPoints[Index];

  FPoints.Delete(Index);
  Dispose(Pp);
end;

destructor TSCDeToolBase.Destroy;
begin
  Clear;
  if FOwner <> nil then FOwner.SetCurrentTool(nil);
  FreeAndNil(FPoints);
  inherited Destroy;
end;

procedure TSCDeToolBase.DoChanged;
begin
  //
end;

procedure TSCDeToolBase.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FChangeCount > 0) then
      Changed;
  end;
end;

function TSCDeToolBase.GetCount: Integer;
begin
  Result := FPoints.Count;
end;

function TSCDeToolBase.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TSCDeToolBase.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeToolBase.GetPoint(Index: Integer): TDoublePoint;
var
  Pp: PDoublePoint;
begin
  Pp := FPoints[Index];

  Result.x := Pp^.x;
  Result.y := Pp^.y;
end;

procedure TSCDeToolBase.Paint(C: TCanvas; X, Y: Double;
  const Data: TSCDeToolPaintData; Zoom: Double);
begin
  //
end;

procedure TSCDeToolBase.Process;
begin
  //
end;

procedure TSCDeToolBase.SetPoint(Index: Integer; P: TDoublePoint);
var
  Pp: PDoublePoint;
begin
  Pp := FPoints[Index];

  if (Pp^.x <> P.x) or (Pp^.y <> P.y) then
  begin
    Pp^.x := P.x;
    Pp^.y := P.y;

    Changed;
  end;
end;

{ TSCDeSelectionProps }

procedure TSCDeSelectionProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeSelectionProps then
  begin
    with TSCDeSelectionProps(Source) do
    begin
      Self.FLineColor := FLineColor;
      Self.FLineStyle := FLineStyle;
      Self.FLineMode  := FLineMode;
      Self.FCtrlInColor   := FCtrlInColor;
      Self.FCtrlOutColor  := FCtrlOutColor;
      Self.FPointInColor  := FPointInColor;
      Self.FPointOutColor := FPointOutColor;
      Self.FPointSize := FPointSize;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeSelectionProps.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TSCDeSelectionProps.Create;
begin
  inherited Create;
  FLineColor := $FFA800;
  FLineStyle := psSolid;
  FLineMode := pmCopy;
  FCtrlInColor := clYellow;
  FCtrlOutColor := $FFA800;
  FPointInColor := clWhite;
  FPointOutColor := $FFA800;
  FPointSize := 5;
end;

procedure TSCDeSelectionProps.SetCtrlInColor(Value: TColor);
begin
  if FCtrlInColor <> Value then
  begin
    FCtrlInColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetCtrlOutColor(Value: TColor);
begin
  if FCtrlOutColor <> Value then
  begin
    FCtrlOutColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetLineMode(Value: TPenMode);
begin
  if FLineMode <> Value then
  begin
    FLineMode := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetLineStyle(Value: TPenStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetPointInColor(Value: TColor);
begin
  if FPointInColor <> Value then
  begin
    FPointInColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetPointOutColor(Value: TColor);
begin
  if FPointOutColor <> Value then
  begin
    FPointOutColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionProps.SetPointSize(Value: Integer);
begin
  if Value < 0 then Value := 0;

  if FPointSize <> Value then
  begin
    FPointSize := Value;
    Changed;
  end;
end;

{ TSCDeHottrackProps }

constructor TSCDeHottrackProps.Create;
begin
  inherited Create;
  FLineColor := clNone;
  FLineStyle := psClear;
  FCtrlInColor := clRed;
  FCtrlOutColor := clRed;
  FPointInColor := clRed;
  FPointOutColor := clRed;
end;

{ TSCDeMoveSizeProps }

procedure TSCDeMoveSizeProps.Assign(Source: TPersistent);
begin
  if Source is TSCDeMoveSizeProps then
  begin
    with TSCDeMoveSizeProps(Source) do
    begin
      Self.FLineColor := FLineColor;
      Self.FLineStyle := FLineStyle;
      Self.FLineMode  := FLineMode;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeMoveSizeProps.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TSCDeMoveSizeProps.Create;
begin
  inherited Create;
  FLineColor := clFuchsia;
  FLineStyle := psDot;
  FLineMode  := pmCopy;
end;

procedure TSCDeMoveSizeProps.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TSCDeMoveSizeProps.SetLineMode(Value: TPenMode);
begin
  if FLineMode <> Value then
  begin
    FLineMode := Value;
    Changed;
  end;
end;

procedure TSCDeMoveSizeProps.SetLineStyle(Value: TPenStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

{ TSCDeSelectionRectProperties }

procedure TSCDeSelectionRectProperties.Assign(Source: TPersistent);
begin
  if Source is TSCDeSelectionRectProperties then
  begin
    with TSCDeSelectionRectProperties(Source) do
    begin
      Self.FLineColor := FLineColor;
      Self.FLineStyle := FLineStyle;
      Self.FLineMode  := FLineMode;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeSelectionRectProperties.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TSCDeSelectionRectProperties.Create(AOwner: TSCDeCustomEditor);
begin
  inherited Create;
  FOwner := AOwner;

  FLineColor := clGray;
  FLineStyle := psDot;
  FLineMode := pmNotXor;
end;

function TSCDeSelectionRectProperties.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeSelectionRectProperties.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionRectProperties.SetLineMode(Value: TPenMode);
begin
  if FLineMode <> Value then
  begin
    FLineMode := Value;
    Changed;
  end;
end;

procedure TSCDeSelectionRectProperties.SetLineStyle(Value: TPenStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

{ TSCDeGuide }

procedure TSCDeGuide.Assign(Source: TPersistent);
begin
  if Source is TSCDeGuide then
  begin
    with TSCDeGuide(Source) do
    begin
      Self.FPosition := Position;
      Self.FGuideType := GuideType;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeGuide.Changed;
begin
  if not FDestroying and (FOwner <> nil) then
    FOwner.Changed(Self);
end;

constructor TSCDeGuide.Create(AOwner: TSCDeGuides);
begin
  inherited Create;
  FPosition := 0.0;
  FGuideType := scgtHorizontal;
  if AOwner <> nil then AOwner.Insert(Self);
end;

destructor TSCDeGuide.Destroy;
begin
  Destroying;
  if FOwner <> nil then FOwner.Remove(Self);
  inherited Destroy;
end;

procedure TSCDeGuide.Destroying;
begin
  FDestroying := True;
end;

function TSCDeGuide.GetIndex: Integer;
begin
  Result := -1;
  if FOwner <> nil then Result := FOwner.IndexOf(Self);
end;

function TSCDeGuide.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeGuide.SetGuideType(Value: TSCDeGuideType);
begin
  if FGuideType <> Value then
  begin
    FGuideType := Value;
    Changed;
  end;
end;

procedure TSCDeGuide.SetPosition(Value: Double);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

{ TSCDeGuides }

procedure TSCDeGuides.Add(G: TSCDeGuide);
begin
  Insert(G);
end;

function TSCDeGuides.Add: TSCDeGuide;
begin
  Result := TSCDeGuide.Create(Self);
end;

procedure TSCDeGuides.Assign(Source: TPersistent);
var
  I: Integer;
  G: TSCDeGuide;
begin
  if Source is TSCDeGuides then
  begin
    with TSCDeGuides(Source) do
    begin
      if Self.Count <> Count then
      begin
        Self.BeginUpdate;
        try
          Self.Clear;

          for I := 0 to Count-1 do
          begin
            G := TSCDeGuide.Create(nil);
            G.Assign(Items[I]);

            Self.Insert(G);
          end;
        finally
          Self.EndUpdate;
        end;  
      end;
    end;
  end else
    inherited Assign(Source);
end;

procedure TSCDeGuides.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeGuides.Changed(G: TSCDeGuide);
begin
  if not FDestroying then
  begin
    if FUpdateCount > 0 then
    begin
      Inc(FChangeCount);
      Exit;
    end;

    if FChangeCount > 0 then G := nil;

    FChangeCount := 0;
    if FOwner <> nil then FOwner.GuideChanged(G);
  end;  
end;

procedure TSCDeGuides.Clear;
begin
  BeginUpdate;
  try
    while FItems.Count > 0 do
      TObject(FItems[FItems.Count-1]).Free;
  finally
    EndUpdate;
  end;
end;

constructor TSCDeGuides.Create(AOwner: TSCDeCustomEditor);
begin
  inherited Create;
  FItems := TList.Create;
  FOwner := AOwner;
end;

procedure TSCDeGuides.Delete(Index: Integer);
begin
  TObject(FItems[Index]).Free;
end;

destructor TSCDeGuides.Destroy;
begin
  Destroying;
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TSCDeGuides.Destroying;
begin
  FDestroying := True;
end;

procedure TSCDeGuides.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FChangeCount > 0) then
      Changed(nil);
  end;
end;

function TSCDeGuides.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSCDeGuides.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TSCDeGuides.GetItem(Index: Integer): TSCDeGuide;
begin
  Result := TSCDeGuide(FItems[Index]);
end;

function TSCDeGuides.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSCDeGuides.IndexOf(G: TSCDeGuide): Integer;
begin
  Result := FItems.IndexOf(G);
end;

procedure TSCDeGuides.Insert(G: TSCDeGuide);
begin
  if (G <> nil) and (G.Owner <> Self) then
  begin
    if G.Owner <> nil then G.Owner.Remove(G);
    G.FOwner := Self;
    FItems.Add(G);

    Changed(G);
  end;
end;

procedure TSCDeGuides.Remove(G: TSCDeGuide);
begin
  if (G <> nil) and (G.Owner = Self) then
  begin
    G.FOwner := nil;
    FItems.Remove(G);

    Changed(G);
  end;
end;

{ TSCDeGuideProperties }

procedure TSCDeGuideProperties.Assign(Source: TPersistent);
begin
  if Source is TSCDeGuideProperties then
  begin
    with TSCDeGuideProperties(Source) do
    begin
      Self.FColor := Color;
      Self.FStyle := Style;
      Self.FMode := Mode;
    end;
    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeGuideProperties.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TSCDeGuideProperties.Create(AOwner: TSCDeCustomEditor);
begin
  inherited Create;
  FOwner := AOwner;
  
  FColor := clBlue;
  FStyle := psSolid;
  FMode := pmCopy;
end;

function TSCDeGuideProperties.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeGuideProperties.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeGuideProperties.SetMode(Value: TPenMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

procedure TSCDeGuideProperties.SetStyle(Value: TPenStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TSCDeRuler }

constructor TSCDeRuler.Create(AOwner: TSCDeRulers);
begin
  inherited Create;
  FOwner := AOwner;
  FOffset := 0;
  FPosition := -1;
  FRulerType := scdrHorizontal;
end;

procedure TSCDeRuler.DrawHorzRuler(C: TCanvas; R: TRect; Zoom: Double);
var
  Step: Double;
  NCount, PCount,
  I, X, Y, H, W: Integer;
begin
  Step := GetStep(Zoom);
  if Step <= 0.0 then
    Exit;

  OffsetRect(R, 1, 0);

  PCount := TSCDeUtils.Round((R.Right - R.Left - FOffset) / Step) + 1;
  NCount := TSCDeUtils.Round(FOffset / Step) + 1;

  if FOwner.ShowNumbers and (FOwner.Font.Color <> clNone) then
  begin
    with C do
    begin
      Brush.Style := bsClear;
      Font.Assign(FOwner.Font);
    end;

    H := C.TextHeight('0');
    W := C.TextWidth('-000');

    if (5*Step > W) and (5*Step > H) then
    begin
      Y := R.Bottom - (H + 5);

      for I := 0 to PCount do
      begin
        X := R.Left + FOffset + TSCDeUtils.Round(I*Step);
        if (X < R.Left) or (X > R.Right) then
          Continue;

        Inc(X, 2);

        if I = 0 then
          C.TextOut(X, Y - 4, '0')
        else
        if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
          C.TextOut(X, Y, IntToStr(I) + '0')
        else
        if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
          C.TextOut(X, Y, IntToStr(I div 2));
      end;

      for I := 1 to NCount do
      begin
        X := R.Left + FOffset - TSCDeUtils.Round(I*Step);
        if (X < R.Left) or (X > R.Right) then
          Continue;

        Inc(X, 2);

        if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
          C.TextOut(X, Y, IntToStr(-I) + '0')
        else
        if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
          C.TextOut(X, Y, IntToStr(-I div 2));
      end;
    end;
  end;

  if FOwner.MarkColor <> clNone then
  begin
    with C do
    begin
      Pen.Width := 1;
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := FOwner.MarkColor;
    end;

    for I := 0 to PCount do
    begin
      X := R.Left + FOffset + TSCDeUtils.Round(I*Step);
      if (X < R.Left) or (X > R.Right) then
        Continue;

      if I = 0 then
      begin
        C.MoveTo(X, R.Bottom - 16);
        C.LineTo(X, R.Bottom);
      end else
      if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
      begin
        C.MoveTo(X, R.Bottom - 10);
        C.LineTo(X, R.Bottom);
      end else
      if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
      begin
        C.MoveTo(X, R.Bottom - 10);
        C.LineTo(X, R.Bottom);
      end else
      begin
        C.MoveTo(X, R.Bottom - 5);
        C.LineTo(X, R.Bottom);
      end;
    end;
    
    for I := 1 to NCount do
    begin
      X := R.Left + FOffset - TSCDeUtils.Round(I*Step);
      if (X < R.Left) or (X > R.Right) then
        Continue;

      if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
      begin
        C.MoveTo(X, R.Bottom - 10);
        C.LineTo(X, R.Bottom);
      end else
      if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
      begin
        C.MoveTo(X, R.Bottom - 10);
        C.LineTo(X, R.Bottom);
      end else
      begin
        C.MoveTo(X, R.Bottom - 5);
        C.LineTo(X, R.Bottom);
      end;
    end;
  end;

  if FOwner.GuideColor <> clNone then
    with C do
    begin
      Pen.Width := 1;
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := FOwner.GuideColor;

      MoveTo(R.Left + FPosition, R.Top);
      LineTo(R.Left + FPosition, R.Bottom);
    end;
end;

procedure TSCDeRuler.DrawVertRuler(C: TCanvas; R: TRect; Zoom: Double);
var
  S: String;
  Step: Double;
  PCount, NCount,
  I, X, Y, H, W: Integer;
begin
  Step := GetStep(Zoom);
  if Step <= 0.0 then
    Exit;

  OffsetRect(R, 0, 1);

  PCount := TSCDeUtils.Round((R.Bottom - R.Top - FOffset) / Step) + 1;
  NCount := TSCDeUtils.Round(FOffset / Step) + 1;

  if FOwner.ShowNumbers and (FOwner.Font.Color <> clNone) then
  begin
    with C do
    begin
      Brush.Style := bsClear;
      Font.Assign(FOwner.Font);
    end;

    H := C.TextHeight('0');
    W := C.TextWidth('-000');

    if (5*Step > H) and (5*Step > W) then
    begin
      for I := 0 to PCount do
      begin
        Y := R.Top + FOffset + TSCDeUtils.Round(I*Step);
        if (Y < R.Top) or (Y > R.Bottom) then
          Continue;

        if I = 0 then
        begin
          W := C.TextWidth('0');
          X := R.Right - (W + 12);

          C.TextOut(X, Y, '0')
        end else
        if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
        begin
          S := IntToStr(I) + '0';

          W := C.TextWidth(S);
          X := R.Right - (W + 6);

          C.TextOut(X, Y, S)
        end else
        if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
        begin
          S := IntToStr(I div 2);

          W := C.TextWidth(S);
          X := R.Right - (W + 6);

          C.TextOut(X, Y, S)
        end;
      end;

      for I := 1 to NCount do
      begin
        Y := R.Top + FOffset - TSCDeUtils.Round(I*Step);
        if (Y < R.Top) or (Y > R.Bottom) then
          Continue;

        if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
        begin
          S := IntToStr(-I) + '0';

          W := C.TextWidth(S);
          X := R.Right - (W + 8);

          C.TextOut(X, Y, S)
        end else
        if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
        begin
          S := IntToStr(-I div 2);

          W := C.TextWidth(S);
          X := R.Right - (W + 8);

          C.TextOut(X, Y, S)
        end;
      end;
    end;
  end;

  if FOwner.MarkColor <> clNone then
  begin
    with C do
    begin
      Pen.Width := 1;
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := FOwner.MarkColor;
    end;

    for I := 0 to PCount do
    begin
      Y := R.Top + FOffset + TSCDeUtils.Round(I*Step);
      if (Y < R.Top) or (Y > R.Bottom) then
        Continue;

      if I = 0 then
      begin
        C.MoveTo(R.Right - 16, Y);
        C.LineTo(R.Right, Y);
      end else
      if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
      begin
        C.MoveTo(R.Right - 10, Y);
        C.LineTo(R.Right, Y);
      end else
      if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
      begin
        C.MoveTo(R.Right - 10, Y);
        C.LineTo(R.Right, Y);
      end else
      begin
        C.MoveTo(R.Right - 5, Y);
        C.LineTo(R.Right, Y);
      end;
    end;

    for I := 1 to NCount do
    begin
      Y := R.Top + FOffset - TSCDeUtils.Round(I*Step);
      if (Y < R.Top) or (Y > R.Bottom) then
        Continue;

      if (FOwner.Mode = scrmPixel) and (I mod 5 = 0) then
      begin
        C.MoveTo(R.Right - 10, Y);
        C.LineTo(R.Right, Y);
      end else
      if (FOwner.Mode in [scrmCm, scrmInch]) and (I mod 2 = 0) then
      begin
        C.MoveTo(R.Right - 10, Y);
        C.LineTo(R.Right, Y);
      end else
      begin
        C.MoveTo(R.Right - 5, Y);
        C.LineTo(R.Right, Y);
      end;
    end;
  end;

  if FOwner.GuideColor <> clNone then
    with C do
    begin
      Pen.Width := 1;
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := FOwner.GuideColor;

      MoveTo(R.Left, R.Top + FPosition);
      LineTo(R.Right, R.Top + FPosition);
    end;
end;

function TSCDeRuler.GetStep(Zoom: Double): Double;
begin
  Result := 10.0;
  if (FOwner <> nil) and (FOwner.Mode in [scrmCm, scrmInch]) then
  begin
    Result := Screen.PixelsPerInch;
    if FOwner.Mode = scrmCm then
      Result := Result / 2.54;

    Result := Result / 2;
  end;

  Result := Result*Zoom;
end;

procedure TSCDeRuler.Paint(C: TCanvas; R: TRect; Zoom: Double);
begin
  if (FOwner <> nil) and not IsRectEmpty(R) then
  begin
    if FRulerType = scdrHorizontal then
      DrawHorzRuler(C, R, Zoom)
    else
      DrawVertRuler(C, R, Zoom);
  end;
end;

procedure TSCDeRuler.SetOffset(Value: Integer);
begin
  FOffset := Value;
end;

procedure TSCDeRuler.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TSCDeRuler.SetRulerType(Value: TSCDeRulerType);
begin
  FRulerType := Value;
end;

{ TSCDeRulers }

procedure TSCDeRulers.Changed(Force: Boolean);
var
  WasForced: Boolean;
begin
  FForced := FForced or Force;

  if FUpdateCount > 0 then
  begin
    Inc(FChangeCount);
    Exit;
  end;

  WasForced := FForced;
  FForced := False;

  FChangeCount := 0;

  if (FOwner <> nil) and (WasForced or FVisible) then
    FOwner.RulersChanged(Self, WasForced);
end;

constructor TSCDeRulers.Create(AOwner: TSCDeCustomEditor);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBtnFace;
  FGuideColor := clBlue;
  FMarkColor := clWindowText;
  FMode := scrmPixel;
  FShowNumbers := True;
  FSize := 25;
  FVisible := True;

  FFont := TFont.Create;
  FFont.OnChange := FontChanged;

  FHorzRuler := TSCDeRuler.Create(Self);
  FHorzRuler.SetRulerType(scdrHorizontal);

  FVertRuler := TSCDeRuler.Create(Self);
  FVertRuler.SetRulerType(scdrVertical);
end;

destructor TSCDeRulers.Destroy;
begin
  FFont.OnChange := nil;
  FreeAndNil(FFont);
  FreeAndNil(FHorzRuler);
  FreeAndNil(FVertRuler);
  inherited Destroy;
end;

procedure TSCDeRulers.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TSCDeRulers.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSCDeRulers.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSCDeRulers.SetSize(Value: Word);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed(FVisible);
  end;
end;

procedure TSCDeRulers.SetMarkColor(Value: TColor);
begin
  if FMarkColor <> Value then
  begin
    FMarkColor := Value;
    Changed;
  end;
end;

procedure TSCDeRulers.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(FSize > 0);
  end;
end;

procedure TSCDeRulers.Assign(Source: TPersistent);
begin
  if Source is TSCDeRulers then
  begin
    with TSCDeRulers(Source) do
    begin
      Self.Color := Color;
      Self.Font := Font;
      Self.Size := Size;
      Self.GuideColor := GuideColor;
      Self.MarkColor := MarkColor;
      Self.Visible := Visible;
    end;
  end else
    inherited Assign(Source);
end;

function TSCDeRulers.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TSCDeRulers.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSCDeRulers.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FChangeCount > 0) then
      Changed;
  end;
end;

function TSCDeRulers.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeRulers.SetGuideColor(Value: TColor);
begin
  if FGuideColor <> Value then
  begin
    FGuideColor := Value;
    Changed;
  end;
end;

procedure TSCDeRulers.SetMode(Value: TSCDeRulerMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

procedure TSCDeRulers.SetShowNumbers(Value: Boolean);
begin
  if FShowNumbers <> Value then
  begin
    FShowNumbers := Value;
    Changed;
  end;
end;

{ TSCDeCustomEditor }

procedure TSCDeCustomEditor.AddSelection(S: TSCDeShapeBase);
begin
  if CanSelect(S) and (FSelectionList.IndexOf(S) = -1) then
  begin
    if scdoMultiSelect in FEditOptions then
      FSelectionList.Add(S)
    else begin
      FSelectionList.Clear;
      FSelectionList.Add(S);
    end;

    SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.AddSelections(A: array of TSCDeShapeBase);
var
  I: Integer;
  S: TSCDeShapeBase;
  ListChanged: Boolean;
begin
  if (scdoCanSelect in FEditOptions) and (Length(A) > 0) then
  begin
    ListChanged := False;

    for I := Low(A) to High(A) do
    begin
      S := A[I];
      if CanSelect(S) and (FSelectionList.IndexOf(S) = -1) then
      begin
        FSelectionList.Add(S);
        ListChanged := True;
      end;
    end;

    if not (scdoMultiSelect in FEditOptions) then
    begin
      S := TSCDeShapeBase(FSelectionList[FSelectionList.Count - 1]);

      FSelectionList.Clear;
      FSelectionList.Add(S);
    end;

    if ListChanged then SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.AfterUpdateBuffers(C: TCanvas);
begin
  UpdateHottrack;
  inherited AfterUpdateBuffers(C);
  UpdateCursorPos;
  UpdateCursor(0, 0, False);
end;

procedure TSCDeCustomEditor.ClearSelection;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        S.ClearSelection;
      end;

      FSelectionList.Clear;
    finally
      EndUpdate;
    end;

    SelectionChanged;
  end;
end;

constructor TSCDeCustomEditor.Create(AOwner: TComponent);
begin
  FSelectionList := TList.Create;
  FNextClipViewer := 0;
  FUndoLimit := -1;

  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks,
    csOpaque, csReplicatable];

  ClickFocus := True;
  BufferedPaint := True;

  InitiateUndoBuffer;
  InitiateRedoBuffer;

  FMouseSense := 1;

  FNewShapeNotifier := TSCDeShapeNotifier.Create;
  FNewShapeNotifier.OnDestroy := NewShapeDestroyed;

  FHitFuzz := 2;
  FNewShapeClass := nil;
  FKeyMoveSizeStep := 1;

  FRulers := TSCDeRulers.Create(Self);

  FDefaultBrush := TSCDeShapeBrush.Create;
  TSCDeFakeBrush(FDefaultBrush).OnChange := DefaultBrushChanged;

  FDefaultPen := TSCDeShapePen.Create;
  TSCDeFakePen(FDefaultPen).OnChange := DefaultPenChanged;

  FDefaultFont := TSCDeShapeFont.Create;
  TSCDeFakeFont(FDefaultFont).OnChange := DefaultFontChanged;

  FEditOptions := DeDefaultEditOptions;
  FEditState := scesNone;

  FControlKeys := [scskAlt];
  FMultiSelectKeys := [scskCtrl];
  FSelectionRectKeys := [scskShift];

  FGuides := TSCDeGuides.Create(Self);

  FCursorGuideProperties := TSCDeGuideProperties.Create(Self);
  FCursorGuideProperties.OnChange := CursorGuidePropertiesChanged;

  FGuideProperties := TSCDeGuideProperties.Create(Self);
  FGuideProperties.OnChange := GuidePropertiesChanged;

  FHottrackProps := TSCDeHottrackProps.Create;
  FHottrackProps.OnChange := HottrackPropsChanged;

  FSelectionProps := TSCDeSelectionProps.Create;
  FSelectionProps.OnChange := SelectionPropsChanged;

  FSelectionRectProperties := TSCDeSelectionRectProperties.Create(Self);
  FSelectionRectProperties.OnChange := SelectionRectPropertiesChanged;

  FMoveSizeProps := TSCDeMoveSizeProps.Create;
end;

procedure TSCDeCustomEditor.DeleteSelection;
var
  L: TList;
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      L := TList.Create;
      try
        for I := 0 to FSelectionList.Count-1 do
        begin
          S := TSCDeShapeBase(FSelectionList[I]);
          if not S.InDestroy and S.Visible then
            L.Add(S);
        end;

        FSelectionList.Clear;

        if L.Count > 0 then
        begin
          BeginUndoGroup;
          try
            for I := 0 to L.Count-1 do
            begin
              S := TSCDeShapeBase(L[I]);
              if Exists(S) then S.Free;
            end;
          finally
            EndUndoGroup;
          end;
        end;
      finally
        L.Free;
      end;
    finally
      EndUpdate;
    end;

    SelectionChanged;
  end;
end;

destructor TSCDeCustomEditor.Destroy;
begin
  FreeAndNil(FUndoBuffer);
  FreeAndNil(FRedoBuffer);

  FNewShapeNotifier.OnDestroy := nil;
  FreeAndNil(FNewShapeNotifier);

  SetCurrentTool(nil);

  FreeAndNil(FDefaultBrush);
  FreeAndNil(FDefaultPen);
  FreeAndNil(FDefaultFont);

  FreeAndNil(FPaintBuffer);
  FreeAndNil(FSelectionList);

  FreeAndNil(FMoveSizeProps);

  FHottrackProps.OnChange := nil;
  FreeAndNil(FHottrackProps);

  FSelectionProps.OnChange := nil;
  FreeAndNil(FSelectionProps);

  FSelectionRectProperties.OnChange := nil;
  FreeAndNil(FSelectionRectProperties);

  FCursorGuideProperties.OnChange := nil;
  FreeAndNil(FCursorGuideProperties);

  FGuideProperties.OnChange := nil;
  FreeAndNil(FGuideProperties);

  FreeAndNil(FGuides);
  FreeAndNil(FRulers);
  inherited Destroy;
end;

procedure TSCDeCustomEditor.DoGuideChanged(G: TSCDeGuide);
begin
  //
end;

procedure TSCDeCustomEditor.DoLayerRemoved(L: TSCDeLayer);
var
  I, Cnt: Integer;
begin
  if not Exists(FHotShape) then FHotShape := nil;

  if not (csDestroying in ComponentState) and (FSelectionList <> nil) then
  begin
    UpdateSelection;
    Cnt := FSelectionList.Count;
    
    for I := 0 to L.ItemCount-1 do
      FSelectionList.Remove(L.Items[I]);

    if Cnt <> FSelectionList.Count then
      SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.DoShapeRemoved(S: TSCDeShapeBase);
var
  Before, After: Integer;
begin
  if FHotShape = S then FHotShape := nil;

  if not (csDestroying in ComponentState) then
  begin
    if FEditState in [scesMoving, scesSizing, scesRotating,
      scesAddPoint, scesRemovePoint] then
      SetEditState(scesNone);

    if FSelectionList <> nil then
    begin
      Before := FSelectionList.IndexOf(S);
      UpdateSelection;

      After := FSelectionList.IndexOf(S);
      if After > -1 then FSelectionList.Delete(After);

      if Before > -1 then SelectionChanged;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawGuides(C: TCanvas);
var
  Z: Double;
  Cl: TColor;
  CR, IR: TRect;
  G: TSCDeGuide;
  I, P: Integer;
  R: TDoubleRect;
begin
  if (scdoShowGuides in FEditOptions) and (FGuides <> nil) and
    (FGuides.Count > 0) and (FGuideProperties <> nil) and
    (FGuideProperties.Color <> clNone) and (FGuideProperties.Style <> psClear) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if not IsRectEmpty(CR) and IntersectRect(IR, IR, CR) then
    begin
      IR := TSCDeUtils.Rect(R);
      
      Z := GetZoom;

      Cl := Layer.Color;
      if Cl = clNone then Cl := clWhite;

      with C do
      begin
        Brush.Color := Cl;
        Brush.Style := bsClear;

        Pen.Color := FGuideProperties.Color;
        Pen.Style := FGuideProperties.Style;
        Pen.Mode  := FGuideProperties.Mode;
        Pen.Width := 1;
      end;

      for I := 0 to FGuides.Count-1 do
      begin
        G := FGuides[I];

        P := TSCDeUtils.Round(Z*G.Position);

        if G.GuideType = scgtHorizontal then
        begin
          Inc(P, IR.Top);

          if (P >= IR.Top) and (P < IR.Bottom) then
            with C do
            begin
              MoveTo(IR.Left, P);
              LineTo(IR.Right, P);
            end;
        end else
        begin
          Inc(P, IR.Left);

          if (P >= IR.Left) and (P < IR.Right) then
            with C do
            begin
              MoveTo(P, IR.Top);
              LineTo(P, IR.Bottom);
            end;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawHottrack(C: TCanvas);
var
  I: Integer;
  CR, IR: TRect;
  R: TDoubleRect;
  Z, X, Y: Double;
  Dp: TSCDeDragPoint;
  SelData: TSCDeSelectionData;
begin
  if not (InUpdate or (csDesigning in ComponentState)) and
    (FHotShape <> nil) and not (FHotShape.InDestroy or FHotShape.Locked) and
    FHotShape.Visible and (scssCanHottrack in FHotShape.ShapeStyle) and
    (FSelectionProps <> nil) and (FSelectionList <> nil) and
    (FSelectionList.IndexOf(FHotShape) = -1) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if not (IsRectEmpty(CR) or IsRectEmpty(IR)) then
    begin
      Z := GetZoom;

      X := R.Left;
      Y := R.Top;

      if (FRulers <> nil) and FRulers.Visible then
      begin
        I := FRulers.Size;
        TSCDeUtils.OffsetRect(R, -I, -I);

        Inc(CR.Left, I);
        Inc(CR.Top, I);
      end;

      TSCDeUtils.ZoomRect(R, 1/Z);

      R.Right := R.Left + (CR.Right - CR.Left);
      R.Bottom := R.Top + (CR.Bottom - CR.Top);

      TSCDeUtils.OffsetRect(R, -2*R.Left, -2*R.Top);

      if not TSCDeUtils.IsRectEmpty(R) and FHotShape.InRect(R) then
      begin
        with FHottrackProps do
        begin
          SelData.LineColor     := LineColor;
          SelData.LineStyle     := LineStyle;
          SelData.LineMode      := LineMode;
          SelData.CtrlInColor   := CtrlInColor;
          SelData.CtrlOutColor  := CtrlOutColor;
          SelData.PointInColor  := PointInColor;
          SelData.PointOutColor := PointOutColor;
          SelData.PointSize     := PointSize;
          SelData.SelectionType := scdsHottrack;
        end;

        Dp.PartIndex := -1;
        Dp.SubIndex := -1;
        Dp.Offset := TSCDeUtils.Point(0, 0);
        Dp.PointType := scptNone;

        FHotShape.DrawSelection(C, X, Y, Dp, SelData, Z);
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawSelection(C: TCanvas);
var
  L: TList;
  I: Integer;
  CR, IR: TRect;
  R: TDoubleRect;
  Z, X, Y: Double;
  S: TSCDeShapeBase;
  Dp: TSCDeDragPoint;
  SelData: TSCDeSelectionData;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if not (IsRectEmpty(CR) or IsRectEmpty(IR)) then
    begin
      Z := GetZoom;

      X := R.Left;
      Y := R.Top;

      if (FRulers <> nil) and FRulers.Visible then
      begin
        I := FRulers.Size;
        TSCDeUtils.OffsetRect(R, -I, -I);

        Inc(CR.Left, I);
        Inc(CR.Top, I);
      end;

      TSCDeUtils.ZoomRect(R, 1/Z);

      R.Right := R.Left + (CR.Right - CR.Left);
      R.Bottom := R.Top + (CR.Bottom - CR.Top);

      TSCDeUtils.OffsetRect(R, -2*R.Left, -2*R.Top);

      if not TSCDeUtils.IsRectEmpty(R) then
      begin
        L := TList.Create;
        try
          for I := 0 to FSelectionList.Count-1 do
          begin
            S := TSCDeShapeBase(FSelectionList[I]);
            if not S.InDestroy and S.Visible and S.InRect(R) then
              L.Add(S);
          end;

          if L.Count > 0 then
          begin
            with FSelectionProps do
            begin
              SelData.LineColor     := LineColor;
              SelData.LineStyle     := LineStyle;
              SelData.LineMode      := LineMode;
              SelData.CtrlInColor   := CtrlInColor;
              SelData.CtrlOutColor  := CtrlOutColor;
              SelData.PointInColor  := PointInColor;
              SelData.PointOutColor := PointOutColor;
              SelData.PointSize     := PointSize;
              SelData.SelectionType := scdsSelected;
            end;

            Dp.PartIndex := -1;
            Dp.SubIndex := -1;
            Dp.Offset := TSCDeUtils.Point(0, 0);
            Dp.PointType := scptNone;

            for I := 0 to L.Count-1 do
              TSCDeShapeBase(L[I]).DrawSelection(C, X, Y, Dp,
                SelData, Z);
          end;
        finally
          L.Free;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawSelectionMove(C: TCanvas);
var
  L: TList;
  I: Integer;
  Gp: TPoint;
  CR, IR: TRect;
  Z, X, Y: Double;
  S: TSCDeShapeBase;
  R, BR: TDoubleRect;
  Dp: TSCDeDragPoint;
  P, Offset: TDoublePoint;
  SelData: TSCDeSelectionData;
begin
  if (FEditState = scesMoving) and (FSelectionList <> nil) and
    (FSelectionList.Count > 0) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if (FSelectionList.Count > 0) and not (IsRectEmpty(CR) or IsRectEmpty(IR)) and
      ((FMoveRec.Test.x <> FHitRec.Test.x) or (FMoveRec.Test.y <> FHitRec.Test.y)) then
    begin
      Z := GetZoom;

      X := R.Left;
      Y := R.Top;

      if (FRulers <> nil) and FRulers.Visible then
      begin
        I := FRulers.Size;
        TSCDeUtils.OffsetRect(R, -I, -I);

        Inc(CR.Left, I);
        Inc(CR.Top, I);
      end;

      P.x := FMoveRec.Test.x - FHitRec.Test.x;
      P.y := FMoveRec.Test.y - FHitRec.Test.y;

      if FMouseSense > 1 then
      begin
        P.x := Math.Floor(P.x);
        P.y := Math.Floor(P.y);

        P.x := P.x - (Math.Floor(P.x) mod FMouseSense);
        P.y := P.y - (Math.Floor(P.y) mod FMouseSense);
      end;

      if (scdoSnapToGrid in EditOptions) and ((P.x <> 0) or (P.y <> 0)) then
      begin
        Offset := TSCDeUtils.Point(0, 0);

        BR := Self.GetSelectionBounds;
        TSCDeUtils.OffsetRect(BR, P.x, P.y);

        Gp.x := Self.Grid.X;
        Gp.y := Self.Grid.Y;

        if Gp.x > 1 then
        begin
          Offset.x := BR.Left - TSCDeUtils.Round(BR.Left / Gp.x)*Gp.x;

          if Offset.x >= Gp.x div 2 then
            Offset.x := Gp.x - Offset.x
          else
            Offset.x := -Offset.x;
        end;

        if Gp.y > 1 then
        begin
          Offset.y := BR.Top - TSCDeUtils.Round(BR.Top / Gp.y)*Gp.y;

          if Offset.y >= Gp.y div 2 then
            Offset.y := Gp.y - Offset.y
          else
            Offset.y := -Offset.y;
        end;

        P.x := P.x + Offset.x;
        P.y := P.y + Offset.y;
      end;

      TSCDeUtils.ZoomRect(R, 1/Z);

      R.Right := R.Left + (CR.Right - CR.Left);
      R.Bottom := R.Top + (CR.Bottom - CR.Top);

      TSCDeUtils.OffsetRect(R, -2*R.Left, -2*R.Top);

      if not TSCDeUtils.IsRectEmpty(R) then
      begin
        L := TList.Create;
        try
          for I := 0 to FSelectionList.Count-1 do
          begin
            S := TSCDeShapeBase(FSelectionList[I]);
            if not (S.InDestroy or S.Locked) and
              S.Visible and S.InRect(R) then
              L.Add(S);
          end;

          if L.Count > 0 then
          begin
            X := X + Z*P.x;
            Y := Y + Z*P.y;

            with FMoveSizeProps do
            begin
              SelData.LineColor     := LineColor;
              SelData.LineStyle     := LineStyle;
              SelData.LineMode      := LineMode;
              SelData.CtrlInColor   := clNone;
              SelData.CtrlOutColor  := clNone;
              SelData.PointInColor  := clNone;
              SelData.PointOutColor := clNone;
              SelData.PointSize     := 0;
              SelData.SelectionType := scdsMoving;
            end;

            Dp.PartIndex := -1;
            Dp.SubIndex := -1;
            Dp.Offset := TSCDeUtils.Point(0, 0);
            Dp.PointType := scptNone;

            for I := 0 to L.Count-1 do
            begin
              if I = L.Count-1 then
              begin
                if SelData.PointOutColor = clNone then
                  SelData.PointOutColor := SelData.PointInColor
                else
                  SelData.PointInColor := SelData.PointOutColor;
              end;

              TSCDeShapeBase(L[I]).DrawSelection(C, X, Y, Dp, SelData, Z);
            end;
          end;
        finally
          L.Free;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawSelectionRect(C: TCanvas);
var
  CR, IR: TRect;
  R: TDoubleRect;
  P: TDoublePoint;
  Z, X, Y: Double;
begin
  if ((FEditState = scesRectSelect) or (MouseIsDown and (FEditState = scesZoomRect))) and
    (FSelectionRectProperties <> nil) and (FSelectionRectProperties.FLineColor <> clNone) and
    (FSelectionRectProperties.FLineStyle <> psClear) then
  begin
    LayerBounds(CR, R);

    Z := GetZoom;

    if not IsRectEmpty(CR) and ((FHitRec.Test.x <> FMoveRec.Test.x) or
      (FHitRec.Test.y <> FMoveRec.Test.y)) then
    begin
      X := R.Left;
      Y := R.Top;

      P := FHitRec.Test;
      P.x := Z*(P.x - X) + X;
      P.y := Z*(P.y - Y) + Y;

      R.TopLeft := P;

      P := FMoveRec.Test;
      P.x := Z*(P.x - X) + X;
      P.y := Z*(P.y - Y) + Y;

      R.BottomRight := P;

      IR := TSCDeUtils.Rect(R);

      if IR.Left > IR.Right then
        TSCDeUtils.Swap(IR.Left, IR.Right);

      if IR.Top > IR.Bottom then
        TSCDeUtils.Swap(IR.Top, IR.Bottom);

      with C, FSelectionRectProperties do
      begin
        Brush.Style := bsClear;

        Pen.Style := LineStyle;
        Pen.Mode  := LineMode;
        Pen.Color := LineColor;
        Pen.Width := 1;
      end;

      if (FSelectionRectProperties.LineColor <> clNone) and
        (FSelectionRectProperties.LineStyle <> psClear) then
      begin
        if (IR.Left < IR.Right) and (IR.Top = IR.Bottom) then
        begin
          C.MoveTo(IR.Left, IR.Top);
          C.LineTo(IR.Right, IR.Top);
        end else
        if (IR.Left = IR.Right) and (IR.Top < IR.Bottom) then
        begin
          C.MoveTo(IR.Left, IR.Top);
          C.LineTo(IR.Left, IR.Bottom);
        end else
        if (IR.Left < IR.Right) and (IR.Top < IR.Bottom) then
          C.Rectangle(IR.Left, IR.Top, IR.Right, IR.Bottom);
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawSelectionSize(C: TCanvas);
var
  Z: Double;
  CR: TRect;
  Gp: TPoint;
  R: TDoubleRect;
  Dp: TSCDeDragPoint;
  PointType: TSCDePointType;
  P, Pt, Offset: TDoublePoint;
  SelData: TSCDeSelectionData;
begin
  if (FEditState = scesSizing) and (FHitRec.HitType in [schtPoint,
    schtControl, schtPointControl]) and (FHitRec.State = schsSelected) and
    (FHitRec.PartIndex > -1) and IsSelected(FHitRec.Shape) and
    not FHitRec.Shape.Locked and ((FMoveRec.Test.x <> FHitRec.Test.x) or
    (FMoveRec.Test.y <> FHitRec.Test.y)) then
  begin
    LayerBounds(CR, R);

    if not IsRectEmpty(CR) then
    begin
      Z := GetZoom;

      P.x := FMoveRec.Test.x - FHitRec.Test.x;
      P.y := FMoveRec.Test.y - FHitRec.Test.y;

      if FMouseSense > 1 then
      begin
        P.x := Math.Floor(P.x);
        P.y := Math.Floor(P.y);

        P.x := P.x - (Math.Floor(P.x) mod FMouseSense);
        P.y := P.y - (Math.Floor(P.y) mod FMouseSense);
      end;

      if (FHitRec.HitType = schtPoint) and (FHitRec.SubIndex = -1) and
        (scdoSnapToGrid in EditOptions) and ((P.x <> 0) or (P.y <> 0)) then
      begin
        Pt := TSCDeFakeShape(FHitRec.Shape).GetPointValue(FHitRec.PartIndex);

        Offset := TSCDeUtils.Point(0, 0);

        Pt.x := Pt.x + P.x;
        Pt.y := Pt.y + P.y;

        Gp.x := Self.Grid.X;
        Gp.y := Self.Grid.Y;

        if Gp.x > 1 then
        begin
          Offset.x := Pt.x - TSCDeUtils.Round(Pt.x / Gp.x)*Gp.x;

          if Offset.x >= Gp.x div 2 then
            Offset.x := Gp.x - Offset.x
          else
            Offset.x := -Offset.x;
        end;

        if Gp.y > 1 then
        begin
          Offset.y := Pt.y - TSCDeUtils.Round(Pt.y / Gp.y)*Gp.y;

          if Offset.y >= Gp.y div 2 then
            Offset.y := Gp.y - Offset.y
          else
            Offset.y := -Offset.y;
        end;

        P.x := P.x + Offset.x;
        P.y := P.y + Offset.y;
      end;

      with FMoveSizeProps do
      begin
        SelData.LineColor     := LineColor;
        SelData.LineStyle     := LineStyle;
        SelData.LineMode      := LineMode;
        SelData.CtrlInColor   := clNone;
        SelData.CtrlOutColor  := clNone;
        SelData.PointInColor  := clNone;
        SelData.PointOutColor := clNone;
        SelData.PointSize     := 0;
        SelData.SelectionType := scdsMoving;
      end;

      PointType := scptPoint;
      if FHitRec.HitType = schtControl then
        PointType := scptControl
      else if FHitRec.HitType = schtPointControl then
        PointType := scptPointControl;

      Dp.PartIndex := FHitRec.PartIndex;
      Dp.SubIndex := FHitRec.SubIndex;
      Dp.Offset := P;
      Dp.PointType := PointType;

      FHitRec.Shape.DrawSelection(C, R.Left, R.Top, Dp, SelData, Z);
    end;
  end;
end;

procedure TSCDeCustomEditor.EndNewShape;
var
  I: Integer;
  L: TSCDeLayer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
  P1, P2: TSCDePoint;
  C: TSCDeSurfaceClient;
begin
  ResetHittest(FActionStartRec);

  if (FEditState = scesCreateNew) and (FNewShape <> nil) then
  begin
    S := FNewShape;
    
    FNewShapeNotifier.UnregisterShape(FNewShape);
    FNewShape := nil;

    if S.GetShapeType = sctyRectangle then
    begin
      R := S.GetBounds;
      if (R.Right - R.Left = 0) and (R.Bottom - R.Top = 0) then
        FreeAndNil(S);
    end else
    if S.GetShapeType = sctyPolyPoints then
    begin
      for I := S.PointCount-1 downto 1 do
      begin
        P1 := S.Points[I];
        P2 := S.Points[I-1];

        if (P1.x = P2.x) and (P1.y = P2.y) then
          P2.Free;
      end;

      if S.PointCount = 0 then
        FreeAndNil(S);
    end;

    if S <> nil then
    begin
      if scssUsesBrush in S.ShapeStyle then
        TSCDeFakeShape(S).Brush.Assign(FDefaultBrush);

      if scssUsesFont in S.ShapeStyle then
        TSCDeFakeShape(S).Font.Assign(FDefaultFont);

      if scssUsesPen in S.ShapeStyle then
        TSCDeFakeShape(S).Pen.Assign(FDefaultPen);

      L := GetActiveLayer;
      if L = nil then L := TSCDeLayer.Create(Self);

      TSCDeFakeShape(S).Refine(GetZoom);

      S.EndUpdate;

      FTempShape := S;
      FNewShapeNotifier.RegisterShape(FTempShape);

      for I := 0 to ClientCount-1 do
      begin
        C := Self.Clients[I];
        if C is TSCDeEditorClient then
          TSCDeEditorClient(C).DoEndNewShape(S);
      end;

      if Assigned(FOnEndNewShape) then
        FOnEndNewShape(Self, S);

      if FTempShape <> nil then
      begin
        FNewShapeNotifier.UnregisterShape(FTempShape);
        FTempShape := nil;

        BeginUndoGroup;
        try
          L.Add(S);
          Select(S);
        finally
          EndUndoGroup;
        end;
      end;
    end;
  end;

  Invalidate;
end;

procedure TSCDeCustomEditor.ExecuteHitAction(MouseAction: TSCDeEditState;
  OldState, NewState: TSCDeHitTest; Shift: TShiftState);
var
  Z: Double;
  CR: TRect;
  I: Integer;
  Gp: TPoint;
  G: TSCDeGuide;
  S: TSCDeShapeBase;
  R, BR: TDoubleRect;
  P, Pt, Offset: TDoublePoint;
begin
  Z := GetZoom;

  case MouseAction of
    scesSizing:
    begin
      S := OldState.Shape;

      if (OldState.PartIndex > -1) and (S <> nil) and
        Exists(S) and IsSelected(S) and not S.Locked and
        ((OldState.Test.x <> NewState.Test.x) or
        (OldState.Test.y <> NewState.Test.y)) then
      begin
        BeginUndoGroup;
        try
          if (OldState.SubIndex = -1) and (OldState.HitType = schtControl) and
            (OldState.State = schsSelected) then
          begin
            NotifyUndo(S, scacSizing, '');

            TSCDeFakeShape(S).BeginNotifyLock;
            try
              ApplyControlMove(S, OldState.PartIndex, OldState.Test,
                NewState.Test, [])
            finally
              TSCDeFakeShape(S).EndNotifyLock;
            end;
          end else
          if (OldState.SubIndex = -1) and (OldState.HitType = schtPoint) and
            (OldState.State = schsSelected) then
          begin
            NotifyUndo(S, scacSizing, '');

            if (scdoSnapToGrid in EditOptions) then
            begin
              P.x := NewState.Test.x - OldState.Test.x;
              P.y := NewState.Test.y - OldState.Test.y;

              if FMouseSense > 1 then
              begin
                P.x := Math.Floor(P.x);
                P.y := Math.Floor(P.y);

                P.x := P.x - (Math.Floor(P.x) mod FMouseSense);
                P.y := P.y - (Math.Floor(P.y) mod FMouseSense);
              end;

              if (P.x <> 0) or (P.y <> 0) then
              begin
                Pt := TSCDeFakeShape(OldState.Shape).GetPointValue(OldState.PartIndex);

                Offset := TSCDeUtils.Point(0, 0);

                Pt.x := Pt.x + P.x;
                Pt.y := Pt.y + P.y;

                Gp.x := Self.Grid.X;
                Gp.y := Self.Grid.Y;

                if Gp.x > 1 then
                begin
                  Offset.x := Pt.x - TSCDeUtils.Round(Pt.x / Gp.x)*Gp.x;

                  if Offset.x >= Gp.x div 2 then
                    Offset.x := Gp.x - Offset.x
                  else
                    Offset.x := -Offset.x;
                end;

                if Gp.y > 1 then
                begin
                  Offset.y := Pt.y - TSCDeUtils.Round(Pt.y / Gp.y)*Gp.y;

                  if Offset.y >= Gp.y div 2 then
                    Offset.y := Gp.y - Offset.y
                  else
                    Offset.y := -Offset.y;
                end;

                NewState.Test.x := NewState.Test.x + Offset.x;
                NewState.Test.y := NewState.Test.y + Offset.y;
              end;
            end;

            TSCDeFakeShape(S).BeginNotifyLock;
            try
              ApplySizing(S, OldState.PartIndex, -1, OldState.Test,
                NewState.Test, []);
            finally
              TSCDeFakeShape(S).EndNotifyLock;
            end;
          end else
          if (OldState.HitType = schtPointControl) and (OldState.State = schsSelected) and
            (OldState.PartIndex < S.PointCount) and (OldState.SubIndex > -1) then
          begin
            NotifyUndo(S, scacSizing, '');

            TSCDeFakeShape(S).BeginNotifyLock;
            try
              ApplySizing(S, OldState.PartIndex, OldState.SubIndex,
                OldState.Test, NewState.Test, []);
            finally
              TSCDeFakeShape(S).EndNotifyLock;
            end;
          end;
        finally
          EndUndoGroup;
        end;
      end;
    end;
    scesMoving:
    begin
      if (scdoMouseActions in FEditOptions) and (FSelectionList.Count > 0) then
      begin
        P.x := NewState.Test.x - OldState.Test.x;
        P.y := NewState.Test.y - OldState.Test.y;

        if FMouseSense > 1 then
        begin
          P.x := Math.Floor(P.x);
          P.y := Math.Floor(P.y);

          P.x := P.x - (Math.Floor(P.x) mod FMouseSense);
          P.y := P.y - (Math.Floor(P.y) mod FMouseSense);
        end;

        if (scdoSnapToGrid in EditOptions) and ((P.x <> 0) or (P.y <> 0)) then
        begin
          Offset := TSCDeUtils.Point(0, 0);

          BR := Self.GetSelectionBounds;
          TSCDeUtils.OffsetRect(BR, P.x, P.y);

          Gp.x := Self.Grid.X;
          Gp.y := Self.Grid.Y;

          if Gp.x > 1 then
          begin
            Offset.x := BR.Left - TSCDeUtils.Round(BR.Left / Gp.x)*Gp.x;

            if Offset.x >= Gp.x div 2 then
              Offset.x := Gp.x - Offset.x
            else
              Offset.x := -Offset.x;
          end;

          if Gp.y > 1 then
          begin
            Offset.y := BR.Top - TSCDeUtils.Round(BR.Top / Gp.y)*Gp.y;

            if Offset.y >= Gp.y div 2 then
              Offset.y := Gp.y - Offset.y
            else
              Offset.y := -Offset.y;
          end;

          P.x := P.x + Offset.x;
          P.y := P.y + Offset.y;
        end;

        BeginUndoGroup;
        try
          BeginUpdate;
          try
            for I := FSelectionList.Count-1 downto 0 do
            begin
              S := TSCDeShapeBase(FSelectionList[I]);

              if not (S.InDestroy or S.Locked) and S.Visible then
              begin
                NotifyUndo(S, scacSizing, '');

                TSCDeFakeShape(S).BeginNotifyLock;
                try
                  S.MoveBy(P.x, P.y);
                  if ((P.x <> 0) or (P.y <> 0)) and not S.Visible then
                    S.Refresh(True);
                finally
                  TSCDeFakeShape(S).EndNotifyLock;
                end;
              end;
            end;
          finally
            EndUpdate;
          end;
        finally
          EndUndoGroup;
        end;
      end;
    end;
    scesZoomRect:
    begin
      LayerBounds(CR, R);

      if not IsRectEmpty(CR) then
      begin
        R.TopLeft := OldState.Test;
        R.BottomRight := NewState.Test;

        if R.Left > R.Right then
          TSCDeUtils.Swap(R.Left, R.Right);

        if R.Top > R.Bottom then
          TSCDeUtils.Swap(R.Top, R.Bottom);

        Self.ZoomRect(R);
        Invalidate;
      end;
    end;
    scesRectSelect:
    begin
      if scdoCanSelect in FEditOptions then
      begin
        LayerBounds(CR, R);

        if not IsRectEmpty(CR) then
        begin
          R.TopLeft := OldState.Test;
          R.BottomRight := NewState.Test;

          if R.Left > R.Right then
            TSCDeUtils.Swap(R.Left, R.Right);

          if R.Top > R.Bottom then
            TSCDeUtils.Swap(R.Top, R.Bottom);

          if not TSCDeUtils.IsRectEmpty(R) and (Self.LayerCount > 0) then
            Self.SelectIn(R);
        end;
      end;
    end;
    scesNewGuide:
    begin
      if (FGuides <> nil) and (scdoShowGuides in FEditOptions) then
      begin
        P := NewState.Test;
        LayerBounds(CR, R);

        if OldState.HitType = schtRulerHorz then
        begin
          if (P.y >= 0) and (P.y <= R.Bottom - R.Top) then
          begin
            G := TSCDeGuide.Create(nil);

            G.Position := P.y;
            G.GuideType := scgtHorizontal;

            FGuides.Add(G);
          end;
        end else
        if (P.x >= 0) and (P.x <= R.Right - R.Left) then
        begin
          G := TSCDeGuide.Create(nil);

          G.Position := P.x;
          G.GuideType := scgtVertical;

          FGuides.Add(G);
        end;
      end;
    end;
    scesDragGuide:
    begin
      if (scdoShowGuides in FEditOptions) and (OldState.PartIndex > -1) and
        (OldState.PartIndex < FGuides.Count) then
      begin
        LayerBounds(CR, R);
        
        G := FGuides[OldState.PartIndex];
        P := NewState.Test;

        if G.GuideType = scgtHorizontal then
        begin
          if (P.y >= 0) and (P.y <= (R.Bottom - R.Top)/Z) then
            G.Position := G.Position + (NewState.Test.y - OldState.Test.y)
          else
            G.Free;
        end else
        if (P.x >= 0) and (P.x <= (R.Right - R.Left)/Z) then
          G.Position := G.Position + (NewState.Test.x - OldState.Test.x)
        else
          G.Free;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.FocusChanged;
begin
  UpdateHottrack;
  inherited FocusChanged;
end;

function TSCDeCustomEditor.GetSelection(Index: Integer): TSCDeShapeBase;
begin
  Result := TSCDeShapeBase(FSelectionList[Index]);
end;

function TSCDeCustomEditor.GetSelectionCount: Integer;
begin
  Result := FSelectionList.Count;
end;

procedure TSCDeCustomEditor.GetSelections(L: TList);
var
  I: Integer;
begin
  L.Clear;
  for I := 0 to FSelectionList.Count-1 do
    L.Add(FSelectionList[I]);
end;

procedure TSCDeCustomEditor.GuidePropertiesChanged(Sender: TObject);
begin
  if not InUpdate and (scdoShowGuides in FEditOptions) then
    Invalidate;
end;

procedure TSCDeCustomEditor.GuideChanged(G: TSCDeGuide);
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  if not (InUpdate or (csDestroying in ComponentState)) then
  begin
    DoGuideChanged(G);
    Invalidate;

    for I := 0 to ClientCount-1 do
    begin
      C := Self.Clients[I];
      if C is TSCDeEditorClient then
        TSCDeEditorClient(C).DoGuideChange;
    end;

    if Assigned(FOnGuideChange) then FOnGuideChange(Self);
  end;  
end;

function TSCDeCustomEditor.HitTest(P: TDoublePoint): TSCDeHitTest;
begin
  Result := Self.HitCase(P, scesNone, []);
end;

procedure TSCDeCustomEditor.HotShapeChanged;
begin
  //
end;

procedure TSCDeCustomEditor.HottrackPropsChanged(Sender: TObject);
begin
  if not InUpdate and (FHotShape <> nil) and (scdoHottrack in FEditOptions) then
    Invalidate;
end;

function TSCDeCustomEditor.IsSelected(S: TSCDeShapeBase): Boolean;
begin
  Result := (S <> nil) and not (S.InDestroy or S.Locked) and
    S.Visible and (FSelectionList.IndexOf(S) > -1) and
    (S.Owner <> nil) and S.Owner.Visible and
    not (S.Owner.InDestroy or S.Owner.Locked);
end;

procedure TSCDeCustomEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
  Gp: TPoint;
  X, Y: Double;
  BR: TDoubleRect;
  S: TSCDeShapeBase;
  Offset: TDoublePoint;
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_ESCAPE then
  begin
    if FEditState = scesCreateNew then
      RestartNewShape
    else if FEditState <> scesNone then
      SetEditState(scesNone)
    else
      ClearSelection; 
  end else
  if (FSelectionList.Count > 0) and (Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) then
  begin
    Offset := TSCDeUtils.Point(0, 0);

    Gp.x := Self.Grid.X;
    Gp.y := Self.Grid.Y;

    BeginUndoGroup;
    try
      BeginUpdate;
      try
        if Key in [VK_UP, VK_DOWN] then
        begin
          X := 0;

          Y := FKeyMoveSizeStep;
          if ssCtrl in Shift then Y := Grid.Y;

          if Key = VK_UP then Y := -Y;

          if (scdoSnapToGrid in EditOptions) and (ssCtrl in Shift) then
          begin
            BR := Self.GetSelectionBounds;
            TSCDeUtils.OffsetRect(BR, X, Y);

            if Gp.y > 1 then
            begin
              Offset.y := BR.Top - TSCDeUtils.Round(BR.Top / Gp.y)*Gp.y;

              if Offset.y >= Gp.y div 2 then
                Offset.y := Gp.y - Offset.y
              else
                Offset.y := -Offset.y;
            end;

            Y := Y + Offset.y;

            if (Abs(Y) > Gp.y) then
            begin
              if (Y < 0) then
                Y := Y + Gp.y
              else Y := Y - Gp.y;
            end;
          end;
        end else
        begin
          Y := 0;

          X := FKeyMoveSizeStep;
          if ssCtrl in Shift then X := Grid.X;

          if Key = VK_LEFT then X := -X;

          if (scdoSnapToGrid in EditOptions) and (ssCtrl in Shift) then
          begin
            BR := Self.GetSelectionBounds;
            TSCDeUtils.OffsetRect(BR, X, Y);

            if Gp.x > 1 then
            begin
              Offset.x := BR.Left - TSCDeUtils.Round(BR.Left / Gp.x)*Gp.x;

              if Offset.x >= Gp.x div 2 then
                Offset.x := Gp.x - Offset.x
              else
                Offset.x := -Offset.x;
            end;

            X := X + Offset.x;

            if (Abs(X) > Gp.x) then
            begin
              if (X < 0) then
                X := X + Gp.x
              else X := X - Gp.x;
            end;
          end;
        end;

        for I := 0 to FSelectionList.Count-1 do
        begin
          S := TSCDeShapeBase(FSelectionList[I]);
          if S.InDestroy or S.Locked or not S.Visible then
            Continue;

          if ssShift in Shift then
          begin
            if scdoKeyActions in FEditOptions then
            begin
              NotifyUndo(S, scacSizing, '');

              TSCDeFakeShape(S).BeginNotifyLock;
              try
                S.ResizeBy(X, Y);
                if ((X <> 0) or (Y <> 0)) and not S.Visible then
                  S.Refresh(True);
              finally
                TSCDeFakeShape(S).EndNotifyLock;
              end;
            end;
          end else
          if scdoKeyActions in FEditOptions then
          begin
            NotifyUndo(S, scacSizing, '');

            TSCDeFakeShape(S).BeginNotifyLock;
            try
              S.MoveBy(X, Y);
              if ((X <> 0) or (Y <> 0)) and not S.Visible then
                S.Refresh(True);
            finally
              TSCDeFakeShape(S).EndNotifyLock;
            end;
          end;
        end;
      finally
        EndUpdate;
      end;
    finally
      EndUndoGroup;
    end;
  end;
end;

procedure TSCDeCustomEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Z: Double;
  CR: TRect;
  Gp: TPoint;
  Index: Integer;
  R: TDoubleRect;
  Pt: TSCDePoint;
  S: TSCDeShapeBase;
  Hit: TSCDeHitTest;
  Pd, Offset: TDoublePoint;
  ShiftKeys: TSCDeShiftKeys;
begin
  FCursorPos := Point(X, Y);
  LayerBounds(CR, R);

  Z := GetZoom;

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := R.Left + ((Pd.x - R.Left)/Z);
  Pd.y := R.Top + ((Pd.y - R.Top)/Z);

  UpdateCursor(Pd.x, Pd.y, True);

  if not (scdoMouseActions in FEditOptions) then
  begin
    SetEditState(scesNone);
    CancelNewShape;
  end else
  if not InUpdate then
  begin
    if not Focused and CanGetFocus and CanFocus then
    begin
      SetFocus;
      if not Focused then
        Exit;
    end;

    ResetHittest(FHitRec);
    UpdateHitTestPos(FHitRec, Pd);

    FMoveRec := FHitRec;

    if not (FEditState in ContinuousEditActions) then
      SetEditState(scesNone);

    CR := ClientRect;

    if not IsRectEmpty(CR) and PtInRect(CR, Point(X, Y)) then
    begin
      ResetHittest(Hit);
      UpdateHitTestPos(Hit, Pd);

      Hit := HitCase(Pd, FEditState, []);

      if Button = mbRight then
      begin
        if FEditOptions*DeMouseOptions <> [] then
        begin
          if (Hit.State = schsHot) and (Hit.HitType in [schtShape, schtPoint]) and
            (Hit.Shape = FHotShape) and not IsSelected(Hit.Shape) then
          begin
            Hit.PartIndex := -1;
            Hit.HitType := schtShape;
          end;

          FHitRec := Hit;
          FMoveRec := FHitRec;

          SetHotShape(nil);
        end;

        if FEditState = scesCreateNew then
        begin
          if (FNewShape <> nil) and (scdoRightClickEndsCreation in FEditOptions) then
          begin
            Pd.x := Pd.x - R.Left;
            Pd.y := Pd.y - R.Top;

            if (scdoSnapToGrid in EditOptions) then
            begin
              Offset := TSCDeUtils.Point(0, 0);
              
              Gp.x := Self.Grid.X;
              Gp.y := Self.Grid.Y;

              if Gp.x > 1 then
              begin
                Offset.x := Pd.x - TSCDeUtils.Round(Pd.x / Gp.x)*Gp.x;

                if Offset.x >= Gp.x div 2 then
                  Offset.x := Gp.x - Offset.x
                else
                  Offset.x := -Offset.x;
              end;

              if Gp.y > 1 then
              begin
                Offset.y := Pd.y - TSCDeUtils.Round(Pd.y / Gp.y)*Gp.y;

                if Offset.y >= Gp.y div 2 then
                  Offset.y := Gp.y - Offset.y
                else
                  Offset.y := -Offset.y;
              end;

              Pd.x := Pd.x + Offset.x;
              Pd.y := Pd.y + Offset.y;
            end;

            Index := -1;
            if FNewShape.GetShapeType = sctyPolyPoints then
              Index := FNewShape.PointCount-1;

            ApplySizing(FNewShape, Index, -1, FActionStartRec.Test, Pd, Shift);

            EndNewShape;
          end;
        end else
        if (FEditState = scesNone) and (Hit.Shape <> nil) and
          not IsSelected(Hit.Shape) then
          Select(Hit.Shape);
      end else
      if Button = mbLeft then
      begin
        if FEditOptions*DeMouseOptions <> [] then
        begin
          if (Hit.State = schsHot) and (Hit.HitType in [schtShape, schtPoint]) and
            (Hit.Shape = FHotShape) and not IsSelected(Hit.Shape) then
          begin
            Hit.PartIndex := -1;
            Hit.HitType := schtShape;
          end;

          FHitRec := Hit;
          FMoveRec := FHitRec;

          SetHotShape(nil);
        end;

        ShiftKeys := TSCDeUtils.ShiftToScShift(Shift);

        BeginUpdate;
        try
          if not (FEditState in BlockedEditActions) and
            (Hit.HitType in [schtRulerHorz, schtRulerVert]) then
          begin
            if scdoShowGuides in FEditOptions then
              SetEditState(scesNewGuide);
          end else
          if not (FEditState in BlockedEditActions) and (Hit.HitType = schtGuide) then
          begin
            if scdoShowGuides in FEditOptions then
              SetEditState(scesDragGuide);
          end else
          if not (FEditState in BlockedEditActions) and (ShiftKeys <> []) and
            (ShiftKeys = FSelectionRectKeys) then
          begin
            if Hit.Shape <> nil then
            begin
              if IsSelected(Hit.Shape) then
              begin
                if (Hit.PartIndex > -1) and (Hit.State = schsSelected) and
                  (Hit.HitType in [schtPoint, schtControl, schtPointControl]) then
                begin
                  if (Hit.PartIndex > -1) and (Hit.HitType = schtPoint) and
                    (Hit.PartIndex < Hit.Shape.PointCount) then
                  begin
                    Pt := Hit.Shape.Points[Hit.PartIndex];

                    if (Shift*[ssLeft, ssDouble] <> []) and (ShiftKeys = FMultiSelectKeys) then
                    begin
                      if Pt <> nil then
                      begin
                        if Hit.Shape.IsSelected(Pt) then
                          Hit.Shape.RemoveSelection(Pt)
                        else
                          Hit.Shape.AddSelection(Pt);
                      end;    
                    end else
                      Hit.Shape.Select(Pt);
                  end;

                  SetEditState(scesSizing);
                end else
                if (Shift*[ssLeft, ssDouble] <> []) and (ShiftKeys = FMultiSelectKeys) then
                begin
                  RemoveSelection(Hit.Shape);
                  SetEditState(scesRectSelect);
                end;
              end else
              if (Shift*[ssLeft, ssDouble] <> []) and (ShiftKeys = FMultiSelectKeys) then
              begin
                AddSelection(Hit.Shape);
                SetEditState(scesMoving);
              end;
            end else
            if scdoCanSelect in FEditOptions then
              SetEditState(scesRectSelect);
          end else
          if FEditState = scesCreateNew then
          begin
            ClearSelection;

            Pd.x := Pd.x - R.Left;
            Pd.y := Pd.y - R.Top;

            if (scdoSnapToGrid in EditOptions) then
            begin
              Offset := TSCDeUtils.Point(0, 0);
              
              Gp.x := Self.Grid.X;
              Gp.y := Self.Grid.Y;

              if Gp.x > 1 then
              begin
                Offset.x := Pd.x - TSCDeUtils.Round(Pd.x / Gp.x)*Gp.x;

                if Offset.x >= Gp.x div 2 then
                  Offset.x := Gp.x - Offset.x
                else
                  Offset.x := -Offset.x;
              end;

              if Gp.y > 1 then
              begin
                Offset.y := Pd.y - TSCDeUtils.Round(Pd.y / Gp.y)*Gp.y;

                if Offset.y >= Gp.y div 2 then
                  Offset.y := Gp.y - Offset.y
                else
                  Offset.y := -Offset.y;
              end;

              Pd.x := Pd.x + Offset.x;
              Pd.y := Pd.y + Offset.y;
            end;

            if FNewShape = nil then
            begin
              SetHotShape(nil);

              if (FNewShapeClass <> nil) and (FEditState = scesCreateNew) then
              begin
                CreateNewShape;

                with FActionStartRec do
                begin
                  Shape := FNewShape;
                  Test := Pd;
                end;

                if FNewShape <> nil then
                begin
                  FNewShape.SetBounds(Pd.x, Pd.y, 0, 0);

                  if FNewShape.GetShapeType = sctyPolyPoints then
                  begin
                    TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y);
                    TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y);
                  end;
                end;
              end;
            end else
            if FNewShape.GetShapeType = sctyRectangle then
            begin
              Index := -1;
              if FNewShape.GetShapeType = sctyPolyPoints then
                Index := FNewShape.PointCount-1;

              ApplySizing(FNewShape, Index, -1, FActionStartRec.Test, Pd, Shift);

              EndNewShape;
            end else
            if FNewShape.GetShapeType = sctyPolyPoints then
            begin
              Pt := nil;
              if FNewShape.PointCount > 0 then
              begin
                Pt := FNewShape.Points[FNewShape.PointCount-1];
                Pt.SetPosition(Pd.x, Pd.y);
              end;

              if not (ssDouble in Shift) or not (scdoDblClickEndsCreation in FEditOptions) then
                TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y)
              else begin
                if (Pt <> nil) and (Pt.x <> Pd.x) and (Pt.y <> Pd.y) then
                  TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y);

                EndNewShape;
              end;
            end;
          end else
          if FEditState = scesRemovePoint then
          begin
            if Self.IsSelected(Hit.Shape) and
              (scssCanRemovePoint in Hit.Shape.ShapeStyle) and
              (Hit.PartIndex > -1) and (Hit.HitType = schtPoint) and
              (Hit.PartIndex < Hit.Shape.PointCount) then
            begin
              Pt := Hit.Shape.Points[Hit.PartIndex];

              if Pt <> nil then
              begin
                BeginUndoGroup;
                try
                  TSCDeFakeShape(Hit.Shape).Remove(Pt);

                  if Exists(Hit.Shape) and (Hit.Shape.PointCount = 0) then
                  begin
                    S := Hit.Shape;
                    Hit.Shape := nil;

                    S.Free;
                  end;
                finally
                  EndUndoGroup;
                end;
              end;
            end;
          end else
          if FEditState = scesAddPoint then
          begin
            if Self.IsSelected(Hit.Shape) and
              (scssAcceptsPoint in Hit.Shape.ShapeStyle) then
            begin
              LayerBounds(CR, R);

              Pd.x := Pd.x - R.Left;
              Pd.y := Pd.y - R.Top;

              Hit.Shape.BeginUpdate;
              try
                Pt := TSCDeFakeShape(Hit.Shape).ApproxInsert(Pd, Self.HitFuzz);
                if Pt <> nil then Hit.Shape.Select(Pt);
              finally
                Hit.Shape.EndUpdate;
              end;
            end;
          end else
          if FEditState = scesZoomRect then
          begin

          end else
          if FEditState = scesNone then
          begin
            if (Hit.Shape <> nil) and IsSelected(Hit.Shape) then
            begin
              if (Hit.State = schsSelected) and (Hit.PartIndex > -1) and
                (Hit.HitType in [schtPoint, schtControl, schtPointControl]) then
              begin
                Pt := nil;
                if Hit.PartIndex < Hit.Shape.PointCount then
                  Pt := Hit.Shape.Points[Hit.PartIndex];

                if (Shift*[ssLeft, ssDouble] <> []) and
                  (ShiftKeys <> []) and (ShiftKeys = FMultiSelectKeys) then
                begin
                  if Hit.Shape.IsSelected(Pt) then
                    Hit.Shape.RemoveSelection(Pt)
                  else begin
                    Hit.Shape.AddSelection(Pt);
                    SetEditState(scesSizing);
                  end;
                end else
                begin
                  Hit.Shape.Select(Pt);
                  SetEditState(scesSizing);
                end;
              end else
              if (Shift*[ssLeft, ssDouble] <> []) and
                (ShiftKeys <> []) and (ShiftKeys = FMultiSelectKeys) then
              begin
                if Self.IsSelected(Hit.Shape) then
                  Self.RemoveSelection(Hit.Shape)
                else begin
                  Self.AddSelection(Hit.Shape);
                  SetEditState(scesMoving);
                end;
              end else
                SetEditState(scesMoving);
            end else
            if (Hit.Shape <> nil) and (Shift*[ssLeft, ssDouble] <> []) and
              (ShiftKeys <> []) and (ShiftKeys = FMultiSelectKeys) then
            begin
              if Self.IsSelected(Hit.Shape) then
                Self.RemoveSelection(Hit.Shape)
              else begin
                Self.AddSelection(Hit.Shape);
                SetEditState(scesMoving);
              end;
            end else
            begin
              Select(Hit.Shape);

              if Hit.Shape = nil then
                SetEditState(scesRectSelect)
              else
              if (Hit.PartIndex > -1) and (Hit.State = schsSelected) and
                (Hit.HitType in [schtPoint, schtControl, schtPointControl]) then
                SetEditState(scesSizing)
              else
                SetEditState(scesMoving);
            end;
          end;
        finally
          EndUpdate;
        end;
      end;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
  Pd.y := ((Pd.y - R.Top)/Z) + R.Top;
  
  UpdateCursor(Pd.x, Pd.y, True);
end;

procedure TSCDeCustomEditor.MouseInControlChanged;
begin
  if not MouseInControl then
    SetHotShape(nil)
  else
    UpdateHottrack;

  inherited MouseInControlChanged;
end;

procedure TSCDeCustomEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Gp: TPoint;
  CR, SR: TRect;
  R: TDoubleRect;
  I, Index: Integer;
  Z, Dx, Dy: Double;
  Pt, Pt2: TSCDePoint;
  S, H: TSCDeShapeBase;
  Pd, Offset: TDoublePoint;
  ShiftKeys: TSCDeShiftKeys;
  OldMove, Hit: TSCDeHitTest;
  HasRulers, InRulerBlank: Boolean;
begin
  FCursorPos := Point(X, Y);
  LayerBounds(CR, R);

  Z := GetZoom;

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
  Pd.y := ((Pd.y - R.Top)/Z) + R.Top;

  UpdateCursor(Pd.x, Pd.y, True);

  if not (scdoMouseActions in FEditOptions) then
  begin
    SetEditState(scesNone);
    CancelNewShape;
  end else
  if not InUpdate then
  begin
    OldMove := FMoveRec;

    ResetHittest(FMoveRec);
    UpdateHitTestPos(FMoveRec, Pd);

    CR := ClientRect;

    if not IsRectEmpty(CR) then
    begin
      InRulerBlank := False;
      HasRulers := (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0);

      if HasRulers then
      begin
        I := FRulers.Size + 1;

        if FRulers.HorzRuler <> nil then FRulers.HorzRuler.SetPosition(X - I);
        if FRulers.VertRuler <> nil then FRulers.VertRuler.SetPosition(Y - I);

        HasRulers := (FRulers.GuideColor <> clNone) and
          ((FRulers.HorzRuler <> nil) or (FRulers.VertRuler <> nil));

        if HasRulers and (scdoShowCursorGuide in FEditOptions) then
        begin
          SR := CR;
          SR.Bottom := SR.Top + I;
          SR.Right := SR.Left + I;

          InRulerBlank := PtInRect(SR, Point(X, Y));
        end;
      end;

      if (FEditState = scesNone) and (not MouseIsDown or
        (FEditOptions*DeMouseOptions = [])) then
      begin
        H := FHotShape;

        if scdoHottrack in FEditOptions then
        begin
          Hit := HitTest(Pd);

          if Self.IsSelected(Hit.Shape) then
            ResetHittest(Hit);

          FMoveRec := Hit;
        end;

        S := nil;
        if (scdoHottrack in EditOptions) and
          not IsRectEmpty(CR) and PtInRect(CR, Point(X, Y)) then
        begin
          S := FMoveRec.Shape;
          if (S <> nil) and not (scssCanHottrack in S.ShapeStyle) then
            S := nil;
        end;

        SetHotShape(S);
        if S = H then
        begin
          if not InRulerBlank and (scdoShowCursorGuide in FEditOptions) then
            Invalidate
          else if HasRulers then
            InvalidateRulers;
        end;    
      end else
      begin
        with FMoveRec do
        begin
          Shape := FHitRec.Shape;
          PartIndex := FHitRec.PartIndex;
          SubIndex := FHitRec.SubIndex;
          HitType := FHitRec.HitType;
        end;

        case FEditState of
          scesDragGuide:
          begin
            if (scdoShowGuides in FEditOptions) or
              (not InRulerBlank and (scdoShowCursorGuide in FEditOptions)) then
              Invalidate
            else
            if HasRulers then
              InvalidateRulers;
          end;
          scesMoving:
          begin
            if FMouseSense = 1 then
              Invalidate
            else begin
              Dx := FMoveRec.Test.x - FHitRec.Test.x;
              Dy := FMoveRec.Test.y - FHitRec.Test.y;

              if (TSCDeUtils.Round(Dx) mod FMouseSense = 0) or
                (TSCDeUtils.Round(Dy) mod FMouseSense = 0) then
                Invalidate;
            end;
          end;
          scesZoomRect:
          begin
            if MouseIsDown then
              Invalidate
            else if HasRulers then
              InvalidateRulers;
          end;
          scesNewGuide,
          scesRectSelect:
          begin
            Invalidate;
          end;
          scesSizing:
          begin
            S := FMoveRec.Shape;
            ShiftKeys := TSCDeUtils.ShiftToScShift(Shift);

            if MouseIsDown and (S <> nil) and (FMoveRec.PartIndex > -1) and
              (FControlKeys <> []) and (ShiftKeys = FControlKeys) and
              (FMoveRec.SubIndex = -1) and (S.GetShapeType in [sctyFreeHand, sctyPolyPoints]) and
              (scssUsesPointControls in S.ShapeStyle) then
            begin
              Pt := S.Points[FMoveRec.PartIndex];

              if Pt <> nil then
              begin
                if (Pt.Control_2 = nil) or ((Pt.Control_2.x = 0) and
                  (Pt.Control_2.y = 0)) then
                begin
                  if Pt.Control_2 = nil then Pt.Control_2 := Pt.NewControl(0, 0);

                  FHitRec.SubIndex := 1;
                  FHitRec.HitType := schtPointControl;

                  FMoveRec.SubIndex := 1;
                  FMoveRec.HitType := schtPointControl;
                end else
                if (Pt.Control_1 = nil) or ((Pt.Control_1.x = 0) and
                  (Pt.Control_1.y = 0)) then
                begin
                  if Pt.Control_1 = nil then Pt.Control_1 := Pt.NewControl(0, 0);

                  FHitRec.SubIndex := 0;
                  FHitRec.HitType := schtPointControl;

                  FMoveRec.SubIndex := 0;
                  FMoveRec.HitType := schtPointControl;
                end;
              end;
            end;

            if (FMouseSense = 1) or ((FMoveRec.PartIndex > -1) and
              (FMoveRec.SubIndex > -1)) then
              Invalidate
            else begin
              Dx := FMoveRec.Test.x - FHitRec.Test.x;
              Dy := FMoveRec.Test.y - FHitRec.Test.y;

              if (TSCDeUtils.Round(Dx) mod FMouseSense = 0) or
                (TSCDeUtils.Round(Dy) mod FMouseSense = 0) then
                Invalidate;
            end;
          end;
          scesCreateNew:
          begin
            if (FNewShape <> nil) and MouseIsDown and
              (FNewShape.GetShapeType = sctyFreeHand) then
            begin
              Pd.x := Pd.x - R.Left;
              Pd.y := Pd.y - R.Top;

              TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y);

              Invalidate;
            end else
            if (FNewShape <> nil) and (FNewShape.GetShapeType <> sctyFreeHand) and
              ((FMoveRec.Test.x <> FHitRec.Test.x) or (FMoveRec.Test.y <> FHitRec.Test.y)) then
            begin
              Pd.x := Pd.x - R.Left;
              Pd.y := Pd.y - R.Top;

              if MouseIsDown and (FNewShape.GetShapeType = sctyPolyPoints) and
                (scssUsesPointControls in FNewShape.ShapeStyle) and
                not (scssSingleClickCreation in FNewShape.ShapeStyle) then
              begin
                Index := FNewShape.PointCount - 1;

                if Index > -1 then
                begin
                  Pt := FNewShape.Points[Index];

                  if Pt <> nil then
                  begin
                    if Index > 0 then
                    begin
                      Pt2 := FNewShape.Points[Index - 1];

                      if (Pt2 <> nil) and (Pt.x = Pt2.x) and (Pt.y = Pt2.y) then
                      begin
                        Pt.Free;
                        Pt := Pt2;
                      end;
                    end;

                    Pt.BeginUpdate;
                    try
                      if Pt.Control_1 = nil then Pt.Control_1 := Pt.NewControl(0, 0);
                      if Pt.Control_2 = nil then Pt.Control_2 := Pt.NewControl(0, 0);

                      Pd.x := Pd.x - Pt.x;
                      Pd.y := Pd.y - Pt.y;

                      Pt.Control_1.SetPosition(-Pd.x, -Pd.y);
                      Pt.Control_2.SetPosition(Pd.x, Pd.y);
                    finally
                      Pt.EndUpdate;
                    end;
                  end;
                end;
              end else
              if (Pd.x <> 0) or (Pd.y <> 0) then
              begin
                Index := -1;
                if FNewShape.GetShapeType = sctyPolyPoints then
                  Index := FNewShape.PointCount-1;

                if (scdoSnapToGrid in EditOptions) then
                begin
                  Offset := TSCDeUtils.Point(0, 0);

                  Gp.x := Self.Grid.X;
                  Gp.y := Self.Grid.Y;

                  if Gp.x > 1 then
                  begin
                    Offset.x := Pd.x - TSCDeUtils.Round(Pd.x / Gp.x)*Gp.x;

                    if Offset.x >= Gp.x div 2 then
                      Offset.x := Gp.x - Offset.x
                    else
                      Offset.x := -Offset.x;
                  end;

                  if Gp.y > 1 then
                  begin
                    Offset.y := Pd.y - TSCDeUtils.Round(Pd.y / Gp.y)*Gp.y;

                    if Offset.y >= Gp.y div 2 then
                      Offset.y := Gp.y - Offset.y
                    else
                      Offset.y := -Offset.y;
                  end;

                  Pd.x := Pd.x + Offset.x;
                  Pd.y := Pd.y + Offset.y;
                end;

                ApplySizing(FNewShape, Index, -1, FActionStartRec.Test, Pd, Shift);
              end;

              Invalidate;
            end else
            if not InRulerBlank and (scdoShowCursorGuide in FEditOptions) then
              Invalidate
            else
            if HasRulers then
              InvalidateRulers;
          end;
          else begin
            if not InRulerBlank and (scdoShowCursorGuide in FEditOptions) then
              Invalidate
            else if HasRulers then
              InvalidateRulers;
          end;
        end;
      end;
    end;
  end;

  inherited MouseMove(Shift, X, Y);

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
  Pd.y := ((Pd.y - R.Top)/Z) + R.Top;

  UpdateCursor(Pd.x, Pd.y, True);
end;

procedure TSCDeCustomEditor.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Z: Double;
  CR: TRect;
  Index: Integer;
  R: TDoubleRect;
  Pt: TSCDePoint;
  Pd: TDoublePoint;
  S: TSCDeShapeBase;
  OldHit, OldMove, StartHit,
  EndHit, CurHit, Hit: TSCDeHitTest;
begin
  FCursorPos := Point(X, Y);
  LayerBounds(CR, R);

  Z := GetZoom;

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
  Pd.y := ((Pd.y - R.Top)/Z) + R.Top;

  UpdateCursor(Pd.x, Pd.y, True);

  if not (scdoMouseActions in FEditOptions) then
  begin
    SetEditState(scesNone);
    CancelNewShape;
  end else
  if not InUpdate then
  begin
    OldHit := FHitRec;
    OldMove := FHitRec;

    with OldMove do
    begin
      Test.x := Pd.x;
      Test.y := Pd.y;
    end;

    ResetHittest(FHitRec);
    ResetHittest(FMoveRec);

    if (OldHit.Shape = nil) and (OldHit.HitType in [schtNone, schtLayer]) and
      (Button = mbLeft) and (FEditState in [scesNone, scesRectSelect, scesZoomRect]) then
    begin
      CurHit := HitCase(Pd, FEditState, []);
      if (OldHit.Test.x = CurHit.Test.x) and (OldHit.Test.y = CurHit.Test.y) then
        ClearSelection;
    end;

    if (Button = mbLeft) and (FEditState <> scesNone) then
    begin
      if (FEditState = scesCreateNew) and (FNewShape <> nil) and
        (FNewShape.GetShapeType = sctyPolyPoints) and
        (FNewShape.ShapeStyle*[scssUsesPointControls,
        scssSingleClickCreation] = [scssUsesPointControls]) then
      begin
        Index := FNewShape.PointCount - 1;

        if Index > -1 then
        begin
          Pt := FNewShape.Points[Index];
          if (Pt <> nil) and (Pt.Control_1 <> nil) and (Pt.Control_2 <> nil) then
            TSCDeFakeShape(FNewShape).Add(Pt.x, Pt.y);
        end;
      end else
      if ((FEditState = scesZoomRect) or not (FEditState in ContinuousEditActions)) and
        ((OldHit.Test.x <> OldMove.Test.x) or (OldHit.Test.y <> OldMove.Test.y)) then
      begin
        StartHit := OldHit;
        EndHit := OldMove;

        StartHit.Test.x := StartHit.Test.x - R.Left;
        StartHit.Test.y := StartHit.Test.y - R.Top;

        EndHit.Test.x := EndHit.Test.x - R.Left;
        EndHit.Test.y := EndHit.Test.y - R.Top;

        ExecuteHitAction(FEditState, StartHit, EndHit, Shift);
      end;
    end;

    CR := ClientRect;

    BeginUpdate;
    try
      if (Button = mbLeft) and (FEditState = scesCreateNew) and (FNewShape <> nil) then
      begin
        if FNewShape.GetShapeType = sctyFreeHand then
        begin
          Pd.x := Pd.x - R.Left;
          Pd.y := Pd.y - R.Top;

          TSCDeFakeShape(FNewShape).Add(Pd.x, Pd.y);

          EndNewShape;
        end else
        if scssSingleClickCreation in FNewShape.ShapeStyle then
        begin
          Pd.x := Pd.x - R.Left;
          Pd.y := Pd.y - R.Top;

          Index := -1;
          if FNewShape.GetShapeType = sctyPolyPoints then
            Index := FNewShape.PointCount-1;

          ApplySizing(FNewShape, Index, -1, FActionStartRec.Test, Pd, Shift);
          EndNewShape;
        end;
      end;

      if not IsRectEmpty(CR) and not MouseIsDown and
        not (FEditState in ContinuousEditActions) then
      begin
        if scdoHottrack in FEditOptions then
        begin
          Hit := HitCase(Pd, FEditState, []);
          FMoveRec := Hit;
        end;

        S := nil;
        if (scdoHottrack in EditOptions) and not IsRectEmpty(CR) and
          (FEditState = scesNone) and PtInRect(CR, Point(X, Y)) then
        begin
          S := FMoveRec.Shape;
          if (S <> nil) and not (scssCanHottrack in S.ShapeStyle) then
            S := nil;
        end;

        SetHotShape(S);
      end;

      if not (FEditState in ContinuousEditActions) then
        SetEditState(scesNone);
    finally
      EndUpdate;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  Pd := TSCDeUtils.Point(X, Y);

  Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
  Pd.y := ((Pd.y - R.Top)/Z) + R.Top;
  
  UpdateCursor(Pd.x, Pd.y, True);
end;

function TSCDeCustomEditor.CreateNewShape: TSCDeShapeBase;
var
  I: Integer;
  L: TSCDeLayer;
  C: TSCDeSurfaceClient;
begin
  EndNewShape;

  L := GetActiveLayer;
  if L = nil then L := TSCDeLayer.Create(Self);

  if (FNewShapeClass <> nil) and (L <> nil) then
  begin
    SetEditState(scesCreateNew);

    FNewShape := FNewShapeClass.Create(Self, nil);
    FNewShapeNotifier.RegisterShape(FNewShape);

    FNewShape.SetBounds(0, 0, 0, 0);

    FNewShape.BeginUpdate;

    for I := 0 to ClientCount-1 do
    begin
      C := Self.Clients[I];
      if C is TSCDeEditorClient then
        TSCDeEditorClient(C).DoNewShape(FNewShape);
    end;

    if Assigned(FOnNewShape) then
      FOnNewShape(Self, FNewShape);
  end;

  Result := FNewShape;
end;

procedure TSCDeCustomEditor.Paint;
var
  CR, R: TRect;
  I, J, XCount,
  YCount, Size, W, H: Integer;
begin
  if BufferedPaint and (FPaintBuffer <> nil) then
  begin
    CR := ClientRect;

    W := CR.Right - CR.Left;
    H := CR.Bottom - CR.Top;

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

        BitBlt(Canvas.Handle, R.Left, R.Top, W, H, FPaintBuffer.Canvas.Handle,
          R.Left - CR.Left, R.Top - CR.Top, SRCCOPY);

        OffsetRect(R, Size, 0);
      end;
    end;

    Exit;
  end;

  inherited Paint;

  DrawHottrack(Canvas);
  DrawGuides(Canvas);

  case FEditState of
    scesNewGuide:
      DrawNewGuide(Canvas);
    scesDragGuide:
      DrawGuideDrag(Canvas);
    scesRectSelect, scesZoomRect:
      DrawSelectionRect(Canvas);
    scesMoving:
      DrawSelectionMove(Canvas);
    scesSizing:
      DrawSelectionSize(Canvas);
    scesCreateNew:
      DrawNewCreation(Canvas);
  end;

  DrawCursorGuide(Canvas);
  DrawRulers(Canvas);
end;

procedure TSCDeCustomEditor.RemoveSelection(S: TSCDeShapeBase);
var
  Index: Integer;
begin
  if (S <> nil) and (S.Surface = Self) then
  begin
    Index := FSelectionList.IndexOf(S);

    if Index > -1 then
    begin
      BeginUpdate;
      try
        S.ClearSelection;
        FSelectionList.Delete(Index);
      finally
        EndUpdate;
      end;

      SelectionChanged;
    end;
  end;
end;

procedure TSCDeCustomEditor.RemoveSelections(A: array of TSCDeShapeBase);
var
  S: TSCDeShapeBase;
  I, Index: Integer;
  ListChanged: Boolean;
begin
  if Length(A) > 0 then
  begin
    ListChanged := False;

    BeginUpdate;
    try
      for I := Low(A) to High(A) do
      begin
        S := A[I];

        if (S <> nil) and (S.Surface = Self) then
        begin
          Index := FSelectionList.IndexOf(S);

          if Index > -1 then
          begin
            ListChanged := True;

            S.ClearSelection;
            FSelectionList.Delete(Index);
          end;
        end;
      end;
    finally
      EndUpdate;
    end;

    if ListChanged then SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.Select(S: TSCDeShapeBase);
var
  I: Integer;
  Shape: TSCDeShapeBase;
  AllowedShape, OnlySelected: Boolean;
begin
  AllowedShape := CanSelect(S);
  OnlySelected := (FSelectionList.Count = 1) and
    (FSelectionList[0] = S);

  if (AllowedShape and not OnlySelected) or
    ((S = nil) and (FSelectionList.Count > 0)) then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        Shape := TSCDeShapeBase(FSelectionList[I]);
        Shape.ClearSelection;
      end;

      FSelectionList.Clear;
      if (S <> nil) and (scdoCanSelect in FEditOptions) then
        FSelectionList.Add(S);
    finally
      EndUpdate;
    end;

    SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.SelectAll;
var
  L: TSCDeLayer;
  I, J: Integer;
  S: TSCDeShapeBase;
begin
  if scdoCanSelect in FEditOptions then
  begin
    FSelectionList.Clear;

    for I := 0 to LayerCount-1 do
    begin
      L := Layers[I];
      if not (L.InDestroy or L.Locked) and L.Visible then
        for J := 0 to L.ItemCount-1 do
        begin
          S := L.Items[J];
          if S.Visible and not (S.InDestroy or S.Locked) then
          begin
            FSelectionList.Add(S);
            if not (scdoMultiSelect in FEditOptions) then
              Break;
          end;
        end;
    end;

    SelectionChanged;
  end;  
end;

procedure TSCDeCustomEditor.SelectIn(R: TDoubleRect; FullSatisfy: Boolean);
var
  L: TSCDeLayer;
  SR: TDoubleRect;
  CanAdd: Boolean;
  S: TSCDeShapeBase;
  I, J, Cnt: Integer;
begin
  if TSCDeUtils.IsRectEmpty(R) then
    ClearSelection
  else if scdoCanSelect in FEditOptions then
  begin
    Cnt := FSelectionList.Count;
    FSelectionList.Clear;

    for I := 0 to LayerCount-1 do
    begin
      L := Layers[I];

      if L.Visible and not (L.InDestroy or L.Locked) then
        for J := 0 to L.ItemCount-1 do
        begin
          S := L.Items[J];
            
          if S.Visible and not (S.InDestroy or S.Locked) then
          begin
            if not FullSatisfy then
              CanAdd := S.InRect(R)
            else begin
              SR := S.GetBounds;
              CanAdd := TSCDeUtils.IsRectInRect(SR, R);
            end;

            if CanAdd then
            begin
              FSelectionList.Add(S);
              if not (scdoMultiSelect in FEditOptions) then
                Break;
            end;    
          end;
        end;
    end;

    if (Cnt > 0) or (FSelectionList.Count > 0) then
      SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.SelectionChanged;
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  if not (csDestroying in ComponentState) then
  begin
    Changed(False);

    for I := 0 to ClientCount-1 do
    begin
      C := Self.Clients[I];
      if C is TSCDeEditorClient then
        TSCDeEditorClient(C).DoSelectionChange;
    end;

    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  end;
end;

procedure TSCDeCustomEditor.SelectionPropsChanged(Sender: TObject);
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) and
    (scdoCanSelect in FEditOptions) then
    SelectionChanged;
end;

procedure TSCDeCustomEditor.SelectList(A: array of TSCDeShapeBase);
var
  L: TList;
  I: Integer;
  S: TSCDeShapeBase;
  SelChanged: Boolean;
begin
  if Length(A) = 0 then
    ClearSelection
  else
  if scdoCanSelect in FEditOptions then
  begin
    SelChanged := False;
    L := TList.Create;
    try
      BeginUpdate;
      try
        for I := FSelectionList.Count-1 downto 0 do
        begin
          S := TSCDeShapeBase(FSelectionList[I]);

          L.Add(S);
          S.ClearSelection;
        end;

        FSelectionList.Clear;

        for I := Low(A) to High(A) do
        begin
          S := A[I];
          if CanSelect(S) and (FSelectionList.IndexOf(S) = -1) then
          begin
            FSelectionList.Add(S);
            if not (scdoMultiSelect in FEditOptions) then
              Break;
          end;
        end;

        SelChanged := L.Count <> FSelectionList.Count;
        
        if not SelChanged then
          for I := 0 to FSelectionList.Count-1 do
            if L.IndexOf(FSelectionList[I]) = -1 then
            begin
              SelChanged := True;
              Break;
            end;
      finally
        EndUpdate;
      end;
    finally
      L.Free;
      
      if SelChanged then
        SelectionChanged;
    end;
  end;
end;

procedure TSCDeCustomEditor.SetEditState(Value: TSCDeEditState);
begin
  if (FEditOptions*DeMouseOptions = []) and (Value in DeMouseActions) then
    Value := scesNone;

  if FEditState <> Value then
  begin
    if Value <> scesCreateNew then SetNewShapeClass(nil);

    FEditState := Value;

    if not (FEditState in ContinuousEditActions) then
      ResetHittest(FActionStartRec);

    if FEditState = scesNone then
    begin
      ResetHittest(FHitRec);
      if not (scdoHottrack in FEditOptions) then
        ResetHittest(FMoveRec);

      if FNewShape <> nil then FreeAndNil(FNewShape);
      SetCurrentTool(nil);
    end;

    UpdateHottrack;

    Invalidate;
    UpdateCursor(0, 0, False);

    EditStateChanged;
  end;
end;

procedure TSCDeCustomEditor.SetEditOptions(Value: TSCDeEditOptions);
begin
  if FEditOptions <> Value then
  begin
    FEditOptions := Value;
    if not (scdoCanUndo in FEditOptions) then
      ClearUndo;

    UpdateSelection;
    UpdateHottrack;

    Invalidate;
  end;
end;

procedure TSCDeCustomEditor.SetGuideProperties(Value: TSCDeGuideProperties);
begin
  FGuideProperties.Assign(Value);
end;

procedure TSCDeCustomEditor.SetHotShape(S: TSCDeShapeBase);
var
  OldShape: TSCDeShapeBase;
begin
  if FHotShape <> S then
  begin
    OldShape := FHotShape;
    FHotShape := S;

    if not (csDestroying in ComponentState) and (((S <> nil) and
      not IsSelected(S)) or ((OldShape <> nil) and not IsSelected(OldShape))) then
      Invalidate;

    HotShapeChanged;
  end;
end;

procedure TSCDeCustomEditor.SetHottrackProps(Value: TSCDeHottrackProps);
begin
  FHottrackProps.Assign(Value);
end;

procedure TSCDeCustomEditor.SetMoveSizeProps(Value: TSCDeMoveSizeProps);
begin
  FMoveSizeProps.Assign(Value);
end;

procedure TSCDeCustomEditor.SetSelectionRectKeys(Value: TSCDeShiftKeys);
begin
  if Value = [] then Value := [scskShift];

  if FSelectionRectKeys <> Value then
  begin
    FSelectionRectKeys := Value;
  end;
end;

procedure TSCDeCustomEditor.SetSelectionProps(Value: TSCDeSelectionProps);
begin
  FSelectionProps.Assign(Value);
end;

procedure TSCDeCustomEditor.SetSelectionRectProperties(
  Value: TSCDeSelectionRectProperties);
begin
  FSelectionRectProperties.Assign(Value);
end;

procedure TSCDeCustomEditor.SelectionRectPropertiesChanged(Sender: TObject);
begin
  if (FEditState = scesZoomRect) or ((FEditState = scesRectSelect) and
    (scdoCanSelect in FEditOptions) and (scdoMultiSelect in FEditOptions)) then
    Invalidate;
end;

procedure TSCDeCustomEditor.UpdateHottrack;
var
  P: TPoint;
  CR: TRect;
  Pd: TDoublePoint;
  S: TSCDeShapeBase;
  Hit: TSCDeHitTest;
begin
  if not InUpdate and HandleAllocated and (Parent <> nil) and
    Parent.HandleAllocated and GetCursorPos(P) and
    (ComponentState*[csLoading, csDesigning, csDestroying] = []) then
  begin
    CR := ClientRect;

    P  := Self.ScreenToClient(P);
    Pd := TSCDeUtils.Point(P);

    if not MouseIsDown then
    begin
      S := nil;
      
      if (scdoHottrack in EditOptions) and not IsRectEmpty(CR) and
        (FEditState = scesNone) and PtInRect(CR, P) then
      begin
        Hit := HitCase(Pd, FEditState, []);

        S := Hit.Shape;
        if (S <> nil) and not (scssCanHottrack in S.ShapeStyle) then
          S := nil;
      end;

      SetHotShape(S);
    end;
  end;
end;

procedure TSCDeCustomEditor.UpdatePaintBuffer;
var
  CR: TRect;
  OldDC: THandle;
begin
  if HandleAllocated and (Parent <> nil) and Parent.HandleAllocated and
    (ComponentState*[csDesigning, csDestroying, csLoading] = []) then
  begin
    if FPaintCount > 0 then
    begin
      FPaintNeeded := True;
      Exit;
    end;

    Inc(FPaintCount);
    try
      if not BufferedPaint then
      begin
        if FPaintBuffer <> nil then FreeAndNil(FPaintBuffer);
        Exit;
      end;

      if FPaintBuffer = nil then FPaintBuffer := TSCDeBitmap.Create;

      CR := ClientRect;
      OffsetRect(CR, -CR.Left, -CR.Top);

      if CR.Right < 0 then CR.Right := 0;
      if CR.Bottom < 0 then CR.Bottom := 0;

      if not IsRectEmpty(CR) then
      begin
        FPaintBuffer.SetBounds(CR.Right, CR.Bottom);

        OldDC := Self.Canvas.Handle;

        SetDirectBufferedPaint(False);
        try
          Self.Canvas.Handle := FPaintBuffer.Canvas.Handle;
          Paint;
        finally
          SetDirectBufferedPaint(True);
          Self.Canvas.Handle := OldDC;
        end;
      end;

      UpdateCursor(0, 0, False);
    finally
      Dec(FPaintCount);

      if FPaintNeeded and (FPaintCount = 0) then
      begin
        FPaintNeeded := False;
        UpdatePaintBuffer;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.UpdateSelection;
var
  I, Cnt: Integer;
  S: TSCDeShapeBase;
begin
  if not (csDestroying in ComponentState) then
  begin
    Cnt := FSelectionList.Count;

    if not (scdoCanSelect in FEditOptions) then
    begin
      FSelectionList.Clear;

      if (Cnt > 0) or (FSelectionList.Count > 0) then
        SelectionChanged;
    end else
    if Cnt > 0 then
    begin
      for I := FSelectionList.Count-1 downto 0 do
        if not Exists(TSCDeShapeBase(FSelectionList[I])) then
          FSelectionList.Delete(I);

      if (FSelectionList.Count > 1) and not (scdoMultiSelect in FEditOptions) then
      begin
        Cnt := -1;
        S := TSCDeShapeBase(FSelectionList[FSelectionList.Count - 1]);

        FSelectionList.Clear;
        FSelectionList.Add(S);
      end;

      if Cnt <> FSelectionList.Count then
        SelectionChanged;
    end;
  end;
end;

procedure TSCDeCustomEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := DLGC_WANTARROWS;
end;

procedure TSCDeCustomEditor.CancelNewShape;
begin
  ResetHittest(FActionStartRec);

  if FNewShape <> nil then
  begin
    FNewShapeNotifier.UnregisterShape(FNewShape);
    FreeAndNil(FNewShape);
  end;

  SetEditState(scesNone);
  Invalidate;
end;

procedure TSCDeCustomEditor.SetNewShapeClass(ShapeClass: TSCDeShapeClass);
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  if FNewShapeClass <> ShapeClass then
  begin
    FNewShapeClass := ShapeClass;

    if ShapeClass <> nil then
      SetEditState(scesCreateNew)
    else begin
      SetEditState(scesNone);
      CancelNewShape;
    end;

    for I := 0 to ClientCount-1 do
    begin
      C := Self.Clients[I];
      if C is TSCDeEditorClient then
        TSCDeEditorClient(C).DoNewShapeClass;
    end;

    if Assigned(FOnNewShapeClass) then
      FOnNewShapeClass(Self);
  end;
end;

procedure TSCDeCustomEditor.DrawNewCreation(C: TCanvas);
var
  Z: Double;
  CR: TRect;
  R: TDoubleRect;
  Dp: TSCDeDragPoint;
  SelData: TSCDeSelectionData;
begin
  if (FEditState = scesCreateNew) and (FNewShape <> nil) and
    ((FMoveRec.Test.x <> FHitRec.Test.x) or (FMoveRec.Test.y <> FHitRec.Test.y)) then
  begin
    LayerBounds(CR, R);

    if not IsRectEmpty(CR) then
    begin
      Z := GetZoom;

      with FMoveSizeProps do
      begin
        SelData.LineColor     := LineColor;
        SelData.LineStyle     := LineStyle;
        SelData.LineMode      := LineMode;
        SelData.CtrlInColor   := clNone;
        SelData.CtrlOutColor  := clNone;
        SelData.PointInColor  := clNone;
        SelData.PointOutColor := clNone;
        SelData.PointSize     := 0;
        SelData.SelectionType := scdsMoving;
      end;

      Dp.PartIndex := -1;
      Dp.SubIndex := -1;
      Dp.Offset := TSCDeUtils.EmptyPoint;
      Dp.PointType := scptNone;

      if MouseIsDown and (FNewShape.PointCount > 0) and
        (FNewShape.GetShapeType = sctyPolyPoints) and
        (scssUsesPointControls in FNewShape.ShapeStyle) and
        not (scssSingleClickCreation in FNewShape.ShapeStyle) then
      begin
        Dp.PointType := scptPointControl;
        Dp.PartIndex := FNewShape.PointCount-1;
        Dp.SubIndex  := 3;
      end;

      FNewShape.DrawSelection(C, R.Left, R.Top, Dp, SelData, Z);
    end;
  end;
end;

procedure TSCDeCustomEditor.SetDefaultBrush(Value: TSCDeShapeBrush);
begin
  FDefaultBrush.Assign(Value);
end;

procedure TSCDeCustomEditor.SetDefaultFont(Value: TSCDeShapeFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TSCDeCustomEditor.SetDefaultPen(Value: TSCDeShapePen);
begin
  FDefaultPen.Assign(Value);
end;

procedure TSCDeCustomEditor.BeginPointAdd;
begin
  SetEditState(scesAddPoint);
end;

procedure TSCDeCustomEditor.EndPointAdd;
begin
  if FEditState = scesAddPoint then
    SetEditState(scesNone);
end;

function TSCDeCustomEditor.HitCase(P: TDoublePoint;
  WithState: TSCDeEditState; Shift: TShiftState): TSCDeHitTest;
var
  I, J: Integer;
  L: TSCDeLayer;
  R: TDoubleRect;
  Pd: TDoublePoint;
  CR, SR, IR: TRect;
  S: TSCDeShapeBase;
  Hit: TSCDeShapeHit;
  Z, Pt, W, H: Double;
  Fuz, PointSize: Double;
  ShiftKeys: TSCDeShiftKeys;
begin
  ResetHittest(Result);

  CR := ClientRect;

  with Result do
  begin
    Test.x := P.x - CR.Left;
    Test.y := P.y - CR.Top;
  end;

  if not IsRectEmpty(CR) then
  begin
    Z := GetZoom;

    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    Pd.x := R.Left + (P.x - R.Left)*Z;
    Pd.y := R.Top + (P.y - R.Top)*Z;
    
    if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
    begin
      I := FRulers.Size + 1;

      SR := CR;
      SR.Bottom := SR.Top + I;

      if TSCDeUtils.PtInRect(TSCDeUtils.Rect(SR), Pd) then
      begin
        Result.HitType := schtRulerHorz;
        Exit;
      end;

      SR := CR;
      SR.Right := SR.Left + I;

      if TSCDeUtils.PtInRect(TSCDeUtils.Rect(SR), Pd) then
      begin
        Result.HitType := schtRulerVert;
        Exit;
      end;
    end;

    if IntersectRect(IR, IR, CR) and TSCDeUtils.PtInRect(R, P) then
      Result.HitType := schtLayer;

    Fuz := FHitFuzz / Z;

    P.x := P.x - R.Left;
    P.y := P.y - R.Top;

    ShiftKeys := TSCDeUtils.ShiftToScShift(Shift);

    if (scdoShowGuides in FEditOptions) and TSCDeUtils.PtInRect(R, Pd) and
      (((ShiftKeys <> []) and (ShiftKeys = FSelectionRectKeys)) or
      (WithState = scesNone) or (WithState in ContinuousEditActions)) then
    begin
      W := (R.Right - R.Left)/Z;
      H := (R.Bottom - R.Top)/Z;

      for I := 0 to FGuides.Count-1 do
      begin
        Pt := FGuides[I].Position;

        if FGuides[I].GuideType = scgtHorizontal then
        begin
          if TSCDeUtils.NearLine(P, TSCDeUtils.Point(0, Pt),
            TSCDeUtils.Point(W, Pt), Fuz) then
          begin
            Result.PartIndex := I;
            Result.HitType := schtGuide;

            Exit;
          end;
        end else
        if TSCDeUtils.NearLine(P, TSCDeUtils.Point(Pt, 0),
          TSCDeUtils.Point(Pt, H), Fuz) then
        begin
          Result.PartIndex := I;
          Result.HitType := schtGuide;

          Exit;
        end;
      end;
    end;

    PointSize := 5.0;
    if FSelectionProps <> nil then PointSize := FSelectionProps.PointSize;

    PointSize := PointSize/Z;

    if (FHotShape <> nil) and not (FHotShape.InDestroy or not IsSelected(FHotShape)) and
      (scssCanHottrack in FHotShape.ShapeStyle) then
    begin
      Hit := FHotShape.HitTest(P, Fuz, PointSize, IsSelected(FHotShape),
        True, Shift);

      if Hit.Part <> scspNone then
      begin
        with Result do
        begin
          PartIndex := Hit.PartIndex;
          SubIndex := Hit.SubIndex;
          State := schsHot;
          Shape := FHotShape;
          HitType := schtShape;
        end;

        if Hit.Part = scspPointControl then
          Result.HitType := schtPointControl
        else
        if Hit.Part = scspPoint then
          Result.HitType := schtPoint
        else
        if Hit.Part = scspControl then
          Result.HitType := schtControl;

        Exit;
      end;
    end;

    for I := 0 to FSelectionList.Count-1 do
    begin
      S := TSCDeShapeBase(FSelectionList[I]);

      if S.Visible and not S.InDestroy then
      begin
        Hit := S.HitTest(P, Fuz, PointSize, True, True, Shift);

        if Hit.Part <> scspNone then
        begin
          with Result do
          begin
            PartIndex := Hit.PartIndex;
            SubIndex := Hit.SubIndex;
            State := schsSelected;
            Shape := S;
            HitType := schtShape;
          end;

          if Hit.Part = scspPointControl then
            Result.HitType := schtPointControl
          else
          if Hit.Part = scspPoint then
            Result.HitType := schtPoint
          else
          if Hit.Part = scspControl then
            Result.HitType := schtControl;

          Exit;
        end;
      end;
    end;

    for I := 0 to LayerCount-1 do
    begin
      L := Layers[I];

      if L.Visible and not (L.InDestroy or L.Locked) then
        for J := L.ItemCount-1 downto 0 do
        begin
          S := L.Items[J];

          if S.Visible and not S.InDestroy then
          begin
            Hit := S.HitTest(P, Fuz, PointSize, False, True, Shift);

            if Hit.Part <> scspNone then
            begin
              if Hit.Part = scspPointControl then
                Exit;

              with Result do
              begin
                PartIndex := Hit.PartIndex;
                SubIndex := Hit.SubIndex;
                State := schsNone;
                Shape := S;
                HitType := schtShape;
              end;

              if Hit.Part = scspPoint then
                Result.HitType := schtPoint
              else
              if Hit.Part = scspControl then
                Result.HitType := schtControl;

              Exit;
            end;
          end;
        end;
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawGuideDrag(C: TCanvas);
var
  Z: Double;
  Cl: TColor;
  P: Integer;
  CR, IR: TRect;
  G: TSCDeGuide;
  R: TDoubleRect;
begin
  if (FEditState = scesDragGuide) and (scdoShowGuides in FEditOptions) and
    (FGuides <> nil) and (FGuides.Count > 0) and (FGuideProperties <> nil) and
    (FGuideProperties.Color <> clNone) and (FGuideProperties.Style <> psClear) and
    (FHitRec.HitType = schtGuide) and (FHitRec.PartIndex > -1) and (FHitRec.PartIndex < FGuides.Count) then
  begin
    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    if not (IsRectEmpty(CR) or IsRectEmpty(IR)) then
    begin
      Z := GetZoom;

      Cl := Layer.Color;
      if Cl = clNone then Cl := clWhite;

      with C do
      begin
        Brush.Color := Cl;
        Brush.Style := bsClear;

        Pen.Color := FGuideProperties.Color;
        Pen.Style := FGuideProperties.Style;
        Pen.Mode  := FGuideProperties.Mode;
        Pen.Width := 1;
      end;

      G := FGuides[FHitRec.PartIndex];

      if G.GuideType = scgtHorizontal then
      begin
        P := TSCDeUtils.Round(Z*(G.Position + (FMoveRec.Test.y - FHitRec.Test.y)));

        TSCDeGraphicUtils.Line(C, Point(IR.Left, P + IR.Top),
          Point(IR.Right, P + IR.Top));
      end else
      begin
        P := TSCDeUtils.Round(Z*(G.Position + (FMoveRec.Test.x - FHitRec.Test.x)));

        TSCDeGraphicUtils.Line(C, Point(P + IR.Left, IR.Top),
          Point(P + IR.Left, IR.Bottom));
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.RestartNewShape;
begin
  ResetHittest(FActionStartRec);

  if FNewShape <> nil then
  begin
    FNewShapeNotifier.UnregisterShape(FNewShape);
    FreeAndNil(FNewShape);
  end;

  Invalidate;
  
  UpdateCursor(0, 0, False);
end;

procedure TSCDeCustomEditor.SetSelecting;
begin
  SetEditState(scesNone);
end;

procedure TSCDeCustomEditor.CMCursorChanged(var Message: TMessage);
begin
  inherited;

  if FUpdatingCursor = 0 then
  begin
    FDefaultCursor := Cursor;
    if not (csLoading in ComponentState) then
      UpdateCursor(-1, -1);
  end;
end;

procedure TSCDeCustomEditor.UpdateCursor(X, Y: Double; UseXY: Boolean);
var
  Z: Double;
  P: TPoint;
  CR: TRect;
  Crs: TCursor;
  R: TDoubleRect;
  Pd: TDoublePoint;
begin
  if HandleAllocated and (Parent <> nil) and Parent.HandleAllocated and
    (ComponentState*[csDesigning, csDestroying, csLoading] = []) then
  begin
    Inc(FUpdatingCursor);
    try
      Pd := TSCDeUtils.Point(X, Y);

      if not UseXY then
      begin
        GetCursorPos(P);
        P := Self.ScreenToClient(P);

        Pd := TSCDeUtils.Point(P.x, P.y);

        LayerBounds(CR, R);

        Z := GetZoom;

        Pd.x := ((Pd.x - R.Left)/Z) + R.Left;
        Pd.y := ((Pd.y - R.Top)/Z) + R.Top;
      end;

      if not GetCurrentCursor(Pd, Crs) then
        Crs := FDefaultCursor;

      Self.Cursor := Crs;
    finally
      Dec(FUpdatingCursor);
    end;
  end;
end;

function TSCDeCustomEditor.GetCurrentCursor(const P: TDoublePoint;
  var Cr: TCursor): Boolean;
var
  Hit: TSCDeHitTest;
  Shift: TShiftState;
  KeyState: TKeyboardState;
begin
  Result := True;

  Cr := Self.Cursor;

  if not MouseIsDown then
  begin
    Cr := FDefaultCursor;

    GetKeyboardState(KeyState);
    Shift := KeyboardStateToShiftState(KeyState);

    if FEditState = scesRemovePoint then
      Cr := crUpArrow
    else
    if FEditState in [scesAddPoint, scesCreateNew] then
      Cr := crCross
    else
    if FEditState = scesToolEdit then
    begin

    end else
    begin
      Hit := GuideTest(P, FEditState, Shift);

      if (Hit.HitType = schtGuide) and (scdoShowGuides in FEditOptions) and
        (Hit.PartIndex > -1) and (Hit.PartIndex < FGuides.Count) then
      begin
        Cr := crHSplit;
        if FGuides[Hit.PartIndex].FGuideType = scgtHorizontal then
          Cr := crVSplit;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateCursorPos;
  UpdateCursor(0, 0, False);
end;

procedure TSCDeCustomEditor.SetMultiSelectKeys(Value: TSCDeShiftKeys);
begin
  if Value = [] then Value := [scskCtrl];

  if FMultiSelectKeys <> Value then
  begin
    FMultiSelectKeys := Value;
  end;
end;

procedure TSCDeCustomEditor.SetControlKeys(Value: TSCDeShiftKeys);
begin
  if Value = [] then Value := [scskAlt];

  if FControlKeys <> Value then
  begin
    FControlKeys := Value;
  end;
end;

procedure TSCDeCustomEditor.ApplyControlMove(S: TSCDeShapeBase;
  CtrlIndex: Integer; StartPoint, EndPoint: TDoublePoint;
  Shift: TShiftState);
var
  Dx, Dy: Double;
  C: TSCDeControl;
  P, Pd: TDoublePoint;
begin
  if (S <> nil) and (CtrlIndex > -1) and (CtrlIndex < S.ControlCount) then
  begin
    C := S.Controls[CtrlIndex];
    if C = nil then
      Exit;

    P  := StartPoint;
    Pd := EndPoint;
    
    Pd.x := Pd.x - P.x;
    Pd.y := Pd.y - P.y;

    if (FMouseSense > 1) and (S <> FNewShape) then
    begin
      Dx := Math.Floor(Pd.x);
      Dy := Math.Floor(Pd.y);

      Pd.x := Dx - (Math.Floor(Dx) mod FMouseSense);
      Pd.y := Dy - (Math.Floor(Dy) mod FMouseSense);
    end;

    P := C.Point;
    Pd.x := Pd.x + P.x;
    Pd.y := Pd.y + P.y;

    C.SetPosition(Pd.x, Pd.y);

    if CtrlIndex < S.ControlCount then
    begin
      C := S.Controls[CtrlIndex];
      P := C.Point;

      if ((P.x <> Pd.x) or (P.y <> Pd.y)) and not S.Visible then
        S.Refresh(True);
    end;
  end;
end;

procedure TSCDeCustomEditor.ApplySizing(S: TSCDeShapeBase;
  PointIndex, SubIndex: Integer; StartPoint, EndPoint: TDoublePoint;
  Shift: TShiftState);
var
  Pt: TSCDePoint;
  DR: TDoubleRect;
  P, Pd: TDoublePoint;
  W, H, Dx, Dy: Double;
  Pc: TSCDePointControl;
  ShiftKeys: TSCDeShiftKeys;
  Rotation: TSCDeSizeRotation;
begin
  if S <> nil then
  begin
    P := StartPoint;
    Pd := EndPoint;

    if PointIndex > -1 then
    begin
      if PointIndex < S.PointCount then
      begin
        Pt := S.Points[PointIndex];

        if Pt = nil then
          Exit;

        if SubIndex > -1 then
        begin
          Pc := nil;
          if SubIndex = 0 then
            Pc := Pt.Control_1
          else
          if SubIndex = 1 then
            Pc := Pt.Control_2;

          if Pc = nil then
            Exit;

          P := Pc.Point;
          Pd := P;

          P.x := P.x + (EndPoint.x - StartPoint.x);
          P.y := P.y + (EndPoint.y - StartPoint.y);

          Pc.SetPosition(P.x, P.y);

          if (P.x = 0.0) and (P.y = 0.0) then Pc.Free
          else P := Pc.Point;

          if ((P.x <> Pd.x) or (P.y <> Pd.y)) and not S.Visible then
            S.Refresh(True);
        end else
        begin
          P := Pt.Point;

          if (FMouseSense > 1) and (S <> FNewShape) then
          begin
            Dx := Math.Floor(Pd.x - P.x);
            Dy := Math.Floor(Pd.y - P.y);

            Pd.x := P.x + (Dx - (Math.Floor(Dx) mod FMouseSense));
            Pd.y := P.y + (Dy - (Math.Floor(Dy) mod FMouseSense));
          end;

          Pt.SetPosition(Pd.x, Pd.y);

          if PointIndex < S.PointCount then
          begin
            Pt := S.Points[PointIndex];
            P := Pt.Point;
            if ((P.x <> Pd.x) or (P.y <> Pd.y)) and not S.Visible then
              S.Refresh(True);
          end;
        end;
      end else
      begin
        P := S.PointValue[PointIndex];

        Dx := EndPoint.x - StartPoint.x;
        Dy := EndPoint.y - StartPoint.y;

        if (FMouseSense > 1) and (S <> FNewShape) then
        begin
          Dx := Math.Floor(Dx);
          Dy := Math.Floor(Dy);

          Dx := Dx - (Math.Floor(Dx) mod FMouseSense);
          Dy := Dy - (Math.Floor(Dy) mod FMouseSense);
        end;

        Pd.x := P.x + Dx;
        Pd.y := P.y + Dy;

        S.PointValue[PointIndex] := Pd;

        Pd := S.PointValue[PointIndex];
        if ((P.x <> Pd.x) or (P.y <> Pd.y)) and not S.Visible then
          S.Refresh(True);
      end;
    end else
    if S.GetShapeType = sctyRectangle then
    begin
      if Pd.x - P.x < 0.0 then
      begin
        Rotation := scsrSouthWest;
        if Pd.y - P.y < 0.0 then Rotation := scsrNorthWest;
      end else
      begin
        Rotation := scsrSouthEast;
        if Pd.y - P.y < 0.0 then Rotation := scsrNorthEast;
      end;

      DR.TopLeft := StartPoint;
      DR.Right   := Pd.x;
      DR.Bottom  := Pd.y;

      ShiftKeys := TSCDeUtils.ShiftToScShift(Shift);

      if (FControlKeys <> []) and (FControlKeys = ShiftKeys) then
      begin
        W := Abs(P.x - Pd.x);
        H := Abs(P.y - Pd.y);

        DR.Left := P.x - W;
        DR.Right := P.x + W;
        DR.Top := P.y - H;
        DR.Bottom := P.y + H;
      end;

      TSCDeFakeShape(S).ArrangeBoundsRect(DR);

      if (FControlKeys <> []) and (FControlKeys = ShiftKeys) then
      begin
        W := Abs(DR.Right - DR.Left)/2;
        H := Abs(DR.Bottom - DR.Top)/2;

        DR.Left := P.x - W;
        DR.Right := P.x + W;
        DR.Top := P.y - H;
        DR.Bottom := P.y + H;
      end else
      begin
        W := Abs(DR.Right - DR.Left);
        H := Abs(DR.Bottom - DR.Top);

        DR.TopLeft := StartPoint;
        DR.BottomRight := DR.TopLeft;

        case Rotation of
          scsrSouthEast:
          begin
            DR.Right := DR.Left + W;
            DR.Bottom := DR.Top + H;
          end;
          scsrSouthWest:
          begin
            DR.Left := DR.Right - W;
            DR.Bottom := DR.Top + H;
          end;
          scsrNorthWest:
          begin
            DR.Left := DR.Right - W;
            DR.Top := DR.Bottom - H;
          end;
          scsrNorthEast:
          begin
            DR.Right := DR.Left + W;
            DR.Top := DR.Bottom - H;
          end;
        end;
      end;

      if DR.Left > DR.Right then TSCDeUtils.Swap(DR.Left, DR.Right);
      if DR.Top > DR.Bottom then TSCDeUtils.Swap(DR.Top, DR.Bottom);

      S.SetBounds(DR);
    end;
  end;
end;

procedure TSCDeCustomEditor.SetCurrentTool(ATool: TSCDeToolBase);
var
  OldTool: TSCDeToolBase;
begin
  if FCurrentTool <> ATool then
  begin
    OldTool := FCurrentTool;
    FCurrentTool := nil;

    if OldTool <> nil then
    begin
      OldTool.FOwner := nil;
      OldTool.Changed;
    end;

    FCurrentTool := ATool;

    if FCurrentTool <> nil then
      with FCurrentTool do
      begin
        BeginUpdate;
        try
          FOwner := Self;
          Clear;
        finally
          Changed;
          EndUpdate;
        end;
      end;

    if FCurrentTool <> nil then
      SetEditState(scesToolEdit)
    else
      SetEditState(scesNone);
  end;
end;

procedure TSCDeCustomEditor.SetRulers(Value: TSCDeRulers);
begin
  FRulers.Assign(Value);
end;

procedure TSCDeCustomEditor.RulersChanged(Sender: TObject; Forced: Boolean);
begin
  if Forced then
    Changed(False)
  else
  if scdoShowCursorGuide in FEditOptions then
    Invalidate
  else
    InvalidateRulers;

  DoRulersChanged(Sender, Forced);
end;

procedure TSCDeCustomEditor.LayerBounds(var CR: TRect; var R: TDoubleRect);
var
  SR: TRect;
  Distance: Integer;
  Z, X, Y, W, H: Double;
begin
  CR := ClientRect;

  if CR.Right < CR.Left then CR.Right := CR.Left;
  if CR.Bottom < CR.Top then CR.Bottom := CR.Top;

  R := TSCDeUtils.EmptyRect;

  if not IsRectEmpty(CR) and (Layer <> nil) then
  begin
    W := Layer.Width;
    H := Layer.Height;

    if W < 0.0 then W := 0.0;
    if H < 0.0 then H := 0.0;

    Z := GetZoom;

    W := W * Z;
    H := H * Z;

    R := TSCDeUtils.Rect(0.0, 0.0, W, H);

    if (W > 0.0) or (H > 0.0) then
    begin
      SR := CR;
      Distance := 0;

      if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
      begin
        Inc(Distance, FRulers.Size + 1);

        Inc(SR.Left, Distance);
        Inc(SR.Top, Distance);
      end;

      Inc(Distance, Indent);

      X := Distance;
      Y := Distance;

      if W < SR.Right - SR.Left then
      begin
        X := SR.Left + TSCDeUtils.Round(((SR.Right - SR.Left) - R.Right) / 2);
        if X < Distance then X := Distance;
      end;

      if H < SR.Bottom - SR.Top then
      begin
        Y := SR.Top + TSCDeUtils.Round(((SR.Bottom - SR.Top) - R.Bottom) / 2);
        if Y < Distance then Y := Distance;
      end;

      TSCDeUtils.OffsetRect(R, X + HorizontalPos, Y + VerticalPos);
    end;
  end;
end;

procedure TSCDeCustomEditor.DoRulersChanged(Sender: TObject;
  Forced: Boolean);
begin
  //
end;

procedure TSCDeCustomEditor.DrawRulers(C: TCanvas);
var
  Z: Double;
  Rgn: HRGN;
  R: TDoubleRect;
  I, SaveIndex: Integer;
  CR, IR, Hr, Vr: TRect;
begin
  if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
  begin
    LayerBounds(CR, R);

    if not IsRectEmpty(CR) then
    begin
      Z := GetZoom;
      I := FRulers.Size + 1;

      IR := TSCDeUtils.Rect(R);
      OffsetRect(IR, -I, -I);

      FRulers.HorzRuler.SetOffset(IR.Left);
      FRulers.VertRuler.SetOffset(IR.Top);

      if FRulers.Color <> clNone then
      begin
        with C do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FRulers.Color;
        end;

        Hr := CR;
        Hr.Bottom := Hr.Top + I;

        C.FillRect(Hr);

        Vr := CR;
        Vr.Right := Vr.Left + I;

        C.FillRect(Vr);

        Inc(Hr.Left, I - 1);
        Dec(Hr.Bottom);

        if not IsRectEmpty(Hr) and (FRulers.HorzRuler <> nil) then
        begin
          Rgn := CreateRectRgnIndirect(Hr);
          try
            SaveIndex := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
          finally
            DeleteObject(Rgn);
          end;

          try
            if SaveIndex <> NULLREGION then
              FRulers.HorzRuler.Paint(C, Hr, Z);
          finally
            SelectClipRgn(C.Handle, 0);
          end;
        end;

        Inc(Vr.Top, I - 1);
        Dec(Vr.Right);

        if not IsRectEmpty(Vr) and (FRulers.VertRuler <> nil) then
        begin
          Rgn := CreateRectRgnIndirect(Vr);
          try
            SaveIndex := ExtSelectClipRgn(C.Handle, Rgn, RGN_COPY);
          finally
            DeleteObject(Rgn);
          end;

          try
            if SaveIndex <> NULLREGION then
              FRulers.VertRuler.Paint(C, Vr, Z);
          finally
            SelectClipRgn(C.Handle, 0);
          end;
        end;

        with C do
        begin
          Pen.Mode  := pmCopy;
          Pen.Style := psSolid;
          Pen.Width := 1;
          Pen.Color := clWindowFrame;

          if not IsRectEmpty(Hr) then
          begin
            MoveTo(Hr.Left,  Hr.Bottom);
            LineTo(Hr.Right, Hr.Bottom);
          end;

          if not IsRectEmpty(Vr) then
          begin
            MoveTo(Vr.Right, Vr.Top);
            LineTo(Vr.Right, Vr.Bottom);
          end;
        end;
      end;
    end;
  end;  
end;

procedure TSCDeCustomEditor.InvalidateRulers;
var
  OldDC: THandle;
  CR, Hr, Vr: TRect;
begin
  if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
  begin
    CR := ClientRect;

    if not IsRectEmpty(CR) then
    begin
      Hr := CR;
      Hr.Bottom := Hr.Top + FRulers.Size + 1;

      Vr := CR;
      Vr.Right := Vr.Left + FRulers.Size + 1;

      if not BufferedPaint or (FPaintBuffer = nil) then
      begin
        InvalidateRect(Self.Handle, @Hr, False);
        InvalidateRect(Self.Handle, @Vr, False);

        Exit;
      end;

      OldDC := Self.Canvas.Handle;

      SetDirectBufferedPaint(False);
      try
        Self.Canvas.Handle := FPaintBuffer.Canvas.Handle;
        DrawRulers(Self.Canvas);
      finally
        SetDirectBufferedPaint(True);
        Self.Canvas.Handle := OldDC;
      end;

      Canvas.CopyRect(Hr, FPaintBuffer.Canvas, Hr);
      Canvas.CopyRect(Vr, FPaintBuffer.Canvas, Vr);
    end;
  end;
end;

procedure TSCDeCustomEditor.DrawNewGuide(C: TCanvas);
var
  Z: Double;
  P: TPoint;
  Cl: TColor;
  CR, IR: TRect;
  R: TDoubleRect;
  Pd: TDoublePoint;
begin
  if (scdoShowGuides in FEditOptions) and (FEditState = scesNewGuide) and
    (FGuides <> nil) and (FGuideProperties <> nil) and
    (FGuideProperties.Color <> clNone) and (FGuideProperties.Style <> psClear) and 
    (FHitRec.HitType in [schtRulerHorz, schtRulerVert]) then
  begin
    LayerBounds(CR, R);

    if not IsRectEmpty(CR) then
    begin
      Z := GetZoom;

      Pd := FMoveRec.Test;
      Pd.x := R.Left + (Pd.x - R.Left)*Z;
      Pd.y := R.Top + (Pd.y - R.Top)*Z;

      P := TSCDeUtils.Point(Pd);

      if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
      begin
        IR := CR;
        IR.Bottom := IR.Top + FRulers.Size + 1;

        if PtInRect(IR, P) then
          Exit;

        IR := CR;
        IR.Right := IR.Left + FRulers.Size + 1;

        if PtInRect(IR, P) then
          Exit;
      end;

      IR := TSCDeUtils.Rect(R);

      Cl := Layer.Color;
      if Cl = clNone then Cl := clWhite;

      with C do
      begin
        Brush.Color := Cl;
        Brush.Style := bsClear;

        Pen.Color := FGuideProperties.Color;
        Pen.Style := FGuideProperties.Style;
        Pen.Mode  := FGuideProperties.Mode;
        Pen.Width := 1;
      end;

      if FHitRec.HitType = schtRulerHorz then
        TSCDeGraphicUtils.Line(C, Point(IR.Left, P.y), Point(IR.Right, P.y))
      else
        TSCDeGraphicUtils.Line(C, Point(P.x, IR.Top), Point(P.x, IR.Bottom));
    end;
  end;
end;

function TSCDeCustomEditor.GuideTest(P: TDoublePoint;
  WithState: TSCDeEditState; Shift: TShiftState): TSCDeHitTest;
var
  I: Integer;
  CR, IR: TRect;
  R: TDoubleRect;
  Pd: TDoublePoint;
  Fuz, Z, Pt, W, H: Double;
  ShiftKeys: TSCDeShiftKeys;
begin
  ResetHittest(Result);

  CR := ClientRect;

  with Result do
  begin
    Test.x := P.x - CR.Left;
    Test.y := P.y - CR.Top;
  end;

  if not IsRectEmpty(CR) then
  begin
    Z := GetZoom;

    LayerBounds(CR, R);
    IR := TSCDeUtils.Rect(R);

    Fuz := Z*FHitFuzz;

    P.x := P.x - R.Left;
    P.y := P.y - R.Top;

    Pd.x := R.Left + P.x*Z;
    Pd.y := R.Top + P.y*Z;

    ShiftKeys := TSCDeUtils.ShiftToScShift(Shift);

    if (scdoShowGuides in FEditOptions) and TSCDeUtils.PtInRect(R, Pd) and
      (((ShiftKeys <> []) and (ShiftKeys = FSelectionRectKeys)) or
      (WithState = scesNone) or (WithState in ContinuousEditActions)) then
    begin
      W := (R.Right - R.Left)/Z;
      H := (R.Bottom - R.Top)/Z;

      for I := 0 to FGuides.Count-1 do
      begin
        Pt := FGuides[I].Position;

        if FGuides[I].GuideType = scgtHorizontal then
        begin
          if TSCDeUtils.NearLine(P, TSCDeUtils.Point(0, Pt),
            TSCDeUtils.Point(W, Pt), Fuz) then
          begin
            Result.PartIndex := I;
            Result.HitType := schtGuide;

            Exit;
          end;
        end else
        if TSCDeUtils.NearLine(P, TSCDeUtils.Point(Pt, 0),
          TSCDeUtils.Point(Pt, H), Fuz) then
        begin
          Result.PartIndex := I;
          Result.HitType := schtGuide;

          Exit;
        end;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.SetCursorGuideProperties(
  Value: TSCDeGuideProperties);
begin
  FCursorGuideProperties.Assign(Value);
end;

procedure TSCDeCustomEditor.CursorGuidePropertiesChanged(Sender: TObject);
begin
  if not InUpdate and (scdoShowCursorGuide in FEditOptions) then
    Invalidate;
end;

procedure TSCDeCustomEditor.DrawCursorGuide(C: TCanvas);
var
  CR: TRect;
  I: Integer;
begin
  if (scdoShowCursorGuide in FEditOptions) and (FCursorGuideProperties <> nil) and
    (FCursorGuideProperties.Color <> clNone) then
  begin
    CR := ClientRect;

    if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
    begin
      I := FRulers.Size + 1;

      Inc(CR.Top, I);
      Inc(CR.Left, I);
    end;

    if not IsRectEmpty(CR) then
      with C, FCursorGuideProperties do
      begin
        Brush.Color := Self.Color;
        Brush.Style := bsClear;

        Pen.Width := 1;
        Pen.Mode  := Mode;
        Pen.Style := Style;
        Pen.Color := Color;

        if (FCursorPos.x >= CR.Left) and (FCursorPos.x < CR.Right) then
          TSCDeGraphicUtils.Line(C, Point(FCursorPos.x, CR.Top),
            Point(FCursorPos.x, CR.Bottom));

        if (FCursorPos.y >= CR.Top) and (FCursorPos.y < CR.Bottom) then
          TSCDeGraphicUtils.Line(C, Point(CR.Left, FCursorPos.y),
            Point(CR.Right, FCursorPos.y));
      end;
  end;
end;

procedure TSCDeCustomEditor.UpdateCursorPos;
var
  P: TPoint;
begin
  if HandleAllocated and (Parent <> nil) and Parent.HandleAllocated and
    (ComponentState*[csDesigning, csDestroying, csLoading] = []) then
  begin
    GetCursorPos(P);
    P := Self.ScreenToClient(P);

    FCursorPos := P;
  end;
end;

function TSCDeCustomEditor.GetPaintRect: TRect;
var
  R: TRect;
  B, I: Integer;
begin
  Result := Rect(0, 0, 0, 0);

  if HandleAllocated then
  begin
    Result := ClientRect;

    R := Self.BoundsRect;

    B := GetBorderSize + BorderWidth + GetInnerBorderSize;
    OffsetRect(R, -R.Left, -R.Top);

    if R.Right < R.Left then R.Right := R.Left;
    if R.Bottom < R.Top then R.Bottom := R.Top;

    if (B > 0) and (Result.Right >= R.Right) then Dec(Result.Right, 2*B);
    if (B > 0) and (Result.Bottom >= R.Bottom) then Dec(Result.Bottom, 2*B);

    if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
    begin
      I := FRulers.Size + 1;

      Inc(Result.Left, I);
      Inc(Result.Top, I);

      if Result.Left > Result.Right then Result.Right := Result.Left;
      if Result.Top > Result.Bottom then Result.Bottom := Result.Top;
    end;
  end;
end;

procedure TSCDeCustomEditor.DoSelectionChanged;
begin
  //
end;

procedure TSCDeCustomEditor.NewShapeDestroyed(Sender: TObject;
  S: TSCDeShapeBase);
begin
  if S <> nil then
  begin
    if S = FNewShape then FNewShape := nil;
    if S = FTempShape then FTempShape := nil;
  end;
end;

procedure TSCDeCustomEditor.BringToFront;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        if S.Owner <> nil then S.Owner.BringToFront(S);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomEditor.SendToBack;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        if S.Owner <> nil then S.Owner.SendToBack(S);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomEditor.BringForward;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        if S.Owner <> nil then S.Owner.BringForward(S);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomEditor.SendBackward;
var
  I: Integer;
  S: TSCDeShapeBase;
begin
  if FSelectionList.Count > 0 then
  begin
    BeginUpdate;
    try
      for I := FSelectionList.Count-1 downto 0 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        if S.Owner <> nil then S.Owner.SendBackward(S);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomEditor.BeginPointRemove;
begin
  SetEditState(scesRemovePoint);
end;

procedure TSCDeCustomEditor.EndPointRemove;
begin
  if FEditState = scesRemovePoint then
    SetEditState(scesNone);
end;

procedure TSCDeCustomEditor.GroupSelection;
var
  I: Integer;
  A: array of TSCDeShapeBase;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    SetLength(A, FSelectionList.Count);
    for I := 0 to FSelectionList.Count-1 do
      A[I] := TSCDeShapeBase(FSelectionList[I]);

    Self.Group(A);
  end;
end;

procedure TSCDeCustomEditor.PackSelection;
var
  I: Integer;
  A: array of TSCDeShapeBase;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    SetLength(A, FSelectionList.Count);
    for I := 0 to FSelectionList.Count-1 do
      A[I] := TSCDeShapeBase(FSelectionList[I]);

    Self.Pack(A);
  end;
end;

procedure TSCDeCustomEditor.UngroupSelection;
var
  I: Integer;
  G: TSCDeGroup;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    G := nil;
    for I := 0 to FSelectionList.Count-1 do
      if TObject(FSelectionList[I]) is TSCDeGroup then
      begin
        G := TSCDeGroup(FSelectionList[I]);
        Break;
      end;

    if G <> nil then Self.Ungroup(G);
  end;
end;

procedure TSCDeCustomEditor.UnpackSelection;
var
  I: Integer;
  P: TSCDePackage;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    P := nil;
    for I := 0 to FSelectionList.Count-1 do
      if TObject(FSelectionList[I]) is TSCDePackage then
      begin
        P := TSCDePackage(FSelectionList[I]);
        Break;
      end;

    if P <> nil then Self.Unpack(P);
  end;
end;

function TSCDeCustomEditor.Group(A: array of TSCDeShapeBase): TSCDeGroup;
begin
  Result := nil;
  if Length(A) > 0 then
  begin
    BeginUpdate;
    try
      Result := inherited Group(A);
      if Result <> nil then Self.Select(Result);
    finally
      EndUpdate;
    end;
  end;
end;

function TSCDeCustomEditor.Pack(A: array of TSCDeShapeBase): TSCDePackage;
begin
  Result := nil;
  if Length(A) > 0 then
  begin
    BeginUpdate;
    try
      Result := inherited Pack(A);
      if Result <> nil then Self.Select(Result);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSCDeCustomEditor.DefaultBrushChanged(Sender: TObject);
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  for I := 0 to ClientCount-1 do
  begin
    C := Self.Clients[I];
    if C is TSCDeEditorClient then
      TSCDeEditorClient(C).DoDefBrushChange;
  end;

  if Assigned(FOnDefBrushChange) then
    FOnDefBrushChange(Self);
end;

procedure TSCDeCustomEditor.DefaultFontChanged(Sender: TObject);
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  for I := 0 to ClientCount-1 do
  begin
    C := Self.Clients[I];
    if C is TSCDeEditorClient then
      TSCDeEditorClient(C).DoDefFontChange;
  end;

  if Assigned(FOnDefFontChange) then FOnDefFontChange(Self);
end;

procedure TSCDeCustomEditor.DefaultPenChanged(Sender: TObject);
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  for I := 0 to ClientCount-1 do
  begin
    C := Self.Clients[I];
    if C is TSCDeEditorClient then
      TSCDeEditorClient(C).DoDefPenChange;
  end;

  if Assigned(FOnDefPenChange) then FOnDefPenChange(Self);
end;

procedure TSCDeCustomEditor.SetMouseSense(Value: Byte);
begin
  if Value < 1 then Value := 1;
  FMouseSense := Value;
end;

procedure TSCDeCustomEditor.CopyToClipboard;

  procedure SaveShape(S: TSCDeShapeBase; Root: TSCDomElement);
  var
    I: Integer;
    C: TSCDeContainer;
    Elm, Elms: TSCDomElement;
  begin
    Elm := TSCDomElement.Create(Root);
    Elm.Name := 'element';
    Elm.AddAttribute('class', S.ClassName);

    Root.AddElement(Elm);

    S.SaveToXML(Elm);

    if (S is TSCDeContainer) and (scssIsContainer in S.ShapeStyle) then
    begin
      C := TSCDeContainer(S);

      if C.ItemCount > 0 then
      begin
        Elms := TSCDomElement.Create(Root);
        Elms.Name := 'elements';

        Root.AddElement(Elms);

        for I := 0 to C.ItemCount-1 do
          SaveShape(C.Items[I], Elms);
      end;
    end;
  end;

  procedure SetClipboardText(const Text: WideString);
  var
    Data: THandle;
    DataPtr: Pointer;
    Size: Cardinal;
  begin
    Clipboard.Clear;
    Size := Length(Text);

    if Size > 0 then
    begin
      Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2*Size + 2);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(Text[1], DataPtr^, 2*Size + 2);
          Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    end;
  end;

var
  I: Integer;
  Doc: TSCDomDocument;
  Root: TSCDomElement;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    Doc := TSCDomDocument.Create(nil);
    try
      Root := TSCDomElement.Create(Doc);
      Root.Name := 'elements';
      Root.AddAttribute('type', 'scde.data');
      Root.AddAttribute('version', '1.0');

      Doc.AddElement(Root);

      for I := 0 to FSelectionList.Count-1 do
        SaveShape(TSCDeShapeBase(FSelectionList[I]), Root);

      SetClipboardText(Doc.ToString(4));
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSCDeCustomEditor.CutToClipboard;
begin
  CopyToClipboard;
  DeleteSelection;
end;

procedure TSCDeCustomEditor.PasteFromClipboard;

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

                TSCDeFakeShape(S).StoreID;
                try
                  S.LoadFromXML(Elm);
                finally
                  TSCDeFakeShape(S).RestoreID;
                end;

                Root.Add(S);

                Self.AddSelection(S);

                if S is TSCDeContainer then
                  LoadFromDom(TSCDeContainer(S), Elm);
              end;
            end;
          end;
    end;
  end;

var
  L: TSCDeLayer;
  P: TSCDomParser;
  Data: WideString;
  Doc: TSCDomDocument;
begin
  L := GetActiveLayer;

  if L <> nil then
  begin
    Data := GetClipboardText;

    if Copy(Data, 1, Length('<elements type="scde.data"')) =
      '<elements type="scde.data"' then
    begin
      P := TSCDomParser.Create(nil);
      try
        Doc := P.ParseString(Data);

        if Doc <> nil then
        begin
          try
            BeginUndoGroup;
            try
              BeginUpdate;
              try
                ClearSelection;

                L.BeginUpdate;
                try
                  LoadFromDom(L, Doc);
                finally
                  L.EndUpdate;
                end;
              finally
                EndUpdate;
              end;
            finally
              EndUndoGroup;
            end;
          finally
            Doc.Free;
          end;
        end;
      finally
        P.Free;
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.Redo;

  function GetProperty(Parent: TSCDomElement; const AName: WideString): WideString;
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

  procedure LoadFromDom(Root: TSCDeContainer; RootElm: TSCDomElement;
    IsFirst: Boolean);
  var
    S: String;
    I: Integer;
    SC: TSCDeShapeClass;          
    Shape: TSCDeShapeBase;
    Elms, Elm: TSCDomElement;
  begin
    S := RootElm.GetAttribute('class');

    if S <> '' then
    begin
      SC := TSCDeRegister.FindShape(S);

      if SC <> nil then
      begin
        Shape := SC.Create(nil, nil);
        Shape.LoadFromXML(RootElm);

        if IsFirst then
        begin
          TSCDeFakeShape(Root).BeginNotifyLock;
          Self.NotifyUndo(Root, scacInserting, '');
        end;

        try
          Root.Add(Shape);
        finally
          if IsFirst then
          begin
            S := RootElm.GetAttribute('isselected');
            if StrToIntDef(S, Integer(False)) = Integer(True) then
              Self.Select(Shape);

            TSCDeFakeShape(Root).EndNotifyLock;
            Self.NotifyUndo(Shape, scacInserted, '');
          end;
        end;

        if Shape is TSCDeContainer then
        begin
          Elms := RootElm.ElementByName('elements');

          if Elms <> nil then
            for I := 0 to Elms.ChildNodeCount-1 do
              if (Elms.ChildNodes[I].Name = 'element') and (Elms.ChildNodes[I] is TSCDomElement) then
              begin
                Elm := TSCDomElement(Elms.ChildNodes[I]);
                LoadFromDom(TSCDeContainer(Shape), Elm, False);
              end;
        end;
      end;
    end;
  end;

  procedure ProcessRedo(UndoNode: TSCDomElement);
  var
    L: TList;
    S: String;
    AID: DWord;
    C: TSCDeContainer;
    Act, I, J: Integer;
    Elm: TSCDomElement;
    Action: TSCDeAction;
    A: array of TSCDeShapeBase;
    OwnerShape, Shape: TSCDeShapeBase;
  begin
    Act := StrToIntDef(UndoNode.GetAttribute('action'), -1);

    if Act > -1 then
    begin
      Action := TSCDeAction(Act);

      case Action of
        scacChangingBrush:
        begin
          Elm := UndoNode.ElementByName('brush');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Brush.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingPen:
        begin
          Elm := UndoNode.ElementByName('pen');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Pen.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingFont:
        begin
          Elm := UndoNode.ElementByName('font');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Font.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingGradient:
        begin
          Elm := UndoNode.ElementByName('gradient');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);

                    TSCDeFakeShape(Shape).CreateGradient;
                    if TSCDeFakeShape(Shape).Gradient <> nil then
                      TSCDeFakeShape(Shape).Gradient.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChanging, scacChangingPicture:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              if Shape.ID <> AID then
                Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                TSCDeFakeShape(Shape).BeginNotifyLock;
                try
                  S := UndoNode.GetAttribute('changed_property');

                  Self.NotifyUndo(Shape, Action, S);
                  Shape.LoadFromXML(UndoNode);
                finally
                  TSCDeFakeShape(Shape).EndNotifyLock;
                end;

                Exit;
              end;
            end;
        end;
        scacSizing:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                TSCDeFakeShape(Shape).BeginNotifyLock;
                try
                  Self.NotifyUndo(Shape, scacSizing, '');
                  Shape.LoadBoundsFromXML(UndoNode);
                finally
                  TSCDeFakeShape(Shape).EndNotifyLock;
                end;
                Exit;
              end;
            end;
        end;
        scacInserted:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                OwnerShape := Shape.Owner;

                if OwnerShape <> nil then
                begin
                  TSCDeFakeShape(OwnerShape).BeginNotifyLock;

                  Self.NotifyUndo(OwnerShape, scacRemoving, '');
                  Self.NotifyUndo(Shape, scacRemoved, '');
                end;

                try
                  Shape.Free;
                finally
                  if OwnerShape <> nil then
                    TSCDeFakeShape(OwnerShape).EndNotifyLock;
                end;

                Exit;
              end;
            end;
        end;
        scacRemoved:
        begin
          S := UndoNode.GetAttribute('ownerid');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              if Shape.ID <> AID then
                Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDeContainer then
                  LoadFromDom(TSCDeContainer(Shape), UndoNode, True);

                Exit;
              end;
            end;
        end;
        scacGrouped:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDeGroup then
                begin
                  OwnerShape := Shape.Owner;

                  if OwnerShape <> nil then
                  begin
                    TSCDeFakeShape(OwnerShape).BeginNotifyLock;
                    Self.NotifyUndo(Shape, scacUngrouping, '');
                  end;

                  try
                    Self.Ungroup(TSCDeGroup(Shape));
                  finally
                    if OwnerShape <> nil then
                      TSCDeFakeShape(OwnerShape).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacPacked:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDePackage then
                begin
                  OwnerShape := Shape.Owner;

                  if OwnerShape <> nil then
                  begin
                    TSCDeFakeShape(OwnerShape).BeginNotifyLock;
                    Self.NotifyUndo(Shape, scacUnpacking, '');
                  end;

                  try
                    Self.Unpack(TSCDePackage(Shape));
                  finally
                    if OwnerShape <> nil then
                      TSCDeFakeShape(OwnerShape).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacUngrouping,
        scacUnpacking:
        begin
          L := TList.Create;
          try
            for I := 0 to UndoNode.ChildNodeCount-1 do
              if UndoNode.ChildNodes[I] is TSCDomElement then
              begin
                Elm := TSCDomElement(UndoNode.ChildNodes[I]);
                
                if Elm.Name = 'element' then
                begin
                  S := Elm.GetAttribute('id');
                  AID := Trunc(scdStrToFloatDef(S, 0));

                  if AID > 0 then
                    for J := 0 to LayerCount-1 do
                    begin
                      Shape := TSCDeShapeBase(Layers[J]).ShapeByID(AID);

                      if (Shape <> nil) and not Shape.InDestroy and
                        not (Shape is TSCDeLayer) then
                        L.Add(Shape);
                    end;
                end;
              end;

            if L.Count > 0 then
            begin
              SetLength(A, L.Count);
              for I := 0 to L.Count-1 do
                A[I] := TSCDeShapeBase(L[I]);

              if Action = scacUngrouping then
                Shape := Self.Group(A)
              else
                Shape := Self.Pack(A);

              if Shape <> nil then
              begin
                Shape.LoadFromXML(UndoNode);

                S := UndoNode.GetAttribute('isselected');
                if StrToIntDef(S, Integer(False)) = Integer(True) then
                  Self.Select(Shape);
              end;
            end;
          finally
            L.Free;
          end;
        end;
        scacSendingBack:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacBringingFront, '');
                    C.BringToFront(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacSendingBackward:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacBringingForward, '');
                    C.BringForward(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacBringingFront:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacSendingBack, '');
                    C.SendToBack(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacBringingForward:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacSendingBackward, '');
                    C.SendBackward(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
      end;
    end;
  end;

var
  UpCount: Boolean;
  I, Action, FirstAction: Integer;
  Root, Elm, UndoNode: TSCDomElement;
begin
  if CanRedo then
  begin
    FInRedo := True;
    try
      BeginUpdate;
      try
        Root := FRedoBuffer.Root;

        if (Root <> nil) and (Root.ChildNodeCount > 0) and
          (Root.ChildNodes[Root.ChildNodeCount-1] is TSCDomElement) then
        begin
          Elm := TSCDomElement(Root.ChildNodes[Root.ChildNodeCount-1]);

          Elm := TSCDomElement(Root.ExtractNode(Elm));
          try
            BeginUndoGroup;
            try
              if Elm.Name = 'redo' then
                ProcessRedo(Elm)
              else
              if (Elm.Name = 'redogroup') and (Elm.ChildNodeCount > 0) then
              begin
                UpCount := True;
                FirstAction := -1;
                
                for I := 0 to Elm.ChildNodeCount-1 do
                begin
                  Action := -1;

                  if Elm.ChildNodes[I] is TSCDomElement then
                  begin
                    UndoNode := TSCDomElement(Elm.ChildNodes[I]);
                    
                    if UndoNode.Name = 'redo' then
                    begin
                      Action := StrToIntDef(UndoNode.GetAttribute('action'), -1);
                      if I = 0 then FirstAction := Action;
                    end;
                  end;

                  if (Action <> FirstAction) or
                    not (Action in [Integer(scacInserted), Integer(scacRemoved)]) then
                  begin
                    UpCount := False;
                    Break;
                  end;
                end;

                if UpCount then
                begin
                  for I := 0 to Elm.ChildNodeCount-1 do
                    if Elm.ChildNodes[I] is TSCDomElement then
                    begin
                      UndoNode := TSCDomElement(Elm.ChildNodes[I]);
                      if UndoNode.Name = 'redo' then
                        ProcessRedo(UndoNode);
                    end;
                end else
                begin
                  for I := Elm.ChildNodeCount-1 downto 0 do
                    if Elm.ChildNodes[I] is TSCDomElement then
                    begin
                      UndoNode := TSCDomElement(Elm.ChildNodes[I]);
                      if UndoNode.Name = 'redo' then
                        ProcessRedo(UndoNode);
                    end;
                end;
              end;
            finally
              EndUndoGroup;
            end;
          finally
            Elm.Free;
          end;
        end;
      finally
        EndUpdate;
      end;
    finally
      FInRedo := False;
      Change;
    end;
  end;
end;

procedure TSCDeCustomEditor.Undo;

  function GetProperty(Parent: TSCDomElement; const AName: WideString): WideString;
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

  procedure LoadFromDom(Root: TSCDeContainer; RootElm: TSCDomElement;
    IsFirst: Boolean);
  var
    S: String;
    I: Integer;
    SC: TSCDeShapeClass;
    Shape: TSCDeShapeBase;
    Elms, Elm: TSCDomElement;
  begin
    S := RootElm.GetAttribute('class');

    if S <> '' then
    begin
      SC := TSCDeRegister.FindShape(S);

      if SC <> nil then
      begin
        Shape := SC.Create(nil, nil);
        Shape.LoadFromXML(RootElm);

        if IsFirst then
        begin
          TSCDeFakeShape(Root).BeginNotifyLock;
          Self.NotifyUndo(Root, scacInserting, '');
        end;

        try
          Root.Add(Shape);
        finally
          if IsFirst then
          begin
            S := RootElm.GetAttribute('isselected');
            if StrToIntDef(S, Integer(False)) = Integer(True) then
              Self.Select(Shape);

            TSCDeFakeShape(Root).EndNotifyLock;
            Self.NotifyUndo(Shape, scacInserted, '');
          end;
        end;

        if Shape is TSCDeContainer then
        begin
          Elms := RootElm.ElementByName('elements');

          if Elms <> nil then
            for I := 0 to Elms.ChildNodeCount-1 do
              if (Elms.ChildNodes[I].Name = 'element') and (Elms.ChildNodes[I] is TSCDomElement) then
              begin
                Elm := TSCDomElement(Elms.ChildNodes[I]);
                LoadFromDom(TSCDeContainer(Shape), Elm, False);
              end;
        end;
      end;
    end;
  end;

  procedure ProcessUndo(UndoNode: TSCDomElement);
  var
    L: TList;
    S: String;
    AID: DWord;
    C: TSCDeContainer;
    Act, I, J: Integer;
    Elm: TSCDomElement;
    Action: TSCDeAction;
    A: array of TSCDeShapeBase;
    OwnerShape, Shape: TSCDeShapeBase;
  begin
    Act := StrToIntDef(UndoNode.GetAttribute('action'), -1);

    if Act > -1 then
    begin
      Action := TSCDeAction(Act);

      case Action of
        scacChangingBrush:
        begin
          Elm := UndoNode.ElementByName('brush');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Brush.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingPen:
        begin
          Elm := UndoNode.ElementByName('pen');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Pen.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingFont:
        begin
          Elm := UndoNode.ElementByName('font');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);
                    TSCDeFakeShape(Shape).Font.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChangingGradient:
        begin
          Elm := UndoNode.ElementByName('gradient');

          if Elm <> nil then
          begin
            S := UndoNode.GetAttribute('id');
            AID := Trunc(scdStrToFloatDef(S, 0));

            if AID > 0 then
              for I := 0 to LayerCount-1 do
              begin
                Shape := TSCDeShapeBase(Layers[I]);
                if Shape.ID <> AID then
                  Shape := Shape.ShapeByID(AID);

                if Shape <> nil then
                begin
                  TSCDeFakeShape(Shape).BeginNotifyLock;
                  try
                    Self.NotifyUndo(Shape, Action, S);

                    TSCDeFakeShape(Shape).CreateGradient;
                    if TSCDeFakeShape(Shape).Gradient <> nil then
                      TSCDeFakeShape(Shape).Gradient.LoadFromXML(Elm);
                  finally
                    TSCDeFakeShape(Shape).EndNotifyLock;
                  end;

                  Exit;
                end;
              end;
          end;
        end;
        scacChanging, scacChangingPicture:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              if Shape.ID <> AID then Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                TSCDeFakeShape(Shape).BeginNotifyLock;
                try
                  S := UndoNode.GetAttribute('changed_property');

                  Self.NotifyUndo(Shape, Action, S);
                  Shape.LoadFromXML(UndoNode);
                finally
                  TSCDeFakeShape(Shape).EndNotifyLock;
                end;

                Exit;
              end;
            end;
        end;
        scacSizing:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);

              if Shape <> nil then
              begin
                TSCDeFakeShape(Shape).BeginNotifyLock;
                try
                  Self.NotifyUndo(Shape, scacSizing, '');
                  Shape.LoadBoundsFromXML(UndoNode);
                finally
                  TSCDeFakeShape(Shape).EndNotifyLock;
                end;

                Exit;
              end;
            end;
        end;
        scacInserted:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);

              if Shape <> nil then
              begin
                OwnerShape := Shape.Owner;

                if OwnerShape <> nil then
                begin
                  TSCDeFakeShape(OwnerShape).BeginNotifyLock;

                  Self.NotifyUndo(OwnerShape, scacRemoving, '');
                  Self.NotifyUndo(Shape, scacRemoved, '');
                end;

                try
                  Shape.Free;
                finally
                  if OwnerShape <> nil then
                    TSCDeFakeShape(OwnerShape).EndNotifyLock;
                end;

                Exit;
              end;
            end;
        end;
        scacRemoved:
        begin
          S := UndoNode.GetAttribute('ownerid');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]);
              if Shape.ID <> AID then Shape := Shape.ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDeContainer then
                  LoadFromDom(TSCDeContainer(Shape), UndoNode, True);

                Exit;
              end;
            end;
        end;
        scacGrouped:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDeGroup then
                begin
                  OwnerShape := Shape.Owner;

                  if OwnerShape <> nil then
                  begin
                    TSCDeFakeShape(OwnerShape).BeginNotifyLock;
                    Self.NotifyUndo(Shape, scacUngrouping, '');
                  end;

                  try
                    Self.Ungroup(TSCDeGroup(Shape));
                  finally
                    if OwnerShape <> nil then
                      TSCDeFakeShape(OwnerShape).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacPacked:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);

              if Shape <> nil then
              begin
                if Shape is TSCDePackage then
                begin
                  OwnerShape := Shape.Owner;

                  if OwnerShape <> nil then
                  begin
                    TSCDeFakeShape(OwnerShape).BeginNotifyLock;
                    Self.NotifyUndo(Shape, scacUnpacking, '');
                  end;

                  try
                    Self.Unpack(TSCDePackage(Shape));
                  finally
                    if OwnerShape <> nil then
                      TSCDeFakeShape(OwnerShape).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacUngrouping,
        scacUnpacking:
        begin
          L := TList.Create;
          try
            for I := 0 to UndoNode.ChildNodeCount-1 do
              if UndoNode.ChildNodes[I] is TSCDomElement then
              begin
                Elm := TSCDomElement(UndoNode.ChildNodes[I]);
                
                if Elm.Name = 'element' then
                begin
                  S := Elm.GetAttribute('id');
                  AID := Trunc(scdStrToFloatDef(S, 0));

                  if AID > 0 then
                    for J := 0 to LayerCount-1 do
                    begin
                      Shape := TSCDeShapeBase(Layers[J]).ShapeByID(AID);

                      if (Shape <> nil) and not Shape.InDestroy and
                        not (Shape is TSCDeLayer) then
                        L.Add(Shape);
                    end;
                end;
              end;

            if L.Count > 0 then
            begin
              SetLength(A, L.Count);
              for I := 0 to L.Count-1 do
                A[I] := TSCDeShapeBase(L[I]);

              if Action = scacUngrouping then
                Shape := Self.Group(A)
              else
                Shape := Self.Pack(A);

              if Shape <> nil then
              begin
                Shape.LoadFromXML(UndoNode);

                S := UndoNode.GetAttribute('isselected');
                if StrToIntDef(S, Integer(False)) = Integer(True) then
                  Self.Select(Shape);
              end;
            end;
          finally
            L.Free;
          end;
        end;
        scacSendingBack:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacBringingFront, '');
                    C.BringToFront(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacSendingBackward:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacBringingForward, '');
                    C.BringForward(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacBringingFront:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacSendingBack, '');
                    C.SendToBack(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
        scacBringingForward:
        begin
          S := UndoNode.GetAttribute('id');
          AID := Trunc(scdStrToFloatDef(S, 0));

          if AID > 0 then
            for I := 0 to LayerCount-1 do
            begin
              Shape := TSCDeShapeBase(Layers[I]).ShapeByID(AID);
              if Shape <> nil then
              begin
                if (Shape.Owner <> nil) and (Shape.Owner is TSCDeContainer) then
                begin
                  C := TSCDeContainer(Shape.Owner);

                  TSCDeFakeShape(C).BeginNotifyLock;
                  try
                    NotifyUndo(Shape, scacSendingBackward, '');
                    C.SendBackward(Shape);
                  finally
                    TSCDeFakeShape(C).EndNotifyLock;
                  end;
                end;

                Exit;
              end;
            end;
        end;
      end;
    end;
  end;

var
  UpCount: Boolean;
  I, Action, FirstAction: Integer;
  Root, Elm, UndoNode: TSCDomElement;
begin
  if CanUndo then
  begin
    FInUndo := True;
    try
      BeginUpdate;
      try
        Root := FUndoBuffer.Root;

        if (Root <> nil) and (Root.ChildNodeCount > 0) and
          (Root.ChildNodes[Root.ChildNodeCount-1] is TSCDomElement) then
        begin
          Elm := TSCDomElement(Root.ChildNodes[Root.ChildNodeCount-1]);

          Elm := TSCDomElement(Root.ExtractNode(Elm));
          try
            BeginRedoGroup;
            try
              if Elm.Name = 'undo' then
                ProcessUndo(Elm)
              else
              if (Elm.Name = 'undogroup') and (Elm.ChildNodeCount > 0) then
              begin
                UpCount := True;
                FirstAction := -1;
                
                for I := 0 to Elm.ChildNodeCount-1 do
                begin
                  Action := -1;

                  if Elm.ChildNodes[I] is TSCDomElement then
                  begin
                    UndoNode := TSCDomElement(Elm.ChildNodes[I]);

                    if UndoNode.Name = 'undo' then
                    begin
                      Action := StrToIntDef(UndoNode.GetAttribute('action'), -1);
                      if I = 0 then FirstAction := Action;
                    end;
                  end;

                  if (Action <> FirstAction) or
                    not (Action in [Integer(scacRemoved), Integer(scacInserted)]) then
                  begin
                    UpCount := False;
                    Break;
                  end;
                end;

                if UpCount then
                begin
                  for I := 0 to Elm.ChildNodeCount-1 do
                    if Elm.ChildNodes[I] is TSCDomElement then
                    begin
                      UndoNode := TSCDomElement(Elm.ChildNodes[I]);
                      if UndoNode.Name = 'undo' then
                        ProcessUndo(UndoNode);
                    end;
                end else
                begin
                  for I := Elm.ChildNodeCount-1 downto 0 do
                    if Elm.ChildNodes[I] is TSCDomElement then
                    begin
                      UndoNode := TSCDomElement(Elm.ChildNodes[I]);
                      if UndoNode.Name = 'undo' then
                        ProcessUndo(UndoNode);
                    end;
                end;
              end;
            finally
              EndRedoGroup;
            end;
          finally
            Elm.Free;
          end;
        end;
      finally
        EndUpdate;
      end;
    finally
      FInUndo := False;
      Change;
    end;
  end;
end;

function TSCDeCustomEditor.HasClipboardData: Boolean;
begin
  Result := FHasClipboardData;
end;

function TSCDeCustomEditor.GetClipboardText: WideString;
var
  Data: THandle;
begin
  Result := '';

  if Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    Data := Clipboard.GetAsHandle(CF_UNICODETEXT);
    if Data <> 0 then
      try
        Result := PWideChar(GlobalLock(Data));
      finally
        GlobalUnlock(Data);
      end;
  end else
  if Clipboard.HasFormat(CF_TEXT) then
    Result := Clipboard.AsText;
end;

procedure TSCDeCustomEditor.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  inherited;
  Message.Result := 0;
  if Message.Remove = FNextClipViewer then
    FNextClipViewer := Message.Next
  else
    SendMessage(FNextClipViewer, WM_CHANGECBCHAIN, Message.Remove, Message.Next);
end;

procedure TSCDeCustomEditor.WMDrawClipboard(var Message: TWMDrawClipboard);
begin
  inherited;
  try
    UpdateClipboardData;
  finally
    if not HandleAllocated or (FNextClipViewer <> Handle) then
      SendMessage(FNextClipViewer, WM_DRAWCLIPBOARD, 0, 0);
  end;
end;

procedure TSCDeCustomEditor.UpdateClipboardData;
var
  I: Integer;
  Data: WideString;
  C: TSCDeSurfaceClient;
begin
  if HandleAllocated and (ComponentState*[csDesigning,
    csDestroying, csLoading] = []) then
  begin
    Data := GetClipboardText;

    FHasClipboardData := Copy(Data, 1,
      Length('<elements type="scde.data" version="1.0"')) =
      '<elements type="scde.data" version="1.0"';

    for I := 0 to ClientCount-1 do
    begin
      C := Self.Clients[I];
      if C is TSCDeEditorClient then
        TSCDeEditorClient(C).DoClipboardChange;
    end;

    if Assigned(FOnClipboardChange) then
      FOnClipboardChange(Self);
  end;
end;

procedure TSCDeCustomEditor.Loaded;
begin
  inherited Loaded;
  InitiateClipViewer;
end;

procedure TSCDeCustomEditor.BeginUndoGroup;
var
  Root: TSCDomElement;
begin
  if not (csDestroying in ComponentState) and (FUndoBuffer <> nil) then
  begin
    Inc(FUndoGrouping);

    if FUndoGroup = nil then
    begin
      Root := FUndoBuffer.Root;

      if Root <> nil then
      begin
        FUndoGroup := TSCDomElement.Create(Root);
        FUndoGroup.Name := 'undogroup';
        Root.AddElement(FUndoGroup);
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.EndUndoGroup;
var
  Node: TSCDomElement;
begin
  if FUndoGrouping > 0 then
  begin
    Dec(FUndoGrouping);

    if FUndoGrouping = 0 then
    begin
      Node := FUndoGroup;
      FUndoGroup := nil;

      if (Node <> nil) and (Node.ChildNodeCount = 0) and
        (Node <> FUndoBuffer.Root) then
        Node.Free;
    end;
  end;
end;

procedure TSCDeCustomEditor.CreateWnd;
begin
  inherited CreateWnd;
  InitiateClipViewer;
end;

procedure TSCDeCustomEditor.InitiateClipViewer;
begin
  if FNextClipViewer = 0 then
  begin
    FClipViewerHandle := Self.Handle;
    FNextClipViewer := SetClipboardViewer(FClipViewerHandle);
  end;

  UpdateClipboardData;
end;

procedure TSCDeCustomEditor.DestroyWindowHandle;
begin
  ReleaseClipViewer;
  inherited DestroyWindowHandle;
end;

procedure TSCDeCustomEditor.WMNCDestroy(var Message: TWMNCDestroy);
begin
  ReleaseClipViewer;
  inherited;
end;

procedure TSCDeCustomEditor.ReleaseClipViewer;
begin
  if (FNextClipViewer <> 0) and (FClipViewerHandle <> 0) then
  begin
    ChangeClipboardChain(FClipViewerHandle, FNextClipViewer);
    FNextClipViewer := 0;
  end;
end;

procedure TSCDeCustomEditor.InitiateUndoBuffer;
var
  Root: TSCDomElement;
begin
  if FUndoBuffer = nil then
  begin
    FUndoBuffer := TSCDomDocument.Create(nil);
    Root := TSCDomElement.Create(FUndoBuffer);
    Root.Name := 'undobuffer';

    FUndoBuffer.AddElement(Root);
  end;
end;

procedure TSCDeCustomEditor.WMUndo(var Message: TMessage);
begin
  Message.Result := Integer(CanUndo);
  Undo;
end;

function TSCDeCustomEditor.CanRedo: Boolean;
var
  Root: TSCDomElement;
begin
  Result := False;
  if not (InLoad or FInUndo or FInRedo) and
    not (csDestroying in ComponentState) and (FRedoBuffer <> nil) then
  begin
    Root := FRedoBuffer.Root;
    Result := (Root.ElementByName('redo') <> nil) or
      (Root.ElementByName('redogroup') <> nil);
  end;
end;

function TSCDeCustomEditor.CanUndo: Boolean;
var
  Root: TSCDomElement;
begin
  Result := False;
  if not (InLoad or FInUndo or FInRedo) and
    not (csDestroying in ComponentState) and (FUndoBuffer <> nil) then
  begin
    Root := FUndoBuffer.Root;
    Result := (Root.ElementByName('undo') <> nil) or
      (Root.ElementByName('undogroup') <> nil);
  end;
end;

procedure TSCDeCustomEditor.ClearUndo;
var
  Root: TSCDomElement;
begin
  if not (FInUndo or FInRedo) then
  begin
    FUndoGroup := nil;
    FRedoGroup := nil;

    FUndoGrouping := 0;
    FRedoGrouping := 0;

    if FUndoBuffer <> nil then
    begin
      Root := FUndoBuffer.Root;
      if Root <> nil then Root.Clear;
    end;

    if FRedoBuffer <> nil then
    begin
      Root := FRedoBuffer.Root;
      if Root <> nil then Root.Clear;
    end;
  end;
end;

procedure TSCDeCustomEditor.InitiateRedoBuffer;
var
  Root: TSCDomElement;
begin
  if FRedoBuffer = nil then
  begin
    FRedoBuffer := TSCDomDocument.Create(nil);
    Root := TSCDomElement.Create(FRedoBuffer);
    Root.Name := 'redobuffer';

    FRedoBuffer.AddElement(Root);
  end;
end;

procedure TSCDeCustomEditor.NotifyUndo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);
begin
  if not InLoad and (scdoCanUndo in FEditOptions) then
  begin
    try
      if not (FInRedo or FInUndo) then
      begin
        FModified := True;
        ClearRedo;
      end;

      if FInRedo or not FInUndo then
        CreateUndo(S, Action, PropName)
      else if FInUndo then
        CreateRedo(S, Action, PropName);
    finally
      Change;
    end;
  end;
end;

procedure TSCDeCustomEditor.SetModified(Value: Boolean);
begin
  FModified := Value;
  if not FModified then ClearUndo;
end;

procedure TSCDeCustomEditor.LoadFromFile(const FileName: String);
begin
  inherited LoadFromFile(FileName);
  SetModified(False);
  Changed;
end;

procedure TSCDeCustomEditor.CreateUndo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);

  function AddProperty(Parent: TSCDomElement; const AName: WideString) : TSCDomElement;
  begin
    Result := TSCDomElement.Create(Parent);
    Result.Name := AName;
    Parent.AddElement(Result);
  end;

  procedure AddPropertyAndText(Parent: TSCDomElement; const AName, AValue: WideString);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := AddProperty(Parent, AName);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

var
  I: Integer;
  OwnerID: DWord;
  C: TSCDeContainer;
  Shape: TSCDeShapeBase;
  Node, Elm, Child: TSCDomElement;
begin
  if (scdoCanUndo in FEditOptions) and (FUndoLimit <> 0) and (FUndoLock = 0) and
    not (csDestroying in ComponentState) and (FUndoBuffer <> nil) and
    not (Action in [scacInserting, scacRemoving, scacGrouping, scacPacking]) then
  begin
    try
      Node := FUndoGroup;
      if Node = nil then Node := FUndoBuffer.Root;

      Elm := TSCDomElement.Create(Node);
      Elm.Name := 'undo';
      Node.AddElement(Elm);

      Elm.SetAttribute('class', S.ClassName);
      Elm.SetAttribute('action', IntToStr(Integer(Action)));
      Elm.SetAttribute('id', IntToStr(S.ID));
      Elm.SetAttribute('isselected', IntToStr(Integer(IsSelected(S))));

      if PropName <> '' then Elm.SetAttribute('changed_property', PropName);

      OwnerID := 0;
      if S.Owner <> nil then OwnerID := S.Owner.ID;

      Elm.SetAttribute('ownerid', IntToStr(OwnerID));

      if Action = scacSizing then
        S.SaveBoundsToXML(Elm)
      else
      if Action = scacChangingBrush then
        TSCDeFakeShape(S).Brush.SaveToXML(AddProperty(Elm, 'brush'))
      else
      if Action = scacChangingPen then
        TSCDeFakeShape(S).Pen.SaveToXML(AddProperty(Elm, 'pen'))
      else
      if Action = scacChangingFont then
        TSCDeFakeShape(S).Font.SaveToXML(AddProperty(Elm, 'font'))
      else
      if Action = scacChangingGradient then
      begin
        if TSCDeFakeShape(S).Gradient <> nil then
          TSCDeFakeShape(S).Gradient.SaveToXML(AddProperty(Elm, 'gradient'));
      end else
      if Action in [scacChanging, scacChangingPicture] then
      begin
        if PropName = 'name' then
          AddPropertyAndText(Elm, 'name', TSCDeFakeShape(S).Name)
        else
        if PropName = 'text' then
          AddPropertyAndText(Elm, 'caption', TSCDeFakeShape(S).Text)
        else
        if PropName = 'caption' then
          AddPropertyAndText(Elm, 'caption', TSCDeFakeShape(S).Caption)
        else
        if PropName = 'locked' then
          AddPropertyAndText(Elm, 'locked', IntToStr(Integer(TSCDeFakeShape(S).Locked)))
        else
        if PropName = 'visible' then
          AddPropertyAndText(Elm, 'visible', IntToStr(Integer(TSCDeFakeShape(S).Visible)))
        else
          S.SaveToXML(Elm);
      end else
      if Action = scacRemoved then
        S.SaveToXML(Elm)
      else
      if (Action in [scacUngrouping, scacUnpacking]) and
        (S is TSCDeContainer) then
      begin
        C := TSCDeContainer(S);

        for I := 0 to C.ItemCount-1 do
        begin
          Child := TSCDomElement.Create(Elm);
          Child.Name := 'element';
          Elm.AddElement(Child);

          Shape := C.Items[I];

          Child.SetAttribute('class', Shape.ClassName);
          Child.SetAttribute('id', IntToStr(Shape.ID));
        end;
      end;
    finally
      CheckUndoBufferLength;
    end;
  end;
end;

procedure TSCDeCustomEditor.BeginRedoGroup;
var
  Root: TSCDomElement;
begin
  if not (csDestroying in ComponentState) and (FRedoBuffer <> nil) then
  begin
    Inc(FRedoGrouping);

    if FRedoGroup = nil then
    begin
      Root := FRedoBuffer.Root;

      if Root <> nil then
      begin
        FRedoGroup := TSCDomElement.Create(Root);
        FRedoGroup.Name := 'redogroup';
        Root.AddElement(FRedoGroup);
      end;
    end;
  end;
end;

procedure TSCDeCustomEditor.EndRedoGroup;
var
  Node: TSCDomElement;
begin
  if FRedoGrouping > 0 then
  begin
    Dec(FRedoGrouping);

    if FRedoGrouping = 0 then
    begin
      Node := FRedoGroup;
      FRedoGroup := nil;

      if (Node <> nil) and (Node.ChildNodeCount = 0) and
        (Node <> FRedoBuffer.Root) then
        Node.Free;
    end;
  end;
end;

procedure TSCDeCustomEditor.CreateRedo(S: TSCDeShapeBase;
  Action: TSCDeAction; const PropName: String);

  function AddProperty(Parent: TSCDomElement; const AName: WideString) : TSCDomElement;
  begin
    Result := TSCDomElement.Create(Parent);
    Result.Name := AName;
    Parent.AddElement(Result);
  end;

  procedure AddPropertyAndText(Parent: TSCDomElement; const AName, AValue: WideString);
  var
    Val: TSCDomText;
    Prop: TSCDomElement;
  begin
    Prop := AddProperty(Parent, AName);

    Val := TSCDomText.Create(Prop);
    Val.Text := AValue;
    Prop.AddNode(Val);
  end;

var
  I: Integer;
  OwnerID: DWord;
  C: TSCDeContainer;
  Shape: TSCDeShapeBase;
  Node, Elm, Child: TSCDomElement;
begin
  if (scdoCanUndo in FEditOptions) and (FUndoLimit <> 0) and (FUndoLock = 0) and
    not (csDestroying in ComponentState) and (FRedoBuffer <> nil) and
    not (Action in [scacInserting, scacRemoving, scacGrouping, scacPacking]) then
  begin
    try
      Node := FRedoGroup;
      if Node = nil then Node := FRedoBuffer.Root;

      Elm := TSCDomElement.Create(Node);
      Elm.Name := 'redo';
      Node.AddElement(Elm);

      Elm.SetAttribute('class', S.ClassName);
      Elm.SetAttribute('action', IntToStr(Integer(Action)));
      Elm.SetAttribute('id', IntToStr(S.ID));
      Elm.SetAttribute('isselected', IntToStr(Integer(IsSelected(S))));

      if PropName <> '' then Elm.SetAttribute('changed_property', PropName);

      OwnerID := 0;
      if S.Owner <> nil then OwnerID := S.Owner.ID;

      Elm.SetAttribute('ownerid', IntToStr(OwnerID));

      if Action = scacSizing then
        S.SaveBoundsToXML(Elm)
      else
      if Action = scacChangingBrush then
        TSCDeFakeShape(S).Brush.SaveToXML(AddProperty(Elm, 'brush'))
      else
      if Action = scacChangingPen then
        TSCDeFakeShape(S).Pen.SaveToXML(AddProperty(Elm, 'pen'))
      else
      if Action = scacChangingFont then
        TSCDeFakeShape(S).Font.SaveToXML(AddProperty(Elm, 'font'))
      else
      if Action = scacChangingGradient then
      begin
        if TSCDeFakeShape(S).Gradient <> nil then
          TSCDeFakeShape(S).Gradient.SaveToXML(AddProperty(Elm, 'gradient'));
      end else
      if Action in [scacChanging, scacChangingPicture] then
      begin
        if PropName = 'name' then
          AddPropertyAndText(Elm, 'name', TSCDeFakeShape(S).Name)
        else
        if PropName = 'text' then
          AddPropertyAndText(Elm, 'caption', TSCDeFakeShape(S).Text)
        else
        if PropName = 'caption' then
          AddPropertyAndText(Elm, 'caption', TSCDeFakeShape(S).Caption)
        else
        if PropName = 'locked' then
          AddPropertyAndText(Elm, 'locked', IntToStr(Integer(TSCDeFakeShape(S).Locked)))
        else
        if PropName = 'visible' then
          AddPropertyAndText(Elm, 'visible', IntToStr(Integer(TSCDeFakeShape(S).Visible)))
        else
          S.SaveToXML(Elm);
      end else
      if Action = scacRemoved then
        S.SaveToXML(Elm)
      else
      if (Action in [scacUngrouping, scacUnpacking]) and
        (S is TSCDeContainer) then
      begin
        C := TSCDeContainer(S);

        for I := 0 to C.ItemCount-1 do
        begin
          Child := TSCDomElement.Create(Elm);
          Child.Name := 'element';
          Elm.AddElement(Child);

          Shape := C.Items[I];

          Child.SetAttribute('class', Shape.ClassName);
          Child.SetAttribute('id', IntToStr(Shape.ID));
        end;
      end;
    finally
      CheckUndoBufferLength;
    end;
  end;
end;

procedure TSCDeCustomEditor.ClearRedo;
var
  Root: TSCDomElement;
begin
  if not (FInUndo or FInRedo) then
  begin
    FRedoGroup := nil;
    FRedoGrouping := 0;

    if FRedoBuffer <> nil then
    begin
      Root := FRedoBuffer.Root;
      if Root <> nil then Root.Clear;
    end;
  end;
end;

procedure TSCDeCustomEditor.SetPanView;
begin
  SetEditState(scesZoomRect);
end;

function TSCDeCustomEditor.GetViewRect: TDoubleRect;
var
  CR: TRect;
  Z: Double;
  I: Integer;
begin
  LayerBounds(CR, Result);

  Z := GetZoom;

  if (FRulers <> nil) and FRulers.Visible then
  begin
    I := FRulers.Size;
    TSCDeUtils.OffsetRect(Result, -I, -I);

    Inc(CR.Left, I);
    Inc(CR.Top, I);
  end;

  TSCDeUtils.ZoomRect(Result, 1/Z);

  Result.Right := Result.Left + (CR.Right - CR.Left);
  Result.Bottom := Result.Top + (CR.Bottom - CR.Top);

  TSCDeUtils.OffsetRect(Result, -2*Result.Left, -2*Result.Top);
end;

function TSCDeCustomEditor.GetNominalViewRect: TDoubleRect;
begin
  Result := inherited GetNominalViewRect;
  if (FRulers <> nil) and FRulers.Visible and (FRulers.Size > 0) then
    TSCDeUtils.OffsetRect(Result, -FRulers.Size, -FRulers.Size);
end;

procedure TSCDeCustomEditor.SetUndoLimit(Value: Integer);
begin
  if Value < -1 then Value := -1;

  if FUndoLimit <> Value then
  begin
    FUndoLimit := Value;
    CheckUndoBufferLength;
  end;
end;

procedure TSCDeCustomEditor.CheckUndoBufferLength;
var
  Cnt: Integer;
  AChanged: Boolean;
  Root: TSCDomElement;
begin
  if FUndoLimit > -1 then
  begin
    AChanged := False;
    
    if (FUndoBuffer <> nil) and (FUndoBuffer.Root <> nil) and
      (FUndoBuffer.Root.ChildNodeCount > FUndoLimit) then
    begin
      AChanged := True;
      Root := FUndoBuffer.Root;

      if FUndoLimit = 0 then
        Root.Clear
      else begin
        Cnt := Root.ChildNodeCount - FUndoLimit;
        while Cnt > 0 do
        begin
          Root.DeleteNode(0);
          Dec(Cnt);
        end;
      end;
    end;

    if (FRedoBuffer <> nil) and (FRedoBuffer.Root <> nil) and
      (FRedoBuffer.Root.ChildNodeCount > FUndoLimit) then
    begin
      AChanged := True;
      Root := FRedoBuffer.Root;

      if FUndoLimit = 0 then
        Root.Clear
      else begin
        Cnt := Root.ChildNodeCount - FUndoLimit;
        while Cnt > 0 do
        begin
          Root.DeleteNode(0);
          Dec(Cnt);
        end;
      end;
    end;

    if AChanged then Change;
  end;
end;

function TSCDeCustomEditor.GetSelectionBounds: TDoubleRect;
var
  L: TList;
  I: Integer;
  R: TDoubleRect;
  S: TSCDeShapeBase;
begin
  Result := TSCDeUtils.Rect(0, 0, 0, 0);

  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    L := TList.Create;
    try
      for I := 0 to FSelectionList.Count-1 do
      begin
        S := TSCDeShapeBase(FSelectionList[I]);
        if S.Visible and not S.InDestroy then
          L.Add(S);
      end;

      if L.Count > 0 then
      begin
        Result := TSCDeFakeShape(L[0]).GetBounds;

        for I := 1 to L.Count-1 do
        begin
          R := TSCDeFakeShape(L[I]).GetBounds;

          if (R.Left < Result.Left) then
            Result.Left := R.Left;

          if (R.Top < Result.Top) then
            Result.Top := R.Top;

          if (R.Right > Result.Right) then
            Result.Right := R.Right;

          if (R.Bottom > Result.Bottom) then
            Result.Bottom := R.Bottom;
        end;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TSCDeCustomEditor.SetKeyMoveSizeStep(Value: Double);
begin
  if Value < 0 then Value := 0;
  if value > 100 then Value := 100;

  FKeyMoveSizeStep := Value;
end;

procedure TSCDeCustomEditor.BeginUndoLock;
begin
  Inc(FUndoLock);
end;

procedure TSCDeCustomEditor.EndUndoLock;
begin
  if (FUndoLock > 0) then
    Dec(FUndoLock);
end;

procedure TSCDeCustomEditor.EditStateChanged;
var
  I: Integer;
  C: TSCDeSurfaceClient;
begin
  DoEditStateChanged;

  for I := 0 to ClientCount - 1 do
  begin
    C := Self.Clients[I];
    if C is TSCDeEditorClient then
      TSCDeEditorClient(C).DoEditStateChange;
  end;

  if Assigned(FOnEditStateChange) then
    FOnEditStateChange(Self);
end;

procedure TSCDeCustomEditor.DoEditStateChanged;
begin
  //
end;

procedure TSCDeCustomEditor.RefineSelections;
var
  I, Cnt: Integer;
  S: TSCDeShapeBase;
begin
  if not (csDestroying in ComponentState) then
  begin
    Cnt := FSelectionList.Count;
    for I := FSelectionList.Count-1 downto 0 do
    begin
      S := TSCDeShapeBase(FSelectionList[I]);
      if not IsSelected(S) then
        FSelectionList.Delete(I);
    end;

    if Cnt <> FSelectionList.Count then
      SelectionChanged;
  end;
end;

procedure TSCDeCustomEditor.LockSelection;
var
  I: Integer;
  A: array of TSCDeShapeBase;
begin
  if (FSelectionList <> nil) and (FSelectionList.Count > 0) then
  begin
    SetLength(A, FSelectionList.Count);
    for I := 0 to FSelectionList.Count-1 do
      A[I] := TSCDeShapeBase(FSelectionList[I]);

    Self.Lock(A);
  end;
end;

procedure TSCDeCustomEditor.Lock(A: array of TSCDeShapeBase);
var
  I: Integer;
begin
  if Length(A) > 0 then
  begin
    BeginUpdate;
    try
      for I := Low(A) to High(A) do
        A[I].Locked := True;
    finally
      EndUpdate;
    end;
  end;
end;

function TSCDeCustomEditor.CanSelect(S: TSCDeShapeBase): Boolean;
begin
  Result := (scdoCanSelect in FEditOptions) and (S <> nil) and
    (S.Surface = Self) and not (S is TSCDeLayer) and
    (S.Owner <> nil) and (S.Owner is TSCDeLayer) and
    S.Visible and S.Owner.Visible and not (S.InDestroy or
    S.Locked or S.Owner.InDestroy or S.Owner.Locked) and
    Exists(S);
end;

{ TSCDeEditorClient }

procedure TSCDeEditorClient.DoClipboardChange;
begin
  if Assigned(OnClipboardChange) then
    OnClipboardChange(Sender);
end;

procedure TSCDeEditorClient.DoDefBrushChange;
begin
  if Assigned(OnDefBrushChange) then
    OnDefBrushChange(Sender);
end;

procedure TSCDeEditorClient.DoDefFontChange;
begin
  if Assigned(OnDefFontChange) then
    OnDefFontChange(Sender);
end;

procedure TSCDeEditorClient.DoDefPenChange;
begin
  if Assigned(OnDefPenChange) then
    OnDefPenChange(Sender);
end;

procedure TSCDeEditorClient.DoEditStateChange;
begin
  if Assigned(OnEditStateChange) then
    OnEditStateChange(Sender);
end;

procedure TSCDeEditorClient.DoEndNewShape(Shape: TSCDeShapeBase);
begin
  if Assigned(OnEndNewShape) then
    OnEndNewShape(Sender, Shape);
end;

procedure TSCDeEditorClient.DoGuideChange;
begin
  if Assigned(OnGuideChange) then
    OnGuideChange(Sender);
end;

procedure TSCDeEditorClient.DoNewShape(Shape: TSCDeShapeBase);
begin
  if Assigned(OnNewShape) then
    OnNewShape(Sender, Shape);
end;

procedure TSCDeEditorClient.DoNewShapeClass;
begin
  if Assigned(OnNewShapeClass) then
    OnNewShapeClass(Sender);
end;

procedure TSCDeEditorClient.DoSelectionChange;
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Sender);
end;

end.
