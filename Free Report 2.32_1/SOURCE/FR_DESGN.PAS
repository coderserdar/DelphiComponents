
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Report Designer             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Desgn;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Printers, ComCtrls,
  Menus {$IFDEF Delphi4}, ImgList {$ENDIF}, FR_Class, FR_Color,
  FR_Ctrls, FR_Dock, FR_Insp
{$IFDEF SYNTMEMO}
,  FR_SntxE
{$ELSE}
,  FR_Edit
{$ENDIF};


const
  MaxUndoBuffer = 100;
  crPencil = 11;

type
  TfrDesignerForm = class;
  TfrDesigner = class(TComponent)  // fake component
  private
    FTemplDir: String;
  public
    procedure Loaded; override;
  published
    property TemplateDir: String read FTemplDir write FTemplDir;
  end;

  TfrSelectionType = (ssBand, ssMemo, ssOther, ssMultiple, ssClipboardFull);
  TfrSelectionStatus = set of TfrSelectionType;
  TfrReportUnits = (ruPixels, ruMM, ruInches);
  TfrShapeMode = (smFrame, smAll);

  TfrUndoAction = (acInsert, acDelete, acEdit, acZOrder);
  PfrUndoObj = ^TfrUndoObj;
  TfrUndoObj = record
    Next: PfrUndoObj;
    ObjID: Integer;
    ObjPtr: TfrView;
    Int: Integer;
  end;

  TfrUndoRec = record
    Action: TfrUndoAction;
    Page: Integer;
    Objects: PfrUndoObj;
  end;

  PfrUndoRec1 = ^TfrUndoRec1;
  TfrUndoRec1 = record
    ObjPtr: TfrView;
    Int: Integer;
  end;

  PfrUndoBuffer = ^TfrUndoBuffer;
  TfrUndoBuffer = Array[0..MaxUndoBuffer - 1] of TfrUndoRec;

  TfrMenuItemInfo = class(TObject)
  private
    MenuItem: TMenuItem;
    Btn: TfrSpeedButton;
  end;
  TfrDesignerDrawMode = (dmAll, dmSelection, dmShape);

  TfrSplitInfo = record
    SplRect: TRect;
    SplX: Integer;
    View1, View2: TfrView;
  end;

  TfrDesignerPage = class(TPanel)
  private
    Down,                          // mouse button was pressed
    Moved,                         // mouse was moved (with pressed btn)
    DFlag,                         // was double click
    RFlag: Boolean;                // selecting objects by framing
    Mode: (mdInsert, mdSelect);    // current mode
    CT: (ctNone, ct1, ct2, ct3, ct4, ct5, ct6, ct7, ct8);  // cursor type
    LastX, LastY: Integer;         // here stored last mouse coords
    SplitInfo: TfrSplitInfo;
    RightBottom: Integer;
    LeftTop: TPoint;
    FirstBandMove: Boolean;
    FDesigner: TfrDesignerForm;
    procedure NormalizeRect(var r: TRect);
    procedure NormalizeCoord(t: TfrView);
    function FindNearestEdge(var x, y: Integer): Boolean;
    procedure RoundCoord(var x, y: Integer);
    procedure Draw(N: Integer; ClipRgn: HRGN);
    procedure DrawPage(DrawMode: TfrDesignerDrawMode);
    procedure DrawRectLine(Rect: TRect);
    procedure DrawFocusRect(Rect: TRect);
    procedure DrawHSplitter(Rect: TRect);
    procedure DrawSelection(t: TfrView);
    procedure DrawShape(t: TfrView);
    procedure MDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DClick(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init;
    procedure SetPage;
    procedure GetMultipleSelected;
  end;

  TfrDesignerForm = class(TfrReportDesigner)
    StatusBar1: TStatusBar;
    frDock1: TfrDock;
    frDock2: TfrDock;
    frDock3: TfrDock;
    Popup1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    ToolMenu: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N26: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N36: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    Pan5: TMenuItem;
    N8: TMenuItem;
    ImageList2: TImageList;
    N38: TMenuItem;
    Pan6: TMenuItem;
    N39: TMenuItem;
    N40: TMenuItem;
    N42: TMenuItem;
    MastMenu: TMenuItem;
    N16: TMenuItem;
    Panel2: TfrToolBar;
    FileBtn1: TfrTBButton;
    FileBtn2: TfrTBButton;
    FileBtn3: TfrTBButton;
    FileBtn4: TfrTBButton;
    CutB: TfrTBButton;
    CopyB: TfrTBButton;
    PstB: TfrTBButton;
    ZB1: TfrTBButton;
    ZB2: TfrTBButton;
    SelAllB: TfrTBButton;
    PgB1: TfrTBButton;
    PgB2: TfrTBButton;
    PgB3: TfrTBButton;
    GB1: TfrTBButton;
    GB2: TfrTBButton;
    ExitB: TfrTBButton;
    Panel3: TfrToolBar;
    AlB1: TfrTBButton;
    AlB2: TfrTBButton;
    AlB3: TfrTBButton;
    AlB4: TfrTBButton;
    AlB5: TfrTBButton;
    FnB1: TfrTBButton;
    FnB2: TfrTBButton;
    FnB3: TfrTBButton;
    ClB2: TfrTBButton;
    HlB1: TfrTBButton;
    AlB6: TfrTBButton;
    AlB7: TfrTBButton;
    Panel1: TfrToolBar;
    FrB1: TfrTBButton;
    FrB2: TfrTBButton;
    FrB3: TfrTBButton;
    FrB4: TfrTBButton;
    ClB1: TfrTBButton;
    ClB3: TfrTBButton;
    FrB5: TfrTBButton;
    FrB6: TfrTBButton;
    frTBSeparator1: TfrTBSeparator;
    frTBSeparator2: TfrTBSeparator;
    frTBSeparator3: TfrTBSeparator;
    frTBSeparator4: TfrTBSeparator;
    frTBSeparator5: TfrTBSeparator;
    frTBPanel1: TfrTBPanel;
    C3: TComboBox;
    C2: TComboBox;
    frTBPanel2: TfrTBPanel;
    frTBSeparator6: TfrTBSeparator;
    frTBSeparator7: TfrTBSeparator;
    frTBSeparator8: TfrTBSeparator;
    frTBSeparator9: TfrTBSeparator;
    frTBSeparator10: TfrTBSeparator;
    N37: TMenuItem;
    Pan2: TMenuItem;
    Pan3: TMenuItem;
    Pan1: TMenuItem;
    Pan4: TMenuItem;
    Panel4: TfrToolBar;
    OB1: TfrTBButton;
    OB2: TfrTBButton;
    OB3: TfrTBButton;
    OB4: TfrTBButton;
    OB5: TfrTBButton;
    frTBSeparator12: TfrTBSeparator;
    Panel5: TfrToolBar;
    Align1: TfrTBButton;
    Align2: TfrTBButton;
    Align3: TfrTBButton;
    Align4: TfrTBButton;
    Align5: TfrTBButton;
    Align6: TfrTBButton;
    Align7: TfrTBButton;
    Align8: TfrTBButton;
    Align9: TfrTBButton;
    Align10: TfrTBButton;
    frTBSeparator13: TfrTBSeparator;
    Tab1: TTabControl;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    frDock4: TfrDock;
    HelpMenu: TMenuItem;
    N34: TMenuItem;
    GB3: TfrTBButton;
    N46: TMenuItem;
    N47: TMenuItem;
    UndoB: TfrTBButton;
    frTBSeparator14: TfrTBSeparator;
    AlB8: TfrTBButton;
    RedoB: TfrTBButton;
    N48: TMenuItem;
    OB6: TfrTBButton;
    frTBSeparator15: TfrTBSeparator;
    Panel6: TfrToolBar;
    Pan7: TMenuItem;
    Image2: TImage;
    N14: TMenuItem;
    Panel7: TPanel;
    PBox1: TPaintBox;
    N17: TMenuItem;
    E1: TEdit;
    Panel8: TPanel;
    SB1: TfrSpeedButton;
    SB2: TfrSpeedButton;
    HelpBtn: TfrTBButton;
    frTBSeparator11: TfrTBSeparator;
    N18: TMenuItem;
    N22: TMenuItem;
    N35: TMenuItem;
    Popup2: TPopupMenu;
    N41: TMenuItem;
    N43: TMenuItem;
    N44: TMenuItem;
    LinePanel: TPanel;
    frSpeedButton1: TfrSpeedButton;
    frSpeedButton2: TfrSpeedButton;
    frSpeedButton3: TfrSpeedButton;
    frSpeedButton4: TfrSpeedButton;
    frSpeedButton5: TfrSpeedButton;
    StB1: TfrTBButton;
    frSpeedButton6: TfrSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure DoClick(Sender: TObject);
    procedure ClB1Click(Sender: TObject);
    procedure GB1Click(Sender: TObject);
    procedure ZB1Click(Sender: TObject);
    procedure ZB2Click(Sender: TObject);
    procedure PgB1Click(Sender: TObject);
    procedure PgB2Click(Sender: TObject);
    procedure OB2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OB1Click(Sender: TObject);
    procedure CutBClick(Sender: TObject);
    procedure CopyBClick(Sender: TObject);
    procedure PstBClick(Sender: TObject);
    procedure SelAllBClick(Sender: TObject);
    procedure ExitBClick(Sender: TObject);
    procedure PgB3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure GB2Click(Sender: TObject);
    procedure FileBtn1Click(Sender: TObject);
    procedure FileBtn2Click(Sender: TObject);
    procedure FileBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure N8Click(Sender: TObject);
    procedure C2DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure HlB1Click(Sender: TObject);
    procedure FileBtn4Click(Sender: TObject);
    procedure N42Click(Sender: TObject);
    procedure Popup1Popup(Sender: TObject);
    procedure N23Click(Sender: TObject);
    procedure N37Click(Sender: TObject);
    procedure Pan2Click(Sender: TObject);
    procedure N14Click(Sender: TObject);
    procedure Align1Click(Sender: TObject);
    procedure Align2Click(Sender: TObject);
    procedure Align3Click(Sender: TObject);
    procedure Align4Click(Sender: TObject);
    procedure Align5Click(Sender: TObject);
    procedure Align6Click(Sender: TObject);
    procedure Align7Click(Sender: TObject);
    procedure Align8Click(Sender: TObject);
    procedure Align9Click(Sender: TObject);
    procedure Align10Click(Sender: TObject);
    procedure Tab1Change(Sender: TObject);
    procedure N34Click(Sender: TObject);
    procedure GB3Click(Sender: TObject);
    procedure UndoBClick(Sender: TObject);
    procedure RedoBClick(Sender: TObject);
    procedure N20Click(Sender: TObject);
    procedure PBox1Paint(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure N22Click(Sender: TObject);
    procedure Tab1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frSpeedButton1Click(Sender: TObject);
    procedure StB1Click(Sender: TObject);
  private
    { Private declarations }
    PageView: TfrDesignerPage;
    InspForm: TfrInspForm;
    EditorForm: TfrEditorForm;
    BPanel, RPanel: TPanel;
    ColorSelector: TColorSelector;
    MenuItems: TList;
    ItemWidths: TStringList;
    FCurPage: Integer;
    FGridSize: Integer;
    FGridShow, FGridAlign: Boolean;
    FUnits: TfrReportUnits;
    FGrayedButtons: Boolean;
    FUndoBuffer, FRedoBuffer: TfrUndoBuffer;
    FUndoBufferLength, FRedoBufferLength: Integer;
    FirstTime: Boolean;
    MaxItemWidth, MaxShortCutWidth: Integer;
    fld: Array[0..6] of String[100];
    FirstInstance: Boolean;
    EditAfterInsert: Boolean;
    FCurDocName, FCaption: String;
    FileModified: Boolean;
    ShapeMode: TfrShapeMode;
    procedure GetFontList;
    procedure SetMenuBitmaps;
    procedure SetCurPage(Value: Integer);
    procedure SetGridSize(Value: Integer);
    procedure SetGridShow(Value: Boolean);
    procedure SetGridAlign(Value: Boolean);
    procedure SetUnits(Value: TfrReportUnits);
    procedure SetGrayedButtons(Value: Boolean);
    procedure SetCurDocName(Value: String);
    procedure SelectionChanged;
    procedure ShowPosition;
    procedure ShowContent;
    procedure EnableControls;
    procedure ResetSelection;
    procedure DeleteObjects;
    procedure AddPage;
    procedure RemovePage(n: Integer);
    procedure SetPageTitles;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure FillInspFields;
    function RectTypEnabled: Boolean;
    function FontTypEnabled: Boolean;
    function ZEnabled: Boolean;
    function CutEnabled: Boolean;
    function CopyEnabled: Boolean;
    function PasteEnabled: Boolean;
    function DelEnabled: Boolean;
    function EditEnabled: Boolean;
    procedure ColorSelected(Sender: TObject);
    procedure MoveObjects(dx, dy: Integer; Resize: Boolean);
    procedure SelectAll;
    procedure Unselect;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure SaveState;
    procedure RestoreState;
    procedure ClearBuffer(Buffer: TfrUndoBuffer; var BufferLength: Integer);
    procedure ClearUndoBuffer;
    procedure ClearRedoBuffer;
    procedure Undo(Buffer: PfrUndoBuffer);
    procedure ReleaseAction(ActionRec: TfrUndoRec);
    procedure AddAction(Buffer: PfrUndoBuffer; a: TfrUndoAction; List: TList);
    procedure AddUndoAction(a: TfrUndoAction);
    procedure DoDrawText(Canvas: TCanvas; Caption: string;
      Rect: TRect; Selected, Enabled: Boolean; Flags: Longint);
    procedure MeasureItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure DrawItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure InsFieldsClick(Sender: TObject);
    function FindMenuItem(AMenuItem: TMenuItem): TfrMenuItemInfo;
    procedure SetMenuItemBitmap(AMenuItem: TMenuItem; ABtn:TfrSpeedButton);
    procedure FillMenuItems(MenuItem: TMenuItem);
    procedure DeleteMenuItems(MenuItem: TMenuItem);
    procedure OnActivateApp(Sender: TObject);
    procedure OnDeactivateApp(Sender: TObject);
    procedure GetDefaultSize(var dx, dy: Integer);
    function SelStatus: TfrSelectionStatus;
  public
    { Public declarations }
    procedure WndProc(var Message: TMessage); override;
    procedure RegisterObject(ButtonBmp: TBitmap; const ButtonHint: String;
      ButtonTag: Integer); override;
    procedure RegisterTool(MenuCaption: String; ButtonBmp: TBitmap;
      OnClick: TNotifyEvent); override;
    procedure BeforeChange; override;
    procedure AfterChange; override;
    procedure ShowMemoEditor;
    procedure ShowEditor;
    procedure RedrawPage; override;
    procedure OnModify(Item: Integer; var EditText: String);
    function PointsToUnits(x: Integer): Double;
    function UnitsToPoints(x: Double): Integer;
    property CurDocName: String read FCurDocName write SetCurDocName;
    property CurPage: Integer read FCurPage write SetCurPage;
    property GridSize: Integer read FGridSize write SetGridSize;
    property ShowGrid: Boolean read FGridShow write SetGridShow;
    property GridAlign: Boolean read FGridAlign write SetGridAlign;
    property Units: TfrReportUnits read FUnits write SetUnits;
    property GrayedButtons: Boolean read FGrayedButtons write SetGrayedButtons;
  end;

procedure frSetGlyph(Color: TColor; sb: TfrSpeedButton; n: Integer);
function frCheckBand(b: TfrBandType): Boolean;

var
  frTemplateDir: String;


implementation

{$R *.DFM}
{$R *.RES}
{$R FR_Lng2.RES}

uses
  FR_Pgopt, FR_GEdit, FR_Templ, FR_Newrp, FR_DsOpt, FR_Const,
  FR_Prntr, FR_Hilit, FR_Flds, FR_Dopt, FR_Ev_ed, FR_BndEd, FR_VBnd,
  FR_BTyp, FR_Utils, FR_GrpEd, FR_About, FR_IFlds, FR_Pars, FR_DBRel,
  FR_DBSet, Registry
{$IFNDEF IBO}
  , DB
{$ENDIF};

type
  THackView = class(TfrView)
  end;

function GetUnusedBand: TfrBandType; forward;
procedure SendBandsToDown; forward;
procedure ClearClipBoard; forward;
function Objects: TList; forward;
procedure GetRegion; forward;
function TopSelected: Integer; forward;

var
  FirstInst: Boolean = True;
  FirstSelected: TfrView;
  SelNum: Integer;               // number of objects currently selected
  MRFlag,                        // several objects was selected
  ObjRepeat,                     // was pressed Shift + Insert Object
  WasOk: Boolean;                // was Ok pressed in dialog
  OldRect, OldRect1: TRect;      // object rect after mouse was clicked
  Busy: Boolean;                 // busy flag. need!
  ShowSizes: Boolean;
  LastFontName: String;
  LastFontSize, LastAdjust: Integer;
  LastFrameWidth, LastLineWidth: Single;
  LastFrameTyp, LastFontStyle: Word;
  LastFrameColor, LastFillColor, LastFontColor: TColor;
  ClrButton: TfrSpeedButton;
  FirstChange: Boolean;
  ClipRgn: HRGN;

// globals
  ClipBd: TList;                 // clipboard
  GridBitmap: TBitmap;           // for drawing grid in design time


{----------------------------------------------------------------------------}
procedure TfrDesigner.Loaded;
begin
  inherited Loaded;
  frTemplateDir := TemplateDir;
end;

{--------------------------------------------------}
constructor TfrDesignerPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Color  := clWhite;
  BorderStyle := bsNone;
  OnMouseDown := MDown;
  OnMouseUp := MUp;
  OnMouseMove := MMove;
  OnDblClick := DClick;
end;

procedure TfrDesignerPage.Init;
begin
  Down := False; DFlag := False; RFlag := False;
  Cursor := crDefault; CT := ctNone;
end;

procedure TfrDesignerPage.SetPage;
var
  Pgw,Pgh: Integer;
begin
  Pgw := FDesigner.Page.PrnInfo.Pgw;
  Pgh := FDesigner.Page.PrnInfo.Pgh;
  if Pgw > Parent.Width then
    SetBounds(10, 10, Pgw, Pgh) else
    SetBounds((Parent.Width - Pgw) div 2, 10, Pgw, Pgh);
  FDesigner.BPanel.Top := Top + Height + 10;
  FDesigner.RPanel.Left := Left + Width + 10;
end;

procedure TfrDesignerPage.Paint;
begin
  Draw(10000, 0);
end;

procedure TfrDesignerPage.NormalizeCoord(t: TfrView);
begin
  if t.dx < 0 then
  begin
    t.dx := -t.dx;
    t.x := t.x - t.dx;
  end;
  if t.dy < 0 then
  begin
    t.dy := -t.dy;
    t.y := t.y - t.dy;
  end;
end;

procedure TfrDesignerPage.NormalizeRect(var r: TRect);
var
  i: Integer;
begin
  with r do
  begin
    if Left > Right then begin i := Left; Left := Right; Right := i end;
    if Top > Bottom then begin i := Top; Top := Bottom; Bottom := i end;
  end;
end;

procedure TfrDesignerPage.DrawHSplitter(Rect: TRect);
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;
    Pen.Color := clSilver;
    Pen.Width := 1;
    MoveTo(Rect.Left, Rect.Top);
    LineTo(Rect.Right, Rect.Bottom);
    Pen.Mode := pmCopy;
  end;
end;

procedure TfrDesignerPage.DrawRectLine(Rect: TRect);
begin
  with Canvas do
  begin
    Pen.Mode := pmNot;
    Pen.Style := psSolid;
    Pen.Width := Round(LastLineWidth);
    with Rect do
      if Abs(Right - Left) > Abs(Bottom - Top) then
      begin
        MoveTo(Left, Top);
        LineTo(Right, Top);
      end
      else
      begin
        MoveTo(Left, Top);
        LineTo(Left, Bottom);
      end;
    Pen.Mode := pmCopy;
  end;
end;

procedure TfrDesignerPage.DrawFocusRect(Rect: TRect);
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;
    Pen.Color := clSilver;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    if (Rect.Right = Rect.Left + 1) or (Rect.Bottom = Rect.Top + 1) then
    begin
      if Rect.Right = Rect.Left + 1 then
        Dec(Rect.Right, 1) else
        Dec(Rect.Bottom, 1);
      MoveTo(Rect.Left, Rect.Top);
      LineTo(Rect.Right, Rect.Bottom);
    end
    else
      Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    Pen.Mode := pmCopy;
    Brush.Style := bsSolid;
  end;
end;

procedure TfrDesignerPage.DrawSelection(t: TfrView);
var
  px, py: Word;
  procedure DrawPoint(x, y: Word);
  begin
    Canvas.MoveTo(x, y);
    Canvas.LineTo(x, y);
  end;
begin
  if t.Selected then
  with t, Canvas do
  begin
    Pen.Width := 5;
    Pen.Mode := pmXor;
    Pen.Color := clWhite;
    px := x + dx div 2;
    py := y + dy div 2;
    DrawPoint(x, y); DrawPoint(x + dx, y);
    DrawPoint(x, y + dy);
    if Objects.IndexOf(t) = RightBottom then
      Pen.Color := clTeal;
    DrawPoint(x + dx, y + dy);
    Pen.Color := clWhite;
    if SelNum = 1 then
    begin
      DrawPoint(px, y); DrawPoint(px, y + dy);
      DrawPoint(x, py); DrawPoint(x + dx, py);
    end;
    Pen.Mode := pmCopy;
  end;
end;

procedure TfrDesignerPage.DrawShape(t: TfrView);
begin
  if t.Selected then
  with t do
    DrawFocusRect(Rect(x, y, x + dx + 1, y + dy + 1))
end;

procedure TfrDesignerPage.Draw(N: Integer; ClipRgn: HRGN);
var
  i: Integer;
  t: TfrView;
  R, R1: HRGN;
  Objects: TList;
  procedure DrawBackground;
  var
    i, j: Integer;
  begin
    with Canvas do
    begin
      if FDesigner.ShowGrid and (FDesigner.GridSize <> 18) then
      begin
        with GridBitmap.Canvas do
        begin
          Brush.Color := clWhite;
          FillRect(Rect(0, 0, 8, 8));
          Pixels[0, 0] := clBlack;
          if FDesigner.GridSize = 4 then
          begin
            Pixels[4, 0] := clBlack;
            Pixels[0, 4] := clBlack;
            Pixels[4, 4] := clBlack;
          end;
        end;
        Brush.Bitmap := GridBitmap;
      end
      else
      begin
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
      end;
      FillRgn(Handle, R, Brush.Handle);
      if FDesigner.ShowGrid and (FDesigner.GridSize = 18) then
      begin
        i := 0;
        while i < Width do
        begin
          j := 0;
          while j < Height do
          begin
            if RectVisible(Handle, Rect(i, j, i + 1, j + 1)) then
              SetPixel(Handle, i, j, clBlack);
            Inc(j, FDesigner.GridSize);
          end;
          Inc(i, FDesigner.GridSize);
        end;
      end;
      Brush.Style := bsClear;
      Pen.Width := 1;
      Pen.Color := clGray;
      Pen.Style := psSolid;
      Pen.Mode := pmCopy;
      with FDesigner.Page do
      begin
        if UseMargins then
          Rectangle(LeftMargin, TopMargin, RightMargin, BottomMargin);
        if ColCount > 1 then
        begin
          ColWidth := (RightMargin - LeftMargin) div ColCount;
          Pen.Style := psDot;
          j := LeftMargin;
          for i := 1 to ColCount do
          begin
            Rectangle(j, -1, j + ColWidth + 1,  PrnInfo.Pgh + 1);
            Inc(j, ColWidth + ColGap);
          end;
          Pen.Style := psSolid;
        end;
      end;
    end;
  end;
  function IsVisible(t: TfrView): Boolean;
  var
    R: HRGN;
  begin
    R := t.GetClipRgn(rtNormal);
    Result := CombineRgn(R, R, ClipRgn, RGN_AND) <> NULLREGION;
    DeleteObject(R);
  end;

begin
  if FDesigner.Page = nil then Exit;
  DocMode := dmDesigning;
  Objects := FDesigner.Page.Objects;
  if ClipRgn = 0 then
    with Canvas.ClipRect do
      ClipRgn := CreateRectRgn(Left, Top, Right, Bottom);
  R := CreateRectRgn(0, 0, Width, Height);
  for i := Objects.Count - 1 downto 0 do
  begin
    t := Objects[i];
    if i <= N then
      if t.Selected then
        t.Draw(Canvas)
      else if IsVisible(t) then
      begin
        R1 := CreateRectRgn(0, 0, 1, 1);
        CombineRgn(R1, ClipRgn, R, RGN_AND);
        SelectClipRgn(Canvas.Handle, R1);
        DeleteObject(R1);
        t.Draw(Canvas);
      end;
    R1 := t.GetClipRgn(rtNormal);
    CombineRgn(R, R, R1, RGN_DIFF);
    DeleteObject(R1);
    SelectClipRgn(Canvas.Handle, R);
  end;
  CombineRgn(R, R, ClipRgn, RGN_AND);
  DrawBackground;

  DeleteObject(R);
  DeleteObject(ClipRgn);
  SelectClipRgn(Canvas.Handle, 0);
  if not Down then
    DrawPage(dmSelection);
end;

procedure TfrDesignerPage.DrawPage(DrawMode: TfrDesignerDrawMode);
var
  i: Integer;
  t: TfrView;
begin
  if DocMode <> dmDesigning then Exit;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    case DrawMode of
      dmAll: t.Draw(Canvas);
      dmSelection: DrawSelection(t);
      dmShape: DrawShape(t);
    end;
  end;
end;

function TfrDesignerPage.FindNearestEdge(var x, y: Integer): Boolean;
var
  i: Integer;
  t: TfrView;
  min: Double;
  p: TPoint;
  function DoMin(a: Array of TPoint): Boolean;
  var
    i: Integer;
    d: Double;
  begin
    Result := False;
    for i := Low(a) to High(a) do
    begin
      d := sqrt((x - a[i].x) * (x - a[i].x) + (y - a[i].y) * (y - a[i].y));
      if d < min then
      begin
        min := d;
        p := a[i];
        Result := True;
      end;
    end;
  end;
begin
  Result := False;
  min := FDesigner.GridSize;
  p := Point(x, y);
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if DoMin([Point(t.x, t.y), Point(t.x + t.dx, t.y),
         Point(t.x + t.dx, t.y + t.dy),  Point(t.x, t.y + t.dy)]) then
      Result := True;
  end;
  x := p.x; y := p.y;
end;

procedure TfrDesignerPage.RoundCoord(var x, y: Integer);
begin
  with FDesigner do
    if GridAlign then
    begin
      x := x div GridSize * GridSize;
      y := y div GridSize * GridSize;
    end;
end;

procedure TfrDesignerPage.GetMultipleSelected;
var
  i, j, k: Integer;
  t: TfrView;
begin
  j := 0; k := 0;
  LeftTop := Point(10000, 10000);
  RightBottom := -1;
  MRFlag := False;
  if SelNum > 1 then                  {find right-bottom element}
  begin
    for i := 0 to Objects.Count-1 do
    begin
      t := Objects[i];
      if t.Selected then
      begin
        t.OriginalRect := Rect(t.x, t.y, t.dx, t.dy);
        if (t.x + t.dx > j) or ((t.x + t.dx = j) and (t.y + t.dy > k)) then
        begin
          j := t.x + t.dx;
          k := t.y + t.dy;
          RightBottom := i;
        end;
        if t.x < LeftTop.x then LeftTop.x := t.x;
        if t.y < LeftTop.y then LeftTop.y := t.y;
      end;
    end;
    t := Objects[RightBottom];
    OldRect := Rect(LeftTop.x, LeftTop.y, t.x + t.dx, t.y + t.dy);
    OldRect1 := OldRect;
    MRFlag := True;
  end;
end;

procedure TfrDesignerPage.MDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  f, DontChange, v: Boolean;
  t: TfrView;
  Rgn: HRGN;
  p: TPoint;
begin
  if DFlag then
  begin
    DFlag := False;
    Exit;
  end;
  if (Button = mbRight) and Down and RFlag then
    DrawFocusRect(OldRect);
  RFlag := False;
  DrawPage(dmSelection);
  Down := True;
  DontChange := False;
  if Button = mbLeft then
    if (ssCtrl in Shift) or (Cursor = crCross) then
    begin
      RFlag := True;
      if Cursor = crCross then
      begin
        DrawFocusRect(OldRect);
        RoundCoord(x, y);
        OldRect1 := OldRect;
      end;
      OldRect := Rect(x, y, x, y);
      FDesigner.Unselect;
      SelNum := 0;
      RightBottom := -1;
      MRFlag := False;
      FirstSelected := nil;
      Exit;
    end
    else if Cursor = crPencil then
    begin
      with FDesigner do
      if GridAlign then
        if not FindNearestEdge(x, y) then
        begin
          x := Round(x / GridSize) * GridSize;
          y := Round(y / GridSize) * GridSize;
        end;
      OldRect := Rect(x, y, x, y);
      FDesigner.Unselect;
      SelNum := 0;
      RightBottom := -1;
      MRFlag := False;
      FirstSelected := nil;
      LastX := x;
      LastY := y;
      Exit;
    end;
  if Cursor = crDefault then
  begin
    f := False;
    for i := Objects.Count - 1 downto 0 do
    begin
      t := Objects[i];
      Rgn := t.GetClipRgn(rtNormal);
      v := PtInRegion(Rgn, X, Y);
      DeleteObject(Rgn);
      if v then
      begin
        if ssShift in Shift then
        begin
          t.Selected := not t.Selected;
          if t.Selected then Inc(SelNum) else Dec(SelNum);
        end
        else
        begin
          if not t.Selected then
          begin
            FDesigner.Unselect;
            SelNum := 1;
            t.Selected := True;
          end
          else DontChange := True;
        end;
        if SelNum = 0 then FirstSelected := nil
        else if SelNum = 1 then FirstSelected := t
        else if FirstSelected <> nil then
          if not FirstSelected.Selected then FirstSelected := nil;
        f := True;
        break;
      end;
    end;
    if not f then
    begin
      FDesigner.Unselect;
      SelNum := 0;
      FirstSelected := nil;
      if Button = mbLeft then
      begin
        RFlag := True;
        OldRect := Rect(x, y, x, y);
        Exit;
      end;
    end;
    GetMultipleSelected;
    if not DontChange then FDesigner.SelectionChanged;
  end;
  if SelNum = 0 then
  begin // reset multiple selection
    RightBottom := -1;
    MRFlag := False;
  end;
  LastX := x;
  LastY := y;
  Moved := False;
  FirstChange := True;
  FirstBandMove := True;
  if Button = mbRight then
  begin
    DrawPage(dmSelection);
    Down := False;
    GetCursorPos(p);
    FDesigner.Popup1Popup(nil);
    TrackPopupMenu(FDesigner.Popup1.Handle,
      TPM_LEFTALIGN or TPM_RIGHTBUTTON, p.X, p.Y, 0, FDesigner.Handle, nil);
  end
  else if FDesigner.ShapeMode = smFrame then
    DrawPage(dmShape);
end;

procedure TfrDesignerPage.MUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i, k, dx, dy: Integer;
  t: TfrView;
  ObjectInserted: Boolean;
  procedure AddObject(ot: Byte);
  begin
    Objects.Add(frCreateObject(ot, ''));
    t := Objects.Last;
  end;
  procedure CreateSection;
  var
    s: String;
  begin
    frBandTypesForm := TfrBandTypesForm.Create(nil);
    ObjectInserted := frBandTypesForm.ShowModal = mrOk;
    if ObjectInserted then
    begin
      Objects.Add(TfrBandView.Create);
      t := Objects.Last;
      (t as TfrBandView).BandType := frBandTypesForm.SelectedTyp;
      s := frBandNames[Integer(frBandTypesForm.SelectedTyp)];
      if Pos(' ', s) <> 0 then
      begin
        s[Pos(' ', s) + 1] := UpCase(s[Pos(' ', s) + 1]);
        Delete(s, Pos(' ', s), 1);
      end;
      THackView(t).BaseName := s;
      SendBandsToDown;
    end;
    frBandTypesForm.Free;
  end;
  procedure CreateSubReport;
  begin
    Objects.Add(TfrSubReportView.Create);
    t := Objects.Last;
    (t as TfrSubReportView).SubPage := CurReport.Pages.Count;
    CurReport.Pages.Add;
  end;

begin
  if Button <> mbLeft then Exit;
  Down := False;
  if FDesigner.ShapeMode = smFrame then
    DrawPage(dmShape);
// inserting a new object
  if Cursor = crCross then
  begin
    Mode := mdSelect;
    DrawFocusRect(OldRect);
    if (OldRect.Left = OldRect.Right) and (OldRect.Top = OldRect.Bottom) then
      OldRect := OldRect1;
    NormalizeRect(OldRect);
    RFlag := False;
    ObjectInserted := True;
    with FDesigner.Panel4 do
    for i := 0 to ControlCount - 1 do
      if Controls[i] is TfrSpeedButton then
      with Controls[i] as TfrSpeedButton do
        if Down then
        begin
          if Tag = gtBand then
            if GetUnusedBand <> btNone then
              CreateSection else
              Exit
          else if Tag = gtSubReport then
            CreateSubReport
          else if Tag >= gtAddIn then
          begin
            k := Tag - gtAddIn;
            Objects.Add(frCreateObject(gtAddIn, frAddIns[k].ClassRef.ClassName));
            t := Objects.Last;
          end
          else
            AddObject(Tag);
          break;
        end;
    if ObjectInserted then
    begin
      t.CreateUniqueName;
      with OldRect do
        if (Left = Right) or (Top = Bottom) then
        begin
          dx := 40; dy := 40;
          if t is TfrMemoView then
            FDesigner.GetDefaultSize(dx, dy);
          OldRect := Rect(Left, Top, Left + dx, Top + dy);
        end;
      FDesigner.Unselect;
      t.x := OldRect.Left; t.y := OldRect.Top;
      t.dx := OldRect.Right - OldRect.Left; t.dy := OldRect.Bottom - OldRect.Top;
      if (t is TfrBandView) and
         (TfrBandType(t.FrameTyp) in [btCrossHeader..btCrossFooter]) and
         (t.dx > Width - 10) then
         t.dx := 40;
      t.FrameWidth := LastFrameWidth;
      t.FrameColor := LastFrameColor;
      t.FillColor := LastFillColor;
      t.Selected := True;
      if t.Typ <> gtBand then
        t.FrameTyp := LastFrameTyp;
      if t is TfrMemoView then
        with t as TfrMemoView do
        begin
          Font.Name := LastFontName;
          Font.Size := LastFontSize;
          Font.Color := LastFontColor;
          Font.Style := frSetFontStyle(LastFontStyle);
          Adjust := LastAdjust;
        end;
      SelNum := 1;
      if t.Typ = gtBand then
        Draw(10000, t.GetClipRgn(rtExtended))
      else
      begin
        t.Draw(Canvas);
        DrawSelection(t);
      end;
      with FDesigner do
      begin
        SelectionChanged;
        AddUndoAction(acInsert);
        if EditAfterInsert then
          ShowEditor;
      end;
    end;
    if not ObjRepeat then
      FDesigner.OB1.Down := True else
      DrawFocusRect(OldRect);
    Exit;
  end;
// line drawing
  if Cursor = crPencil then
  begin
    DrawRectLine(OldRect);
    AddObject(gtLine);
    t.CreateUniqueName;
    t.x := OldRect.Left; t.y := OldRect.Top;
    t.dx := OldRect.Right - OldRect.Left; t.dy := OldRect.Bottom - OldRect.Top;
    if t.dx < 0 then
    begin
      t.dx := -t.dx; if Abs(t.dx) > Abs(t.dy) then t.x := OldRect.Right;
    end;
    if t.dy < 0 then
    begin
      t.dy := -t.dy; if Abs(t.dy) > Abs(t.dx) then t.y := OldRect.Bottom;
    end;
    t.Selected := True;
    t.FrameWidth := LastLineWidth;
    t.FrameColor := LastFrameColor;
    SelNum := 1;
    t.Draw(Canvas);
    DrawSelection(t);
    FDesigner.SelectionChanged;
    FDesigner.AddUndoAction(acInsert);
    Exit;
  end;

// calculating which objects contains in frame (if user select it with mouse+Ctrl key)
  if RFlag then
  begin
    DrawFocusRect(OldRect);
    RFlag := False;
    NormalizeRect(OldRect);
    for i := 0 to Objects.Count - 1 do
    begin
      t := Objects[i];
      with OldRect do
      if t.Typ <> gtBand then
        if not ((t.x > Right) or (t.x + t.dx < Left) or
                (t.y > Bottom) or (t.y + t.dy < Top)) then
        begin
          t.Selected := True;
          Inc(SelNum);
        end;
    end;
    GetMultipleSelected;
    FDesigner.SelectionChanged;
    DrawPage(dmSelection);
    Exit;
  end;
// splitting
  if Moved and MRFlag and (Cursor = crHSplit) then
  begin
    with SplitInfo do
    begin
      dx := SplRect.Left - SplX;
      if (View1.dx + dx > 0) and (View2.dx - dx > 0) then
      begin
        Inc(View1.dx, dx);
        Inc(View2.x, dx);
        Dec(View2.dx, dx);
      end;
    end;
    GetMultipleSelected;
    Draw(TopSelected, ClipRgn);
    Exit;
  end;
// resizing several objects
  if Moved and MRFlag and (Cursor <> crDefault) then
  begin
    Draw(TopSelected, ClipRgn);
    Exit;
  end;
// redrawing all moved or resized objects
  if not Moved then DrawPage(dmSelection);
  if (SelNum >= 1) and Moved then
    if SelNum > 1 then
    begin
      Draw(TopSelected, ClipRgn);
      GetMultipleSelected;
      FDesigner.ShowPosition;
    end
    else
    begin
      t := Objects[TopSelected];
      NormalizeCoord(t);
      if Cursor <> crDefault then t.Resized;
      Draw(TopSelected, ClipRgn);
      FDesigner.ShowPosition;
    end;
  Moved := False;
  CT := ctNone;
end;

procedure TfrDesignerPage.MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, j, kx, ky, w, dx, dy: Integer;
  t, t1, Bnd: TfrView;
  nx, ny, x1, x2, y1, y2: Double;
  hr, hr1: HRGN;

  function Cont(px, py, x, y: Integer): Boolean;
  begin
    Result := (x >= px - w) and (x <= px + w + 1) and
      (y >= py - w) and (y <= py + w + 1);
  end;
  function GridCheck:Boolean;
  begin
    with FDesigner do
    begin
      Result := (kx >= GridSize) or (kx <= -GridSize) or
                (ky >= GridSize) or (ky <= -GridSize);
      if Result then
      begin
        kx := kx - kx mod GridSize;
        ky := ky - ky mod GridSize;
      end;
    end;
  end;

  procedure AddRgn(var HR: HRGN; T: TfrView);
  var
    tr: HRGN;
  begin
    tr := t.GetClipRgn(rtExtended);
    CombineRgn(HR, HR, TR, RGN_OR);
    DeleteObject(TR);
  end;


begin
  Moved := True;
  w := 2;
  if FirstChange and Down and not RFlag then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if not FDesigner.GridAlign or GridCheck then
    begin
      GetRegion;
      FDesigner.AddUndoAction(acEdit);
    end;
  end;

  if not Down then
    if FDesigner.OB6.Down then
    begin
      Mode := mdSelect;
      Cursor := crPencil;
    end
    else if FDesigner.OB1.Down then
    begin
      Mode := mdSelect;
      Cursor := crDefault;
    end
    else
    begin
      Mode := mdInsert;
      if Cursor <> crCross then
      begin
        RoundCoord(x, y);
        kx := Width; ky := 40;
        if not FDesigner.OB3.Down then
          FDesigner.GetDefaultSize(kx, ky);
        OldRect := Rect(x, y, x + kx, y + ky);
        DrawFocusRect(OldRect);
      end;
      Cursor := crCross;
    end;
  if (Mode = mdInsert) and not Down then
  begin
    DrawFocusRect(OldRect);
    RoundCoord(x, y);
    OffsetRect(OldRect, x - OldRect.Left, y - OldRect.Top);
    DrawFocusRect(OldRect);
    ShowSizes := True;
    FDesigner.PBox1Paint(nil);
    ShowSizes := False;
    Exit;
  end;

 // cursor shapes
  if not Down and (SelNum = 1) and (Mode = mdSelect) and
    not FDesigner.OB6.Down then
  begin
    t := Objects[TopSelected];
    if Cont(t.x, t.y, x, y) or Cont(t.x + t.dx, t.y + t.dy, x, y) then
      Cursor := crSizeNWSE
    else if Cont(t.x + t.dx, t.y, x, y) or Cont(t.x, t.y + t.dy, x, y)then
      Cursor := crSizeNESW
    else if Cont(t.x + t.dx div 2, t.y, x, y) or Cont(t.x + t.dx div 2, t.y + t.dy, x, y) then
      Cursor := crSizeNS
    else if Cont(t.x, t.y + t.dy div 2, x, y) or Cont(t.x + t.dx, t.y + t.dy div 2, x, y) then
      Cursor := crSizeWE
    else
      Cursor := crDefault;
  end;
  // selecting a lot of objects
  if Down and RFlag then
  begin
    DrawFocusRect(OldRect);
    if Cursor = crCross then
      RoundCoord(x, y);
    OldRect := Rect(OldRect.Left, OldRect.Top, x, y);
    DrawFocusRect(OldRect);
    ShowSizes := True;
    if Cursor = crCross then
      FDesigner.PBox1Paint(nil);
    ShowSizes := False;
    Exit;
  end;
  // line drawing
  if Down and (Cursor = crPencil) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    DrawRectLine(OldRect);
    OldRect := Rect(OldRect.Left, OldRect.Top, OldRect.Right + kx, OldRect.Bottom + ky);
    DrawRectLine(OldRect);
    Inc(LastX, kx);
    Inc(LastY, ky);
    Exit;
  end;
  // check for multiple selected objects - right-bottom corner
  if not Down and (SelNum > 1) and (Mode = mdSelect) then
  begin
    t := Objects[RightBottom];
    if Cont(t.x + t.dx, t.y + t.dy, x, y) then
      Cursor := crSizeNWSE
  end;
  // split checking
  if not Down and (SelNum > 1) and (Mode = mdSelect) then
  begin
    for i := 0 to Objects.Count-1 do
    begin
      t := Objects[i];
      if (t.Typ <> gtBand) and t.Selected then
        if (x >= t.x) and (x <= t.x + t.dx) and (y >= t.y) and (y <= t.y + t.dy) then
        begin
          for j := 0 to Objects.Count - 1 do
          begin
            t1 := Objects[j];
            if (t1.Typ <> gtBand) and (t1 <> t) and t1.Selected then
              if ((t.x = t1.x + t1.dx) and ((x >= t.x) and (x <= t.x + 2))) or
              ((t1.x = t.x + t.dx) and ((x >= t1.x - 2) and (x <= t.x))) then
              begin
                Cursor := crHSplit;
                with SplitInfo do
                begin
                  SplRect := Rect(x, t.y, x, t.y + t.dy);
                  if t.x = t1.x + t1.dx then
                  begin
                    SplX := t.x;
                    View1 := t1;
                    View2 := t;
                  end
                  else
                  begin
                    SplX := t1.x;
                    View1 := t;
                    View2 := t1;
                  end;
                  SplRect.Left := SplX;
                  SplRect.Right := SplX;
                end;
              end;
          end;
        end;
    end;
  end;
  // splitting
  if Down and MRFlag and (Mode = mdSelect) and (Cursor = crHSplit) then
  begin
    kx := x - LastX;
    ky := 0;
    if FDesigner.GridAlign and not GridCheck then Exit;
    with SplitInfo do
    begin
      DrawHSplitter(SplRect);
      SplRect := Rect(SplRect.Left + kx, SplRect.Top, SplRect.Right + kx, SplRect.Bottom);
      DrawHSplitter(SplRect);
    end;
    Inc(LastX, kx);
    Exit;
  end;
  // sizing several objects
  if Down and MRFlag and (Mode = mdSelect) and (Cursor <> crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;

    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      hr := CreateRectRgn(0, 0, 0, 0);
      hr1 := CreateRectRgn(0, 0, 0, 0);
    end;
    OldRect := Rect(OldRect.Left, OldRect.Top, OldRect.Right + kx, OldRect.Bottom + ky);
    nx := (OldRect.Right - OldRect.Left) / (OldRect1.Right - OldRect1.Left);
    ny := (OldRect.Bottom - OldRect.Top) / (OldRect1.Bottom - OldRect1.Top);
    for i := 0 to Objects.Count - 1 do
    begin
      t := Objects[i];
      if t.Selected then
      begin
        if FDesigner.ShapeMode = smAll then
          AddRgn(hr, t);
        x1 := (t.OriginalRect.Left - LeftTop.x) * nx;
        x2 := t.OriginalRect.Right * nx;
        dx := Round(x1 + x2) - (Round(x1) + Round(x2));
        t.x := LeftTop.x + Round(x1); t.dx := Round(x2) + dx;

        y1 := (t.OriginalRect.Top - LeftTop.y) * ny;
        y2 := t.OriginalRect.Bottom * ny;
        dy := Round(y1 + y2) - (Round(y1) + Round(y2));
        t.y := LeftTop.y + Round(y1); t.dy := Round(y2) + dy;
        if FDesigner.ShapeMode = smAll then
          AddRgn(hr1, t);
      end;
    end;
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      Draw(10000, hr);
      Draw(10000, hr1);
    end;
    Inc(LastX, kx);
    Inc(LastY, ky);
    FDesigner.PBox1Paint(nil);
    Exit;
  end;
  // moving
  if Down and (Mode = mdSelect) and (SelNum >= 1) and (Cursor = crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    if FirstBandMove and (SelNum = 1) and ((kx <> 0) or (ky <> 0)) and
      not (ssAlt in Shift) then
      if TfrView(Objects[TopSelected]).Typ = gtBand then
      begin
        Bnd := Objects[TopSelected];
        for i := 0 to Objects.Count-1 do
        begin
          t := Objects[i];
          if t.Typ <> gtBand then
            if (t.x >= Bnd.x) and (t.x + t.dx <= Bnd.x + Bnd.dx) and
               (t.y >= Bnd.y) and (t.y + t.dy <= Bnd.y + Bnd.dy) then
            begin
              t.Selected := True;
              Inc(SelNum);
            end;
        end;
        FDesigner.SelectionChanged;
        GetMultipleSelected;
      end;
    FirstBandMove := False;
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      hr := CreateRectRgn(0, 0, 0, 0);
      hr1 := CreateRectRgn(0, 0, 0, 0);
    end;
    for i := 0 to Objects.Count - 1 do
    begin
      t := Objects[i];
      if not t.Selected then continue;
      if FDesigner.ShapeMode = smAll then
        AddRgn(hr, t);
      t.x := t.x + kx;
      t.y := t.y + ky;
      if FDesigner.ShapeMode = smAll then
        AddRgn(hr1, t);
    end;
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      CombineRgn(hr, hr, hr1, RGN_OR);
      DeleteObject(hr1);
      Draw(10000, hr);
    end;
    Inc(LastX, kx);
    Inc(LastY, ky);
    FDesigner.PBox1Paint(nil);
  end;
 // resizing
  if Down and (Mode = mdSelect) and (SelNum = 1) and (Cursor <> crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    t := Objects[TopSelected];
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape) else
      hr := t.GetClipRgn(rtExtended);
    w := 3;
    if Cursor = crSizeNWSE then
      if (CT <> ct2) and ((CT = ct1) or Cont(t.x, t.y, LastX, LastY)) then
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        t.y := t.y + ky;
        t.dy := t.dy - ky;
        CT := ct1;
      end
      else
      begin
        t.dx := t.dx + kx;
        t.dy := t.dy + ky;
        CT := ct2;
      end;
    if Cursor = crSizeNESW then
      if (CT <> ct4) and ((CT = ct3) or Cont(t.x + t.dx, t.y, LastX, LastY)) then
      begin
        t.y := t.y + ky;
        t.dx := t.dx + kx;
        t.dy := t.dy - ky;
        CT := ct3;
      end
      else
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        t.dy := t.dy + ky;
        CT := ct4;
      end;
    if Cursor = crSizeWE then
      if (CT <> ct6) and ((CT = ct5) or Cont(t.x, t.y + t.dy div 2, LastX, LastY)) then
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        CT := ct5;
      end
      else
      begin
        t.dx := t.dx + kx;
        CT := ct6;
      end;
    if Cursor = crSizeNS then
      if (CT <> ct8) and ((CT = ct7) or Cont(t.x + t.dx div 2, t.y, LastX, LastY)) then
      begin
        t.y := t.y + ky;
        t.dy := t.dy - ky;
        CT := ct7;
      end
      else
      begin
        t.dy := t.dy + ky;
        CT := ct8;
      end;
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      CombineRgn(hr, hr, t.GetClipRgn(rtExtended), RGN_OR);
      Draw(10000, hr);
    end;
    Inc(LastX, kx);
    Inc(LastY, ky);
    FDesigner.PBox1Paint(nil);
  end;
end;

procedure TfrDesignerPage.DClick(Sender: TObject);
begin
  Down := False;
  if SelNum = 0 then
  begin
    FDesigner.PgB3Click(nil);
    DFlag := True;
  end
  else if SelNum = 1 then
  begin
    DFlag := True;
    FDesigner.ShowEditor;
  end
  else Exit;
end;

procedure TfrDesignerPage.CMMouseLeave(var Message: TMessage);
begin
  if (Mode = mdInsert) and not Down then
  begin
    DrawFocusRect(OldRect);
    OffsetRect(OldRect, -10000, -10000);
  end;
end;

{-----------------------------------------------------------------------------}
procedure BDown(SB: TfrSpeedButton);
begin
  SB.Down := True;
end;

procedure BUp(SB: TfrSpeedButton);
begin
  SB.Down := False;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TfrDesignerForm(frDesigner).C2.Items.AddObject(StrPas(LogFont.lfFaceName), TObject(FontType));
  Result := 1;
end;

procedure TfrDesignerForm.GetFontList;
var
  DC: HDC;
begin
  C2.Items.Clear;
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, nil);
  ReleaseDC(0, DC);
  LastFontName := C2.Items[0];
  if C2.Items.IndexOf('Arial') <> -1 then
    LastFontName := 'Arial'
  else if C2.Items.IndexOf('Arial Cyr') <> -1 then
    LastFontName := 'Arial Cyr';
  LastFontSize := 10;
end;

procedure TfrDesignerForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Busy := True;
  FirstTime := True;
  FirstInstance := FirstInst;
  // these invisible panels are added to set scroll range to
  // 0..Preview.Width+10 and 0..Preview.Height+10
  BPanel := TPanel.Create(ScrollBox1);
  BPanel.Parent := ScrollBox1;
  BPanel.Height := 1; BPanel.Left := 0; BPanel.Top := ScrollBox1.Height;
  BPanel.Color := ScrollBox1.Color;

  RPanel := TPanel.Create(ScrollBox1);
  RPanel.Parent := ScrollBox1;
  RPanel.Width := 1; RPanel.Left := ScrollBox1.Width; RPanel.Top := 0;
  RPanel.Color := ScrollBox1.Color;

  PageView := TfrDesignerPage.Create(ScrollBox1);
  PageView.FDesigner := Self;
  PageView.PopupMenu := Popup1;
  PageView.ShowHint := True;

  ColorSelector := TColorSelector.Create(Self);
  ColorSelector.OnColorSelected := ColorSelected;

  for i := 0 to frAddInsCount - 1 do
  with frAddIns[i] do
    RegisterObject(ButtonBmp, ButtonHint, Integer(gtAddIn) + i);

  if FirstInstance then
  begin
    frRegisterTool(LoadStr(SInsertFields), Image2.Picture.Bitmap, InsFieldsClick);
    for i := 0 to frToolsCount - 1 do
    with frTools[i] do
      RegisterTool(Caption, ButtonBmp, OnClick);
  end;

  InspForm := TfrInspForm.Create(nil);
  EditorForm := TfrEditorForm.Create(nil);
  with InspForm do
  begin
    ClearItems;
    Items.AddObject('Name', TProp.Create(@fld[0], csEdit, nil));
    Items.AddObject('Left', TProp.Create(@fld[1], csEdit, nil));
    Items.AddObject('Top', TProp.Create(@fld[2], csEdit, nil));
    Items.AddObject('Width', TProp.Create(@fld[3], csEdit, nil));
    Items.AddObject('Height', TProp.Create(@fld[4], csEdit, nil));
    Items.AddObject('Visible', TProp.Create(@fld[5], csEdit, nil));
    Items.AddObject('Memo', TProp.Create(@fld[6], csDefEditor, EditorForm));
    OnModify := Self.OnModify;
  end;

  MenuItems := TList.Create;
  ItemWidths := TStringlist.Create;
  if FirstInstance then
  begin
    Application.OnActivate := OnActivateApp;
    Application.OnDeactivate := OnDeactivateApp;
  end
  else
  begin
    PgB1.Enabled := False;
    PgB2.Enabled := False;
    N41.Enabled := False;
    N43.Enabled := False;
    N29.Enabled := False;
    N30.Enabled := False;
  end;
  FirstInst := False;

  FCaption :=         LoadStr(frRes + 080);
  Panel1.Caption :=   LoadStr(frRes + 081);
  Panel2.Caption :=   LoadStr(frRes + 082);
  Panel3.Caption :=   LoadStr(frRes + 083);
  Panel4.Caption :=   LoadStr(frRes + 084);
  Panel5.Caption :=   LoadStr(frRes + 085);
  Panel6.Caption :=   LoadStr(frRes + 086);
  FileBtn1.Hint :=    LoadStr(frRes + 087);
  FileBtn2.Hint :=    LoadStr(frRes + 088);
  FileBtn3.Hint :=    LoadStr(frRes + 089);
  FileBtn4.Hint :=    LoadStr(frRes + 090);
  CutB.Hint :=        LoadStr(frRes + 091);
  CopyB.Hint :=       LoadStr(frRes + 092);
  PstB.Hint :=        LoadStr(frRes + 093);
  UndoB.Hint :=       LoadStr(frRes + 094);
  RedoB.Hint :=       LoadStr(frRes + 095);
  ZB1.Hint :=         LoadStr(frRes + 096);
  ZB2.Hint :=         LoadStr(frRes + 097);
  SelAllB.Hint :=     LoadStr(frRes + 098);
  PgB1.Hint :=        LoadStr(frRes + 099);
  PgB2.Hint :=        LoadStr(frRes + 100);
  PgB3.Hint :=        LoadStr(frRes + 101);
  GB1.Hint :=         LoadStr(frRes + 102);
  GB2.Hint :=         LoadStr(frRes + 103);
  GB3.Hint :=         LoadStr(frRes + 104);
  HelpBtn.Hint :=     LoadStr(frRes + 032);
  ExitB.Caption :=    LoadStr(frRes + 105);
  ExitB.Hint :=       LoadStr(frRes + 106);
  AlB1.Hint :=        LoadStr(frRes + 107);
  AlB2.Hint :=        LoadStr(frRes + 108);
  AlB3.Hint :=        LoadStr(frRes + 109);
  AlB4.Hint :=        LoadStr(frRes + 110);
  AlB5.Hint :=        LoadStr(frRes + 111);
  AlB6.Hint :=        LoadStr(frRes + 112);
  AlB7.Hint :=        LoadStr(frRes + 113);
  AlB8.Hint :=        LoadStr(frRes + 114);
  FnB1.Hint :=        LoadStr(frRes + 115);
  FnB2.Hint :=        LoadStr(frRes + 116);
  FnB3.Hint :=        LoadStr(frRes + 117);
  ClB2.Hint :=        LoadStr(frRes + 118);
  HlB1.Hint :=        LoadStr(frRes + 119);
  C3.Hint :=          LoadStr(frRes + 120);
  C2.Hint :=          LoadStr(frRes + 121);
  FrB1.Hint :=        LoadStr(frRes + 122);
  FrB2.Hint :=        LoadStr(frRes + 123);
  FrB3.Hint :=        LoadStr(frRes + 124);
  FrB4.Hint :=        LoadStr(frRes + 125);
  FrB5.Hint :=        LoadStr(frRes + 126);
  FrB6.Hint :=        LoadStr(frRes + 127);
  ClB1.Hint :=        LoadStr(frRes + 128);
  ClB3.Hint :=        LoadStr(frRes + 129);
  E1.Hint :=          LoadStr(frRes + 130);
  OB1.Hint :=         LoadStr(frRes + 132);
  OB2.Hint :=         LoadStr(frRes + 133);
  OB3.Hint :=         LoadStr(frRes + 134);
  OB4.Hint :=         LoadStr(frRes + 135);
  OB5.Hint :=         LoadStr(frRes + 136);
  OB6.Hint :=         LoadStr(frRes + 137);
  Align1.Hint :=      LoadStr(frRes + 138);
  Align2.Hint :=      LoadStr(frRes + 139);
  Align3.Hint :=      LoadStr(frRes + 140);
  Align4.Hint :=      LoadStr(frRes + 141);
  Align5.Hint :=      LoadStr(frRes + 142);
  Align6.Hint :=      LoadStr(frRes + 143);
  Align7.Hint :=      LoadStr(frRes + 144);
  Align8.Hint :=      LoadStr(frRes + 145);
  Align9.Hint :=      LoadStr(frRes + 146);
  Align10.Hint :=     LoadStr(frRes + 147);
  N2.Caption :=       LoadStr(frRes + 148);
  N1.Caption :=       LoadStr(frRes + 149);
  N3.Caption :=       LoadStr(frRes + 150);
  N5.Caption :=       LoadStr(frRes + 151);
  N16.Caption :=      LoadStr(frRes + 152);
  N6.Caption :=       LoadStr(frRes + 153);
  FileMenu.Caption := LoadStr(frRes + 154);
  N23.Caption :=      LoadStr(frRes + 155);
  N19.Caption :=      LoadStr(frRes + 156);
  N20.Caption :=      LoadStr(frRes + 157);
  N42.Caption :=      LoadStr(frRes + 158);
  N8.Caption :=       LoadStr(frRes + 159);
  N25.Caption :=      LoadStr(frRes + 160);
  N39.Caption :=      LoadStr(frRes + 161);
  N10.Caption :=      LoadStr(frRes + 162);
  EditMenu.Caption := LoadStr(frRes + 163);
  N46.Caption :=      LoadStr(frRes + 164);
  N48.Caption :=      LoadStr(frRes + 165);
  N11.Caption :=      LoadStr(frRes + 166);
  N12.Caption :=      LoadStr(frRes + 167);
  N13.Caption :=      LoadStr(frRes + 168);
  N27.Caption :=      LoadStr(frRes + 169);
  N28.Caption :=      LoadStr(frRes + 170);
  N36.Caption :=      LoadStr(frRes + 171);
  N29.Caption :=      LoadStr(frRes + 172);
  N30.Caption :=      LoadStr(frRes + 173);
  N32.Caption :=      LoadStr(frRes + 174);
  N33.Caption :=      LoadStr(frRes + 175);
  ToolMenu.Caption := LoadStr(frRes + 176);
  N37.Caption :=      LoadStr(frRes + 177);
  MastMenu.Caption := LoadStr(frRes + 178);
  N14.Caption :=      LoadStr(frRes + 179);
  Pan1.Caption :=     LoadStr(frRes + 180);
  Pan2.Caption :=     LoadStr(frRes + 181);
  Pan3.Caption :=     LoadStr(frRes + 182);
  Pan4.Caption :=     LoadStr(frRes + 183);
  Pan5.Caption :=     LoadStr(frRes + 184);
  Pan6.Caption :=     LoadStr(frRes + 185);
  Pan7.Caption :=     LoadStr(frRes + 186);
  N34.Caption :=      LoadStr(frRes + 187);
  N17.Caption :=      LoadStr(frRes + 188);
  N22.Caption :=      LoadStr(frRes + 189);
  N35.Caption :=      LoadStr(frRes + 190);
  StB1.Hint   :=      LoadStr(frRes + 191);
  FnB1.Glyph.Handle := LoadBitmap(hInstance, 'FR_BOLD');
  FnB2.Glyph.Handle := LoadBitmap(hInstance, 'FR_ITALIC');
  FnB3.Glyph.Handle := LoadBitmap(hInstance, 'FR_UNDRLINE');

  N41.Caption :=      N29.Caption;
  N41.OnClick :=      N29.OnClick;
  N43.Caption :=      N30.Caption;
  N43.OnClick :=      N30.OnClick;
  N44.Caption :=      N25.Caption;
  N44.OnClick :=      N25.OnClick;
end;

procedure TfrDesignerForm.FormShow(Sender: TObject);
begin
  Screen.Cursors[crPencil] := LoadCursor(hInstance, 'FR_PENCIL');
  Panel7.Hide;
  if FirstTime then
    SetMenuBitmaps;
  FirstTime := False;
  FileBtn1.Enabled := FirstInstance;
  FileBtn4.Enabled := FirstInstance and not (CurReport is TfrCompositeReport);
  N39.Enabled := FileBtn4.Enabled;
  N23.Enabled := FirstInstance;
  OB3.Enabled := FirstInstance;
  OB5.Enabled := FirstInstance;

  ClearUndoBuffer;
  ClearRedoBuffer;
  Modified := False;
  FileModified := False;
  Busy := True;
  DocMode := dmDesigning;
  GetFontList;
  C2.Perform(CB_SETDROPPEDWIDTH, 170, 0);
  CurPage := 0; // this cause page sizing
  CurDocName := CurReport.FileName;
  Unselect;
  PageView.Init;
  EnableControls;
  BDown(OB1);
  frSetGlyph(0, ClB1, 1); frSetGlyph(0, ClB2, 0); frSetGlyph(0, ClB3, 2);
  ColorSelector.Hide;
  LinePanel.Hide;
  ShowPosition;
  RestoreState;
  FormResize(nil);
end;

procedure TfrDesignerForm.FormHide(Sender: TObject);
begin
  ClearUndoBuffer;
  ClearRedoBuffer;
  SaveState;
  CurReport.FileName := CurDocName;
end;

procedure TfrDesignerForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MenuItems.Count - 1 do
    TfrMenuItemInfo(MenuItems[i]).Free;
  MenuItems.Free;
  ItemWidths.Free;
  PageView.Free;
  BPanel.Free;
  RPanel.Free;
  ColorSelector.Free;
  InspForm.Free;
  EditorForm.Free;
end;

procedure TfrDesignerForm.FormResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
  with ScrollBox1 do
  begin
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
  end;
  PageView.SetPage;
  Panel7.Top := StatusBar1.Top + 3;
  Panel7.Show;
end;

procedure TfrDesignerForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin // for best view - not actual in Win98 :(
  with Msg.MinMaxInfo^ do
  begin
    ptMaxSize.x := Screen.Width;
    ptMaxSize.y := Screen.Height;
    ptMaxPosition.x := 0;
    ptMaxPosition.y := 0;
  end;
end;

procedure TfrDesignerForm.SetCurPage(Value: Integer);
begin // setting curpage and do all manipulation
  FCurPage := Value;
  Page := CurReport.Pages[CurPage];
  ScrollBox1.VertScrollBar.Position := 0;
  ScrollBox1.HorzScrollBar.Position := 0;
  PageView.SetPage;
  SetPageTitles;
  Tab1.TabIndex := Value;
  ResetSelection;
  SendBandsToDown;
  PageView.Repaint;
end;

procedure TfrDesignerForm.SetGridSize(Value: Integer);
begin
  if FGridSize = Value then Exit;
  FGridSize := Value;
  RedrawPage;
end;

procedure TfrDesignerForm.SetGridShow(Value: Boolean);
begin
  if FGridShow = Value then Exit;
  FGridShow := Value;
  GB1.Down := Value;
  RedrawPage;
end;

procedure TfrDesignerForm.SetGridAlign(Value: Boolean);
begin
  if FGridAlign = Value then Exit;
  GB2.Down := Value;
  FGridAlign := Value;
end;

procedure TfrDesignerForm.SetUnits(Value: TfrReportUnits);
var
  s: String;
begin
  FUnits := Value;
  case Value of
    ruPixels: s := LoadStr(SPixels);
    ruMM:     s := LoadStr(SMM);
    ruInches: s := LoadStr(SInches);
  end;
  StatusBar1.Panels[0].Text := s;
  ShowPosition;
end;

procedure TfrDesignerForm.SetGrayedButtons(Value: Boolean);
  procedure DoButtons(t: Array of TControl);
  var
    i, j: Integer;
    c: TWinControl;
    c1: TControl;
  begin
    for i := Low(t) to High(t) do
    begin
      c := TWinControl(t[i]);
      for j := 0 to c.ControlCount - 1 do
      begin
        c1 := c.Controls[j];
        if c1 is TfrSpeedButton then
          TfrSpeedButton(c1).GrayedInactive := FGrayedButtons;
      end;
    end;
  end;
begin
  FGrayedButtons := Value;
  DoButtons([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
end;

procedure TfrDesignerForm.SetCurDocName(Value: String);
begin
  FCurDocName := Value;
  if FirstInstance then
    Caption := FCaption + ' - ' + ExtractFileName(Value) else
    Caption := FCaption;
end;

procedure TfrDesignerForm.RegisterObject(ButtonBmp: TBitmap;
  const ButtonHint: String; ButtonTag: Integer);
var
  b: TfrTBButton;
begin
  b := TfrTBButton.Create(Self);
  with b do
  begin
    Parent := Panel4;
    Glyph := ButtonBmp;
    Hint := ButtonHint;
    GroupIndex := 1;
    Flat := True;
    SetBounds(1000, 1000, 22, 22);
    Tag := ButtonTag;
    OnMouseDown := OB2MouseDown;
  end;
end;

procedure TfrDesignerForm.RegisterTool(MenuCaption: String; ButtonBmp: TBitmap;
  OnClick: TNotifyEvent);
var
  m: TMenuItem;
  b: TfrTBButton;
begin
  m := TMenuItem.Create(MastMenu);
  m.Caption := MenuCaption;
  m.OnClick := OnClick;
  MastMenu.Enabled := True;
  MastMenu.Add(m);
  Panel6.Height := 26; Panel6.Width := 26;
  b := TfrTBButton.Create(Self);
  with b do
  begin
    Parent := Panel6;
    Glyph := ButtonBmp;
    Hint := MenuCaption;
    Flat := True;
    SetBounds(1000, 1000, 22, 22);
    Tag := 36;
  end;
  b.OnClick := OnClick;
  Panel6.AdjustBounds;
end;

procedure TfrDesignerForm.AddPage;
begin
  CurReport.Pages.Add;
  Page := CurReport.Pages[CurReport.Pages.Count - 1];
  PgB3Click(nil);
  if WasOk then
  begin
    Modified := True;
    FileModified := True;
    CurPage := CurReport.Pages.Count - 1
  end
  else
  begin
    CurReport.Pages.Delete(CurReport.Pages.Count - 1);
    CurPage := CurPage;
  end;
end;

procedure TfrDesignerForm.RemovePage(n: Integer);
  procedure AdjustSubReports;
  var
    i, j: Integer;
    t: TfrView;
  begin
    with CurReport do
      for i := 0 to Pages.Count - 1 do
      begin
        j := 0;
        while j < Pages[i].Objects.Count do
        begin
          t := Pages[i].Objects[j];
          if t.Typ = gtSubReport then
            if TfrSubReportView(t).SubPage = n then
            begin
              Pages[i].Delete(j);
              Dec(j);
            end
            else if TfrSubReportView(t).SubPage > n then
              Dec(TfrSubReportView(t).SubPage);
          Inc(j);
        end;
      end;
  end;
begin
  Modified := True;
  FileModified := True;
  with CurReport do
    if (n >= 0) and (n < Pages.Count) then
      if Pages.Count = 1 then
        Pages[n].Clear else
      begin
        CurReport.Pages.Delete(n);
        Tab1.Tabs.Delete(n);
        Tab1.TabIndex := 0;
        AdjustSubReports;
        CurPage := 0;
      end;
  ClearUndoBuffer;
  ClearRedoBuffer;
end;

procedure TfrDesignerForm.SetPageTitles;
var
  i: Integer;
  s: String;
  function IsSubreport(PageN: Integer): Boolean;
  var
    i, j: Integer;
    t: TfrView;
  begin
    Result := False;
    with CurReport do
      for i := 0 to Pages.Count - 1 do
        for j := 0 to Pages[i].Objects.Count - 1 do
        begin
          t := Pages[i].Objects[j];
          if t.Typ = gtSubReport then
            if TfrSubReportView(t).SubPage = PageN then
            begin
              s := t.Name;
              Result := True;
              Exit;
            end;
        end;
  end;
begin
  if Tab1.Tabs.Count = CurReport.Pages.Count then
    for i := 0 to Tab1.Tabs.Count - 1 do
    begin
      if not IsSubreport(i) then
        s := LoadStr(SPg) + IntToStr(i + 1);
      if Tab1.Tabs[i] <> s then
        Tab1.Tabs[i] := s;
    end
  else
  begin
    Tab1.Tabs.Clear;
    for i := 0 to CurReport.Pages.Count - 1 do
    begin
      if not IsSubreport(i) then
        s := LoadStr(SPg) + IntToStr(i + 1);
      Tab1.Tabs.Add(s);
    end;
  end;
end;

procedure TfrDesignerForm.CutToClipboard;
var
  i: Integer;
  t: TfrView;
begin
  ClearClipBoard;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      ClipBd.Add(frCreateObject(t.Typ, t.ClassName));
      TfrView(ClipBd.Last).Assign(t);
    end;
  end;
  for i := Objects.Count - 1 downto 0 do
  begin
    t := Objects[i];
    if t.Selected then Page.Delete(i);
  end;
  SelNum := 0;
end;

procedure TfrDesignerForm.CopyToClipboard;
var
  i: Integer;
  t: TfrView;
begin
  ClearClipBoard;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      ClipBd.Add(frCreateObject(t.Typ, t.ClassName));
      TfrView(ClipBd.Last).Assign(t);
    end;
  end;
end;

procedure TfrDesignerForm.SelectAll;
var
  i: Integer;
begin
  SelNum := 0;
  for i := 0 to Objects.Count - 1 do
  begin
    TfrView(Objects[i]).Selected := True;
    Inc(SelNum);
  end;
end;

procedure TfrDesignerForm.Unselect;
var
  i: Integer;
begin
  SelNum := 0;
  for i := 0 to Objects.Count - 1 do
    TfrView(Objects[i]).Selected := False;
end;

procedure TfrDesignerForm.ResetSelection;
begin
  Unselect;
  EnableControls;
  ShowPosition;
end;

function TfrDesignerForm.PointsToUnits(x: Integer): Double;
begin
  Result := x;
  case FUnits of
    ruMM: Result := x / 18 * 5;
    ruInches: Result := x / 18 * 5 / 25.4;
  end;
end;

function TfrDesignerForm.UnitsToPoints(x: Double): Integer;
begin
  Result := Round(x);
  case FUnits of
    ruMM: Result := Round(x / 5 * 18);
    ruInches: Result := Round(x * 25.4 / 5 * 18);
  end;
end;

procedure TfrDesignerForm.RedrawPage;
begin
  PageView.Draw(10000, 0);
end;

procedure TfrDesignerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  StepX, StepY: Integer;
  i, tx, ty, tx1, ty1, d, d1: Integer;
  t, t1: TfrView;
begin
  StepX := 0; StepY := 0;
  if (Key = vk_Return) and (ActiveControl = C3) then
  begin
    Key := 0;
    DoClick(C3);
  end;
  if (Key = vk_Return) and (ActiveControl = E1) then
  begin
    Key := 0;
    DoClick(E1);
  end;
  if (Key = vk_Delete) and DelEnabled then
  begin
    DeleteObjects;
    Key := 0;
  end;
  if (Key = vk_Return) and EditEnabled then
  begin
    if ssCtrl in Shift then
      ShowMemoEditor else
      ShowEditor;
  end;
  if (Chr(Key) in ['1'..'9']) and (ssCtrl in Shift) and DelEnabled then
  begin
    E1.Text := Chr(Key);
    DoClick(E1);
    Key := 0;
  end;
  if (Chr(Key) = 'F') and (ssCtrl in Shift) and DelEnabled then
  begin
    FrB5.Click;
    Key := 0;
  end;
  if (Chr(Key) = 'D') and (ssCtrl in Shift) and DelEnabled then
  begin
    FrB6.Click;
    Key := 0;
  end;
  if (Chr(Key) = 'G') and (ssCtrl in Shift) then
  begin
    ShowGrid := not ShowGrid;
    Key := 0;
  end;
  if (Chr(Key) = 'B') and (ssCtrl in Shift) then
  begin
    GridAlign := not GridAlign;
    Key := 0;
  end;
  if CutEnabled then
    if (Key = vk_Delete) and (ssShift in Shift) then CutBClick(Self);
  if CopyEnabled then
    if (Key = vk_Insert) and (ssCtrl in Shift) then CopyBClick(Self);
  if PasteEnabled then
    if (Key = vk_Insert) and (ssShift in Shift) then PstBClick(Self);
  if Key = vk_Prior then
    with ScrollBox1.VertScrollBar do
    begin
      Position := Position - 200;
      Key := 0;
    end;
  if Key = vk_Next then
    with ScrollBox1.VertScrollBar do
    begin
      Position := Position + 200;
      Key := 0;
    end;
  if SelNum > 0 then
  begin
    if Key = vk_Up then StepY := -1
    else if Key = vk_Down then StepY := 1
    else if Key = vk_Left then StepX := -1
    else if Key = vk_Right then StepX := 1;
    if (StepX <> 0) or (StepY <> 0) then
    begin
      if ssCtrl in Shift then
        MoveObjects(StepX, StepY, False)
      else if ssShift in Shift then
        MoveObjects(StepX, StepY, True)
      else if SelNum = 1 then
      begin
        t := Objects[TopSelected];
        tx := t.x; ty := t.y; tx1 := t.x + t.dx; ty1 := t.y + t.dy;
        d := 10000; t1 := nil;
        for i := 0 to Objects.Count-1 do
        begin
          t := Objects[i];
          if not t.Selected and (t.Typ <> gtBand) then
          begin
            d1 := 10000;
            if StepX <> 0 then
            begin
              if t.y + t.dy < ty then
                d1 := ty - (t.y + t.dy)
              else if t.y > ty1 then
                d1 := t.y - ty1
              else if (t.y <= ty) and (t.y + t.dy >= ty1) then
                d1 := 0
              else
                d1 := t.y - ty;
              if ((t.x <= tx) and (StepX = 1)) or
                 ((t.x + t.dx >= tx1) and (StepX = -1)) then
                d1 := 10000;
              if StepX = 1 then
                if t.x >= tx1 then
                  d1 := d1 + t.x - tx1 else
                  d1 := d1 + t.x - tx
              else if t.x + t.dx <= tx then
                  d1 := d1 + tx - (t.x + t.dx) else
                  d1 := d1 + tx1 - (t.x + t.dx);
            end
            else if StepY <> 0 then
            begin
              if t.x + t.dx < tx then
                d1 := tx - (t.x + t.dx)
              else if t.x > tx1 then
                d1 := t.x - tx1
              else if (t.x <= tx) and (t.x + t.dx >= tx1) then
                d1 := 0
              else
                d1 := t.x - tx;
              if ((t.y <= ty) and (StepY = 1)) or
                 ((t.y + t.dy >= ty1) and (StepY = -1)) then
                d1 := 10000;
              if StepY = 1 then
                if t.y >= ty1 then
                  d1 := d1 + t.y - ty1 else
                  d1 := d1 + t.y - ty
              else if t.y + t.dy <= ty then
                  d1 := d1 + ty - (t.y + t.dy) else
                  d1 := d1 + ty1 - (t.y + t.dy);
            end;
            if d1 < d then
            begin
              d := d1;
              t1 := t;
            end;
          end;
        end;
        if t1 <> nil then
        begin
          t := Objects[TopSelected];
          if not (ssAlt in Shift) then
          begin
            PageView.DrawPage(dmSelection);
            Unselect;
            SelNum := 1;
            t1.Selected := True;
            PageView.DrawPage(dmSelection);
          end
          else
          begin
            if (t1.x >= t.x + t.dx) and (Key = vk_Right) then
              t.x := t1.x - t.dx
            else if (t1.y > t.y + t.dy) and (Key = vk_Down) then
              t.y := t1.y - t.dy
            else if (t1.x + t1.dx <= t.x) and (Key = vk_Left) then
              t.x := t1.x + t1.dx
            else if (t1.y + t1.dy <= t.y) and (Key = vk_Up) then
              t.y := t1.y + t1.dy;
            RedrawPage;
          end;
          SelectionChanged;
        end;
      end;
    end;
  end;
end;

procedure TfrDesignerForm.MoveObjects(dx, dy: Integer; Resize: Boolean);
var
  i: Integer;
  t: TfrView;
begin
  AddUndoAction(acEdit);
  GetRegion;
  PageView.DrawPage(dmSelection);
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      if Resize then
      begin
        Inc(t.dx, dx); Inc(t.dy, dy);
      end
      else
      begin
        Inc(t.x, dx); Inc(t.y, dy);
      end;
  end;
  ShowPosition;
  PageView.GetMultipleSelected;
  PageView.Draw(TopSelected, ClipRgn);
end;

procedure TfrDesignerForm.DeleteObjects;
var
  i: Integer;
  t: TfrView;
begin
  AddUndoAction(acDelete);
  GetRegion;
  PageView.DrawPage(dmSelection);
  for i := Objects.Count - 1 downto 0 do
  begin
    t := Objects[i];
    if t.Selected then Page.Delete(i);
  end;
  SetPageTitles;
  ResetSelection;
  FirstSelected := nil;
  PageView.Draw(10000, ClipRgn);
end;

function TfrDesignerForm.SelStatus: TfrSelectionStatus;
var
  t: TfrView;
begin
  Result := [];
  if SelNum = 1 then
  begin
    t := Objects[TopSelected];
    if t.Typ = gtBand then
      Result := [ssBand]
    else if t is TfrMemoView then
      Result := [ssMemo] else
      Result := [ssOther];
  end
  else if SelNum > 1 then
    Result := [ssMultiple];
  if ClipBd.Count > 0 then
    Result := Result + [ssClipboardFull];
end;

function TfrDesignerForm.RectTypEnabled: Boolean;
begin
  Result := [ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.FontTypEnabled: Boolean;
begin
  Result := [ssMemo, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.ZEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.CutEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.CopyEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.PasteEnabled: Boolean;
begin
  Result := ssClipboardFull in SelStatus;
end;

function TfrDesignerForm.DelEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.EditEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther] * SelStatus <> [];
end;

procedure TfrDesignerForm.EnableControls;
  procedure SetEnabled(const Ar: Array of TObject; en: Boolean);
  var
    i: Integer;
  begin
    for i := Low(Ar) to High(Ar) do
      if Ar[i] is TControl then
        (Ar[i] as TControl).Enabled := en
      else if Ar[i] is TMenuItem then
        (Ar[i] as TMenuItem).Enabled := en;
  end;
begin
  SetEnabled([FrB1, FrB2, FrB3, FrB4, FrB5, FrB6, ClB1, ClB3, E1, SB1, SB2, StB1],
    RectTypEnabled);
  SetEnabled([ClB2, C2, C3, FnB1, FnB2, FnB3, AlB1, AlB2, AlB3, AlB4, AlB5, AlB6, AlB7, AlB8, HlB1],
    FontTypEnabled);
  SetEnabled([ZB1, ZB2, N32, N33, GB3], ZEnabled);
  SetEnabled([CutB, N11, N2], CutEnabled);
  SetEnabled([CopyB, N12, N1], CopyEnabled);
  SetEnabled([PstB, N13, N3], PasteEnabled);
  SetEnabled([N27, N5], DelEnabled);
  SetEnabled([N36, N6], EditEnabled);
  if not C2.Enabled then
  begin
    C2.ItemIndex := -1;
    C3.Text := '';
  end;
  InspForm.EnableItem(6, EditEnabled);
  StatusBar1.Repaint;
  PBox1Paint(nil);
end;

procedure TfrDesignerForm.SelectionChanged;
var
  t: TfrView;
begin
  Busy := True;
  ColorSelector.Hide;
  LinePanel.Hide;
  EnableControls;
  if SelNum = 1 then
  begin
    t := Objects[TopSelected];
    if t.Typ <> gtBand then
    with t do
    begin
      FrB1.Down := (FrameTyp and $8) <> 0;
      FrB2.Down := (FrameTyp and $4) <> 0;
      FrB3.Down := (FrameTyp and $2) <> 0;
      FrB4.Down := (FrameTyp and $1) <> 0;
      E1.Text := FloatToStrF(FrameWidth, ffGeneral, 2, 2);
      frSetGlyph(FillColor, ClB1, 1);
      frSetGlyph(FrameColor, ClB3, 2);
      if t is TfrMemoView then
      with t as TfrMemoView do
      begin
        frSetGlyph(Font.Color, ClB2, 0);
        if C2.ItemIndex <> C2.Items.IndexOf(Font.Name) then
          C2.ItemIndex := C2.Items.IndexOf(Font.Name);
        if C3.Text <> IntToStr(Font.Size) then
          C3.Text := IntToStr(Font.Size);
        FnB1.Down := fsBold in Font.Style;
        FnB2.Down := fsItalic in Font.Style;
        FnB3.Down := fsUnderline in Font.Style;
        AlB4.Down := (Adjust and $4) <> 0;
        AlB5.Down := (Adjust and $18) = $8;
        AlB6.Down := (Adjust and $18) = 0;
        AlB7.Down := (Adjust and $18) = $10;
        case (Adjust and $3) of
          0: BDown(AlB1);
          1: BDown(AlB2);
          2: BDown(AlB3);
          3: BDown(AlB8);
        end;
      end;
    end;
  end
  else if SelNum > 1 then
  begin
    BUp(FrB1); BUp(FrB2); BUp(FrB3); BUp(FrB4);
    frSetGlyph(0, ClB1, 1); E1.Text := '1'; C2.ItemIndex := -1; C3.Text := '';
    BUp(FnB1); BUp(FnB2); BUp(FnB3);
    BDown(AlB1); BUp(AlB4); BUp(AlB5);
  end;
  Busy := False;
  ShowPosition;
  ShowContent;
  ActiveControl := nil;
end;

procedure TfrDesignerForm.ShowPosition;
begin
  FillInspFields;
  InspForm.ItemsChanged;
  StatusBar1.Repaint;
  PBox1Paint(nil);
end;

procedure TfrDesignerForm.ShowContent;
var
  t: TfrView;
  s: String;
begin
  s := '';
  if SelNum = 1 then
  begin
    t := Objects[TopSelected];
    s := t.Name;
    if t is TfrBandView then
      s := s + ': ' + frBandNames[Integer(TfrBandView(t).BandType)]
    else if t.Memo.Count > 0 then
      s := s + ': ' + t.Memo[0];
  end;
  StatusBar1.Panels[2].Text := s;
end;

procedure SetBit(var w: Word; e: Boolean; m: Integer);
begin
  if e then w := w or m
  else w := w and not m;
end;

procedure TfrDesignerForm.DoClick(Sender: TObject);
var
  i, j, b: Integer;
  s: String;
  DRect: TRect;
  t: TfrView;
begin
  if Busy then Exit;
  AddUndoAction(acEdit);
  PageView.DrawPage(dmSelection);
  GetRegion;
  b := (Sender as TControl).Tag;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected and ((t.Typ <> gtBand) or (b = 16)) then
    with t do
    begin
      if t is TfrMemoView then
      with t as TfrMemoView do
        case b of
          7: if C2.ItemIndex <> 0 then
             begin
               Font.Name := C2.Text;
               LastFontName := Font.Name;
             end;
          8: begin
               Font.Size := StrToInt(C3.Text);
               LastFontSize := Font.Size;
             end;
          9: begin
               LastFontStyle := frGetFontStyle(Font.Style);
               SetBit(LastFontStyle, FnB1.Down, 2);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         10: begin
               LastFontStyle := frGetFontStyle(Font.Style);
               SetBit(LastFontStyle, FnB2.Down, 1);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         11..13:
             begin
               Adjust := (Adjust and $FC) + (b - 11);
               LastAdjust := Adjust;
             end;
         14: begin
               Adjust := (Adjust and $FB) + Word(AlB4.Down) * 4;
               LastAdjust := Adjust;
             end;
         15: begin
               Adjust := (Adjust and $E7) + Word(AlB5.Down) * 8 + Word(AlB7.Down) * $10;
               LastAdjust := Adjust;
             end;
         17: begin
               Font.Color := ColorSelector.Color;
               LastFontColor := Font.Color;
             end;
         18: begin
               LastFontStyle := frGetFontStyle(Font.Style);
               SetBit(LastFontStyle, FnB3.Down, 4);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         22: begin
               Adjust := (Adjust and $FC) + 3;
               LastAdjust := Adjust;
             end;
        end;
      case b of
        1:
         begin
           SetBit(FrameTyp, FrB1.Down, 8);
           DRect := Rect(t.x - 10, t.y - 10, t.x + t.dx + 10, t.y + 10)
         end;
        2:
         begin
           SetBit(FrameTyp, FrB2.Down, 4);
           DRect := Rect(t.x - 10, t.y - 10, t.x + 10, t.y + t.dy + 10)
         end;
        3:
         begin
           SetBit(FrameTyp, FrB3.Down, 2);
           DRect := Rect(t.x - 10, t.y + t.dy - 10, t.x + t.dx + 10, t.y + t.dy + 10)
         end;
        4:
         begin
           SetBit(FrameTyp, FrB4.Down, 1);
           DRect := Rect(t.x + t.dx - 10, t.y - 10, t.x + t.dx + 10, t.y + t.dy + 10)
         end;
        20:
         begin
           FrameTyp := FrameTyp or $F;
           LastFrameTyp := $F;
         end;
        21:
         begin
           FrameTyp := FrameTyp and not $F;
           LastFrameTyp := 0;
         end;
        5:
         begin
           FillColor := ColorSelector.Color;
           LastFillColor := FillColor;
         end;
        6:
         begin
           s := E1.Text;
           for j := 1 to Length(s) do
             if s[j] in ['.', ','] then
               s[j] := DecimalSeparator;
           FrameWidth := StrToFloat(s);
           if t is TfrLineView then
             LastLineWidth := FrameWidth else
             LastFrameWidth := FrameWidth;
         end;
        19:
         begin
           FrameColor := ColorSelector.Color;
           LastFrameColor := FrameColor;
         end;
        25..30:
          FrameStyle := b - 25;
      end;
    end;
  end;
  PageView.Draw(TopSelected, ClipRgn);
  ActiveControl := nil;
  if b in [20, 21] then SelectionChanged;
end;

procedure TfrDesignerForm.frSpeedButton1Click(Sender: TObject);
begin
  LinePanel.Hide;
  DoClick(Sender);
end;

procedure TfrDesignerForm.HlB1Click(Sender: TObject);
var
  t: TfrMemoView;
begin
  t := Objects[TopSelected];
  frHilightForm := TfrHilightForm.Create(nil);
  with frHilightForm do
  begin
    FontColor := t.Highlight.FontColor;
    FillColor := t.Highlight.FillColor;
    CB1.Checked := (t.Highlight.FontStyle and $2) <> 0;
    CB2.Checked := (t.Highlight.FontStyle and $1) <> 0;
    CB3.Checked := (t.Highlight.FontStyle and $4) <> 0;
    Edit1.Text := t.HighlightStr;
    if ShowModal = mrOk then
    begin
      AddUndoAction(acEdit);
      t.HighlightStr := Edit1.Text;
      t.Highlight.FontColor := FontColor;
      t.Highlight.FillColor := FillColor;
      SetBit(t.Highlight.FontStyle, CB1.Checked, 2);
      SetBit(t.Highlight.FontStyle, CB2.Checked, 1);
      SetBit(t.Highlight.FontStyle, CB3.Checked, 4);
    end;
  end;
  frHilightForm.Free;
end;

procedure TfrDesignerForm.FillInspFields;
var
  t: TfrView;
  i, x, y, dx, dy, v: Integer;
  procedure FillFields(x, y, dx, dy, v: Integer);
  begin
    if FUnits = ruPixels then
    begin
      if x <> -10000 then fld[1] := IntToStr(x);
      if y <> -10000 then fld[2] := IntToStr(y);
      if dx <> -10000 then fld[3] := IntToStr(dx);
      if dy <> -10000 then fld[4] := IntToStr(dy);
    end
    else
    begin
      if x <> -10000 then fld[1] := FloatToStrF(PointsToUnits(x), ffFixed, 4, 2);
      if y <> -10000 then fld[2] := FloatToStrF(PointsToUnits(y), ffFixed, 4, 2);
      if dx <> -10000 then fld[3] := FloatToStrF(PointsToUnits(dx), ffFixed, 4, 2);
      if dy <> -10000 then fld[4] := FloatToStrF(PointsToUnits(dy), ffFixed, 4, 2);
    end;
    if v <> -10000 then
    begin
      if v <> 0 then v := 1;
      fld[5] := IntToStr(v);
    end;
  end;
begin
  fld[0] := ''; fld[1] := ''; fld[2] := '';
  fld[3] := ''; fld[4] := ''; fld[5] := ''; fld[6] := '';
  InspForm.V := nil;
  if SelNum = 1 then
  begin
    t := Objects[TopSelected];
    InspForm.V := t;
    fld[0] := t.Name;
    FillFields(t.x, t.y, t.dx, t.dy, Integer(t.Visible));
  end
  else if SelNum > 1 then
  begin
    t := Objects[TopSelected];
    x := t.x; y := t.y; dx := t.dx; dy := t.dy; v := Integer(t.Visible);
    for i := 0 to Objects.Count - 1 do
    begin
      t := Objects[i];
      if t.Selected then
      begin
        if t.x <> x then x := -10000;
        if t.y <> y then y := -10000;
        if t.dx <> dx then dx := -10000;
        if t.dy <> dy then dy := -10000;
        if Integer(t.Visible) <> v then v := -10000;
      end;
    end;
    FillFields(x, y, dx, dy, v);
  end;
  InspForm.HideProperties := SelNum = 0;
end;

procedure TfrDesignerForm.OnModify(Item: Integer; var EditText: String);
var
  t: TfrView;
  i, k: Integer;
begin
  AddUndoAction(acEdit);
  if (Item = 0) and (SelNum = 1) then
  begin
    t := Objects[TopSelected];
    if CurReport.FindObject(fld[0]) = nil then
      t.Name := fld[0] else
      EditText := t.Name;
    SetPageTitles;
  end
  else if Item in [1..5] then
  begin
    EditText := frParser.Calc(fld[Item]);
    if Item <> 6 then
      k := UnitsToPoints(StrToFloat(EditText)) else
      k := StrToInt(EditText);
    for i := 0 to Objects.Count-1 do
    begin
      t := Objects[i];
      if t.Selected then
      with t do
        case Item of
          1: if (k > 0) and (k < Page.PrnInfo.Pgw) then
               x := k;
          2: if (k > 0) and (k < Page.PrnInfo.Pgh) then
             y := k;
          3: if (k > 0) and (k < Page.PrnInfo.Pgw) then
             dx := k;
          4: if (k > 0) and (k < Page.PrnInfo.Pgh) then
             dy := k;
          5: Visible := Boolean(k);
        end;
    end;
  end;
  FillInspFields;
  if Item in [1..5] then
    EditText := fld[Item];
  RedrawPage;
  StatusBar1.Repaint;
  PBox1Paint(nil);
end;

procedure TfrDesignerForm.StB1Click(Sender: TObject);
var
  p: TPoint;
begin
  if not LinePanel.Visible then
  begin
    LinePanel.Parent := Self;
    with (Sender as TControl) do
      p := Self.ScreenToClient(Parent.ClientToScreen(Point(Left, Top)));
    LinePanel.Left := p.X;
    LinePanel.Top := p.Y + 26;
  end;
  LinePanel.Visible := not LinePanel.Visible;
end;

procedure TfrDesignerForm.ClB1Click(Sender: TObject);
var
  p: TPoint;
begin
  with (Sender as TControl) do
    p := Self.ScreenToClient(Parent.ClientToScreen(Point(Left, Top)));
  if ColorSelector.Left = p.X then
    ColorSelector.Visible := not ColorSelector.Visible
  else
  begin
    ColorSelector.Left := p.X;
    ColorSelector.Top := p.Y + 26;
    ColorSelector.Visible := True;
  end;
  ClrButton := Sender as TfrSpeedButton;
  with ClrButton.Glyph.Canvas do
    if Pixels[2, 14] <> Pixels[1, 13] then
      ColorSelector.Color := clNone else
      ColorSelector.Color := Pixels[2, 14];
end;

procedure TfrDesignerForm.ColorSelected(Sender: TObject);
var
  n: Integer;
begin
  n := 0;
  if ClrButton = ClB1 then
    n := 1
  else if ClrButton = ClB3 then
    n := 2;
  frSetGlyph(ColorSelector.Color, ClrButton, n);
  DoClick(ClrButton);
end;

procedure TfrDesignerForm.PBox1Paint(Sender: TObject);
var
  t: TfrView;
  s: String;
  nx, ny: Double;
  x, y, dx, dy: Integer;
begin
  with PBox1.Canvas do
  begin
    FillRect(Rect(0, 0, PBox1.Width, PBox1.Height));
    ImageList1.Draw(PBox1.Canvas, 2, 0, 0);
    ImageList1.Draw(PBox1.Canvas, 92, 0, 1);
    if (SelNum = 1) or ShowSizes then
    begin
      t := nil;
      if ShowSizes then
      begin
        x := OldRect.Left; y := OldRect.Top;
        dx := OldRect.Right - x; dy := OldRect.Bottom - y;
      end
      else
      begin
        t := Objects[TopSelected];
        x := t.x; y := t.y; dx := t.dx; dy := t.dy;
      end;
      if FUnits = ruPixels then
        s := IntToStr(x) + ';' + IntToStr(y) else
        s := FloatToStrF(PointsToUnits(x), ffFixed, 4, 2) + '; ' +
          FloatToStrF(PointsToUnits(y), ffFixed, 4, 2);
      TextOut(20, 1, s);
      if FUnits = ruPixels then
        s := IntToStr(dx) + ';' + IntToStr(dy) else
        s := FloatToStrF(PointsToUnits(dx), ffFixed, 4, 2) + '; ' +
          FloatToStrF(PointsToUnits(dy), ffFixed, 4, 2);
      TextOut(110, 1, s);
      if not ShowSizes and (t.Typ = gtPicture) then
        with t as TfrPictureView do
        if (Picture.Graphic <> nil) and not Picture.Graphic.Empty then
        begin
          s := IntToStr(dx * 100 div Picture.Width) + ',' +
               IntToStr(dy * 100 div Picture.Height);
          TextOut(170, 1, '% ' + s);
        end;
    end
    else if (SelNum > 0) and MRFlag then
    begin
      nx := 0; ny := 0;
      if OldRect1.Right - OldRect1.Left <> 0 then
        nx := (OldRect.Right - OldRect.Left) / (OldRect1.Right - OldRect1.Left);
      if OldRect1.Bottom - OldRect1.Top <> 0 then
        ny := (OldRect.Bottom - OldRect.Top) / (OldRect1.Bottom - OldRect1.Top);
      s := IntToStr(Round(nx * 100)) + ',' + IntToStr(Round(ny * 100));
      TextOut(170, 1, '% ' + s);
    end;
  end;
end;

procedure TfrDesignerForm.C2DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with C2.Canvas do
  begin
    FillRect(Rect);
    if (Integer(C2.Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      ImageList2.Draw(C2.Canvas, Rect.Left, Rect.Top + 1, 0);
    TextOut(Rect.Left + 20, Rect.Top + 1, C2.Items[Index]);
  end;
end;

procedure TfrDesignerForm.ShowMemoEditor;
begin
  with EditorForm do
  begin
    View := Objects[TopSelected];
    if ShowEditor = mrOk then
    begin
      PageView.DrawPage(dmSelection);
      PageView.Draw(TopSelected, View.GetClipRgn(rtExtended));
    end;
  end;
  ActiveControl := nil;
end;

procedure TfrDesignerForm.ShowEditor;
var
  t: TfrView;
  i: Integer;
  bt: TfrBandType;
begin
  t := Objects[TopSelected];
  if t.Typ = gtMemo then
    ShowMemoEditor
  else if t.Typ = gtPicture then
  begin
    frGEditorForm := TfrGEditorForm.Create(nil);
    with frGEditorForm do
    begin
      Image1.Picture.Assign((t as TfrPictureView).Picture);
      if ShowModal = mrOk then
      begin
        AddUndoAction(acEdit);
        PageView.DrawPage(dmSelection);
        (t as TfrPictureView).Picture.Assign(Image1.Picture);
        PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
      end;
    end;
    frGEditorForm.Free;
  end
  else if t.Typ = gtBand then
  begin
    PageView.DrawPage(dmSelection);
    bt := (t as TfrBandView).BandType;
    if bt in [btMasterData, btDetailData, btSubDetailData] then
    begin
      frBandEditorForm := TfrBandEditorForm.Create(nil);
      frBandEditorForm.ShowEditor(t);
      frBandEditorForm.Free;
    end
    else if bt = btGroupHeader then
    begin
      frGroupEditorForm := TfrGroupEditorForm.Create(nil);
      frGroupEditorForm.ShowEditor(t);
      frGroupEditorForm.Free;
    end
    else if bt = btCrossData then
    begin
      frVBandEditorForm := TfrVBandEditorForm.Create(nil);
      frVBandEditorForm.ShowEditor(t);
      frVBandEditorForm.Free;
    end
    else
      PageView.DFlag := False;
    PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
  end
  else if t.Typ = gtSubReport then
    CurPage := (t as TfrSubReportView).SubPage
  else if t.Typ = gtAddIn then
  begin
    for i := 0 to frAddInsCount - 1 do
      if frAddIns[i].ClassRef.ClassName = t.ClassName then
      begin
        if frAddIns[i].EditorForm <> nil then
        begin
          PageView.DrawPage(dmSelection);
          frAddIns[i].EditorForm.ShowEditor(t);
          PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
        end
        else
          ShowMemoEditor;
        break;
      end;
  end;
  ShowContent;
  ShowPosition;
  ActiveControl := nil;
end;

procedure TfrDesignerForm.ReleaseAction(ActionRec: TfrUndoRec);
var
  p, p1: PfrUndoObj;
begin
  p := ActionRec.Objects;
  while p <> nil do
  begin
    if ActionRec.Action in [acDelete, acEdit] then
      p^.ObjPtr.Free;
    p1 := p;
    p := p^.Next;
    FreeMem(p1, SizeOf(TfrUndoObj));
  end;
end;

procedure TfrDesignerForm.ClearBuffer(Buffer: TfrUndoBuffer; var BufferLength: Integer);
var
  i: Integer;
begin
  for i := 0 to BufferLength - 1 do
    ReleaseAction(Buffer[i]);
  BufferLength := 0;
end;

procedure TfrDesignerForm.ClearUndoBuffer;
begin
  ClearBuffer(FUndoBuffer, FUndoBufferLength);
  N46.Enabled := False;
  UndoB.Enabled := N46.Enabled;
end;

procedure TfrDesignerForm.ClearRedoBuffer;
begin
  ClearBuffer(FRedoBuffer, FRedoBufferLength);
  N48.Enabled := False;
  RedoB.Enabled := N48.Enabled;
end;

procedure TfrDesignerForm.Undo(Buffer: PfrUndoBuffer);
var
  p, p1: PfrUndoObj;
  r: PfrUndoRec1;
  BufferLength: Integer;
  List: TList;
  a: TfrUndoAction;
begin
  if Buffer = @FUndoBuffer then
    BufferLength := FUndoBufferLength else
    BufferLength := FRedoBufferLength;

  if Buffer[BufferLength - 1].Page <> CurPage then Exit;
  List := TList.Create;
  a := Buffer[BufferLength - 1].Action;
  p := Buffer[BufferLength - 1].Objects;
  while p <> nil do
  begin
    GetMem(r, SizeOf(TfrUndoRec1));
    r^.ObjPtr := p^.ObjPtr;
    r^.Int := p^.Int;
    List.Add(r);
    case Buffer[BufferLength - 1].Action of
      acInsert:
        begin
          r^.Int := Page.FindObjectByID(p^.ObjID);
          r^.ObjPtr := Objects[r^.Int];
          a := acDelete;
        end;
      acDelete: a := acInsert;
      acEdit:   r^.ObjPtr := Objects[p^.Int];
      acZOrder:
        begin
          r^.Int := Page.FindObjectByID(p^.ObjID);
          r^.ObjPtr := Objects[r^.Int];
          p^.ObjPtr := r^.ObjPtr;
        end;
    end;
    p := p^.Next;
  end;
  if Buffer = @FUndoBuffer then
    AddAction(@FRedoBuffer, a, List) else
    AddAction(@FUndoBuffer, a, List);
  List.Free;

  p := Buffer[BufferLength - 1].Objects;
  while p <> nil do
  begin
    case Buffer[BufferLength - 1].Action of
      acInsert: Page.Delete(Page.FindObjectByID(p^.ObjID));
      acDelete: Objects.Insert(p^.Int, p^.ObjPtr);
      acEdit:
        begin
          TfrView(Objects[p^.Int]).Assign(p^.ObjPtr);
          p^.ObjPtr.Free;
        end;
      acZOrder: Objects[p^.Int] := p^.ObjPtr;
    end;
    p1 := p;
    p := p^.Next;
    FreeMem(p1, SizeOf(TfrUndoObj));
  end;

  if Buffer = @FUndoBuffer then
    Dec(FUndoBufferLength) else
    Dec(FRedoBufferLength);

  ResetSelection;
  RedrawPage;
  N46.Enabled := FUndoBufferLength > 0;
  UndoB.Enabled := N46.Enabled;
  N48.Enabled := FRedoBufferLength > 0;
  RedoB.Enabled := N48.Enabled;
end;

procedure TfrDesignerForm.AddAction(Buffer: PfrUndoBuffer; a: TfrUndoAction; List: TList);
var
  i: Integer;
  p, p1: PfrUndoObj;
  r: PfrUndoRec1;
  t, t1: TfrView;
  BufferLength: Integer;
begin
  if Buffer = @FUndoBuffer then
    BufferLength := FUndoBufferLength else
    BufferLength := FRedoBufferLength;
  if BufferLength >= MaxUndoBuffer then
  begin
    ReleaseAction(Buffer[0]);
    for i := 0 to MaxUndoBuffer - 2 do
      Buffer^[i] := Buffer^[i + 1];
    BufferLength := MaxUndoBuffer - 1;
  end;
  Buffer[BufferLength].Action := a;
  Buffer[BufferLength].Page := CurPage;
  Buffer[BufferLength].Objects := nil;
  p := nil;
  for i := 0 to List.Count - 1 do
  begin
    r := List[i];
    t := r^.ObjPtr;
    GetMem(p1, SizeOf(TfrUndoObj));
    p1^.Next := nil;
    if Buffer[BufferLength].Objects = nil then
      Buffer[BufferLength].Objects := p1 else
      p^.Next := p1;
    p := p1;
    case a of
      acInsert: p^.ObjID := t.ID;
      acDelete, acEdit:
        begin
          t1 := frCreateObject(t.Typ, t.ClassName);
          t1.Assign(t);
          t1.ID := t.ID;
          p^.ObjID := t.ID;
          p^.ObjPtr := t1;
          p^.Int := r^.Int;
        end;
      acZOrder:
        begin
          p^.ObjID := t.ID;
          p^.Int := r^.Int;
        end;
    end;
    FreeMem(r, SizeOf(TfrUndoRec1));
  end;
  if Buffer = @FUndoBuffer then
  begin
    FUndoBufferLength := BufferLength + 1;
    N46.Enabled := True;
    UndoB.Enabled := True;
  end
  else
  begin
    FRedoBufferLength := BufferLength + 1;
    N48.Enabled := True;
    RedoB.Enabled := True;
  end;
  Modified := True;
  FileModified := True;
end;

procedure TfrDesignerForm.AddUndoAction(a: TfrUndoAction);
var
  i: Integer;
  t: TfrView;
  List: TList;
  p: PfrUndoRec1;
begin
  ClearRedoBuffer;
  List := TList.Create;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected or (a = acZOrder) then
    begin
      GetMem(p, SizeOf(TfrUndoRec1));
      p^.ObjPtr := t;
      p^.Int := i;
      List.Add(p);
    end;
  end;
  AddAction(@FUndoBuffer, a, List);
  List.Free;
end;

procedure TfrDesignerForm.BeforeChange;
begin
  AddUndoAction(acEdit);
end;

procedure TfrDesignerForm.AfterChange;
begin
  PageView.DrawPage(dmSelection);
  PageView.Draw(TopSelected, 0);
end;

procedure TfrDesignerForm.ZB1Click(Sender: TObject);   // go up
var
  i, j, n: Integer;
  t: TfrView;
begin
  AddUndoAction(acZOrder);
  n := Objects.Count; i := 0; j := 0;
  while j < n do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      Objects.Delete(i);
      Objects.Add(t);
    end else Inc(i);
    Inc(j);
  end;
  SendBandsToDown;
  RedrawPage;
end;

procedure TfrDesignerForm.ZB2Click(Sender: TObject);    // go down
var
  t: TfrView;
  i, j, n: Integer;
begin
  AddUndoAction(acZOrder);
  n := Objects.Count; j := 0; i := n - 1;
  while j < n do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
    end else Dec(i);
    Inc(j);
  end;
  SendBandsToDown;
  RedrawPage;
end;

procedure TfrDesignerForm.PgB1Click(Sender: TObject); // add page
begin
  ResetSelection;
  AddPage;
end;

procedure TfrDesignerForm.PgB2Click(Sender: TObject); // remove page
begin
  if MessageBox(0, PChar(LoadStr(SRemovePg)),
    PChar(LoadStr(SConfirm)), mb_IconQuestion + mb_YesNo) = mrYes then
    RemovePage(CurPage);
end;

procedure TfrDesignerForm.OB1Click(Sender: TObject);
begin
  ObjRepeat := False;
end;

procedure TfrDesignerForm.OB2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ObjRepeat := ssShift in Shift;
  PageView.Cursor := crDefault;
end;

procedure TfrDesignerForm.CutBClick(Sender: TObject); //cut
begin
  AddUndoAction(acDelete);
  CutToClipboard;
  FirstSelected := nil;
  EnableControls;
  ShowPosition;
  RedrawPage;
end;

procedure TfrDesignerForm.CopyBClick(Sender: TObject); //copy
begin
  CopyToClipboard;
  EnableControls;
end;

procedure TfrDesignerForm.PstBClick(Sender: TObject); //paste
var
  i, minx, miny: Integer;
  t, t1: TfrView;
begin
  Unselect;
  SelNum := 0;
  minx := 32767; miny := 32767;
  with ClipBd do
  for i := 0 to Count-1 do
  begin
    t := Items[i];
    if t.x < minx then minx := t.x;
    if t.y < miny then miny := t.y;
  end;
  for i := 0 to ClipBd.Count - 1 do
  begin
    t := ClipBd.Items[i];
    if t.Typ = gtBand then
      if not (TfrBandType(t.FrameTyp) in [btMasterHeader..btSubDetailFooter,
                                          btGroupHeader, btGroupFooter]) and
        frCheckBand(TfrBandType(t.FrameTyp)) then
        continue;
    if PageView.Left < 0 then
      t.x := t.x - minx + ((-PageView.Left) div GridSize * GridSize) else
      t.x := t.x - minx;
    if PageView.Top < 0 then
      t.y := t.y - miny + ((-PageView.Top) div GridSize * GridSize) else
      t.y := t.y - miny;
    Inc(SelNum);
    t1 := frCreateObject(t.Typ, t.ClassName);
    t1.Assign(t);
    if CurReport.FindObject(t1.Name) <> nil then
      t1.CreateUniqueName;
    Objects.Add(t1);
  end;
  SelectionChanged;
  SendBandsToDown;
  PageView.GetMultipleSelected;
  RedrawPage;
  AddUndoAction(acInsert);
end;

procedure TfrDesignerForm.UndoBClick(Sender: TObject); // undo
begin
  Undo(@FUndoBuffer);
end;

procedure TfrDesignerForm.RedoBClick(Sender: TObject); // redo
begin
  Undo(@FRedoBuffer);
end;

procedure TfrDesignerForm.SelAllBClick(Sender: TObject); // select all
begin
  PageView.DrawPage(dmSelection);
  SelectAll;
  PageView.GetMultipleSelected;
  PageView.DrawPage(dmSelection);
  SelectionChanged;
end;

procedure TfrDesignerForm.ExitBClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


procedure TfrDesignerForm.N5Click(Sender: TObject); // popup delete command
begin
  DeleteObjects;
end;

procedure TfrDesignerForm.N6Click(Sender: TObject); // popup edit command
begin
  ShowEditor;
end;

procedure TfrDesignerForm.FileBtn1Click(Sender: TObject); // create new
var
  w: Word;
begin
  if FileModified then
  begin
    w := MessageBox(0, PChar(LoadStr(SSaveChanges) + ' ' + LoadStr(STo) + ' ' +
      ExtractFileName(CurDocName) + '?'),
      PChar(LoadStr(SConfirm)), mb_IconQuestion + mb_YesNoCancel);
    if w = mrCancel then Exit;
    if w = mrYes then
    begin
      FileBtn3Click(nil);
      if not WasOk then Exit;
    end;
  end;
  ClearUndoBuffer;
  CurReport.Pages.Clear;
  CurReport.Pages.Add;
  CurPage := 0;
  CurDocName := LoadStr(SUntitled);
  FileModified := False;
end;

procedure TfrDesignerForm.N23Click(Sender: TObject); // create new from template
begin
  frTemplForm := TfrTemplForm.Create(nil);
  with frTemplForm do
  if ShowModal = mrOk then
  begin
    ClearUndoBuffer;
    CurReport.LoadTemplate(TemplName, nil, nil, True);
    CurDocName := LoadStr(SUntitled);
    CurPage := 0; // do all
  end;
  frTemplForm.Free;
end;

procedure TfrDesignerForm.FileBtn2Click(Sender: TObject); // open
var
  w: Word;
begin
  w := mrNo;
  if FileModified then
    w := MessageBox(0, PChar(LoadStr(SSaveChanges) + ' ' + LoadStr(STo) + ' ' +
      ExtractFileName(CurDocName) + '?'),
      PChar(LoadStr(SConfirm)), mb_IconQuestion + mb_YesNoCancel);
  if w = mrCancel then Exit;
  if w = mrYes then
  begin
    FileBtn3Click(nil);
    if not WasOk then Exit;
  end;
  OpenDialog1.Filter := LoadStr(SFormFile) + ' (*.frf)|*.frf';
  if OpenDialog1.Execute then
  begin
    ClearUndoBuffer;
    CurDocName := OpenDialog1.FileName;
    CurReport.LoadFromFile(CurDocName);
    FileModified := False;
    CurPage := 0; // do all
  end;
end;

procedure TfrDesignerForm.N20Click(Sender: TObject); // save as
var
  s: String;
begin
  WasOk := False;
  with SaveDialog1 do
  begin
    Filter := LoadStr(SFormFile) + ' (*.frf)|*.frf|' +
              LoadStr(STemplFile) + ' (*.frt)|*.frt';
    FileName := CurDocName;
    FilterIndex := 1;
    if Execute then
      if FilterIndex = 1 then
      begin
        s := ChangeFileExt(FileName, '.frf');
        CurReport.SaveToFile(s);
        CurDocName := s;
        WasOk := True;
      end
      else
      begin
        s := ExtractFileName(ChangeFileExt(FileName, '.frt'));
        if frTemplateDir <> '' then
          s := frTemplateDir + '\' + s;
        frTemplNewForm := TfrTemplNewForm.Create(nil);
        with frTemplNewForm do
        if ShowModal = mrOk then
        begin
          CurReport.SaveTemplate(s, Memo1.Lines, Image1.Picture.Bitmap);
          WasOk := True;
        end;
        frTemplNewForm.Free;
      end;
  end;
end;

procedure TfrDesignerForm.FileBtn3Click(Sender: TObject); // save
begin
  if CurDocName <> LoadStr(SUntitled) then
  begin
    CurReport.SaveToFile(CurDocName);
    FileModified := False;
  end
  else
    N20Click(nil);
end;

procedure TfrDesignerForm.FileBtn4Click(Sender: TObject); // preview
var
  v1, v2: Boolean;
begin
  if CurReport is TfrCompositeReport then Exit;
  v1 := InspForm.Visible;
  v2 := CurReport.ModalPreview;
  InspForm.Hide;
  Application.ProcessMessages;
  CurReport.ModalPreview := True;
  try
    CurReport.ShowReport;
  finally
    InspForm.Visible := v1;
    CurReport.ModalPreview := v2;
    SetFocus;
    DisableDrawing := False;
    CurPage := 0;
  end;
end;

procedure TfrDesignerForm.N42Click(Sender: TObject); // var editor
begin
  if ShowEvEditor(CurReport) then
  begin
    Modified := True;
    FileModified := True;
  end;
end;

procedure TfrDesignerForm.PgB3Click(Sender: TObject); // page setup
var
  w, h, p: Integer;
begin
  frPgoptForm := TfrPgoptForm.Create(nil);
  with frPgoptForm, Page do
  begin
    CB1.Checked := PrintToPrevPage;
    CB5.Checked := not UseMargins;
    if pgOr = poPortrait then
      RB1.Checked := True else
      RB2.Checked := True;
    ComB1.Items := Prn.PaperNames;
    ComB1.ItemIndex := Prn.GetArrayPos(pgSize);
    E1.Text := ''; E2.Text := '';
    if pgSize = $100 then
    begin
      E1.Text := IntToStr(pgWidth div 10);
      E2.Text := IntToStr(pgHeight div 10);
    end;
    E3.Text := IntToStr(pgMargins.Left * 5 div 18);
    E4.Text := IntToStr(pgMargins.Top * 5 div 18);
    E5.Text := IntToStr(pgMargins.Right * 5 div 18);
    E6.Text := IntToStr(pgMargins.Bottom * 5 div 18);
    E7.Text := IntToStr(ColGap * 5 div 18);
    Edit1.Text := IntToStr(ColCount);
    WasOk := False;
    if ShowModal = mrOk then
    begin
      Modified := True;
      FileModified := True;
      WasOk := True;
      PrintToPrevPage :=  CB1.Checked;
      UseMargins := not CB5.Checked;
      if RB1.Checked then
        pgOr := poPortrait else
        pgOr := poLandscape;
      p := Prn.PaperSizes[ComB1.ItemIndex];
      w := 0; h := 0;
      if p = $100 then
        try
          w := StrToInt(E1.Text) * 10; h := StrToInt(E2.Text) * 10;
        except
          on exception do p := 9; // A4
        end;
      try
        pgMargins := Rect(StrToInt(E3.Text) * 18 div 5,
                          StrToInt(E4.Text) * 18 div 5,
                          StrToInt(E5.Text) * 18 div 5,
                          StrToInt(E6.Text) * 18 div 5);
        ColGap := StrToInt(E7.Text) * 18 div 5;
      except
        on exception do
        begin
          pgMargins := Rect(0, 0, 0, 0);
          ColGap := 0;
        end;
      end;
      ColCount := StrToInt(Edit1.Text);
      ChangePaper(p, w, h, pgOr);
      CurPage := CurPage; // for repaint and other
    end;
  end;
  frPgoptForm.Free;
end;

procedure TfrDesignerForm.N8Click(Sender: TObject); // report setup
begin
  frDocOptForm := TfrDocOptForm.Create(nil);
  with frDocOptForm do
  begin
    CB1.Checked := not CurReport.PrintToDefault;
    CB2.Checked := CurReport.DoublePass;
    if ShowModal = mrOk then
    begin
      CurReport.PrintToDefault := not CB1.Checked;
      CurReport.DoublePass := CB2.Checked;
      CurReport.ChangePrinter(Prn.PrinterIndex, ComB1.ItemIndex);
      Modified := True;
      FileModified := True;
    end;
    CurPage := CurPage;
    Free;
  end;
end;

procedure TfrDesignerForm.N14Click(Sender: TObject); // grid menu
var
  DesOptionsForm: TfrDesOptionsForm;
begin
  DesOptionsForm := TfrDesOptionsForm.Create(nil);
  with DesOptionsForm do
  begin
    CB1.Checked := ShowGrid;
    CB2.Checked := GridAlign;
    case GridSize of
      4: RB1.Checked := True;
      8: RB2.Checked := True;
      18: RB3.Checked := True;
    end;
    if ShapeMode = smFrame then
      RB4.Checked := True else
      RB5.Checked := True;
    case Units of
      ruPixels: RB6.Checked := True;
      ruMM:     RB7.Checked := True;
      ruInches: RB8.Checked := True;
    end;
    CB3.Checked := not GrayedButtons;
    CB4.Checked := EditAfterInsert;
    CB5.Checked := ShowBandTitles;
    if ShowModal = mrOk then
    begin
      ShowGrid := CB1.Checked;
      GridAlign := CB2.Checked;
      if RB1.Checked then
        GridSize := 4
      else if RB2.Checked then
        GridSize := 8
      else
        GridSize := 18;
      if RB4.Checked then
        ShapeMode := smFrame else
        ShapeMode := smAll;
      if RB6.Checked then
        Units := ruPixels
      else if RB7.Checked then
        Units := ruMM
      else
        Units := ruInches;
      GrayedButtons := not CB3.Checked;
      EditAfterInsert := CB4.Checked;
      ShowBandTitles := CB5.Checked;
      RedrawPage;
    end;
    Free;
  end;
end;

procedure TfrDesignerForm.GB1Click(Sender: TObject);
begin
  ShowGrid := GB1.Down;
end;

procedure TfrDesignerForm.GB2Click(Sender: TObject);
begin
  GridAlign := GB2.Down;
end;

procedure TfrDesignerForm.GB3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  AddUndoAction(acEdit);
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      t.x := Round(t.x / GridSize) * GridSize;
      t.y := Round(t.y / GridSize) * GridSize;
      t.dx := Round(t.dx / GridSize) * GridSize;
      t.dy := Round(t.dy / GridSize) * GridSize;
      if t.dx = 0 then t.dx := GridSize;
      if t.dy = 0 then t.dy := GridSize;
    end;
  end;
  RedrawPage;
  ShowPosition;
  PageView.GetMultipleSelected;
end;

procedure TfrDesignerForm.Tab1Change(Sender: TObject);
begin
  CurPage := Tab1.TabIndex;
end;

procedure TfrDesignerForm.Popup1Popup(Sender: TObject);
var
  i: Integer;
  t, t1: TfrView;
  fl: Boolean;
begin
  DeleteMenuItems(N2.Parent);
  EnableControls;
  while Popup1.Items.Count > 7 do
    Popup1.Items.Delete(7);
  if SelNum = 1 then
    TfrView(Objects[TopSelected]).DefinePopupMenu(Popup1)
  else if SelNum > 1 then
  begin
    t := Objects[TopSelected];
    fl := True;
    for i := 0 to Objects.Count - 1 do
    begin
      t1 := Objects[i];
      if t1.Selected then
        if not (((t is TfrMemoView) and (t1 is TfrMemoView)) or
           ((t.Typ <> gtAddIn) and (t.Typ = t1.Typ)) or
           ((t.Typ = gtAddIn) and (t.ClassName = t1.ClassName))) then
        begin
          fl := False;
          break;
        end;
    end;
    if fl and not (t.Typ = gtBand) then t.DefinePopupMenu(Popup1);
  end;

  FillMenuItems(N2.Parent);
  SetMenuItemBitmap(N2, CutB);
  SetMenuItemBitmap(N1, CopyB);
  SetMenuItemBitmap(N3, PstB);
  SetMenuItemBitmap(N16, SelAllB);
end;

procedure TfrDesignerForm.N37Click(Sender: TObject);
begin // toolbars
  Pan1.Checked := Panel1.IsVisible;
  Pan2.Checked := Panel2.IsVisible;
  Pan3.Checked := Panel3.IsVisible;
  Pan4.Checked := Panel4.IsVisible;
  Pan5.Checked := InspForm.Visible;
  Pan6.Checked := Panel5.Visible;
  Pan7.Checked := Panel6.Visible;
end;

procedure TfrDesignerForm.Pan2Click(Sender: TObject);
  procedure SetShow(c: Array of TWinControl; i: Integer; b: Boolean);
  begin
    if c[i] is TfrToolBar then
    with c[i] as TfrToolBar do
    begin
      if IsFloat then
        FloatWindow.Visible := b
      else
      begin
        if b then
          AddToDock(Parent as TfrDock);
        Visible := b;
        (Parent as TfrDock).AdjustBounds;
      end;
    end
    else
      (c[i] as TForm).Visible := b;
  end;
begin // each toolbar
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    SetShow([Panel1, Panel2, Panel3, Panel4, Panel5, InspForm, Panel6], Tag, Checked);
  end;
end;

procedure TfrDesignerForm.N34Click(Sender: TObject);
begin // about box
  frAboutForm := TfrAboutForm.Create(nil);
  frAboutForm.ShowModal;
  frAboutForm.Free;
end;

{$HINTS OFF}
procedure TfrDesignerForm.InsFieldsClick(Sender: TObject);
var
  i, x, y, dx, dy, pdx, adx: Integer;
  HeaderL, DataL: TList;
  t, t1: TfrView;
  b: TfrBandView;
  f: TfrTField;
{$IFNDEF IBO}
  f1: TFieldDef;
{$ENDIF}
  fSize: Integer;
  fName: String;

  function FindDataset(DataSet: TfrTDataSet): String;
  var
    i: Integer;
    function EnumComponents(f: TComponent): String;
    var
      i: Integer;
      c: TComponent;
      d: TfrDBDataSet;
    begin
      Result := '';
      for i := 0 to f.ComponentCount - 1 do
      begin
        c := f.Components[i];
        if c is TfrDBDataSet then
        begin
          d := c as TfrDBDataSet;
          if d.GetDataSet = DataSet then
          begin
            if d.Owner = CurReport.Owner then
              Result := d.Name else
              Result := d.Owner.Name + '.' + d.Name;
            break;
          end;
        end;
      end;
    end;
  begin
    Result := '';
    for i := 0 to Screen.FormCount - 1 do
    begin
      Result := EnumComponents(Screen.Forms[i]);
      if Result <> '' then Exit;
    end;
    for i := 0 to Screen.DataModuleCount - 1 do
    begin
      Result := EnumComponents(Screen.DataModules[i]);
      if Result <> '' then Exit;
    end;
  end;

begin
  HeaderL := TList.Create;
  DataL := TList.Create;
  frInsertFieldsForm := TfrInsertFieldsForm.Create(nil);
  with frInsertFieldsForm do
  begin
    if (ShowModal = mrOk) and (DataSet <> nil) and
      (FieldsL.Items.Count > 0) and (FieldsL.SelCount > 0) then
    begin
      x := Page.LeftMargin; y := Page.TopMargin;
      Unselect;
      SelNum := 0;
      for i := 0 to FieldsL.Items.Count - 1 do
        if FieldsL.Selected[i] then
        begin
          f := TfrTField(DataSet.FindField(FieldsL.Items[i]));
          fSize := 0;
          if f <> nil then
          begin
            fSize := f.DisplayWidth;
            fName := f.DisplayName;
          end
{$IFNDEF IBO}
          else
          begin
            f1 := DataSet.FieldDefs[i];
            fSize := f1.Size;
            fName := f1.Name;
          end
{$ENDIF};
          if (fSize = 0) or (fSize > 255) then
            fSize := 6;

          t := frCreateObject(gtMemo, '');
          t.CreateUniqueName;
          t.x := x;
          t.y := y;
          GetDefaultSize(t.dx, t.dy);
          with t as TfrMemoView do
          begin
            Font.Name := LastFontName;
            Font.Size := LastFontSize;
            if HeaderCB.Checked then
              Font.Style := [fsBold];
          end;
          PageView.Canvas.Font.Assign(TfrMemoView(t).Font);
          t.Selected := True;
          Inc(SelNum);
          if HeaderCB.Checked then
          begin
            t.Memo.Add(fName);
            t.dx := PageView.Canvas.TextWidth(fName + '   ') div GridSize * GridSize;
          end
          else
          begin
            t.Memo.Add('[' + DatasetCB.Items[DatasetCB.ItemIndex] +
              '."' + FieldsL.Items[i] + '"]');
            t.dx := (fSize * PageView.Canvas.TextWidth('=')) div GridSize * GridSize;
          end;
          dx := t.dx;
          Page.Objects.Add(t);
          if HeaderCB.Checked then
            HeaderL.Add(t) else
            DataL.Add(t);
          if HeaderCB.Checked then
          begin
            t := frCreateObject(gtMemo, '');
            t.CreateUniqueName;
            t.x := x;
            t.y := y;
            GetDefaultSize(t.dx, t.dy);
            if HorzRB.Checked then
              Inc(t.y, 72) else
              Inc(t.x, dx + GridSize * 2);
            with t as TfrMemoView do
            begin
              Font.Name := LastFontName;
              Font.Size := LastFontSize;
            end;
            t.Selected := True;
            Inc(SelNum);
            t.Memo.Add('[' + DatasetCB.Items[DatasetCB.ItemIndex] +
              '."' + FieldsL.Items[i] + '"]');
            t.dx := (fSize * PageView.Canvas.TextWidth('=')) div GridSize * GridSize;
            Page.Objects.Add(t);
            DataL.Add(t);
          end;
          if HorzRB.Checked then
            Inc(x, t.dx + GridSize) else
            Inc(y, t.dy + GridSize);
        end;

      if HorzRB.Checked then
      begin
        t := DataL[DataL.Count - 1];
        adx := t.x + t.dx;
        pdx := Page.RightMargin - Page.LeftMargin;
        x := Page.LeftMargin;
        if adx > pdx then
        begin
          for i := 0 to DataL.Count - 1 do
          begin
            t := DataL[i];
            t.x := Round((t.x - x) / (adx / pdx)) + x;
            t.dx := Round(t.dx / (adx / pdx));
          end;
          if HeaderCB.Checked then
            for i := 0 to DataL.Count - 1 do
            begin
              t := HeaderL[i];
              t1 := DataL[i];
              t.x := Round((t.x - x) / (adx / pdx)) + x;
              if t.dx > t1.dx then
                t.dx := t1.dx;
            end;
        end;
      end;

      if BandCB.Checked then
      begin
        if HeaderCB.Checked then
          t := HeaderL[DataL.Count - 1] else
          t := DataL[DataL.Count - 1];
        dy := t.y + t.dy - Page.TopMargin;
        b := frCreateObject(gtBand, '') as TfrBandView;
        b.CreateUniqueName;
        b.y := Page.TopMargin;
        b.dy := dy;
        b.Selected := True;
        Inc(SelNum);
        if not HeaderCB.Checked or not HorzRB.Checked then
        begin
          Page.Objects.Add(b);
          b.BandType := btMasterData;
          b.DataSet := FindDataset(DataSet);
        end
        else
        begin
          if frCheckBand(btPageHeader) then
          begin
            Dec(SelNum);
            b.Free;
          end
          else
          begin
            b.BandType := btPageHeader;
            Page.Objects.Add(b);
          end;
          b := frCreateObject(gtBand, '') as TfrBandView;
          b.BandType := btMasterData;
          b.DataSet := FindDataset(DataSet);
          b.CreateUniqueName;
          b.y := Page.TopMargin + 72;
          b.dy := dy;
          b.Selected := True;
          Inc(SelNum);
          Page.Objects.Add(b);
        end;
      end;
      SelectionChanged;
      SendBandsToDown;
      PageView.GetMultipleSelected;
      RedrawPage;
      AddUndoAction(acInsert);
    end;
    Free;
  end;
  HeaderL.Free;
  DataL.Free;
end;
{$HINTS ON}

procedure TfrDesignerForm.Tab1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  GetCursorPos(p);
  if Button = mbRight then
    TrackPopupMenu(Popup2.Handle,
      TPM_LEFTALIGN or TPM_RIGHTBUTTON, p.X, p.Y, 0, Handle, nil);
end;

{----------------------------------------------------------------------------}
// state storing/retrieving
const
  rsGridShow = 'GridShow';
  rsGridAlign = 'GridAlign';
  rsGridSize = 'GridSize';
  rsUnits = 'Units';
  rsButtons = 'GrayButtons';
  rsEdit = 'EditAfterInsert';
  rsSelection = 'Selection';


procedure TfrDesignerForm.SaveState;
var
  Ini: TRegIniFile;
  Nm: String;
  procedure DoSaveToolbars(t: Array of TfrToolBar);
  var
    i: Integer;
  begin
    for i := Low(t) to High(t) do
    begin
      if FirstInstance or (t[i] <> Panel6) then
        SaveToolbarPosition(t[i]);
      t[i].IsVisible := False;
    end;
  end;

begin
  Ini := TRegIniFile.Create(RegRootKey);
  Nm := rsForm + Name;
  Ini.WriteBool(Nm, rsGridShow, ShowGrid);
  Ini.WriteBool(Nm, rsGridAlign, GridAlign);
  Ini.WriteInteger(Nm, rsGridSize, GridSize);
  Ini.WriteInteger(Nm, rsUnits, Word(Units));
  Ini.WriteBool(Nm, rsButtons, GrayedButtons);
  Ini.WriteBool(Nm, rsEdit, EditAfterInsert);
  Ini.WriteInteger(Nm, rsSelection, Integer(ShapeMode));
  Ini.Free;
  DoSaveToolbars([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
  SaveFormPosition(InspForm);
  InspForm.Hide;
end;

procedure TfrDesignerForm.RestoreState;
var
  Ini: TRegIniFile;
  Nm: String;
  procedure DoRestoreToolbars(t: Array of TfrToolBar);
  var
    i: Integer;
  begin
    for i := Low(t) to High(t) do
      RestoreToolbarPosition(t[i]);
  end;

begin
  Ini := TRegIniFile.Create(RegRootKey);
  Nm := rsForm + Name;
  GridSize := Ini.ReadInteger(Nm, rsGridSize, 4);
  GridAlign := Ini.ReadBool(Nm, rsGridAlign, True);
  ShowGrid := Ini.ReadBool(Nm, rsGridShow, True);
  Units := TfrReportUnits(Ini.ReadInteger(Nm, rsUnits, 0));
  GrayedButtons := Ini.ReadBool(Nm, rsButtons, False);
  EditAfterInsert := Ini.ReadBool(Nm, rsEdit, True);
  ShapeMode := TfrShapeMode(Ini.ReadInteger(Nm, rsSelection, 1));
  Ini.Free;
  DoRestoreToolbars([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
  if Panel6.Height < 26 then
    Panel6.Height := 26;
  if Panel6.Width < 26 then
    Panel6.Width := 26;
  if Panel6.ControlCount < 2 then
    Panel6.Hide;
  frDock1.AdjustBounds;
  frDock2.AdjustBounds;
  frDock3.AdjustBounds;
  frDock4.AdjustBounds;
  RestoreFormPosition(InspForm);
  InspForm.Panel1.Show;
end;


{----------------------------------------------------------------------------}
// menu bitmaps
procedure TfrDesignerForm.SetMenuBitmaps;
var
  i: Integer;
begin
  MaxItemWidth := 0; MaxShortCutWidth := 0;
  FillMenuItems(FileMenu);
  FillMenuItems(EditMenu);
  FillMenuItems(ToolMenu);
  FillMenuItems(HelpMenu);

  SetMenuItemBitmap(N23, FileBtn1);
  SetMenuItemBitmap(N19, FileBtn2);
  SetMenuItemBitmap(N20, FileBtn3);
  SetMenuItemBitmap(N39, FileBtn4);
  SetMenuItemBitmap(N10, ExitB);

  SetMenuItemBitmap(N46, UndoB);
  SetMenuItemBitmap(N48, RedoB);
  SetMenuItemBitmap(N11, CutB);
  SetMenuItemBitmap(N12, CopyB);
  SetMenuItemBitmap(N13, PstB);
  SetMenuItemBitmap(N28, SelAllB);
  SetMenuItemBitmap(N29, PgB1);
  SetMenuItemBitmap(N30, PgB2);
  SetMenuItemBitmap(N32, ZB1);
  SetMenuItemBitmap(N33, ZB2);
  SetMenuItemBitmap(N35, HelpBtn);
  for i := 0 to MastMenu.Count - 1 do
    SetMenuItemBitmap(MastMenu.Items[i], Panel6.Controls[i + 1] as TfrTBButton);

  SetMenuItemBitmap(N41, PgB1);
  SetMenuItemBitmap(N43, PgB2);
  SetMenuItemBitmap(N44, PgB3);
end;

function TfrDesignerForm.FindMenuItem(AMenuItem: TMenuItem): TfrMenuItemInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MenuItems.Count - 1 do
    if TfrMenuItemInfo(MenuItems[i]).MenuItem = AMenuItem then
    begin
      Result := TfrMenuItemInfo(MenuItems[i]);
      Exit;
    end;
end;

procedure TfrDesignerForm.SetMenuItemBitmap(AMenuItem: TMenuItem; ABtn: TfrSpeedButton);
var
  m: TfrMenuItemInfo;
begin
  m := FindMenuItem(AMenuItem);
  if m = nil then
  begin
    m := TfrMenuItemInfo.Create;
    m.MenuItem := AMenuItem;
    MenuItems.Add(m);
  end;
  m.Btn := ABtn;
  ModifyMenu(AMenuItem.Parent.Handle, AMenuItem.MenuIndex,
    MF_BYPOSITION + MF_OWNERDRAW, AMenuItem.Command, nil);
end;

procedure TfrDesignerForm.FillMenuItems(MenuItem: TMenuItem);
var
  i: Integer;
  m: TMenuItem;
begin
  for i := 0 to MenuItem.Count - 1 do
  begin
    m := MenuItem.Items[i];
    SetMenuItemBitmap(m, nil);
    if m.Count > 0 then FillMenuItems(m);
  end;
end;

procedure TfrDesignerForm.DeleteMenuItems(MenuItem: TMenuItem);
var
  i, j: Integer;
  m: TMenuItem;
begin
  for i := 0 to MenuItem.Count - 1 do
  begin
    m := MenuItem.Items[i];
    for j := 0 to MenuItems.Count - 1 do
    if TfrMenuItemInfo(MenuItems[j]).MenuItem = m then
    begin
      TfrMenuItemInfo(MenuItems[j]).Free;
      MenuItems.Delete(j);
      break;
    end;
  end;
end;

procedure TfrDesignerForm.DoDrawText(Canvas: TCanvas; Caption: string;
  Rect: TRect; Selected, Enabled: Boolean; Flags: Longint);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if not Enabled then
    begin
      if not Selected then
      begin
        OffsetRect(Rect, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Caption), Length(Caption), Rect, Flags);
        OffsetRect(Rect, -1, -1);
      end;
      Font.Color := clBtnShadow;
    end;
    DrawText(Handle, PChar(Caption), Length(Caption), Rect, Flags);
  end;
end;

procedure TfrDesignerForm.DrawItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
var
  GlyphRect: TRect;
  Btn: TfrSpeedButton;
  Glyph: TBitmap;
begin
  MaxItemWidth := 0;
  MaxShortCutWidth := 0;
  with ACanvas do
  begin
    if Selected then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText
    end
    else
    begin
      Brush.Color := clMenu;
      Font.Color := clMenuText;
    end;
    if AMenuItem.Caption <> '-' then
    begin
      FillRect(ARect);
      Btn := FindMenuItem(AMenuItem).Btn;
      GlyphRect := Bounds(ARect.Left + 1, ARect.Top + (ARect.Bottom - ARect.Top - 16) div 2, 16, 16);

      if AMenuItem.Checked then
      begin
        Glyph := TBitmap.Create;
        if AMenuItem.RadioItem then
        begin
          Glyph.Handle := LoadBitmap(hInstance, 'FR_RADIO');
          BrushCopy(GlyphRect, Glyph, Rect(0, 0, 16, 16), Glyph.TransparentColor);
        end
        else
        begin
          Glyph.Handle := LoadBitmap(hInstance, 'FR_CHECK');
          Draw(GlyphRect.Left, GlyphRect.Top, Glyph);
        end;
        Glyph.Free;
      end
      else if Btn <> nil then
      begin
        Glyph := TBitmap.Create;
        Glyph.Width := 16; Glyph.Height := 16;
        Btn.DrawGlyph(Glyph.Canvas, 0, 0, AMenuItem.Enabled);
        BrushCopy(GlyphRect, Glyph, Rect(0, 0, 16, 16), Glyph.TransparentColor);
        Glyph.Free;
      end;
      ARect.Left := GlyphRect.Right + 4;
    end;

    if AMenuItem.Caption <> '-' then
    begin
      OffsetRect(ARect, 0, 2);
      DoDrawText(ACanvas, AMenuItem.Caption, ARect, Selected, AMenuItem.Enabled, DT_LEFT);
      if AMenuItem.ShortCut <> 0 then
      begin
        ARect.Left := StrToInt(ItemWidths.Values[AMenuItem.Parent.Name]) + 6;
        DoDrawText(ACanvas, ShortCutToText(AMenuItem.ShortCut), ARect,
          Selected, AMenuItem.Enabled, DT_LEFT);
      end;
    end
    else
    begin
      Inc(ARect.Top, 4);
      DrawEdge(Handle, ARect, EDGE_ETCHED, BF_TOP);
    end;
  end;
end;

procedure TfrDesignerForm.MeasureItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  w: Integer;
begin
  w := ACanvas.TextWidth(AMenuItem.Caption) + 31;
  if MaxItemWidth < w then
    MaxItemWidth := w;
  ItemWidths.Values[AMenuItem.Parent.Name] := IntToStr(MaxItemWidth);

  if AMenuItem.ShortCut <> 0 then
  begin
    w := ACanvas.TextWidth(ShortCutToText(AMenuItem.ShortCut)) + 15;
    if MaxShortCutWidth < w then
      MaxShortCutWidth := w;
  end;

  if frGetWindowsVersion = '98' then
    AWidth := MaxItemWidth else
    AWidth := MaxItemWidth + MaxShortCutWidth;
  if AMenuItem.Caption <> '-' then
    AHeight := 19 else
    AHeight := 10;
end;

procedure TfrDesignerForm.WndProc(var Message: TMessage);
var
  MenuItem: TMenuItem;
  CCanvas: TCanvas;

  function FindItem(ItemId: Integer): TMenuItem;
  begin
    Result := MainMenu1.FindItem(ItemID, fkCommand);
    if Result = nil then
      Result := Popup1.FindItem(ItemID, fkCommand);
    if Result = nil then
      Result := Popup2.FindItem(ItemID, fkCommand);
  end;

begin
  case Message.Msg of
    WM_COMMAND:
      if Popup1.DispatchCommand(Message.wParam) or
         Popup2.DispatchCommand(Message.wParam) then Exit;
    WM_INITMENUPOPUP:
      with TWMInitMenuPopup(Message) do
        if Popup1.DispatchPopup(MenuPopup) or
           Popup2.DispatchPopup(MenuPopup) then Exit;
    WM_DRAWITEM:
      with PDrawItemStruct(Message.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and (Message.WParam = 0) then
        begin
          MenuItem := FindItem(ItemId);
          if MenuItem <> nil then
          begin
            CCanvas := TControlCanvas.Create;
            with CCanvas do
            begin
              Handle := hDC;
              DrawItem(MenuItem, CCanvas, rcItem, ItemState and ODS_SELECTED <> 0);
              Free;
            end;
            Exit;
          end;
        end;
      end;
    WM_MEASUREITEM:
      with PMeasureItemStruct(Message.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and (Message.WParam = 0) then
        begin
          MenuItem := FindItem(ItemId);
          if MenuItem <> nil then
          begin
            MeasureItem(MenuItem, Canvas, Integer(ItemWidth), Integer(ItemHeight));
            Exit;
          end;
        end;
      end;
  end;
  inherited WndProc(Message);
end;


{----------------------------------------------------------------------------}
// alignment palette
function GetFirstSelected: TfrView;
begin
  if FirstSelected <> nil then
    Result := FirstSelected else
    Result := Objects[TopSelected];
end;

function GetLeftObject: Integer;
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  t := Objects[TopSelected];
  x := t.x;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      if t.x < x then
      begin
        x := t.x;
        Result := i;
      end;
  end;
end;

function GetRightObject: Integer;
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  t := Objects[TopSelected];
  x := t.x + t.dx;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      if t.x + t.dx > x then
      begin
        x := t.x + t.dx;
        Result := i;
      end;
  end;
end;

function GetTopObject: Integer;
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  t := Objects[TopSelected];
  y := t.y;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      if t.y < y then
      begin
        y := t.y;
        Result := i;
      end;
  end;
end;

function GetBottomObject: Integer;
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  t := Objects[TopSelected];
  y := t.y + t.dy;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      if t.y + t.dy > y then
      begin
        y := t.y + t.dy;
        Result := i;
      end;
  end;
end;

procedure TfrDesignerForm.Align1Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.x := x;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align6Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.y := y;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align5Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x+t.dx;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.x := x - t.dx;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align10Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y + t.dy;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.y := y - t.dy;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align2Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x + t.dx div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.x := x - t.dx div 2;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align7Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y + t.dy div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
      t.y := y - t.dy div 2;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum = 0 then Exit;
  BeforeChange;
  t := Objects[GetLeftObject];
  x := t.x;
  t := Objects[GetRightObject];
  x := x + (t.x + t.dx - x - Page.PrnInfo.Pgw) div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then Dec(t.x, x);
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align8Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum = 0 then Exit;
  BeforeChange;
  t := Objects[GetTopObject];
  y := t.y;
  t := Objects[GetBottomObject];
  y := y + (t.y + t.dy - y - Page.PrnInfo.Pgh) div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then Dec(t.y, y);
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align4Click(Sender: TObject);
var
  s: TStringList;
  i, dx: Integer;
  t: TfrView;
begin
  if SelNum < 3 then Exit;
  BeforeChange;
  s := TStringList.Create;
  s.Sorted := True;
  s.Duplicates := dupAccept;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then s.AddObject(Format('%4.4d', [t.x]), t);
  end;
  dx := (TfrView(s.Objects[s.Count - 1]).x - TfrView(s.Objects[0]).x) div (s.Count - 1);
  for i := 1 to s.Count - 2 do
    TfrView(s.Objects[i]).x := TfrView(s.Objects[i-1]).x + dx;
  s.Free;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align9Click(Sender: TObject);
var
  s: TStringList;
  i, dy: Integer;
  t: TfrView;
begin
  if SelNum < 3 then Exit;
  BeforeChange;
  s := TStringList.Create;
  s.Sorted := True;
  s.Duplicates := dupAccept;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then s.AddObject(Format('%4.4d', [t.y]), t);
  end;
  dy := (TfrView(s.Objects[s.Count - 1]).y - TfrView(s.Objects[0]).y) div (s.Count - 1);
  for i := 1 to s.Count - 2 do
    TfrView(s.Objects[i]).y := TfrView(s.Objects[i - 1]).y + dy;
  s.Free;
  PageView.GetMultipleSelected;
  RedrawPage;
end;


{----------------------------------------------------------------------------}
// miscellaneous
function Objects: TList;
begin
  Result := frDesigner.Page.Objects;
end;

procedure frSetGlyph(Color: TColor; sb: TfrSpeedButton; n: Integer);
var
  b: TBitmap;
  r: TRect;
  i, j: Integer;
begin
  b := TBitmap.Create;
  b.Width := 32;
  b.Height := 16;
  with b.Canvas do
  begin
    r := Rect(n * 32, 0, n * 32 + 32, 16);
    CopyRect(Rect(0, 0, 32, 16), TfrDesignerForm(frDesigner).Image1.Picture.Bitmap.Canvas, r);
    for i := 0 to 32 do
      for j := 0 to 16 do
        if Pixels[i, j] = clRed then
          Pixels[i, j] := Color;
    if Color = clNone then
      for i := 1 to 14 do
        Pixels[i, 13] := clBtnFace;
  end;
  sb.Glyph.Assign(b);
  sb.NumGlyphs := 2;
  b.Free;
end;

function TopSelected: Integer;
var
  i: Integer;
begin
  Result := Objects.Count - 1;
  for i := Objects.Count - 1 downto 0 do
    if TfrView(Objects[i]).Selected then
    begin
      Result := i;
      break;
    end;
end;

function frCheckBand(b: TfrBandType): Boolean;
var
  i: Integer;
  t: TfrView;
begin
  Result := False;
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Typ = gtBand then
      if b = TfrBandType(t.FrameTyp) then
      begin
        Result := True;
        break;
      end;
  end;
end;

function GetUnusedBand: TfrBandType;
var
  b: TfrBandType;
begin
  Result := btNone;
  for b := btReportTitle to btNone do
    if not frCheckBand(b) then
    begin
      Result := b;
      break;
    end;
  if Result = btNone then Result := btMasterData;
end;

procedure SendBandsToDown;
var
  i, j, n, k: Integer;
  t: TfrView;
begin
  n := Objects.Count; j := 0; i := n - 1;
  k := 0;
  while j < n do
  begin
    t := Objects[i];
    if t.Typ = gtBand then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
      Inc(k);
    end else Dec(i);
    Inc(j);
  end;
  for i := 0 to n - 1 do // sends btOverlay to back
  begin
    t := Objects[i];
    if (t.Typ = gtBand) and (t.FrameTyp = Integer(btOverlay)) then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
      break;
    end;
  end;
  i := 0; j := 0;
  while j < n do // sends btCrossXXX to front
  begin
    t := Objects[i];
    if (t.Typ = gtBand) and
       (TfrBandType(t.FrameTyp) in [btCrossHeader..btCrossFooter]) then
    begin
      Objects.Delete(i);
      Objects.Insert(k - 1, t);
    end
    else Inc(i);
    Inc(j);
  end;
end;

procedure ClearClipBoard;
var
  t: TfrView;
begin
  if Assigned(ClipBd) then
    with ClipBd do
    while Count > 0 do
    begin
      t := Items[0];
      t.Free;
      Delete(0);
    end;
end;

procedure GetRegion;
var
  i: Integer;
  t: TfrView;
  R: HRGN;
begin
  ClipRgn := CreateRectRgn(0, 0, 0, 0);
  for i := 0 to Objects.Count - 1 do
  begin
    t := Objects[i];
    if t.Selected then
    begin
      R := t.GetClipRgn(rtExtended);
      CombineRgn(ClipRgn, ClipRgn, R, RGN_OR);
      DeleteObject(R);
    end;
  end;
  FirstChange := False;
end;

procedure TfrDesignerForm.GetDefaultSize(var dx, dy: Integer);
begin
  dx := 96;
  if GridSize = 18 then dx := 18 * 6;
  dy := 18;
  if GridSize = 18 then dy := 18;
  if LastFontSize in [12, 13] then dy := 20;
  if LastFontSize in [14..16] then dy := 24;
end;


procedure TfrDesignerForm.SB1Click(Sender: TObject);
var
  d: Double;
begin
  d := StrToFloat(E1.Text);
  d := d + 1;
  E1.Text := FloatToStrF(d, ffGeneral, 2, 2);
  DoClick(E1);
end;

procedure TfrDesignerForm.SB2Click(Sender: TObject);
var
  d: Double;
begin
  d := StrToFloat(E1.Text);
  d := d - 1;
  if d <= 0 then d := 1;
  E1.Text := FloatToStrF(d, ffGeneral, 2, 2);
  DoClick(E1);
end;

type
  THackBtn = class(TfrSpeedButton)
  end;

procedure TfrDesignerForm.HelpBtnClick(Sender: TObject);
begin
  HelpBtn.Down := True;
  Screen.Cursor := crHelp;
  SetCapture(Handle);
  THackBtn(HelpBtn).FMouseInControl := False;
  HelpBtn.Invalidate;
end;

procedure TfrDesignerForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c: TControl;
  t: Integer;
begin
  HelpBtn.Down := False;
  Screen.Cursor := crDefault;
  c := frControlAtPos(Self, Point(X, Y));
  if (c <> nil) and (c <> HelpBtn) then
  begin
    t := c.Tag;
    if (c.Parent = Panel4) and (t > 4) then
      t := 5;
    if c.Parent = Panel4 then
      Inc(t, 430) else
      Inc(t, 400);
    Application.HelpCommand(HELP_CONTEXTPOPUP, t);
  end;
end;

procedure TfrDesignerForm.N22Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TfrDesignerForm.OnActivateApp(Sender: TObject);
  procedure SetZOrder(Form: TForm);
  begin
    SetWindowPos(Form.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
  end;
begin
  SetZOrder(InspForm);
  if Panel1.IsFloat then SetZOrder(Panel1.FloatWindow);
  if Panel2.IsFloat then SetZOrder(Panel2.FloatWindow);
  if Panel3.IsFloat then SetZOrder(Panel3.FloatWindow);
  if Panel4.IsFloat then SetZOrder(Panel4.FloatWindow);
  if Panel5.IsFloat then SetZOrder(Panel5.FloatWindow);
  if Panel6.IsFloat then SetZOrder(Panel6.FloatWindow);
end;

procedure TfrDesignerForm.OnDeactivateApp(Sender: TObject);
  procedure SetZOrder(Form: TForm);
  begin
    SetWindowPos(Form.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
  end;
begin
  if not Visible then Exit;
  SetZOrder(InspForm);
  if Panel1.IsFloat then SetZOrder(Panel1.FloatWindow);
  if Panel2.IsFloat then SetZOrder(Panel2.FloatWindow);
  if Panel3.IsFloat then SetZOrder(Panel3.FloatWindow);
  if Panel4.IsFloat then SetZOrder(Panel4.FloatWindow);
  if Panel5.IsFloat then SetZOrder(Panel5.FloatWindow);
  if Panel6.IsFloat then SetZOrder(Panel6.FloatWindow);
end;


initialization
  frDesigner := TfrDesignerForm.Create(nil);
  ClipBd := TList.Create;
  GridBitmap := TBitmap.Create;
  with GridBitmap do
  begin
    Width := 8; Height := 8;
  end;
  LastFrameTyp := 0;
  LastFrameWidth := 1;
  LastLineWidth := 2;
  LastFillColor := clNone;
  LastFrameColor := clBlack;
  LastFontColor := clBlack;
  LastFontStyle := 0;
  LastAdjust := 0;
  RegRootKey := 'Software\FastReport\' + Application.Title;

finalization
  frDesigner.Free;
  ClearClipBoard;
  ClipBd.Free;
  GridBitmap.Free;

end.

