{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TvgDBTreeCombo                                }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgTreeCm;

interface

uses
  Windows, Messages, Classes, Menus, Graphics, Controls, ToolEdit,
  ComCtrls, DB, vgCtrls, vgDBTree, Forms {$IFDEF _D4_}, ImgList {$ENDIF};

type
  TDBTreeWindow = class;

  TvgDBTreeCombo = class(TCustomComboEdit)
  private
    { Private declarations }
    FCanvas: TControlCanvas;
    FBorderStyle: TBorderStyle;
    FStreamedFieldID, FStreamedFieldParentID,
    FStreamedFieldText, FStreamedRootID: String;
    FDropDownHeight, FDropDownWidth: Integer;
    FTreeView: TvgDBTreeView;
    FSelected: TTreeNode;
    FNodeText: String;
    FOnSetRange: TSetRangeEvent;
    FOnCancelRange: TCancelRangeEvent;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DoSetRange(Sender: TObject; DataSet: TDataSet; ParentID: String);
    procedure DoCancelRange(Sender: TObject; DataSet: TDataSet);
    function GetPopupColor: TColor;
    procedure SetPopupColor(Value: TColor);
    procedure SetDropDownHeight(Value: Integer);
    procedure SetDropDownWidth(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure UpdateBounds;
    procedure SetSelection(Node: TTreeNode);
    function GetTextMargins: TPoint;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    { TreeView prperties }
    function GetTreeViewDataFieldID: String;
    procedure SetTreeViewDataFieldID(Value: String);
    function GetTreeViewDataFieldParentID: String;
    procedure SetTreeViewDataFieldParentID(Value: String);
    function GetTreeViewDataFieldText: String;
    procedure SetTreeViewDataFieldText(Value: String);
    function GetTreeViewDataSource: TDataSource;
    procedure SetTreeViewDataSource(Value: TDataSource);
    function GetTreeViewRootID: String;
    procedure SetTreeViewRootID(Value: String);
    function GetTreeViewOnCompareNode: TCompareNodeEvent;
    procedure SetTreeViewOnCompareNode(Value: TCompareNodeEvent);
    function GetTreeViewOnCreateNode: TCreateNodeEvent;
    procedure SetTreeViewOnCreateNode(Value: TCreateNodeEvent);
    function GetTreeViewOnDestroyNode: TNodeEvent;
    procedure SetTreeViewOnDestroyNode(Value: TNodeEvent);
    function GetTreeViewOnProcessBranches: TProcessNodeEvent;
    procedure SetTreeViewOnProcessBranches(Value: TProcessNodeEvent);
    function GetTreeViewOnUpdateNode: TNodeEvent;
    procedure SetTreeViewOnUpdateNode(Value: TNodeEvent);
    function GetTreeViewShowButtons: Boolean;
    procedure SetTreeViewShowButtons(Value: Boolean);
    function GetTreeViewShowLines: Boolean;
    procedure SetTreeViewShowLines(Value: Boolean);
    function GetTreeViewShowRoot: Boolean;
    procedure SetTreeViewShowRoot(Value: Boolean);
    function GetTreeViewReadOnly: Boolean;
    procedure SetTreeViewReadOnly(Value: Boolean);
    function GetTreeViewHideSelection: Boolean;
    procedure SetTreeViewHideSelection(Value: Boolean);
    function GetTreeViewIndent: Integer;
    procedure SetTreeViewIndent(Value: Integer);
    function GetTreeViewOnEditing: TTVEditingEvent;
    procedure SetTreeViewOnEditing(Value: TTVEditingEvent);
    function GetTreeViewOnEdited: TTVEditedEvent;
    procedure SetTreeViewOnEdited(Value: TTVEditedEvent);
    function GetTreeViewOnExpanding: TTVExpandingEvent;
    procedure SetTreeViewOnExpanding(Value: TTVExpandingEvent);
    function GetTreeViewOnExpanded: TTVExpandedEvent;
    procedure SetTreeViewOnExpanded(Value: TTVExpandedEvent);
    function GetTreeViewOnCollapsing: TTVCollapsingEvent;
    procedure SetTreeViewOnCollapsing(Value: TTVCollapsingEvent);
    function GetTreeViewOnCollapsed: TTVExpandedEvent;
    procedure SetTreeViewOnCollapsed(Value: TTVExpandedEvent);
    function GetTreeViewOnChanging: TTVChangingEvent;
    procedure SetTreeViewOnChanging(Value: TTVChangingEvent);
    function GetTreeViewOnChange: TTVChangedEvent;
    procedure SetTreeViewOnChange(Value: TTVChangedEvent);
    function GetTreeViewOnGetImageIndex: TTVExpandedEvent;
    procedure SetTreeViewOnGetImageIndex(Value: TTVExpandedEvent);
    function GetTreeViewOnGetSelectedIndex: TTVExpandedEvent;
    procedure SetTreeViewOnGetSelectedIndex(Value: TTVExpandedEvent);
    function GetTreeViewColor: TColor;
    procedure SetTreeViewColor(Value: TColor);
    function GetTreeViewSortType: TSortType;
    procedure SetTreeViewSortType(Value: TSortType);
    function GetTreeViewOnClick: TNotifyEvent;
    procedure SetTreeViewOnClick(Value: TNotifyEvent);
    function GetTreeViewOnEnter: TNotifyEvent;
    procedure SetTreeViewOnEnter(Value: TNotifyEvent);
    function GetTreeViewOnExit: TNotifyEvent;
    procedure SetTreeViewOnExit(Value: TNotifyEvent);
    function GetTreeViewOnDblClick: TNotifyEvent;
    procedure SetTreeViewOnDblClick(Value: TNotifyEvent);
    function GetTreeViewOnKeyPress: TKeyPressEvent;
    procedure SetTreeViewOnKeyPress(Value: TKeyPressEvent);
    function GetTreeViewPopupMenu: TPopupMenu;
    procedure SetTreeViewPopupMenu(Value: TPopupMenu);
    function GetTreeViewImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF};
    procedure SetTreeViewImages(Value: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF});
    function GetTreeViewStateImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF};
    procedure SetTreeViewStateImages(Value: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF});
    function StoredTreeViewRootID: Boolean;
    function GetTreeViewOptions: TvgDBTreeOptions;
    procedure SetTreeViewOptions(Value: TvgDBTreeOptions);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property DBTreeView: TvgDBTreeView read FTreeView;
    property Selected: TTreeNode read FSelected;
  published
{$IFDEF _D3_}
{$IFDEF _D4_}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
    property ImeMode;
    property ImeName;
{$ENDIF}
    { Published declarations }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property PopupColor: TColor read GetPopupColor write SetPopupColor
      default clBtnFace;
    property DropDownHeight: Integer read FDropDownHeight write SetDropDownHeight;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth;
    property Alignment;
    property AutoSelect;
    property ButtonHint;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property MaxLength;
    property NumGlyphs;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  { TreeView properties }
    property DataFieldID: String read GetTreeViewDataFieldID write SetTreeViewDataFieldID;
    property DataFieldParentID: String read GetTreeViewDataFieldParentID write SetTreeViewDataFieldParentID;
    property DataFieldText: String read GetTreeViewDataFieldText write SetTreeViewDataFieldText;
    property DataSource: TDataSource read GetTreeViewDataSource write SetTreeViewDataSource;
    property Options: TvgDBTreeOptions read GetTreeViewOptions write SetTreeViewOptions;
    property TreeViewRootID: String read GetTreeViewRootID write SetTreeViewRootID stored StoredTreeViewRootID;
    property TreeViewOnSetRange: TSetRangeEvent read FOnSetRange write FOnSetRange;
    property TreeViewOnCancelRange: TCancelRangeEvent read FOnCancelRange write FOnCancelRange;
    property TreeViewOnCompareNode: TCompareNodeEvent read GetTreeViewOnCompareNode write SetTreeViewOnCompareNode;
    property TreeViewOnCreateNode: TCreateNodeEvent read GetTreeViewOnCreateNode write SetTreeViewOnCreateNode;
    property TreeViewOnDestroyNode: TNodeEvent read GetTreeViewOnDestroyNode write SetTreeViewOnDestroyNode;
    property TreeViewOnProcessBranches: TProcessNodeEvent read GetTreeViewOnProcessBranches write SetTreeViewOnProcessBranches;
    property TreeViewOnUpdateNode: TNodeEvent read GetTreeViewOnUpdateNode write SetTreeViewOnUpdateNode;
  { inherited TreeView Control properties }
    property TreeViewShowButtons: Boolean read GetTreeViewShowButtons write SetTreeViewShowButtons;
    property TreeViewShowLines: Boolean read GetTreeViewShowLines write SetTreeViewShowLines;
    property TreeViewShowRoot: Boolean read GetTreeViewShowRoot write SetTreeViewShowRoot;
    property TreeViewReadOnly: Boolean read GetTreeViewReadOnly write SetTreeViewReadOnly default True;
    property TreeViewHideSelection: Boolean read GetTreeViewHideSelection write SetTreeViewHideSelection default False;
    property TreeViewIndent: Integer read GetTreeViewIndent write SetTreeViewIndent;
    property TreeViewOnEditing: TTVEditingEvent read GetTreeViewOnEditing write SetTreeViewOnEditing;
    property TreeViewOnEdited: TTVEditedEvent read GetTreeViewOnEdited write SetTreeViewOnEdited;
    property TreeViewOnExpanding: TTVExpandingEvent read GetTreeViewOnExpanding write SetTreeViewOnExpanding;
    property TreeViewOnExpanded: TTVExpandedEvent read GetTreeViewOnExpanded write SetTreeViewOnExpanded;
    property TreeViewOnCollapsing: TTVCollapsingEvent read GetTreeViewOnCollapsing write SetTreeViewOnCollapsing;
    property TreeViewOnCollapsed: TTVExpandedEvent read GetTreeViewOnCollapsed write SetTreeViewOnCollapsed;
    property TreeViewOnChanging: TTVChangingEvent read GetTreeViewOnChanging write SetTreeViewOnChanging;
    property TreeViewOnChange: TTVChangedEvent read GetTreeViewOnChange write SetTreeViewOnChange;
    property TreeViewOnGetImageIndex: TTVExpandedEvent read GetTreeViewOnGetImageIndex write SetTreeViewOnGetImageIndex;
    property TreeViewOnGetSelectedIndex: TTVExpandedEvent read GetTreeViewOnGetSelectedIndex write SetTreeViewOnGetSelectedIndex;
    property TreeViewColor: TColor read GetTreeViewColor write SetTreeViewColor;
    property TreeViewSortType: TSortType read GetTreeViewSortType write SetTreeViewSortType;
    property TreeViewOnClick: TNotifyEvent read GetTreeViewOnClick write SetTreeViewOnClick;
    property TreeViewOnEnter: TNotifyEvent read GetTreeViewOnEnter write SetTreeViewOnEnter;
    property TreeViewOnExit: TNotifyEvent read GetTreeViewOnExit write SetTreeViewOnExit;
    property TreeViewOnDblClick: TNotifyEvent read GetTreeViewOnDblClick write SetTreeViewOnDblClick;
    property TreeViewOnKeyPress: TKeyPressEvent read GetTreeViewOnKeyPress write SetTreeViewOnKeyPress;
    property TreeViewPopupMenu: TPopupMenu read GetTreeViewPopupMenu write SetTreeViewPopupMenu;
    property TreeViewImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF} read GetTreeViewImages write SetTreeViewImages;
    property TreeViewStateImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF} read GetTreeViewStateImages write SetTreeViewStateImages;
{$IFDEF _D4_}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

  TDBTreeWindow = class(TPopupWindow)
  private
    Combo: TvgDBTreeCombo;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
{$IFDEF RX240}
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
{$ELSE}
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Color;
  end;

implementation
uses vgVclUtl;

type
  TControlHack = class(TControl);

  TvgComboTreeView = class(TvgDBTreeView)
  private
    FPopup: TDBTreeWindow;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
     X, Y: Integer); override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure ActiveChanged; override;
    procedure Change(Node: TTreeNode); override;
  end;


constructor TvgDBTreeCombo.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crArrow;
  FBorderStyle := bsSingle;
  ControlState := ControlState + [csCreating];
  try
    GlyphKind := ToolEdit.gkDropDown;
  finally
    ControlState := ControlState - [csCreating];
  end;
  TabStop := False;
  DirectInput := False;
  FPopup := TDBTreeWindow.Create(Self);
  TDBTreeWindow(FPopup).Combo := Self;
  TDBTreeWindow(FPopup).Color := clBtnFace;
  FStreamedRootID := NullValue;
  FTreeView := TvgComboTreeView.Create(Self);
  FTreeView.Parent := FPopup;
  TvgComboTreeView(FTreeView).FPopup := TDBTreeWindow(FPopup);
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.HideSelection := False;
  FTreeView.ReadOnly := True;
  FTreeView.OnSetRange := DoSetRange;
  FTreeView.OnCancelRange := DoCancelRange;
end;

procedure TvgDBTreeCombo.CMFontChanged(var Message: TMessage);
begin
  FTreeView.Font.Assign(Self.Font);
  inherited;
end;

procedure TvgDBTreeCombo.DoSetRange(Sender: TObject; DataSet: TDataSet; ParentID: String);
begin
  if Assigned(FOnSetRange) then
    FOnSetRange(Sender, DataSet, ParentID) else
    FTreeView.DefaultSetRange(DataSet, ParentID);
end;

procedure TvgDBTreeCombo.DoCancelRange(Sender: TObject; DataSet: TDataSet);
begin
  if Assigned(FOnCancelRange) then
    FOnCancelRange(Sender, DataSet) else
    FTreeView.DefaultCancelRange(DataSet);
end;

function TvgDBTreeCombo.GetPopupColor: TColor;
begin
  Result := TControlHack(FPopup).Color;
end;

procedure TvgDBTreeCombo.SetPopupColor(Value: TColor);
begin
  TControlHack(FPopup).Color := Value;
end;

destructor TvgDBTreeCombo.Destroy;
begin
  FTreeView.Free;
  FTreeView := nil;
  FPopup.Free;
  FPopup := nil;
  FCanvas.Free;
  inherited;
end;

procedure TvgDBTreeCombo.Invalidate;
begin
  inherited;
  if HandleAllocated then HideCaret(Handle);
end;

procedure TvgDBTreeCombo.CreateWnd;
begin
  inherited;
  FTreeView.HandleNeeded;
end;

procedure TvgDBTreeCombo.Loaded;
begin
  inherited;
  FTreeView.HandleNeeded;
  TreeViewRootID := FStreamedRootID;
  DataFieldID := FStreamedFieldID;
  DataFieldParentID := FStreamedFieldParentID;
  DataFieldText := FStreamedFieldText;
end;

function TvgDBTreeCombo.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TvgDBTreeCombo.WMPaint(var Message: TWMPaint);
var
  Margins: TPoint;
  R, ImageRect: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
begin
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;

      ImageRect.Right := 0;
      if Assigned(TreeViewImages) and Assigned(FSelected) then
      begin
        ImageRect := R;
        ImageRect.Left := ImageRect.Left + 2;
        ImageRect.Right := ImageRect.Left + TreeViewImages.Width + 2;
        R.Left := ImageRect.Right;
        DrawCellImage(FCanvas, TreeViewImages, FSelected.SelectedIndex, ImageRect, 0);
      end;

      S := FNodeText;
      Margins := GetTextMargins;
      TextRect(R, ImageRect.Right + Margins.X, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TvgDBTreeCombo.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TvgDBTreeCombo.SetDropDownHeight(Value: Integer);
begin
  FDropDownHeight := Value;
  UpdateBounds;
end;

procedure TvgDBTreeCombo.SetDropDownWidth(Value: Integer);
begin
  FDropDownWidth := Value;
  UpdateBounds;
end;

procedure TvgDBTreeCombo.UpdateBounds;
begin
  if FDropDownWidth = 0 then
    FPopup.Width := Self.Width
  else
    FPopup.Width := FDropDownWidth;

  if FDropDownHeight = 0 then
    FPopup.Height := 200
  else
    FPopup.Height := FDropDownHeight;
end;

procedure TvgDBTreeCombo.SetSelection(Node: TTreeNode);
begin
  FSelected := Node;
  if Assigned(Node) then
    FNodeText := Node.Text
  else
    FNodeText := '';
  Invalidate;
end;

procedure TvgDBTreeCombo.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csLoading in ComponentState) then UpdateBounds;
end;

procedure TvgDBTreeCombo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  HideCaret(Handle);
end;

procedure TvgDBTreeCombo.WMKillFocus(var Message: TWMKillFocus);
begin
  if Message.FocusedWnd = FPopup.Handle then
    Message.Result := 0
  else
    inherited;
end;

function TvgDBTreeCombo.GetTreeViewDataFieldID: String;
begin
  Result := FTreeView.DataFieldID;
end;

procedure TvgDBTreeCombo.SetTreeViewDataFieldID(Value: String);
begin
  if (csLoading in ComponentState) then
    FStreamedFieldID := Value
  else
    FTreeView.DataFieldID := Value;
end;

function TvgDBTreeCombo.GetTreeViewDataFieldParentID: String;
begin
  Result := FTreeView.DataFieldParentID;
end;

procedure TvgDBTreeCombo.SetTreeViewDataFieldParentID(Value: String);
begin
  if (csLoading in ComponentState) then
    FStreamedFieldParentID := Value
  else
    FTreeView.DataFieldParentID := Value;
end;

function TvgDBTreeCombo.GetTreeViewDataFieldText: String;
begin
  Result := FTreeView.DataFieldText;
end;

procedure TvgDBTreeCombo.SetTreeViewDataFieldText(Value: String);
begin
  if (csLoading in ComponentState) then
    FStreamedFieldText := Value
  else
    FTreeView.DataFieldText := Value;
end;

function TvgDBTreeCombo.GetTreeViewDataSource: TDataSource;
begin
  Result := FTreeView.DataSource;
end;

procedure TvgDBTreeCombo.SetTreeViewDataSource(Value: TDataSource);
begin
  FTreeView.DataSource := Value;
end;

function TvgDBTreeCombo.GetTreeViewRootID: String;
begin
  Result := FTreeView.RootID;
end;

procedure TvgDBTreeCombo.SetTreeViewRootID(Value: String);
begin
  FTreeView.RootID := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnCompareNode: TCompareNodeEvent;
begin
  Result := FTreeView.OnCompareNode;
end;

procedure TvgDBTreeCombo.SetTreeViewOnCompareNode(Value: TCompareNodeEvent);
begin
  FTreeView.OnCompareNode := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnCreateNode: TCreateNodeEvent;
begin
  Result := FTreeView.OnCreateNode;
end;

procedure TvgDBTreeCombo.SetTreeViewOnCreateNode(Value: TCreateNodeEvent);
begin
  FTreeView.OnCreateNode := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnDestroyNode: TNodeEvent;
begin
  Result := FTreeView.OnDestroyNode;
end;

procedure TvgDBTreeCombo.SetTreeViewOnDestroyNode(Value: TNodeEvent);
begin
  FTreeView.OnDestroyNode := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnProcessBranches: TProcessNodeEvent;
begin
  Result := FTreeView.OnProcessBranches;
end;

procedure TvgDBTreeCombo.SetTreeViewOnProcessBranches(Value: TProcessNodeEvent);
begin
  FTreeView.OnProcessBranches := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnUpdateNode: TNodeEvent;
begin
  Result := FTreeView.OnUpdateNode;
end;

procedure TvgDBTreeCombo.SetTreeViewOnUpdateNode(Value: TNodeEvent);
begin
  FTreeView.OnUpdateNode := Value;
end;

function TvgDBTreeCombo.GetTreeViewShowButtons: Boolean;
begin
  Result := FTreeView.ShowButtons;
end;

procedure TvgDBTreeCombo.SetTreeViewShowButtons(Value: Boolean);
begin
  FTreeView.ShowButtons := Value;
end;

function TvgDBTreeCombo.GetTreeViewShowLines: Boolean;
begin
  Result := FTreeView.ShowLines;
end;

procedure TvgDBTreeCombo.SetTreeViewShowLines(Value: Boolean);
begin
  FTreeView.ShowLines := Value;
end;

function TvgDBTreeCombo.GetTreeViewShowRoot: Boolean;
begin
  Result := FTreeView.ShowRoot;
end;

procedure TvgDBTreeCombo.SetTreeViewShowRoot(Value: Boolean);
begin
  FTreeView.ShowRoot := Value;
end;

function TvgDBTreeCombo.GetTreeViewReadOnly: Boolean;
begin
  Result := FTreeView.ReadOnly;
end;

procedure TvgDBTreeCombo.SetTreeViewReadOnly(Value: Boolean);
begin
  FTreeView.ReadOnly := Value;
end;

function TvgDBTreeCombo.GetTreeViewHideSelection: Boolean;
begin
  Result := FTreeView.HideSelection;
end;

procedure TvgDBTreeCombo.SetTreeViewHideSelection(Value: Boolean);
begin
  FTreeView.HideSelection := Value;
end;

function TvgDBTreeCombo.GetTreeViewIndent: Integer;
begin
  Result := FTreeView.Indent;
end;

procedure TvgDBTreeCombo.SetTreeViewIndent(Value: Integer);
begin
  FTreeView.Indent := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnEditing: TTVEditingEvent;
begin
  Result := FTreeView.OnEditing;
end;

procedure TvgDBTreeCombo.SetTreeViewOnEditing(Value: TTVEditingEvent);
begin
  FTreeView.OnEditing := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnEdited: TTVEditedEvent;
begin
  Result := FTreeView.OnEdited;
end;

procedure TvgDBTreeCombo.SetTreeViewOnEdited(Value: TTVEditedEvent);
begin
  FTreeView.OnEdited := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnExpanding: TTVExpandingEvent;
begin
  Result := FTreeView.OnExpanding;
end;

procedure TvgDBTreeCombo.SetTreeViewOnExpanding(Value: TTVExpandingEvent);
begin
  FTreeView.OnExpanding := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnExpanded: TTVExpandedEvent;
begin
  Result := FTreeView.OnExpanded;
end;

procedure TvgDBTreeCombo.SetTreeViewOnExpanded(Value: TTVExpandedEvent);
begin
  FTreeView.OnExpanded := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnCollapsing: TTVCollapsingEvent;
begin
  Result := FTreeView.OnCollapsing;
end;

procedure TvgDBTreeCombo.SetTreeViewOnCollapsing(Value: TTVCollapsingEvent);
begin
  FTreeView.OnCollapsing := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnCollapsed: TTVExpandedEvent;
begin
  Result := FTreeView.OnCollapsed;
end;

procedure TvgDBTreeCombo.SetTreeViewOnCollapsed(Value: TTVExpandedEvent);
begin
  FTreeView.OnCollapsed := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnChanging: TTVChangingEvent;
begin
  Result := FTreeView.OnChanging;
end;

procedure TvgDBTreeCombo.SetTreeViewOnChanging(Value: TTVChangingEvent);
begin
  FTreeView.OnChanging := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnChange: TTVChangedEvent;
begin
  Result := FTreeView.OnChange;
end;

procedure TvgDBTreeCombo.SetTreeViewOnChange(Value: TTVChangedEvent);
begin
  FTreeView.OnChange := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnGetImageIndex: TTVExpandedEvent;
begin
  Result := FTreeView.OnGetImageIndex;
end;

procedure TvgDBTreeCombo.SetTreeViewOnGetImageIndex(Value: TTVExpandedEvent);
begin
  FTreeView.OnGetImageIndex := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnGetSelectedIndex: TTVExpandedEvent;
begin
  Result := FTreeView.OnGetSelectedIndex;
end;

procedure TvgDBTreeCombo.SetTreeViewOnGetSelectedIndex(Value: TTVExpandedEvent);
begin
  FTreeView.OnGetSelectedIndex := Value;
end;

function TvgDBTreeCombo.GetTreeViewColor: TColor;
begin
  Result := FTreeView.Color;
end;

procedure TvgDBTreeCombo.SetTreeViewColor(Value: TColor);
begin
  FTreeView.Color := Value;
end;

function TvgDBTreeCombo.GetTreeViewSortType: TSortType;
begin
  Result := FTreeView.SortType;
end;

procedure TvgDBTreeCombo.SetTreeViewSortType(Value: TSortType);
begin
  FTreeView.SortType := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnClick: TNotifyEvent;
begin
  Result := FTreeView.OnClick;
end;

procedure TvgDBTreeCombo.SetTreeViewOnClick(Value: TNotifyEvent);
begin
  FTreeView.OnClick := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnEnter: TNotifyEvent;
begin
  Result := FTreeView.OnEnter;
end;

procedure TvgDBTreeCombo.SetTreeViewOnEnter(Value: TNotifyEvent);
begin
  FTreeView.OnEnter := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnExit: TNotifyEvent;
begin
  Result := FTreeView.OnExit;
end;

procedure TvgDBTreeCombo.SetTreeViewOnExit(Value: TNotifyEvent);
begin
  FTreeView.OnExit := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnDblClick: TNotifyEvent;
begin
  Result := FTreeView.OnDblClick;
end;

procedure TvgDBTreeCombo.SetTreeViewOnDblClick(Value: TNotifyEvent);
begin
  FTreeView.OnDblClick := Value;
end;

function TvgDBTreeCombo.GetTreeViewOnKeyPress: TKeyPressEvent;
begin
  Result := FTreeView.OnKeyPress;
end;

procedure TvgDBTreeCombo.SetTreeViewOnKeyPress(Value: TKeyPressEvent);
begin
  FTreeView.OnKeyPress := Value;
end;

function TvgDBTreeCombo.GetTreeViewPopupMenu: TPopupMenu;
begin
  Result := FTreeView.PopupMenu;
end;

procedure TvgDBTreeCombo.SetTreeViewPopupMenu(Value: TPopupMenu);
begin
  FTreeView.PopupMenu := Value;
end;

function TvgDBTreeCombo.GetTreeViewImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF};
begin
  Result := FTreeView.Images;
end;

procedure TvgDBTreeCombo.SetTreeViewImages(Value: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF});
begin
  FTreeView.Images := Value;
end;

function TvgDBTreeCombo.GetTreeViewStateImages: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF};
begin
  Result := FTreeView.StateImages;
end;

procedure TvgDBTreeCombo.SetTreeViewStateImages(Value: {$IFDEF _D4_} TCustomImageList {$ELSE} TImageList {$ENDIF});
begin
  FTreeView.StateImages := Value;
end;

function TvgDBTreeCombo.StoredTreeViewRootID: Boolean;
begin
  Result := FTreeView.RootID <> NullValue;
end;

function TvgDBTreeCombo.GetTreeViewOptions: TvgDBTreeOptions;
begin
  Result := FTreeView.Options;
end;

procedure TvgDBTreeCombo.SetTreeViewOptions(Value: TvgDBTreeOptions);
begin
  FTreeView.Options := Value;
end;

{TPicturePopup}
constructor TDBTreeWindow.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 100;
end;

destructor TDBTreeWindow.Destroy;
begin
  inherited;
end;

{$IFDEF RX240}
function TDBTreeWindow.GetValue: Variant;
{$ELSE}
function TDBTreeWindow.GetValue: string;
{$ENDIF}
begin
  Result := '';
end;

{$IFDEF RX240}
procedure TDBTreeWindow.SetValue(const Value: Variant);
{$ELSE}
procedure TDBTreeWindow.SetValue(const Value: string);
{$ENDIF}
begin
end;

procedure TDBTreeWindow.WMSetFocus(var Msg: TWMSetFocus);
var
  Form: TCustomForm;
begin
  inherited;
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    FlashWindow(Form.Handle, True);
  end;
end;

procedure TvgComboTreeView.WMKillFocus(var Message: TWMKillFocus);
begin
  FPopup.Combo.SetFocus;
  inherited;
end;

procedure TvgComboTreeView.ActiveChanged;
begin
  inherited;
  if Items.Count > 0 then SelectNode(Items[0]);
end;

procedure TvgComboTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (ssDouble in Shift) then
{$IFDEF RX240}
  begin
    FPopup.Combo.PopupCloseUp(FPopup, True);
    HideCaret(FPopup.Combo.Handle);
  end;
{$ELSE}
    FPopup.CloseUp(True);
{$ENDIF}
  inherited;
end;

procedure TvgComboTreeView.KeyPress(var Key: Char);
begin
  if Key in [#13, #27] then
  begin
    FPopup.CloseUp(True);
    Key := #0;
  end;
  inherited;
end;

procedure TvgComboTreeView.Change(Node: TTreeNode);
begin
  inherited;
  FPopup.Combo.SetSelection(Node);
end;

end.
