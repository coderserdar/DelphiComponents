unit acDBGrid;
{$I sDefs.inc}
 //{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Grids, DBGrids, DB,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  acsbUtils, sConst, sDefaults, sCommonData, sMessages;


type
  TsDBGrid = class;

  TColumnSortOrder = (csoNone, csoAsc, csoDesc);

  TacColumnTitle = class(TColumnTitle)
  private
    function GetCaption: string;
    function IsCaptionStored: boolean;
  protected
    procedure SetCaption(const Value: string);
  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
  end;


  TacDBColumn = class(TColumn)
  private
    FMinWidth: integer;
    FShowMemoBtn: boolean;
    FBoolAsCheckbox: boolean;
    FTableSpacePercent: double;
    FSortOrder: TColumnSortOrder;
    FImmediateToggle: boolean;
    procedure SetSortOrder(Value: TColumnSortOrder);
    procedure SetWidth(const Value: integer);
    function GetWidth: integer;
    procedure SetVisible(Value: Boolean);
    function GetVisible: boolean;
    function CanBeSorted: boolean;
    function CanShowBtn: boolean;
    procedure SetBoolAsCheckbox(const Value: boolean);
    procedure SetImmediateToggle(const Value: boolean);
  protected
    function CanBeBoolean: boolean;
    function GetAsBool: boolean;
    procedure SetAsBool(Value: boolean);
    function CreateTitle: TColumnTitle; override;
    procedure ChangedTitle(DoRebuild: boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property BoolAsCheckbox: boolean read FBoolAsCheckbox write SetBoolAsCheckbox default False;
    property ImmediateToggle: boolean read FImmediateToggle write SetImmediateToggle default False;
    property Visible: boolean read GetVisible write SetVisible;
    property MinWidth: integer read FMinWidth write FMinWidth default 0;
    property SortOrder: TColumnSortOrder read FSortOrder write SetSortOrder default csoNone;
    property ShowMemoBtn: boolean read FShowMemoBtn write FShowMemoBtn default True;
    property Width: integer read GetWidth write SetWidth;
  end;


  TacDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: Integer): TacDBColumn;
    procedure SetColumn(Index: Integer; Value: TacDBColumn);
    procedure ColumnAdded;
  public
    property Items[Index: Integer]: TacDBColumn read GetColumn write SetColumn; default;
  end;

  TGridDrawStateEx = set of (geHighlight, geActiveRow, geMultiSelected);

  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; State: TGridDrawState; StateEx: TGridDrawStateEx) of object;

  TAfterScroll = procedure (Sender: TObject; ScrollBar: Cardinal) of object;

  TGetCellTextEvent = procedure (Sender: TObject; Field: TField; var Text: string) of object;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBGrid = class(TDBGrid)
  private
    FRowColorOdd,
    FRowColorEven: TColor;

    FColumnSort,
    FTitleBarUp,
    FExecSorting,
    FExecColAjust,
    FColumnStretch,
    FDefaultDrawing,
    FActiveRowSelected,
    FCellButtonPressed: boolean;

    FCellButtonRow,
    FCellButtonCol,
    FCellButtonDown,
    FTitleButtonDown,
    FOldTitleButtonDown: integer;

    FCellButtonRect,
    FCellButtonBRect: TRect;

    FLevelDelimiterChar: char;
    FOnScrollData: TNotifyEvent;
    FCommonData: TsScrollWndData;
    FOnAfterScroll: TAfterScroll;
    FOnGetCellText: TGetCellTextEvent;
    FOnGetCellParams: TGetCellParamsEvent;
    procedure UpdateHeaderHeight;
    function IsOnButton(X, Y: integer): boolean;
    function CalcFilterBar(Column: TColumn): TRect;
    function GetButtonRect(Cell: TGridCoord): TRect;
    procedure SetLevelDelimiterChar(const Value: char);
    procedure DrawButton(X, Y: integer; State: boolean);
    function MouseInLowerstLevel(X, Y: integer; Column: TColumn = nil): boolean;

    procedure CalcTableSpacePercent;
    function GetColumns: TacDBGridColumns;
    procedure SetColumns(const Value: TacDBGridColumns);
    procedure SetColor(const Index: integer; Value: TColor);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    FHeaderHeight: integer;
    FExecSizing: boolean;
    ListSW: TacScrollWnd;

{$IFDEF D2010}
    GradColorsSaved: boolean;
    SavedGradEndColor,
    SavedGradStartColor: TColor;
    procedure SaveColors;
    procedure RestoreColors;
{$ENDIF}
    function CreateEditor: TInplaceEdit; override;
    procedure PaintWindow(DC: HDC); override;
    function GetClientRect: TRect; override;
    procedure Loaded; override;
    function CreateColumns: TDBGridColumns; override;

    procedure ColWidthsChanged; override;
    procedure Resize; override;
    procedure ResizeColumns(ResizedColumn: integer = -1);

    procedure DrawColumnCell(const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState); override;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor; State: TGridDrawState; StateEx: TGridDrawStateEx); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

    function CanEditShow: boolean; override;
    procedure TopLeftChanged; override;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure TitleClick(Column: TColumn); override;
    procedure LayoutChanged; override;
    property DataLink;
    procedure Scroll(Distance: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    procedure acUpdateScrollBar;
  public
    procedure AfterConstruction; override;
    function GetGridSize: integer;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property SelectedRows;
    procedure CalcTitleLevel(Level: integer; var aRect: TRect);
    function GetTitleLevel(Level: integer): TRect;
    procedure AdjustColumns;
  published
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
    property Columns: TacDBGridColumns read GetColumns write SetColumns stored False;
    property DefaultDrawing: boolean read FDefaultDrawing write FDefaultDrawing default True;
    property DefaultRowHeight;
    property LevelDelimiterChar: char read FLevelDelimiterchar write SetLevelDelimiterchar default '|';
    property RowColorEven: TColor index 0 read FRowColorEven write SetColor default clWindow;
    property RowColorOdd:  TColor index 1 read FRowColorOdd  write SetColor default clWindow;
    property OnAfterScroll: TAfterScroll read FOnAfterScroll write FOnAfterScroll;
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetCellText: TGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnScrollData: TNotifyEvent read FOnScrollData write FOnScrollData; // Event occurs when grid is scrolled by vertical scroll bar
  end;


implementation

uses
  Math,
  {$IFNDEF DELPHI5}Types,{$ENDIF}
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  acntTypes, sStyleSimply, acntUtils, sVclUtils, sMaskData, sGraphUtils, sSkinProps,
  acgpUtils, sSkinManager;


var
  DrawBitmap: TBitmap;
  UserCount: integer;


type
  _TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
  end;

  TInthernalEdit = class(TEdit)
  end;

{$IFNDEF CLR}
  _TCustomGrid = class(TCustomGrid)
  end;
{$ENDIF}


procedure RefreshGridScrolls(SkinData: TsScrollWndData; var ListSW: TacScrollWnd);
var
  sp: TacSkinParams;
begin
  if SkinData.Skinned then begin
    if (ListSW <> nil) and ListSW.Destroyed then
      FreeAndNil(ListSW);

    if ListSW = nil then begin
      sp.Control := nil;
      sp.HorzScrollBtnSize := -1;
      sp.HorzScrollSize := -1;
      sp.VertScrollBtnSize := -1;
      sp.VertScrollSize := -1;
      sp.UseSkinFontColor := True;
      sp.UseSkinColor := True;
      sp.SkinSection := s_Edit;
      ListSW := TacEditWnd.Create(TWinControl(SkinData.FOwnerControl).Handle, SkinData, SkinData.SkinManager, sp);
    end;
  end
  else
    if ListSW <> nil then
      FreeAndNil(ListSW);
end;


procedure UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;

  Inc(UserCount);
end;


procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then
    DrawBitmap.Free;
end;


function GetCaptionDepth(const Str: string; Delim: char): integer;
var
  i: integer;
  St: string;
begin
  Result := 0;
  if Str <> '' then begin
    Result := 1;
    i := Pos(Delim, Str);
    St := Str;
    while i > 0 do begin
      Inc(Result);
      St[i] := #255;
      i := Pos(Delim, St);
    end;
  end;
end;


function GetCaptionLevel(const Str: string; Level: integer; Delim: char): string;
var
  i,j: integer;
  St: string;
begin
  j := 0;
  Result := '';
  if Str <> '' then begin
    i := Pos(Delim, Str);
    St := Str;
    while (Level > 0) and (I > 0) do begin
      Dec(Level);
      St[i] := #255;
      if Level <= -2 then begin
        Result := Copy(St, j + 1, i - 1);
        Exit;
      end;
      j := i;
      i := Pos(Delim, St);
    end;
    if Level <= 0 then begin
      if i = 0 then
        i := Length(St) + j
      else
        Dec(i);

      Result := Copy(Str, j + 1, i - j);
      Exit;
    end;
  end;
end;


function TacColumnTitle.GetCaption: string;
begin
  Result := inherited Caption;
end;


function TacColumnTitle.IsCaptionStored: boolean;
begin
  Result := (cvTitleCaption in Column.AssignedValues) and (Caption <> DefaultCaption);
end;


procedure TacColumnTitle.SetCaption(const Value: string);
begin
  if Value <> inherited Caption then begin
    inherited Caption := Value;
    TacDBColumn(Column).ChangedTitle(True);
  end;
end;


procedure TacDBColumn.SetAsBool(Value: boolean);
begin
  if Field <> nil then
    case Field.DataType of
      ftBoolean: Field.AsBoolean := Value;
      ftSmallint, ftInteger: Field.AsInteger := integer(Value);
    end
end;


procedure TacDBColumn.SetBoolAsCheckbox(const Value: boolean);
begin
  if FBoolAsCheckbox <> Value then
    if CanBeBoolean or ([csDesigning, csLoading] * Grid.ComponentState <> []) then begin
      FBoolAsCheckbox := Value;
      TsDBGrid(Grid).Invalidate;
    end
    else
      FBoolAsCheckbox := False;
end;


procedure TacDBColumn.SetImmediateToggle(const Value: boolean);
begin
  if FImmediateToggle <> Value then
    if (Field is TBooleanField) or ([csDesigning, csLoading] * Grid.ComponentState <> []) then
      FImmediateToggle := Value
    else
      FImmediateToggle := False;
end;


procedure TacDBColumn.SetSortOrder(Value: TColumnSortOrder);
begin
  if FSortOrder <> Value then begin
    FSortOrder := Value;
    TsDBGrid(Grid).Invalidate;
  end;
end;


function TacDBColumn.GetAsBool: boolean;
begin
  if Field <> nil then
    case Field.DataType of
      ftBoolean:             Result := Field.AsBoolean;
      ftSmallint, ftInteger: Result := Field.AsInteger <> 0 // iff(Field.AsInteger = 0, False, True);
      else                   Result := False;
    end
  else
    Result := False;
end;


procedure TacDBColumn.Assign(Source: TPersistent);
begin
  if Source is TacDBColumn then begin
    if Assigned(Collection) then
      Collection.BeginUpdate;

    inherited Assign(Source);
    try
      FMinWidth := TacDBColumn(Source).FMinWidth;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


function TacDBColumn.CanBeBoolean: boolean;
begin
  Result := (Field <> nil) and (Field.DataType in [ftSmallint, ftInteger, ftBoolean])
end;


function TacDBColumn.CanBeSorted: boolean;
begin
  if Assigned(Field) then
    Result := (Field.FieldKind = fkData) and not (Field.DataType in [ftFmtMemo, ftMemo{$IFNDEF VER4}, ftOraClob {$ENDIF}])
  else
    Result := False;
end;


procedure TacDBColumn.ChangedTitle(DoRebuild: boolean);
begin
  if DoRebuild and Assigned(Grid) then
    TsDBGrid(Grid).LayoutChanged;
end;


function TacDBColumn.CreateTitle: TColumnTitle;
begin
  Result := TacColumnTitle.Create(Self);
end;


constructor TacDBColumn.Create(Collection: TCollection);
begin
  inherited;
  FMinWidth := 0;
  TacDBGridColumns(Collection).ColumnAdded;
  FBoolAsCheckbox := False;
  FImmediateToggle := False;
  FShowMemoBtn := True;
  FSortOrder := csoNone;
end;


procedure TacDBColumn.SetWidth(const Value: integer);
begin
  if Value > FMinWidth then
    inherited Width := Value
  else
    inherited Width := FMinWidth
end;


function TacDBColumn.GetWidth: integer;
begin
  Result := inherited Width;
end;


procedure TacDBColumn.SetVisible(Value: Boolean);
var
  OldVisible: boolean;
begin
  OldVisible := inherited Visible;
  inherited Visible := Value;
  if (OldVisible <> Value) and Assigned(Grid) and TsDBGrid(Grid).FColumnStretch and not TsDBGrid(Grid).FExecSizing then begin
    TsDBGrid(Grid).FExecSizing := True;
    TsDBGrid(Grid).ResizeColumns;
    TsDBGrid(Grid).FExecSizing := False;
  end;
end;


function TacDBColumn.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;


function TacDBGridColumns.GetColumn(Index: Integer): TacDBColumn;
begin
  Result := TacDBColumn(inherited Items[Index]);
end;


procedure TacDBGridColumns.SetColumn(Index: Integer; Value: TacDBColumn);
begin
  inherited Items[Index] := Value;
end;


procedure TacDBGridColumns.ColumnAdded;
begin
  TsDBGrid(Grid).CalcTableSpacePercent;
end;


constructor TsDBGrid.Create(Owner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  inherited Create(Owner);
  Columns.State := csDefault;
  UsesBitmap;
  FLevelDelimiterChar := '|';
  inherited DefaultDrawing := False;
  FDefaultDrawing := True;
  FColumnStretch := False;
  FColumnSort := False;
  FExecSizing := False;
{$IFDEF D2010}
  GradColorsSaved := False;
{$ENDIF}
  FTitleButtonDown := -1;
  FOldTitleButtonDown := -1;
  FCellButtonDown := -1;
  FRowColorEven := clWindow;
  FRowColorOdd  := clWindow;
  FCommonData.COC := COC_TsDBGrid;
end;


destructor TsDBGrid.Destroy;
begin
  FreeAndNil(ListSW);
  FreeAndNil(FCommonData);
  ReleaseBitmap;
  inherited;
end;


procedure TsDBGrid.Loaded;
var
  Stretched: Boolean;
begin
  Stretched := FColumnStretch;
  FColumnStretch := false;
  inherited Loaded;
{$IFDEF D2010}
  SaveColors;
{$ENDIF}
  try
    FCommonData.Loaded;
  except
    Application.HandleException(Self);
  end;
  FColumnStretch := Stretched;
  CalcTableSpacePercent;
end;


procedure TsDBGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  try
    FCommonData.Loaded;
  except
    Application.HandleException(Self);
  end;
end;


function TsDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TacDBGridColumns.Create(Self, TacDBColumn);
end;


procedure TsDBGrid.Resize;
begin
  inherited;
  if FColumnStretch and not (csLoading in ComponentState) and (not FExecSizing) then begin
    FExecSizing := True;
    try
      ResizeColumns;
    finally
      FExecSizing := False;
    end;
  end;
end;


procedure TsDBGrid.ColWidthsChanged;
var
  i: integer;
  ResizedColumn: integer;
begin
  if FColumnStretch and not (csLoading in ComponentState) and not FExecSizing then begin
    FExecSizing := True;
    ResizedColumn := -1;
    for i := 0 to Columns.Count - 1 do
      if ColWidths[i + IndicatorOffset] <> Columns[i].Width then begin
        ResizedColumn := i;
        break;
      end;

    if ResizedColumn >= 0 then begin
      if ColWidths[ResizedColumn + IndicatorOffset] <= TacDBColumn(Columns[ResizedColumn]).MinWidth then
        ColWidths[ResizedColumn + IndicatorOffset] := TacDBColumn(Columns[ResizedColumn]).MinWidth;

      ResizeColumns(ResizedColumn);
    end;
    FExecSizing := False;
  end
  else
    if not (csLoading in ComponentState) and not FExecSizing then
      CalcTableSpacePercent;

  inherited;
end;


function TsDBGrid.GetGridSize: integer;
begin
  Result := ClientWidth - 1;
  if dgIndicator in Options then
    Dec(Result, IndicatorWidth);

  if dgColLines in Options then
    Dec(Result, Columns.Count * GridLineWidth);
end;


procedure TsDBGrid.ResizeColumns(ResizedColumn: integer);
const
  MinWidth = 10;
var
  Width, i, GridSize, ColumnsSize, UnresizedSize: integer;
  K, Curr,Prev, VisiblePercent: double;
  MinimizeRest: boolean;
begin
  if Columns.Count <> 0 then begin
    GridSize := ClientWidth - 1;
    if dgIndicator in Options then
      Dec(GridSize, IndicatorWidth);

    if dgColLines in Options then
      for i := 0 to Columns.Count - 1 do
        if TacDBColumn(Columns[i]).Visible then
          Dec(GridSize, GridLineWidth);

    if ResizedColumn >= 0 then begin
      ColumnsSize := 0;
      UnresizedSize := 0;
      MinimizeRest := False;
      for i := 0 to Columns.Count - 1 do begin
        if i <= ResizedColumn then begin
          Inc(UnresizedSize, ColWidths[i + IndicatorOffset]);
          if i = ResizedColumn then
            if ColumnsSize + ColWidths[i + IndicatorOffset] + (Columns.Count - i) * MinWidth > GridSize then begin
              ColWidths[i + IndicatorOffset] := GridSize - ColumnsSize - (Columns.Count - i - 1) * MinWidth;
              MinimizeRest := True;
            end
            else
              if i = Columns.Count - 1 then
                ColWidths[i + IndicatorOffset] := GridSize - ColumnsSize;
        end
        else
          if MinimizeRest then
            ColWidths[i + IndicatorOffset] := MinWidth;

        Inc(ColumnsSize, ColWidths[i + IndicatorOffset]);
      end;

      if ColumnsSize = UnresizedSize then
        Exit;

      K := (GridSize - UnresizedSize) / (ColumnsSize - UnresizedSize);
      ColumnsSize := 0;
      Prev := 0;
      for i := 0 to Columns.Count - 1 do begin
        if i <= ResizedColumn then
          Curr := Prev + ColWidths[i + IndicatorOffset]
        else begin
          Curr := Prev + ColWidths[i + IndicatorOffset]*K;
          if i < Columns.Count - 1 then
            Width := Round(Curr - Prev)
          else
            Width := GridSize - ColumnsSize;

          if Width < TacDBColumn(Columns[i]).MinWidth then
            Width := TacDBColumn(Columns[i]).MinWidth;

          ColWidths[i + IndicatorOffset] := Width;
        end;
        Inc(ColumnsSize, ColWidths[i + IndicatorOffset]);
        Prev := Curr;
      end;
      CalcTableSpacePercent;
    end
    else begin // for a full resize
      Inc(GridSize, 2);
      if FColumnStretch then begin
        VisiblePercent := 0;
        for i := 0 to Columns.Count - 1 do
          if TacDBColumn(Columns[i]).Visible then
            VisiblePercent := VisiblePercent + TacDBColumn(Columns[i]).FTableSpacePercent;

        if VisiblePercent < 0.0001 then
          VisiblePercent := 1;
      end
      else
        VisiblePercent := 1;

      for i := 0 to Columns.Count - 1 do
        ColWidths[i + IndicatorOffset] := Trunc(TacDBColumn(Columns[i]).FTableSpacePercent * GridSize / VisiblePercent);
    end;
  end;
end;


procedure TsDBGrid.GetCellProps(Field: TField; AFont: TFont; var Background: TColor; State: TGridDrawState; StateEx: TGridDrawStateEx);
begin
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, State, StateEx);
end;


procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: integer; const Text: string; Alignment: TAlignment; ARightToLeft: boolean);
const
  RTL: array [boolean] of integer = (0, DT_RTLREADING);
  TextFlag = DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX;
  AlignFlags: array [TAlignment] of integer = (DT_LEFT or TextFlag, DT_RIGHT or TextFlag, DT_CENTER or TextFlag);
var
  Hold: integer;
  I: TColorRef;
  B, R: TRect;
  Size: TSize;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
    ChangeBiDiModeAlignment(Alignment);

  if ARightToLeft then
    ACanvas.TextFlags := ACanvas.TextFlags or ETO_RTLREADING;

  if GetNearestColor(ACanvas.Handle, I) = I then
    if Text <> '' then begin
      R := ARect;
      InflateRect(R, -4, 0);
      acDrawText(ACanvas.Handle, Text, R, DT_CALCRECT);
      Size.cy := HeightOf(R);
      R := ARect;
      R.Top := ARect.Top + (HeightOf(ARect) - Size.cy) div 2; // Align to VCENTER
      R.Bottom := R.Top + Size.cy;
      R.Left := ARect.Left + 3;
      R.Right := ARect.Right - 3;
      if ACanvas.CanvasOrientation = coRightToLeft then begin
//        ACanvas.TextFlags := ACanvas.TextFlags or AlignFlags[BidiAlign[True, Alignment]];
        ACanvas.TextRect(R, R.Left, R.Top, Text);
      end
      else
        acDrawText(ACanvas.Handle, Text, R, AlignFlags[Alignment] or RTL[ARightToLeft]);
    end
    else
      ACanvas.FillRect(ARect)
  else // Use FillRect and Drawtext for different colors
    if ACanvas.Brush.Style <> bsClear then begin
      DrawBitmap.Canvas.Lock;
      try
        with DrawBitmap, ARect do begin // Use offscreen bitmap to eliminate flicker and brush origin tics in painting / scrolling.
          Width  := Max(Width,  Right - Left);
          Height := Max(Height, Bottom - Top);
          R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
          B := MkRect(Right - Left, Bottom - Top);
        end;
        with DrawBitmap.Canvas do begin
          Font := ACanvas.Font;
          Font.Color := acColorToRGB(ACanvas.Font.Color);
          Brush := ACanvas.Brush;
          Brush.Style := bsSolid;
          FillRect(B);
          SetBkMode(Handle, TRANSPARENT);
          DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment] or RTL[ARightToLeft]);
        end;
        if ACanvas.CanvasOrientation = coRightToLeft then begin
          Hold := ARect.Left;
          ARect.Left := ARect.Right;
          ARect.Right := Hold;
        end;
        ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
      finally
        DrawBitmap.Canvas.Unlock;
      end;
    end;
end;


function TsDBGrid.GetButtonRect(Cell: TGridCoord): TRect;
var
  aCellRect: TRect;
begin
  aCellRect := CellRect(Cell.X, Cell.Y);
  if aCellRect.Right - aCellRect.Left < aCellRect.Bottom - aCellRect.Top + 5 then
    Result := MkRect
  else begin
    Result.Left := aCellRect.Right - (aCellRect.Bottom - aCellRect.Top) + 1;
    Result.Right := aCellRect.Right - 1;
    Result.Top := aCellRect.Top + 1;
    Result.Bottom := aCellRect.Bottom - 1;
  end;
end;


function TsDBGrid.IsOnButton(X, Y: integer): boolean;
var
  Cell: TGridCoord;
  Column: TColumn;
  aCellRect, ButtonRect: TRect;
begin
  Cell := MouseCoord(X,Y);
  Column := Columns[RawToDataColumn(Cell.X)];
  // detecting - is there a button on cell?
  if Assigned(Column.Field) then
    Result := TacDBColumn(Column).CanShowBtn
  else
    Result := False;

  aCellRect := CellRect(Cell.X, Cell.Y);
  if Result and (aCellRect.Right - aCellRect.Left < aCellRect.Bottom - aCellRect.Top + 5) then
    Result := False;

  if Result then begin // button present
    ButtonRect := GetButtonRect(Cell);
    Result := PtInRect(ButtonRect, Point(X, Y))
  end
  else // there is no button on cell
    Result := False;
end;


procedure TsDBGrid.DrawButton(X, Y: integer; State: boolean);
var
  Cell: TGridCoord;
  ButtonRect: TRect;
  Hi, i, Diam, Flag: integer;
begin
  Cell.X := X; Cell.Y := Y;
  ButtonRect := GetButtonRect(Cell);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ButtonRect);
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := clBlack;

  Flag := iff(State, BDR_SUNKENINNER, BDR_RAISEDINNER);
  DrawEdge(Canvas.Handle, ButtonRect, Flag, BF_TOPLEFT );
  InflateRect(ButtonRect, -1, -1);
  DrawEdge(Canvas.Handle, ButtonRect, Flag, BF_BOTTOMRIGHT);
  InflateRect(ButtonRect, 1, 1);
  Canvas.MoveTo(ButtonRect.Left, ButtonRect.Bottom - 1);
  Canvas.LineTo(ButtonRect.Right - 1, ButtonRect.Bottom - 1);
  Canvas.LineTo(ButtonRect.Right - 1, ButtonRect.Top - 1);

  Diam := (ButtonRect.Bottom - ButtonRect.Top) div 7;
  Hi := (ButtonRect.Bottom - ButtonRect.Top - Diam) div 2;
  inc(ButtonRect.Left,Diam * 2 - 1);
  if State then begin
    inc(ButtonRect.Left);
    inc(ButtonRect.Top);
  end;
  for i := 0 to 2 do
    Canvas.Ellipse(ButtonRect.Left + i * Diam * 2, ButtonRect.Top + Hi, ButtonRect.Left + i * Diam * 2 + Diam, ButtonRect.Top + Hi + Diam);
end;


function IsDigits(const Value: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(Value) do begin
    if not CharInSet(Value[i], ['0'..'9']) then begin
      Result := False;
      Exit;
    end;
  end;
end;


procedure TsDBGrid.DrawColumnCell(const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
var
  TextWidth, ThreeDotWidth, ColWidth, SelNdx, TextMargin, i, y: integer;
  isDrawButton, bgChanged: boolean;
  StateEx: TGridDrawStateEx;
  Alignment: TAlignment;
  OldCanvasFont: TFont;
  NewBackgrnd: TColor;
  CI: TCacheInfo;
  Field: TField;
  Value: string;
  Bmp: TBitmap;
begin
  Field := Column.Field;
  if Assigned(Column.Field) then begin
    Value := Column.Field.DisplayText;
    if Assigned(FOnGetCellText) then
      FOnGetCellText(Self, Column.Field, Value);

    isDrawButton := TacDBColumn(Column).CanShowBtn;
  end
  else begin
    Value := '';
    isDrawButton := False;
  end;
  isDrawButton := isDrawButton and (gdSelected in State) and not (dgRowSelect in Options);
  if isDrawButton and (Rect.Right - Rect.Left < Rect.Bottom - Rect.Top + 5) then
    isDrawButton := False;

  Alignment := Column.Alignment;
  TextMargin := iff(Alignment = taRightJustify, 4, 2);
  ThreeDotWidth := Canvas.TextWidth(s_Ellipsis);
  TextWidth := Canvas.TextWidth(Value) + TextMargin;
  OldCanvasFont := TFont.Create;
  OldCanvasFont.Assign(Canvas.Font);
  try
    ColWidth := Column.Width;  // changes font and brush
    Canvas.Font.Assign(OldCanvasFont);
  finally
    OldCanvasFont.Free;
  end;
  if isDrawButton then
    ColWidth := ColWidth - (Rect.Bottom - Rect.Top);

  if TextWidth > ColWidth then begin
    if (Field is TNumericField) and IsDigits(Value) then begin
      for i := 1 to Length(Value) do
        if (Value[i] >= '0') and (Value[i] <= '9') then
          Value[i] := '#';
    end
    else begin
      while (TextWidth > ColWidth) and (Length(Value) > 1) do begin
        SetLength(Value, Length(Value) - 1);
        TextWidth := Canvas.TextWidth(Value) + TextMargin + ThreeDotWidth;
      end;
      Value := Value + s_Ellipsis;
    end;
    Alignment := taLeftJustify;
  end;


  if HighlightCell(Col, Row, Value, State) then begin
    StateEx := [geHighlight];
    if not FActiveRowSelected then
      Include(StateEx, geMultiSelected);
  end
  else
    StateEx := [];

  if FActiveRowSelected then
    Include(StateEx, geActiveRow);

  if geHighlight in StateEx then
    if SkinData.Skinned then begin
      SelNdx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection];
      FillDC(Canvas.Handle, Rect, SkinData.CommonSkinData.gd[SelNdx].Props[0].Color);
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := SkinData.CommonSkinData.gd[SelNdx].Props[integer(Focused)].FontColor.Color;
    end
    else begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end
  else
    if SkinData.Skinned and not SkinData.CustomFont then
      Canvas.Font.Color := SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[0].FontColor.Color;

  if Enabled then begin
    y := Rect.Bottom div HeightOf(Rect) + FixedRows;
    if not (geHighlight in StateEx) then
      if RowColorEven <> RowColorOdd then begin
        if SkinData.Skinned then
          if y mod 2 = 0 then
            Canvas.Brush.Color := SkinData.SkinManager.Palette[pcEditBG_OddRow]
          else
            Canvas.Brush.Color := SkinData.SkinManager.Palette[pcEditBG_EvenRow]
        else
          if y mod 2 = 0 then
            Canvas.Brush.Color := acColorToRGB(RowColorOdd)
          else
            Canvas.Brush.Color := acColorToRGB(RowColorEven);
      end;

    NewBackgrnd := Canvas.Brush.Color;
    GetCellProps(Field, Canvas.Font, NewBackgrnd, State, StateEx);
    if Canvas.Brush.Color <> NewBackgrnd then begin
      Canvas.Brush.Color := acColorToRGB(NewBackgrnd);
      bgChanged := True;
    end
    else
      bgChanged := False;
  end
  else begin
    Canvas.Font.Color := clGrayText;
    bgChanged := False;
  end;
  if FDefaultDrawing then begin
    if bgChanged then
      FillDC(Canvas.Handle, Rect, Canvas.Brush.Color)
    else
      if (geHighlight in StateEx) and SkinData.Skinned then begin
        SelNdx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection];
        Canvas.Brush.Style := bsClear;
        if Focused then begin
          CI := EmptyCI;
          CI.FillColor := acColorToRGB(Color);
//          CI.FillRect := MkRect;
//          CI.Ready := False;
          PaintItem(SelNdx, CI, True, 1, Rect, MkPoint, Canvas.Handle, SkinData.CommonSkinData);
          Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor;
        end
        else begin
          FillDC(Canvas.Handle, Rect, SkinData.CommonSkinData.gd[SelNdx].Props[0].Color);
          Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(Focused);
        end;
      end
      else
        FillDC(Canvas.Handle, Rect, Canvas.Brush.Color);

    if TacDBColumn(Column).CanBeBoolean and TacDBColumn(Column).BoolAsCheckbox then begin
      Bmp := CreateBmp32(Rect);
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, Rect.Left, Rect.Top, SRCCOPY);
      CI := MakeCacheInfo(Bmp);
//      CI.Bmp := Bmp;
//      CI.Ready := True;
//      CI.FillRect := MkRect;
      acDrawCheck(MkRect(Bmp), TCheckBoxState(TacDBColumn(Column).GetAsBool), True, Bmp, CI, SkinData.CommonSkinData);
      BitBlt(Canvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      Bmp.Free;
    end
    else
      WriteText(Canvas, Rect, 2, 2, Value, Alignment, UseRightToLeftAlignmentForField(Column.Field, Alignment));

    if (geHighlight in StateEx) and
         ((dgAlwaysShowSelection in Options) or Focused) and
           not (csDesigning in ComponentState) and
             not (dgRowSelect in Options) and
               (UpdateLock = 0) and
                 (ValidParentForm(Self).ActiveControl = Self) then
      Windows.DrawFocusRect(Canvas.Handle, Rect);
  end;
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  if isDrawButton then
    if FCellButtonDown >= 0 then
      DrawButton(Col, Row, FCellButtonPressed)
    else
      DrawButton(COl, Row, False);
end;


function TsDBGrid.GetTitleLevel(Level: integer): TRect;
begin
  Result := MkRect;
  if Columns.Count <> 0 then begin
    Result.Top := Level * (DefaultRowHeight + 1);
    Result.Bottom := Result.Top + DefaultRowHeight + 1;
    if dgRowLines in Options then
      dec(Result.Bottom);
  end;
end;


procedure TsDBGrid.CalcTitleLevel(Level: integer; var aRect: TRect);
var
  X: TRect;
begin
  if Columns.Count = 0 then
    aRect.TopLeft := MkPoint
  else begin
    X := GetTitleLevel(Level);
    aRect.Top    := X.Top;
    aRect.Bottom := X.Bottom;
  end;
end;


procedure TsDBGrid.DrawCell(ACol, ARow: longint; ARect: TRect; AState: TGridDrawState);
var
  FrameOffs: Byte;
  CI: TCacheInfo;
  R: TRect;
  OldActive: Integer;
  MultiSelected: Boolean;

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (dgMultiSelect in Options) and Datalink.Active and
      SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;

  function IndiColor(ASkinIndex: integer): TColor;
  begin
    if ASkinIndex >= 0 then
      Result := FCommonData.CommonSkinData.gd[ASkinIndex].Props[0].FontColor.Color
    else
      Result := FCommonData.SkinManager.Palette[pcLabelText];
  end;

  function GetRightColIndex(ACol, ACurLevel: integer): integer;
  var
    i: integer;
    CurColumn: TColumn;
    s1, s2: string;
  begin
    Result := ACol;
    CurColumn := Columns[ACol];
    if CurColumn.Title.Caption <> '' then
      for i := ACol + 1 to Columns.Count - 1 do begin
        s1 := ExtractWord(ACurLevel + 1, CurColumn.Title.Caption,  [FLevelDelimiterChar]);
        s2 := ExtractWord(ACurLevel + 1, Columns[i].Title.Caption, [FLevelDelimiterChar]);
        if UpperCase(s1) = UpperCase(s2) then
          inc(Result)
        else
          Break;
      end;
  end;

  function GetBottomRow(ACol, ACurLevel: integer): integer;
  begin
    if WordCount(Columns[ACol].Title.Caption, [FLevelDelimiterChar]) = 1 then
      Result := FHeaderHeight - 1
    else
      Result := 1;
  end;

  procedure DrawIndicator(ARect: TRect; AllowArrow: boolean = True);
  const
    iEllSize = 5;
  var
    iRow, iSkinNdx: integer;
    Size: TSize;
  begin
    if (dgRowLines in options) or (dgColLines in options) then
      if FCommonData.SkinManager.SkinCommonInfo.Sections[ssColHeader] >= 0 then
        iSkinNdx := FCommonData.SkinManager.SkinCommonInfo.Sections[ssColHeader]
      else
        iSkinNdx := FCommonData.SkinManager.SkinCommonInfo.Sections[ssButton]
    else
      iSkinNdx := -1;

    if iSkinNdx >= 0 then
      PaintItem(iSkinNdx, CI, True, 0, ARect, MkPoint, Canvas.Handle, FCommonData.CommonSkinData)
    else
      FillDC(Canvas.Handle, ARect, SkinData.SkinManager.Palette[pcMainColor]);

    if AllowArrow and FActiveRowSelected then
      if (DataLink <> nil) and DataLink.Active and DataLink.Editing then begin
        Canvas.Font.Assign(Font);
        Canvas.Font.Color := IndiColor(iSkinNdx);
        Canvas.Font.Name := 'Courier';
        Size := acTextExtent(Canvas, 'I');
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(ARect.Left + (WidthOf(ARect) - Size.cx) div 2 - 1, ARect.Top + (HeightOf(ARect) - Size.cy) div 2, 'I');
      end
      else begin
        Canvas.Font.Color := IndiColor(iSkinNdx);
        DrawColorArrow(Canvas, Canvas.Font.Color, ARect, asRight, FCommonData.CommonSkinData.PPI);
      end
    else
      if (dgMultiSelect in Options) and Assigned(DataLink) and Datalink.Active then begin
        MultiSelected := False;
        iRow := ARow - 1;
        if iRow >= 0 then begin
          OldActive := DataLink.ActiveRecord;
          try
            Datalink.ActiveRecord := iRow;
            MultiSelected := RowIsMultiselected;
          finally
            Datalink.ActiveRecord := OldActive;
          end;
        end;
        if MultiSelected then
          acgpFillEllipse(Canvas.Handle, ARect.Left + (WidthOf(ARect) - iEllSize) div 2, ARect.Top + (HeightOf(ARect) - iEllSize) div 2, iEllSize, iEllSize, IndiColor(iSkinNdx));
      end;
  end;

  procedure DrawTitleCell(ACol, ARow: integer; Column: TColumn; var AState: TGridDrawState);
  const
    ScrollArrows: array [boolean, boolean] of integer = ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    bSkinned, lvCheckLeft, lvCheckRight, lvShowCaption, lvUpBorder, lvDownBorder, lvLeftBorder, lvRightBorder, lvCheckTextWidth: boolean;
    CaptionWidth, CurLevel, iSkinNdx, iMaxRow, lvTmpColIndex, lvCaptionXOffset, CaptionDepth, PressOffset: integer;
    Caption, CurCaption, TmpCaption: string;
    CellRect, TitleRect, TextRect: TRect;
    lvCaptionAlignment: TAlignment;
    MasterCol, lvTmpCol: TColumn;
    CellFlag: cardinal;
    ArrowStyle: TacArrowsStyle;
    ArrowSide: TacSide;
    ArrowSize: TSize;
    rText, rArrow: TRect;
{$IFDEF DELPHI7UP}
    Elem: TThemedHeader;
    Details: TThemedElementDetails;
{$ENDIF}

    procedure CheckWidth;
    begin
      if lvCheckTextWidth then begin
        CaptionWidth := Canvas.TextWidth(CurCaption);
        if CaptionWidth > WidthOf(TextRect) then begin
          while (CaptionWidth > WidthOf(TextRect)) and (Length(CurCaption) > 1) do begin
            SetLength(CurCaption, Length(CurCaption) - 1);
            CaptionWidth := Canvas.TextWidth(CurCaption) + Canvas.TextWidth(s_Ellipsis);
          end;
          CurCaption := CurCaption + s_Ellipsis;
        end;
      end;
    end;

  begin
    CellRect  := CalcTitleRect(Column, ARow, MasterCol);
    TitleRect := CellRect;
    bSkinned := FCommonData.Skinned;
    if bSkinned and (MasterCol.Title.Color = clBtnFace) and ((dgRowLines in options) or (dgColLines in options)) then
      if FCommonData.SkinManager.SkinCommonInfo.Sections[ssColHeader] >= 0 then
        iSkinNdx := FCommonData.SkinManager.SkinCommonInfo.Sections[ssColHeader]
      else
        iSkinNdx := FCommonData.SkinManager.SkinCommonInfo.Sections[ssButton]
    else
      iSkinNdx := -1;

    if MasterCol = nil then begin
      if not bSkinned then
{$IFDEF DELPHI7UP}
        if acThemesEnabled then begin
          Elem := thHeaderItemNormal;
          Details := acThemeServices.GetElementDetails(Elem);
          acThemeServices.DrawElement(Canvas.Handle, Details, aRect);
        end
        else
{$ENDIF}
          Canvas.FillRect(ARect);

      Exit;
    end;

    Canvas.Font := Column.Title.Font;
    if not ParentFont then
      Canvas.Font.Height := ScaleInt(Canvas.Font.Height, SkinData);

    Canvas.Brush.Color := acColorToRGB(Column.Title.Color);
    if not bSkinned then
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        Elem := thHeaderItemNormal;
        Details := acThemeServices.GetElementDetails(Elem);
        acThemeServices.DrawElement(Canvas.Handle, Details, aRect);
      end
      else
{$ENDIF}
        Canvas.FillRect(ARect);

    TextRect := TitleRect;
    Caption := MasterCol.Title.Caption;
    lvCheckLeft  := True;
    lvCheckRight := True;
    lvShowCaption:= True;
    lvLeftBorder := True;
    lvRightBorder:= True;
    if TacColumnTitle(MasterCol.Title).IsCaptionStored then
      CaptionDepth := GetCaptionDepth(Caption, FLevelDelimiterChar)
    else
      CaptionDepth := 1;

    FrameOffs := 1;
    PressOffset := integer((Column.Index = FTitleButtonDown) and (dgRowLines in Options));

    for CurLevel := 0 to FHeaderHeight - 1 do begin
      // Check dependencies
      if TacColumnTitle(MasterCol.Title).IsCaptionStored then
        CurCaption := GetCaptionLevel(Caption, CurLevel, FLevelDelimiterChar)
      else
        CurCaption := iff(CurLevel = 0, Caption, '');

      lvDownBorder := (FHeaderHeight - 1 = CurLevel) or (GetCaptionLevel(Caption, CurLevel + 1, FLevelDelimiterChar) <> '');
      lvUpBorder   := (CurCaption <> '');
      lvCaptionXOffset := 0;
      if CurCaption <> '' then begin
        if lvCheckLeft then begin
          lvLeftBorder := True;
          lvShowCaption := True;
          if (Column.Index = LeftCol - IndicatorOffset) or (CurLevel = (CaptionDepth - 1)) then
            lvCheckLeft := False
          else begin
            lvTmpColIndex := Column.Index - 1;
            while lvTmpColIndex >= 0 do begin
              lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
              tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption, CurLevel, FLevelDelimiterChar);
              if UpperCase(tmpCaption) <> UpperCase(CurCaption) then begin
                if lvTmpColIndex = Column.Index - 1 then
                  lvCheckLeft := False;

                Break;
              end
              else begin
                lvShowCaption := False;
                lvLeftBorder := False;
                inc(lvCaptionXOffset, lvTmpCol.Width);
                if dgColLines in Options then
                  inc(lvCaptionXOffset);

                dec(lvTmpColIndex)
              end;
            end;
          end;
        end;
        if lvCheckRight then begin
          lvRightBorder := True;
          if (Column.Index = MasterCol.Collection.Count - 1) or (CurLevel = (CaptionDepth - 1)) then
            lvCheckRight := False
          else begin
            lvTmpColIndex := Column.Index + 1;
            lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
            tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption, CurLevel, FLevelDelimiterChar);
            if UpperCase(tmpCaption) <> UpperCase(CurCaption) then
              lvCheckRight := False
            else
              lvRightBorder := False;
          end;
        end;
      end
      else
        if Caption <> '' then
          lvShowCaption := False;

      if Column.Index = MasterCol.Collection.Count - 1 then
        lvCheckTextWidth := True
      else begin
        lvTmpColIndex := Column.Index + 1;
        lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
        tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption, CurLevel, FLevelDelimiterChar);
        lvCheckTextWidth := UpperCase(tmpCaption) <> UpperCase(CurCaption);
      end;
      TitleRect := CellRect;
      CalcTitleLevel(CurLevel, TitleRect);
      TextRect := TitleRect;
      InflateRect(TextRect, -1, -1);
      if not lvRightBorder then begin
        inc(TextRect.Right);
        if dgColLines in Options then
          inc(TextRect.Right);
      end;

      if CurLevel <> CaptionDepth - 1 then begin
        Canvas.Font := Self.TitleFont;
        Canvas.Brush.Color := acColorToRGB(Self.FixedColor);
        lvCaptionAlignment := taCenter;
      end
      else begin
        Canvas.Font := MasterCol.Title.Font;
        Canvas.Brush.Color := acColorToRGB(MasterCol.Title.Color);
        lvCaptionAlignment := BidiAlign[UseRightToLeftAlignment, Column.Title.Alignment];
      end;
      if not ParentFont and (CurLevel = 0) and (pos(FLevelDelimiterChar, Caption) > 0) then
        Canvas.Font.Height := ScaleInt(Canvas.Font.Height, SkinData);
{$IFDEF DELPHI7UP}
      if not (bSkinned or acThemesEnabled) then
{$ENDIF}
      begin // Paint without themes/skins
        Canvas.FillRect(TextRect);
        // Draw borders for level
        CellFlag := BDR_RAISEDINNER;
        if (FTitleButtonDown = Column.Index) and (CurLevel >= CaptionDepth-1) then
          CellFlag := BDR_SUNKENINNER;

        if not lvDownBorder then begin
          Inc(TitleRect.Bottom, 1);
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(TitleRect.Left,TitleRect.Bottom - 2);
          Canvas.LineTo(TitleRect.Right + 1, TitleRect.Bottom - 2);
          if dgRowLines in Options then begin
            Canvas.MoveTo(TitleRect.Left, TitleRect.Bottom - 1);
            Canvas.LineTo(TitleRect.Right + 1, TitleRect.Bottom - 1);
          end;
        end;
        if not lvUpBorder then begin
          Canvas.Pen.Color := clBtnFace;
          Canvas.MoveTo(TitleRect.Left, TitleRect.Top);
          Canvas.LineTo(TitleRect.Right + 1, TitleRect.Top);
        end;
        if lvRightBorder then begin
          if (dgRowLines in Options) and (dgColLines in Options) then
            DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_RIGHT);
        end
        else
          Inc(TitleRect.Right,1);

        if dgColLines in Options then begin
          Canvas.Pen.Color := clBlack;
          Canvas.MoveTo(TitleRect.Right, TitleRect.Top);
          Canvas.LineTo(TitleRect.Right, TitleRect.Bottom + 1);
        end;
        if lvDownBorder and ((dgRowLines in Options) and (dgColLines in Options)) then
          DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_BOTTOM);

        if dgRowLines in Options then begin
          Canvas.Pen.Color := clBlack;
          Canvas.MoveTo(TitleRect.Left,TitleRect.Bottom);
          Canvas.LineTo(TitleRect.Right + 1,TitleRect.Bottom);
        end;
        if lvUpBorder and ((dgRowLines in Options) and (dgColLines in Options)) then
          DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_TOP);

        if lvLeftBorder and ((dgRowLines in Options) and (dgColLines in Options)) then
          DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_LEFT);
      end;

      if lvShowCaption then begin
        // Paint skinned column if caption is shown only
        lvTmpColIndex := GetRightColIndex(ACol, CurLevel);
        iMaxRow := GetBottomRow(ACol, CurLevel);
        if ACol <> lvTmpColIndex then
          R := CalcTitleRect(Columns[lvTmpColIndex], ARow, MasterCol)
        else
          R := TitleRect;

        R.TopLeft := TitleRect.TopLeft;
        if iMaxRow <> CurLevel then
          CalcTitleLevel(iMaxRow, TitleRect);

        if (R.Top = 0) and (CaptionDepth > 1) then
          R.Bottom := TitleRect.Top
        else
          R.Bottom := TitleRect.Bottom;

        inc(R.Bottom);
        if bSkinned then begin
          Inc(R.Right);
          if iSkinNdx >= 0 then begin
            PaintItem(iSkinNdx, CI, True, 0, R, MkPoint, Canvas.Handle, FCommonData.CommonSkinData);
            if Column.Title.Font.Color = clWindowText then
              Canvas.Font.Color := FCommonData.CommonSkinData.gd[iSkinNdx].Props[0].FontColor.Color
            else
              Canvas.Font.Color := acColorToRGB(Column.Title.Font.Color);
          end
          else begin
            if Column.Title.Color = clBtnFace then
              Canvas.Brush.Color := SkinData.SkinManager.Palette[pcMainColor]
            else
              Canvas.Brush.Color := acColorToRGB(Column.Title.Color);

            FillDC(Canvas.Handle, R, Canvas.Brush.Color);
            if Column.Title.Font.Color = clWindowText then
              Canvas.Font.Color := FCommonData.SkinManager.Palette[pcLabelText]
            else
              Canvas.Font.Color := acColorToRGB(Column.Title.Font.Color);
          end;
{$IFDEF DELPHI7UP}
        end
        else
          if acThemesEnabled then begin
            Elem := thHeaderItemNormal;
            Details := acThemeServices.GetElementDetails(Elem);
            acThemeServices.DrawElement(Canvas.Handle, Details, R);
{$ENDIF}
          end;

        CheckWidth;
        rText := R;
        if (TacDBColumn(Column).SortOrder <> csoNone) and (CurLevel = CaptionDepth - 1) then begin
          if bSkinned then begin
            ArrowSize := MkSize(SkinData.CommonSkinData.ArrowSize * 2, SkinData.CommonSkinData.ArrowSize);
            ArrowStyle := SkinData.SkinManager.Options.ActualArrowStyle;
          end
          else begin
            ArrowSize := MkSize(ac_BaseArrowWidth * 2, ac_BaseArrowWidth);
            ArrowStyle := arsSolid1;
          end;

          if TacDBColumn(Column).SortOrder = csoAsc then
            ArrowSide := asBottom
          else
            ArrowSide := asTop;

          DrawText(Canvas.Handle, PChar(CurCaption), Length(CurCaption), R, DT_CALCRECT);

          rText.Top := R.Top;
          rText.Bottom := R.Bottom;
          rText.Left := rText.Left + (WidthOf(rText) - WidthOf(R) - ArrowSize.cx - SkinData.CommonSkinData.Spacing) div 2;
          rText.Right := rText.Left + WidthOf(R) + 2;

          rArrow.Top := rText.Top;
          rArrow.Bottom := rText.Bottom;
          if IsRightToLeft then begin
            rArrow.Right := rText.Left - SkinData.CommonSkinData.Spacing;
            rArrow.Left := rArrow.Right - ArrowSize.cx;
          end
          else begin
            rArrow.Left := rText.Right + SkinData.CommonSkinData.Spacing;
            rArrow.Right := rArrow.Left + ArrowSize.cx;
          end;

          DrawArrow(Canvas.Handle, Canvas.Font.Color, clNone, rArrow, ArrowSide, 0, 0, ArrowSize.cy, ArrowStyle, GetPPI(SkinData));
          InflateRect(rText, 4, 0);
        end;
        Canvas.Brush.Style := bsClear;
        WriteText(Canvas, rText, FrameOffs + PressOffset, FrameOffs + PressOffset, CurCaption, lvCaptionAlignment, IsRightToLeft);
      end
      else
{$IFDEF DELPHI7UP}
        if not (bSkinned or acThemesEnabled) then
{$ENDIF}
          if CurCaption = '' then begin
            if not bSkinned then
              WriteText(Canvas, TextRect, FrameOffs, FrameOffs, '', lvCaptionAlignment, IsRightToLeft) // mean there is continue of previous column
          end
          else begin
            if dgColLines in Options then begin
              dec(TextRect.Left);
              dec(lvCaptionXOffset);
            end;
            CheckWidth;
            WriteText(Canvas, TextRect, FrameOffs - lvCaptionXOffset, FrameOffs, CurCaption, lvCaptionAlignment, IsRightToLeft);
          end;
    end;
    AState := AState - [gdFixed]; // prevent box drawing later
  end;

var
  DrawColumn: TColumn;
begin
  CI := EmptyCI;
  CI.FillColor := acColorToRGB(Color);
//  CI.FillRect := MkRect;
//  CI.Ready := False;
//  CI.Bmp := nil;
  if UseRightToLeftAlignment then
    Canvas.TextFlags := Canvas.TextFlags or ETO_RTLREADING;

  if (ARow = 0) and (dgTitles in Options) then // Columns line
    if ACol >= IndicatorOffset then begin
      DrawColumn := Columns[ACol - IndicatorOffset];
      DrawTitleCell(ACol - IndicatorOffset, ARow, DrawColumn, AState);
    end
    else
      if SkinData.Skinned then begin
        inc(aRect.Right);
        inc(aRect.Bottom);
        DrawIndicator(aRect, False);
      end
      else
        inherited DrawCell(ACol, ARow, ARect, AState)
  else begin
    if DataLink.Active then
      if dgTitles in Options then
        FActiveRowSelected := ARow - 1 = DataLink.ActiveRecord
      else
        FActiveRowSelected := ARow = DataLink.ActiveRecord
    else
      FActiveRowSelected := False;

    if SkinData.Skinned then begin
      if ACol - IndicatorOffset < 0 then begin
        inc(ARect.Right);
        inc(ARect.Bottom);
        DrawIndicator(ARect);
      end
      else
        inherited DrawCell(ACol, ARow, ARect, AState);

      if not (gdFixed in AState) then begin
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := SkinData.SkinManager.Palette[pcGrid];
        Canvas.Pen.Style := psSolid;
        if dgColLines in Options then begin
          Canvas.MoveTo(aRect.Right, aRect.Top);
          Canvas.LineTo(aRect.Right, aRect.Bottom + 1);
        end;
        if dgRowLines in Options then begin
          Canvas.MoveTo(aRect.Left, aRect.Bottom);
          Canvas.LineTo(aRect.Right, aRect.Bottom);
        end;
      end;
    end
    else begin
      inherited DrawCell(ACol, ARow, ARect, AState);
{$IFDEF DELPHI7UP}
      if not acThemesEnabled then
{$ENDIF}
        if gdFixed in AState then begin
          if dgColLines in Options then begin
            Canvas.Pen.color := clBlack;
            Canvas.Pen.style := psSolid;
            Canvas.MoveTo(aRect.Right, aRect.Top);
            Canvas.LineTo(aRect.Right, aRect.Bottom + 1);
          end;
          if dgRowLines in Options then begin
            Canvas.Pen.color := clBlack;
            Canvas.Pen.style := psSolid;
            Canvas.MoveTo(aRect.Left, aRect.Bottom);
            Canvas.LineTo(aRect.Right, aRect.Bottom);
          end;
        end;
    end;
  end;
end;


procedure TsDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ColWidth, ValueWidth, Pos, Ofs, OldActive, i: integer;
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Cell: TGridCoord;
  Column: TColumn;
  Index: longint;
  Value: string;
begin
  FExecColAjust := False;
  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;

  if not (State in [gsColSizing]) and DataLink.Active then
    if (Button = mbLeft) and (dgTitles in Options) then begin
      Cell := MouseCoord(X, Y);
      if Cell.X >= IndicatorOffset then begin
        if not (dgRowSelect in Options) and
             (Cell.Y >= FixedRows) and
               (TopRow + Cell.Y - FixedRows = Row) and
                 IsOnButton(X,Y) then begin
          FCellButtonDown := RawToDataColumn(Cell.X);
          FCellButtonRow := Cell.Y;
          FCellButtonCol := Cell.X;
          FCellButtonBRect := GetButtonRect(Cell);
          FCellButtonRect := CellRect(Cell.X,Cell.Y);
          HideEditor;
          DrawButton(Cell.X, Cell.Y, PtInRect(FCellButtonBRect, Point(x, y)));
          FCellButtonPressed := True;
          Exit;
        end;
        if DataLink.Active and (Cell.Y < FixedRows) and FColumnSort and MouseInLowerstLevel(X, Y, nil) then begin
          i := FTitleButtonDown;
          FTitleButtonDown := RawToDataColumn(Cell.X);
          FOldTitleButtonDown := FTitleButtonDown;
          if i >= 0 then
            InvalidateCol(i + 1);

          InvalidateCol(FTitleButtonDown + 1);
        end;
      end;
    end;

  if (mbLeft = Button) and DataLink.Active then
    if (State = gsColSizing) then begin
      if ssDouble in Shift then begin
        Index := Min(RawToDataColumn(MouseCoord(X, Y).X), RawToDataColumn(MouseCoord(X - 7, Y).X));
        if Index < 0 then
          Index := Columns.Count - 1;

        Column := Columns[Index];
        ColWidth := 0;
        OldActive := DataLink.ActiveRecord;
        try
          for i := TopRow - 1 to VisibleRowCount - 1 do begin
            Datalink.ActiveRecord := i;
            if Assigned(Column.Field) then
              Value := Column.Field.DisplayText
            else
              Value := '';

            ValueWidth := Canvas.TextWidth(Value);
            if ValueWidth > ColWidth then
              ColWidth := ValueWidth;
          end;
        finally
          DataLink.ActiveRecord := OldActive;
        end;
        ColWidths[Index + IndicatorOffset] := ColWidth + 4;
        FExecColAjust := True;
      end;
    end;

  inherited;
end;


procedure TsDBGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Pos, Ofs: integer;
  Index: Longint;
  Col: TColumn;
  Rect: TRect;
begin
  inherited;
  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;

  if FCellButtonDown >= 0 then begin
    FCellButtonPressed := PtInRect(FCellButtonBRect, Point(x, y));
    DrawButton(FCellButtonCol,FCellButtonRow,FCellButtonPressed);
  end;
  if (ssLeft in Shift) and (FOldTitleButtonDown >= 0) then begin
    Rect := CalcTitleRect(Columns[FOldTitleButtonDown], 0, Col);
    if (FTitleButtonDown < 0) and PtInRect(Rect, Point(X, Y)) then begin
      FTitleButtonDown := FOldTitleButtonDown;
      InvalidateCol(FTitleButtonDown + 1);
    end
    else
      if (FTitleButtonDown >= 0) and ((Y < Rect.Top) or (Y > Rect.Bottom) or ((X < Self.Left) and
           (Columns[FTitleButtonDown].Index = 0)) or ((X > Self.Left + Self.Width) and
             (Columns[FTitleButtonDown].Index = Columns.Count - 1))) then begin
        Index := FTitleButtonDown + 1;
        FTitleButtonDown := -1;
        InvalidateCol(Index)
      end;
  end;
end;


procedure TsDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Pos, Ofs, LastBtn: integer;
  Widths: array of integer;
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Index, i: Longint;
  Column: TColumn;
  Cell: TGridCoord;
begin
  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;

  FTitleBarUp := False;
  if not (State in [gsColSizing]) and DataLink.Active and not FExecColAjust then begin
    Cell := MouseCoord(X, Y);
    if not (dgRowSelect in Options) then
      if FCellButtonDown >= 0 then begin
        DrawButton(Cell.X, Cell.Y, False);
        if FCellButtonDown = RawToDataColumn(Cell.X) then
          if FCellButtonPressed then begin
            FCellButtonDown := -1;
            FCellButtonRow := -1;
            FCellButtonCol := -1;
            Invalidate;
            EditButtonClick;
          end;
      end;

    FCellButtonDown := -1;
    FCellButtonRow := -1;
    FCellButtonCol := -1;
    LastBtn := FTitleButtonDown;
    FOldTitleButtonDown := -1;
    if FTitleButtonDown >= 0 then begin
      InvalidateCol(FTitleButtonDown + 1);
      FTitleButtonDown := - 1;
    end;
    if Button = mbLeft then
      if (Cell.Y = 0) and (dgTitles in Options) then begin
        if Cell.X >= IndicatorOffset then begin
          Column := Columns[RawToDataColumn(Cell.X)];
          FTitleBarUp := True;
          if TacDBColumn(Column).CanBeSorted and FColumnSort and MouseInLowerstLevel(X, Y, Column) and (LastBtn = Column.Index) then begin
            FExecSorting := True;
            BeginLayout;
            try
              SetLength(Widths, Columns.Count);
              for i := 0 to Columns.Count - 1 do
                Widths[i] := Columns[i].Width;
            finally
              EndLayout;
              for i := 0 to Columns.Count - 1 do
                Columns[i].Width := Widths[i];

              FExecSorting := False;
            end;
          end;
        end;
      end
      else
        if not ReadOnly and (Cell.X >= 0) then begin
          inherited;
          FTitleBarUp := False;
          i := RawToDataColumn(Cell.X);
          if i >= 0 then begin
            Column := Columns[i];
            if (Column.Field <> nil) and Column.Field.DataSet.Active and not Column.ReadOnly and Column.Field.CanModify and TacDBColumn(Column).CanBeBoolean and TacDBColumn(Column).BoolAsCheckbox then
              if Column.Field.DataSet.State = dsEdit then begin
                EditorMode := False; // Prevents the grid to draw a blank cell
                TacDBColumn(Column).SetAsBool(not TacDBColumn(Column).GetAsBool);
              end
              else
                if DataSource.AutoEdit then begin
                  Column.Field.DataSet.Edit;
                  if TacDBColumn(Column).ImmediateToggle then
                    TacDBColumn(Column).SetAsBool(not TacDBColumn(Column).GetAsBool);
                end;
          end;
          Exit;
        end;
  end;
  inherited;
  FTitleBarUp := False;
end;


procedure TsDBGrid.UpdateHeaderHeight;
var
  Cur, i, aHeight: integer;
begin
  if not (dgTitles in Options) then
    RowHeights[0] := DefaultRowHeight
  else begin
    FHeaderHeight := 1;
    for i := 0 to Columns.Count - 1 do begin
      if {$IFNDEF D2005} (DataLink <> nil) and DataLink.Active and {$ENDIF} TacColumnTitle(Columns[i].Title).IsCaptionStored then
        Cur := GetCaptionDepth(Columns[i].Title.Caption, FLevelDelimiterChar)
      else
        Cur := 1;

      if Cur > FHeaderHeight then
        FHeaderHeight := Cur;
    end;
    aHeight := (DefaultRowHeight + 1) * FHeaderHeight;
    RowHeights[0] := aHeight - 1;
  end;
end;


function TsDBGrid.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if dgRowLines in options then
    Inc(Result.Bottom);
end;


procedure TsDBGrid.LayoutChanged;
var
  h: integer;
begin
  h := DefaultRowHeight;
  inherited;
  DefaultRowHeight := h;
  UpdateHeaderHeight;
end;


procedure TsDBGrid.SetLevelDelimiterChar(const Value: char);
begin
  FLevelDelimiterChar := Value;
end;


procedure TsDBGrid.SetColor(const Index: integer; Value: TColor);

  procedure ChangeValue(var aProp: TColor; aValue: TColor);
  begin
    if aProp <> aValue then begin
      aProp := aValue;
      Repaint;
    end;
  end;

begin
  case Index of
    0: ChangeValue(FRowColorEven, Value);
    1: ChangeValue(FRowColorOdd,  Value);
  end;
end;


function TsDBGrid.CalcFilterBar(Column: TColumn): TRect;
var
  MasterCol: TColumn;
  aRow: integer;
  Rect: TRect;
begin
  aRow := 0;
  Rect := CalcTitleRect(Column, aRow, MasterCol);
  Rect.Top := Rect.Bottom - (DefaultRowHeight + 9);
  Result := Rect;
end;


function TsDBGrid.MouseInLowerstLevel(X, Y: integer; Column: TColumn = nil): boolean;
var
  MasterCol: TColumn;
  Index: integer;
  Rect: TRect;
begin
  Result := False;
  if Column = nil then begin
    Index := RawToDataColumn(MouseCoord(X, Y).X);
    if Index < 0 then
      Exit;

    Column := Columns[Index];
  end;
  Index := 0;
  Rect := CalcTitleRect(Column, Index, MasterCol);
  Index := GetCaptionDepth(Column.Title.Caption, FLevelDelimiterChar);
  if Index > 0 then begin
    Index := (Index - 1) * (DefaultRowHeight + 1);
    Rect.Top := Index;
    Rect.Bottom := CalcFilterBar(Column).top;
    Result := PtInRect(Rect, Point(X, Y));
  end
  else
    Result := True;
end;


procedure TsDBGrid.CalcTableSpacePercent;
var
  ColumnsSize, i: integer;
begin
  ColumnsSize := 0;
  for i := 0 to Columns.count - 1 do
    if ColWidths[i + IndicatorOffset] > 0 then
      ColumnsSize := ColumnsSize + ColWidths[i + IndicatorOffset];

  for i := 0 to Columns.Count - 1 do
    if ColumnsSize > 0 then
      TacDBColumn(Columns[i]).FTableSpacePercent := ColWidths[i + IndicatorOffset] / ColumnsSize;
end;


function TsDBGrid.CanEditShow: boolean;
var
  iColIndex: integer;
begin
  iColIndex := SelectedIndex;
  if IsValidIndex(iColIndex, Columns.Count) and
       (((Columns[iColIndex].Field is TMemoField) and TacDBColumn(Columns[iColIndex]).CanShowBtn) or
         (Columns[iColIndex].CanBeBoolean and TacDBColumn(Columns[iColIndex]).BoolAsCheckbox)) then
    Result := False
  else
    Result := inherited CanEditShow;
end;


procedure TsDBGrid.TopLeftChanged;
{$IFDEF VER4}
var
  R: TRect;
  DrawInfo: TGridDrawInfo;
{$ENDIF}
begin
  inherited;
{$IFDEF VER4}
  if HandleAllocated and (dgTitles in Options) then begin
    CalcFixedInfo(DrawInfo);
    R := MkRect(Width, DrawInfo.Vert.FixedBoundary);
    InvalidateRect(Handle, {$IFNDEF CLR}@{$ENDIF}R, False);
  end;
{$ENDIF}
end;


procedure TsDBGrid.AdjustColumns;
var
  Width: array of integer;
  i, j, OldActive, CurWidth: integer;
begin
  if DataLink.Active then begin
    SetLength(Width, Columns.Count);
    OldActive := DataLink.ActiveRecord;
    try
      for i := TopRow - 1 to VisibleRowCount - 1 do begin
        Datalink.ActiveRecord := i;
        for j := 0 to Columns.Count - 1 do begin
          if Assigned(Columns[j].Field) then
            CurWidth := Canvas.TextWidth(Columns[j].Field.DisplayText)
          else
            CurWidth := 0;

          if CurWidth > Width[j] then
            Width[j] := CurWidth;
        end;
      end;
    finally
      DataLink.ActiveRecord := OldActive;
    end;
    for i := 0 to Columns.Count - 1 do begin
      CurWidth := Canvas.TextWidth(Columns[i].Title.Caption);
      if CurWidth > Width[i] then
        ColWidths[i + IndicatorOffset] := CurWidth + 4
      else
        ColWidths[i + IndicatorOffset] := Width[i] + 4;
    end;
  end;
end;


function TsDBGrid.GetColumns: TacDBGridColumns;
begin
  Result := TacDBGridColumns(inherited Columns);
end;


procedure TsDBGrid.SetColumns(const Value: TacDBGridColumns);
begin
  inherited Columns.Assign(Value);
end;


procedure TsDBGrid.WMMouseWheel(var Message: TWMMouseWheel);
begin
  SendMessage(Handle, WM_KEYDOWN, iff(Message.WheelDelta > 0, VK_UP, VK_DOWN), 0);
end;


procedure TsDBGrid.TitleClick(Column: TColumn);
begin
  if FTitleBarUp then
    inherited TitleClick(Column);
end;


procedure TsDBGrid.WndProc(var Message: TMessage);
var
  SavedDC: hdc;
  Offset: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end;

        AC_SETNEWSKIN:
          if Message.LParam = LParam(SkinData.SkinManager) then begin
{$IFDEF D2010}
            SaveColors;
{$ENDIF}
            if ListSW = nil then
              RefreshGridScrolls(SkinData, ListSW);

            CommonWndProc(Message, FCommonData);
            Exit;
          end;

        AC_REFRESH:
          if Message.LParam = LParam(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            if not (csLoading in ComponentState) and FCommonData.Skinned then begin
              RefreshGridScrolls(SkinData, ListSW);
              RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
            end;
            Exit;
          end;

        AC_REMOVESKIN:
          if Message.LParam = LParam(SkinData.SkinManager) then begin
//          if (Message.LParam = LParam(SkinData.SkinManager)) and (SkinData.FOwnerControl is TCustomGrid) then begin
            SkinData.SkinIndex := -1;
            Canvas.Brush.Handle := 0;
            Color := clWindow;
            Font.Color := clWindowText;
{$IFDEF D2010}
            RestoreColors;
{$ENDIF}
            FreeAndNil(ListSW);
            SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
            RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
          end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssEdit] + 1;

          Exit;
        end;
      end;

    WM_ERASEBKGND:
      if SkinData.Skinned then begin
        SkinData.FUpdating := SkinData.Updating;
        Message.Result := 1;
        Exit;
      end;

    WM_PRINT:
      with ListSW, TWMPaint(Message) do begin
        SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_PRINTING;
        if ListSW = nil then
          RefreshGridScrolls(Self.SkinData, ListSW);

        InitCtrlData(Handle, ListSW.ParentWnd, ListSW.WndRect, ListSW.ParentRect, ListSW.WndSize, ListSW.WndPos);
        SkinData.Updating := False;
        SkinData.BGChanged := True;
        if not ListSW.ParamsChanged then
          ListSW.SetSkinParams;

        PrepareCache(SkinData, CtrlHandle, DlgMode);
        SavedDC := SaveDC(DC);
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, cxLeftEdge);
        Message.Result := Ac_NCDraw(ListSW, Handle, -1, DC);
        Offset := cxLeftEdge - 2;

        MoveWindowOrg(DC, Offset, Offset);
        IntersectClipRect(DC, 0, 0,
                          SkinData.FCacheBmp.Width  - integer(sBarVert.fScrollVisible) * GetScrollMetric(sBarVert, SM_SCROLLWIDTH),
                          SkinData.FCacheBmp.Height - integer(sBarHorz.fScrollVisible) * GetScrollMetric(sBarHorz, SM_SCROLLWIDTH));

        if IsRightToLeft and sBarVert.fScrollVisible then
          MoveWindowOrg(DC, GetScrollMetric(sBarVert, SM_SCROLLWIDTH), 0);

        SendMessage(CtrlHandle, WM_PAINT, WParam(DC), 0);
        RestoreDC(DC, SavedDC);
        SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_PRINTING;
        Message.Result := 2; // Do not paint anymore
        Exit;
      end;
  end;
  CommonWndProc(Message, FCommonData);
  inherited;
  if ControlIsReady(Self) and FCommonData.Skinned then
    case Message.Msg of
      CM_SHOWINGCHANGED:
        RefreshGridScrolls(SkinData, ListSW);

      WM_SETFOCUS, WM_KILLFOCUS:
        if dgMultiSelect in Options then
          InvalidateRow(Row);
    end;
end;


procedure TsDBGrid.PaintWindow(DC: HDC);
var
  SavedCanvas: TCanvas;
  bWidth: integer;
  Bmp: TBitmap;
  R: TRect;
begin
  if FCommonData.Skinned then begin
    if ([csDestroying, csLoading, csReading] * ComponentState = []) then begin
      SavedCanvas := _TCustomControl(Self).FCanvas;
      Bmp := CreateBmp32(Width, Height);
      _TCustomControl(Self).FCanvas := Bmp.Canvas;
      try
        GetClipBox(DC, R);
        Paint;
        bWidth := integer(SkinData.CtrlSkinState and ACS_PRINTING <> 0) * 2;
        BitBlt(DC, bWidth, bWidth, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        _TCustomControl(Self).FCanvas := SavedCanvas;
        Bmp.Free;
      end;
    end
  end
  else
    inherited;
end;


function TsDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  Repaint;
end;


procedure TsDBGrid.WMVScroll(var Message: TWMVScroll);
var
  NewRecNo: Integer;
  SI: TScrollInfo;
begin
  if Message.ScrollCode = SB_THUMBTRACK then begin
    if DataLink.Active and HandleAllocated then begin
      SI.cbSize := sizeof(SI);
      SI.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SI);
      if SI.nTrackPos <= 1 then
        NewRecNo := 1
      else
        if SI.nTrackPos >= DataSource.DataSet.RecordCount then
          NewRecNo := DataSource.DataSet.RecordCount
        else
          NewRecNo := SI.nTrackPos;

      DataSource.DataSet.MoveBy(NewRecNo - DataSource.DataSet.RecNo);
    end;
  end
  else
    inherited;

  if Assigned(OnAfterScroll) then
    OnAfterScroll(Self, SB_VERT);
end;


procedure TsDBGrid.acUpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
  ScrollBarVisible: Boolean;
begin
  if Datalink.Active and HandleAllocated then
    with Datalink.DataSet do begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      ScrollBarVisible := RecordCount > 1;
      if ScrollBarVisible then begin
        SINew.nMin := 1;
        SINew.nPage := Self.VisibleRowCount;
        SINew.nMax := Integer(DWORD(RecordCount) + SINew.nPage - 1);
        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := RecNo; // else keep old pos
      end;
      ShowScrollBar(Self.Handle, SB_VERT, ScrollBarVisible);
      if ScrollBarVisible then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
end;


procedure TsDBGrid.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  if Assigned(OnAfterScroll) then
    OnAfterScroll(Self, SB_VERT);
end;

{$IFDEF D2010}
procedure TsDBGrid.SaveColors;
begin
  if not GradColorsSaved then begin
    SavedGradEndColor := GradientEndColor;
    SavedGradStartColor := GradientStartColor;
    GradColorsSaved := True;
  end;
end;


procedure TsDBGrid.RestoreColors;
begin
  if GradColorsSaved then begin
    GradientEndColor := SavedGradEndColor;
    GradientStartColor := SavedGradStartColor;
    GradColorsSaved := False;
  end;
end;
{$ENDIF}


procedure TsDBGrid.Scroll(Distance: Integer);
begin
  if RowColorOdd <> RowColorEven then begin
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    inherited;
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    Invalidate;
  end
  else
    inherited;

  if Assigned(FOnScrollData) then
    FOnScrollData(Self);
end;


function TacDBColumn.CanShowBtn: boolean;
begin
  Result := (Field <> nil) and (Field.DataType in [ftFmtMemo, ftMemo{$IFNDEF VER4}, ftOraClob {$ENDIF}]) and FShowMemoBtn;
end;

end.
