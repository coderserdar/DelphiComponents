unit o_GTDBGrid;

interface
uses
   Classes
  ,DBGrids
  ,Messages
  ,Controls
  ,DB
  ,Types
  ,Grids
  ,Graphics
  ,DBCtrls
  ,ComCtrls
  ;

{------------------------------------------------------------------------------}
type
{------------------------------------------------------------------------------}
  TgtInplaceEditor = (
                       ieDefault
                      ,ieBoolean
                      ,ieDateTime
                      ,ieMemo
                      ,ieProgress
                      )
                      ;
{------------------------------------------------------------------------------}
  TgtDBGrid = class;
{------------------------------------------------------------------------------}
  TgtDBGridMemo = class(TDBMemo)
  private
    FGrid: TgtDBGrid;
    procedure SetGrid(const Value: TgtDBGrid);
    { Private declarations }
  protected
    { Protected declarations }
    procedure WndProc(var Message : TMessage);override;
    procedure Notification(AComponent : TComponent ;Operation : TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Grid : TgtDBGrid read FGrid write SetGrid;
  end;
{------------------------------------------------------------------------------}
  TgtDBGridCheckBox = class(TDBCheckBox)
  private
    FGrid: TgtDBGrid;
    procedure SetGrid(const Value: TgtDBGrid);
    { Private declarations }
  protected
    { Protected declarations }
    procedure WndProc(var Message : TMessage);override;
    procedure Notification(AComponent : TComponent ;Operation : TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Grid : TgtDBGrid read FGrid write SetGrid;
  end;
{------------------------------------------------------------------------------}
  TgtDBGridProgress = class(TProgressBar)
  private
    FGrid: TgtDBGrid;
    procedure SetGrid(const Value: TgtDBGrid);
    { Private declarations }
  protected
    { Protected declarations }
    procedure WndProc(var Message : TMessage);override;
    procedure Notification(AComponent : TComponent ;Operation : TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property Grid : TgtDBGrid read FGrid write SetGrid;
  end;
{------------------------------------------------------------------------------}
  TgtDBGridColumn = class(TColumn)
  private
    FMinimumWidth : Integer;
    FAutoSize     : Boolean;
    FInplaceEditor: TgtInplaceEditor;
    FValueChecked: string;
    FValueUnChecked: string;
    FMaxValue: Integer;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property AutoSize       : Boolean           read FAutoSize       write FAutoSize;
    property MinimumWidth   : Integer           read FMinimumWidth   write FMinimumWidth;
    property InplaceEditor  : TgtInplaceEditor  read FInplaceEditor  write FInplaceEditor;
    property ValueChecked   : string            read FValueChecked   write FValueChecked;
    property ValueUnChecked : string            read FValueUnChecked write FValueUnChecked;
    property MaxValue       : Integer           read FMaxValue       write FMaxValue;
  end;
{------------------------------------------------------------------------------}
  TgtDBGridColumns = class(TDBGridColumns)
  private
    FGrid: TgtDBGrid;
    function GetColumn(Index: Integer): TgtDBGridColumn;
    procedure SetColumn(Index: Integer; const Value: TgtDBGridColumn);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Grid: TgtDBGrid; ColumnClass: TColumnClass);
    function  Add: TgtDBGridColumn;
    property  Grid: TgtDBGrid read FGrid;
    property  Items[Index: Integer]: TgtDBGridColumn read GetColumn write SetColumn; default;
  end;
{------------------------------------------------------------------------------}
  TgtDBGrid = class(TDBGrid)
  private
    FAutoResizeColumns: Boolean;
    FParentResize: TWinControl;
    FUseEnterAsTab: Boolean;
    FColorEvenRow: TColor;
    FColorOddRow: TColor;
    FColorRows: Boolean;
    FFontOddRow: TFont;
    FFontEvenRow: TFont;
    procedure SetAutoResizeColumns(const Value: Boolean);
    procedure SetParentResize(const Value: TWinControl);
    procedure SetFontEvenRow(const Value: TFont);
    procedure SetFontOddRow(const Value: TFont);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification (AComponent : TComponent ; Operation : TOperation);override;
    procedure WndProc(var Message : TMessage);override;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    function  CreateColumns: TDBGridColumns;override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);override;
  protected
    FOldParentOnResize : TNotifyEvent;
    FMemoEditor        : TgtDBGridMemo;
    FBooleanEditor     : TgtDBGridCheckBox;
    FProgessEditor     : TgtDBGridProgress;
    procedure EnabledAutoResize;
    procedure DisableAutoResize;
    procedure InternalOnParentResize(Sender: TObject);
    function  HasDataLink  : Boolean;
    procedure DrawColumnInplaceEditor(AColumn : TgtDBGridColumn;DataCol : Integer;const Rect : TRect;GridState : TGridDrawState);virtual;
    property  ParentResize : TWinControl read FParentResize write SetParentResize;
  protected
    procedure SetParent(AParent : TWinControl);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property AutoResizeColumns  : Boolean read FAutoResizeColumns write SetAutoResizeColumns;
    property UseEnterAsTab      : Boolean read FUseEnterAsTab     write FUseEnterAsTab;
    property ColorEvenRow       : TColor  read FColorEvenRow      write FColorEvenRow;
    property ColorOddRow        : TColor  read FColorOddRow       write FColorOddRow;
    property FontEvenRow        : TFont   read FFontEvenRow       write SetFontEvenRow;
    property FontOddRow         : TFont   read FFontOddRow        write SetFontOddRow;
    property ColorRows          : Boolean read FColorRows         write FColorRows;
  published
    property DataSource;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   Windows
  ,Forms //TBorderStyle;
  ,StdCtrls // TScrollStyle
  ,SysUtils
  ;






{ TgtDBGridColumn }
{------------------------------------------------------------------------------}
constructor TgtDBGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FMinimumWidth   := 40;
  FAutoSize       := True;
  FInplaceEditor  := ieDefault;
  FValueChecked   := '1';
  FValueUnChecked := '0';
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridColumn.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}








{ TgtDBGrid }

type
  _TWinControl = class(TWinControl);


{------------------------------------------------------------------------------}
constructor TgtDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseEnterAsTab      := True;
  FColorEvenRow       := clCream;
  FColorOddRow        := clLtGray;
  FColorRows          := False;
  FFontEvenRow        := TFont.Create;
  FFontOddRow         := TFont.Create;
  FMemoEditor         := TgtDBGridMemo.Create(Self);
  FMemoEditor.Grid    := Self;
  FBooleanEditor      := TgtDBGridCheckBox.Create(Self);
  FBooleanEditor.Grid := Self;
  FProgessEditor      := TgtDBGridProgress.Create(Self);
  FProgessEditor.Grid := Self;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGrid.Destroy;
begin
  FFontEvenRow.Free;
  FFontOddRow.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FParentResize then
      ParentResize := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
function TgtDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TgtDBGridColumns.Create(Self,TgtDBGridColumn);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  if (DataLink <> nil) and (DataLink.Active) then
  begin
    DataLink.DataSet.MoveBy(-Msg.WheelDelta div WHEEL_DELTA);
    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.EnabledAutoResize;
begin
  if Assigned(ParentResize) then
  begin
    FOldParentOnResize                  := _TWinControl(ParentResize).OnResize;
    _TWinControl(ParentResize).OnResize := InternalOnParentResize;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.DisableAutoResize;
begin
  if Assigned(ParentResize) then
  begin
    _TWinControl(ParentResize).OnResize := FOldParentOnResize;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.InternalOnParentResize(Sender: TObject);
var
  i        : integer;
  TotWidth : integer;
  VarWidth : integer;
  AColumn  : TgtDBGridColumn;
  ResizableColumnCount  : Integer;
begin
  if Assigned(FOldParentOnResize) then
    FOldParentOnResize(ParentResize);
  if HasDataLink then
  begin
    //total width of all columns before resize
    TotWidth := 0;
    ResizableColumnCount := 0;

    for i := 0 to Pred(Columns.Count) do
    begin
      TotWidth := TotWidth + Columns[i].Width;
      inc(ResizableColumnCount)
    end;

    if dgColLines in Self.Options then
      TotWidth := TotWidth + Columns.Count;

    //add indicator column width
    if dgIndicator in Self.Options then
      TotWidth := TotWidth + IndicatorWidth;

    //width vale "left"
    VarWidth :=  ClientWidth - TotWidth;

    VarWidth := VarWidth div Columns.Count;

    for i := 0 to Pred(ResizableColumnCount) do
    begin
      AColumn := TgtDBGridColumn(Columns[i]);
      if Assigned(AColumn) and (AColumn.AutoSize) then
      begin
        AColumn.MinimumWidth := 4 + Self.Canvas.TextWidth(AColumn.Field.AsString);
        AColumn.Width := AColumn.Width + VarWidth;
        if AColumn.Width < AColumn.MinimumWidth then
          AColumn.Width := AColumn.MinimumWidth;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FUseEnterAsTab then
  begin
    if Key = VK_RETURN then
    begin
      if HiWord(GetKeyState(VK_SHIFT)) <> 0 then
      begin
        if SelectedIndex > 0 then
          SelectedIndex := SelectedIndex  - 1
        else
        begin
          if HasDataLink then
          begin
            DataSource.DataSet.Prior;
            SelectedIndex := Pred(FieldCount);
          end;
        end;
      end
      else
      begin
        if SelectedIndex < Pred(FieldCount) then
          SelectedIndex := SelectedIndex + 1
        else
        begin
          if HasDataLink then
          begin
            DataSource.DataSet.Next;
            SelectedIndex := 0;
          end;
        end;
      end;
    end;
  end;
  inherited KeyDown(Key,Shift);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;Column: TColumn; State: TGridDrawState);
begin
  if FColorRows then
  begin
    if HasDataLink then
    begin
      if Odd(DataSource.DataSet.RecNo) then
      begin
        Canvas.Brush.Color := FColorOddRow;
        Canvas.Font        := FFontOddRow;
      end
      else
      begin
        Canvas.Font        := FFontEvenRow;
        Canvas.Brush.Color := FColorEvenRow;
      end;
    end;
  end;
  DrawColumnInplaceEditor(TgtDBGridColumn(Column),DataCol,Rect,State);
 // DefaultDrawColumnCell(Rect,DataCol,Column,State);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.DrawColumnInplaceEditor(AColumn: TgtDBGridColumn;DataCol : Integer;const Rect: TRect; GridState: TGridDrawState);
const IsChecked : array[Boolean] of Integer =
      (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var
  DrawRect : TRect;
  DrawState: Integer;
begin
  case AColumn.InplaceEditor of
    ieDefault : DefaultDrawColumnCell(Rect,DataCol,AColumn,GridState);
    ieMemo    :
      begin
        if (gdFocused in GridState) or (gdSelected in GridState) then
        begin
          FMemoEditor.BoundsRect := Rect;
          FMemoEditor.DataSource := DataSource;
          FMemoEditor.DataField  := AColumn.FieldName;
          FMemoEditor.Visible    := True;
          if gdSelected in GridState then
          begin
            FMemoEditor.SetFocus;
            FMemoEditor.SelectAll;
          end;
        end
        else
        begin
          FMemoEditor.Visible    := False;
          Canvas.FillRect(Rect);
          Canvas.TextOut(Rect.Left,Rect.Top,Copy(AColumn.Field.AsString,0,49));
        end;
      end;
    ieBoolean :
      begin
        if (gdFocused in GridState) or (gdSelected in GridState)  or (gdFixed in GridState) then
        begin
          Canvas.FillRect(Rect);
          FBooleanEditor.Left           := (Rect.Right - Rect.Left) div 2 + Rect.Left - 6;
          FBooleanEditor.Top            := Rect.Top;
          FBooleanEditor.Width          := (Rect.Right - Rect.Left);
          FBooleanEditor.DataSource     := DataSource;
          FBooleanEditor.DataField      := AColumn.FieldName;
          FBooleanEditor.ValueChecked   := AColumn.ValueChecked;
          FBooleanEditor.ValueUnchecked := AColumn.ValueUnChecked;
          FBooleanEditor.Visible        := True;
          if gdSelected in GridState then
          begin
            FBooleanEditor.SetFocus;
          end;
        end
        else
        begin
          DrawRect               := Rect;
          FBooleanEditor.Visible := False;
          Canvas.FillRect(DrawRect);
          InflateRect(DrawRect,-1,-1);

          if AColumn.Field.DataType = ftBoolean then
            DrawState := IsChecked[AColumn.Field.AsBoolean]
          else
          begin
            if SameText(AColumn.Field.AsString,AColumn.ValueChecked) then
              DrawState := IsChecked[False]
            else
              DrawState := IsChecked[True];
          end;
          DrawFrameControl(Canvas.Handle, DrawRect,DFC_BUTTON, DrawState);
        end;
      end;
    ieProgress :
      begin
        if (gdFocused in GridState) or (gdSelected in GridState)  or (gdFixed in GridState) then
        begin
          Canvas.FillRect(Rect);
          FProgessEditor.BoundsRect     := Rect;
          FProgessEditor.Max            := AColumn.MaxValue + 1;
          FProgessEditor.Position       := AColumn.Field.AsInteger;
          FProgessEditor.Visible        := True;
          if gdSelected in GridState then
          begin
            FProgessEditor.SetFocus;
          end;
        end
        else
        begin
         // Canvas.FillRect(DrawRect);
//          Canvas.Draw();
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtDBGrid.HasDataLink: Boolean;
begin
  Result := Assigned(DataLink) and (DataLink.Active);
end;
{------------------------------------------------------------------------------}







{------------------------------------------------------------------------------}
procedure TgtDBGrid.SetAutoResizeColumns(const Value: Boolean);
begin
  if FAutoResizeColumns <> Value then
  begin
    FAutoResizeColumns := Value;
    if FAutoResizeColumns then
      EnabledAutoResize
    else
      DisableAutoResize;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  ParentResize := AParent;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.SetParentResize(const Value: TWinControl);
begin
  if Assigned(FParentResize) then
    FParentResize.RemoveFreeNotification(Self);

  FParentResize := Value;

  if Assigned(FParentResize) then
    FParentResize.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.SetFontEvenRow(const Value: TFont);
begin
  FFontEvenRow.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGrid.SetFontOddRow(const Value: TFont);
begin
  FFontOddRow.Assign(Value);
end;
{------------------------------------------------------------------------------}

























{ TgtDBGridMemo }
{------------------------------------------------------------------------------}
constructor TgtDBGridMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  ParentColor := True;
  Ctl3D       := False;
  BorderStyle := bsNone;
  ScrollBars  := ssVertical;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridMemo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridMemo.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  //Passing the message to the Grid so it can handle the mouse wheel move.
  if Message.Msg = WM_MOUSEWHEEL then
    Grid.WindowProc(Message);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridMemo.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = Grid then
      FGrid := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtDBGridMemo.SetGrid(const Value: TgtDBGrid);
begin
  if Assigned(FGrid) then
  begin
    FGrid.RemoveFreeNotification(Self);
    Parent := nil;
  end;

  FGrid := Value;

  if Assigned(FGrid) then
  begin
    FGrid.FreeNotification(Self);
    Parent := FGrid;
  end;
end;
{------------------------------------------------------------------------------}

{ TgtDBGridCheckBox }

{------------------------------------------------------------------------------}
constructor TgtDBGridCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible     := False;
  ParentColor := True;
  Ctl3D       := False;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridCheckBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridCheckBox.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FGrid then
      Grid := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridCheckBox.SetGrid(const Value: TgtDBGrid);
begin
  if Assigned(FGrid) then
  begin
    FGrid.RemoveFreeNotification(Self);
    Parent := nil;
  end;

  FGrid := Value;

  if Assigned(FGrid) then
  begin
    FGrid.FreeNotification(Self);
    Parent := FGrid;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridCheckBox.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  //Passing the message to the Grid so it can handle the mouse wheel move.
  if Message.Msg = WM_MOUSEWHEEL then
    Grid.WindowProc(Message);
end;
{------------------------------------------------------------------------------}



{ TgtDBGridProgress }
{------------------------------------------------------------------------------}
constructor TgtDBGridProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible     := False;
  ParentColor := True;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridProgress.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridProgress.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FGrid then
      Grid := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridProgress.SetGrid(const Value: TgtDBGrid);
begin
  if Assigned(FGrid) then
  begin
    FGrid.RemoveFreeNotification(Self);
    Parent := nil;
  end;

  FGrid := Value;

  if Assigned(FGrid) then
  begin
    FGrid.FreeNotification(Self);
    Parent := FGrid;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridProgress.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  //Passing the message to the Grid so it can handle the mouse wheel move.
  if Message.Msg = WM_MOUSEWHEEL then
    Grid.WindowProc(Message);
end;
{------------------------------------------------------------------------------}




{ TgtDBGridColumns }
{------------------------------------------------------------------------------}
constructor TgtDBGridColumns.Create(Grid: TgtDBGrid;ColumnClass: TColumnClass);
begin
  inherited Create(Grid,ColumnClass);
  FGrid := Grid;
end;
{------------------------------------------------------------------------------}
function TgtDBGridColumns.Add: TgtDBGridColumn;
begin
  Result :=  TgtDBGridColumn(inherited Add);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridColumns.SetColumn(Index: Integer;const Value: TgtDBGridColumn);
begin
  Items[Index].Assign(Value);
end;
{------------------------------------------------------------------------------}
function TgtDBGridColumns.GetColumn(Index: Integer): TgtDBGridColumn;
begin
  Result := nil;
  if (Count >0) and (Index >=0) and (Index <= Count) then
    Result := TgtDBGridColumn(inherited Items[Index]);
end;
{------------------------------------------------------------------------------}






end.
