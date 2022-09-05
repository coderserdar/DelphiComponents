unit DXMapEdit;
//(c)2007 Jaro Benes
//All Rights Reserved

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) create image map and store it into rersource.
 b) allow do change.
 c) controll map by each chip.

}
interface
{$INCLUDE DelphiXcfg.inc}        
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, {$IFDEF VER4UP}ImgList,{$ENDIF}
  Grids, Menus, DXMapEditProperties, Spin,
  DXSprite, DXClass, DXDraws{$IfNDef StandardDX}, DirectX{$EndIf}, DIB;

type
  {injected class}

  {  TEdit  }

  TEdit = class(StdCtrls.TEdit)
  private
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  published
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  {  TDrawGrid  }
  
//  TDrawGrid = class(Grids.TDrawGrid)
//    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
//    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
//  end;

  {  TDelphiXMapEditForm  }

  TDelphiXMapEditForm = class(TForm)
    pblBase: TPanel;
    LHeight: TLabel;
    LMapSizeX: TLabel;
    LMapSizeY: TLabel;
    EHeight: TEdit;
    EWidth: TEdit;
    eMapSizeX: TSpinEdit;
    eMapSizeY: TSpinEdit;
    OKButton: TButton;
    CancelButton: TButton;
    LWidth: TLabel;
    ImageToSet: TComboBox;
    LImageToSet: TLabel;
    ScrollBox1: TScrollBox;
    BtnSetSize: TSpeedButton;
    PicturesToChip: TListBox;
    LPicturesToChip: TLabel;
    MapArea: TDrawGrid;
    PopupMenu1: TPopupMenu;
    Fillall1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Clear1: TMenuItem;
    LAreaOfChips: TLabel;
    ClearOneChip1: TMenuItem;
    DelphiXMapEditPropertiesPane: TPanel;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    pnlLabels: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure OKButtonClick(Sender: TObject);
    procedure MapAreaDblClick(Sender: TObject);
    procedure ClearOneChip1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Fillall1Click(Sender: TObject);
    procedure MapAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageToSetChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PicturesToChipDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PicturesToChipMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MapAreaDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure MapAreaDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BtnSetSizeClick(Sender: TObject);
    procedure MapAreaDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MapAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    DIB: TDIB;
    maparea_X, maparea_Y: Integer;
    pct: TDXPictureClip;
    FocusPopUpPositionCol, FocusPopUpPositionRow: Integer;
    DragPositionCol, DragPositionRow: Integer;
    DelphiXMapEditPropertiesForm: TDelphiXMapEditPropertiesForm;
    procedure MapTypeDefaultValuesByObject(out MapType: TMapType);
  public
    DXBackgroundSprite: TBackgroundSprite;
    DXImageList: TCustomDXImageList; //instance only
    function LoadSplittedImage(ClearContentOfMapArea: Boolean{$IFDEF VER4UP} = False{$ENDIF}): Boolean;
    procedure ResizeMapArea;
  end;

var
  DelphiXMapEditForm: TDelphiXMapEditForm;

implementation

{$R *.dfm}

{ TEdit }

function TEdit.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Self.Text);
  except
    Result := 0;
  end;
end;

procedure TEdit.SetAsInteger(const Value: Integer);
begin
  Self.Text := IntToStr(Value)
end;

{ TDrawGrid  }

//procedure TDrawGrid.CMMouseEnter(var Msg: TMessage);
//begin
//
//end;
//
//procedure TDrawGrid.CMMouseLeave(var Msg: TMessage);
//begin
//end;

//------------------------------------------------------------------------------

{TDelphiXMapEditForm}

procedure TDelphiXMapEditForm.MapAreaDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  DropPositionCol, DropPositionRow: Integer;
begin
  MapArea.MouseToCell(X, Y, DropPositionCol, DropPositionRow); // convert mouse coord.
  if Source is TListBox then begin
    if Sender is TDrawGrid then
      with Sender as TDrawGrid do begin
        with DXBackgroundSprite do
          Chips[DropPositionCol, DropPositionRow] := (Source as TListBox).ItemIndex;
        Invalidate;
      end;
  end
  else
    if Source is TDrawGrid then begin
      if Sender is TDrawGrid then
        if (DropPositionCol <> DragPositionCol) or (DropPositionRow <> DragPositionRow) then
          with Sender as TDrawGrid do begin
            with DXBackgroundSprite do
              Map[DropPositionCol, DropPositionRow] := Map[DragPositionCol, DragPositionRow];
            Invalidate;
          end;
    end;
end;

procedure TDelphiXMapEditForm.MapAreaDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  CurrentCol, CurrentRow: Integer;
begin
  MapArea.MouseToCell(X, Y, CurrentCol, CurrentRow); // convert mouse coord.
  Accept := ((Source = PicturesToChip) or (Source = Sender))
  { Accept dragged stuff only when the mouse is now over an acceptable region }
  and (CurrentCol >= 0) and (CurrentRow >= 0);
  if Source = Sender then
    MapArea.SetFocus;
end;

procedure TDelphiXMapEditForm.MapAreaDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  TmpRect: TRect;
  A: array[1..255] of Char;
begin
  with MapArea.Canvas do begin
    FillRect(Rect);
    if pct.IsEmpty then Exit;
    with DXBackgroundSprite do begin
      DIB.SetSize(pct.Width, pct.Height, 24);
      pct.Draw(DIB.Canvas, 0, 0, Chips[ACol, ARow]);
      if gdFocused in State then begin
        DIB.DoDarkness(80);
      end
      else begin
        if CollisionMap[ACol, ARow] then
          DIB.Darker(50)
      end;
      if Map[ACol, ARow].MirrorFlip <> [] then DIB.Mirror(rmfMirror in Map[ACol, ARow].MirrorFlip, rmfFlip in Map[ACol, ARow].MirrorFlip);
      Draw(Rect.Left, Rect.Top, DIB);
    end;
    if gdFocused in State then
    begin
      if goDrawFocusSelected in MapArea.Options then
        Pen.Color := clHighlight
      else
        Pen.Color := MapArea.Color;
      Brush.Style := bsClear;
{$IFDEF VER5UP}
      Rectangle(Rect); InFlateRect(Rect, -1, -1); Rectangle(Rect);
{$ELSE}
      Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom); InFlateRect(Rect, -1, -1);Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
{$ENDIF}
    end;
    with DXBackgroundSprite do begin
      Font.Name := 'Arial';
      Font.Size := 9;
      Font.Color := clHighlightText;
      Brush.Style := bsClear;
      TmpRect := Rect;
      TmpRect.Bottom := TmpRect.Bottom - ((TmpRect.Bottom - TmpRect.Top) div 2);
      if CollisionMap[ACol, ARow] then
        DrawText(MapArea.Canvas.Handle, StrPCopy(@A, 'brick'), -1, TmpRect, dt_SingleLine or dt_Center or dt_VCenter);
      TmpRect := Rect;
      TmpRect.Top := TmpRect.Bottom - ((TmpRect.Bottom - TmpRect.Top) div 2);
      case Map[ACol, ARow].Rendered of
        rtDraw: DrawText(MapArea.Canvas.Handle, StrPCopy(@A, 'D:' + IntToStr(Map[ACol, ARow].Alpha)), -1, TmpRect, dt_SingleLine or dt_Center or dt_VCenter);
        rtBlend: DrawText(MapArea.Canvas.Handle, StrPCopy(@A, 'B:' + IntToStr(Map[ACol, ARow].Alpha)), -1, TmpRect, dt_SingleLine or dt_Center or dt_VCenter);
        rtAdd: DrawText(MapArea.Canvas.Handle, StrPCopy(@A, 'A:' + IntToStr(Map[ACol, ARow].Alpha)), -1, TmpRect, dt_SingleLine or dt_Center or dt_VCenter);
        rtSub: DrawText(MapArea.Canvas.Handle, StrPCopy(@A, 'S:' + IntToStr(Map[ACol, ARow].Alpha)), -1, TmpRect, dt_SingleLine or dt_Center or dt_VCenter);
      end
    end;
  end;
end;

procedure TDelphiXMapEditForm.FormCreate(Sender: TObject);
begin
  DelphiXMapEditPropertiesForm := TDelphiXMapEditPropertiesForm.Create(Self);
  DelphiXMapEditPropertiesForm.Parent := DelphiXMapEditPropertiesPane;
  DelphiXMapEditPropertiesForm.ParentWindow := Self.DelphiXMapEditPropertiesPane.Handle;
  DelphiXMapEditPropertiesForm.Top := 0;
  DelphiXMapEditPropertiesForm.Left := 0;
  DelphiXMapEditPropertiesForm.BorderStyle := bsNone;
  DelphiXMapEditPropertiesForm.Align := alClient;
  {nothing selected}
  FocusPopUpPositionCol := -1;
  FocusPopUpPositionRow := -1;
  DIB := TDIB.Create;
  pct := TDXPictureClip.Create(nil);
{$IFDEF VER4UP}
  MapArea.DoubleBuffered := True;
{$ENDIF}
end;

procedure TDelphiXMapEditForm.PicturesToChipMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := pct.Height;
end;

procedure TDelphiXMapEditForm.ResizeMapArea;
{$IFNDEF VER4UP}
type
  TOldMap = array[0..0, 0..0] of TMapType;
  POldMap = ^TOldMap;
{$ENDIF}
var
{$IFDEF VER4UP}
  oldmap: array of array of TMapType;
{$ELSE}
  oldmap: POldMap;
{$ENDIF}
  i, j, oldw, oldh: Integer;
  MapType: TMapType;
begin
  {older size, save old layout}
  oldw := DXBackgroundSprite.MapWidth;
  oldh := DXBackgroundSprite.MapHeight;
{$IFDEF VER4UP}
  SetLength(oldmap, oldw, oldh);
{$ELSE}
  GetMem(oldmap, oldw * oldh * SizeOf(TMapType));
  try
{$ENDIF}
    {old the content of map}
    if not DXBackgroundSprite.IsMapEmpty then
      for i := 0 to oldw - 1 do
        for j := 0 to oldh - 1 do
          oldmap{$IFNDEF VER4UP}^{$ENDIF} [i, j] := DXBackgroundSprite.Map[i, j];
    {size realocation}
    with DXBackgroundSprite do begin
      SetMapSize(eMapSizeX.Value, eMapSizeY.Value);
      {prevent resize of cleared by the content object}
      MapTypeDefaultValuesByObject(MapType);
      for i := 0 to eMapSizeX.Value - 1 do
        for j := 0 to eMapSizeY.Value - 1 do begin
          Chips[i, j] := 0;
          CollisionMap[i, j] := False;
          Map[i, j] := MapType;
        end;
    end;
    {back context}
    for i := 0 to oldw - 1 do
      for j := 0 to oldh - 1 do
        DXBackgroundSprite.Map[i, j] := oldmap{$IFNDEF VER4UP}^{$ENDIF} [i, j];
{$IFNDEF VER4UP}
  finally
    FreeMem(oldmap)
  end;
{$ENDIF}
  MapArea.ColCount := eMapSizeX.Value;
  MapArea.RowCount := eMapSizeY.Value;
  MapArea.Width := eMapSizeX.Value * (EWidth.AsInteger + 1);
  MapArea.Height := eMapSizeY.Value * (EHeight.AsInteger + 1);
  MapArea.Invalidate;
end;

procedure TDelphiXMapEditForm.MapTypeDefaultValuesByObject(out MapType: TMapType);
begin
  FillChar(MapType, SizeOf(MapType), 0);
  { default values from owner's object }
  MapType.CollisionChip := DXBackgroundSprite.Collisioned;
  MapType.Overlap := 0;
  MapType.AnimLooped:= DXBackgroundSprite.AnimLooped;
  MapType.AnimStart := DXBackgroundSprite.AnimStart;
  MapType.AnimCount := DXBackgroundSprite.AnimCount;
  MapType.AnimSpeed := DXBackgroundSprite.AnimSpeed;
  MapType.Alpha := DXBackgroundSprite.Alpha;
  MapType.Rendered := DXBackgroundSprite.BlendMode;
  MapType.AnimPos := DXBackgroundSprite.AnimPos;
  MapType.Angle := DXBackgroundSprite.Angle;
  MapType.MirrorFlip := DXBackgroundSprite.MirrorFlip;
  MapType.TextureFilter := DXBackgroundSprite.TextureFilter;
  MapType.CenterX := DXBackgroundSprite.CenterX;
  MapType.CenterY := DXBackgroundSprite.CenterY;
end;

procedure TDelphiXMapEditForm.BtnSetSizeClick(Sender: TObject);
begin
  ResizeMapArea
end;

procedure TDelphiXMapEditForm.PicturesToChipDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R: TRect;
  I: Integer;
begin
  with PicturesToChip.Canvas do begin
    if not (odSelected in State) then
      if Odd(Index) then Brush.Color := {$IFDEF VER6UP}clMoneyGreen{$ELSE}clGreen{$ENDIF}
      else Brush.Color := clWhite;
    FillRect(Rect);
    pct.Draw(PicturesToChip.Canvas, Rect.Left, Rect.Top, Index);
    Brush.Style := bsClear;
    R := Rect;
    R.Left := Rect.Left + pct.Width + 2;
    I := Rect.Top + (Rect.Bottom - Rect.Top - PicturesToChip.Canvas.TextHeight(PicturesToChip.Items[index])) div 2;
    TextOut(Rect.Left + pct.Width + 2, I, PicturesToChip.Items[index]);
  end;
end;

procedure TDelphiXMapEditForm.FormDestroy(Sender: TObject);
begin
  DIB.Free;
  pct.Free;
end;

function TDelphiXMapEditForm.LoadSplittedImage(ClearContentOfMapArea: Boolean): Boolean;
var
  I, V: Integer;
begin
  if (ImageToSet.ItemIndex <> -1) and Assigned(DXImageList) then begin
    DXImageList.Items[ImageToSet.ItemIndex].Restore;
    pct.Picture := DXImageList.Items[ImageToSet.ItemIndex].Picture;
    pct.Width := DXImageList.Items[ImageToSet.ItemIndex].PatternWidth;
    pct.Height := DXImageList.Items[ImageToSet.ItemIndex].PatternHeight;
    EWidth.AsInteger := pct.Width;
    EHeight.AsInteger := pct.Height;
    PicturesToChip.Clear;
    V := DXImageList.Items[ImageToSet.ItemIndex].PatternCount;
    for I := 0 to V - 1 do begin
      PicturesToChip.Items.Add(Format(ImageToSet.Text + ' [%d]', [I]));
    end;
    PicturesToChip.ItemHeight := DXImageList.Items[ImageToSet.ItemIndex].PatternHeight;
    {seznam je videt a take MapArea}
    PicturesToChip.Visible := True;
    {pri zmene obrazku vymazat content!}
    if ClearContentOfMapArea then
      Clear1.Click;
    MapArea.Visible := True;
    MapArea.Invalidate;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDelphiXMapEditForm.ImageToSetChange(Sender: TObject);
begin
  if ImageToSet.ItemIndex <> -1 then
  begin
    LoadSplittedImage(True);
  end;
end;

procedure TDelphiXMapEditForm.MapAreaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { Convert mouse coordinates X, Y to to StringGrid related col and row numbers }
  MapArea.MouseToCell(X, Y, DragPositionCol, DragPositionRow);
  { Allow dragging only if an acceptable cell was clicked (cell beyond the fixed column and row) }
  if (Button = mbLeft) and (DragPositionCol >= 0) and (DragPositionRow >= 0) then
  begin
    { Begin dragging after mouse has moved 4 pixels }
    MapArea.BeginDrag(False{$IFDEF VER4UP}, 2{$ENDIF});
  end;
end;

procedure TDelphiXMapEditForm.MapAreaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  FocusPositionCol, FocusPositionRow: Integer;
begin
  maparea_X := X; maparea_Y := Y;
  MapArea.MouseToCell(X, Y, FocusPositionCol, FocusPositionRow);
  if not MapArea.Focused then
    MapArea.SetFocus;
  MapArea.Row := FocusPositionRow;
  MapArea.Col := FocusPositionCol;
end;

procedure TDelphiXMapEditForm.Fillall1Click(Sender: TObject);
var
  i, j: Integer;
begin
  if PicturesToChip.ItemIndex <> -1 then
  begin
    with DXBackgroundSprite do
      for i := 0 to MapArea.ColCount - 1 do
        for j := 0 to MapArea.RowCount - 1 do
          Chips[i, j] := PicturesToChip.ItemIndex;
    MapArea.Invalidate;
  end;
end;

procedure TDelphiXMapEditForm.Clear1Click(Sender: TObject);
var
  i, j: Integer;
  MapType: TMapType;
begin
  MapTypeDefaultValuesByObject(MapType);
  with DXBackgroundSprite do
    for i := 0 to MapArea.ColCount - 1 do
      for j := 0 to MapArea.RowCount - 1 do
      begin
        Chips[i, j] := 0;
        CollisionMap[i, j] := False;
        Map[i, j] := MapType;
      end;
  MapArea.Invalidate;
end;

procedure TDelphiXMapEditForm.PopupMenu2Popup(Sender: TObject);
var
  Shift: TShiftState;
begin
  Shift := [];
  MouseMove(Shift, maparea_X, maparea_Y);
  MapArea.MouseToCell(maparea_X, maparea_Y, FocusPopUpPositionCol, FocusPopUpPositionRow);
end;

procedure TDelphiXMapEditForm.PopupMenu1Popup(Sender: TObject);
begin
  Fillall1.Enabled := PicturesToChip.ItemIndex <> -1;
end;

procedure TDelphiXMapEditForm.ClearOneChip1Click(Sender: TObject);
var
  MapType: TMapType;
begin
  MapTypeDefaultValuesByObject(MapType);
  with DXBackgroundSprite do
  begin
    Map[MapArea.Col, MapArea.Row] := MapType;
  end;
  MapArea.Invalidate;
end;

procedure TDelphiXMapEditForm.MapAreaDblClick(Sender: TObject);
begin
  {Reset the flag}
  DelphiXMapEditPropertiesForm.Tag := 0;
  DelphiXMapEditPropertiesForm.Panel2.Color := {$IFDEF VER6UP}clSkyBlue{$ELSE}clBlue{$ENDIF};
  if not DelphiXMapEditPropertiesForm.Showing then
    DelphiXMapEditPropertiesForm.Show;
  Application.ProcessMessages;
  DelphiXMapEditPropertiesForm.Col := MapArea.Col;
  DelphiXMapEditPropertiesForm.Row := MapArea.Row;
  DelphiXMapEditPropertiesForm.LoadCellToForm(DXBackgroundSprite.Map[MapArea.Col, MapArea.Row], MapArea.Col, MapArea.Row);
end;

procedure TDelphiXMapEditForm.OKButtonClick(Sender: TObject);
begin
  if DelphiXMapEditPropertiesForm.Tag = 1 then
    DelphiXMapEditPropertiesForm.btnOK.Click;
end;

end.