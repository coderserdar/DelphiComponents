{*******************************************************}
{File:      NCDblLst.PAS                                }
{Revision:  2.07 / 06.02.2000                           }
{Comment:   Double list box                             }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCDblLst;

interface

uses Messages, Windows, Classes, Graphics, Forms, Controls, Buttons,
     StdCtrls, ExtCtrls;

Type
    TNCDblListBox = class(TCustomPanel)
    private
        procedure IncludeBtnClick(Sender: TObject);
        procedure ExcludeBtnClick(Sender: TObject);
        procedure IncAllBtnClick(Sender: TObject);
        procedure ExcAllBtnClick(Sender: TObject);
        procedure MoveSelected(List: TCustomListBox; Items: TStrings);
        procedure SetItem(List: TListBox; Index: Integer);
        function GetFirstSelection(List: TCustomListBox): Integer;
        procedure ListDragOver(Sender, Source: TObject; X, Y: Integer;
          State: TDragState; var Accept: Boolean);
        procedure ListDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure SetS(AIndex: Integer; const AValue: String);
        function GetS(AIndex: Integer): String;
        procedure SetItems(AIndex: Integer; AValues: TStrings);
        function GetItems(AIndex: Integer): TStrings;
        function GetEvent(AIndex: Integer): TNotifyEvent;
        procedure SetEvent(AIndex: Integer; AValue: TNotifyEvent);
        procedure SetI(AIndex: Integer; AValue: Integer);
        function GetI(AIndex: Integer): Integer;
        procedure SetB(AIndex: Integer; AValue: Boolean);
        function GetB(AIndex: Integer): Boolean;
        procedure ListKeys(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure ListClick(Sender: TObject);
    protected
        procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    public
        DstLabel: TLabel;
        SrcLabel: TLabel;
        SrcList: TListBox;
        IncludeBtn: TSpeedButton;
        IncAllBtn: TSpeedButton;
        ExcludeBtn: TSpeedButton;
        ExAllBtn: TSpeedButton;
        DstList: TListBox;
        constructor Create(AOwner: TComponent); override;
        procedure SetButtons;
        property SrcItemIndex: Integer index 0 read GetI write SetI;
        property DstItemIndex: Integer index 1 read GetI write SetI;
    published
        property DestCaption: String index 0 read GetS write SetS;
        property SrcCaption: String index 1 read GetS write SetS;
        property DestListHint: String index 2 read GetS write SetS;
        property SrcListHint: String index 3 read GetS write SetS;
        property IncHint: String index 4 read GetS write SetS;
        property IncAllHint: String index 5 read GetS write SetS;
        property ExHint: String index 6 read GetS write SetS;
        property ExAllHint: String index 7 read GetS write SetS;
        property SrcItems: TStrings index 0 read GetItems write SetItems;
        property DstItems: TStrings index 1 read GetItems write SetItems;
        property SrcItemsSorted: Boolean index 0 read GetB write SetB
            default True;
        property OnSrcClick: TNotifyEvent index 0 read GetEvent write SetEvent;
        property OnDstClick: TNotifyEvent index 1 read GetEvent write SetEvent;
        property Align;
        property BevelInner;
        property BevelOuter;
        property BevelWidth;
        property BorderWidth;
        property BorderStyle;
        property DragCursor;
        property Enabled;
        property Color;
        property Ctl3D;
        property Font;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Visible;
        property OnEnter;
        property OnExit;
        property OnResize;
    end;

implementation

Uses NCStrs;

constructor TNCDblListBox.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Top := 0;
    Left := 0;
    Width := 422;
    Height := 270;
    BevelOuter := bvNone;
    ControlStyle := ControlStyle - [csSetCaption];
    Caption := '';
    DstLabel := TLabel.Create(Self);
    with DstLabel do begin
        Width := 145;
        Height := 16;
        AutoSize := False;
        Caption := SSortOrder;
        Parent := Self;
    end;
    SrcLabel := TLabel.Create(Self);
    with SrcLabel do begin
        Width := 145;
        Height := 16;
        AutoSize := False;
        Caption := SSourceFields;
        Parent := Self;
    end;
    SrcList := TListBox.Create(Self);
    with SrcList do begin
        Hint := SSourceFieldsList;
        DragMode := dmAutomatic;
        MultiSelect := True;
        Sorted := True;
        TabOrder := 0;
        OnDragDrop := ListDragDrop;
        OnDragOver := ListDragOver;
        OnKeyDown := ListKeys;
        OnClick := ListClick;
        Parent := Self;
    end;
    IncludeBtn := TSpeedButton.Create(Self);
    with IncludeBtn do begin
        Width := 24;
        Height := 24;
        Hint := SIncludeField;
        Caption := '>';
        Font.Style := [fsBold];
        OnClick := IncludeBtnClick;
        Parent := Self;
    end;
    IncAllBtn := TSpeedButton.Create(Self);
    with IncAllBtn do begin
        Width := 24;
        Height := 24;
        Hint := SIncludeAllFields;
        Caption := '>>';
        Font.Style := [fsBold];
        OnClick := IncAllBtnClick;
        Parent := Self;
    end;
    ExcludeBtn := TSpeedButton.Create(Self);
    with ExcludeBtn do begin
        Width := 24;
        Height := 24;
        Hint := SExcludeField;
        Caption := '<';
        Font.Style := [fsBold];
        Enabled := False;
        OnClick := ExcludeBtnClick;
        Parent := Self;
    end;
    ExAllBtn := TSpeedButton.Create(Self);
    with ExAllBtn do begin
        Width := 24;
        Height := 24;
        Hint := SExcludeAllFields;
        Caption := '<<';
        Font.Style := [fsBold];
        Enabled := False;
        OnClick := ExcAllBtnClick;
        Parent := Self;
    end;
    DstList := TListBox.Create(Self);
    with DstList do begin
        Hint := SDestFieldsList;
        DragMode := dmAutomatic;
        MultiSelect := True;
        TabOrder := 1;
        OnDragDrop := ListDragDrop;
        OnDragOver := ListDragOver;
        OnKeyDown := ListKeys;
        OnClick := ListClick;
        Parent := Self;
    end;
end;

procedure TNCDblListBox.AlignControls(AControl: TControl; var Rect: TRect);
var
    BevelSize: Integer;
begin
    BevelSize := BorderWidth;
    if BevelOuter <> bvNone then
        Inc(BevelSize, BevelWidth);
    if BevelInner <> bvNone then
        Inc(BevelSize, BevelWidth);
    InflateRect(Rect, -BevelSize, -BevelSize);
    with SrcLabel do begin
        Left := Rect.Left;
        Top := Rect.Top;
        Width := ((Rect.Right - Rect.Left + 1) - 24 - 10 * 2) div 2;
    end;
    with SrcList do begin
        Left := SrcLabel.Left;
        Top := (SrcLabel.Top + SrcLabel.Height - 1) + 5;
        Height := Rect.Bottom - Top;
        Width := SrcLabel.Width;
    end;
    with IncludeBtn do begin
        Left := (SrcList.Left + SrcList.Width - 1) + 10;
        Top := SrcList.Top;
    end;
    with IncAllBtn do begin
        Left := IncludeBtn.Left;
        Top := (IncludeBtn.Top + IncludeBtn.Height - 1) + 11;
    end;
    with ExcludeBtn do begin
        Left := IncludeBtn.Left;
        Top := (IncAllBtn.Top + IncAllBtn.Height - 1) + 11;
    end;
    with ExAllBtn do begin
        Left := IncludeBtn.Left;
        Top := (ExcludeBtn.Top + ExcludeBtn.Height - 1) + 11;
    end;
    with DstLabel do begin
        Left := (IncludeBtn.Left + IncludeBtn.Width - 1) + 10;
        Top := SrcLabel.Top;
        Width := SrcList.Width;
    end;
    with DstList do begin
        Left := DstLabel.Left;
        Top := SrcList.Top;
        Height := SrcList.Height;
        Width := SrcList.Width;
    end;
    SetButtons;
end;

procedure TNCDblListBox.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TNCDblListBox.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TNCDblListBox.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I], SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
  DstList.SetFocus;
end;

procedure TNCDblListBox.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
  SrcList.SetFocus;
end;

procedure TNCDblListBox.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TNCDblListBox.ListKeys(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_INSERT then begin
        IncludeBtnClick(Sender);
        Key := 0;
    end
    else if Key = VK_DELETE then begin
        ExcludeBtnClick(Sender);
        Key := 0;
    end
end;

procedure TNCDblListBox.ListClick(Sender: TObject);
begin
    SetButtons;
end;

procedure TNCDblListBox.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TNCDblListBox.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TNCDblListBox.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

procedure TNCDblListBox.ListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source is TListBox) and ((Sender <> Source) or
        (TListBox(Sender) = DstList));
end;

procedure TNCDblListBox.ListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
    j, i: Integer;

    procedure ExchangeItem(i, j: Integer);
    var
        s: String;
        o: TObject;
    begin
        with TListBox(Source) do begin
            s := Items[i];
            o := Items.Objects[i];
            Items.Delete(i);
        end;
        with TListBox(Sender) do begin
            if j > Items.Count then
                j := Items.Count;
            Items.InsertObject(j, s, o);
        end;
    end;

begin
    j := TListBox(Sender).ItemAtPos(Point(x, y), False);
    for i := TListBox(Source).Items.Count - 1 downto 0 do
        if TListBox(Source).Selected[i] then
            ExchangeItem(i, j);
    if SrcList.Sorted then
        try
            SrcList.Sorted := False;
        finally
            SrcList.Sorted := True;
        end;
    SetButtons;
end;

procedure TNCDblListBox.SetS(AIndex: Integer; const AValue: String);
begin
    if GetS(AIndex) <> AValue then begin
        case AIndex of
        0: DstLabel.Caption := AValue;
        1: SrcLabel.Caption := AValue;
        2: DstList.Hint := AValue;
        3: SrcList.Hint := AValue;
        4: IncludeBtn.Hint := AValue;
        5: IncAllBtn.Hint := AValue;
        6: ExcludeBtn.Hint := AValue;
        7: ExAllBtn.Hint := AValue;
        end;
    end;
end;

function TNCDblListBox.GetS(AIndex: Integer): String;
begin
    case AIndex of
    0: Result := DstLabel.Caption;
    1: Result := SrcLabel.Caption;
    2: Result := DstList.Hint;
    3: Result := SrcList.Hint;
    4: Result := IncludeBtn.Hint;
    5: Result := IncAllBtn.Hint;
    6: Result := ExcludeBtn.Hint;
    7: Result := ExAllBtn.Hint;
    end;
end;

procedure TNCDblListBox.SetItems(AIndex: Integer; AValues: TStrings);
begin
    case AIndex of
    0: SrcList.Items.Assign(AValues);
    1: DstList.Items.Assign(AValues);
    end;
end;

{$WARNINGS OFF}
function TNCDblListBox.GetItems(AIndex: Integer): TStrings;
begin
    case AIndex of
    0: Result := SrcList.Items;
    1: Result := DstList.Items;
    end;
end;

function TNCDblListBox.GetEvent(AIndex: Integer): TNotifyEvent;
begin
    case AIndex of
    0: Result := SrcList.OnClick;
    1: Result := DstList.OnClick;
    end;
end;
{$WARNINGS ON}

procedure TNCDblListBox.SetEvent(AIndex: Integer; AValue: TNotifyEvent);
begin
    case AIndex of
    0: SrcList.OnClick := AValue;
    1: DstList.OnClick := AValue;
    end;
end;

procedure TNCDblListBox.SetI(AIndex: Integer; AValue: Integer);
begin
    case AIndex of
    0: SrcList.ItemIndex := AValue;
    1: DstList.ItemIndex := AValue;
    end;
end;

{$WARNINGS OFF}
function TNCDblListBox.GetI(AIndex: Integer): Integer;
begin
    case AIndex of
    0: Result := SrcList.ItemIndex;
    1: Result := DstList.ItemIndex;
    end;
end;
{$WARNINGS ON}

procedure TNCDblListBox.SetB(AIndex: Integer; AValue: Boolean);
begin
    case AIndex of
    0: SrcList.Sorted := AValue;
    end;
end;

{$WARNINGS OFF}
function TNCDblListBox.GetB(AIndex: Integer): Boolean;
begin
    case AIndex of
    0: Result := SrcList.Sorted;
    end;
end;
{$WARNINGS ON}

end.
