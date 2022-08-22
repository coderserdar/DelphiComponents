
{*******************************************************}
{                                                       }
{       DragListBox Visual Component                    }
{       Copyright (C) 2000-02 Farsi Components          }
{                                                       }
{       TDragListBox                                    }
{       ver. 1.2 (build 4) (March 10, 2002)             }
{                                                       }
{       http://www.farsicomponents.com/                 }
{*******************************************************}


unit DragListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TDragListBox = class(TListBox)
  private
    FAutoDrag: Boolean;
    FCrossDrag: Boolean;
    FLastIndex: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure MoveUp; dynamic;
    procedure MoveDown; dynamic;
    procedure MoveTop; dynamic;
    procedure MoveBottom; dynamic;
    procedure MoveTo( DestList: TCustomListBox); dynamic;
    procedure MoveAll( DestList: TCustomListBox); dynamic;
  published
    property DragMode default dmAutomatic;
    property AutoDrag: Boolean read FAutoDrag write FAutoDrag default True;
    property CrossDrag: Boolean read FCrossDrag write FCrossDrag default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Farsi Components', [TDragListBox]);
end;

{ TDragListBox }

constructor TDragListBox.Create(AOwner: TComponent);
begin
  inherited;
  DragMode:= dmAutomatic;
  FLastIndex:= -1;
  FAutoDrag:= True;
  FCrossDrag:= True;
end;

procedure TDragListBox.DoEndDrag(Target: TObject; X, Y: Integer);
var SaveTopIndex: Integer;
begin
  inherited;
  if (Target=Self) then
  begin
    SaveTopIndex:= TopIndex;
    if (ItemIndex<SaveTopIndex) then
      Dec(SaveTopIndex);
    if FLastIndex>=Items.Count then
      Dec(FLastIndex);
    Items.Move(ItemIndex, FLastIndex);
    ItemIndex:= FLastIndex;
    TopIndex:= SaveTopIndex;
  end;
  FLastIndex:=-1;
end;

procedure TDragListBox.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
  FLastIndex:= ItemAtPos(ScreenToClient(Mouse.CursorPos), True);
  if FLastIndex=-1 then
    CancelDrag;
end;

procedure TDragListBox.DragCanceled;
var R: TRect;
begin
  inherited;
  R:= ItemRect(FLastIndex);
  InvalidateRect(Handle, @R, True);
  FLastIndex:=-1;
end;

procedure TDragListBox.DragDrop(Source: TObject; X, Y: Integer);
var P: Integer;
begin
  if (Source is TCustomListBox) and (Source<>Self) then
    with (Source as TCustomListbox) do
    begin
      P:= Self.ItemAtPos(Point(X, Y), False);
      Self.Items.InsertObject(P, Items[ItemIndex], Items.Objects[ItemIndex]);
      Items.Delete(ItemIndex);
      Self.ItemIndex:= P;
      FLastIndex:=-1;
    end;
  inherited;
end;

procedure TDragListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var NewIndex: Integer;
begin
  inherited;
  if not Assigned(OnDragOver) or Accept then
  begin
    if (FAutoDrag and (Source=Self) and (FLastIndex>=0)) or
      (FCrossDrag and (Source<>Self) and (Source is TCustomListBox) and
      ((Source as TCustomListbox).ItemIndex>=0)) then
    begin
      // auto scroll
      if (Y<10) then
      begin
        Canvas.DrawFocusRect(ItemRect(FLastIndex));
        TopIndex:= TopIndex-1;
        NewIndex:= TopIndex;
        FLastIndex:= NewIndex;
        Canvas.DrawFocusRect(ItemRect(NewIndex));
      end else
      if (Y>ClientHeight-10) then
      begin
        Canvas.DrawFocusRect(ItemRect(FLastIndex));
        TopIndex:= TopIndex+1;
        NewIndex:= FLastIndex+1;
        FLastIndex:= NewIndex;
        Canvas.DrawFocusRect(ItemRect(NewIndex));
       end;

      NewIndex:= ItemAtPos(Point(X, Y), False);
      if (NewIndex>=0) and (NewIndex<=Items.Count) then
      begin
        Accept:= True;
        if NewIndex<>FLastIndex then
        begin
          Canvas.DrawFocusRect(ItemRect(FLastIndex));
          Canvas.DrawFocusRect(ItemRect(NewIndex));
          FLastIndex:= NewIndex;
        end;
      end
      else
        Accept:= False;
    end
    else
      Accept:= False;
  end;
end;

procedure TDragListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (ssCTRL in Shift) and (Key in [VK_UP, VK_DOWN]) then
  begin
    case Key of
      VK_UP: MoveUp;
      VK_Down: MoveDown;
    end;
    Key:=0;
  end;
end;

procedure TDragListBox.MoveAll(DestList: TCustomListBox);
var FirstIndex: Integer;
begin
  if Items.Count>0 then
  begin
    FirstIndex:= DestList.Items.Count;
    DestList.Items.AddStrings(Items);
    Clear;
    DestList.ItemIndex:= FirstIndex;
  end;
end;

procedure TDragListBox.MoveDown;
begin
  if (ItemIndex<Items.Count-1) and (ItemIndex>=0) then
    Items.Exchange(ItemIndex, ItemIndex+1);
end;

procedure TDragListBox.MoveTo(DestList: TCustomListBox);
var SaveItemIndex: Integer;
begin
  if (ItemIndex>=0) and (ItemIndex<Items.Count) then
  begin
    DestList.Items.AddObject(Items[ItemIndex], Items.Objects[ItemIndex]);
    SaveItemIndex:= ItemIndex;
    Items.Delete(ItemIndex);
    DestList.ItemIndex:= DestList.Items.Count-1;
    if SaveItemIndex>Items.Count-1 then
      SaveItemIndex:= Items.Count-1;
    ItemIndex:= SaveItemIndex;
  end
  else
    if Items.Count>0 then
      ItemIndex:= 0;
end;

procedure TDragListBox.MoveTop;
begin
  if (ItemIndex>0) then
  begin
    Items.Move(ItemIndex, 0);
    ItemIndex:= 0;
  end;
end;

procedure TDragListBox.MoveBottom;
begin
  if (ItemIndex<Items.Count-1) and (ItemIndex>=0) then
  begin
    Items.Move(ItemIndex, Items.Count-1);
    ItemIndex:= Items.Count-1;
  end;
end;

procedure TDragListBox.MoveUp;
begin
  if (ItemIndex>0) then
    Items.Exchange(ItemIndex, ItemIndex-1);
end;

procedure TDragListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FLastIndex>=0 then
  begin
    Canvas.DrawFocusRect(ItemRect(FLastIndex));
    FLastIndex:= ItemIndex;
  end;
end;

end.
