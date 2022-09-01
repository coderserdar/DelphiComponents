unit VQBField;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, VQBConst;

type

  //CheckEvent
  TCheckEvent = Procedure(Sender: TObject; const Item: Integer; Checked: boolean) of object;

  THintWnd = class(THintWindow)
  Protected
    procedure NCPaint(DC: HDC); override;
  end;

  TCheckListBoxEx = class(TCheckListBox)
  Private
    FSize: Boolean;
    HintWnd: THintWnd;
    FItem: Integer;
    FTableName: string;
    FTableAlias: String;
    FBoldArr: array of boolean;
    FTimer: TTimer;
    FChangeState: TNotifyEvent;
    FClose: TNotifyEvent;
    FCheck: TCheckEvent;
    FTypeField: TStringList;
    FNameField: TStringList;
    function GetBoldItem(Index: Integer): Boolean;
    procedure SetBoldItem(Index: Integer; const Value: Boolean);
    Procedure InitHint(HintIndex: Integer);
    Procedure RemoveHint;
    Procedure TimerProc(Sender: TObject);
    procedure SetTableAlias(const Value: String);
    procedure SetTableName(const Value: string);
    function GetTitleHeight: Integer;
    procedure SetNameField(const Value: TStringList);
    procedure SetTypeField(const Value: TStringList);
  Protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    Procedure HintProc;
    Procedure ClickCheck; override;
    Procedure DoDrawItem(Control: TWinControl; Index: Integer;
                                  Rect: TRect; State: TOwnerDrawState);
  Public
    UseAlias: Boolean;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function AddStr(const S: String): Integer;
    Property BoldItem[Index: Integer]: Boolean read GetBoldItem write SetBoldItem;
    Property TableName: string read FTableName write SetTableName;
    Property TableAlias: String read FTableAlias write SetTableAlias;
    Property TitleHeight: Integer read GetTitleHeight;
    Property OnChangeState: TNotifyEvent read FChangeState write FChangeState;
    Property NameField: TStringList read FNameField write SetNameField;
    Property TypeField: TStringList read FTypeField write SetTypeField;
    Property OnClose: TNotifyEvent read FClose write FClose;
    Property OnCheck: TCheckEvent read FCheck write FCheck;
    Property Caption;
  end;

Const
    HintHidePause = 2500;

implementation

{ THintWnd }

procedure THintWnd.NCPaint(DC: HDC);
var
  R: TRect;
begin
   R := Rect(0, 0, Width, Height);
   Windows.DrawEdge(DC, R, BDR_RAISEDOUTER , BF_RECT or BF_MONO);
end;

{ TCheckListBoxEx }

function TCheckListBoxEx.GetBoldItem(Index: Integer): Boolean;
begin
   Result:= False;
   if (Index >= Low(FBoldArr)) and(Index <= High(FBoldArr)) then
   Result:= FBoldArr[Index];
end;

procedure TCheckListBoxEx.SetBoldItem(Index: Integer;
  const Value: Boolean);
begin
   if (Index >= Low(FBoldArr)) and(Index <= High(FBoldArr)) and
      (FBoldArr[Index] <> Value) then
      begin
         FBoldArr[Index]:= Value;
         Invalidate;
      end;
end;

Procedure TCheckListBoxEx.InitHint(HintIndex: Integer);
var
  ShortHint: String;
  HintPos: TPoint;
  HintBox, IRect: TRect;
begin
   HintWnd := THintWnd.Create(Self);
   With HintWnd do
   begin
      Color:= clInfoBk;
      Canvas.Font.Size:= Self.Canvas.Font.Size;
      Canvas.Font.Name:= Self.Canvas.Font.Name;
      ShortHint := Items[HintIndex];
      IRect:= ItemRect(HintIndex);

      HintPos.X:= IRect.Left;
      HintPos.Y:= IRect.Top;
      Windows.ClientToScreen(Self.Handle, HintPos);
      HintBox := Bounds(0, 0, HintWnd.Canvas.TextWidth(ShortHint), ItemHeight);

      Canvas.TextOut(0,0, ShortHint);

      Inc(HintBox.Right, 6);
      Inc(HintBox.Bottom, 2);
      inc(HintPos.X, 2); //Borland Fix
      OffsetRect(HintBox, HintPos.X + GetCheckWidth - 3, HintPos.Y - (HintBox.Bottom - ItemHeight) - 1);
      ActivateHint(HintBox, ShortHint);
   end;
end;

procedure TCheckListBoxEx.RemoveHint;
begin
   if Assigned(HintWnd) then
   begin
      HintWnd.ReleaseHandle;
      HintWnd.Free;
      HintWnd:= nil;
   end;
end;

procedure TCheckListBoxEx.TimerProc(Sender: TObject);
begin
   FTimer.Enabled:= False;
   RemoveHint;
end;

procedure TCheckListBoxEx.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
     Style:= Style or WS_CAPTION or WS_SIZEBOX or WS_SYSMENU xor WS_MAXIMIZEBOX;
     WindowClass.hIcon:= LoadIcon(hInstance, 'TABLEICON');

     // fix scroll when click on partially visible item
     WindowClass.style:= WindowClass.style xor CS_VREDRAW;
  end;
  Style:= lbOwnerDrawFixed;
  DragMode:= dmAutomatic;
  ItemHeight:= 13;
  IntegralHeight:= True;
  ShowHint:= False;
end;

procedure TCheckListBoxEx.WndProc(var Message: TMessage);
begin
   case Message.Msg of
     CN_DRAWITEM:
       begin
          with TWMDrawItem(Message).DrawItemStruct^ do
            if (ItemAction = ODA_DRAWENTIRE) and Assigned(OnChangeState) and not FSize
                then FChangeState(Self);
       end;
     WM_SIZE:
       begin
          with TWMSize(Message) do
            if (Width > 0) or (Height > 0) then Invalidate;
          RemoveHint;
          FSize:= True;
          if Assigned(OnChangeState) then FChangeState(Self);
       end;
     WM_MOVE:
       begin
          SendMessage(Handle, WM_SIZE, SIZE_RESTORED, 0);
          RemoveHint;
       end;
     CM_MOUSELEAVE:
       begin
          RemoveHint;
          FItem:= -1;
       end;
     WM_EXITSIZEMOVE, WM_SETFOCUS, CM_MOUSEENTER: FSize:= False;
     WM_MOUSEWHEEL, WM_MOUSEMOVE: HintProc;
     WM_CLOSE:
       begin
          if Assigned(OnClose) then FClose(Self);
          exit;
       end;
     WM_VSCROLL: RemoveHint;

   end;
   inherited WndProc(Message);
end;

procedure TCheckListBoxEx.HintProc;
var
  Item: Integer;
  CursorPos: TPoint;
  TempDC: TImage;
  R: TRect;
begin
  CursorPos:= Mouse.CursorPos;
  CursorPos:= Self.ScreenToClient(CursorPos);
  Item:= ItemAtPos(CursorPos, True);
  if (Item < 0) or (Item > Items.Count - 1) or(item = FItem) then exit;

  RemoveHint;
  FItem:= - 1;
  TempDC:= TImage.Create(nil);
  TempDC.Canvas.Font.Size:= Self.Canvas.Font.Size;
  if BoldItem[Item] then TempDC.Canvas.Font.Style:= [fsBold] else
     TempDC.Canvas.Font.Style:= [];
  R:= ItemRect(Item);
  Dec(R.Right, 2);
  if (TempDC.Canvas.TextWidth(Items[Item]) > R.Right - GetCheckWidth) then
  begin
     InitHint(Item);
     FItem:= Item;
     FTimer.Enabled:= False;
     FTimer.Enabled:= True;
  end;
  FreeAndNil(TempDC);
end;

procedure TCheckListBoxEx.DoDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
   if FBoldArr[Index] = True then Canvas.Font.Style:= [fsBold]
   else Canvas.Font.Style:= [];
   if (odSelected in State) then Canvas.Brush.Color:= clTeal;
   Canvas.FillRect(Rect);
   inc(Rect.Left, 2); //Borland Fix

   DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, DT_LEFT or DT_END_ELLIPSIS);
end;

constructor TCheckListBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  FTypeField:= TStringList.Create;
  FNameField:= TStringList.Create;
  Visible:= False;
  Width:= 153;
  Height:= 160;
  FItem:= - 1;
  OnDrawItem:= DoDrawItem;
  DragCursor:= crCUR_LINK; //Cur_Link;
  FTimer:= TTimer.Create(Self);
  FTimer.Interval:= HintHidePause;
  FTimer.Enabled:= False;
  FTimer.OnTimer:= TimerProc;

end;

destructor TCheckListBoxEx.Destroy;
begin
  FTimer.Free;
  RemoveHint;
  FTypeField.Free;
  FNameField.Free;
  //Beep; //Debug
  inherited;
end;

function TCheckListBoxEx.AddStr(const S: String): Integer;
begin
   Result:= inherited Items.Add(S);
   SetLength(FBoldArr, items.Count);
end;

procedure TCheckListBoxEx.SetTableAlias(const Value: String);
begin
  FTableAlias := Value;
  UseAlias:= Value <> FTableName;
  Caption:= TableName;
  if TableName <> Value then Caption:= Caption + '(' + Value + ')';
end;

procedure TCheckListBoxEx.SetTableName(const Value: string);
begin
   FTableName:= Value;
   TableAlias:= Value;
end;

function TCheckListBoxEx.GetTitleHeight: Integer;
begin
   Result:= Height - ClientHeight;
end;

procedure TCheckListBoxEx.ClickCheck;
var
  i: integer;
begin
  inherited;
  for i:= 0 to Items.Count - 1 do
  begin
     if Selected[i] then
     begin
        if Assigned(OnCheck) then FCheck(Self, i, Checked[i]);
        exit;
     end;
  end;
end;

procedure TCheckListBoxEx.SetNameField(const Value: TStringList);
begin
  FNameField := Value;
end;

procedure TCheckListBoxEx.SetTypeField(const Value: TStringList);
begin
  FTypeField := Value;
end;

end.
