{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmOutlookExtras
Purpose  : A simple implementation of the M$ Outlook style buttons components
Date     : 03-07-01
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.

           06-03-01: The TrmOutlookActionLink component was originally written
           by Piotr Jurago of Poland.  He sent the code back to me and I've
           included it here.
================================================================================}

unit rmOutLookExtras;

interface

{$I CompilerDefines.INC}

uses windows, messages, ActnList, controls, classes, rmSpeedBtns, rmOutlook, imglist;

type
  TrmOutLookButtonList = class;
  TrmOutlookListButton = class;

  TOLButtonStyle = (olbsLarge, olbsSmall, olbsCustom);
  TOLViewStyle = (vsSmallIcons, vsLargeIcons, vsCustomIcons);
  TOLButtonClickEvent = procedure(Sender : TObject; Button : TrmOutlookListButton) of Object;

  TrmOutlookListBaseButton = class(TrmSpeedButton)
  private
    FOutLookButtonList : TrmOutlookButtonList;
    FMouseInControl : Boolean;
    FCaption: String;
    FButtonStyle: TOLButtonStyle;
    FImageIndex: Integer;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure SetrmOutlookButtons(Value : TrmOutlookButtonList);
    procedure SetCaption(const Value: String);
    procedure SetButtonStyle(const Value: TOLButtonStyle);
    procedure SetImageIndex(const Value: Integer);
  protected
    { Protected Declarations }
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure EraseBK(var Message : TMessage); message WM_ERASEBKGND;
    procedure Paint; override;
  public
    { Public Declarations }
    constructor create(AOwner:TComponent); override;
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property OutLookButtonList : TrmOutlookButtonList read FOutLookButtonList write SeTrmOutlookButtons;
    property ButtonStyle : TOLButtonStyle read FButtonStyle write SetButtonStyle;
    property ButtonCaption : String read FCaption write SetCaption;
  end;

  TrmOutlookListButton = class(TCollectionItem)
  private
    FButton : TrmOutlookListBaseButton;
    FData: Pointer;
    procedure ClickButton(Sender : TObject);
    function GetImageIndex: Integer;
    function GetCaption: String;
    function GetAction: TBasicAction;
    procedure SetImageIndex(const Value: Integer);
    procedure SetCaption(const Value: String);
    procedure SetAction(const Value: TBasicAction);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetShowHint: boolean;
    procedure SetShowHint(const Value: boolean);
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;
    property Data : Pointer read FData write FData;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property ImageIndex : Integer read GetImageIndex write SetImageIndex;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property Hint:string read GetHint write SetHint;
    property Caption : String read GetCaption write SetCaption;
    property ShowHint : boolean read GetShowHint write SetShowHint;
  end;

  TrmOutlookButtons = class(TCollection)
  private
    FOLBtnList: TrmOutlookButtonList;
    function GetItem(Index: Integer): TrmOutlookListButton;
    procedure SetItem(Index: Integer; Value: TrmOutlookListButton);
  protected
    fupdating : integer;
    function GetOwner:TPersistent; override;
  public
    function Add: TrmOutlookListButton;
    constructor Create(AOwner: TrmOutlookButtonList);
    property Items[Index: Integer]: TrmOutlookListButton read GetItem write SetItem; default;
    property OLBtnList: TrmOutlookButtonList read FOLBtnList;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ifdef BD5}
    property Owner: TrmOutlookButtonList read FOLBtnList;
    {$endif}
  end;

  TrmOutlookButtonListPanel = class(TCustomControl)
  protected
    procedure Paint; override;
    procedure EraseBK(var Message : TMessage); message WM_ERASEBKGND;
  public
    constructor create(AOwner:TComponent); override;
  end;

  TrmOutlookButtonList = class(TCustomControl)
  private
    FButtons : TrmOutlookButtons;
    FUpButton : TrmTimerSpeedButton;
    FDownButton : TrmTimerSpeedButton;

    FBasePanel : TrmOutlookButtonListPanel;

    FLargeImages : TImageList;
    FSmallImages: TImageList;
    FImageChangeLink : TChangeLink;

    FViewStyle: TOLViewStyle;

    FTopButton: Integer;

    FOnClickButton: TOLButtonClickEvent;
    fCustomHeight: integer;
    procedure GoUp(Sender : TObject);
    procedure GoDown(Sender : TObject);
    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure SetLargeImages(Value : TImageList);
    procedure ImageListChange(Sender : TObject);
    procedure SetViewStyle(const Value: TOLViewStyle);
    procedure SetTopButton(const Value: Integer);
    procedure SetSmallImages(const Value: TImageList);
    procedure SetCustomHeight(const Value: integer);
  protected
    procedure Loaded; override;
    procedure WMSize(var Message : TMessage); message WM_SIZE;
    procedure RefreshButtons;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property PopUpMenu;
    property Buttons : TrmOutlookButtons read FButtons write FButtons;
    property LargeImages : TImageList read FLargeImages write SetLargeImages;
    property SmallImages : TImageList read FSmallImages write SetSmallImages;
    property ShowHint;
    property ViewStyle : TOLViewStyle read FViewStyle write SetViewStyle;
    property TopButton : Integer read FTopButton write SetTopButton;
    property CustomButtonHeight : integer read fCustomHeight write SetCustomHeight;
    property OnClickButton : TOLButtonClickEvent read FOnClickButton write FOnClickButton;
  end;

  TrmOutlookActionLink = class(TComponent)
  private
   { Private declarations }
     FViewStyle: TOLViewStyle;
     FActionList: TActionList;
     FOutlookControl: TrmOutlookControl;
     function BothAssigned: Boolean;
     procedure SetActionList(const Value: TActionList) ;
     procedure SetOutlookControl(const Value: TrmOutlookControl) ;
     procedure SetViewStyle(const Value: TOLViewStyle) ;
  protected
   { Protected }
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
   { Public declarations }
     procedure AddPagesFromActionCategorys;
     procedure AddButtonsFromActionsInCategory(Category: String) ;
  published
   { Published declarations }
     property ViewStyle: TOLViewStyle read FViewStyle write SetViewStyle;
     property ActionList: TActionList read FActionList write SetActionList;
     property OutlookControl: TrmOutlookControl read FOutlookControl write SetOutlookControl;
  end;

implementation

uses graphics, ExtCtrls, Dialogs, rmLibrary;

{ TrmOutlookButtonList }

constructor TrmOutlookButtonList.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  Color := clGray;
  ControlStyle := [csDoubleClicks];
  FBasePanel := TrmOutlookButtonListPanel.Create(Self);
  FBasePanel.Parent := Self;

  FUpButton := TrmTimerSpeedButton.Create(Self);
  FUpButton.Parent := Self;
  FUpButton.Width := 17;
  FUpButton.Height := 17;
  FUpButton.OnClick := GoUp;
  FUpButton.Font.Name := 'Marlett';
  FUpButton.Font.size := 10;
  FUpButton.Caption := '5';
  FUpButton.TimeBtnState := [tbAllowTimer];

  FDownButton := TrmTimerSpeedButton.Create(Self);
  FDownButton.Parent := Self;
  FDownButton.Width := 17;
  FDownButton.Height := 17;
  FDownButton.OnClick := GoDown;
  FDownButton.Font.Name := 'Marlett';
  FDownButton.Font.size := 10;
  FDownButton.Caption := '6';
  FDownButton.TimeBtnState := [tbAllowTimer];

  FButtons := TrmOutlookButtons.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  Height := 200;
  Width := 150;
end;

destructor TrmOutlookButtonList.Destroy;
begin
  FButtons.Free;
  FImageChangeLink.Free;
  FUpButton.Free;
  FDownButton.Free;
  inherited Destroy;
end;

procedure TrmOutlookButtonList.ImageListChange(Sender : TObject);
begin
   Invalidate;
end;

procedure TrmOutlookButtonList.Loaded;
begin
  inherited;
  RefreshButtons;
end;

procedure TrmOutlookButtonList.WMSize(var Message : TMessage);
begin
  inherited;
  RefreshButtons;
end;

procedure TrmOutlookButtonList.CMDialogKey(var Message : TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TrmOutlookButtonList.SetLargeImages(Value : TImageList);
begin
  if FLargeImages <> nil then
     FLargeImages.UnRegisterChanges(FImageChangeLink);
  FLargeImages := Value;
  if FLargeImages <> nil then
  begin
    FLargeImages.RegisterChanges(FImageChangeLink);
    FLargeImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TrmOutlookButtonList.SetSmallImages(const Value: TImageList);
begin
  if FSmallImages <> nil then
     FSmallImages.UnRegisterChanges(FImageChangeLink);
  FSmallImages := Value;
  if FSmallImages <> nil then
  begin
    FSmallImages.RegisterChanges(FImageChangeLink);
    FSmallImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TrmOutlookButtonList.RefreshButtons;
var
  loop : Integer;
  TotalHeight : Integer;
  wBtnHeight : integer;
  wTop : integer;
  wBtnStyle : TOLButtonStyle;

begin
  if (csLoading in ComponentState) or
     (csDestroying	in ComponentState) then
    Exit;

  if FButtons.fupdating = 0 then
  begin
     FUpButton.Visible := False;
     FUpButton.Top := 5;
     FUpButton.Left := Width - 16 - 5;

     FDownButton.Visible := False;
     FDownButton.Top := Height - 16 - 5;
     FDownButton.Left := Width - 16 - 5;

     FBasePanel.Left := 10;
     FBasePanel.Top := 10;
     FBasePanel.Width := ClientWidth - 36;
     FBasePanel.Height := ClientHeight - 20;

     case FViewStyle of
       vsLargeIcons : begin
                         wBtnHeight := 65;
                         wBtnStyle := olbsLarge;
                      end;
       vsCustomIcons: begin
                         wBtnHeight := fCustomHeight;
                         wBtnStyle := olbsCustom;
                      end;
     else
       wBtnHeight := 25;
       wBtnStyle := olbsSmall;
     end;

     TotalHeight := FButtons.Count * wBtnHeight;
     if TotalHeight > FBasePanel.Height then
        wTop := -(FTopButton * wBtnHeight)
     else
        wTop := 0;

     for loop := 0 to FButtons.Count - 1 do
     begin
       FButtons.Items[loop].FButton.ButtonStyle := wBtnStyle;

       FButtons.Items[loop].FButton.Width := FBasePanel.ClientWidth;
       FButtons.Items[loop].FButton.Height := wBtnHeight;
       FButtons.Items[loop].FButton.Top := wTop;
       FButtons.Items[loop].FButton.Left := 0;

       inc(wTop, wBtnHeight);
     end;

     FUpButton.Visible := (FTopButton > 0);
     FDownButton.Visible := (((FButtons.Count - FTopButton) * wBtnHeight) > FBasePanel.ClientHeight);

     Repaint;
  end;
end;

procedure TrmOutlookButtonList.SetViewStyle(const Value: TOLViewStyle);
begin
  FViewStyle := Value;
  RefreshButtons;
end;

procedure TrmOutlookButtonList.SetTopButton(const Value: Integer);
begin
  FTopButton := Value;
  RefreshButtons;
end;

procedure TrmOutlookButtonList.GoDown(Sender : TObject);
begin
  if FDownButton.visible then
  begin
     FTopButton := FTopButton + 1;
     RefreshButtons;
  end;
end;

procedure TrmOutlookButtonList.GoUp(Sender : TObject);
begin
  if FUpButton.visible then
  begin
     FTopButton := FTopButton - 1;
     RefreshButtons;
  end;
end;

procedure TrmOutlookButtonList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FLargeImages then
      fLargeImages := nil;

    if AComponent = FSmallImages then
      FSmallImages := nil;
  end;
end;

procedure TrmOutlookButtonList.SetCustomHeight(const Value: integer);
begin
  fCustomHeight := Value;
  RefreshButtons;
end;

{ TrmOutlookListBaseButton }

procedure TrmOutlookListBaseButton.SeTrmOutlookButtons(Value : TrmOutlookButtonList);
begin
  if FOutLookButtonList <> value then
     FOutLookButtonList := Value;
end;

procedure TrmOutlookListBaseButton.CMMouseEnter(var Message : TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
end;


procedure TrmOutLookListBaseButton.CMMouseLeave(var Message : TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;


procedure TrmOutLookListBaseButton.Paint;
var
  wButtonRect : TRect;
  wTextRect : TRect;
  wDrawFlags : integer;
  wTopColor, wBottomColor : TColor;
  wImageList : TImageList;
begin
  //paint the frame..
  if FOutLookButtonList <> nil then
    Canvas.Brush.Color := FOutLookButtonList.Color
  else
    Canvas.Brush.Color := clAppWorkspace;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  wImageList := nil;

  wDrawFlags := DT_SINGLELINE or DT_END_ELLIPSIS;

  case FButtonStyle of
    olbsCustom :
       begin
          wImageList := FOutLookButtonList.FLargeImages;
          wButtonRect := Rect(0, 0, Width, height);
          wTextRect := Rect(0, height-Canvas.textheight('X')-3 , Width, Height);
          wDrawFlags := wDrawFlags or DT_CENTER;
       end;
    olbsLarge :
       begin
          wImageList := FOutLookButtonList.FLargeImages;
          wButtonRect := Rect((Width div 2) - 18, 0, (Width div 2) + 18, 36);
          wTextRect := Rect(0, wButtonRect.Bottom + 3, Width, Height);
          wDrawFlags := wDrawFlags or DT_CENTER;
       end;
    olbsSmall :
       begin
          wImageList := FOutLookButtonList.FSmallImages;
          wButtonRect := Rect(0, 0, 20, 20);
          wTextRect := Rect(wButtonRect.Right + 4, 0, Width, wButtonRect.Bottom);
          wDrawFlags := wDrawFlags or DT_VCENTER;
       end;
  end;

  if FMouseInControl then
  begin
    if FState in [bsDown] then
    begin
      //the button is pushed...
      wTopColor := cl3dDkShadow;
      wBottomColor := cl3DLight;
    end
    else
    begin
      wTopColor := cl3DLight;
      wBottomColor := cl3dDkShadow;
    end;

    Frame3d(Canvas, wButtonRect, wTopColor, wBottomColor, 1);

    if (FState in [bsDown]) then
       OffsetRect(wButtonRect, 1, 1);
  end
  else
    InflateRect(wButtonRect, -1, -1);

  if (wImageList <> nil) and (FImageIndex > -1) then
  begin
     if FButtonStyle = olbsCustom then
     begin
        if assigned(parent) and (csdesigning in Parent.ComponentState) then
           wImageList.Draw(Canvas, (RectWidth(wButtonRect) div 2) - (wImageList.Width div 2), wButtonRect.Top + 1, FImageIndex)
        else
           wImageList.Draw(Canvas, (RectWidth(wButtonRect) div 2) - (wImageList.Width div 2), wButtonRect.Top + 1, FImageIndex, self.enabled)
     end
     else
     begin
        if assigned(parent) and (csdesigning in Parent.ComponentState) then
           wImageList.Draw(Canvas, wButtonRect.Left + 1, wButtonRect.Top + 1, FImageIndex)
        else
           wImageList.Draw(Canvas, wButtonRect.Left + 1, wButtonRect.Top + 1, FImageIndex, self.enabled)
     end;
  end;

  Canvas.Font.Color := clCaptionText;
  Canvas.Brush.Color := clAppWorkspace;
  DrawTextEx(Canvas.Handle, PCHar(FCaption), Length(FCaption), wTextRect, wDrawFlags, nil);
end;



procedure TrmOutLookListBaseButton.EraseBK(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmOutLookListBaseButton.SetCaption(const Value: String);
begin
  FCaption := Value;
  Invalidate;
end;

procedure TrmOutLookListBaseButton.SetButtonStyle(
  const Value: TOLButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

constructor TrmOutlookListBaseButton.create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDesignInteractive];
end;

procedure TrmOutlookListBaseButton.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  invalidate;
end;

procedure TrmOutlookListBaseButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.ButtonCaption = '') or (Self.ButtonCaption = Self.Name) then
        Self.ButtonCaption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
      Self.ImageIndex := ImageIndex;
    end;
end;

{ TrmOutLookButtons }

function TrmOutLookButtons.Add: TrmOutlookListButton;
begin
  Result := TrmOutlookListButton(inherited Add);
end;

procedure TrmOutlookButtons.BeginUpdate;
begin
  inherited;
  inc(fupdating);
end;

constructor TrmOutLookButtons.Create(AOwner: TrmOutlookButtonList);
begin
  inherited Create(TrmOutlookListButton);
  FOLBtnList := AOwner;
  fupdating := 0;
end;

procedure TrmOutlookButtons.EndUpdate;
begin
  inherited;
  if fupdating > 0 then
  begin
     dec(fupdating);
     FOLBtnList.RefreshButtons;
  end;
end;

function TrmOutLookButtons.GetItem(Index: Integer): TrmOutlookListButton;
begin
  Result := TrmOutlookListButton(inherited GetItem(Index));
end;

function TrmOutlookButtons.GetOwner: TPersistent;
begin
   result := FOLBtnList;
end;

procedure TrmOutLookButtons.SetItem(Index: Integer;
  Value: TrmOutlookListButton);
begin
  inherited SetItem(Index, Value);
end;

{ TrmOutlookListButton }

procedure TrmOutLookListButton.ClickButton(Sender: TObject);
begin
   if Assigned(TrmOutlookButtons(Collection).OLBtnList.FOnClickButton) then
     TrmOutlookButtons(Collection).OLBtnList.FOnClickButton(TrmOutlookButtons(Collection).OLBtnList, Self)
   else
   begin
     if assigned(Action) and assigned(Action.OnExecute) then
     Action.OnExecute(Sender);
   end;
end;

constructor TrmOutLookListButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FButton := TrmOutlookListBaseButton.Create(nil);
  FButton.OutLookButtonList := TrmOutlookButtons(Collection).OLBtnList;
  FButton.Parent := TrmOutlookButtons(Collection).OLBtnList.FBasePanel;
  FButton.OnClick := ClickButton;
  TrmOutlookButtons(Collection).OLBtnList.RefreshButtons;
end;

destructor TrmOutLookListButton.Destroy;
begin
  FButton.Free;
  inherited Destroy;
end;

function TrmOutlookListButton.GetAction: TBasicAction;
begin
   result := FButton.Action;
end;

function TrmOutlookListButton.GetCaption: String;
begin
   result := FButton.ButtonCaption;
end;

function TrmOutlookListButton.GetEnabled: boolean;
begin
   result := fButton.Enabled;  
end;

function TrmOutlookListButton.GetHint: string;
begin
   result := FButton.Hint;  
end;

function TrmOutlookListButton.GetImageIndex: Integer;
begin
   result := FButton.ImageIndex;  
end;

function TrmOutlookListButton.GetShowHint: boolean;
begin
   result := FButton.ShowHint;
end;

procedure TrmOutlookListButton.SetAction(const Value: TBasicAction);
begin
  FButton.Action := Value;
end;

procedure TrmOutLookListButton.SetCaption(const Value: String);
begin
  FButton.ButtonCaption := Value;
end;

procedure TrmOutlookListButton.SetEnabled(const Value: boolean);
begin
   fbutton.enabled := value;
end;

procedure TrmOutlookListButton.SetHint(const Value: string);
begin
   fButton.Hint := value;
end;

procedure TrmOutLookListButton.SetImageIndex(const Value: Integer);
begin
  FButton.ImageIndex := Value;
end;

procedure TrmOutlookListButton.SetShowHint(const Value: boolean);
begin
   FButton.ShowHint := value;
end;

{ TrmOutlookButtonListPanel }

constructor TrmOutlookButtonListPanel.create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDesignInteractive];
end;

procedure TrmOutLookButtonListPanel.EraseBK(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmOutLookButtonListPanel.Paint;
begin
  with Canvas do
  begin
    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(ClientRect);
  end;
  inherited;
end;

{ TrmOutlookActionLink }

procedure TrmOutlookActionLink.AddButtonsFromActionsInCategory(Category: String) ;
var
   i: integer;
   InCategory: integer;
   AOutLookButtonList: TrmOutLookButtonList;
   AOutLookListButton: TrmOutLookListButton;
begin
   if not BothAssigned then
      exit;

   InCategory := -1;

   for i := 0 to FOutlookControl.PageCount - 1 do
   begin
      if FOutlookControl.Pages[i].Caption = Category then
         InCategory := i;
   end;

   if Incategory <> -1 then
   begin
      AOutLookButtonList := TrmOutLookButtonList.Create(Owner) ;
      AOutLookButtonList.Parent := FOutlookControl.Pages[inCategory];
      AOutLookButtonList.Align := AlClient;
      AOutLookButtonList.ViewStyle := FViewStyle;

      if FViewStyle = vsSmallIcons then
         AOutLookButtonList.SmallImages := TImageList(FActionList.Images)
      else
         AOutLookButtonList.LargeImages := TImageList(FActionList.Images) ;

      for i := 0 to FActionList.ActionCount - 1 do
      begin
         if FActionList.Actions[i].Category = Category then
         begin
            AOutLookListButton := TrmOutLookListButton.Create(AOutLookButtonList.Buttons) ;
            AOutLookListButton.Action := TAction(FActionList.Actions[i]) ;
         end;
      end;
   end;
end;

procedure TrmOutlookActionLink.AddPagesFromActionCategorys;
var
   i: integer;
   UniqeCategorys: TStringList;
   Page: TrmOutlookPage;
begin
   if not BothAssigned then
      exit;

   Page := nil;

   UniqeCategorys := TStringList.Create;
   try
      UniqeCategorys.Sorted := true;
      UniqeCategorys.Duplicates := dupIgnore;

      for i := 0 to ActionList.ActionCount - 1 do
         UniqeCategorys.add(ActionList.Actions[i].Category) ;

      For i := 0 to UniqeCategorys.Count - 1 do
      begin
         Page := TrmOutlookPage.Create(Owner) ;
         Page.Caption := UniqeCategorys.Strings[i];
         Page.OutlookControl := FOutlookControl;
         AddButtonsFromActionsInCategory(UniqeCategorys.Strings[i]) ;
      end;

      if assigned(page) then
         FOutlookControl.ActivePage := Page;

      FOutlookControl.Repaint;
   finally
      UniqeCategorys.Free;
   end;
end;

function TrmOutlookActionLink.BothAssigned: Boolean;
begin
   result := ((FActionList <> nil) and (FOutlookControl <> nil));
   if not result then
   begin
      if FActionList = nil then
         MessageDlg('An ActionList must be assigned', mtInformation, [mbok], 0);

      if FOutlookControl = nil then
         MessageDlg('An rmOutlookControl must be assigned', mtInformation, [mbok], 0);
   end;
end;

procedure TrmOutlookActionLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
     if (AComponent is TrmOutlookControl) and (AComponent = FOutlookControl) then
        FOutlookControl := nil;

     if (AComponent is TActionList) and (AComponent = FActionList) then
        FActionList := nil;
  end;
end;

procedure TrmOutlookActionLink.SetActionList(const Value: TActionList) ;
begin
   FActionList := Value;
   if FActionList <> nil then
      FActionList.FreeNotification(self);
end;

procedure TrmOutlookActionLink.SetOutlookControl(const Value: TrmOutlookControl) ;
begin
   FOutlookControl := Value;
   if FOutLookControl <> nil then
      FOutlookControl.FreeNotification(self);
end;

procedure TrmOutlookActionLink.SetViewStyle(const Value: TOLViewStyle) ;
begin
   FViewStyle := Value;
end;

end.



