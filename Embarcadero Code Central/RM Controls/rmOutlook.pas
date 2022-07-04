{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmOutlook
Purpose  : A simple implementation of the M$ Outlook style control
Date     : 03-06-01
Author   : Ryan J. Mills
Version  : 1.80
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmOutlook;

interface

{$I CompilerDefines.INC}

uses Windows, Messages, Forms, Classes, Controls, Graphics, ImgList;

type
  TrmOutlookControl = class;
  TrmOutlookPage = class;

  TrmOutlookPageEvent = procedure(ASheet : TrmOutlookPage) of object;
  TrmOutlookQueryPageEvent = procedure(ASheet : TrmOutlookPage; var CanClose : Boolean) of object;

  TrmDrawingStyle = (ds3D, dsFlat, dsNone);

  TrmOutlookPage = class(TCustomControl)
  private
    FOutlookControl : TrmOutlookControl;
    FImageIndex : Integer;

    FAlignment: TAlignment;

    FCloseButtonDown : Boolean;
    FCloseMouseOver : Boolean;
    FCloseButton: Boolean;

    fMouseOverBtn: boolean;

    FOnQueryClosePage: TrmOutlookQueryPageEvent;
    FOnDestroy : TrmOutlookPageEvent;

    procedure SetImageIndex(Value : Integer);
    function GetPageIndex : Integer;

    procedure SetOutlookControl(AOutlookControl : TrmOutlookControl);
    procedure SetPageIndex(Value : Integer);
    procedure SetAlignment(const Value: TAlignment);

    procedure SetCloseButton(const Value: Boolean);

    procedure UpdatePage;
    function BtnRect:TRect;
    function CloseBtnRect:TRect;
  protected
    function GetClientRect: TRect; override;

    procedure CreateParams(var Params : TCreateParams); override;
    procedure ReadState(Reader : TReader); override;

    procedure WMSize(var Message : TMessage); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure PaintButton;
    procedure Paint; override;

    property OutlookControl : TrmOutlookControl read FOutlookControl write SetOutlookControl;
  published
    property Color default clAppWorkSpace;
    property Caption;
    property Font;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property ParentFont;
    property ParentColor;

    property Alignment : TAlignment read FAlignment write SetAlignment default taCenter;
    property CloseButton : Boolean read FCloseButton write SetCloseButton;
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property PageIndex : Integer read GetPageIndex write SetPageIndex stored False;

    property OnEnter;
    property OnExit;
    property OnResize;
    property OnDestroy : TrmOutlookPageEvent read FOnDestroy write FOnDestroy;
    property OnQueryClosePage : TrmOutlookQueryPageEvent read FOnQueryClosePage write FOnQueryClosePage;
  end;

  TrmOutlookControl = class(TCustomControl)
  private
    FPages : TList;
    FImages : TCustomImageList;
    FActivePage : TrmOutlookPage;
    FImageChangeLink : TChangeLink;

    FButtonHeight : Integer;
    fDrawingStyle: TrmDrawingStyle;

    FPageChanged : TNotifyEvent;

    procedure AdjustPages;

    function GetPage(Index : Integer) : TrmOutlookPage;
    function GetPageCount : Integer;

    procedure InsertPage(Page : TrmOutlookPage);
    procedure RemovePage(Page : TrmOutlookPage);
    procedure SetActivePage(Page : TrmOutlookPage);

    procedure SetImages(Value : TCustomImageList);
    procedure ImageListChange(Sender : TObject);

    procedure SetButtonHeight(value : integer);
    procedure SetDrawingStyle(const Value: TrmDrawingStyle);

    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
    procedure SetChildOrder(Child : TComponent; Order : Integer); override;
    procedure ShowControl(AControl : TControl); override;

    procedure Loaded; override;

    procedure WMSize(var Message : TMessage); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetBorderWidth:integer;
    function GetClientRect: TRect; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    function FindNextPage(CurPage : TrmOutlookPage; GoForward : Boolean) : TrmOutlookPage;
    procedure SelectNextPage(GoForward : Boolean);

    property PageCount : Integer read GetPageCount;
    property Pages[Index : Integer] : TrmOutlookPage read GetPage;
  published
    property Align;
    property Color default clAppWorkspace;
    property Font;
    property Images : TCustomImageList read FImages write SetImages;
    property ActivePage : TrmOutlookPage read FActivePage write SetActivePage;
    property ButtonHeight : Integer read fButtonHeight write SetButtonHeight default 18;
    property DrawingStyle : TrmDrawingStyle read fDrawingStyle write SetDrawingStyle default ds3D;
    property OnPageChanged : TNotifyEvent read fPageChanged write fPageChanged;
    property OnResize;
  end;


implementation

uses extCtrls;

const
  SOutlookIndexError = 'Sheet index Error';

{ TrmOutlookPage }

constructor TrmOutlookPage.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csAcceptsControls] - [csDesignInteractive];

  Visible := True;
  Caption := '';
  FImageIndex := -1;
  color := clAppWorkSpace;

  FAlignment := taCenter;
end;

function TrmOutlookPage.GetClientRect : TRect;
begin
  Result := inherited GetClientRect;
  if Assigned(FOutlookControl) then
    Result.Top := Result.Top + FOutlookControl.FButtonHeight;
end;

procedure TrmOutLookPage.UpdatePage;
var
  loop : Integer;
begin
  if Assigned(FOutlookControl) then
  begin
    for loop := 0 to ControlCount - 1 do
        Controls[loop].Visible := (Self = FOutlookControl.FActivePage);
  end;
  Realign;
end;

procedure TrmOutlookPage.SetImageIndex(Value : Integer);
begin
  FImageIndex := Value;
  PaintButton;
end;

destructor TrmOutlookPage.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);

  inherited Destroy;
end;

function TrmOutlookPage.GetPageIndex : Integer;
begin
  if FOutlookControl <> nil then
    Result := FOutlookControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TrmOutlookPage.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TrmOutlookPage.ReadState(Reader : TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TrmOutlookControl then
    OutlookControl := TrmOutlookControl(Reader.Parent);
end;

procedure TrmOutlookPage.WMSize(var Message : TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmOutlookPage.SetOutlookControl(AOutlookControl : TrmOutlookControl);
begin
  if FOutlookControl <> AOutlookControl then
  begin
    if FOutlookControl <> nil then
       FOutlookControl.RemovePage(Self);

    Parent := AOutlookControl;
    
    if AOutlookControl <> nil then
       AOutlookControl.InsertPage(Self);
  end;
end;

procedure TrmOutlookPage.SetPageIndex(Value : Integer);
var
  MaxPageIndex : Integer;
begin
  if FOutlookControl <> nil then
  begin
    MaxPageIndex := FOutlookControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt(SOutlookIndexError, [Value, MaxPageIndex]);
    FOutlookControl.FPages.Move(PageIndex, Value);
  end;
end;

procedure TrmOutlookPage.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  PaintButton;
end;

procedure TrmOutlookPage.WMMove(var Message: TWMMove);
begin
  if csDesigning in ComponentState then
  begin
     if Assigned(FOutlookControl) then
        FOutlookControl.Invalidate;
  end;
  inherited;
end;

procedure TrmOutlookPage.Paint;
var
   loop : integer;
begin
   if not (csDestroying in ComponentState) and (Assigned(FOutlookControl)) then
   begin
      if ParentColor then
         Canvas.Brush.Color := FOutlookControl.Color
      else
         Canvas.Brush.Color := Color;

      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0, FOutlookControl.ButtonHeight, Width, Height));
      Canvas.Brush.Style := bsClear;

      PaintButton;

      loop := 0;
      while loop < controlcount do
      begin
         Controls[loop].Refresh;
         inc(loop);
      end;
   end;
end;

procedure TrmOutlookPage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if PtInRect(BtnRect, Point(X,Y)) and (Button = mbLeft) then
  begin
     if (FCloseButton and PtInRect(CloseBtnRect, Point(X, Y))) then
     begin
        FCloseButtonDown := True;
        SetCaptureControl(self);
        PaintButton;  // Might have to be invalidate...?
     end
     else
        FOutlookControl.SetActivePage(self);
  end;
end;

procedure TrmOutlookPage.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmOutlookPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   wLast1, wLast2 : boolean;
begin
  inherited MouseMove(Shift, X, Y);

  wLast1 := fMouseOverBtn;
  fMouseOverBtn := PtInRect(BtnRect, Point(X, Y));

  wLast2 := fCloseMouseOver;
  FCloseMouseOver := fMouseOverBtn and FCloseButton and PtInRect(CloseBtnRect, Point(X, Y));

  if (wLast1 <> fMouseOverBtn) or (wLast2 <> fCloseMouseOver) then
     PaintButton;
end;

procedure TrmOutlookPage.CMMouseEnter(var Message: TMessage);
begin
  FCloseMouseOver := false;
  fMouseOverBtn := false;
  PaintButton;
end;

procedure TrmOutlookPage.CMMouseLeave(var Message: TMessage);
begin
  FCloseMouseOver := false;
  fMouseOverBtn := false;
  PaintButton;
end;

procedure TrmOutlookPage.SetCloseButton(const Value: Boolean);
begin
  FCloseButton := Value;
  PaintButton;
end;

procedure TrmOutlookPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  CanClose : Boolean;

begin
  inherited MouseUp(Button, Shift, X, Y);

  SetCaptureControl(nil);

  if (FCloseButton and PtInRect(CloseBtnRect, Point(X, Y))) then
  begin
    if FCloseButtonDown then
    begin
      //close the page....
      CanClose := True;

      if Assigned(FOnQueryClosePage) then
        FOnQueryClosePage(Self, CanClose);

      if CanClose then
        Self.Free;

      FCloseButtonDown := False;
      PaintButton;
    end;
  end
  else
  begin
     FCloseMouseOver := False;
     PaintButton;
  end;
end;

procedure TrmOutlookPage.CMVisibleChanged(var Message: TMessage);
begin
  Inherited;
  if Assigned(FOutlookControl) then
  begin
{    if Visible then
       FOutlookControl.AdjustPages
    else}
    begin
      if Self.PageIndex = (FOutlookControl.FPages.Count - 1) then
        FOutlookControl.SelectNextPage(False)
      else
        FOutlookControl.SelectNextPage(True);
    end;
  end;
end;

procedure TrmOutlookPage.CMFontChanged(var Message: TMessage);
begin
   Inherited;
   PaintButton;
end;

function TrmOutlookPage.BtnRect: TRect;
begin
    result := Rect(0, 0, Width, FOutlookControl.FButtonHeight);
end;

function TrmOutlookPage.CloseBtnRect: TRect;
var
   wBtn : TRect;
begin
    wBtn := BtnRect;
    result := Rect((wBtn.Right - FOutlookControl.FButtonHeight) + 4,
                   wBtn.Top + 2,
                   wBtn.Right - 3,
                   wBtn.Bottom - 3);
end;

procedure TrmOutlookPage.CMColorChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;
end;

procedure TrmOutlookPage.CMTextChanged(var Message: TMessage);
begin
   Inherited;
   PaintButton;
end;

procedure TrmOutlookPage.CMParentColorChanged(var Message: TMessage);
begin
   if ParentColor then
      Invalidate;  
end;

procedure TrmOutlookPage.CMParentFontChanged(var Message: TMessage);
begin
   Inherited;
   if ParentFont then
      Invalidate;
end;

procedure TrmOutlookPage.PaintButton;
var
  PaintRect : TRect;
  DrawFlags : Integer;
begin
   if not (csDestroying in ComponentState) and (Assigned(FOutlookControl)) then
   begin
      //paint the frame..
      DrawFlags := DFCS_BUTTONPUSH;

      case FOutlookControl.DrawingStyle of
         ds3D :; //Do nothing...
         dsFlat: DrawFlags := DrawFlags or DFCS_FLAT;
         dsNone: DrawFlags := DrawFlags or DFCS_Mono;
      end;

      DrawFrameControl(Canvas.Handle, BtnRect, DFC_BUTTON, DrawFlags);

      if FCloseButton then
      begin
         Canvas.Brush.Color := clBlack;

         DrawFlags := DFCS_CAPTIONCLOSE;

         if FCloseButtonDown and FCloseMouseOver then
           DrawFlags := Drawflags or DFCS_PUSHED
         else
           DrawFlags := Drawflags or DFCS_FLAT;

         DrawFrameControl(Canvas.Handle, CloseBtnRect, DFC_CAPTION, DrawFlags);

         Canvas.Brush.Style := bsClear;
      end;

      PaintRect := BtnRect;

      //paint the bitmap if there is one...
      if ImageIndex <> -1 then
      begin
         if Assigned(FOutlookControl.FImages) then
         begin
             FOutlookControl.FImages.Draw(Canvas, 2, (FOutlookControl.ButtonHeight div 2) - (FOutlookControl.FImages.Width div 2) , ImageIndex);
             PaintRect.left := FOutlookControl.FImages.Width+4;
         end;
      end;

      //Adjust for closebtn...
      PaintRect.right := ClosebtnRect.Left - 2;

      //paint the text...
      DrawFlags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;

      case FAlignment of
        taLeftJustify : DrawFlags := DrawFlags or DT_LEFT;
        taRightJustify : DrawFlags := DrawFlags or DT_RIGHT;
        taCenter : DrawFlags := DrawFlags or DT_CENTER;
      end;

      if ParentFont then
         Canvas.Font.assign(fOutlookControl.Font)
      else
         Canvas.Font.assign(Font);

      if fMouseOverBtn then
         canvas.font.color := clHighlight;

      DrawTextEx(Canvas.Handle, PChar(Caption), Length(Caption), PaintRect, DrawFlags, nil);
   end;
end;

{ TrmOutlookControl }

constructor TrmOutlookControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  width := 175;
  height := 250;
  fDrawingStyle := ds3D;
  Caption := '';
  FButtonHeight := 18;
  color := clAppWorkspace;

  FPages := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TrmOutlookControl.Destroy;
var
  I : Integer;

begin
  for I := FPages.Count - 1 downto 0 do
    TrmOutlookPage(FPages[I]).Free;
  FPages.Free;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TrmOutlookControl.ImageListChange(Sender : TObject);
begin
   Invalidate;
end;

procedure TrmOutlookControl.AdjustPages;
var
  loop : Integer;
  wVisibleCount : Integer;
  ProcessFlag : Boolean;
  wTop : integer;                         
  wPage : TrmOutLookPage;

begin
   if (csDestroying in ComponentState) then
      exit;

   //how many are visible?
   wVisibleCount := 0;
   if (csDesigning in ComponentState) then
   begin
      wVisibleCount := FPages.Count;
   end
   else
   begin
      for loop := 0 to FPages.Count - 1 do
      begin
        if TrmOutLookPage(fPages[loop]).Visible then
          inc(wVisibleCount);
      end;
   end;

   wTop := GetBorderWidth;
   for loop := 0 to FPages.Count - 1 do
   begin
     if (csDesigning in ComponentState) then
       ProcessFlag := True
     else
     begin
       if TrmOutLookPage(FPages[loop]).Visible then
         ProcessFlag := True
       else
         ProcessFlag := False;
     end;

     if ProcessFlag then
     begin
        wPage := TrmOutLookPage(FPages[loop]);

        wPage.Left := GetBorderWidth;
        wPage.Width := ClientWidth - GetBorderWidth;
        wPage.Top := wTop;

        if loop = FActivePage.PageIndex then
           wPage.Height := ((ClientHeight - GetBorderWidth) - ((wVisibleCount - 1) * FButtonHeight))
        else
           wPage.Height := FButtonHeight;

        wPage.UpdatePage;
        wPage.Invalidate;

        inc(wTop, wPage.Height);
     end;
   end;
end;


function TrmOutlookControl.FindNextPage(CurPage : TrmOutlookPage; GoForward : Boolean) : TrmOutlookPage;
var
  I, StartIndex : Integer;
begin
  Result := nil;
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
    begin
      if GoForward then
      begin
        StartIndex := FPages.Count - 1;
        for I := StartIndex downto 0 do
        begin
          if TrmOutlookPage(FPages[I]).Visible then
          begin
            StartIndex := I;
            Break;
          end;
        end;
      end
      else
      begin
        StartIndex := 0;
        for I := 0 to FPages.Count - 1 do
        begin
          if TrmOutlookPage(FPages[I]).Visible then
          begin
            StartIndex := I;
            Break;
          end;
        end;
      end;
    end;

    if GoForward then
    begin
      Inc(StartIndex);
      if StartIndex = FPages.Count then
        StartIndex := 0;
      for I := StartIndex to FPages.Count - 1 do
      begin
        if TrmOutlookPage(FPages[I]).Visible then
        begin
          StartIndex := I;
          Break;
        end;
      end;
    end
    else
    begin
      if StartIndex = 0 then
        StartIndex := FPages.Count;
      Dec(StartIndex);
      for I := StartIndex downto 0 do
      begin
        if TrmOutlookPage(FPages[I]).Visible then
        begin
          StartIndex := I;
          Break;
        end;
      end;
    end;
    Result := FPages[StartIndex];
  end;
end;

procedure TrmOutlookControl.GetChildren(Proc : TGetChildProc; Root : TComponent);
var
  I : Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TrmOutlookControl.GetPage(Index : Integer) : TrmOutlookPage;
begin
  Result := FPages[Index];
end;

function TrmOutlookControl.GetPageCount : Integer;
begin
  Result := FPages.Count;
end;

procedure TrmOutlookControl.InsertPage(Page : TrmOutlookPage);
begin
  FPages.Add(Page);
  Page.FOutlookControl := Self;
  Page.FreeNotification(self);
end;

procedure TrmOutlookControl.RemovePage(Page : TrmOutlookPage);
var
   wPage : TrmOutlookPage;
begin
  if FActivePage = Page then
  begin
     wPage := FindNextPage(FActivePage, True);

     if wPage = Page then
        FActivePage := nil
     else
        FActivePage := wPage;
  end;

  FPages.Remove(Page);
  Page.FOutlookControl := nil;

  if not (csDestroying in ComponentState) then
     Invalidate;
end;

procedure TrmOutlookControl.SelectNextPage(GoForward : Boolean);
begin
  SetActivePage(FindNextPage(ActivePage, GoForward));
end;

procedure TrmOutlookControl.SetActivePage(Page : TrmOutlookPage);
begin
  if not (csDestroying in ComponentState) then
  begin
     if (assigned(Page) and (Page.OutlookControl = Self)) or (Page = nil) then
     begin
        fActivePage := Page;
        AdjustPages;

        if Assigned(FPageChanged) and not (csDestroying in ComponentState) then
           FPageChanged(self);
     end;
  end; 
end;

procedure TrmOutlookControl.SetChildOrder(Child : TComponent; Order : Integer);
begin
  TrmOutlookPage(Child).PageIndex := Order;
end;

procedure TrmOutlookControl.ShowControl(AControl : TControl);
begin
  if (AControl is TrmOutlookPage) and (TrmOutlookPage(AControl).OutlookControl = Self) then
    SetActivePage(TrmOutlookPage(AControl));
  inherited ShowControl(AControl);
end;

procedure TrmOutlookControl.WMSize(var Message : TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmOutlookControl.CMDialogKey(var Message : TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TrmOutlookControl.SetImages(Value : TCustomImageList);
begin
  if Images <> nil then
     Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TrmOutlookControl.SetButtonHeight(value : integer);
begin
  FButtonHeight := value;
  Invalidate;
end;

procedure TrmOutlookControl.WMMove(var Message: TWMMove);
begin
  inherited;
  Invalidate;
end;

procedure TrmOutlookControl.Paint;
var
   wRect : TRect;
begin
  wRect := ClientRect;
  InflateRect(wRect, GetBorderWidth, GetBorderWidth);
  Canvas.Brush.Color := clAppworkspace;
  Canvas.Brush.Style := bsSolid;

  case fDrawingStyle of
     ds3D:
        begin
           Frame3d(Canvas, wRect, clBtnShadow, clBtnHighlight, 1);
           Frame3d(Canvas, wRect, cl3DDkShadow, cl3DLight, 1);
        end;
     dsFlat:
        begin
           Frame3d(Canvas, wRect, cl3DDkShadow, cl3DDkShadow, 1);
        end;
  else
     //Do Nothing...
  end;

  Canvas.FillRect(wRect);

  AdjustPages;
{  if assigned(FActivePage) then
     fActivePage.Invalidate;}
end;

procedure TrmOutlookControl.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

function TrmOutlookControl.GetClientRect: TRect;
begin
   result := inherited GetClientRect;
   InflateRect(result, -GetBorderWidth, -GetBorderWidth);
end;

procedure TrmOutlookControl.SetDrawingStyle(const Value: TrmDrawingStyle);
begin
  fDrawingStyle := Value;
  invalidate;
end;

function TrmOutlookControl.GetBorderWidth: integer;
begin
   case fDrawingStyle of
     ds3D : result := 2;
     dsFlat : result := 1;
   else
     result := 0; 
   end;
end;

procedure TrmOutlookControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;

    if (AComponent is TrmOutlookPage) and (TrmOutlookPage(AComponent).OutlookControl = self) then
       RemovePage(TrmOutlookPage(AComponent));
  end;
end;

procedure TrmOutlookControl.Loaded;
begin
  inherited;
end;

procedure TrmOutlookControl.CMColorChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;  
end;

procedure TrmOutlookControl.CMFontChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;
end;

end.

