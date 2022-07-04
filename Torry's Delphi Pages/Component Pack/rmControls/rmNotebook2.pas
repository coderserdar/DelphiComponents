{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmNotebook
Purpose  : A replacement for Borlands Notebook component.
Date     : 11-20-02
Author   : Ryan J. Mills
Version  : 1.92
Notes    :
================================================================================}

unit rmNotebook2;

interface

{$I CompilerDefines.INC}
{$D+}

uses Windows, rmTabs3x, Messages, Forms, Classes, Controls, Graphics, ImgList, sysutils;

type
  TrmCustomNotebookControl = class;
  TrmNotebookPage = class;

  TrmNotebookPageEvent = procedure(ASheet : TrmNotebookPage) of object;
  TrmNotebookQueryPageEvent = procedure(ASheet : TrmNotebookPage; var CanClose : Boolean) of object;

  TrmNotebookPage = class(TCustomControl)
  private
    FNotebookControl : TrmCustomNotebookControl;
    FImageIndex : Integer;

    FOnQueryClosePage: TrmNotebookQueryPageEvent;
    FOnDestroy : TrmNotebookPageEvent;
    fData: integer;

    procedure SetImageIndex(Value : Integer);
    function GetPageOrderIndex : Integer;

    procedure SetNotebookControl(ANotebookControl : TrmCustomNotebookControl);
    procedure SetPageOrderIndex(Value : Integer);
  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure ReadState(Reader : TReader); override;

    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property NotebookControl : TrmCustomNotebookControl read FNotebookControl write SetNotebookControl;
  published
    property Color;
    property Caption;
    property Data : integer read fData write fData;
    property Font;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible default true;
    property ParentFont;
    property ParentColor;

    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property PageOrderIndex : Integer read GetPageOrderIndex write SetPageOrderIndex stored False;

    property OnEnter;
    property OnExit;
    property OnResize;
    property OnDestroy : TrmNotebookPageEvent read FOnDestroy write FOnDestroy;
    property OnQueryClosePage : TrmNotebookQueryPageEvent read FOnQueryClosePage write FOnQueryClosePage;
  end;

  TPageChangingEvent = procedure(Sender: TObject; NewPageIndex : integer; var AllowChange: Boolean) of object;

  TrmCustomNotebookControl = class(TCustomControl)
  private
    FPages : TList;
    FImages : TCustomImageList;
    FActivePage : TrmNotebookPage;
    FImageChangeLink : TChangeLink;
    FBorderStyle: TBorderStyle;

    FPageChanged : TNotifyEvent;
    FPageChanging : TPageChangingEvent;

    function GetPage(Index : Integer) : TrmNotebookPage;
    function GetPageCount : Integer;

    procedure SetImages(Value : TCustomImageList);
    procedure ImageListChange(Sender : TObject);

    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;

    function GetActivePageIndex: integer;
    procedure SetBorderStyle(Value: TBorderStyle);
  protected
    procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
    procedure SetChildOrder(Child : TComponent; Order : Integer); override;
    procedure ShowControl(AControl : TControl); override;

    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InsertPage(Page : TrmNotebookPage); virtual;
    procedure RemovePage(Page : TrmNotebookPage); virtual;
    procedure SetActivePage(Page : TrmNotebookPage); virtual;
    procedure SetActivePageIndex(PageIndex:integer); virtual;

    procedure DoPageChanged; virtual;
    function DoPageChanging(NewPageIndex:integer):boolean; virtual;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Images : TCustomImageList read FImages write SetImages;
    property ActivePageIndex : integer read GetActivePageIndex write SetActivePageIndex stored false;
    property OnPageChanging : TPageChangingEvent read fPageChanging write fPageChanging;
    property OnPageChanged : TNotifyEvent read fPageChanged write fPageChanged;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    function NewPage:TrmNoteBookPage;

    function FindNextPage(CurPage : TrmNotebookPage; GoForward : Boolean) : TrmNotebookPage;
    procedure SelectNextPage(GoForward : Boolean);

    property PageCount : Integer read GetPageCount;
    property Pages[Index : Integer] : TrmNotebookPage read GetPage;

  published
    property ActivePage : TrmNotebookPage read FActivePage write SetActivePage;
  end;

  TrmTabbedNoteBookControl = class(TrmCustomNotebookControl)
  private
    fTabs : TrmTabSet;
    fchanging : boolean;
    
    procedure DoTabChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);

    function GetTabsVisible: boolean;
    procedure SetTabsVisible(const Value: boolean);

    procedure RebuildTabs;
    function GetTabBackgroundcolor: TColor;
    function GetTabDitherBackground: Boolean;
    function GetTabSelectedColor: TColor;
    function GetTabType: TTabType;
    function GetTabUnselectedColor: TColor;
    procedure setTabBackgroundColor(const Value: TColor);
    procedure SetTabDitherBackground(const Value: Boolean);
    procedure SetTabSelectedColor(const Value: TColor);
    procedure SetTabType(const Value: TTabType);
    procedure SetTabUnselectedColor(const Value: TColor);
  protected
    procedure InsertPage(Page : TrmNotebookPage); override;
    procedure RemovePage(Page : TrmNotebookPage); override;
    procedure DoPageChanged; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Color;
    property Font;
    property OnResize;
    property BorderStyle;
    property ActivePageIndex;
    property OnPageChanging;
    property OnPageChanged;
    property Images;
    property TabSelectedColor: TColor read GetTabSelectedColor write SetTabSelectedColor;
    property TabUnselectedColor: TColor read GetTabUnselectedColor write SetTabUnselectedColor;
    property TabDitherBackground: Boolean read GetTabDitherBackground write SetTabDitherBackground;
    property TabBackgroundColor : TColor read GetTabBackgroundcolor write setTabBackgroundColor;
    property TabType : TTabType read GetTabType write SetTabType;
    property TabsVisible:boolean read GetTabsVisible write SetTabsVisible;
  end;

  TrmNoteBookControl = class(TrmCustomNoteBookControl)
  published
    property Align;
    property Color;
    property Font;
    property OnResize;
    property BorderStyle;
    property ActivePageIndex;
    property OnPageChanging;
    property OnPageChanged;
    property Images;
  end;


implementation

uses rmLibrary;

const
  SNotebookIndexError = 'Sheet index Error';

{ TrmNotebookPage }

constructor TrmNotebookPage.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls] - [csDesignInteractive];

  Visible := True;
  Caption := '';
  FImageIndex := -1;
  align := alClient;
end;

procedure TrmNotebookPage.SetImageIndex(Value : Integer);
begin
  FImageIndex := Value;
//  PaintButton;
end;

destructor TrmNotebookPage.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);

  inherited Destroy;
end;

function TrmNotebookPage.GetPageOrderIndex : Integer;
begin
  if FNotebookControl <> nil then
    Result := FNotebookControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TrmNotebookPage.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TrmNotebookPage.ReadState(Reader : TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TrmCustomNotebookControl then
    NotebookControl := TrmCustomNotebookControl(Reader.Parent);
end;

procedure TrmNotebookPage.SetNotebookControl(ANotebookControl : TrmCustomNotebookControl);
begin
  if FNotebookControl <> ANotebookControl then
  begin
    if FNotebookControl <> nil then
       FNotebookControl.RemovePage(Self);

    Parent := ANotebookControl;
    
    if ANotebookControl <> nil then
       ANotebookControl.InsertPage(Self);
  end;
end;

procedure TrmNotebookPage.SetPageOrderIndex(Value : Integer);
var
  MaxPageIndex : Integer;
begin
  if FNotebookControl <> nil then
  begin
    MaxPageIndex := FNotebookControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt(SNotebookIndexError, [Value, MaxPageIndex]);
    FNotebookControl.FPages.Move(PageOrderIndex, Value);
  end;
end;

procedure TrmNotebookPage.Paint;
begin
   if not (csDestroying in ComponentState) and (Assigned(FNotebookControl)) then
   begin
      if ParentColor then
         Canvas.Brush.Color := FNotebookControl.Color
      else
         Canvas.Brush.Color := Color;

      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0, 0, Width, Height));
   end;

   if csDesigning in ComponentState then
   with Canvas do
   begin
     Pen.Style := psDash;
     Brush.Style := bsClear;
     Rectangle(0, 0, Width, Height);
   end;

end;

procedure TrmNotebookPage.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmNotebookPage.CMColorChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;
end;

procedure TrmNotebookPage.CMParentColorChanged(var Message: TMessage);
begin
   inherited;
   if ParentColor then
      Invalidate;
end;

{ TrmCustomNotebookControl }

constructor TrmCustomNotebookControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  width := 250;
  height := 250;
  Caption := '';
  

  FBorderStyle := bsNone;

  FPages := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TrmCustomNotebookControl.Destroy;
var
  I : Integer;

begin
  for I := FPages.Count - 1 downto 0 do
    TrmNotebookPage(FPages[I]).Free;
  FPages.Free;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TrmCustomNotebookControl.ImageListChange(Sender : TObject);
begin
   Invalidate;
end;

function TrmCustomNotebookControl.FindNextPage(CurPage : TrmNotebookPage; GoForward : Boolean) : TrmNotebookPage;
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
          if TrmNotebookPage(FPages[I]).Visible then
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
          if TrmNotebookPage(FPages[I]).Visible then
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
        if TrmNotebookPage(FPages[I]).Visible then
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
        if TrmNotebookPage(FPages[I]).Visible then
        begin
          StartIndex := I;
          Break;
        end;
      end;
    end;
    Result := FPages[StartIndex];
  end;
end;

procedure TrmCustomNotebookControl.GetChildren(Proc : TGetChildProc; Root : TComponent);
var
  I : Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TrmCustomNotebookControl.GetPage(Index : Integer) : TrmNotebookPage;
begin
  Result := FPages[Index];
end;

function TrmCustomNotebookControl.GetPageCount : Integer;
begin
  Result := FPages.Count;
end;

procedure TrmCustomNotebookControl.InsertPage(Page : TrmNotebookPage);
begin
  FPages.Add(Page);
  Page.FNotebookControl := Self;
  Page.FreeNotification(self);
end;

procedure TrmCustomNotebookControl.RemovePage(Page : TrmNotebookPage);
var
   wPage : TrmNotebookPage;
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
  Page.FNotebookControl := nil;

  if not (csDestroying in ComponentState) then
     Invalidate;
end;

procedure TrmCustomNotebookControl.SelectNextPage(GoForward : Boolean);
begin
  SetActivePage(FindNextPage(ActivePage, GoForward));
end;

procedure TrmCustomNotebookControl.SetActivePage(Page : TrmNotebookPage);
var
   woldPage : TrmNotebookPage;
   wPageChanged : boolean;
begin
  wPageChanged := false;

  if not (csDestroying in ComponentState) then
  begin
     if (assigned(Page) and (Page.NotebookControl = Self)) or (Page = nil) then
     begin
        if assigned(fActivePage) then
        begin
           
           if fActivePage <> Page then
           begin
              wOldPage := fActivePage;
              fActivePage := Page;
              if csdesigning in componentstate then
              begin
                 fActivePage.BringToFront;
                 wOldPage.SendToBack;
              end
              else
              begin
                 fActivePage.Show;
                 fActivePage.BringToFront;

                 wOldPage.SendToBack;
                 wOldPage.Hide;

              end;

              try
                 fActivePage.SelectFirst;
              except
                 //do nothing...
              end;
              
              wPageChanged := true;
           end;
        end
        else
        begin
           fActivePage := Page;
           fActivePage.Show;
           if csdesigning in componentstate then
              fActivePage.BringToFront;
           try
              fActivePage.SelectFirst;
           except
              //do nothing...
           end;
           wPageChanged := true;
        end;
     end;
  end;

  if wPageChanged then
     DoPageChanged;
end;

procedure TrmCustomNotebookControl.SetChildOrder(Child : TComponent; Order : Integer);
begin
  TrmNotebookPage(Child).PageOrderIndex := Order;
end;

procedure TrmCustomNotebookControl.ShowControl(AControl : TControl);
begin
  if (AControl is TrmNotebookPage) and (TrmNotebookPage(AControl).NotebookControl = Self) then
    SetActivePage(TrmNotebookPage(AControl));
  inherited ShowControl(AControl);
end;

procedure TrmCustomNotebookControl.CMDialogKey(var Message : TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TrmCustomNotebookControl.SetImages(Value : TCustomImageList);
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

procedure TrmCustomNotebookControl.Paint;
var
   wRect : TRect;
begin
  wRect := ClientRect;
  Canvas.Brush.Color := color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(wRect);

  if assigned(FActivePage) then
     fActivePage.Invalidate;
end;

procedure TrmCustomNotebookControl.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmCustomNotebookControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;

    if (AComponent is TrmNotebookPage) and (TrmNotebookPage(AComponent).NotebookControl = self) then
       RemovePage(TrmNotebookPage(AComponent));
  end;
end;

procedure TrmCustomNotebookControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TrmCustomNotebookControl.CMColorChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;  
end;

procedure TrmCustomNotebookControl.CMFontChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;
end;

procedure TrmCustomNotebookControl.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmCustomNotebookControl.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TrmCustomNotebookControl.SetActivePageIndex(PageIndex: integer);
begin
   SetActivePage(TrmNotebookPage(fPages[PageIndex]));
end;

procedure TrmCustomNotebookControl.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TrmCustomNotebookControl.GetActivePageIndex: integer;
begin
   if assigned(factivepage) then
      result := FPages.IndexOf(FActivePage)
   else
      result := -1;
end;

function TrmCustomNotebookControl.NewPage: TrmNoteBookPage;
var
   wPage : TrmNotebookPage;
begin
   wPage := TrmNotebookPage.Create(nil);
   wPage.NotebookControl := self;
   ActivePage := wPage;
   result := wPage;
end;

procedure TrmCustomNotebookControl.DoPageChanged;
begin
   if not (csDestroying in ComponentState) and Assigned(FPageChanged) then
      FPageChanged(self);
end;

function TrmCustomNotebookControl.DoPageChanging(NewPageIndex:integer): boolean;
var
   wResult : boolean;
begin
   wResult := true;
   if Assigned(fPageChanging) then
      FPageChanging(Self, NewPageIndex, wResult);
   result := wResult;
end;

{ TrmTabbedNoteBookControl }

constructor TrmTabbedNoteBookControl.Create(AOwner: TComponent);
begin
  inherited;
  fTabs := TrmTabSet.Create(self);
  fTabs.parent := self;
  fTabs.Align := alBottom;
  fTabs.OnChange := DoTabChange;
  fchanging := false;
end;

destructor TrmTabbedNoteBookControl.Destroy;
begin
  fTabs.Free;
  inherited;
end;

function TrmTabbedNoteBookControl.GetTabsVisible: boolean;
begin
   result := fTabs.Visible;
end;

procedure TrmTabbedNoteBookControl.SetTabsVisible(const Value: boolean);
begin
   fTabs.Visible := value;
end;

procedure TrmTabbedNoteBookControl.RebuildTabs;
var
   loop : integer;
begin
   if (csdestroying in componentstate) then
      exit;

   fTabs.Tabs.BeginUpdate;
   try
      fTabs.Tabs.Clear;
      if fPages.count > 0 then
      begin
         for loop := 0 to fPages.Count-1 do
            fTabs.Tabs.Add(TrmNotebookPage(fPages[loop]).Caption);
         fTabs.TabIndex := ActivePageIndex;
      end;
   finally
      fTabs.Tabs.EndUpdate;
   end;
end;



procedure TrmTabbedNoteBookControl.DoPageChanged;
begin
  inherited;
  fTabs.TabIndex := ActivePageIndex;
end;

procedure TrmTabbedNoteBookControl.InsertPage(Page: TrmNotebookPage);
begin
  inherited;
  RebuildTabs;
end;

procedure TrmTabbedNoteBookControl.RemovePage(Page: TrmNotebookPage);
begin
  inherited;
  RebuildTabs;
end;

function TrmTabbedNoteBookControl.GetTabBackgroundcolor: TColor;
begin
   result := fTabs.BackgroundColor;
end;

function TrmTabbedNoteBookControl.GetTabDitherBackground: Boolean;
begin
   result := fTabs.DitherBackground;
end;

function TrmTabbedNoteBookControl.GetTabSelectedColor: TColor;
begin
   result := fTabs.SelectedColor;
end;

function TrmTabbedNoteBookControl.GetTabType: TTabType;
begin
   result := fTabs.TabType;
end;

function TrmTabbedNoteBookControl.GetTabUnselectedColor: TColor;
begin
   result := fTabs.UnselectedColor;
end;

procedure TrmTabbedNoteBookControl.setTabBackgroundColor(
  const Value: TColor);
begin
   fTabs.BackgroundColor := value;
end;

procedure TrmTabbedNoteBookControl.SetTabDitherBackground(
  const Value: Boolean);
begin
   fTabs.DitherBackground := Value;
end;

procedure TrmTabbedNoteBookControl.SetTabSelectedColor(
  const Value: TColor);
begin
   fTabs.SelectedColor := Value;
end;

procedure TrmTabbedNoteBookControl.SetTabType(const Value: TTabType);
begin
   fTabs.TabType := Value;
end;

procedure TrmTabbedNoteBookControl.SetTabUnselectedColor(
  const Value: TColor);
begin
   fTabs.UnselectedColor := Value;
end;

procedure TrmTabbedNoteBookControl.DoTabChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
   if fChanging then
      exit;

   fChanging := true;
   try
      AllowChange := DoPageChanging(newtab);
      if AllowChange then
         ActivePageIndex := newTab;
   finally
      fchanging := false;
   end;
end;

end.

