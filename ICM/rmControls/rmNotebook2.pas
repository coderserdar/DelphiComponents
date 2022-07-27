{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmNotebook
Purpose  : A replacement for Borlands Notebook component.
Date     : 11-20-02
Author   : Ryan J. Mills
Version  : 1.90
Notes    : 
================================================================================}

unit rmNotebook2;

interface

{$I CompilerDefines.INC}

uses Windows, Messages, Forms, Classes, Controls, Graphics, ImgList, sysutils;

type
  TrmNotebookControl = class;
  TrmNotebookPage = class;

  TrmNotebookPageEvent = procedure(ASheet : TrmNotebookPage) of object;
  TrmNotebookQueryPageEvent = procedure(ASheet : TrmNotebookPage; var CanClose : Boolean) of object;

  TrmNotebookPage = class(TCustomControl)
  private
    FNotebookControl : TrmNotebookControl;
    FImageIndex : Integer;

    FOnQueryClosePage: TrmNotebookQueryPageEvent;
    FOnDestroy : TrmNotebookPageEvent;
    fData: integer;

    procedure SetImageIndex(Value : Integer);
    function GetPageIndex : Integer;

    procedure SetNotebookControl(ANotebookControl : TrmNotebookControl);
    procedure SetPageIndex(Value : Integer);

  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure ReadState(Reader : TReader); override;

    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;

    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property NotebookControl : TrmNotebookControl read FNotebookControl write SetNotebookControl;
  published
    property Color default clAppWorkSpace;
    property Caption;
    property Data : integer read fData write fData;
    property Font;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property ParentFont;
    property ParentColor;

    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property PageIndex : Integer read GetPageIndex write SetPageIndex stored False;

    property OnEnter;
    property OnExit;
    property OnResize;
    property OnDestroy : TrmNotebookPageEvent read FOnDestroy write FOnDestroy;
    property OnQueryClosePage : TrmNotebookQueryPageEvent read FOnQueryClosePage write FOnQueryClosePage;
  end;

  TrmNotebookControl = class(TCustomControl)
  private
    FPages : TList;
    FImages : TCustomImageList;
    FActivePage : TrmNotebookPage;
    FImageChangeLink : TChangeLink;

    FPageChanged : TNotifyEvent;

    function GetPage(Index : Integer) : TrmNotebookPage;
    function GetPageCount : Integer;

    procedure InsertPage(Page : TrmNotebookPage);
    procedure RemovePage(Page : TrmNotebookPage);
    procedure SetActivePage(Page : TrmNotebookPage);

    procedure SetImages(Value : TCustomImageList);
    procedure ImageListChange(Sender : TObject);

    procedure CMDialogKey(var Message : TCMDialogKey); message CM_DIALOGKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
    procedure SetChildOrder(Child : TComponent; Order : Integer); override;
    procedure ShowControl(AControl : TControl); override;

    procedure WMErase(var Message : TMessage); message WM_ERASEBKGND;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    function FindNextPage(CurPage : TrmNotebookPage; GoForward : Boolean) : TrmNotebookPage;
    procedure SelectNextPage(GoForward : Boolean);

    property PageCount : Integer read GetPageCount;
    property Pages[Index : Integer] : TrmNotebookPage read GetPage;
  published
    property Align;
    property Color default clAppWorkspace;
    property Font;
    property Images : TCustomImageList read FImages write SetImages;
    property ActivePage : TrmNotebookPage read FActivePage write SetActivePage;
    property OnPageChanged : TNotifyEvent read fPageChanged write fPageChanged;
    property OnResize;
  end;


implementation

uses extCtrls, rmLibrary;

const
  SNotebookIndexError = 'Sheet index Error';

{ TrmNotebookPage }

constructor TrmNotebookPage.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csAcceptsControls]-[csDesignInteractive];

  Visible := True;
  Caption := '';
  FImageIndex := -1;
  color := clAppWorkSpace;
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

function TrmNotebookPage.GetPageIndex : Integer;
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
  if Reader.Parent is TrmNotebookControl then
    NotebookControl := TrmNotebookControl(Reader.Parent);
end;

procedure TrmNotebookPage.SetNotebookControl(ANotebookControl : TrmNotebookControl);
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

procedure TrmNotebookPage.SetPageIndex(Value : Integer);
var
  MaxPageIndex : Integer;
begin
  if FNotebookControl <> nil then
  begin
    MaxPageIndex := FNotebookControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt(SNotebookIndexError, [Value, MaxPageIndex]);
    FNotebookControl.FPages.Move(PageIndex, Value);
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
end;

procedure TrmNotebookPage.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmNotebookPage.CMVisibleChanged(var Message: TMessage);
begin
  Inherited;
  if Assigned(FNotebookControl) then
  begin
    if Visible then
//       FNotebookControl.AdjustPages
    else
    begin
      if Self.PageIndex = (FNotebookControl.FPages.Count - 1) then
        FNotebookControl.SelectNextPage(False)
      else
        FNotebookControl.SelectNextPage(True);
    end;
  end;
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

{ TrmNotebookControl }

constructor TrmNotebookControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  width := 175;
  height := 250;
  Caption := '';
  color := clAppWorkspace;

  FPages := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TrmNotebookControl.Destroy;
var
  I : Integer;

begin
  for I := FPages.Count - 1 downto 0 do
    TrmNotebookPage(FPages[I]).Free;
  FPages.Free;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TrmNotebookControl.ImageListChange(Sender : TObject);
begin
   Invalidate;
end;

function TrmNotebookControl.FindNextPage(CurPage : TrmNotebookPage; GoForward : Boolean) : TrmNotebookPage;
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

procedure TrmNotebookControl.GetChildren(Proc : TGetChildProc; Root : TComponent);
var
  I : Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TrmNotebookControl.GetPage(Index : Integer) : TrmNotebookPage;
begin
  Result := FPages[Index];
end;

function TrmNotebookControl.GetPageCount : Integer;
begin
  Result := FPages.Count;
end;

procedure TrmNotebookControl.InsertPage(Page : TrmNotebookPage);
begin
  FPages.Add(Page);
  Page.FNotebookControl := Self;
  Page.FreeNotification(self);
end;

procedure TrmNotebookControl.RemovePage(Page : TrmNotebookPage);
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

procedure TrmNotebookControl.SelectNextPage(GoForward : Boolean);
begin
  SetActivePage(FindNextPage(ActivePage, GoForward));
end;

procedure TrmNotebookControl.SetActivePage(Page : TrmNotebookPage);
var
   woldPage : TrmNotebookPage;
   wPageChanged : boolean;
begin
  if (csDesigning in componentstate) then
  begin
     if (assigned(Page) and (Page.NotebookControl = Self)) or (Page = nil) then
     begin
        if assigned(fActivePage) then
        begin
           if fActivePage <> Page then
           begin
              wOldPage := fActivePage;
              fActivePage := Page;
              fActivePage.Show;
              fActivePage.BringToFront;
              if FActivePage.CanFocus then
                 fActivePage.SetFocus;
              wOldPage.Hide;
           end;
        end
        else
        begin
           fActivePage := Page;
           fActivePage.Show;
           fActivePage.BringToFront;
           if FActivePage.CanFocus then
              fActivePage.SetFocus;
        end;
     end;
  end;

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
              fActivePage.Show;
              if FActivePage.CanFocus then
                 fActivePage.SetFocus;
              wOldPage.Hide;
              wPageChanged := true;
           end;
        end
        else
        begin
           fActivePage := Page;
           fActivePage.Show;
           if FActivePage.CanFocus then
              fActivePage.SetFocus;
           wPageChanged := true;
        end;
     end;
  end;
  if wPageChanged and Assigned(FPageChanged) and not (csDestroying in ComponentState) then
     FPageChanged(self);
end;

procedure TrmNotebookControl.SetChildOrder(Child : TComponent; Order : Integer);
begin
  TrmNotebookPage(Child).PageIndex := Order;
end;

procedure TrmNotebookControl.ShowControl(AControl : TControl);
begin
  if (AControl is TrmNotebookPage) and (TrmNotebookPage(AControl).NotebookControl = Self) then
    SetActivePage(TrmNotebookPage(AControl));
  inherited ShowControl(AControl);
end;

procedure TrmNotebookControl.CMDialogKey(var Message : TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TrmNotebookControl.SetImages(Value : TCustomImageList);
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

procedure TrmNotebookControl.Paint;
var
   wRect : TRect;
begin
  wRect := ClientRect;
  Canvas.Brush.Color := clAppworkspace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(wRect);

  if assigned(FActivePage) then
     fActivePage.Invalidate;
end;

procedure TrmNotebookControl.WMErase(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TrmNotebookControl.Notification(AComponent: TComponent;
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

procedure TrmNotebookControl.CMColorChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;  
end;

procedure TrmNotebookControl.CMFontChanged(var Message: TMessage);
begin
   Inherited;
   Invalidate;
end;

end.

