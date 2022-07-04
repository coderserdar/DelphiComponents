unit mbXPArrowEdit;

interface

uses
  SysUtils, Classes, StdCtrls, Windows, Messages, Graphics, Controls,
  mbXPArrowButton, Menus, ImgList;

type
  TmbXPArrowEdit = class(TEdit)
  private
   FButton: TmbXPArrowButton;
   FMenu: TPopupMenu;
   FButtonEnabled: boolean;
   FButtonImageIndex: TImageIndex;
   FButtonImages: TCustomImageList;
   FButtonHint: string;
   FButtonShowHint: boolean;
   FButtonVisible: boolean;
   FButtonWidth: integer;
   FOnButtonClick: TNotifyEvent;
   FButtonJust: boolean;
   FArrowStyle: TArrowStyle;
   FBDrop, FADrop: TNotifyEvent;

   function GetMinHeight: Integer;
   procedure BeforeDrop(Sender: TObject);
   procedure AfterDrop(Sender: TObject);
   procedure SetJust(j: boolean);
   procedure SetStyle(s: TArrowStyle);
   procedure SetMenu(p: TPopupMenu);
   procedure SetButtonEnabled(b: boolean);
   procedure SetButtonImageIndex(i: TImageIndex);
   procedure SetButtonImages(c: TCustomImageList);
   procedure SetButtonHint(s: string);
   procedure SetButtonShowHint(b: boolean);
   procedure SetButtonVisible(b: boolean);
   procedure SetButtonWidth(i: integer);
   procedure ButtonClick(Sender: TObject);
  protected
   procedure CreateParams(var Params: TCreateParams); override;
   procedure CreateWnd; override;
   procedure SetEditRect;
   procedure WMSize(var Message: TWMSize); message WM_SIZE;
   procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property ButtonDropMenu: TPopupMenu read FMenu write SetMenu;
   property ButtonEnabled: boolean read FButtonEnabled write SetButtonEnabled default true;
   property ButtonImageIndex: TImageIndex read FButtonImageIndex write SetButtonImageIndex default -1;
   property ButtonImageList: TCustomImageList read FButtonImages write SetButtonImages;
   property ButtonHint: string read FButtonHint write SetButtonHint;
   property ButtonShowHint: boolean read FButtonShowHint write SetButtonShowHint default false;
   property ButtonVisible: boolean read FButtonVisible write SetButtonVisible default true;
   property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 18;
   property ButtonJustPaintArrow: boolean read FButtonJust write SetJust default false;
   property ButtonArrowStyle: TArrowStyle read FArrowStyle write SetStyle default asRight;

   property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
   property OnAfterDropMenu: TNotifyEvent read FADrop write FADrop;
   property OnBeforeDropMenu: TNotifyEvent read FBDrop write FBDrop;
  end;

implementation

constructor TmbXPArrowEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TmbXPArrowButton.Create(Self);
  with FButton do
   ControlStyle := ControlStyle + [csReplicatable];
  FButton.Parent := Self;
  FButton.Width := 9;
  FButtonWidth := 9;
  FButton.Height := Height - 2;
  FButton.Left := Width - FButton.Width - 4;
  FButton.Top := 0;
  FButton.Visible := True;
  FButtonVisible := true;
  FButton.Enabled := true;
  FButtonEnabled := true;
  FButton.ImageIndex := -1;
  FButtonImageIndex := -1;
  FButton.ShowHint := false;
  FButtonShowHint := false;
  FArrowStyle := asRight;
  FButtonJust := false;
  FButton.OnClick := ButtonClick;
  FButton.OnBeforeDropMenu := BeforeDrop;
  FButton.OnAfterDropMenu := AfterDrop;
end;

destructor TmbXPArrowEdit.Destroy;
begin
 FButton.OnClick := nil;
 FButton.OnBeforeDropMenu := nil;
 FButton.OnAfterDropMenu := nil;
 FButton.Free;
 inherited Destroy;
end;

procedure TmbXPArrowEdit.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 if FButton <> nil then
  FButton.Enabled := Enabled;
end;

procedure TmbXPArrowEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TmbXPArrowEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TmbXPArrowEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TmbXPArrowEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc := Rect(0, 0, ClientWidth - FButton.Width, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
end;

procedure TmbXPArrowEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 3)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TmbXPArrowEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TmbXPArrowEdit.SetMenu(p: TPopupMenu);
begin
 FMenu := p;
 FButton.DropMenu := p;
end;

procedure TmbXPArrowEdit.SetButtonEnabled(b: boolean);
begin
 FButtonEnabled := b;
 FButton.Enabled := b;
end;

procedure TmbXPArrowEdit.SetButtonImageIndex(i: TImageIndex);
begin
 FButtonImageIndex := i;
 FButton.ImageIndex := i;
end;

procedure TmbXPArrowEdit.SetButtonImages(c: TCustomImageList);
begin
 FButtonImages := c;
 FButton.Images := c;
end;

procedure TmbXPArrowEdit.SetButtonHint(s: string);
begin
 FButtonHint := s;
 FButton.Hint := s;
end;

procedure TmbXPArrowEdit.SetJust(j: boolean);
begin
 FButtonJust := j;
 FButton.JustPaintArrow := j;
end;

procedure TmbXPArrowEdit.SetStyle(s: TArrowStyle);
begin
 FArrowStyle := s;
 FButton.ArrowStyle := s;
end;

procedure TmbXPArrowEdit.SetButtonShowHint(b: boolean);
begin
 FButtonShowHint := b;
 FButton.ShowHint := b;
end;

procedure TmbXPArrowEdit.SetButtonVisible(b: boolean);
var
 w, t, l: integer;
begin
 FButtonVisible := b;
 FButton.Visible := b;
 w := FButtonWidth;
 t := FButton.Top;
 l := FButton.Left;
 if b = true then
  FButton.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 3)
 else
  FButton.SetBounds(0, 0, 0, 0);
 SetEditRect;
 Invalidate;
 FButtonWidth := w;
 FButton.Width := w;
 FButton.Top := t;
 FButton.Left := l;
 FButton.Height := Height - 3;
end;

procedure TmbXPArrowEdit.SetButtonWidth(i: integer);
begin
 FButtonWidth := i;
 FButton.Width := i;
 FButton.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 3);
 SetEditRect;
 Invalidate;
end;

procedure TmbXPArrowEdit.ButtonClick(Sender: TObject);
begin
 if Assigned(FOnButtonClick) then
  FOnButtonClick(Self);
end;

procedure TmbXPArrowEdit.BeforeDrop(Sender: TObject);
begin
 if Assigned(FBDrop) then FBDrop(Self);
end;

procedure TmbXPArrowEdit.AfterDrop(Sender: TObject);
begin
 if Assigned(FADrop) then FADrop(Self);
end;

end.
