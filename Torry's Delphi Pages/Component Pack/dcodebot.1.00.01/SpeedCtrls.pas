
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit SpeedCtrls;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PopCtrls, Buttons, StdCtrls, GraphTools, ImgList;

type
  TPopupButton = class(TSpeedButton)
  public
    procedure Click; override;
  end;

  TPopupOrientation = (poLeft, poTop, poRight, poBottom);

  TPopupButtonForm = class(TCustomPopupForm)
  private
    FAssociateButton: TSpeedButton;
    FList: TList;
    FOrientation: TPopupOrientation;
    procedure AssociateButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetAssociateButton(Value: TSpeedButton);
    function GetButton(Index: Integer): TSpeedButton;
    function GetButtonCount: Integer;
    procedure SetButtonCount(Value: Integer);
    procedure SetOrientation(Value: TPopupOrientation);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AdjustButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup; override;
    procedure Cancel; override;
    procedure Select; override;
    property Associate;
    property AssociateButton: TSpeedButton read FAssociateButton write
      SetAssociateButton;
    property Button[Index: Integer]: TSpeedButton read GetButton;
    property ButtonCount: Integer read GetButtonCount write SetButtonCount;
    property Orientation: TPopupOrientation read FOrientation write SetOrientation;
  end;

  TToolbarForm = class(TForm)
  private
    FActive: Boolean;
    FAssociate: TWinControl;
    FImages: TImagelist;
    FOnButtonClick: TNotifyEvent;
    procedure ButtonClick(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function AddPopup(Button: TSpeedButton): TPopupButtonForm;
    procedure ApplyImage(Button: TSpeedButton; Index: Integer);
    procedure Paint; override;
    procedure ResetFocus;
    property Images: TImageList read FImages write FImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    property Associate: TWinControl read FAssociate write FAssociate;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

implementation

{ TPopupButton }

procedure TPopupButton.Click;
begin
  (Owner as TPopupButtonForm).Hide;
  inherited Click;
end;

{ TPopupButtonForm }

constructor TPopupButtonForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sizeable := False;
  FList := TList.Create;
  FOrientation := poRight;
end;

destructor TPopupButtonForm.Destroy;
begin
  ButtonCount := 0;
  FList.Free;
  inherited Destroy;
end;

procedure TPopupButtonForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and (not WS_BORDER);
end;

procedure TPopupButtonForm.AdjustButtons;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TSpeedButton(FList[I]) do
      case FOrientation of
        poLeft, poRight:
          begin
            Top := 0;
            Left := I * Width;
          end;
        poTop, poBottom:
          begin
            Top := I * Height;
            Left := 0;
          end;
      end;
end;

procedure TPopupButtonForm.AssociateButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Popup;
end;

procedure TPopupButtonForm.Cancel;
begin
  Associate := nil;
end;

procedure TPopupButtonForm.Select;
begin
  Associate := nil;
end;

procedure TPopupButtonForm.Popup;
var
  Control: TWinControl;
  Button: TSpeedButton;
begin
  Control:= (Owner as TToolbarForm).FAssociate;
  if (Control <> nil) and Control.CanFocus and (FList.Count > 0) then
  begin
    Associate := Control;
    Associate.SetFocus;
    Button := TSpeedButton(FList[0]);
    with FAssociateButton, Parent.ClientToScreen(BoundsRect.TopLeft) do
      case FOrientation of
        poLeft:
          Self.SetBounds(x - (Button.Width * FList.Count + 1), y, Button.Width *
            FList.Count, Button.Height);
        poTop:
          Self.SetBounds(x, y - (Button.Height * FList.Count + 1), Button.Width,
            Button.Height * FList.Count);
        poRight:
          Self.SetBounds(x + Width + 1, y, Button.Width * FList.Count,
            Button.Height);
        poBottom:
          Self.SetBounds(x, y + Height + 1, Button.Width,
            Button.Height * FList.Count);
      end;
    Show;
  end;
end;

procedure TPopupButtonForm.SetAssociateButton(Value: TSpeedButton);
begin
  FAssociateButton := Value;
  FAssociateButton.OnMouseDown := AssociateButtonMouseDown;
end;

function TPopupButtonForm.GetButton(Index: Integer): TSpeedButton;
begin
  Result := TSpeedButton(FList[Index]);
end;

function TPopupButtonForm.GetButtonCount: Integer;
begin
  Result := FList.Count;
end;

procedure TPopupButtonForm.SetButtonCount(Value: Integer);
var
  Button: TSpeedButton;
  I: Integer;
begin
  if Value <> FList.Count then
    if Value < FList.Count then
      for I := FList.Count - 1 downto Value do
      begin
        TObject(FList[I]).Free;
        FList.Delete(I);
      end
    else
      for I := FList.Count to Value - 1 do
      begin
        Button := TPopupButton.Create(Self);
        Button.Parent := Self;
        Button.AllowAllUp := True;
        Button.GroupIndex := 1;
        FList.Add(Button);
        AdjustButtons;
      end;
end;

procedure TPopupButtonForm.SetOrientation(Value: TPopupOrientation);
begin
  if Value <> FOrientation then
  begin
    Cancel;
    FOrientation := Value;;
    AdjustButtons;
  end;
end;

{ TToolbarForm }

constructor TToolbarForm.Create(AOwner: TComponent);
var
  Rect: TRect;
  Control: TControl;
  I: Integer;
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  Font.Size := 8;
  Font.Color := clWindow;
  with Rect do
  begin
    Left := 100;
    Top := 100;
    Right := 0;
    Bottom := 0;
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      if Control.Left < Left then
        Left := Control.Left;
      if Control.Top < Top then
        Top := Control.Top;
      if Control.Left + Control.Width > Right then
        Right := Control.Left + Control.Width;
      if Control.Top + Control.Height > Bottom
        then Bottom := Control.Top + Control.Height;
    end;
    Height := Bottom + 4;
    Width := Left + Right;
  end;
end;

destructor TToolbarForm.Destroy;
begin
  Parent := nil;
  DestroyHandle;
  inherited Destroy;
end;

function TToolbarForm.AddPopup(Button: TSpeedButton): TPopupButtonForm;
begin
  Result := TPopupButtonForm.Create(Self);
  Result.AssociateButton := Button;
end;

procedure TToolbarForm.ApplyImage(Button: TSpeedButton; Index: Integer);
var
  Bitmap: TBitmap;
begin
  if FImages <> nil then
  begin
    Button.AllowAllUp := True;
    Button.GroupIndex := Index + 1;
    Button.Tag := Index;
    Button.OnClick := ButtonClick;
    Bitmap := TBitmap.Create;
    try
      FImages.GetBitmap(Index, Bitmap);
      Button.Glyph := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TToolbarForm.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Sender);
end;

procedure TToolbarForm.Paint;
var
  Rect: TRect;
  Mode: Integer;
begin
  Rect := ClientRect;
  DrawFrame(Canvas.Handle, Rect, dfFramed);
  InflateRect(Rect, -3, -3);
  Rect.Bottom := Rect.Top + 15;
  if FActive then
    Canvas.Brush.Color := clActiveCaption
  else
    Canvas.Brush.Color := clInactiveCaption;
  Canvas.FillRect(Rect);
  Mode := GetBkMode(Canvas.Handle);
  DrawText(Canvas.Handle, PChar(Caption), -1, Rect, DT_LEFT or DT_VCENTER or
    DT_SINGLELINE);
  SetBkMode(Canvas.Handle, Mode);
end;

procedure TToolbarForm.ResetFocus;
begin
  if (FAssociate <> nil) and FAssociate.CanFocus then
    FAssociate.SetFocus;
end;

procedure TToolbarForm.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Invalidate;
  end;
end;

procedure TToolbarForm.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  ResetFocus;
end;

procedure TToolbarForm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  ResetFocus;
end;

procedure TToolbarForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if ScreenToClient(SmallPointToPoint(Message.Pos)).Y < 18 then
    Message.Result := HTCAPTION
  else
    Message.Result := HTCLIENT;
end;

end.
