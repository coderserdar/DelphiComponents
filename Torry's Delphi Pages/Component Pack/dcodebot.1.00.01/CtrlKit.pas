
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(*******************************************************)

unit CtrlKit;

interface

{$I STD.INC}

uses
  Classes, Controls, Forms, Windows, Graphics, Math, Dialogs, Messages,
  ExtCtrls, StdCtrls, SysUtils, MathTools, GraphTools;

{ TProportionDirector }

type
  TProportionalAnchors = set of (paHorizontal, paVertical);

  TProportionDirector = class(TObject)
  private
    FBounds: TRect;
    FControl: TWinControl;
    FList: TList;
    FHandleAllocated: Boolean;
  protected
    function Find(Control: TControl): TObject;
  public
    constructor Create(WinControl: TWinControl);
    destructor Destroy; override;
    procedure UpdateControls;
    procedure Add(Control: TControl); overload;
    procedure Add(Control: TControl; CanSize: Boolean); overload;
    procedure Add(Control: TControl; Anchors: TProportionalAnchors); overload;
    procedure Remove(Control: TControl);
  end;

{ TCaretManager }

  TCaretStyle = (csDefault, csArrow, csUnderscore, csBlock);

  TCaretManager = class(TObject)
  private
    FCaret: TBitmap;
    FCaretStyle: TCaretStyle;
    FCaretWnd: HWND;
    FPriorEditProc: Pointer;
    procedure UpdateCaret(Wnd: HWND);
    procedure SetCaretStyle(const Value: TCaretStyle);
  public
    constructor Create;
    destructor Destroy; override;
    property Style: TCaretStyle read FCaretStyle write SetCaretStyle;
  end;

function CaretManager: TCaretManager;

{ The AdjustBounds procedure }

procedure AdjustBounds(Form: TCustomForm);

{ The IsControlClass function }

function IsControlClass(Control: TControl; ControlClass: array of TControlClass): Boolean;

{ The FindParentControl function }

function FindParentControl(Control: TControl; ControlClass: TWinControlClass): TWinControl;

{ The Check procedure }

procedure CheckControl(Control: TWinControl; Valid: Boolean; const Msg: string);

{ The ForEach procedure }

procedure ForEach(Owner: TComponent; ComponentClass: TComponentClass);

{ The NextEach function }

function NextEach(var Instance): Boolean;

{ The HiddenForm function returns a custom invisible form that can be used to
  hide child controls }

function HiddenForm: TCustomForm;

{ The MessageBtnDlg function }

function MessageBtnDlg(const Msg: string; DlgType: TMsgDlgType;
  const Buttons: array of string): Integer;

implementation

{ TBasicProportionObject }

type
  TBasicProportionObject = class(TObject)
  private
    FBounds: TRect;
    FControl: TControl;
  public
    constructor Create(Control: TControl);
    property Bounds: TRect read FBounds;
    property Control: TControl read FControl;
  end;

constructor TBasicProportionObject.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FBounds := FControl.BoundsRect;
end;

{ TSizeableProportionObject }

type
  TSizeableProportionObject = class(TBasicProportionObject)
  private
    FCanSize: Boolean;
  public
    constructor Create(Control: TControl; CanSize: Boolean);
    property CanSize: Boolean read FCanSize;
  end;

constructor TSizeableProportionObject.Create(Control: TControl; CanSize: Boolean);
begin
  inherited Create(Control);
  FCanSize := CanSize;
end;

{ TAnchoredProportionObject }

type
  TAnchoredProportionObject = class(TBasicProportionObject)
  private
    FAnchors: TProportionalAnchors;
  public
    constructor Create(Control: TControl; Anchors: TProportionalAnchors);
    property Anchors: TProportionalAnchors read FAnchors;
  end;

constructor TAnchoredProportionObject.Create(Control: TControl; Anchors: TProportionalAnchors);
begin
  inherited Create(Control);
  FAnchors := Anchors;
end;

{ TProportionDirector }

constructor TProportionDirector.Create(WinControl: TWinControl);
begin
  inherited Create;
  FList := TList.Create;
  FControl := WinControl;
  if FControl is TCustomForm then
  begin
    FBounds := FControl.ClientRect;
    FHandleAllocated := True;
  end
  else
  begin
    FHandleAllocated := FControl.HandleAllocated;
    if FHandleAllocated then
      FBounds := FControl.ClientRect;
  end;
end;

destructor TProportionDirector.Destroy;
var
  I: Integer;
begin
  inherited Destroy;
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Free;
end;

procedure TProportionDirector.Add(Control: TControl);
begin
  Add(Control, [paHorizontal]);
end;


procedure TProportionDirector.Add(Control: TControl; CanSize: Boolean);
begin
  if not FHandleAllocated then
  begin
    FBounds := FControl.ClientRect;
    FHandleAllocated := True;
  end;
  if (Control <> nil) and (Find(Control) = nil) then
    FList.Add(TSizeableProportionObject.Create(Control, CanSize));
end;


procedure TProportionDirector.Add(Control: TControl; Anchors: TProportionalAnchors);
begin
  if not FHandleAllocated then
  begin
    FBounds := FControl.ClientRect;
    FHandleAllocated := True;
  end;
  if (Control <> nil) and (Find(Control) = nil) then
    FList.Add(TAnchoredProportionObject.Create(Control, Anchors));
end;

function TProportionDirector.Find(Control: TControl): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FList.Count - 1 do
    if FList[I] = Control then
    begin
      Result := TObject(FList[I]);
      Break;
    end;
end;

procedure TProportionDirector.Remove(Control: TControl);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if FList[I] = Control then
    begin
      TObject(FList[I]).Free;
      FList.Delete(I);
      Break;
    end;
end;

procedure TProportionDirector.UpdateControls;
var
  Rect: TRect;
  HRatio, VRatio: Double;
  Instance: TbasicProportionObject;
  I: Integer;
begin
  if not FHandleAllocated then Exit;
  Rect := FControl.ClientRect;
  HRatio := WidthOf(Rect) / WidthOf(FBounds);
  VRatio := HeightOf(Rect) / HeightOf(FBounds);
  for I := 0 to FList.Count - 1 do
  begin
    Instance := TBasicProportionObject(FList[I]);
    if Instance is TSizeableProportionObject then
      with Instance as TSizeableProportionObject, Control, Bounds do
        if CanSize then
          SetBounds(Round(Left * HRatio), Top, Round(WidthOf(Bounds) * HRatio),
            HeightOf(Bounds))
        else
          SetBounds(Round(Left * HRatio), Top, WidthOf(Bounds), HeightOf(Bounds));
    if Instance is TAnchoredProportionObject then
      with Instance as TAnchoredProportionObject, Control, Bounds do
        if Anchors = [] then
          Continue
        else if TAnchoredProportionObject(Instance).Anchors = [paHorizontal] then
          SetBounds(Round(Left * HRatio), Top, Round(WidthOf(Bounds) * HRatio),
            HeightOf(Bounds))
        else if TAnchoredProportionObject(Instance).Anchors = [paVertical] then
          SetBounds(Left, Round(Top * VRatio), WidthOf(Bounds),
            Round(HeightOf(Bounds) * VRatio))
        else
          SetBounds(Round(Left * HRatio), Round(Top * VRatio),
            Round(WidthOf(Bounds) * HRatio),  Round(HeightOf(Bounds) * VRatio));
  end;
end;

{ TCaretManager }

var
  InternalCaretManager: TObject;

function CaretManager: TCaretManager;
begin
  if InternalCaretManager = nil then
    InternalCaretManager := TCaretManager.Create;
  Result := TCaretManager(InternalCaretManager);
end;

function EditProc(Wnd: HWND; Msg: Cardinal; wParam: Integer; lParam: Integer): Integer; stdcall;
begin
  Result := CallWindowProc(CaretManager.FPriorEditProc, Wnd, Msg, wParam, lParam);
  case Msg of
    WM_SETFOCUS:
      CaretManager.UpdateCaret(Wnd);
    WM_KILLFOCUS:
      CaretManager.UpdateCaret(0);
  end;
end;

constructor TCaretManager.Create;
var
  Wnd: HWND;
begin
  inherited Create;
  if InternalCaretManager <> nil then
    raise Exception.Create('Only instance of caret manager allowed');
  FCaret := TBitmap.Create;
  Wnd := CreateWindow('EDIT', nil, 0, 0, 0, 0, 0, 0, 0, 0, nil);
  Cardinal(FPriorEditProc) := GetClassLong(Wnd, GCL_WNDPROC);
  SetClassLong(Wnd, GCL_WNDPROC, Cardinal(@EditProc));
  DestroyWindow(Wnd);
end;

destructor TCaretManager.Destroy;
var
  Wnd: HWND;
begin
  Wnd := CreateWindow('EDIT', nil, 0, 0, 0, 0, 0, 0, 0, 0, nil);
  SetClassLong(Wnd, GCL_WNDPROC, Cardinal(FPriorEditProc));
  DestroyWindow(Wnd);
  FCaret.Free;
  InternalCaretManager := nil;
  inherited Destroy;
end;

procedure TCaretManager.UpdateCaret(Wnd: HWND);

  procedure BuildArrowCaret(Width, Height: Integer);
  var
    Polygon: TPolygon;
  begin
    FCaret.Width := Height div 2;
    if Odd(Height) then
      Dec(Height);
    FCaret.Height := Height;
    FCaret.Canvas.Brush.Color := clBlack;
    FCaret.Canvas.FillRect(Rect(0, 0, Height div 2, Height));
    FCaret.Canvas.Brush.Color := clWhite;
    SetLength(Polygon, 3);
    Polygon[0] := Point(0, 0);
    Polygon[1] := Point(Height div 2, Height div 2);
    Polygon[2] := Point(0, Height);
    DrawPolygon(FCaret.Canvas.Handle, Polygon);
  end;

  procedure BuildUnderscoreCaret(Width, Height: Integer);
  begin
    FCaret.Width := Width;
    FCaret.Height := Height;
    FCaret.Canvas.Brush.Color := clWhite;
    FCaret.Canvas.FillRect(Rect(0, 0, Width, Height));
    FCaret.Canvas.Brush.Color := clBlack;
    FCaret.Canvas.FillRect(Rect(0, 0, Width, Height - 3));
  end;

  procedure BuildBlockCaret(Width, Height: Integer);
  begin
    FCaret.Width := Height div 2;
    if Odd(Height) then
      Dec(Height);
    FCaret.Height := Height;
    FCaret.Canvas.Brush.Color := clWhite;
    FCaret.Canvas.FillRect(Rect(0, 0, Height, Height));
  end;

var
  DC: HDC;
  PriorFont: HFONT;
begin
  FCaretWnd := Wnd;
  if (FCaretWnd = 0) or (FCaretStyle = csDefault) then Exit;
  DC := GetDC(Wnd);
  PriorFont := SelectObject(DC, SendMessage(Wnd, WM_GETFONT, 0, 0));
  with CalculateCaptionSize(DC, 'T') do
    case FCaretStyle of
      csArrow:
        BuildArrowCaret(cx, cy);
      csUnderscore:
        BuildUnderscoreCaret(cx, cy);
      csBlock:
        BuildBlockCaret(cx, cy);
    end;
  SelectObject(DC, PriorFont);
  CreateCaret(Wnd, FCaret.Handle, 0, 0);
  ShowCaret(Wnd);
  ReleaseDC(Wnd, DC);
end;

procedure TCaretManager.SetCaretStyle(const Value: TCaretStyle);
begin
  if Value <> FCaretStyle then
  begin
    FCaretStyle := Value;
    if FCaretWnd <> 0 then
      UpdateCaret(FCaretWnd);
  end;
end;

procedure AdjustBounds(Form: TCustomForm);
var
  X, Y: Integer;
  I: Integer;
begin
  X := 0;
  Y := 0;
  for I := 0 to Form.ControlCount - 1 do
    with Form.Controls[I] do
    begin
      X := Max(X, Left + Width + 8);
      Y := Max(Y, Top + Height + 8);
    end;
  Form.ClientWidth := X;
  Form.ClientHeight := Y;
end;

function IsControlClass(Control: TControl; ControlClass: array of TControlClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(ControlClass) to High(ControlClass) do
    if Control is ControlClass[I] then
    begin
      Result := True;
      Break;
    end;
end;

function FindParentControl(Control: TControl; ControlClass: TWinControlClass): TWinControl;
begin
  if Control <> nil then
    repeat
      Control := Control.Parent;
    until Control is TWinControlClass;
  if Control <> nil then
    Result := nil
  else
    Result := TWinControl(Control);
end;

procedure CheckControl(Control: TWinControl; Valid: Boolean; const Msg: string);
begin
  if Control.Enabled and Control.Visible and (not Valid) then
  begin
    MessageDlg(Msg, mtError, [mbOK], 0);
    Control.SetFocus;
    Abort;
  end;
end;

var
  ItterationStruct: record
    Owner: TComponent;
    ComponentClass: TComponentClass;
    Index: Integer;
  end;

procedure ForEach(Owner: TComponent; ComponentClass: TComponentClass);
begin
  ItterationStruct.Owner := Owner;
  ItterationStruct.ComponentClass := ComponentClass;
  ItterationStruct.Index := 0;
end;

function NextEach(var Instance): Boolean;
var
  Component: TComponent absolute Instance;
  I: Integer;
begin
  Result := ItterationStruct.Owner <> nil;
  if Result then
    with ItterationStruct do
    begin
      Result := False;
      I := Owner.ComponentCount;
      while I > Index do
      begin
        Component := Owner.Components[Index];
        Inc(Index);
        Result := Component is ComponentClass;
        if Result then
          Break;
      end;
    end;
  if not Result then
    Component := nil;
end;

var
  InternalHiddenForm: TObject;

function HiddenForm: TCustomForm;
begin
  if InternalHiddenForm = nil then
    InternalHiddenForm := TCustomForm.CreateNew(nil, 0);
  Result := TCustomForm(InternalHiddenForm);
end;

type
  TButtonDialog = class(TForm)
  private
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;

constructor TButtonDialog.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner, Dummy);
  BorderStyle := bsDialog;
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

procedure TButtonDialog.DoShow;
begin
  inherited DoShow;
  SetTimer(Handle, 1, 1, nil);
end;

procedure TButtonDialog.WMTimer(var Message: TWMtimer);
begin
  KillTimer(Handle, Message.TimerID);
  ActiveControl := nil;
  Windows.SetFocus(Handle);
end;

function CreateButtonDialog(const Msg: string; const Caption: string;
  ImageResource: PChar; Buttons: array of string): TForm;
const
  Margin = 12;
var
  DC: HDC;
  Rect: TRect;
  Widths: array of Integer;
  FormWidth: Integer;
  FormHeight: Integer;
  GroupWidth: Integer;
  GroupLeft: Integer;
  I: Integer;
begin
  Result := nil;
  if Length(Buttons) = 0 then Exit;
  Result := TButtonDialog.CreateNew(Application);
  Result.Caption := Caption;
  DC := Result.Canvas.Handle;
  FormWidth := 32 + Margin * 2 + 12 * (Length(Buttons) - 1);
  SetLength(Widths, Length(Buttons));
  for I := Low(Buttons) to High(Buttons) do
  begin
    FillChar(Rect, SizeOf(Rect), #0);
    DrawText(DC, PChar(Buttons[I]), -1, Rect, DT_LEFT or DT_SINGLELINE or
      DT_CALCRECT);
    Widths[I] := 75;
    with Rect do
      if Right - Left + 12 > Widths[I] then
        Widths[I] := Right - Left + 12;
    FormWidth := FormWidth + Widths[I];
  end;
  if ImageResource <> nil then
    with TImage.Create(Result) do
    begin
      Name := 'Image';
      Parent := Result;
      Picture.Icon.Handle := LoadIcon(0, ImageResource);
      SetBounds(Margin, Margin, 32, 32);
    end;
  with TLabel.Create(Result) do
  begin
    Name := 'Message';
    Parent := Result;
    AutoSize := False;
    WordWrap := True;
    with Rect do
    begin
      Left := Margin * 2 + 32;
      Right := Left;
      Top := Margin;
      Bottom := Top;
      Bottom := DrawText(DC, PChar(Msg), -1, Rect, DT_LEFT or DT_CALCRECT);
      FormHeight := Bottom + Margin * 5 + 25;
      if FormWidth < 32 + Margin * 3 + Right - Left then
        FormWidth := 32 + Margin * 3 + Right - Left;
      SetBounds(Left, Top, Right - Left, Bottom);
    end;
    Caption := Msg;
  end;
  GroupWidth := 0;
  for I := Low(Buttons) to High(Buttons) do
    GroupWidth := GroupWidth + Widths[I] + 12;
  Dec(GroupWidth, 12);
  GroupLeft := (FormWidth - GroupWidth) div 2;
  for I := Low(Buttons) to High(Buttons) do
    with TButton.Create(Result) do
    begin
      Name := 'Button' + IntToStr(I + 1);
      Parent := Result;
      Caption := Buttons[I];
      ModalResult := I + 1;
      SetBounds(GroupLeft, FormHeight - (Margin + 25), Widths[I], 25);
      Inc(GroupLeft, Widths[I] + 12);
    end;
  with Result do
  begin
    ClientWidth := FormWidth;
    ClientHeight := FormHeight;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
  end;
end;

function MessageBtnDlg(const Msg: string; DlgType: TMsgDlgType;
  const Buttons: array of string): Integer;
const
  Captions: array[TMsgDlgType] of string = ('Warning', 'Error', 'Information',
    'Confirm', 'Question');
  Resources: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, IDI_QUESTION);
var
  Form: TForm;
begin
  Form := CreateButtonDialog(Msg, Captions[DlgType], Resources[DlgType], Buttons);
  if Form <> nil then
  try
    Result := Form.ShowModal;
  finally
    Form.Free;
  end
  else
    Result := 0;
end;

initialization
  InternalCaretManager := nil;
  InternalHiddenForm := nil;
finalization
  if InternalHiddenForm <> nil then
    TCustomForm(InternalHiddenForm).Release;
  InternalCaretManager.Free;
end.

