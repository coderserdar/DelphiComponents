//---------------------------------------------------------------------------
//  TVolgaCalculator - calculator form
//  Some code is borrow from RX calculator
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
unit VolCalc;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, Menus, ExtCtrls, Buttons, Mask, VolDBConst;

type

{ TVolgaCalculatorForm }

  TVolgaCalcState = (csFirst, csValid, csError);
  TCalcExitEvent = procedure(Sender: TObject; Selected: Boolean) of object;

  TVolgaCalculator = class(TCustomPanel)
  private
    PanelMemory: TLabel;
    BackButton: TSpeedButton;
    ButtonC: TSpeedButton;
    ButtonMC: TSpeedButton;
    ButtonMR: TSpeedButton;
    ButtonMS: TSpeedButton;
    ButtonMP: TSpeedButton;
    Button9: TSpeedButton;
    Button6: TSpeedButton;
    Button3: TSpeedButton;
    Button7: TSpeedButton;
    Button4: TSpeedButton;
    Button1: TSpeedButton;
    Button0: TSpeedButton;
    Button8: TSpeedButton;
    Button5: TSpeedButton;
    Button2: TSpeedButton;
    ButtonPM: TSpeedButton;
    ButtonPnt: TSpeedButton;
    ButtonDiv: TSpeedButton;
    ButtonMul: TSpeedButton;
    ButtonSub: TSpeedButton;
    ButtonAdd: TSpeedButton;
    ButtonSqrt: TSpeedButton;
    ButtonPercent: TSpeedButton;
    ButtonRev: TSpeedButton;
    ButtonResult: TSpeedButton;
    OkButton: TSpeedButton;
    CancelButton: TSpeedButton;
    PanelFract: TLabel;
    ButtonUp: TSpeedButton;
    ButtonDown: TSpeedButton;
    FExitClick: TCalcExitEvent;
    procedure NumButtonClick(Sender: TObject);
    procedure ButtonPntClick(Sender: TObject);
    procedure ButtonDivClick(Sender: TObject);
    procedure ButtonMulClick(Sender: TObject);
    procedure ButtonSubClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonResultClick(Sender: TObject);
    procedure ButtonCClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ButtonPMClick(Sender: TObject);
    procedure ButtonRevClick(Sender: TObject);
    procedure ButtonPercentClick(Sender: TObject);
    procedure ButtonSqrtClick(Sender: TObject);
    procedure ButtonMCClick(Sender: TObject);
    procedure ButtonMRClick(Sender: TObject);
    procedure ButtonMSClick(Sender: TObject);
    procedure ButtonMPClick(Sender: TObject);
    procedure CheckFirst;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
  private
    FStatus: TVolgaCalcState;
    FOperator: Char;
    FOperand: double;
    FMemory: double;
    FOldValue: double;
    procedure CalcKey(Key: Char);
    procedure Clear;
    procedure Error;
    function GetControl: TCustomMaskEdit;
    procedure SetOldValue(const Value: double);
    procedure SetValue(const Value: double);
    function GetValue: double;
    function GetStrValue: string;
    procedure SetStrValue(const Value: string);
    procedure PrepareControls;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MainCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property OldValue: double read FOldValue write SetOldValue;
    property StrValue: string read GetStrValue write SetStrValue;
    property Value: double read GetValue write SetValue;
    property Control: TCustomMaskEdit read GetControl;
  published
    property BevelInner;
    property BevelOuter;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnExitClick: TCalcExitEvent read FExitClick write FExitClick;
  end;

implementation
{$R VOLCALC.RES}

const
  VK_0 = $30;
  VK_1 = $31;
  VK_2 = $32;
  VK_3 = $33;
  VK_4 = $34;
  VK_5 = $35;
  VK_6 = $36;
  VK_7 = $37;
  VK_8 = $38;
  VK_9 = $39;

{ TVolgaCalculator }

constructor TVolgaCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption, csDoubleClicks];
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  Width := 173;
  Height := 116;
  BevelInner := bvLowered;
  Font.Color := clBlack;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';
  Font.Style := [fsBold];
  OnCanResize := MainCanResize;
  ShowHint := True;
  PrepareControls;
  Invalidate;
  Clear;
  FOldValue := 0;
  FMemory := 0.0;
end;

procedure TVolgaCalculator.Clear;
begin
  FStatus := csFirst;
  Value := 0;
  FOperator := '=';
end;

procedure TVolgaCalculator.Error;
begin
  FStatus := csError;
  MessageBeep(MB_OK);
end;

procedure TVolgaCalculator.SetOldValue(const Value: double);
begin
  FOldValue := Value;
end;

function TVolgaCalculator.GetValue: double;
begin
  if FStatus = csError then Result := 0
  else
  try
    Result := StrToFloat(StrValue);
  except Result := 0; end;
end;

procedure TVolgaCalculator.SetValue(const Value: double);
begin
  StrValue := FormatFloat('0.' + StringOfChar('#', StrToInt(PanelFract.Caption)), Value);
end;

function TVolgaCalculator.GetStrValue: string;
begin
  if Control <> nil then
    Result := Control.Text
  else Result := '';
end;

procedure TVolgaCalculator.SetStrValue(const Value: string);
var S: string;
begin
  S := Trim(Value);
  if Control <> nil then Control.Text := Trim(S);
end;

procedure TVolgaCalculator.CheckFirst;
begin
  if FStatus = csFirst then begin
    FStatus := csValid;
    Value := 0;
  end;
end;

procedure TVolgaCalculator.CalcKey(Key: Char);
var
  R: double;
begin
  Key := UpCase(Key);
  if (FStatus = csError) and (Key <> 'C') then Key := ' ';
  if Key = DecimalSeparator then begin
    CheckFirst;
    if (Pos(DecimalSeparator, StrValue) = 0) then
      StrValue := StrValue + DecimalSeparator;
  end else
    case Key of
      '0'..'9': begin
          CheckFirst;
          if (Control.SelLength = Length(Control.Text)) or (StrValue = '0') then
             StrValue := '';
          if (Pos('E', StrValue) = 0) then
            StrValue := StrValue + Key;
        end;
      #8: begin
          CheckFirst;
          if (Length(StrValue) = 1) or ((Length(StrValue) = 2) and
            (StrValue[1] = '-')) then
            StrValue := '0'
          else
            StrValue := Copy(StrValue, 1, Length(StrValue) - 1);
        end;
      '_': Value := -Value;
      '+', '-', '*', '/', '=', '%', #13: begin
          if FStatus = csValid then begin
            FStatus := csFirst;
            R := Value;
            if Key = '%' then
              case FOperator of
                '+', '-': R := FOperand * R / 100;
                '*', '/': R := R / 100;
              end;
            case FOperator of
              '+': Value := FOperand + R;
              '-': Value := FOperand - R;
              '*': Value := FOperand * R;
              '/': if R = 0 then Error else Value := FOperand / R;
            end;
          end;
          FOperator := Key;
          FOperand := Value;
        end;
      'C': Clear;
    end;
end;

procedure TVolgaCalculator.KeyDown(var Key: Word; Shift: TShiftState);
var akey: Word;
begin
  akey := Key;
  Key := 0;
  case akey of
    VK_0..VK_9: CalcKey(Char(akey));
    VK_NUMPAD0..VK_NUMPAD9: CalcKey(Char(akey - VK_NUMPAD0 + Ord('0')));
    VK_BACK: BackButtonClick(Self);
    67: ButtonCClick(Self);  //c
    187: ButtonResultClick(Self);
    VK_RETURN: OkButtonClick(Self);
    VK_ESCAPE: CancelButtonClick(Self);
    VK_DIVIDE: ButtonDivClick(Self);
    VK_MULTIPLY: ButtonMulClick(Self);
    VK_SUBTRACT: ButtonSubClick(Self);
    VK_ADD: ButtonAddClick(Self);
    188, 190, VK_DECIMAL: ButtonPntClick(Self);
  else Error;
  end;
end;

procedure TVolgaCalculator.NumButtonClick(Sender: TObject);
begin
  CalcKey(Char(TComponent(Sender).Tag + Ord('0')));
end;

procedure TVolgaCalculator.ButtonPntClick(Sender: TObject);
begin
  CalcKey(DecimalSeparator);
end;

procedure TVolgaCalculator.ButtonDivClick(Sender: TObject);
begin
  CalcKey('/');
end;

procedure TVolgaCalculator.ButtonMulClick(Sender: TObject);
begin
  CalcKey('*');
end;

procedure TVolgaCalculator.ButtonSubClick(Sender: TObject);
begin
  CalcKey('-');
end;

procedure TVolgaCalculator.ButtonAddClick(Sender: TObject);
begin
  CalcKey('+');
end;

procedure TVolgaCalculator.ButtonResultClick(Sender: TObject);
begin
  CalcKey('=');
end;

procedure TVolgaCalculator.ButtonCClick(Sender: TObject);
begin
  CalcKey('C');
end;

procedure TVolgaCalculator.BackButtonClick(Sender: TObject);
begin
  CalcKey(#8);
end;

procedure TVolgaCalculator.ButtonPMClick(Sender: TObject);
begin
  CalcKey('_');
end;

procedure TVolgaCalculator.ButtonPercentClick(Sender: TObject);
begin
  CalcKey('%');
end;

procedure TVolgaCalculator.ButtonRevClick(Sender: TObject);
begin
  FStatus := csFirst;
  if Value = 0 then Error else Value := 1.0 / Value;
end;

procedure TVolgaCalculator.ButtonSqrtClick(Sender: TObject);
begin
  FStatus := csFirst;
  if Value < 0 then Error else Value := Sqrt(Value);
end;

procedure TVolgaCalculator.ButtonMCClick(Sender: TObject);
begin
  FMemory := 0.0;
  PanelMemory.Caption := '';
end;

procedure TVolgaCalculator.ButtonMRClick(Sender: TObject);
begin
  if (FStatus = csValid) or (FStatus = csFirst) then begin
    FStatus := csFirst;
    CheckFirst;
    Value := FMemory;
  end;
end;

procedure TVolgaCalculator.ButtonMSClick(Sender: TObject);
begin
  if (FStatus = csValid) or (FStatus = csFirst) then begin
    FStatus := csFirst;
    FMemory := Value;
    if FMemory <> 0 then PanelMemory.Caption := 'M'
    else PanelMemory.Caption := '';
  end;
end;

procedure TVolgaCalculator.ButtonMPClick(Sender: TObject);
begin
  if (FStatus = csValid) or (FStatus = csFirst) then begin
    FStatus := csFirst;
    FMemory := FMemory + Value;
    if FMemory <> 0 then PanelMemory.Caption := 'M'
    else PanelMemory.Caption := '';
  end;
end;

procedure TVolgaCalculator.OkButtonClick(Sender: TObject);
begin
  StrToFloat(Trim(StrValue)); { to raise exception on error }
  if Assigned(FExitClick) then FExitClick(Self, true);
end;

procedure TVolgaCalculator.CancelButtonClick(Sender: TObject);
begin
  Value := OldValue; //вернуть старое значение
  if Assigned(FExitClick) then FExitClick(Self, false);
end;

procedure TVolgaCalculator.ButtonUpClick(Sender: TObject);
begin
  if PanelFract.Caption < '8' then
    PanelFract.Caption := IntToStr(StrToInt(PanelFract.Caption) + 1);
end;

procedure TVolgaCalculator.ButtonDownClick(Sender: TObject);
begin
  if PanelFract.Caption > '0' then
    PanelFract.Caption := IntToStr(StrToInt(PanelFract.Caption) - 1);
end;

function TVolgaCalculator.GetControl: TCustomMaskEdit;
begin
  if Owner is TCustomMaskEdit then
    Result := TCustomMaskEdit(Owner)
  else Result := nil;
end;

procedure TVolgaCalculator.PrepareControls;
begin
  PanelMemory := TLabel.Create(Self);
  PanelMemory.Parent := Self;
  PanelMemory.SetBounds(9, 7, 20, 21);
  PanelMemory.Caption := '';

  BackButton := TSpeedButton.Create(Self);
  BackButton.Parent := Self;
  BackButton.SetBounds(30, 2, 28, 22);
  BackButton.Hint := 'Delete the digit';
  BackButton.Caption := '<=';
  BackButton.Font.Color := clMaroon;
  BackButton.Font.Height := -13;
  BackButton.OnClick := BackButtonClick;

  ButtonC := TSpeedButton.Create(Self);
  ButtonC.Parent := Self;
  ButtonC.SetBounds(58, 2, 28, 22);
  ButtonC.Hint := 'Clear';
  ButtonC.Caption := 'C';
  ButtonC.Font.Color := clMaroon;
  ButtonC.Font.Height := -16;
  ButtonC.OnClick := ButtonCClick;

  ButtonMC := TSpeedButton.Create(Self);
  ButtonMC.Parent := Self;
  ButtonMC.SetBounds(2, 24, 28, 22);
  ButtonMC.Hint := 'Clear memory';
  ButtonMC.Caption := 'MC';
  ButtonMC.Font.Color := clNavy;
  ButtonMC.Font.Height := -11;
  ButtonMC.OnClick := ButtonMCClick;

  ButtonMR := TSpeedButton.Create(Self);
  ButtonMR.Parent := Self;
  ButtonMR.SetBounds(2, 46, 28, 22);
  ButtonMR.Hint := 'Read memory';
  ButtonMR.Caption := 'MR';
  ButtonMR.Font.Color := clNavy;
  ButtonMR.Font.Height := -11;
  ButtonMR.OnClick := ButtonMRClick;

  ButtonMS := TSpeedButton.Create(Self);
  ButtonMS.Parent := Self;
  ButtonMS.SetBounds(2, 68, 28, 22);
  ButtonMS.Hint := 'Write to memory';
  ButtonMS.Caption := 'MS';
  ButtonMS.Font.Color := clNavy;
  ButtonMS.Font.Height := -11;
  ButtonMS.OnClick := ButtonMSClick;

  ButtonMP := TSpeedButton.Create(Self);
  ButtonMP.Parent := Self;
  ButtonMP.SetBounds(2, 90, 28, 22);
  ButtonMP.Hint := 'Add in memory';
  ButtonMP.Caption := 'M+';
  ButtonMP.Font.Color := clNavy;
  ButtonMP.Font.Height := -11;
  ButtonMP.OnClick := ButtonMPClick;

  Button0 := TSpeedButton.Create(Self);
  Button0.Parent := Self;
  Button0.SetBounds(30, 90, 28, 22);
  Button0.Caption := '0';
  Button0.Font.Height := -16;
  Button0.Tag := 0;
  Button0.OnClick := NumButtonClick;

  Button1 := TSpeedButton.Create(Self);
  Button1.Parent := Self;
  Button1.SetBounds(30, 68, 28, 22);
  Button1.Caption := '1';
  Button1.Font.Height := -16;
  Button1.Tag := 1;
  Button1.OnClick := NumButtonClick;

  Button2 := TSpeedButton.Create(Self);
  Button2.Parent := Self;
  Button2.SetBounds(58, 68, 28, 22);
  Button2.Caption := '2';
  Button2.Font.Height := -16;
  Button2.Tag := 2;
  Button2.OnClick := NumButtonClick;

  Button3 := TSpeedButton.Create(Self);
  Button3.Parent := Self;
  Button3.SetBounds(86, 68, 28, 22);
  Button3.Caption := '3';
  Button3.Font.Height := -16;
  Button3.Tag := 3;
  Button3.OnClick := NumButtonClick;

  Button4 := TSpeedButton.Create(Self);
  Button4.Parent := Self;
  Button4.SetBounds(30, 46, 28, 22);
  Button4.Caption := '4';
  Button4.Font.Height := -16;
  Button4.Tag := 4;
  Button4.OnClick := NumButtonClick;

  Button5 := TSpeedButton.Create(Self);
  Button5.Parent := Self;
  Button5.SetBounds(58, 46, 28, 22);
  Button5.Caption := '5';
  Button5.Font.Height := -16;
  Button5.Tag := 5;
  Button5.OnClick := NumButtonClick;

  Button6 := TSpeedButton.Create(Self);
  Button6.Parent := Self;
  Button6.SetBounds(86, 46, 28, 22);
  Button6.Caption := '6';
  Button6.Font.Height := -16;
  Button6.Tag := 6;
  Button6.OnClick := NumButtonClick;

  Button7 := TSpeedButton.Create(Self);
  Button7.Parent := Self;
  Button7.SetBounds(30, 24, 28, 22);
  Button7.Caption := '7';
  Button7.Font.Height := -16;
  Button7.Tag := 7;
  Button7.OnClick := NumButtonClick;

  Button8 := TSpeedButton.Create(Self);
  Button8.Parent := Self;
  Button8.SetBounds(58, 24, 28, 22);
  Button8.Caption := '8';
  Button8.Font.Height := -16;
  Button8.Tag := 8;
  Button8.OnClick := NumButtonClick;

  Button9 := TSpeedButton.Create(Self);
  Button9.Parent := Self;
  Button9.SetBounds(86, 24, 28, 22);
  Button9.Caption := '9';
  Button9.Font.Height := -16;
  Button9.Tag := 9;
  Button9.OnClick := NumButtonClick;

  ButtonPM := TSpeedButton.Create(Self);
  ButtonPM.Parent := Self;
  ButtonPM.SetBounds(58, 90, 28, 22);
  ButtonPM.Caption := '+/-';
  ButtonPM.Font.Height := -16;
  ButtonPM.OnClick := ButtonPMClick;

  ButtonPnt := TSpeedButton.Create(Self);
  ButtonPnt.Parent := Self;
  ButtonPnt.SetBounds(86, 90, 28, 22);
  ButtonPnt.Caption := ',';
  ButtonPnt.Font.Height := -19;
  ButtonPnt.OnClick := ButtonPntClick;

  ButtonDiv := TSpeedButton.Create(Self);
  ButtonDiv.Parent := Self;
  ButtonDiv.SetBounds(114, 24, 28, 22);
  ButtonDiv.Caption := '/';
  ButtonDiv.Font.Height := -16;
  ButtonDiv.OnClick := ButtonDivClick;

  ButtonMul := TSpeedButton.Create(Self);
  ButtonMul.Parent := Self;
  ButtonMul.SetBounds(114, 46, 28, 22);
  ButtonMul.Caption := '*';
  ButtonMul.Font.Height := -19;
  ButtonMul.OnClick := ButtonMulClick;

  ButtonSub := TSpeedButton.Create(Self);
  ButtonSub.Parent := Self;
  ButtonSub.SetBounds(114, 68, 28, 22);
  ButtonSub.Caption := '-';
  ButtonSub.Font.Height := -19;
  ButtonSub.OnClick := ButtonSubClick;

  ButtonAdd := TSpeedButton.Create(Self);
  ButtonAdd.Parent := Self;
  ButtonAdd.SetBounds(114, 90, 28, 22);
  ButtonAdd.Caption := '+';
  ButtonAdd.Font.Height := -19;
  ButtonAdd.OnClick := ButtonAddClick;

  ButtonSqrt := TSpeedButton.Create(Self);
  ButtonSqrt.Parent := Self;
  ButtonSqrt.SetBounds(142, 24, 28, 22);
  ButtonSqrt.Caption := 'sqrt';
  ButtonSqrt.Font.Height := -13;
  ButtonSqrt.OnClick := ButtonSqrtClick;

  ButtonPercent := TSpeedButton.Create(Self);
  ButtonPercent.Parent := Self;
  ButtonPercent.SetBounds(142, 46, 28, 22);
  ButtonPercent.Caption := '%';
  ButtonPercent.Font.Height := -16;
  ButtonPercent.OnClick := ButtonPercentClick;

  ButtonRev := TSpeedButton.Create(Self);
  ButtonRev.Parent := Self;
  ButtonRev.SetBounds(142, 68, 28, 22);
  ButtonRev.Caption := '1/x';
  ButtonRev.Font.Height := -16;
  ButtonRev.OnClick := ButtonRevClick;

  ButtonResult := TSpeedButton.Create(Self);
  ButtonResult.Parent := Self;
  ButtonResult.SetBounds(142, 90, 28, 22);
  ButtonResult.Caption := '=';
  ButtonResult.Font.Color := clRed;
  ButtonResult.Font.Height := -19;
  ButtonResult.OnClick := ButtonResultClick;

  OkButton := TSpeedButton.Create(Self);
  OkButton.Parent := Self;
  OkButton.SetBounds(114, 2, 28, 22);
  OkButton.Hint := V_OKBTNHINT;
  OkButton.Glyph.LoadFromResourceName(HInstance, 'CALCOKBTN');
  OkButton.OnClick := OkButtonClick;

  CancelButton := TSpeedButton.Create(Self);
  CancelButton.Parent := Self;
  CancelButton.SetBounds(142, 2, 28, 22);
  CancelButton.Hint := V_CANCELBTNHINT;
  CancelButton.Glyph.LoadFromResourceName(HInstance, 'CALCCANCELBTN');
  CancelButton.OnClick := CancelButtonClick;

  PanelFract := TLabel.Create(Self);
  PanelFract.Parent := Self;
  PanelFract.SetBounds(87, 7, 14, 22);
  PanelFract.Hint := V_PANELFRACT;
  PanelFract.Caption := '2';

  ButtonUp := TSpeedButton.Create(Self);
  ButtonUp.Parent := Self;
  ButtonUp.SetBounds(98, 2, 15, 12);
  ButtonUp.Glyph.LoadFromResourceName(HInstance, 'CALCUPBTN');
  ButtonUp.Hint := V_BTNUPHINT;
  ButtonUp.OnClick := ButtonUpClick;

  ButtonDown := TSpeedButton.Create(Self);
  ButtonDown.Parent := Self;
  ButtonDown.SetBounds(98, 12, 15, 12);
  ButtonDown.Glyph.LoadFromResourceName(HInstance, 'CALCDOWNBTN');
  ButtonDown.Hint := V_BTNDOWNHINT;
  ButtonDown.OnClick := ButtonDownClick;
end;

procedure TVolgaCalculator.MainCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := false;
end;

procedure TVolgaCalculator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  AWidth := 173;
  AHeight := 116;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TVolgaCalculator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TVolgaCalculator.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

end.

