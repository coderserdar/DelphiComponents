{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_calculator;

interface
{$I psc_defines.inc}

Uses
  sysutils,
  Types,
  winapi.Windows,
  winapi.messages,
  forms,
  controls,
  classes,

  myla_system,
  myla_interfaces,

  psc_edit,
  psc_procs,
  psc_wrapper,
  psc_const;

{-------------------------------------------------------------------------}

Const
  CPSCWinCalcDisplayBorderColor=$00B99D7F;
  CPSCDefCalcShowDisplay=True;
  CPSCDefCalcNumbersColor=clPSCBlue;
  CPSCDefCalcActionsColor=clPSCRed;
  cPSCDefCalcBtnBevelWidth=1;
  cPSCDefCalcBtnRounded=False;
  cPSCDefCalcBtnLongDist=10;
  cPSCDefCalcBtnDist=5;
  CPSCCalcDisplayBackColor=clPSCWindow;
  CPSCCalcDisplayBorderColor=clPSCBtnShadow;
  CPSCDefCalcFlat=True;
  cSPSCDefCalcDispValue = '0';
  PSCDefaultCalcPrecision = 13;
  cPSCDefCalcBorderStyle = bsNone;

Type
  TPSCCalcButtonKind = (
    cbkNone,cbkBack,cbkCE,cbkC,
    cbkMC,cbk7,cbk8,cbk9,cbkDiv,cbkSqrt,
    cbkMR,cbk4,cbk5,cbk6,cbkMul,cbkPercent,
    cbkMS,cbk1,cbk2,cbk3,cbkMin,cbkRev,
    cbkMPlus,cbk0,cbkNeg,cbkDot,cbkPlus,cbkEqual,

    cbkSta,cbkAve,cbkSum,cbkS,cbkDat,cbkF_E,cbkdms,cbksin,
    cbkcos,cbktan,cbkOpenBracket,cbkCloseBracket,cbkExp,
    cbkln,cbkxPowY,cbklog,cbkxPow3,cbkxPow2,cbknFactorial,
    cbkpi,cbkA,cbkB,cbkCDigit,cbkD,cbkE,cbkF,cbkMod,cbkOr,
    cbkLsh,cbkAnd,cbkXor,cbkNot,cbkInt
  );

  TPSCCalcButtonKinds=set of TPSCCalcButtonKind;

  TPSCOnCalcButtonClick = Procedure(Sender: TObject;
    Var AButtonKind: TPSCCalcButtonKind) Of Object;

  TPSCOnCalcUpdateAssociate = Procedure(Sender: TObject;
    const ADisplayText:String) of Object;

  TPSCCalcStatus =
    (stCalcFirst,stCalcError,stCalcValid);

  TPSCCustomCalculator = Class(TPSCCustomControlAncestor)
  private
    FOnUpdateAssociate:TPSCOnCalcUpdateAssociate;
    FDisplayBackColor:TPSCColor;
    FDisplayBorderColor:TPSCColor;
    FButtonBevelWidth:Integer;
    FButtonRounded:Boolean;
    FOperator: TPSCCalcButtonKind;
    FOnCalcError: TPSCNotifyEvent;
    FCalcStatus: TPSCCalcStatus;
    FOnButtonClick: TPSCOnCalcButtonClick;
    FOnResult: TPSCNotifyEvent;
    FCalcMemory: Extended;
    FOperand: Extended;
    FCalcValue: String;
    FPrecision: Integer;
    FIsButtonDown: Boolean;
    FDownButton: TPSCCalcButtonKind;
    FBigButtonWidth: Integer;
    FButtonLongDist: Integer;
    FButtonDist: Integer;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FBorderStyle: TBorderStyle;
    FNumbersColor: TPSCColor;
    FActionsColor: TPSCColor;
    FShowDisplay:Boolean;
    FFlat:Boolean;
    FDigits:Integer;
    FFormat: TFloatFormat;

    Function GetValue: Extended;
    Function GetCalcValue: String;
    Function GetMaxValueLength: Integer;
    Function GetButtonRect(ButtonKind: TPSCCalcButtonKind): TRect;
    Function GetButtonAtPos(P: TPoint): TPSCCalcButtonKind;
    Function GetButtonFromChar(C: Char): TPSCCalcButtonKind;
    Function GetButtonFromKey(Key: Word; Shift: TShiftState):
      TPSCCalcButtonKind;
    Function CanPressButton(ButtonKind: TPSCCalcButtonKind): boolean;
    Function IsValueStored:Boolean;
    function GetCalcDisplayRect:TRect;

    Procedure SetDigits(V:Integer);
    Procedure SetFormat(V:TFloatFormat);
    Procedure SetPrecision(V:Integer);
    Procedure WMEraseBkgnd(Var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    Procedure WMTimer(Var Message: TMessage); message WM_TIMER;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FontChanged;
    Procedure SetValue(V: Extended);
    Procedure SetCalcValue(Const V: String);
    Procedure SetBorderStyle(V: TBorderStyle);
    Procedure SetCalcMemory(V: Extended);
    Procedure CalcError;
    Procedure CalcClear;
    Procedure CalcCheckFirst;
    Procedure UpdateAssociate;
    Procedure UpdateSize;
    Procedure InvalidateButton(ButtonKind: TPSCCalcButtonKind);
    Procedure InvalidateMemBtns;
    Procedure AnimatedPressButton(ButtonKind: TPSCCalcButtonKind);
    Procedure PressButton(ButtonKind: TPSCCalcButtonKind);
    Procedure ButtonUp;
    Procedure DoBeepOnError;
    Procedure DoButtonClick(ButtonKind: TPSCCalcButtonKind);
    Procedure DoOnResult;
    Procedure SetNumbersColor(V:TPSCColor);
    Procedure SetActionsColor(V:TPSCColor);
    Procedure SetButtonLongDist(V:Integer);
    Procedure SetButtonDist(V:Integer);
    Procedure SetShowDisplay(V:Boolean);
    procedure DrawDisplay;
    procedure SetDisplayBackColor(V:TPSCColor);
    procedure SetDisplayBorderColor(V:TPSCColor);
    procedure SetFlat(V:Boolean);
    Procedure InvalidateCalcDisplay;
    Procedure FontChanged;
    Procedure DrawButtonCaption(const ARect:TRect;const ACaption:String;
      AColor:TPSCColor);virtual;
    Procedure DrawButton(const ARect:TRect;const ACaption:String;
      AFaceColor,ABackColor:TPSCColor;ABtnPressed,ABtnEnabled:Boolean);virtual;
  protected
    function FloatToStrFormatted(const AValue:Extended;AForDisplay:Boolean):String;

    Procedure WMSize(Var Message: TWMSize); message WM_Size;
    Procedure CMEnabledChanged(Var Message: TMessage); message
      CM_ENABLEDCHANGED;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure CreateWnd; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    Procedure Paint; override;

    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyPress(Var Key: Char); override;
    Procedure KeyUp(Var Key: Word; Shift: TShiftState); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
      override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
      override;

    property Operator: TPSCCalcButtonKind Read FOperator Write FOperator;
    property CalcStatus: TPSCCalcStatus Read FCalcStatus Write FCalcStatus;
    property Operand: Extended Read FOperand Write FOperand;
    Property OnResult: TPSCNotifyEvent read FOnResult write FOnResult;
    Property OnButtonClick: TPSCOnCalcButtonClick read FOnButtonClick write
      FOnButtonClick;
    Property Precision: Integer read FPrecision write SetPrecision default
      PSCDefaultCalcPrecision;
    Property NumbersColor: TPSCColor Read FNumbersColor Write SetNumbersColor
      Default CPSCDefCalcNumbersColor;
    Property ActionsColor: TPSCColor Read FActionsColor Write SetActionsColor
      Default CPSCDefCalcActionsColor;
    Property ButtonLongDist:Integer Read FButtonLongDist Write SetButtonLongDist
      Default cPSCDefCalcBtnLongDist;
    Property ButtonDist:Integer Read FButtonDist Write SetButtonDist
      Default cPSCDefCalcBtnDist;
    Property ShowDisplay:Boolean Read FShowDisplay Write SetShowDisplay Default CPSCDefCalcShowDisplay;
    Property DisplayBackColor:TPSCColor Read FDisplayBackColor Write SetDisplayBackColor
      Default CPSCCalcDisplayBackColor;
    Property DisplayBorderColor:TPSCColor Read FDisplayBorderColor Write SetDisplayBorderColor
      Default CPSCCalcDisplayBorderColor;
    Property Digits:Integer Read FDigits Write SetDigits Default 0;
    Property Format: TFloatFormat Read FFormat Write SetFormat Default ffGeneral;
  public
    Function GetDisplayValue:String;
    Function GetButtonsRect(AButtons:TPSCCalcButtonKinds):TRect;
    
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property Value: Extended read GetValue write SetValue Stored IsValueStored;
    Property CalcValue: String read GetCalcValue write SetCalcValue;
    Property CalcMemory: Extended read FCalcMemory write SetCalcMemory;
    Property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default cPSCDefCalcBorderStyle;
    Property OnCalcError: TPSCNotifyEvent read FOnCalcError write FOnCalcError;
    Property Flat:Boolean Read FFlat Write SetFlat Default CPSCDefCalcFlat;
    Property OnUpdateAssociate:TPSCOnCalcUpdateAssociate
      Read FOnUpdateAssociate Write FOnUpdateAssociate;
  End;

  TPSCCalculator = Class(TPSCCustomCalculator)
  published
    Property Digits;
    Property Format;

    Property Flat;
    Property Value;
    Property DisplayBackColor;
    Property DisplayBorderColor;
    Property ShowDisplay;
    Property ButtonLongDist;
    Property ButtonDist;
    Property NumbersColor;
    Property ActionsColor;
    Property Precision;
    Property BorderStyle;
    Property OnResult;
    Property OnButtonClick;
    Property OnCalcError;
    Property Anchors;
    Property Color;
    Property Constraints;
    Property BiDiMode;
    Property Ctl3D;
    Property DockSite;
    Property DragCursor;
    Property DragKind;
    Property ParentBiDiMode;
    Property ParentCtl3D;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property ParentColor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop default True;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
    Property OnUpdateAssociate;
  End;

  TPSCCalculatorClass = Class Of TPSCCustomCalculator;

  TPSCCalculatorPopup = Class(TPSCPopupForm)
  private
    FCalc: TPSCCalculator;
    Procedure OnCalcResult(Sender: TObject);
  protected
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
  public
    Procedure ResizeFormControls; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property Calculator: TPSCCalculator read FCalc;
  End;

  TPSCCustomCalcEdit = class(TPSCCustomContainerEdit)
  private
    FNullValue:Extended;
    FUseNullValue:Boolean;
    Function IsNullValueStored:Boolean;
    function GetCalculator:TPSCCalculator;
    Function GetPrecision: Integer;

    procedure SetPrecision(V:Integer);
    Procedure SetValueAsFloat(const AValue:Extended);

    Procedure SetDigits(V:Integer);
    Procedure SetFormat(V:TFloatFormat);
    function GetDigits:Integer;
    function GetFormat:TFloatFormat;
  protected
    Function CreatePopup: TPSCPopupForm; override;

    Procedure EditOnKeyPress(Sender: TObject; Var Key: Char); override;
    Procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean); override;
    Procedure UpdatePopup; override;

    Property NullValue:Extended Read FNullValue Write FNullValue Stored IsNullValueStored;
    Property UseNullValue:Boolean Read FUseNullValue Write FUseNullValue Default False;
    Property Precision: Integer read GetPrecision write SetPrecision default
      PSCDefaultCalcPrecision;
    Property Digits:Integer Read GetDigits Write SetDigits Default 0;
    Property Format: TFloatFormat Read GetFormat Write SetFormat Default ffGeneral;
  public
    property Calculator:TPSCCalculator Read GetCalculator;
    Constructor Create(AOwner: TComponent);override;
  end;

  TPSCCalcEdit = class(TPSCCustomCalcEdit)
  public
    Property Field;
  published
    Property Digits;
    Property Format;
    Property NullValue;
    Property UseNullValue;
    Property DataSource;
    Property DataField;
    Property EditMask;
    Property OnDropDown;
    Property ButtonsVisible default True;
    Property OnCloseUp;
    Property PopupColor;
    Property Precision;

    Property Constraints;
    Property Anchors;
    Property BiDiMode;
    Property DragKind;
    Property ParentBiDiMode;
    Property AutoSelect;
    Property AutoSize;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property HideSelection default True;
    Property DragCursor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ReadOnly;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnChange;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnStartDock;
    Property OnStartDrag;
  end;

  TPSCCalculatorPopupClass=class of TPSCCalculatorPopup;
  
var
  CPSCUsedPopupCalcClass:TPSCCalculatorPopupClass=TPSCCalculatorPopup;

{------------------------------------------------------------------}

implementation

{------------------------------------------------------------------}

Function TPSCCustomCalcEdit.CreatePopup: TPSCPopupForm;
begin
  Result:=CPSCUsedPopupCalcClass.CreateNew(nil,0);
end;

{------------------------------------------------------------------}

function TPSCCustomCalculator.FloatToStrFormatted(const AValue:Extended;
  AForDisplay:Boolean):String;
begin
  If AForDisplay then
    Result:=FloatToStrF(AValue,Format,Precision,Digits)
  else
    Result:=FloatToStrF(AValue,ffgeneral,Precision,0);
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.SetValueAsFloat(const AValue:Extended);
begin
  If Field <> Nil Then
    Begin
      If Field.AsFloat <> AValue Then
        Begin
          if not Field.ReadOnly and (Field.DataSet <> nil) then
            Field.DataSet.Edit;

          If (AValue = NullValue) and UseNullValue Then
            Field.Clear
          Else
            Field.AsFloat := AValue;
        End;
    End
  else
    Text:=Calculator.FloatToStrFormatted(AValue,False);
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.EditOnKeyPress(Sender: TObject; Var Key: Char);
begin
  inherited;
  //If (Key>#31) and not (Key in ['0'..'9',DecimalSeparator]) then
  If (Key>#31) and not CharInSet(Key, ['0'..'9',FormatSettings.DecimalSeparator]) then
    Key:=#0;
  If (Key=FormatSettings.DecimalSeparator) and (Pos(FormatSettings.DecimalSeparator,Text)>0) then
    Key:=#0;
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.PopupCloseEvent(Sender: TObject; Canceled: Boolean);
begin
  If ReadOnly Then
    exit;
  If Not Canceled Then
    SetValueAsFloat(Calculator.Value);
end;

{------------------------------------------------------------------}

Function TPSCCustomCalcEdit.IsNullValueStored:Boolean;
begin
  Result:=FNullValue<>0;
end;

{------------------------------------------------------------------}

Function TPSCCustomCalcEdit.GetPrecision: Integer;
begin
  Result:=Calculator.Precision;
end;

{------------------------------------------------------------------}

procedure TPSCCustomCalcEdit.SetPrecision(V:Integer);
begin
  Calculator.Precision:=V;
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.SetDigits(V:Integer);
begin
  Calculator.Digits:=V;
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.SetFormat(V:TFloatFormat);
begin
  Calculator.Format:=V;
end;

{------------------------------------------------------------------}

function TPSCCustomCalcEdit.GetDigits:Integer;
begin
  Result:=Calculator.Digits;
end;

{------------------------------------------------------------------}

function TPSCCustomCalcEdit.GetFormat:TFloatFormat;
begin
  Result:=Calculator.Format;
end;

{------------------------------------------------------------------}

function TPSCCustomCalcEdit.GetCalculator:TPSCCalculator;
begin
  Result:=TPSCCalculatorPopup(Popup).Calculator;
end;

{------------------------------------------------------------------}

Constructor TPSCCustomCalcEdit.Create(AOwner: TComponent);
begin
  inherited;
  ButtonsVisible:=True;
  BtnKind:=bkComboBox;
  Text:='0';
end;

{------------------------------------------------------------------}

Procedure TPSCCustomCalcEdit.UpdatePopup;
var
  S:String;
begin
  inherited;
  If PopupCreated Then
    begin
      Calculator.Operator:=cbkNone;
      Calculator.CalcStatus:=stCalcFirst;
      Calculator.Operand:=0;

      If Field<>nil then
        Calculator.Value:=Self.Field.AsFloat
      else
        begin
          S:=PSCTrim(Self.Text);
          If S='' then
            Calculator.Value:=0
          else
            Calculator.Value:=StrToFloat(S);
        end;
    end;
end;

{------------------------------------------------------------------}

Const
  SPSCCalcMR                                 = 'MR';
  SPSCCalcMS                                 = 'MS';
  SPSCCalcMP                                 = 'M+';
  SPSCCalcCE                                 = 'CE';
  SPSCCalcMC                                 = 'MC';
  SPSCCalcC                                  = 'C';
  SPSCCalcBack                               = 'Back';
  SPSCCalcSQRT                               = 'sqrt';
  
  cOperationBtns = [cbkPlus,cbkMin,cbkMul,cbkDiv];
  cResultBtns = [cbkEqual,cbkPercent];
  cRepeatBtns = [cbkBack];

  cOtherColorButtons: Set Of TPSCCalcButtonKind = [
  cbkBack,cbkCE,cbkC,cbkMC,cbkDiv,
    cbkMR,cbkMul,cbkMS,cbkMin,
    cbkMPlus,cbkPlus,cbkEqual];

  cDisableIfNoMemButtons: Set Of TPSCCalcButtonKind = [
  cbkMC,cbkMR];
//BeginSkipConst
  cButtonCaption: Array[TPSCCalcButtonKind] Of String = (
    '', {cbkNone}
    SPSCCalcBack, {cbkBack}
    SPSCCalcCE, {cbkCE}
    SPSCCalcC, {cbkC}
    SPSCCalcMC, {cbkMC}
    '7', {cbk7}
    '8', {cbk8}
    '9', {cbk9}
    '/', {cbkDiv}
    SPSCCalcSQRT, {cbkSqrt}
    SPSCCalcMR, {cbkMR}
    '4', {cbk4}
    '5', {cbk5}
    '6', {cbk6}
    '*', {cbkMul}
    '%', {cbkPercent}
    SPSCCalcMS, {cbkMS}
    '1', {cbk1}
    '2', {cbk2}
    '3', {cbk3}
    '-', {cbkMin}
    '1/x', {cbkRev} //don't resource
    SPSCCalcMP, {cbkMPlus}
    '0', {cbk0}
    '+/-', {cbkNeg}
    '.', {cbkDot}
    '+', {cbkPlus}
    '=', {cbkEqual}
    'Sta'{cbkSta},
    'Ave'{cbkAve},
    'Sum'{cbkSum},
    's'{cbkS},
    'Dat'{cbkDat},
    'F-E'{cbkF_E},
    'dms'{cbkdms},
    'sin'{cbksin},
    'cos'{cbkcos},
    'tan'{cbktan},
    '('{cbkOpenBracket},
    ')'{cbkCloseBracket},
    'Exp'{cbkExp},
    'ln'{cbkln},
    'x^y'{cbkxPowY},
    'log'{cbklog},
    'x^3'{cbkxPow3},
    'x^2'{cbkxPow2},
    'n!'{cbknFactorial},
    'pi'{cbkpi},
    'A'{cbkA},
    'B'{cbkB},
    'C'{cbkCDigit},
    'D'{cbkD},
    'E'{cbkE},
    'F'{cbkF},
    'Mod'{cbkMod},
    'Or'{cbkOr},
    'Lsh'{cbkLsh},
    'And'{cbkAnd},
    'Xor'{cbkXor},
    'Not'{cbkNot},
    'Int'{cbkInt}
    );
//EndSkipConst

Constructor TPSCCustomCalculator.Create(AOwner: TComponent);
Begin
  Inherited;
  FShowDisplay:=CPSCDefCalcShowDisplay;
  FFlat:=CPSCDefCalcFlat;
  FDisplayBackColor:=CPSCCalcDisplayBackColor;
  FDisplayBorderColor:=CPSCCalcDisplayBorderColor;

  FButtonBevelWidth:=cPSCDefCalcBtnBevelWidth;
  FButtonRounded:=cPSCDefCalcBtnRounded;

  FPrecision := PSCDefaultCalcPrecision;
  FCalcValue := '0';

  FBorderStyle := cPSCDefCalcBorderStyle;
  FButtonLongDist := cPSCDefCalcBtnLongDist;
  FButtonDist := cPSCDefCalcBtnDist;
  FNumbersColor := CPSCDefCalcNumbersColor;
  FActionsColor := CPSCDefCalcActionsColor;
  ControlStyle := [csCaptureMouse,csOpaque];

  If NewStyleControls Then
    ControlStyle := ControlStyle
  Else
    ControlStyle := ControlStyle + [csFramed];

  TabStop := True;

End;

{-------------------------------------------}

Destructor TPSCCustomCalculator.Destroy;
Begin
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
    Begin
      Style := Style Or WS_TABSTOP Or WS_CLIPCHILDREN;
      WindowClass.Style := WindowClass.Style And Not CS_DBLCLKS;
      PSCUpdateParamsWithBorderStyle(Params,FBorderStyle,Ctl3d);
    End;
End;

{-------------------------------------------}

Procedure TPSCCustomCalculator.CreateWnd;
Begin
  Inherited CreateWnd;
  SendMessage(Handle,CM_FONTCHANGED,0,0);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetNumbersColor(V:TPSCColor);
begin
  If FNumbersColor<>V then
  begin
    FNumbersColor:=V;
    Invalidate;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetButtonLongDist(V:Integer);
begin
  If FButtonLongDist<>V then
  begin
    FButtonLongDist:=V;
    UpdateSize;
  end;
end;

{--------------------------------------------}

procedure TPSCCustomCalculator.SetDisplayBackColor(V:TPSCColor);
begin
  If FDisplayBackColor<>V then
  begin
    FDisplayBackColor:=V;
    InvalidateCalcDisplay;
  end;
end;

{--------------------------------------------}

procedure TPSCCustomCalculator.SetFlat(V:Boolean);
begin
  If FFLat<>V then
  begin
    FFlat:=V;
    Invalidate;
  end;
end;

{--------------------------------------------}

procedure TPSCCustomCalculator.SetDisplayBorderColor(V:TPSCColor);
begin
  If FDisplayBorderColor<>V then
  begin
    FDisplayBorderColor:=V;
    InvalidateCalcDisplay;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetShowDisplay(V:Boolean);
begin
  If FShowDisplay<>V then
  begin
    FShowDisplay:=V;
    UpdateSize;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetButtonDist(V:Integer);
begin
  If FButtonDist<>V then
  begin
    FButtonDist:=V;
    UpdateSize;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetActionsColor(V:TPSCColor);
begin
  If FActionsColor<>V then
  begin
    FActionsColor:=V;
    Invalidate;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.DrawButtonCaption(const ARect:TRect;
  const ACaption:String;AColor:TPSCColor);
var
  CaptionSize: TSize;
Begin
  With ARect,MylaCanvas Do
    Begin
      Font.Color := AColor;
      CaptionSize:=Canvas.TextExtent(ACaption);
      Brush.Style:=BS_Clear;
      With CaptionSize Do
        TextOut(Left + ((Right - Left - cX) Div 2) + 1,
          Top + ((Bottom - Top - cY) Div 2) +
            1,ACaption);
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.DrawButton(const ARect:TRect;const ACaption:String;
  AFaceColor,ABackColor:TPSCColor;ABtnPressed,ABtnEnabled:Boolean);
Var
  BtnClientRect: TRect;
  MyState:Cardinal;
Begin
  BtnClientRect:=ARect;
  MyState:=PSC_DFCS_ADJUSTRECT or PSC_DFCS_BUTTONPUSH;

  If not ABtnEnabled then
    MyState:=MyState or PSC_DFCS_INACTIVE;

  If ABtnPressed then
    MyState:=MyState or PSC_DFCS_PUSHED;

  If FFlat and not ABtnPressed then
    MyState:=MyState or PSC_DFCS_FLAT;

  DrawFrameControl(Canvas.Handle,BtnClientRect,PSC_DFC_BUTTON,MyState);

  If ABtnEnabled Then
      DrawButtonCaption(BtnClientRect,ACaption,AFaceColor)
  Else
    Begin
      DrawButtonCaption(BtnClientRect,ACaption,ABackColor);
      OffsetRect(BtnClientRect, -1, -1);
      DrawButtonCaption(BtnClientRect,ACaption,AFaceColor);
    End;
End;

{--------------------------------------------}

function TPSCCustomCalculator.GetCalcDisplayRect:TRect;
begin
  Result:=PSCRect(FButtonLongDist,FButtonLongDist,
          ClientWidth-FButtonLongDist,
          FButtonLongDist+PSCGetEditHeight(Font,False));
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.InvalidateCalcDisplay;
Begin
  If ShowDisplay then
    InvalidateRect(GetCalcDisplayRect);
End;

{--------------------------------------------}

Function TPSCCustomCalculator.IsValueStored:Boolean;
begin
  Result:=Value<>0;
end;

{--------------------------------------------}

procedure TPSCCustomCalculator.DrawDisplay;
var
  MyRect:TRect;
  MyTextRect:TRect;
  MyTextSize:TSize;
  S:String;
begin
  If ShowDisplay then
    With Canvas do
    begin
      MyRect:=GetCalcDisplayRect;

      Brush.Color:=FDisplayBackColor;
      Pen.Color:=FDisplayBorderColor;
      Rectangle(MyRect);

      InflateRect(MyRect, -3, -1);
      S:=GetDisplayValue;

      MyTextSize:=Canvas.TextExtent(S);
      With MyTextRect do
      begin
        Left:=0;
        Top:=0;
        Right:=MyTextSize.cx;
        Bottom:=MyTextSize.cy;
      end;
      MyTextRect:=PSCAlignRectInRect(MyTextRect,MyRect,haRight,vaCenter);
      TextRect(MyRect,MyTextRect.Left,MyTextRect.Top,S);
      InflateRect(MyRect, 3, 1);
      MylaCanvas.ExcludeClipRect(MyRect.Left,MyRect.Top,MyRect.Right,MyRect.Bottom);
    end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.Paint;
Var
  BtnRect: TRect;
  i: TPSCCalcButtonKind;
  FaceColor,BackColor: TPSCColor;
  BtnEnabled,BtnPressed: Boolean;
  BtnCaption: String;
Begin
  If not HandleAllocated then
    Exit;
  With Canvas Do
    Begin
      Font := Self.Font;

      DrawDisplay;

      Brush := Self.Brush;


      For i := cbkBack To cbkEqual Do
        Begin

          BtnCaption := cButtonCaption[i];
          BtnRect := GetButtonRect(i);
          BtnEnabled := True;
          BtnPressed := FIsButtonDown And (i = FDownButton);
          BackColor := clPSCBtnHighlight;

          If Not CanPressButton(i) Then
            Begin
              FaceColor := clPSCGrayText;
              BtnEnabled := False;
            End
          Else
            If i In cOtherColorButtons Then
              FaceColor := FActionsColor
            Else
              FaceColor := FNumbersColor;

          DrawButton(BtnRect,BtnCaption,FaceColor,BackColor,
            BtnPressed,BtnEnabled);

          MylaCanvas.ExcludeClipRect(BtnRect.Left,BtnRect.Top,
            BtnRect.Right,BtnRect.Bottom);

        End;

      Font := Self.Font;
      Brush := Self.Brush;
      FillRect(ClientRect);

    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.CalcError;
Begin
  FCalcStatus := stCalcError;
  CalcValue := PSCConsts.CalcError;
  Invalidate;
  DoBeepOnError;
  If Assigned(FOnCalcError) Then
    FOnCalcError(Self);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.CalcClear;
Begin
  FCalcStatus := stCalcFirst;
  FOperator := cbkEqual;
  Value := 0.0;
  Invalidate;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.CalcCheckFirst;
Begin
  If FCalcStatus = stCalcFirst Then
    Begin
      FCalcStatus := stCalcValid;
      CalcValue := '0';
    End;
End;

{--------------------------------------------}

function TPSCCustomCalculator.GetDisplayValue:String;
begin
  Result:=FloatToStrFormatted(Value,True);
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.UpdateAssociate;
Begin
  If Assigned(FOnUpdateAssociate) then
    FOnUpdateAssociate(Self,CalcValue);

  InvalidateCalcDisplay;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.UpdateSize;
var
  MyClientHeight:Integer;
Begin
  ClientWidth := 3 * FButtonLongDist + 6 * FButtonWidth + 4 * FButtonDist;
  MyClientHeight := 2 * FButtonLongDist + 5 * FButtonHeight + 4 * FButtonDist;

  If ShowDisplay then
    inc(MyClientHeight,PSCGetEditHeight(Font,False)+FButtonLongDist);

  ClientHeight:=MyClientHeight;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.FontChanged;
Begin
  Canvas.Font := Font;
  With MylaCanvas Do
    Begin
      FButtonWidth := TextWidth('00000');
      FBigButtonWidth := ((FButtonWidth * 5 + FButtonDist * 4) Div 3) -
        FButtonDist;
      FButtonHeight := Round(TextHeight('0') * 1.8);
    End;

  SendMessage(Handle,WM_SIZE,0,0);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.InvalidateButton(ButtonKind: TPSCCalcButtonKind);
Begin
  InvalidateRect(GetButtonRect(ButtonKind));
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.InvalidateMemBtns;
Begin
  InvalidateButton(cbkMC);
  InvalidateButton(cbkMR);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.AnimatedPressButton(ButtonKind:
  TPSCCalcButtonKind);
Begin
  If FIsButtonDown Then
    ButtonUp;

  FDownButton := ButtonKind;
  FIsButtonDown := True;
  InvalidateButton(ButtonKind);
  Update;
  KillTimer(Handle,1);
  SetTimer(Handle,1,70,Nil);
  PressButton(ButtonKind);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.PressButton(ButtonKind: TPSCCalcButtonKind);

  Function ValidStatus: boolean;
  Begin
    Result := FCalcStatus In [stCalcValid,stCalcFirst];
  End;

  Procedure PressBack;
  Var
    S: String;
  Begin
    CalcCheckFirst;
    S := CalcValue;
    Delete(S,Length(S),1);
    If (S = '') Or (S = '-') Then
      S := '0';
    CalcValue := S;
  End;

  Procedure PressRev;
  Begin
    If ValidStatus Then
      Begin
        If FOperator In cOperationBtns Then
          FCalcStatus := stCalcValid
        Else
          FCalcStatus := stCalcFirst;

        If Value = 0 Then
          CalcError
        Else
          Value := (1.0 / Value);
      End;
  End;

  Procedure PressNumber;
  Begin
    CalcCheckFirst;
    If CalcValue = '0' Then
      CalcValue := '';

    If Length(CalcValue) < GetMaxValueLength Then
      CalcValue := CalcValue + cButtonCaption[ButtonKind]
    Else
      DoBeepOnError;
  End;

  Procedure PressSQRT;
  Begin
    If ValidStatus Then
      Begin
        If FOperator In cOperationBtns Then
          FCalcStatus := stCalcValid
        Else
          FCalcStatus := stCalcFirst;

        If Value < 0 Then
          CalcError
        Else
          Value := Sqrt(Value);
      End;
  End;

  Procedure PressOperation;
  Var
    TempValue: Extended;
  Begin
    If FCalcStatus = stCalcValid Then
      Begin
        FCalcStatus := stCalcFirst;
        TempValue := Value;

        If ButtonKind = cbkPercent Then
          Case FOperator Of
            cbkMul,cbkDiv: TempValue := TempValue / 100.0;
            cbkPlus,cbkMin: TempValue := FOperand * TempValue / 100.0;
          End;

        Case FOperator Of
          cbkMin: Value := FOperand - TempValue;
          cbkPlus: Value := FOperand + TempValue;
          cbkMul: Value := FOperand * TempValue;
          cbkDiv:
            If TempValue = 0 Then
              CalcError
            Else
              Value := FOperand / TempValue;
        End;
      End;

    FOperator := ButtonKind;
    FOperand := Value;
    If ButtonKind In cResultBtns Then
      DoOnResult;
  End;

  Procedure PressMR;
  Begin
    If ValidStatus Then
      Begin
        FCalcStatus := stCalcFirst;
        Value := FCalcMemory;
      End;
  End;

  Procedure PressMS;
  Begin
    If ValidStatus Then
      Begin
        FCalcStatus := stCalcFirst;
        CalcMemory := Value;
      End;
  End;

  Procedure PressMPlus;
  Begin
    If ValidStatus Then
      Begin
        FCalcStatus := stCalcFirst;
        CalcMemory := CalcMemory + Value;
      End;
  End;

  Procedure PressDot;
  Begin
    CalcCheckFirst;
    If Pos(PSCGetDecimalSeparator,CalcValue) = 0 Then
      CalcValue := CalcValue + PSCGetDecimalSeparator;
  End;

Begin
  DoButtonClick(ButtonKind);
  If (FCalcStatus = stCalcError) And Not (ButtonKind In [cbkCE,cbkC]) Then
    Begin
      CalcError;
      Exit;
    End;

  Case ButtonKind Of
    cbkDot: PressDot;
    cbkRev: PressRev;
    cbkSqrt: PressSQRT;
    cbk7,cbk8,cbk9,cbk4,cbk5,cbk6,cbk1,cbk2,cbk3,cbk0: PressNumber;
    cbkBack: PressBack;
    cbkNeg: Value := -Value;
    cbkPlus,cbkMin,cbkMul,cbkDiv,cbkEqual,cbkPercent: PressOperation;
    cbkC,cbkCE: CalcClear;
    cbkMPlus: PressMPlus;
    cbkMS: PressMS;
    cbkMR: PressMR;
    cbkMC:
      Begin
        CalcMemory := 0.0;
        FCalcStatus := stCalcFirst;
      End;
  End;

End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.DoBeepOnError;
begin
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.ButtonUp;
Begin
  FIsButtonDown := False;
  InvalidateButton(FDownButton);
  FDownButton := cbkNone;
  Update;
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetMaxValueLength: Integer;
Begin
  Result := Ord(Boolean(Pos('-',CalcValue))) + PSCMax(2,FPrecision);
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetButtonsRect(AButtons:TPSCCalcButtonKinds):TRect;
var
  i:TPSCCalcButtonKind;
  MyRect:TRect;
  MyFirst:Boolean;
begin
  MyFirst:=True;
  for i:=Low(TPSCCalcButtonKind) to High(TPSCCalcButtonKind) do
    If i in AButtons then
    begin
      If MyFirst then
        begin
          Result:=GetButtonRect(i);
          MyFirst:=False;
        end
      else
        begin
          MyRect:=GetButtonRect(i);
          With Result do
          begin
            Left:= PSCMin(Left,MyRect.Left);
            Top:= PSCMin(Top,MyRect.Top);
            Right:= PSCMax(Right,MyRect.Right);
            Bottom:= PSCMax(Bottom,MyRect.Bottom);
          end;
        end;
    end;
end;

{--------------------------------------------}

Function TPSCCustomCalculator.GetButtonRect(ButtonKind: TPSCCalcButtonKind):
  TRect;
Const
  cButtonPos: Array[TPSCCalcButtonKind] Of Array[0..1] Of Integer = (
    (0,0), {cbkNone}
    (0,0), {cbkBack}
    (1,0), {cbkCE}
    (2,0), {cbkC}
    (0,1), {cbkMC}
    (1,1), {cbk7}
    (2,1), {cbk8}
    (3,1), {cbk9}
    (4,1), {cbkDiv}
    (5,1), {cbkSqrt}
    (0,2), {cbkMR}
    (1,2), {cbk4}
    (2,2), {cbk5}
    (3,2), {cbk6}
    (4,2), {cbkMul}
    (5,2), {cbkPercent}
    (0,3), {cbkMS}
    (1,3), {cbk1}
    (2,3), {cbk2}
    (3,3), {cbk3}
    (4,3), {cbkMin}
    (5,3), {cbkRev}
    (0,4), {cbkMPlus}
    (1,4), {cbk0}
    (2,4), {cbkNeg}
    (3,4), {cbkDot}
    (4,4), {cbkPlus}
    (5,4), {cbkEqual}

    (-1,-1){cbkSta},
    (-1,-1){cbkAve},
    (-1,-1){cbkSum},
    (-1,-1){cbkS},
    (-1,-1){cbkDat},
    (-1,-1){cbkF_E},
    (-1,-1){cbkdms},
    (-1,-1){cbksin},
    (-1,-1){cbkcos},
    (-1,-1){cbktan},
    (-1,-1){cbkOpenBracket},
    (-1,-1){cbkCloseBracket},
    (-1,-1){cbkExp},
    (-1,-1){cbkln},
    (-1,-1){cbkxPowY},
    (-1,-1){cbklog},
    (-1,-1){cbkxPow3},
    (-1,-1){cbkxPow2},
    (-1,-1){cbknFactorial},
    (-1,-1){cbkpi},
    (-1,-1){cbkA},
    (-1,-1){cbkB},
    (-1,-1){cbkCDigit},
    (-1,-1){cbkD},
    (-1,-1){cbkE},
    (-1,-1){cbkF},
    (-1,-1){cbkMod},
    (-1,-1){cbkOr},
    (-1,-1){cbkLsh},
    (-1,-1){cbkAnd},
    (-1,-1){cbkXor},
    (-1,-1){cbkNot},
    (-1,-1){cbkInt}
    );

Var
  X,Y: Integer;
Begin
  Y := cButtonPos[ButtonKind,1];
  X := cButtonPos[ButtonKind,0];
  With Result Do
    Begin
      Top := FButtonLongDist + Y * (FButtonHeight + FButtonDist);

      If ShowDisplay then
        inc(Top,PSCGetEditHeight(Font,False)+FButtonLongDist);

      If ButtonKind In [cbkBack..cbkC] Then
        Begin
          Left := ClientWidth + FBigButtonWidth * (x - 3) - FButtonLongDist
            + FButtonDist * (x - 2);
          Right := Left + FBigButtonWidth;
        End
      Else
        Begin
          Left := FButtonLongDist + X * (FButtonWidth + FButtonDist);
          If X > 0 Then
            Inc(Left,FButtonLongDist - FButtonDist);
          Right := Left + FButtonWidth;
        End;
      Bottom := Top + FButtonHeight;
    End;
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetButtonAtPos(P: TPoint): TPSCCalcButtonKind;
Var
  i: TPSCCalcButtonKind;
Begin
  Result := cbkNone;
  For i := cbkBack To cbkEqual Do
    If PtInRect(GetButtonRect(i),P) Then
      Begin
        Result := i;
        Exit;
      End;
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetButtonFromChar(C: Char): TPSCCalcButtonKind;
Const
  cCharToButtonKind: Array['0'..'9'] Of TPSCCalcButtonKind =
  (cbk0,cbk1,cbk2,cbk3,cbk4,cbk5,cbk6,cbk7,cbk8,cbk9);
Begin
  Result := cbkNone;
  Case C Of
    '0'..'9': Result := cCharToButtonKind[C];
    '+': Result := cbkPlus;
    '-': Result := cbkMin;
    '*': Result := cbkMul;
    '/': Result := cbkDiv;
    '%': Result := cbkPercent;
    '=': Result := cbkEqual;
    #8: Result := cbkBack;
    '@': Result := cbkSqrt;
  Else
    If PSCGetDecimalSeparator = C Then
      Result := cbkDot;
  End; {case C of}
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetButtonFromKey(Key: Word;
  Shift: TShiftState): TPSCCalcButtonKind;
Var
  C: Boolean;
Begin
  C := ssCtrl In Shift;
  Result := cbkNone;
  Case Key Of
    VK_RETURN: Result := cbkEqual;
    VK_ESCAPE: Result := cbkC;
    VK_F9: Result := cbkNeg;
    VK_DELETE: Result := cbkCE;
    Ord('C') {VK_C}:
      If Not C Then
        Result := cbkC;
    Ord('P') {VK_P}:
      If C Then
        Result := cbkMPlus;
    Ord('L') {VK_L}:
      If C Then
        Result := cbkMC;
    Ord('R') {VK_R}:
      If C Then
        Result := cbkMR
      Else
        Result := cbkRev;
    Ord('M') {VK_M}:
      If C Then
        Result := cbkMS;
  End;
End;

{--------------------------------------------}

Function TPSCCustomCalculator.CanPressButton(ButtonKind: TPSCCalcButtonKind):
  boolean;
Begin
  Result := Enabled;
  If Result then
    Result := Not ((ButtonKind In cDisableIfNoMemButtons) And (CalcMemory = 0));
  If Result then
    Result := (CalcStatus <> stCalcError) or (ButtonKind In [cbkCE,cbkC]);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  Button: TPSCCalcButtonKind;
Begin
  Button := GetButtonFromKey(Key,Shift);
  If (Button <> cbkNone) And (CanPressButton(Button)) Then
    Begin
      AnimatedPressButton(Button);
      Key := 0;
    End;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.KeyPress(Var Key: Char);
Var
  Button: TPSCCalcButtonKind;
Begin
  Button := GetButtonFromChar(Key);
  If (Button <> cbkNone) And (CanPressButton(Button)) Then
    Begin
      AnimatedPressButton(Button);
      Key := #0;
    End;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.KeyUp(Var Key: Word; Shift: TShiftState);
Begin
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X,Y: Integer);
Var
  TempButton: TPSCCalcButtonKind;
Begin
  If Button = mbLeft Then
    Begin
      TempButton := GetButtonAtPos(Point(X,Y));
      If (TempButton <> cbkNone) And CanPressButton(TempButton) Then
        Begin
          FDownButton := TempButton;
          FIsButtonDown := True;
          InvalidateButton(FDownButton);
        End;
    End;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  NewIsButtonDown: boolean;
Begin
  If FDownButton <> cbkNone Then
    Begin
      NewIsButtonDown := PtInRect(GetButtonRect(FDownButton),Point(X,Y));
      If NewIsButtonDown <> FIsButtonDown Then
        Begin
          FIsButtonDown := NewIsButtonDown;
          InvalidateButton(FDownButton);
        End;
    End;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  If (FDownButton <> cbkNone) And (Button = mbLeft) Then
    Begin
      If FIsButtonDown Then
        Begin
          FIsButtonDown := False;
          InvalidateButton(FDownButton);
          PressButton(FDownButton);
        End;
      FDownButton := cbkNone;
    End;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.DoButtonClick(ButtonKind: TPSCCalcButtonKind);
Begin
  If Assigned(FOnButtonClick) Then
    FOnButtonClick(Self,ButtonKind);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.DoOnResult;
Begin
  If Assigned(FOnResult) Then
    FOnResult(Self);
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.cmEnabledChanged(Var Message: TMessage);
Begin
  Inherited;
  Invalidate;
End;

{--------------------------------------------}

Function TPSCCustomCalculator.GetValue: Extended;
Begin
  If (FCalcStatus = stCalcError) or
    (not TryStrToFloat(PSCTrim(CalcValue),Result)) Then
    Result := 0.0
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetValue(V: Extended);
Begin
  // don't perform any checks here on V<>Value
  CalcValue := FloatToStrFormatted(V,False);
  DrawDisplay;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetDigits(V:Integer);
begin
  If FDigits<>V then
  begin
    FDigits:=V;
    Value:=Value;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetFormat(V:TFloatFormat);
begin
  If FFormat<>V then
  begin
    FFormat:=V;
    Value:=Value;
  end;
end;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetPrecision(V:Integer);
begin
  If FPrecision<>V then
  begin
    FPrecision:=V;
    Value:=Value;
  end;
end;

{--------------------------------------------}

Function TPSCCustomCalculator.GetCalcValue: String;
Begin
  Result := FCalcValue;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetCalcValue(Const V: String);
Begin
  If FCalcValue <> v Then
    Begin
      FCalcValue := V;
      UpdateAssociate;
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetBorderStyle(V: TBorderStyle);
Begin
  If FBorderStyle <> V Then
    Begin
      FBorderStyle := V;
      RecreateWnd;
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.SetCalcMemory(V: Extended);
Begin
  If FCalcMemory <> V Then
    Begin
      FCalcMemory := V;
      InvalidateMemBtns;
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.WMEraseBkgnd(Var Message: TWMEraseBkgnd);
Begin
  Message.Result := 1;
End;

{-------------------------------------------}

Procedure TPSCCustomCalculator.WMSize(Var Message: TWMSize);
Begin
  Inherited;
  UpdateSize;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.WMTimer(Var Message: TMessage);
Begin
  If (Message.wParam = 1) And FIsButtonDown Then
    Begin
      ButtonUp;
      KillTimer(Handle,1);
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomCalculator.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  FontChanged;
End;

{------------------------------------------------------------------------------}

constructor TPSCCalculatorPopup.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Var
  FParent: TWinControl;
Begin
  Inherited;
  GetFooterPanel;
  FParent := Self;
  FCalc := TPSCCalculator.Create(Self);
  FCalc.Parent := FParent;

  With FCalc Do
    Begin
      OnResult := OnCalcResult;
      ShowDisplay := True;
    End;

  ActiveControl := FCalc;

  ResizeFormControls;
End;

{-------------------------------------------}

Procedure TPSCCalculatorPopup.ResizeFormControls;
Begin
  HandleNeeded;
  FCalc.HandleNeeded;
  FCalc.Top := 0;
  FCalc.Left := 0;
  ClientHeight := FCalc.Top + FCalc.Height+GetFooterPanel.Height;
  ClientWidth := FCalc.Width;
End;

{-------------------------------------------}

Procedure TPSCCalculatorPopup.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_Return Then
    Calculator.KeyDown(Key,Shift);
  Inherited;
End;

{-------------------------------------------}

Procedure TPSCCalculatorPopup.OnCalcResult(Sender: TObject);
Begin
  ClosePopup(False,True);
End;

{-------------------------------------------}
end.
