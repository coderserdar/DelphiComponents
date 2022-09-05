{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}
{}
{ $Log:  13538: umcmIntE.pas
//
//    Rev 1.15    2014-04-03 20:24:14  mcm    Version: DT 4.0
//
//    Rev 1.14    01-03-2011 20:40:30  mcm    Version: IMG 3.4
//
//    Rev 1.13    25-10-2009 16:33:08  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//    Rev 1.12    30-11-2008 15:31:34  mcm
// Delphi 2009
//
//    Rev 1.11    26-02-2006 10:50:20  mcm
// Changed TmcmRealSpin - disabled firing OnChange when the Decimals property is
// changed.
//
//    Rev 1.10    18-02-2006 20:07:52  mcm
// Disabled firering the OnChange event during csLoading and csUpdating state.
//
//   Rev 1.9    07-01-2005 13:43:46  mcm
// Improved entering values, taking care of incorrect character and position
// thereof.
// Remove multi-line editing.

//
//   Rev 1.8    26-09-2004 20:52:12  mcm

//
//   Rev 1.7    16-09-2004 20:43:30  mcm
// Included TmcmIntSpin control to replace Borland's TSpinEdit.

//
//   Rev 1.6    06-07-2003 11:23:10  mcm    Version: DT 2.5
// Added compiler conditions.

//
//   Rev 1.5    15-04-2003 11:50:58  mcm
// Added compiler directives to suppres pointer warnings (Delphi 7).

//
//   Rev 1.4    06-03-2003 12:53:18  mcm
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.3    22-02-2002 16:00:34  mcm
// Corrected handling of very small numbers below zero.

//
//   Rev 1.2    22-11-2001 12:52:58
// Changed header

{
{   Rev 1.1    18-02-00 16:34:52  mcm
}
{
{   Rev 1.0    04-01-00 23:16:03  mcm    Version: 1.0
{ Initial Revision
}
{}
unit umcmIntE;

interface

{$IFDEF VER100} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER110} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER120} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_6} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$DEFINE DCB3_5} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER145} {$DEFINE DCB3_6} {$ENDIF}

{$IFDEF VER220} {$DEFINE LATDXE} {$ENDIF}  // DELPHI XE  >= VER220
{$IFDEF VER230} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE2 >= VER230
{$IFDEF VER240} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE3 >= VER240
{$IFDEF VER250} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE4 >= VER250
{$IFDEF VER260} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE5 >= VER260
{$IFDEF VER270} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE6 >= VER270
{$IFDEF VER280} {$DEFINE LATDXE} {$DEFINE GE_DXE2} {$ENDIF}  // DELPHI XE7 >= VER280

uses {$IFDEF GE_DXE2}
     WinApi.Windows, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Graphics, Vcl.Controls,
     Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.SysUtils, System.Classes,
     WinApi.Messages;
     {$ELSE}
     Windows,
     Messages,
     Controls,
     StdCtrls,
     ExtCtrls,
     Buttons,
     Graphics,
     {$IFDEF CLR} // VCL .NET
       System.Drawing,
       System.ComponentModel,
       Borland.Vcl.Types,
     {$ELSE} // VCL Win32
        Forms, Dialogs, Menus,
     {$ENDIF}
     SysUtils, Classes;
     {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

type

//------------------------------------------------------------------------------
// TmcmIntEdit.
//------------------------------------------------------------------------------

  TmcmIntEdit = class(TCustomEdit)
  private
    // Private declarations
    FValue    : longint;
    FMinValue : longint;
    FMaxValue : longint;
    procedure CMEnter   (var Message  : TCMGotFocus); message CM_ENTER;
    procedure CMExit    (var Message  : TCMExit);     message CM_EXIT;
  protected
    // Protected declarations
    procedure Change; override;
    procedure KeyPress  (var Key      : Char); override;
    function  IsValidKey(    Key      : Char) : boolean;
    function  GetValue : longint;
    procedure SetValue  (    NewValue : longint);
    function  CheckValue(    NewValue : longint) : longint;
  public
    // Public declarations
    constructor Create  (    AOwner   : TComponent); override;
  published
    // Published declarations
    {$IFNDEF DCB3}
    property Anchors;
    {$ENDIF}

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;

    {$IFNDEF DCB3}
    property Constraints;
    {$ENDIF}

    property Ctl3D;

    {$IFNDEF DCB3}
    property DragKind;
    {$ENDIF}

    property DragCursor;
    property DragMode;
    property Enabled;

    property Font;
    property Height;
    property HideSelection;
    property Hint;
    property ImeMode;
    property ImeName;
    property Left;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;

    {$IFNDEF DCB3}
    property ParentBiDiMode;
    {$ENDIF}

    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;

    {$IFNDEF DCB3_4}
    property OnContextPopup;
    {$ENDIF}

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;

    {$IFNDEF DCB3}
    property OnEndDock;
    {$ENDIF}

    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFNDEF DCB3}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;

    property Value : longint
      read   GetValue
      write  SetValue;
    property MaxValue : longint
      read   FMaxValue
      write  FMaxValue;
    property MinValue : longint
      read   FMinValue
      write  FMinValue;
  end;

//------------------------------------------------------------------------------
// TmcmRealEdit.
//------------------------------------------------------------------------------

  TmcmRealEdit = class(TCustomEdit)
  private
    // Private declarations
    FMinValue      : extended;
    FMaxValue      : extended;
    FValue         : extended;
    FDecimals      : word;
    procedure CMEnter   (var Message  : TCMGotFocus); message CM_ENTER;
    procedure CMExit    (var Message  : TCMExit);     message CM_EXIT;
  protected
    // Protected declarations
    procedure Change; override;
    procedure KeyPress   (var Key      : Char); override;
    function  IsValidKey (    Key      : Char) : boolean;
    function  GetValue : extended;
    procedure SetValue   (    NewValue : extended);
    function  CheckValue (    NewValue : extended) : extended;
    procedure SetDecimals(    Decimals : word);
  public
    // Public declarations
    constructor Create   (    AOwner   : TComponent); override;
  published
    // Published declarations
    {$IFNDEF DCB3}
    property Anchors;
    {$ENDIF}
    property AutoSelect;
    property AutoSize;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property Value : extended
      read   GetValue
      write  SetValue;
    property MaxValue : extended
      read   FMaxValue
      write  FMaxValue;
    property MinValue : extended
      read   FMinValue
      write  FMinValue;
    property Decimals : word
      read   FDecimals
      write  SetDecimals;
  end;


const
  mcmInitRepeatPause = 400;  // pause before repeat timer (ms)
  mcmRepeatPause     = 100;  // pause before hint window displays (ms)


type
  TmcmTimeSpeedBtn = class;

  TmcmTimeBtnState = set of (tbFocusRect, tbAllowTimer);

//------------------------------------------------------------------------------
// TmcmRealSpinBtn.
//------------------------------------------------------------------------------

  TmcmRealSpinBtn = class (TWinControl)
  private
    // Private declarations
    FUpButton      : TmcmTimeSpeedBtn;
    FDownButton    : TmcmTimeSpeedBtn;
    FFocusedButton : TmcmTimeSpeedBtn;
    FFocusControl  : TWinControl;
    FOnUpClick     : TNotifyEvent;
    FOnDownClick   : TNotifyEvent;
    function  CreateButton : TmcmTimeSpeedBtn;
    function  GetUpGlyph   : TBitmap;
    function  GetDownGlyph : TBitmap;
    procedure SetUpGlyph  (    Value   : TBitmap);
    procedure SetDownGlyph(    Value   : TBitmap);
    procedure BtnClick    (    Sender  : TObject);
    procedure BtnMouseDown(    Sender  : TObject;
                               Button  : TMouseButton;
                               Shift   : TShiftState;
                               X, Y    : Integer);
    procedure SetFocusBtn (    Btn     : TmcmTimeSpeedBtn);
    procedure Adjust_Size  (var W       : Integer;
                           var H       : Integer);
    procedure WMSize      (var Message : TWMSize);       message WM_SIZE;
    procedure WMSetFocus  (var Message : TWMSetFocus);   message WM_SETFOCUS;
    procedure WMKillFocus (var Message : TWMKillFocus);  message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message : TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    // Protected declarations
    procedure Loaded; override;
    procedure KeyDown     (var Key     : Word;
                               Shift   : TShiftState); override;
  public
    // Public declarations
    constructor Create    (    AOwner  : TComponent); override;
    procedure SetBounds   (    ALeft   : integer;
                               ATop    : integer;
                               AWidth  : integer;
                               AHeight : integer); override;
  published
    // Published declarations
    property Align;
    property Ctl3D;
    property DownGlyph : TBitmap
      read   GetDownGlyph
      write  SetDownGlyph;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl : TWinControl
      read   FFocusControl
      write  FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph : TBitmap
      read   GetUpGlyph
      write  SetUpGlyph;
    property Visible;
    property OnDownClick : TNotifyEvent
      read   FOnDownClick
      write  FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnUpClick : TNotifyEvent
      read   FOnUpClick
      write  FOnUpClick;
  end;

//------------------------------------------------------------------------------
// TmcmTimeSpeedBtn.
//------------------------------------------------------------------------------

  TmcmTimeSpeedBtn = class(TSpeedButton)
  private
    // Private declarations
    FmcmRepeatTimer  : TTimer;
    FmcmTimeBtnState : TmcmTimeBtnState;
    procedure TimerExpired(   Sender  : TObject);
  protected
    // Protected declarations
    procedure Paint; override;
    procedure MouseDown   (    Button : TMouseButton;
                               Shift  : TShiftState;
                               X, Y   : integer); override;
    procedure MouseUp     (    Button : TMouseButton;
                               Shift  : TShiftState;
                               X, Y   : Integer); override;
  public
    // Public declarations
    destructor Destroy; override;
    property TimeBtnState : TmcmTimeBtnState
      read  FmcmTimeBtnState
      write FmcmTimeBtnState;
  end;

//------------------------------------------------------------------------------
// TmcmIntSpin.
//------------------------------------------------------------------------------

  TmcmIntSpin = class(TmcmIntEdit)
  private
    // Private declarations
    FIncrement : integer;
    FButton    : TmcmRealSpinBtn;
  protected
    // Protected declarations
    function  GetMinHeight : integer;
    procedure SetEditRect;
    procedure WMSize (var Message : TWMSize);     message WM_SIZE;
    procedure WMPaste(var Message : TWMPaste);    message WM_PASTE;
    procedure WMCut  (var Message : TWMCut);      message WM_CUT;

    procedure UpClick     (    Sender : TObject); virtual;
    procedure DownClick   (    Sender : TObject); virtual;
    procedure KeyDown     (var Key    : Word;
                               Shift  : TShiftState);   override;
    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button : TmcmRealSpinBtn
      read   FButton;
  published
    property Increment : integer
      read   FIncrement
      write  FIncrement default 1;
  end;

//------------------------------------------------------------------------------
// TmcmRealSpin.
//------------------------------------------------------------------------------

  TmcmRealSpin = class(TmcmRealEdit)
  private
    // Private declarations
    FIncrement : Extended;
    FButton    : TmcmRealSpinBtn;
    function  GetMinHeight : integer;
    procedure SetEditRect;
    procedure WMSize (var Message : TWMSize);     message WM_SIZE;
    procedure WMPaste(var Message : TWMPaste);    message WM_PASTE;
    procedure WMCut  (var Message : TWMCut);      message WM_CUT;
  protected
    // Protected declarations
    procedure UpClick     (    Sender : TObject); virtual;
    procedure DownClick   (    Sender : TObject); virtual;
    procedure KeyDown     (var Key    : Word;
                               Shift  : TShiftState);   override;
    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWnd; override;
  public
    // Public declarations
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button : TmcmRealSpinBtn
      read   FButton;
  published
    // Published declarations
    property Increment : extended
      read   FIncrement
      write  FIncrement;
  end;

procedure Register;


implementation

{$IFOPT T+} {$DEFINE TYPED_ADDRESS_ON} {$T-} {$ENDIF}
{$IFOPT X-} {$DEFINE EXTENDED_SYNTAX} {$X+} {$ENDIF}

{$IFDEF CLR} // VCL .NET
uses System.Reflection,
     System.Runtime.InteropServices,
     Themes;

{$R 'umcmIntE.resources'}
{$R 'umcmIntE.TmcmIntEdit.bmp'}
{$R 'umcmIntE.TmcmIntSpin.bmp'}
{$R 'umcmIntE.TmcmRealEdit.bmp'}
{$R 'umcmIntE.TmcmRealSpin.bmp'}

const
  ResourceBaseName = 'umcmIntE'; { Do not localize }

{$ELSE} // VCL
{$ENDIF}
{$R RMCMSPIN.RES}
{R RMCMSPIN}

//------------------------------------------------------------------------------
// TmcmIntEdit.
//------------------------------------------------------------------------------


procedure Register;
begin
  RegisterComponents('Imaging Toolbox', [TmcmIntEdit]);
  RegisterComponents('Imaging Toolbox', [TmcmIntSpin]);
  RegisterComponents('Imaging Toolbox', [TmcmRealEdit]);
  RegisterComponents('Imaging Toolbox', [TmcmRealSpin]);
end; // Register.


constructor TmcmIntEdit.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  Text      := '0';
  Width     := 41;
  FValue    := 0;
  FMinValue := 0;
  FMaxValue := 0;
end; // TmcmIntEdit.Create.


function TmcmIntEdit.IsValidKey(Key : Char) : boolean;
begin
  {$IFNDEF UNICODE}
  Result := (Key in ['0'..'9', '-', '+']) or
            (Key = Chr(VK_BACK)) or
            (Key = Chr(VK_DELETE));
  {$ELSE}
  Result := CharInSet(Key, ['0'..'9', '-', '+']) or
            (Key = Chr(VK_BACK)) or
            (Key = Chr(VK_DELETE));
  {$ENDIF}
  {$IFDEF LATDXE}
  if (Key = FormatSettings.DecimalSeparator) or
  {$ELSE}
  if (Key = DecimalSeparator) or
  {$ENDIF}
     (Key = '.') or (Key = ',')
  then Result := False;
  if ((Key = '-') or (Key = '+')) and (GetSelStart > 0)
  then Result := False;
end; // TmcmIntEdit.IsValidKey.


procedure TmcmIntEdit.KeyPress(var Key : Char);
begin
  if IsValidKey(Key)
  then inherited KeyPress(Key)
  else begin
       MessageBeep(0);
       Key := #0;
  end;
end; // TmcmIntEdit.KeyPress.


function TmcmIntEdit.GetValue : longint;
begin
  if (Text <> '') and (Text <> '-') and (Text <> '+')
  then begin
       try
         Result := StrToInt(Text);
         if Result <> CheckValue(Result)
         then Result := CheckValue(Result);
       except
         Result := FMinValue;
       end;
  end
  else Result := FMinValue;
end; // TmcmIntEdit.GetValue.


procedure TmcmIntEdit.SetValue(NewValue : longint);
begin
  if (Value <> NewValue)
  then Text := IntToStr(CheckValue(NewValue));
end; // TmcmIntEdit.SetValue.


function TmcmIntEdit.CheckValue(NewValue : longint) : longint;
begin
  Result := NewValue;
  if ((FMinValue <= NewValue) and (NewValue <= FMaxValue)) or
     ((FMinValue  = 0) and (0 = FMaxValue))
  then Result := NewValue
  else begin
       if (FMinValue > NewValue)
       then Result := FMinValue
       else if (FMaxValue < NewValue)
            then Result := FMaxValue;
  end;
end; // TmcmIntEdit.CheckValue.


procedure TmcmIntEdit.CMExit(var Message : TCMExit);
var AValue : longint;
begin
  AValue := Value;
  inherited;
  if (CheckValue(Value) <> AValue)
  then SetValue(Value);
end; // TmcmIntEdit.CMExit.


procedure TmcmIntEdit.CMEnter(var Message : TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState)
  then SelectAll;
  inherited;
end; // TmcmIntEdit.CMEnter.


procedure TmcmIntEdit.Change;
var AValue : longint;
begin
  if (Text <> '')  and
     (Text <> '-') and
     (Text <> '+')
  then begin
       try
         if (ComponentState * [csLoading, csUpdating] = [])
         then Inherited Change;

         AValue := StrToInt(Text);
         if (AValue <> CheckValue(AValue))
         then SetValue(Value);
       except
       end;
  end;
end; // TmcmIntEdit.OnChange.


//------------------------------------------------------------------------------
// TmcmRealEdit.
//------------------------------------------------------------------------------

constructor TmcmRealEdit.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  Width     := 41;
  FDecimals := 2;
  FMinValue := 0;
  FMaxValue := 0;
  Text      := '1';
end; // TmcmRealEdit.Create.


function TmcmRealEdit.IsValidKey(Key : Char) : boolean;
var DecPos : integer;
begin
  {$IFNDEF UNICODE}
  Result := (Key in ['0'..'9', '-', '+']) or
            (Key = Chr(VK_BACK)) or
            (Key = Chr(VK_DELETE)) or
            //(Key = Chr(VK_RETURN)) or
            //(Key = Chr(VK_ESCAPE)) or
            (Key = DecimalSeparator);
  {$ELSE}
  Result := CharInSet(Key, ['0'..'9', '-', '+']) or
            (Key = Chr(VK_BACK)) or
            (Key = Chr(VK_DELETE)) or
            //(Key = Chr(VK_RETURN)) or
            //(Key = Chr(VK_ESCAPE)) or
            {$IFDEF LATDXE}
            (Key = FormatSettings.DecimalSeparator);
            {$ELSE}
            (Key = DecimalSeparator);
            {$ENDIF}
  {$ENDIF}

  {$IFDEF LATDXE}
  if (Key <> FormatSettings.DecimalSeparator) and
  {$ELSE}
  if (Key <> DecimalSeparator) and
  {$ENDIF}
  ((Key = ',') or (Key = '.'))
  then Result := False
  else begin
       DecPos := Pos(',', Text);
       if (DecPos = 0)
       then DecPos := Pos('.', Text);
       if (DecPos <> 0) and ((Key = ',') or (Key = '.'))
       then Result := False;
  end;
  if ((Key = '-') or (Key = '+')) and (GetSelStart > 0)
  then Result := False;
end; // TmcmRealEdit.IsValidKey.


procedure TmcmRealEdit.KeyPress(var Key : Char);
begin
  if IsValidKey(Key)
  then inherited KeyPress(Key)
  else begin
       MessageBeep(0);
       Key := #0;
  end;
end; // TmcmRealEdit.KeyPress.


function TmcmRealEdit.GetValue : extended;
begin
  if (Text <> '') and (Text <> '-') and (Text <> '+')
  then begin
       try
         Result := StrToFloat(Text);
         if Result <> CheckValue(Result)
         then Result := CheckValue(Result);
       except
         Result := FMinValue;
       end;
  end
  else Result := FMinValue;
end; // TmcmRealEdit.GetValue.


procedure TmcmRealEdit.SetValue(NewValue : extended);
var ValStr : String;
    DecPos : integer;
begin
  FValue := CheckValue(NewValue);

  ValStr := FloatToStrF(FValue, ffFixed, 18, FDecimals);

  {$IFDEF LATDXE}
  DecPos := Pos(FormatSettings.DecimalSeparator, ValStr);
  {$ELSE}
  DecPos := Pos(DecimalSeparator, ValStr);
  {$ENDIF}
  if (DecPos = 0)
  then begin
       DecPos := Pos(',', ValStr);
       if (DecPos = 0)
       then DecPos := Pos('.', ValStr);
  end;
  if (DecPos > 0)
  then begin
       if (FDecimals = 0)
       then dec(DecPos);
       if (FDecimals < (Length(ValStr) - DecPos))
       then ValStr := Copy(ValStr, 1, DecPos + FDecimals);
  end;
  {$IFDEF LATDXE}
  DecPos := Pos(FormatSettings.DecimalSeparator, ValStr);
  {$ELSE}
  DecPos := Pos(DecimalSeparator, ValStr);
  {$ENDIF}
  if (DecPos > 0)
  then begin
       while (ValStr[Length(ValStr)] = '0') and
             {$IFDEF LATDXE}
             (ValStr[Length(ValStr)-1] <> FormatSettings.DecimalSeparator)
             {$ELSE}
             (ValStr[Length(ValStr)-1] <> DecimalSeparator)
             {$ENDIF}
       do ValStr := Copy(ValStr, 1, Length(ValStr)-1);
  end;

  try
    Text := ValStr;
  except
    on e:Exception
    do ;
  end;
end; // TmcmRealEdit.SetValue.


function TmcmRealEdit.CheckValue(NewValue : extended) : extended;
begin
  Result := NewValue;
  if ((FMinValue <= NewValue) and (NewValue <= FMaxValue)) or
     ((FMinValue  =        0) and (0         = FMaxValue))
  then Result := NewValue
  else begin
       if (FMinValue > NewValue)
       then Result := FMinValue
       else if (FMaxValue < NewValue)
            then Result := FMaxValue;
  end;
end; // TmcmRealEdit.CheckValue.


procedure TmcmRealEdit.SetDecimals(Decimals : word);
begin
  Updating;
  if (Decimals < 13)
  then begin
       if (Decimals > 0)
       then FDecimals := Decimals
       else FDecimals := 0; // 1;
       SetValue(GetValue);
  end
  else FDecimals := 12;
  Updated;
end; // TmcmRealEdit.SetDecimals.


procedure TmcmRealEdit.CMExit(var Message : TCMExit);
var AValue : extended;
begin
  AValue := Value;
  inherited;
  if (CheckValue(Value) <> AValue)
  then SetValue(Value);
end; // TmcmRealEdit.CMExit.


procedure TmcmRealEdit.CMEnter(var Message : TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState)
  then SelectAll;
  inherited;
end; // TmcmRealEdit.CMEnter.


procedure TmcmRealEdit.Change;
var AValue : extended;
begin
  if Assigned(Parent) 
  then if (ComponentState * [csLoading, csUpdating] = [])
       then Inherited Change;
  if (Text <> '') and (Text <> '-') and (Text <> '+')
  then begin
       try
         AValue := StrToFloat(Text);
         if (AValue <> CheckValue(AValue))
         then SetValue(Value);
       except
       end;
  end;
end; // TmcmRealEdit.OnChange.


//------------------------------------------------------------------------------
// TmcmTimeSpeedBtn.
//------------------------------------------------------------------------------

destructor TmcmTimeSpeedBtn.Destroy;
begin
  if (FmcmRepeatTimer <> nil)
  then FmcmRepeatTimer.Free;
  inherited Destroy;
end; // TmcmTimeSpeedBtn.Destroy.


procedure TmcmTimeSpeedBtn.MouseDown(Button : TMouseButton;
                                     Shift  : TShiftState;
                                     X, Y   : integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if tbAllowTimer in FmcmTimeBtnState
  then begin
       if (FmcmRepeatTimer = nil)
       then FmcmRepeatTimer := TTimer.Create(Self);
       FmcmRepeatTimer.OnTimer  := TimerExpired;
       FmcmRepeatTimer.Interval := mcmInitRepeatPause;
       FmcmRepeatTimer.Enabled  := True;
  end;
end; // TmcmTimeSpeedBtn.MouseDown.


procedure TmcmTimeSpeedBtn.MouseUp(Button : TMouseButton;
                                   Shift  : TShiftState;
                                   X, Y   : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (FmcmRepeatTimer <> nil)
  then FmcmRepeatTimer.Enabled := False;
end; // TmcmTimeSpeedBtn.MouseUp.


procedure TmcmTimeSpeedBtn.TimerExpired(Sender : TObject);
begin
  FmcmRepeatTimer.Interval := mcmRepeatPause;
  if (FState = bsDown) and MouseCapture
  then begin
       try
         Click;
       except
         FmcmRepeatTimer.Enabled := False;
       raise;
       end;
  end;
end; // TmcmTimeSpeedBtn.TimerExpired.


procedure TmcmTimeSpeedBtn.Paint;
var R : TRect;
begin
  inherited Paint;
  if tbFocusRect in FmcmTimeBtnState
  then begin
       R := Bounds(0, 0, Width, Height);
       InflateRect(R, -3, -3);
       if (FState = bsDown)
       then OffsetRect(R, 1, 1);
       DrawFocusRect(Canvas.Handle, R);
  end;
end; // TmcmTimeSpeedBtn.Paint.


{------------------------------------------------------------------------------}
{ TmcmRealSpinBtn.                                                             }
{------------------------------------------------------------------------------}

constructor TmcmRealSpinBtn.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];
  FUpButton    := CreateButton;
  FUpButton.Flat := False;
  FDownButton  := CreateButton;
  FDownButton.Flat := False;
  UpGlyph      := Nil;
  DownGlyph    := Nil;
  Width        := 20;
  Height       := 25;
  FFocusedButton := FUpButton;
end; // TmcmRealSpinBtn.Create.


function TmcmRealSpinBtn.CreateButton : TmcmTimeSpeedBtn;
begin
  Result := TmcmTimeSpeedBtn.Create(Self);
  Result.OnClick      := BtnClick;
  Result.OnMouseDown  := BtnMouseDown;
  Result.Visible      := True;
  Result.Enabled      := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.NumGlyphs    := 1;
  Result.Parent       := Self;
end; // TmcmRealSpinBtn.CreateButton.


procedure TmcmRealSpinBtn.Adjust_Size(var W : integer;
                                      var H : integer);
begin
  if (FUpButton = Nil) or (csLoading in ComponentState)
  then Exit;
  if (W < 15)
  then W := 15;
  FUpButton.SetBounds(0, 1, W, H div 2);
  FDownButton.SetBounds(0, FUpButton.Height, W, H - FUpButton.Height);
end; // TmcmRealSpinBtn.Adjust_Size.


procedure TmcmRealSpinBtn.SetBounds(ALeft, ATop, AWidth, AHeight : integer);
var W, H : integer;
begin
  W := AWidth;
  H := AHeight;
  Adjust_Size(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end; // TmcmRealSpinBtn.SetBounds.


procedure TmcmRealSpinBtn.WMSize(var Message : TWMSize);
var W, H : integer;
begin
  inherited;
  // check for minimum size
  W := Width;
  H := Height;
  Adjust_Size(W, H);
  if (W <> Width) or (H <> Height)
  then inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end; // TmcmRealSpinBtn.WMSize.


procedure TmcmRealSpinBtn.WMSetFocus(var Message : TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end; // TmcmRealSpinBtn.WMSetFocus.


procedure TmcmRealSpinBtn.WMKillFocus(var Message : TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end; // TmcmRealSpinBtn.WMKillFocus.


procedure TmcmRealSpinBtn.KeyDown(var Key : Word; Shift: TShiftState);
begin
  case Key of
  VK_UP    : begin
               SetFocusBtn(FUpButton);
               FUpButton.Click;
             end;
  VK_DOWN  : begin
               SetFocusBtn(FDownButton);
               FDownButton.Click;
             end;
  VK_SPACE : FFocusedButton.Click;
  end;
end; // TmcmRealSpinBtn.KeyDown.


procedure TmcmRealSpinBtn.BtnMouseDown(Sender : TObject;
                                       Button : TMouseButton;
                                       Shift  : TShiftState;
                                       X, Y   : integer);
begin
  if (Button = mbLeft)
  then begin
       SetFocusBtn(TmcmTimeSpeedBtn(Sender));
       if (FFocusControl <> nil) and
           FFocusControl.TabStop and
           FFocusControl.CanFocus and
          (FFocusControl.Handle <> GetFocus)
       then FFocusControl.SetFocus
       else if TabStop and (GetFocus <> Handle) and CanFocus
            then SetFocus;
  end;
end; // TmcmRealSpinBtn.BtnMouseDown.


procedure TmcmRealSpinBtn.BtnClick(Sender : TObject);
begin
  if (Sender = FUpButton)
  then begin
       if Assigned(FOnUpClick)
       then FOnUpClick(Self);
  end
  else if Assigned(FOnDownClick)
       then FOnDownClick(Self);
end; // TmcmRealSpinBtn.BtnClick.


procedure TmcmRealSpinBtn.SetFocusBtn(Btn : TmcmTimeSpeedBtn);
begin
  if TabStop and CanFocus and (Btn <> FFocusedButton)
  then begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
       FFocusedButton := Btn;
       if (GetFocus = Handle)
       then begin
            FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
            Invalidate;
       end;
  end;
end; // TmcmRealSpinBtn.SetFocusBtn.


procedure TmcmRealSpinBtn.WMGetDlgCode(var Message : TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end; // TmcmRealSpinBtn.WMGetDlgCode.


procedure TmcmRealSpinBtn.Loaded;
var W, H : integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  Adjust_Size(W, H);
  if (W <> Width) or (H <> Height)
  then inherited SetBounds(Left, Top, W, H);
end; // TmcmRealSpinBtn.Loaded.


function TmcmRealSpinBtn.GetUpGlyph : TBitmap;
begin
  Result := FUpButton.Glyph;
end; // TmcmRealSpinBtn.GetUpGlyph.


procedure TmcmRealSpinBtn.SetUpGlyph(Value : TBitmap);
begin
  if (Value <> nil)
  then FUpButton.Glyph := Value
  else begin
       {$IFDEF CLR} // VCL .NET
         FUpButton.Glyph.LoadFromResourceName('SpinUp.bmp', ResourceBaseName, Assembly.GetExecutingAssembly);
       {$ELSE} // VCL
         FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'mcmSpinUp');
         FUpButton.Margin := -1;
         FUpButton.Spacing := -1;
       {$ENDIF}
       FUpButton.Invalidate;
  end;
end; // TmcmRealSpinBtn.SetUpGlyph.


function TmcmRealSpinBtn.GetDownGlyph : TBitmap;
begin
  Result := FDownButton.Glyph;
end; // TmcmRealSpinBtn.GetDownGlyph.


procedure TmcmRealSpinBtn.SetDownGlyph(Value : TBitmap);
begin
  if (Value <> nil)
  then FDownButton.Glyph := Value
  else begin
       {$IFDEF CLR} // VCL .NET
        FDownButton.Glyph.LoadFromResourceName('SpinDown.bmp', ResourceBaseName, Assembly.GetExecutingAssembly);
       {$ELSE} // VCL
         FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'mcmSpinDown');
         FDownButton.Margin := -1;
         FDownButton.Spacing := -1;
       {$ENDIF}
       FDownButton.Invalidate;
  end;
end; // TmcmRealSpinBtn.SetDownGlyph.


//------------------------------------------------------------------------------
// TmcmIntSpin.
//------------------------------------------------------------------------------

constructor TmcmIntSpin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width                := 54;
  FButton              := TmcmRealSpinBtn.Create(Self);
  FButton.Width        := 15;
  FButton.Height       := 17;
  FButton.Visible      := True;
  FButton.Parent       := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick    := UpClick;
  FButton.OnDownClick  := DownClick;
  ControlStyle         := ControlStyle - [csSetCaption];
  FIncrement           := 1;
end; // TmcmIntSpin.Create.


destructor TmcmIntSpin.Destroy;
begin
  if Assigned(FButton)
  then FButton.Free;
  FButton := Nil;
  inherited Destroy;
end; // TmcmIntSpin.Destroy.


procedure TmcmIntSpin.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style (*or ES_MULTILINE *) or WS_CLIPCHILDREN;
end; // TmcmIntSpin.CreateParams.


procedure TmcmIntSpin.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end; // TmcmIntSpin.CreateWnd.


function TmcmIntSpin.GetMinHeight : integer;
var DC         : HDC;
    SaveFont   : HFont;
    i          : integer;
    SysMetrics : TTextMetric;
    Metrics    : TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  i := SysMetrics.tmHeight;
  if (i > Metrics.tmHeight)
  then i := Metrics.tmHeight;
  Result := Metrics.tmHeight + i div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end; // TmcmIntSpin.GetMinHeight.


procedure TmcmIntSpin.SetEditRect;
var Loc : TRect;
    {$IFDEF CLR} // VCL .NET
    p   : IntPtr;
    {$ELSE}
    p   : integer;
    {$ENDIF}
begin
  {$IFDEF CLR} // VCL .NET
    p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TRect)));
    Marshal.StructureToPtr(Loc, p, false);
  {$ELSE}
    p := integer(@Loc);
  {$ENDIF}
  SendMessage(Handle, EM_GETRECT, 0, Integer(p));
  Loc.Bottom := ClientHeight + 1;
  Loc.Right  := ClientWidth - FButton.Width - 2;
  Loc.Top    := 0;
  Loc.Left   := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, Integer(p));
  SendMessage(Handle, EM_GETRECT, 0, Integer(p));
end; // TmcmIntSpin.SetEditRect.


procedure TmcmIntSpin.WMSize(var Message : TWMSize);
var MinHeight : Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  // text edit bug: if size to less than minheight, then edit ctrl does not
  // display the text.
  if (Height < MinHeight)
  then Height := MinHeight
  else begin
       if (FButton <> Nil)
       then begin
            if (NewStyleControls and Ctl3D)
            then FButton.SetBounds(Width - FButton.Width - 4, -1, FButton.Width, Height - 3)
            else FButton.SetBounds(Width - FButton.Width, 1, FButton.Width, Height - 3);
            SetEditRect;
       end;
  end;
end; // TmcmIntSpin.WMSize.


procedure TmcmIntSpin.WMPaste(var Message : TWMPaste);
begin
  if {not FEditorEnabled or} ReadOnly
  then Exit;
  inherited;
end; // TmcmIntSpin.WMPaste.


procedure TmcmIntSpin.WMCut(var Message : TWMCut);
begin
  if {not FEditorEnabled or} ReadOnly
  then Exit;
  inherited;
end; // TmcmIntSpin.WMCut.


procedure TmcmIntSpin.UpClick(Sender : TObject);
begin
  if ReadOnly
  then MessageBeep(0)
  else Value := Value + FIncrement;
end; // TmcmIntSpin.UpClick.


procedure TmcmIntSpin.DownClick(Sender : TObject);
begin
  if ReadOnly
  then MessageBeep(0)
  else Value := Value - FIncrement;
end; // TmcmIntSpin.DownClick.


procedure TmcmIntSpin.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if (Key = VK_UP)
  then UpClick(Self)
  else if (Key = VK_DOWN)
       then DownClick(Self);
  inherited KeyDown(Key, Shift);
end; // TmcmIntSpin.KeyDown.

//------------------------------------------------------------------------------
// TmcmRealSpin.
//------------------------------------------------------------------------------

constructor TmcmRealSpin.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width                := 54;
  FButton              := TmcmRealSpinBtn.Create(Self);
  FButton.Width        := 15;
  FButton.Height       := 17;
  FButton.Visible      := True;
  FButton.Parent       := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick    := UpClick;
  FButton.OnDownClick  := DownClick;
  ControlStyle         := ControlStyle - [csSetCaption];
  FIncrement           := 1.0;
end; // TmcmRealSpin.Create.


destructor TmcmRealSpin.Destroy;
begin
  if Assigned(FButton)
  then FButton.Free;
  FButton := Nil;
  inherited Destroy;
end; // TmcmRealSpin.Destroy.


procedure TmcmRealSpin.KeyDown(var Key   : Word;
                                   Shift : TShiftState);
begin
  if (Key = VK_UP)
  then UpClick(Self)
  else if (Key = VK_DOWN)
       then DownClick(Self);
  inherited KeyDown(Key, Shift);
end; // TmcmRealSpin.KeyDown.


procedure TmcmRealSpin.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style (*or ES_MULTILINE *) or WS_CLIPCHILDREN;
end; // TmcmRealSpin.CreateParams.


procedure TmcmRealSpin.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end; // TmcmRealSpin.CreateWnd.


procedure TmcmRealSpin.SetEditRect;
var Loc : TRect;
    {$IFDEF CLR} // VCL .NET
    p   : IntPtr;
    {$ELSE}
    p   : integer;
    {$ENDIF}
begin
  {$IFDEF CLR} // VCL .NET
    p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TRect)));
    Marshal.StructureToPtr(Loc, p, false);
  {$ELSE}
    p := integer(@Loc);
  {$ENDIF}
  SendMessage(Handle, EM_GETRECT, 0, Integer(p));
  Loc.Bottom := ClientHeight + 1;
  Loc.Right  := ClientWidth - FButton.Width - 2;
  Loc.Top    := 0;
  Loc.Left   := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, Integer(p));
  SendMessage(Handle, EM_GETRECT, 0, Integer(p));
end; // TmcmRealSpin.SetEditRect.


procedure TmcmRealSpin.WMSize(var Message : TWMSize);
var MinHeight : Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  // text edit bug: if size to less than minheight, then edit ctrl does not
  // display the text.
  if (Height < MinHeight)
  then Height := MinHeight
  else begin
       if (FButton <> Nil)
       then begin
            if (NewStyleControls and Ctl3D)
            then FButton.SetBounds(Width - FButton.Width - 4, -1, FButton.Width, Height - 3)
            else FButton.SetBounds(Width - FButton.Width, 1, FButton.Width, Height - 3);
            SetEditRect;
       end;
  end;
end; // TmcmRealSpin.WMSize.


function TmcmRealSpin.GetMinHeight : integer;
var DC         : HDC;
    SaveFont   : HFont;
    i          : integer;
    SysMetrics : TTextMetric;
    Metrics    : TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  i := SysMetrics.tmHeight;
  if (i > Metrics.tmHeight)
  then i := Metrics.tmHeight;
  Result := Metrics.tmHeight + i div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end; // TmcmRealSpin.GetMinHeight.


procedure TmcmRealSpin.UpClick(Sender : TObject);
begin
  if ReadOnly
  then MessageBeep(0)
  else Value := Value + FIncrement;
end; // TmcmRealSpin.UpClick.


procedure TmcmRealSpin.DownClick(Sender : TObject);
begin
  if ReadOnly
  then MessageBeep(0)
  else Value := Value - FIncrement;
end; // TmcmRealSpin.DownClick.


procedure TmcmRealSpin.WMPaste(var Message : TWMPaste);
begin
  if {not FEditorEnabled or} ReadOnly
  then Exit;
  inherited;
end; // TmcmRealSpin.WMPaste.


procedure TmcmRealSpin.WMCut(var Message : TWMPaste);
begin
  if {not FEditorEnabled or} ReadOnly
  then Exit;
  inherited;
end; // TmcmRealSpin.WMCut.

{$IFDEF TYPED_ADDRESS_ON} {$T+} {$UNDEF TYPED_ADDRESS_ON} {$ENDIF}
{$IFDEF EXTENDED_SYNTAX} {$X-} {$UNDEF EXTENDED_SYNTAX} {$ENDIF}

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
