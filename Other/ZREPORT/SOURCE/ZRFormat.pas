unit ZRFormat;

interface

{$I ZRDefine.inc}

uses
  SysUtils,                                         // Delphi RTL
{$IFDEF D6Above}
  Variants,
  MaskUtils,
{$ENDIF}
  Classes, Mask;                                    // Delphi VCL

type
  TZValueKind = (zvkError, zvkNull, zvkString, zvkInteger, zvkFloat, zvkCurrency, zvkBoolean, zvkDateTime);

function ValueKind(const Value: Variant): TZValueKind;
function ValueIsEmpty(const Value: Variant): Boolean;


type
  { TZFormat }
  TZFormat = class(TPersistent)
  private
    fDisplayMask    : String;
    fFloatFormat    : TFloatFormat;
    fWidth,
    fDigits         : Integer;
    fBlankIfZero    : Boolean;
    fOnChange       : TNotifyEvent;
    procedure SetBlankIfZero(Value: Boolean);
    procedure SetFloatFormat(Value: TFloatFormat);
    procedure SetDisplayMask(Value: String);
    procedure SetDigits(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function Format(const Value: Variant): String;
  published
    property BlankIfZero    : Boolean read fBlankIfZero write SetBlankIfZero default False;
    property FloatFormat    : TFloatFormat read fFloatFormat write SetFloatFormat default ffGeneral;
    property Digits         : Integer read fDigits write SetDigits default 0;
    property DisplayMask    : String read fDisplayMask write SetDisplayMask;
    property Width          : Integer read fWidth write SetWidth default 16;
    property OnChange       : TNotifyEvent read fOnChange write fOnChange;
  end;

const
  UseCurrencyDecimals: Boolean = True;

implementation

function ValueKind(const Value: Variant): TZValueKind;
const
  KindMap: array[0..varByte] of TZValueKind = (
    zvkError,    { varEmpty    }
    zvkNull,     { varNull     }
    zvkInteger,  { varSmallint }
    zvkInteger,  { varInteger  }
    zvkFloat,    { varSingle   }
    zvkFloat,    { varDouble   }
    zvkCurrency, { varCurrency }
    zvkDateTime, { varDate     }
    zvkString,   { varOleStr   }
    zvkError,    { varDispatch }
    zvkError,    { varError    }
    zvkBoolean,  { varBoolean  }
    zvkError,    { varVariant  }
    zvkError,    { varUnknown  }
    zvkError,    { Undefined   }
    zvkError,    { Undefined   }
    zvkError,    { Undefined   }
    zvkInteger); { varByte     }
var
  VT: Word;
begin
  VT:= VarType(Value);
  if (VT and varArray > 0) then Result:= zvkError else
  if (VT = varString) then Result:= zvkString else Result:= KindMap[VT];
end;

function ValueIsEmpty(const Value: Variant): Boolean;
var
  Kind: TZValueKind;
begin
  Kind:= ValueKind(Value);
  Result:= (Kind in [zvkError, zvkNull]) or
          ((Kind in [zvkInteger, zvkFloat, zvkCurrency]) and (Value = 0)) or
          ((Kind = zvkString) and (Trim(Value) = ''));
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                 TZRFormat                              !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZFormat.Create;
begin
  inherited;
  fFloatFormat:= ffGeneral;
  fWidth      := 16;
end;

procedure TZFormat.Assign(Source: TPersistent);
begin
  if Source is TZFormat then begin
    fDisplayMask:= (Source as TZFormat).fDisplayMask;
    fFloatFormat:= (Source as TZFormat).fFloatFormat;
    fWidth      := (Source as TZFormat).fWidth;
    fDigits     := (Source as TZFormat).fDigits;
    fBlankIfZero:= (Source as TZFormat).fBlankIfZero;
    Changed;
  end else
    inherited;
end;

procedure TZFormat.Changed;
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TZFormat.SetBlankIfZero(Value: Boolean);
begin
  if BlankIfZero <> Value then begin
    fBlankIfZero:= Value;
    Changed;
  end;
end;
procedure TZFormat.SetFloatFormat(Value: TFloatFormat);
begin
  if FloatFormat <> Value then begin
    fFloatFormat:= Value;
    Changed;
  end;
end;
procedure TZFormat.SetDisplayMask(Value: String);
begin
  if DisplayMask <> Value then begin
    fDisplayMask:= Value;
    Changed;
  end;
end;
procedure TZFormat.SetDigits(Value: Integer);
begin
  if Digits <> Value then begin
    fDigits:= Value;
    Changed;
  end;
end;
procedure TZFormat.SetWidth(Value: Integer);
begin
  if Width <> Value then begin
    fWidth:= Value;
    Changed;
  end;
end;

function TZFormat.Format(const Value: Variant): String;
var
  UseMask  : Boolean;
  Separator: Integer;
begin
  if BlankIfZero and ValueIsEmpty(Value) then
    Result:= ''
  else begin
    UseMask:= Trim(DisplayMask) <> '';
    case ValueKind(Value) of
      zvkString  : if UseMask then
                     Result:= FormatMaskText(DisplayMask, Value)
                   else
                     Result:= Value;
      zvkInteger,
      zvkFloat,
      zvkCurrency: if UseMask then
                     Result:= FormatFloat(DisplayMask, Value)
                   else
                   if (FloatFormat = ffCurrency) and UseCurrencyDecimals then
                     Result:= FloatToStrF(Value, FloatFormat, Width, CurrencyDecimals)
                   else
                     Result:= FloatToStrF(Value, FloatFormat, Width, Digits);
      zvkDateTime: if UseMask then
                     Result:= FormatDateTime(DisplayMask, Value)
                   else
                     Result:= DateTimeToStr(Value);
      zvkBoolean : if UseMask then begin
                     Separator:= Pos(';', DisplayMask);
                     if Separator = 0 then Separator:= length(DisplayMask);
                     if Value then
                       Result:= copy(DisplayMask, 1, Separator)
                     else
                       Result:= copy(DisplayMask, Separator, length(DisplayMask));
                   end else
                     if Value then Result := '+' else Result := '-';
      else Result:= '';
    end;
  end;
end;

end.

