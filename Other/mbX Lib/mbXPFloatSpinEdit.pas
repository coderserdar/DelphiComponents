unit mbXPFloatSpinEdit;

interface

uses
 Windows, Classes, StdCtrls, Controls, Messages, SysUtils, mbXPSpinEdit;

type
  TmbXPFloatSpinEdit = class(TmbXPCustomSpinEdit)
  private
    FPrecision, FDigits: Integer;
    FFloatFormat: TFloatFormat;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FEditorEnabled: Boolean;
    FDefaultValue: Extended;

    function GetValue: Extended;
    function CheckValue(Value: Extended): Extended;
    procedure SetDefaultValue(v: Extended);
    procedure SetValue (Value: Extended);
    procedure SetPrecision (Value: Integer);
    procedure SetDigits (Value: Integer);
    procedure SetFloatFormat (Value: TFloatFormat);
  protected
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property AllowEmpty;
    property Empty;
    property Text;
    property AutoSelect;
    property AutoSize;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DefaultValue: extended read FDefaultValue write SetDefaultValue;
    property Digits: Integer read FDigits write SetDigits;
    property Precision: Integer read FPrecision write SetPrecision;
    property FloatFormat: TFloatFormat read FFloatFormat write SetFloatFormat default ffGeneral;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property Value: Extended read GetValue write SetValue;  
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
    property OnStartDrag;
  end;

implementation

constructor TmbXPFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0' + DecimalSeparator + '00';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 0.5;
  FEditorEnabled := True;
  OnChange := SpinChanged;
  FDefaultValue := 0;
  FDigits := 2;
  FPrecision := 9;
  FFloatFormat := ffGeneral;
end;

procedure TmbXPFloatSpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then
   MessageBeep(0)
  else
   begin
    if Text = '' then Text := FloatToStr(FDefaultValue);
    Value := Value + FIncrement;
   end;
end;

procedure TmbXPFloatSpinEdit.DownClick (Sender: TObject);
begin
  if ReadOnly then
   MessageBeep(0)
  else
   begin
    if Text = '' then Text := FloatToStr(FDefaultValue);
    Value := Value - FIncrement;
   end;
end;

function TmbXPFloatSpinEdit.GetValue: Extended;
var
 s: string;
begin
 try
  s := Text;
  if (s = '') and AllowEmpty then
   begin
    Result := FDefaultValue;
    Exit;
   end;
  while Pos(CurrencyString, S) > 0 do
   Delete(S, Pos(CurrencyString, S), Length(CurrencyString));
  while Pos(' ', S) > 0 do
   Delete(S, Pos(' ', S), 1);
  while Pos(ThousandSeparator, S) > 0 do
   Delete(S, Pos(ThousandSeparator, S), Length(ThousandSeparator));
  //Delete negative numbers in format Currency
  if Pos('(', S) > 0 then
   begin
    Delete(S, Pos('(', S), 1);
    if Pos(')', S) > 0 then
     Delete(S, Pos(')', S), 1);
    Result := StrToFloat(S)*-1;
   end
  else
   Result := StrToFloat(S);
  except
    Result := FMinValue;
  end;
end;

procedure TmbXPFloatSpinEdit.SetFloatFormat(Value: TFloatFormat);
begin
 FFloatFormat := Value;
 Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TmbXPFloatSpinEdit.SetDigits(Value: Integer);
begin
 FDigits := Value;
 Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TmbXPFloatSpinEdit.SetPrecision(Value: Integer);
begin
 FPrecision := Value;
 Text := FloatToStrF(CheckValue(GetValue), FloatFormat, Precision, Digits);
end;

procedure TmbXPFloatSpinEdit.CMExit(var Message: TCMExit);
var
 FWasEmpty: boolean;
begin
  inherited;
  FWasEmpty := false;
  if Text = '' then
   begin
    Text := FloatToStr(FDefaultValue);
    FWasEmpty := true;
   end;
  if CheckValue(Value) <> Value then
   SetValue(Value);
  if Empty and AllowEmpty and FWasEmpty then
   Text := '';
end;

procedure TmbXPFloatSpinEdit.SetValue(Value: Extended);
begin
 Text := FloatToStrF(CheckValue(Value), FloatFormat, Precision, Digits);
end;

function TmbXPFloatSpinEdit.CheckValue(Value: Extended): Extended;
begin
  Result := Value;
  if (FMaxValue <> FMinValue) then
  begin
    if Value < FMinValue then
      Result := FMinValue
    else
      if Value > FMaxValue then
        Result := FMaxValue;
  end;
end;

procedure TmbXPFloatSpinEdit.SetDefaultValue(v: extended);
begin
 FDefaultValue := v;
 if FDefaultValue < FMinValue then FDefaultValue := FMinValue;
 if FDefaultValue > FMaxValue then FDefaultValue := FMaxValue;
 SetValue(v);
end;

end.
