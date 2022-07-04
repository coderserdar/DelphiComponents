///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMFloatEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin;

{This simple descendant of TSpinEdit works with extended numbers}

type
  TFloatEdit=class(TSpinEdit)
  private
    { Private declarations }
    FIncrement,FMinValue,FMaxValue: extended;
    FMinWidth, FDecimals: integer;
    FFType: TFloatFormat;
    FMultiply: boolean;
    procedure SetValue(NewValue: extended);
    function GetValue: extended;
    function CheckValue(NewValue: extended): extended;
    procedure SetFType(T: TFloatFormat);
    procedure SetMinWidth(I: integer);
    procedure SetDecimals(I: integer);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
 protected
    { Protected declarations }
    function IsValidChar(Key: Char): Boolean; override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value: extended read GetValue write SetValue;
    property FType: TFloatFormat read FFType write SetFType;
    property MinWidth: integer read FMinWidth write SetMinWidth;
    property Decimals: integer read FDecimals write SetDecimals;
    property Increment: extended read FIncrement write FIncrement;
    property MinValue: extended read FMinValue write FMinValue;
    property MaxValue: extended read FMaxValue write FMaxValue;
    property Multiply: boolean read FMultiply write FMultiply;
  end;

procedure Register;

implementation

function TFloatEdit.GetValue: extended;
begin
  try
    Result:=StrToFloat(Text);
  except
    Result:=FMinValue;
  end;
end;

procedure TFloatEdit.SetValue(NewValue: extended);
begin
  Text:=FloatToStrF(CheckValue(NewValue), FFType, FMinWidth, FDecimals);
end;

procedure TFloatEdit.SetFType(T: TFloatFormat);
begin 
  if T<>FFType then 
  begin 
    FFType:=T; 
    SetValue(Value); 
  end; 
end;

procedure TFloatEdit.SetMinWidth(I: integer);
begin 
  if I<>FMinWidth then 
  begin 
    FMinWidth:=I; 
    SetValue(Value); 
  end; 
end;

procedure TFloatEdit.SetDecimals(I: integer);
begin 
  if I<>FDecimals then 
  begin 
     FDecimals:=I; 
     SetValue(Value); 
  end; 
end;

function TFloatEdit.CheckValue(NewValue: extended): extended;
begin
  Result:=NewValue;
  if (FMaxValue<>FMinValue) then
  begin
    if NewValue<FMinValue 
    then Result:=FMinValue else
      if NewValue>FMaxValue 
      then Result:=FMaxValue;
  end;
end;

constructor TFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncrement:=1; 
  FFType:=ffGeneral; 
  FMinWidth:=10; 
  FDecimals:=4;
  FMultiply:=false;
end;

function TFloatEdit.IsValidChar(Key: Char): Boolean;
begin
  Result:=(Key in [DecimalSeparator, '+', '-', '0'..'9', 'e', 'E']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not EditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) 
  then Result:=False;
end;

procedure TFloatEdit.UpClick (Sender: TObject);
begin
  if ReadOnly 
  then MessageBeep(0) else
    if Multiply 
    then Value:=Value*FIncrement 
    else Value:=Value+FIncrement;
end;

procedure TFloatEdit.DownClick (Sender: TObject);
begin
  if (FIncrement=0) or ReadOnly 
  then MessageBeep(0) else
    if Multiply 
    then Value:=Value/FIncrement 
    else Value:=Value-FIncrement;
end;

procedure TFloatEdit.CMExit(var Message: TCMExit);
begin
  DoExit;
  {^support inherited behavior, i.e. OnExit; but "inherited;" cause integer
  conversion error if text doesn't present integer! (see spinedit)}
  if CheckValue(Value)<>Value then SetValue(Value);
end;

{component registration - this unit may be used separately from dm2000}
procedure Register;
begin 
  RegisterComponents('DM2003', [TFloatEdit]); 
end;

end.
