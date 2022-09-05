{Author:	Poul Bak}
{}
{Copyright © 1999 - 2003 : BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.50.00.00}
{}
{PBBinHexEdit is a special Edit-component for Binary, Hexadecimal and integer(64)
 editing, display and conversion.}
{}
{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}

unit PBBinHexEdit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls;

type
{Number = standard integer(64) format.}
{Binary = number with only 0 and 1 like '0110'.
{HexaDecimal = number with hexadecimal format like $7FFFFFFF.}
	TBaseFormat = (Number, Binary, HexaDecimal);

{Author:	Poul Bak}
{}
{Copyright © 1999 - 2003 : BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.50.00.00}
{}
{PBBinHexEdit is a special Edit-component for Binary, Hexadecimal and integer(64)
 editing, display and conversion.}
{}
{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}
	TPBBinHexEdit = class(TCustomEdit)
	private
		{ Private declarations }
		FAlignment: TAlignment;
		FBaseFormat : TBaseFormat;
		FEnter, FParentColor : Boolean;
		FInvalidEntry: TNotifyEvent;
		FBinLength, FHexlength : integer;
{$IFDEF VER100}
		FMaxValue: Integer;
		FMinValue: Integer;
{$ELSE}
		FMaxValue: Int64;
		FMinValue: Int64;
{$ENDIF}
		FOnClear : TNotifyEvent;
		FVersion: String;
		FDisabledColor, FEnabledColor : TColor;
{$IFDEF VER100}
		function BinToInt(B : string): integer;
		function FormatText(Value: Integer; NFormat: TBaseFormat): string;
		function GetAsInteger: Integer;
		function IntToBin(I : integer): string;
		procedure SetAsInteger(Value: Integer);
		procedure SetMaxValue(Value: Integer);
		procedure SetMinValue(Value: Integer);
{$ELSE}
		function BinToInt(B : string): Int64;
		function FormatText(Value: Int64; NFormat: TBaseFormat): string;
		function GetAsInteger: Int64;
		function IntToBin(I : Int64): string;
		procedure SetAsInteger(Value: Int64);
		procedure SetMaxValue(Value: Int64);
		procedure SetMinValue(Value: Int64);
{$ENDIF}
		function GetAsBin: string;
		function GetAsHex: string;
		procedure SetParentColor(Value : Boolean);
		procedure InvalidEntry;
		procedure SetColor(Value : TColor);
		procedure SetDisabledColor(Value : TColor);
		procedure SetAlignment(Value: TAlignment);
		procedure SetAsBin(Value: string);
		procedure SetAsHex(Value: string);
		procedure SetBaseFormat(Value: TBaseFormat);
		procedure SetVersion(Value: String);
		procedure WMClear(var Msg : TMessage); message WM_CLEAR;
		procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
		procedure WMPaste(var Message: TMessage); message WM_PASTE;
		procedure CMPARENTCOLORCHANGED(var M:TMessage); message CM_PARENTCOLORCHANGED;
	protected
		{ Protected declarations }
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure DoEnter; override;
		procedure DoExit; override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyPress(var Key: Char); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
		procedure SetEnabled(Value : Boolean); override;
		procedure Loaded; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure CreateParams(var Params: TCreateParams); override;
	published		{ Published declarations }
{Set Alignment to: taLeftJustify, taCenter or taRightJustify.}
{Default : taLeftJustify.}
{Supports Windows 95, 98 and NT.}
		property Alignment: TAlignment read FAlignment write SetAlignment;
{Set or access the value as a binary string: 1010101010}
		property AsBin: string read GetAsBin write SetAsBin;
{Set or access the value as an integer type (normal number)}
{$IFDEF VER100}
		property AsInteger: Integer read GetAsInteger write SetAsInteger;
{$ELSE}
		property Anchors;
		property AsInteger: Int64 read GetAsInteger write SetAsInteger;
{$ENDIF}
{Set or access the value as a Hexadecimal string: $FFFFFFFF}
		property AsHex: string read GetAsHex write SetAsHex;
{Default: True.}
{Set AutoSelect to True to select all text when you set focus:}
{Notice that when you set focus using the mouse, all text is also selected -
unlike standard Delphi components that only selects all when setting focus with <tab>.}
{When a form has a defaultbutton and you press <enter>, the click event
triggers and focus is returned to the edit control which autoselects all.}
		property AutoSelect;
		property AutoSize;
{BaseFormat is the edit- and displaytype}
		property BaseFormat: TBaseFormat read FBaseFormat write SetBaseFormat;
{Specifies the number of chars that binary numbers should use.}
		property BinLength : integer read FBinlength write FBinLength default 32;
{Specifies the number of chars that hexadecimal numbers should use.}
		property HexLength : integer read FHexlength write FHexLength default 8;
{The Color property sets only the enabled-color. When you disable the
edit-control the Disabled color will be used.}
		property Color : TColor read FEnabledColor write SetColor default clWindow;
{DisabledColor is the background color used when the edit-control is disabled.}
{To set the color when Enabled - use the Color property.}
		property DisabledColor : TColor read FDisabledColor
			write SetDisabledColor default clBtnFace;
{Besides standard enabling/disabling - this property also changes the background
color to either the Color or the DisabledColor property.}
		property Enabled : Boolean read GetEnabled
			write SetEnabled default True;
		property BorderStyle;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property MaxLength;
{Set MaxValue to prevent users from entering values greater than MaxValue.
OnInvalidEntry triggers when the edit component looses focus.
When MaxValue and MinValue are both zero, they have no effect.}
{$IFDEF VER100}
		property MaxValue: Integer read FMaxValue write SetMaxValue;
{$ELSE}
		property MaxValue: Int64 read FMaxValue write SetMaxValue;
{$ENDIF}
{Set MinValue to prevent users from entering values less than MinValue.
OnInvalidEntry triggers when the edit component looses focus.
When MaxValue and MinValue are both zero, they have no effect.}
{$IFDEF VER100}
		property MinValue: Int64 read FMinValue write SetMinValue;
{$ELSE}
		property MinValue: Int64 read FMinValue write SetMinValue;
{$ENDIF}
		property OnChange;
		property OnClear : TNotifyEvent read FOnClear write FOnClear;
		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
{Is called when the user enters a value greater than MaxValue or smaller
than MinValue.}
		property OnInvalidEntry: TNotifyEvent read FInvalidEntry write FInvalidEntry;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;
{Set ParentColor to True to set the Color property to be the same as
the parent's Color.}
		property ParentColor : Boolean read FParentColor
			write SetParentColor default False;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
{Read only}
		property Version: String read FVersion write SetVersion;
{Set Visible to False if you just need the conversion routines.}
		property Visible;
	end;

procedure Register;

implementation

uses Clipbrd;

constructor TPBBinHexEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Width := 100;
	FAlignment := taCenter;
	FEnter := False;
	FMaxValue := 0;
	FMinValue := 0;
	FBaseFormat := HexaDecimal;
	FBinLength := 32;
	FHexLength := 8;
	FVersion := '8.50.00.00';
	AsInteger := 0;
	Text := FormatText(0, FBaseFormat);
	inherited Enabled := True;
	inherited Color := clWindow;
	FEnabledColor := clWindow;
	FDisabledColor := clBtnFace;
end;

procedure TPBBinHexEdit.Loaded;
begin
	inherited;
	if Enabled then inherited Color := FEnabledColor
	else inherited Color := FDisabledColor;
end;

procedure TPBBinHexEdit.CreateParams(var Params: TCreateParams);
const
	Alignments: array[TAlignment] of Word = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
	inherited CreateParams(Params);
	Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment];
end;

procedure TPBBinHexEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
	if (Button = mbLeft) or (ssLeft in Shift) then
	begin
		if FEnter = True then
		begin
			FEnter := False;
			if AutoSelect then SelectAll;
		end;
	end;
end;

procedure TPBBinHexEdit.DoEnter;
begin
	inherited DoEnter;
	if csLButtonDown in ControlState then FEnter := True;
	if AutoSelect then SelectAll;
end;

procedure TPBBinHexEdit.DoExit;
begin
	inherited DoExit;
	if (FMinValue <> 0) and (FMaxValue <> 0)
		and ((AsInteger < FMinValue) or (AsInteger > FMaxValue)) then InvalidEntry;
end;

procedure TPBBinHexEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
	FEnter := False;
	if not ReadOnly then
	begin
		if Key in [VK_DELETE] then if (SelStart = 0)
			and ((Text[1] in ['$']) or (SelLength = length(Text))) then
		begin
			if (FBaseFormat = HexaDecimal) then
			begin
				Text := '$0';
				SelStart := 1;
			end
			else
			begin
				Text := '0';
				SelStart := 0;
			end;
			Key := 0;
			SelLength := 1;
		end;
	end;
end;

procedure TPBBinHexEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
	OldStart : integer;
begin
	inherited KeyUp(Key, Shift);
	if not ReadOnly then
	begin
		if (FBaseFormat = HexaDecimal) then
		begin
			if (Text = '') or (Text = '$') then
			begin
				Text := '$0';
				SelStart := 1;
				SelLength := 1;
			end
			else if Text[1] <> '$' then
			begin
				OldStart := SelStart;
				Text := '$' + Text;
				SelStart := OldStart + 1;
				SelLength := 0;
			end;
		end
		else if Text = '' then
		begin
			Text := '0';
			SelStart := 0;
			SelLength := 1;
		end;
	end;
end;

procedure TPBBinHexEdit.KeyPress(var Key: Char);
var
	SelSt, t : integer;
	TempText : string;
begin
	if (Key in [#13, #27]) then
	begin
		MessageBeep(0);
		Key := #0;
		Exit;
	end;
	inherited KeyPress(Key);
	if Key in [#3] then Exit;
	if ReadOnly then
	begin
		MessageBeep(0);
		Key := #0;
		Exit;
	end;
	TempText := Copy(Text, 1, SelStart) + Key + Copy(Text,
		SelStart + SelLength + 1, 999);
	SelSt := SelStart;
	if (Key in [#8, #22, #24]) then Exit
	else if (FBaseFormat = HexaDecimal) then
	begin
		if Key in ['a'..'f'] then Key := Chr(Ord(Key) - 32);
		if (Key in ['$']) then
		begin
			Text := '$0';
			Key := #0;
			SelStart := 1;
			SelLength := 1;
		end
		else if (not (Key in ['0'..'9','A'..'F']))
			or (Length(TempText) > FHexLength + 1) then
		begin
			MessageBeep(0);
			Key := #0;
		end;
	end
	else if (FBaseFormat = Binary) then
	begin
		if (not (Key in ['0','1'])) or (Length(TempText) > FBinLength) then
		begin
			MessageBeep(0);
			Key := #0;
		end;
	end
	else
	begin
		if not (Key in ['0'..'9','-']) then
		begin
			MessageBeep(0);
			Key := #0;
		end
		else if (Key = '-') then
		begin
			t := Pos('-', TempText);
			while t <> 0 do
			begin
				Delete(TempText, t, 1);
				t := Pos('-', TempText);
			end;
			if (TempText = '') then TempText := '0';
			if (Pos('-', Text) = 0) or (TempText = '0') then
			begin
				Text := '-' + TempText;
				SelStart := SelSt + 1;
				if Text = '-0' then SelLength := 1
				else SelLength := 0;
				Key := #0;
			end
			else
			begin
				Text := TempText;
				if SelSt > 0 then SelStart := SelSt - 1;
				SelLength := 0;
				Key := #0;
			end;
		end
{$IFDEF VER100}
		else if (StrToIntDef(TempText, -1) = -1)
			and (StrToIntDef(TempText, 1) = 1) then
{$ELSE}
		else if (StrToInt64Def(TempText, -1) = -1)
			and (StrToInt64Def(TempText, 1) = 1) then
{$ENDIF}
		begin
			MessageBeep(0);
			Key := #0;
		end;
	end;
end;

{$IFDEF VER100}
function TPBBinHexEdit.GetAsInteger: Integer;
begin
	if (FBaseFormat = Binary) then Result :=  BinToInt(Text)
	else Result := StrToInt(Text);
end;
{$ELSE}
function TPBBinHexEdit.GetAsInteger: Int64;
begin
	if (FBaseFormat = Binary) then Result :=  BinToInt(Text)
	else Result := StrToInt64(Text);
end;
{$ENDIF}

function TPBBinHexEdit.GetAsBin: string;
begin
	Result := FormatText(AsInteger, Binary);
end;

function TPBBinHexEdit.GetAsHex: string;
begin
	Result := FormatText(AsInteger, HexaDecimal);
end;

{$IFDEF VER100}
procedure TPBBinHexEdit.SetAsInteger(Value: Integer);
{$ELSE}
procedure TPBBinHexEdit.SetAsInteger(Value: Int64);
{$ENDIF}
begin
	if csDesigning in ComponentState then
	begin
		if (Value > FMaxValue) and ((FMaxValue <> 0) or (FMinValue <> 0))
			then InvalidEntry;
		if (Value < FMinValue) and ((FMaxValue <> 0) or (FMinValue <> 0))
			then InvalidEntry;
	end;
	if Text <> FormatText(Value, FBaseFormat)
		then Text := FormatText(Value, FBaseFormat);
end;

procedure TPBBinHexEdit.SetAsBin(Value: string);
begin
	if AsInteger <> BinToInt(Value) then AsInteger := BinToInt(Value);
end;

procedure TPBBinHexEdit.SetAsHex(Value: string);
begin
	if Copy(Value, 1, 1) <> '$' then Value := '$' + Value;
{$IFDEF VER100}
	if AsInteger <> StrToInt(Value) then AsInteger := StrToInt(Value);
{$ELSE}
	if AsInteger <> StrToInt64(Value) then AsInteger := StrToInt64(Value);
{$ENDIF}
end;

procedure TPBBinHexEdit.SetAlignment(Value: TAlignment);
var
	SelSt, SelLe : integer;
begin
	if FAlignment <> Value then
	begin
		SelSt := SelStart;
		SelLe := SelLength;
		FAlignment := Value;
		RecreateWnd;
		SelStart := SelSt;
		SelLength := SelLe;
	end;
end;

procedure TPBBinHexEdit.SetColor(Value : TColor);
begin
	if FEnabledColor <> Value then
	begin
		FEnabledColor := Value;
		if Enabled then inherited Color := Value;
		if (Parent <> nil) and (FEnabledColor <> Parent.Brush.Color)
			then FParentColor := False;
	end;
end;

procedure TPBBinHexEdit.SetDisabledColor(Value : TColor);
begin
	if FDisabledColor <> Value then
	begin
		FDisabledColor := Value;
		if (not Enabled) then inherited Color := Value;
	end;
end;

procedure TPBBinHexEdit.SetEnabled(Value : Boolean);
begin
	inherited;
	if Enabled then inherited Color := FEnabledColor
	else inherited Color := FDisabledColor;
end;

{$IFDEF VER100}
procedure TPBBinHexEdit.SetMaxValue(Value: Integer);
{$ELSE}
procedure TPBBinHexEdit.SetMaxValue(Value: Int64);
{$ENDIF}
begin
	if (FMaxValue <> Value) and (Value >= FminValue) then
	begin
		FMaxValue := Value;
		if ((Value <> 0) or (FMinValue <> 0))
			and (AsInteger > Value) then InvalidEntry;
	end;
end;

{$IFDEF VER100}
procedure TPBBinHexEdit.SetMinValue(Value: Integer);
{$ELSE}
procedure TPBBinHexEdit.SetMinValue(Value: Int64);
{$ENDIF}
begin
	if (FMinValue <> Value) and (Value <= FmaxValue) then
	begin
		FMinValue := Value;
		if ((Value <> 0) or (FMaxValue <> 0))
			and (AsInteger < Value) then InvalidEntry;
	end;
end;

procedure TPBBinHexEdit.InvalidEntry;
begin
	if not (csLoading in ComponentState) then
	begin
		if Assigned(FInvalidEntry) then FInvalidEntry(Self)
		else
		begin
			if AsInteger < FMinValue then AsInteger := FMinValue
			else if AsInteger > FMaxValue then AsInteger := FMaxValue;
			MessageBeep(0);
			if CanFocus and (not (csDesigning in ComponentState)) then SetFocus;
		end;
	end;
end;

procedure TPBBinHexEdit.SetParentColor(Value : Boolean);
begin
	if FParentColor <> Value then
	begin
		FParentColor := Value;
		if FParentColor and (Parent <> nil) then FEnabledColor := Parent.Brush.Color;
		SetEnabled(Enabled);
	end;
end;

procedure TPBBinHexEdit.SetVersion(Value: String);
begin
	{ Read only! }
end;

procedure TPBBinHexEdit.SetBaseFormat(Value: TBaseFormat);
var
{$IFDEF VER100}
	Asi : integer;
{$ELSE}
	Asi : Int64;
{$ENDIF}
begin
	if FBaseFormat <> Value then
	begin
		Asi := AsInteger;
		FBaseFormat := Value;
		Text := FormatText(AsI, FBaseFormat);
	end;
end;

{$IFDEF VER100}
function TPBBinHexEdit.FormatText(Value: Integer; NFormat: TBaseFormat): string;
{$ELSE}
function TPBBinHexEdit.FormatText(Value: Int64; NFormat: TBaseFormat): string;
{$ENDIF}
begin
	if NFormat = Number then Result := IntToStr(Value)
	else if NFormat = Binary then Result := IntToBin(Value)
	else Result := '$' + IntToHex(Value, FHexLength);
end;

{$IFDEF VER100}
function TPBBinHexEdit.IntToBin(I : integer): string;
var
	b, t, c : integer;
begin
	Result := '';
	if I < 0 then
	begin
		Result := Result + '1';
		c := I + MAXINT + 1;
	end
	else c := I;
	t := MAXINT div 2 + 1;
	repeat
		b := c - t;
		if b >= 0 then
		begin
			Result := Result + '1';
			c := b;
		end
		else if c <> I then Result := Result + '0';
		t := t div 2;
	until t = 0;
	if Result = '' then Result := '0';
end;
{$ELSE}
function TPBBinHexEdit.IntToBin(I : Int64): string;
var
	b, t, c : Int64;
begin
	Result := '';
	if I < 0 then
	begin
		Result := Result + '1';
		c := I + High(Int64) + 1;
	end
	else c := I;
	t := High(Int64) div 2 + 1;
	repeat
		b := c - t;
		if b >= 0 then
		begin
			Result := Result + '1';
			c := b;
		end
		else if c <> I then Result := Result + '0';
		t := t div 2;
	until t = 0;
	if Result = '' then Result := '0';
end;
{$ENDIF}

{$IFDEF VER100}
function TPBBinHexEdit.BinToInt(B : string): integer;
var
	b1: string;
	a : char;
	Ok : boolean;
	t, t1 : integer;
begin
	Ok := True;
	b1 := B;
	Result := 0;
	if b1 = '' then Exit;
	for t1 := 1 to Length(b1) do if not (b1[t1] in ['0', '1']) then Ok := False;
	if Ok then
	begin
		t := 1;
		repeat
			a := b1[Length(b1)];
			if a = '1' then Result := Result + t;
			if (t = -MAXINT - 1) then Exit
			else if t = MAXINT div 2 + 1 then t := -MAXINT - 1
			else t := t * 2;
			b1 := Copy(b1, 1, Length(b1) - 1);
		until b1 ='';
	end;
end;
{$ELSE}
function TPBBinHexEdit.BinToInt(B : string): Int64;
var
	b1: string;
	a : char;
	Ok : boolean;
	t : Int64;
	t1 : integer;
begin
	Ok := True;
	b1 := B;
	Result := 0;
	if b1 = '' then Exit;
	for t1 := 1 to Length(b1) do if not (b1[t1] in ['0', '1']) then Ok := False;
	if Ok then
	begin
		t := 1;
		repeat
			a := b1[Length(b1)];
			if a = '1' then Result := Result + t;
			if (t = -High(Int64) - 1) then Exit
			else if t = High(Int64) div 2 + 1 then t := -High(Int64) - 1
			else t := t * 2;
			b1 := Copy(b1, 1, Length(b1) - 1);
		until b1 ='';
	end;
end;
{$ENDIF}

procedure TPBBinHexEdit.WMPaste(var Message: TMessage);
var
	X, P: integer;
	S: String;
	W: Word;
begin
	P := SelStart;
	Text := Copy(Text, 1, SelStart)
		+ Copy(Text, SelStart + SelLength + 1, Length(Text) - SelStart - SelLength);
	SelStart := P;
	SelLength := 0;
	S := Clipboard.AsText;
	for X := 1 to Length(S) do
	begin
		W := Ord(S[X]);
		Perform(WM_CHAR, W, 0);
	end;
end;

procedure TPBBinHexEdit.CMPARENTCOLORCHANGED(var M:TMessage);
begin
	if FParentColor and (Parent <> nil) then FEnabledColor := Parent.Brush.Color;
	if Parent <> nil then Invalidate;
	SetEnabled(Enabled);
end;

procedure TPBBinHexEdit.WMClear(var Msg : TMessage);
begin
	if Assigned(FOnClear) then FOnClear(Self)
	else
	begin
		Perform(WM_KEYDOWN, VK_DELETE, 0);
		Perform(WM_KEYUP, VK_DELETE, 0);
	end;
end;

procedure TPBBinHexEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
	Msg.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
end;

procedure Register;
begin
	RegisterComponents('PBEdit', [TPBBinHexEdit]);
end;

end.

