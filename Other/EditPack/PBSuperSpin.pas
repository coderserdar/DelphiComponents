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
{PBSuperSpin is a PBNumEdit component with spin-buttons.}
{PBSuperSpin can display all that PBNumEdit can.}
{You can use decimal values as Increment.}
{It has a Wrap property that wraps to MinValue when you exceed MaxValue.}
{Accelerated spin when holding down the mouse-button or up/down keys.}
{Can replace standard components without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click. (Standard SpinEdit does not).}
{Supports Cancel-button click.}

unit PBSuperSpin;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Spin, PBNumEdit;

type
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
{PBSuperSpin is a PBNumEdit component with spin-buttons.}
{PBSuperSpin can display all that PBNumEdit can.}
{You can use decimal values as Increment.}
{Accelerated spin when holding down the mouse-button or up/down keys.}
{It has a Wrap property that wraps to MinValue when you exceed MaxValue.}
{Can replace standard components without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click. (Standard SpinEdit does not).}
	TPBSuperSpin = class(TPBNumEdit)
	private
		{ Private declarations }
		FButton: TSpinButton;
		FIncrement, TempIncrement : extended;
		FEditorEnabled, FWrap, FRoundValues : Boolean;
		FVersion : string;
		ClickTime : DWord;
		RepeatCount : integer;
		function GetCursor : TCursor;
		function GetMinHeight: Integer;
		procedure SetEditRect;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
		procedure WMCut(var Message: TWMCut); message WM_CUT;
		procedure Dummy(Value: string);
		procedure SetCursor(Value : TCursor);
		procedure SetIncrement(Value : extended);
	protected
		{ Protected declarations }
		procedure CreateWnd; override;
		procedure DoExit; override;
		procedure DownClick (Sender: TObject);
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyPress(var Key: Char); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
		procedure UpClick (Sender: TObject);
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
{Steps down the Value by Increment. Same as clicking the Up-button except that it
does not accelerate the increment. See also Wrap.}
		procedure DownStep;
{Manually rounds the Value. See RoundValues.}
		procedure RoundValue;
{Steps up the Value by Increment. Same as clicking the Up-button except that it
does not accelerate the increment. See also Wrap.}
		procedure UpStep;
	published
		property Cursor : TCursor read GetCursor write SetCursor;
{EditorEnabled decides whether it is possible to enter a value directly in the editor.}
		property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
{Increment is the decimal value that Value steps by when you click the buttons up or down.
If you keep the mouse-button down (or the up/down keys) the value will increase to accelerate the spin.}
//Note: You can set Increment to decimal values like 0.25.
		property Increment: extended read FIncrement write SetIncrement;
{When True: Values will always be MinValue + an integer times Increment.}
{Example: MinValue = 10, Increment = 5. If a user enters say 23 it will be
rounded to 25 upon exit or if a spin-button is pressed.}
{If EditorEnabled is False, it has no purpose.}
		property RoundValues : Boolean read FRoundValues write FRoundValues;
{Read only}
		property Version : string read FVersion write Dummy stored False;
{Wrap decides whether or not the value should stop incrementing when it reaches MaxValue
 or it should wrap around to MinValue (or vice versa when pressing down-button).}
		property Wrap : Boolean read FWrap write FWrap;
	end;

procedure Register;

implementation

constructor TPBSuperSpin.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FButton := TSpinButton.Create(Self);
	FButton.Parent := Self;
	FButton.Width := 15;
	FButton.Height := 17;
	FButton.Visible := True;
	FButton.FocusControl := Self;
	FButton.OnUpClick := UpClick;
	FButton.OnDownClick := DownClick;
	ControlStyle := ControlStyle - [csSetCaption];
	FIncrement := 1;
	TempIncrement := 1;
	FEditorEnabled := True;
	FRoundValues := False;
	FWrap := False;
	FVersion := '8.50.00.00';
	ClickTime := 0;
	RepeatCount := 0;
end;

destructor TPBSuperSpin.Destroy;
begin
	FButton.Free;
	FButton := nil;
	inherited Destroy;
end;

procedure TPBSuperSpin.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (not ReadOnly) then
	begin
		if Key = VK_UP then UpClick (Self)
		else if Key = VK_DOWN then DownClick (Self);
		inherited KeyDown(Key, Shift);
	end
	else
	begin
		MessageBeep(0);
		Key := 0;
	end;
end;

procedure TPBSuperSpin.KeyUp(var Key: Word; Shift: TShiftState);
begin
	TempIncrement := FIncrement;
	RepeatCount := 0;
	if (not ReadOnly) then inherited KeyUp(Key, Shift)
	else Key := 0;
end;

procedure TPBSuperSpin.KeyPress(var Key: Char);
begin
	if FEditorEnabled then inherited KeyPress(Key)
	else
	begin
		MessageBeep(0);
		Key := #0;
	end;
end;

procedure TPBSuperSpin.CreateWnd;
begin
	inherited CreateWnd;
	SetEditRect;
end;

procedure TPBSuperSpin.SetEditRect;
var
	Loc: TRect;
	BorderWidth, DX : integer;
begin
	SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
	Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
	if (BorderStyle = bsSingle) and Ctl3D then BorderWidth := 2
	else if (BorderStyle = bsSingle) then BorderWidth := 1
	else BorderWidth := 0;
	if (BorderStyle = bsNone) then DX := -2
	else DX := 1;
	Loc.Right := ClientWidth - FButton.Width - BorderWidth + DX;
	Loc.Top := 2 - BorderWidth;
	Loc.Left := 2;
	SendMessage(Handle, EM_SETRECT, 0, LongInt(@Loc));
end;

procedure TPBSuperSpin.WMSize(var Message: TWMSize);
var
	MinHeight, BorderWidth, Delta : Integer;
begin
	inherited;
	MinHeight := GetMinHeight;
		{ text edit bug: if size is less than minheight, then edit ctrl does
			not display the text }
	if Height < MinHeight then Height := MinHeight
	else if FButton <> nil then
	begin
		Delta := 0;
		if (BorderStyle = bsSingle) then
		begin
			if Ctl3D then BorderWidth := 2
			else
			begin
				BorderWidth := 1;
				Delta := 1;
			end;
		end
		else BorderWidth := 0;
		FButton.SetBounds(ClientWidth - FButton.Width - Delta, Delta, FButton.Width, Height - BorderWidth * 2 - 1);
		SetEditRect;
	end;
end;

function TPBSuperSpin.GetMinHeight: Integer;
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

procedure TPBSuperSpin.UpClick (Sender: TObject);
begin
	if ReadOnly and (Sender = FButton) then MessageBeep(0)
	else
	begin
		if GetTickCount - ClickTime > 200 then
		begin
			TempIncrement := FIncrement;
			RepeatCount := 0;
		end
		else if TempIncrement < (MaxValue - MinValue) / 5 then
		begin
			Inc(RepeatCount);
			if (RepeatCount > 4) and (RepeatCount mod 2 = 0) then	TempIncrement := TempIncrement + FIncrement;
		end;
		ClickTime := GetTickCount;
		if (Value < MinValue) and ((MinValue <> 0) or (MaxValue <> 0)) then InvalidEntry
		else if (Value + TempIncrement <= MaxValue) or ((MinValue = 0) and (MaxValue = 0)) then Value := Value + TempIncrement
		else if FWrap then Value := MinValue
		else if (Value + FIncrement <= MaxValue) or ((MinValue = 0) and (MaxValue = 0)) then
		begin
			TempIncrement := FIncrement;
			Value := Value + TempIncrement;
		end
		else InvalidEntry;
		if FRoundValues then RoundValue;
	end;
end;

procedure TPBSuperSpin.DownClick (Sender: TObject);
begin
	if ReadOnly and (Sender = FButton) then MessageBeep(0)
	else
	begin
		if GetTickCount - ClickTime > 200 then
		begin
			TempIncrement := FIncrement;
			RepeatCount := 0;
		end
		else if TempIncrement < (MaxValue - MinValue) / 5 then
		begin
			Inc(RepeatCount);
			if (RepeatCount > 4) and (RepeatCount mod 2 = 0) then	TempIncrement := TempIncrement + FIncrement;
		end;
		ClickTime := GetTickCount;
		if (Value > MaxValue) and ((MinValue <> 0) or (MaxValue <> 0)) then InvalidEntry
		else if (Value - TempIncrement >= MinValue) or ((MinValue = 0) and (MaxValue = 0)) then Value := Value - TempIncrement
		else if FWrap then Value := MaxValue
		else if (Value - FIncrement >= MinValue) or ((MinValue = 0) and (MaxValue = 0)) then
		begin
			TempIncrement := FIncrement;
			Value := Value - TempIncrement;
		end
		else InvalidEntry;
		if FRoundValues then RoundValue;
	end;
end;

procedure TPBSuperSpin.WMPaste(var Message: TWMPaste);
begin
	if not FEditorEnabled or ReadOnly then Exit;
	inherited;
end;

procedure TPBSuperSpin.WMCut(var Message: TWMPaste);
begin
	if not FEditorEnabled or ReadOnly then Exit;
	inherited;
end;

procedure TPBSuperSpin.Dummy(Value: string);
begin
	// Read-only
end;

procedure TPBSuperSpin.DoExit;
begin
	if FRoundValues and (Value >= MinValue) and (Value <= MaxValue)
		or ((MinValue = 0) and (MaxValue = 0)) then RoundValue;
	inherited DoExit;
end;

procedure TPBSuperSpin.RoundValue;
var
	X : extended;
begin
	X := Round((Value - MinValue) / FIncrement);
	Value := MinValue + X * FIncrement;
	if (Value > MaxValue) and ((MinValue <> 0) or (MaxValue <> 0)) then Value := Value - FIncrement;
end;

function TPBSuperSpin.GetCursor : TCursor;
begin
	Result := inherited Cursor;
end;

procedure TPBSuperSpin.SetCursor(Value : TCursor);
begin
	if inherited Cursor <> Value then
	begin
		inherited Cursor := Value;
		FButton.Cursor := Value;
	end;
end;

procedure TPBSuperSpin.SetIncrement(Value : extended);
begin
	if (FIncrement <> Value) then
	begin
		if (Value <= MaxValue - MinValue) or ((MinValue = 0) and (MaxValue = 0)) then
		begin
			FIncrement := Value;
			TempIncrement := Value;
			if FRoundValues then RoundValue;
		end;
	end;
end;

procedure TPBSuperSpin.DownStep;
begin
	if (Value > MaxValue) and ((MinValue <> 0) or (MaxValue <> 0)) then InvalidEntry
	else if (Value - FIncrement >= MinValue) or ((MinValue = 0) and (MaxValue = 0)) then Value := Value - FIncrement
	else if FWrap then Value := MaxValue
	else InvalidEntry;
	if FRoundValues then RoundValue;
end;

procedure TPBSuperSpin.UpStep;
begin
	if (Value < MinValue) and ((MinValue <> 0) or (MaxValue <> 0)) then InvalidEntry
	else if (Value + FIncrement <= MaxValue) or ((MinValue = 0) and (MaxValue = 0))
		then Value := Value + FIncrement
	else if FWrap then Value := MinValue
	else InvalidEntry;
	if FRoundValues then RoundValue;
end;

procedure Register;
begin
	RegisterComponents('PBEdit', [TPBSuperSpin]);
end;

end.

