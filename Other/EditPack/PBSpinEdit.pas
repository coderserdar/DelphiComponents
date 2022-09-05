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
{PBSpinEdit is a standard Delphi Edit component with Alignment, DisabledColor
and mouse-AutoSelect-all.}
{Can replace standard components without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click. (Standard SpinEdit does not).}
{Supports Cancel-button click.}

unit PBSpinEdit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Spin;

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
{PBSpinEdit is a standard Delphi Edit component with Alignment, DisabledColor
 and
mouse-AutoSelect-all.}
{Can replace standard components without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click. (Standard SpinEdit does not).}
{Supports Cancel-button click.}
	TPBSpinEdit = class(TSpinEdit)
	private
		{ Private declarations }
		FAlignment : TAlignment;
		FEnter, FParentColor : Boolean;
		FVersion : string;
		FDisabledColor, FEnabledColor : TColor;
		FOnClear : TNotifyEvent;
		function GetCursor : TCursor;
		procedure SetColor(Value : TColor);
		procedure SetDisabledColor(Value : TColor);
		procedure SetAlignment(Value: TAlignment);
		procedure SetCursor(Value : TCursor);
		procedure SetParentColor(Value : Boolean);
		procedure SetVersion(Value: String);
		procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
		procedure CMPARENTCOLORCHANGED(var M:TMessage); message CM_PARENTCOLORCHANGED;
		procedure WMClear(var Msg : TMessage); message WM_CLEAR;
	protected
		{ Protected declarations }
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure DoEnter; override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyPress(var Key: Char); override;
		procedure SetEnabled(Value : Boolean); override;
		procedure Loaded; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure CreateParams(var Params: TCreateParams); override;
	published
{Set Alignment to: taLeftJustify, taCenter or taRightJustify.}
{Default : taLeftJustify.}
{Supports Windows 95, 98 and NT.}
		property Alignment: TAlignment read FAlignment write SetAlignment;
{Default: True.}
{Set AutoSelect to True to select all text when you set focus:}
{Notice that when you set focus using the mouse, all text is also selected -
unlike standard Delphi components that only selects all when setting focus with <tab>.}
{When a form has a defaultbutton and you press <enter>, the click event
triggers and focus is returned to the edit control which autoselect all.}
		property AutoSelect;
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
		property Cursor : TCursor read GetCursor write SetCursor;
		property OnClear : TNotifyEvent read FOnClear write FOnClear;
{Set ParentColor to True to set the Color property to be the same as
the parent's Color.}
		property ParentColor : Boolean read FParentColor
			write SetParentColor default False;
{Read only. }
		property Version: String read FVersion write SetVersion stored False;
end;

procedure Register;

implementation

constructor TPBSpinEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Width := 100;
	FAlignment := taLeftJustify;
	FVersion := '8.50.00.00';
	Text := '';
	FEnter := False;
	inherited Enabled := True;
	inherited Color := clWindow;
	FEnabledColor := clWindow;
	FDisabledColor := clBtnFace;
	Value := MinValue;
end;

procedure TPBSpinEdit.Loaded;
begin
	inherited;
	if Enabled then inherited Color := FEnabledColor
	else inherited Color := FDisabledColor;
end;

procedure TPBSpinEdit.CreateParams(var Params: TCreateParams);
const
	Alignments: array[TAlignment] of Word = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
	inherited CreateParams(Params);
	Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment];
end;

procedure TPBSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbLeft) or (ssLeft in Shift) then
	begin
		if FEnter = True then
		begin
			FEnter := False;
			if AutoSelect then SelectAll;
		end;
	end;
	inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPBSpinEdit.DoEnter;
begin
	inherited DoEnter;
	if csLButtonDown in ControlState then FEnter := True;
	if AutoSelect then SelectAll;
end;

procedure TPBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
	FEnter := False;
end;

procedure TPBSpinEdit.KeyPress(var Key: Char);
begin
	if (Key in [#13, #27]) then
	begin
		Key := #0;
		MessageBeep(0);
	end
	else inherited;
end;

procedure TPBSpinEdit.SetAlignment(Value: TAlignment);
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

procedure TPBSpinEdit.SetColor(Value : TColor);
begin
	if FEnabledColor <> Value then
	begin
		FEnabledColor := Value;
		if Enabled then inherited Color := Value;
		if (Parent <> nil) and (FEnabledColor <> Parent.Brush.Color)
			then FParentColor := False;
	end;
end;

procedure TPBSpinEdit.SetDisabledColor(Value : TColor);
begin
	if FDisabledColor <> Value then
	begin
		FDisabledColor := Value;
		if (not Enabled) then inherited Color := Value;
	end;
end;

procedure TPBSpinEdit.SetEnabled(Value : Boolean);
begin
	inherited;
	if Enabled then inherited Color := FEnabledColor
	else inherited Color := FDisabledColor;
end;

procedure TPBSpinEdit.SetParentColor(Value : Boolean);
begin
	if FParentColor <> Value then
	begin
		FParentColor := Value;
		if FParentColor and (Parent <> nil) then FEnabledColor := Parent.Brush.Color;
		SetEnabled(Enabled);
	end;
end;

procedure TPBSpinEdit.SetVersion(Value: String);
begin
	{ Read only! }
end;

function TPBSpinEdit.GetCursor : TCursor;
begin
	Result := inherited Cursor;
end;

procedure TPBSpinEdit.SetCursor(Value : TCursor);
begin
	if inherited Cursor <> Value then
	begin
		inherited Cursor := Value;
		Button.Cursor := Value;
	end;
end;

procedure TPBSpinEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
	Msg.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
end;

procedure TPBSpinEdit.WMClear(var Msg : TMessage);
begin
	if Assigned(FOnClear) then FOnClear(Self)
	else Value := MinValue;
end;

procedure TPBSpinEdit.CMPARENTCOLORCHANGED(var M:TMessage);
begin
	if FParentColor and (Parent <> nil) then FEnabledColor := Parent.Brush.Color;
	if Parent <> nil then Invalidate;
	SetEnabled(Enabled);
end;

procedure Register;
begin
	RegisterComponents('PBEdit', [TPBSpinEdit]);
end;

end.

