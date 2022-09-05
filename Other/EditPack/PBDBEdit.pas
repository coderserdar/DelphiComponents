{Author:	Poul Bak}
{}
{Copyright © 1999 - 2003: BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.00.00.00}
{}
{PBDBEdit is a standard Delphi Edit component with Alignment and
mouse-AutoSelect-all.}
{Can replace standard component without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}

unit PBDBEdit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, DBCtrls;

type
{Author:	Poul Bak}
{}
{Copyright © 1999 - 2002: BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.00.00.00}
{}
{PBDBEdit is a standard Delphi Edit component with Alignment and
mouse-AutoSelect-all.}
{Can replace standard component without any disadvantages.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}
	TPBDBEdit = class(TDBEdit)
	private
		{ Private declarations }
		FAlignment : TAlignment;
		FEnter : Boolean;
		FVersion : string;
		procedure SetAlignment(Value: TAlignment);
		procedure SetVersion(Value: String);
		procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
	protected
		{ Protected declarations }
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure DoEnter; override;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyPress(var Key: Char); override;
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
{Read only. }
		property Version: String read FVersion write SetVersion stored False;
end;

procedure Register;

implementation

constructor TPBDBEdit.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Width := 100;
	FAlignment := taLeftJustify;
	FVersion := '8.00.00.00';
	Text := '';
	FEnter := False;
end;

procedure TPBDBEdit.CreateParams(var Params: TCreateParams);
const
	Alignments: array[TAlignment] of Word = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
	inherited CreateParams(Params);
	Params.Style := Params.Style or ES_MULTILINE or Alignments[FAlignment];
end;

procedure TPBDBEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TPBDBEdit.DoEnter;
begin
	inherited DoEnter;
	if csLButtonDown in ControlState then FEnter := True;
	if AutoSelect then SelectAll;
end;

procedure TPBDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
	FEnter := False;
end;

procedure TPBDBEdit.KeyPress(var Key: Char);
begin
	if (Key in [#13, #27]) then
	begin
		Key := #0;
		MessageBeep(0);
	end
	else inherited;
end;

procedure TPBDBEdit.SetAlignment(Value: TAlignment);
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

procedure TPBDBEdit.SetVersion(Value: String);
begin
	{ Read only! }
end;

procedure TPBDBEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
	Msg.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
end;

procedure Register;
begin
	RegisterComponents('PBEdit', [TPBDBEdit]);
end;

end.

