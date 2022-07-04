{***********************************************}
{File:      NCEditrs.PAS                        }
{Revision:  2.04 / 06.02.2000                   }
{Comment:   User interface utilities            }
{Copyright: (c) 1997-2000, Dmitry Arefiev       }
{Author:    Dmitry Arefiev, dmitrya@inthink.com }
{***********************************************}
{$I NCOciDef.inc}

unit NCUIUtil;

interface

Uses Messages, Windows, Grids, Buttons, Controls, Graphics, Classes
{$IFDEF OCI_D6}
    , Variants
{$ENDIF}
    ;

Type
    __TCustomGridCaster = class(TCustomGrid)
    public
        function XY2GridCoord(x, y: Integer): TGridCoord;
        { calc sum of column's width }
        function CalcClientWidth: Integer;
        { calc grid width desired to fit all column's }
        function CalcExtent: Integer;
        { calc offset's of grid data }
        function P_CanEditAcceptKey(Key: Char): Boolean;

        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property BorderStyle;
        property Options;
        property ColCount;
        property ColWidths;
        property OnKeyDown;
        property OnKeyUp;
        property OnKeyPress;
    end;

{Loads glyph from resource}
procedure LoadBtnGlyph(btn: TBitBtn; nm: String);
{Restores window, if it was maximized or minimized}
procedure RestoreWindow(ctrl: TWinControl);
{Sends char into window - keyboard manipulation}
procedure SendChar(ctrl: TControl; ch: Char);
{Sends string into window - keyboard manipulation}
procedure SendStr(ctrl: TControl; s: String);
{Draws control}
procedure DrawCheckBox(ACanvas: TCanvas; ARect: TRect; AValue: Variant);
procedure DrawComboButton(ACanvas: TCanvas; ARect: TRect; APressed: Boolean; ADisabled: Boolean);
{Shows message box}
function Balert(const TypeMessage: string; Prompts: array of string; Buttons: array of const):integer;
procedure ErrorDlg(msg: array of String; HelpCtx: LongInt);
procedure WarningDlg(msg: array of String; HelpCtx: LongInt);
function YesNoDlg(msg: array of String; HelpCtx: LongInt): Boolean;
procedure InformationDlg(msg: array of String; HelpCtx: LongInt);
procedure ViewStrings(ADoIt: Boolean; AStrs: TStrings; const ACaption: String);
{font <-> string}
procedure IUStr2Font(AStr: String; AFont: TFont);
function IUFont2Str(AFont: TFont): String;

var
    CurrentButtonWidth: Integer;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }

implementation

Uses NCUtil, NCStrs, Forms, SysUtils, ExtCtrls, StdCtrls, Dialogs, NCMemo;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }
{ Work with grids }

function __TCustomGridCaster.XY2GridCoord(x, y: Integer): TGridCoord;
begin
    Result := MouseCoord(x, y);
end;

function __TCustomGridCaster.CalcClientWidth: Integer;
var
    I, BorderWidth: Integer;
begin
    Result := 0;
    if ColCount > 0 then begin
        BorderWidth := 0;
        if goVertLine in Options then
            BorderWidth := 1;
        for I := 0 to ColCount - 1 do
            Result := Result + ColWidths[I] + BorderWidth;
    end;
end;

function __TCustomGridCaster.CalcExtent: Integer;
begin
    Result := CalcClientWidth + GetSystemMetrics(SM_CXHTHUMB) + 1;
    if BorderStyle = bsSingle then
        Inc(Result, 2);
end;

function __TCustomGridCaster.P_CanEditAcceptKey(Key: Char): Boolean;
begin
    Result := CanEditAcceptKey(Key);
end;
{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }

procedure LoadBtnGlyph(btn: TBitBtn; nm: String);
begin
    btn.Glyph.Handle := LoadBitmap(HInstance, PChar(nm));
    btn.NumGlyphs := 2;
end;

procedure RestoreWindow(ctrl: TWinControl);
begin
    if GetWindowLong(ctrl.Handle, GWL_STYLE) and (WS_MINIMIZE or WS_MAXIMIZE) <> 0 then
        ShowWindow(ctrl.Handle, SW_RESTORE);
end;

procedure SendChar(ctrl: TControl; ch: Char);
var
    VkCode: Word;

    procedure PostVk(AMsg: Word; AVKey: Word);
    begin
        Ctrl.Perform(AMsg, AVKey, MapVirtualKey(AVKey, 0));
    end;

begin
    VkCode := VkKeyScan(ch);
    if (HiByte(VkCode) and 1) = 1 then
        PostVk(WM_KEYDOWN, VK_SHIFT);
    PostVk(WM_KEYDOWN, VkCode);
    PostVk(WM_CHAR, Word(ch));
    PostVk(WM_KEYUP, VkCode);
    if (HiByte(VkCode) and 1) = 1 then
        PostVk(WM_KEYUP, VK_SHIFT);
end;

procedure SendStr(ctrl: TControl; s: String);
var
    n, i: Integer;
begin
    n := Length(s);
    for i := 1 to n do
        SendChar(ctrl, s[i]);
end;

procedure DrawCheckBox(ACanvas: TCanvas; ARect: TRect; AValue: Variant);
var
    i: Integer;
begin
    try
        if VarIsNull(AValue) or VarIsEmpty(AValue) then
            i := DFCS_INACTIVE
        else if AValue <> 0 then
            i := DFCS_CHECKED
        else
            i := 0;
    except
        i := DFCS_INACTIVE;
    end;
    DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONCHECK or i);
end;

procedure DrawComboButton(ACanvas: TCanvas; ARect: TRect; APressed: Boolean; ADisabled: Boolean);
var
    i: Integer;
begin
    i := 0;
    if APressed then
        i := i or DFCS_FLAT or DFCS_PUSHED;
    if ADisabled then
        i := i or DFCS_INACTIVE;
    DrawFrameControl(ACanvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or i);
end;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }
{   TypeMessage - window caption
    Prompts - array of strings to show in window
    Buttons - array of button names
}

function Balert(const TypeMessage: string; Prompts: array of string; Buttons: array of const):integer;
var
    i: integer;
    box: TForm;
    panel: TPanel;
    WidthBox,X,Y: Integer;
begin
    box := TForm.Create(Application);
    try
        WidthBox := 0;
        with box do begin
            Position := poScreenCenter;
            Caption :=  TypeMessage;
            BorderStyle := bsDialog;
            BorderIcons := [];
            IUStr2Font(SDefaultFont, Font);
            HandleNeeded;
            Canvas.Font := Font;

            with TLabel.Create(Box) do begin
                parent := box;
                alignment := taCenter;
                caption := #13#10 + Strs2Text(prompts);
                autosize := true;
                Y := height + 20;
                if width > WidthBox then
                    WidthBox := width;
                align := alTop;
            end;

            panel := TPanel.Create(Box);
            panel.top := Y;
            panel.left := 10;
            panel.height := 34;
            panel.bevelouter := bvNone;
            panel.parent := box;

            X := 0;
            for i := low(buttons) to high(buttons) do
                with TBitBtn.Create(panel) do begin
                    parent := panel;
                    if buttons[i].VType = vtInteger then
                        Kind := TBitBtnKind(buttons[i].VInteger)
                    else begin
                        Caption := buttons[i].VPChar;
                        modalresult := i+1;
                    end;
                    if width < Canvas.TextWidth(Caption) + 30 then
                        width := Canvas.TextWidth(Caption) + 30;
                    top := 0;
                    left := X;
                    Inc(X, Width + 10);
                end;

            panel.width := X - 10;
            if panel.width > WidthBox then
                WidthBox := panel.width;
            inc(WidthBox, 30);
            panel.left := (WidthBox - panel.width) div 2;

            ClientHeight := panel.Top + panel.Height + 5;
            ClientWidth := WidthBox;
        end; {with box begin}

        result := box.ShowModal;
    finally
        box.Free;
    end;
end; {function Balert}

procedure ErrorDlg(msg: array of String; HelpCtx: LongInt);
begin
    MessageDlg(Strs2Text(msg), mtError, [mbOk], HelpCtx);
end;

procedure WarningDlg(msg: array of String; HelpCtx: LongInt);
begin
    MessageDlg(Strs2Text(msg), mtWarning, [mbOk], HelpCtx);
end;

procedure InformationDlg(msg: array of String; HelpCtx: LongInt);
begin
    MessageDlg(Strs2Text(msg), mtInformation, [mbOk], HelpCtx);
end;

function YesNoDlg(msg: array of String; HelpCtx: LongInt): Boolean;
begin
    Result := (MessageDlg(Strs2Text(msg), mtConfirmation, [mbYes, mbNo], HelpCtx) = mrYes);
end;

procedure ViewStrings(ADoIt: Boolean; AStrs: TStrings; const ACaption: String);
begin
    if ADoIt and not MemoEdit(AStrs, ACaption, False) then
        SysUtils.Abort;
end;

procedure IUStr2Font(AStr: String; AFont: TFont);
var
    i: Integer;
    s: String;
    Style: TFontStyles;
begin
    i := 1;
    AFont.Name := Trim(StrToken(AStr, [','], i));
    AFont.Size := StrToInt(Trim(StrToken(AStr, [','], i)));
    Style := [];
    s := UpperCase(Trim(StrToken(AStr, [','], i)));
    if s <> '' then begin
        if Pos('B', s) <> 0 then
            Include(Style, fsBold);
        if Pos('I', s) <> 0 then
            Include(Style, fsItalic);
        if Pos('U', s) <> 0 then
            Include(Style, fsUnderline);
        if Pos('S', s) <> 0 then
            Include(Style, fsStrikeOut);
    end;
    AFont.Style := Style;
end;

function IUFont2Str(AFont: TFont): String;
var
    FirstStyle: Boolean;
    i: TFontStyle;
begin
    Result := AFont.Name + ', ' + IntToStr(AFont.Size);
    FirstStyle := True;
    for i := Low(TFontStyle) to High(TFontStyle) do
        if i in AFont.Style then begin
            if FirstStyle then begin
                Result := Result + ', ';
                FirstStyle := False;
            end;
            case i of
            fsBold: Result := Result + 'B';
            fsItalic: Result := Result + 'I';
            fsUnderline: Result := Result + 'U';
            fsStrikeOut: Result := Result + 'S';
            end;
        end;
end;

initialization
    CurrentButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
end.
