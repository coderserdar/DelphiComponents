unit TBXStrEdit;

// TBX Package
// Copyright 2001-2005 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXStrEdit.pas 119 2005-10-06 00:00:01Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Types, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, TBXStrUtils;

type
  TTBXStrEditDlg = class
  private
    FBoundsRect: TRect;
    FCancelButton: HWND;
    FCaption: WideString;
    FEdit: HWND;
    FHandle: HWnd;
    FLabel: HWND;
    FLabelText: WideString;
    FLoadButton: HWND;
    FMinTrackSize: TPoint;
    FModalResult: TModalResult;
    FMultiLine: Boolean;
    FOkButton: HWND;
    FOldEditWndProc: Pointer;
    FParentHandle: HWND;
    FSaveButton: HWND;
    FWinClassName: WideString;
    function GetEditText: WideString;
    procedure SetEditText(const Value: WideString);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure AlignControls;
    procedure CreateHandle;
    procedure CreateControls;
    procedure DestroyControls;
    procedure DestroyHandle;
    procedure UpdateLabelText;
    property WndParent: HWND read FParentHandle;
  public
    constructor Create(AParentHandle: HWND; const ACaption: WideString; MultiLine: Boolean);
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    procedure LoadFile;
    procedure SaveFile;
    procedure SetEditLimit(NewLimit: Integer);
    function ShowModal: Integer;
    property BoundsRect: TRect read FBoundsRect;
    property EditText: WideString read GetEditText write SetEditText;
    property Handle: HWnd read FHandle;
    property ModalResult: TModalResult read FModalResult write FModalResult;
  end;


implementation

uses Math, RTLConsts, CommDlg;

const
  SOKButtonText = WideString('&OK');
  SCancelButtonText = WideString('Cancel');
  SLoadButtonText = WideString('&Load...');
  SSaveButtonText = WideString('&Save...');

  ID_EDIT = 1;
  ID_OK = 2;
  ID_CANCEL = 3;
  ID_LOAD = 4;
  ID_SAVE = 5;
  ID_LABEL = 6;

var
  DlgWindowRefCounter: Integer = 0;
  StoredMultiLineRect: TRect;
  IsMultiLineRectStored: Boolean = False;
  StoredSingleLineRect: TRect;
  IsSingleLineRectStored: Boolean = False;

function DlgWindowProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  W: TTBXStrEditDlg;
  M: TMessage;
begin
  W := TTBXStrEditDlg(GetWindowLongW(Wnd, GWL_USERDATA));
  if Assigned(W) then
  begin
    M.Msg := Msg;
    M.WParam := WParam;
    M.LParam := LParam;
    M.Result := 0;
    W.Dispatch(M);
    Result := M.Result;
  end
  else Result := DefWindowProcW(Wnd, Msg, WParam, LParam);
end;

function EditProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  W: TTBXStrEditDlg;
  M: PMsg;
  ShiftState: TShiftState;

  function DefaultHandler: LRESULT;
  begin
    Result := CallWindowProcW(W.FOldEditWndProc, Wnd, Msg, WParam, LParam)
  end;

begin
  W := TTBXStrEditDlg(GetWindowLongW(Wnd, GWL_USERDATA));
  if Assigned(W) then
  begin
    case Msg of
      WM_GETDLGCODE:
      begin
        Result := DefaultHandler;
        Result := Result and not DLGC_HASSETSEL;
        if LParam <> 0 then
        begin
          M := Pointer(LParam);
          if (M.message = WM_CHAR) or (M.message = WM_KEYDOWN) then
          begin
            if M.wParam = VK_TAB then
            begin
              ShiftState := KeyDataToShiftState(TWMKey(Pointer(LParam)^).KeyData);
              if not (ssCtrl in ShiftState) then Result := Result and not DLGC_WANTALLKEYS;
            end
            else if (M.wParam = VK_RETURN) and not W.FMultiLine then
            begin
              W.ModalResult := mrOK;
              Result := Result and not DLGC_WANTALLKEYS;
            end;    
          end;
        end;
        Exit;
      end;
    end;
    Result := DefaultHandler;
  end
  else Result := DefWindowProcW(Wnd, Msg, WParam, LParam);
end;

{ TTBXStrEditDlg }

procedure TTBXStrEditDlg.AlignControls;
var
  DC: HDC;
  CR, R: TRect;
  I: Integer;
  DialogUnits: TPoint;
  TextSz: TPoint;
  BtnSize: TPoint;
  LabelSize: TPoint;
  Margins: TPoint;
  BtnGap: TPoint;
  OldFont: HFont;
  Buffer: array [0..51] of Char;
begin
  DC := GetDC(Handle);
  try
    OldFont := SelectObject(DC, GetStockObject(DEFAULT_GUI_FONT));

    { calculate dialog units }
    for I := 0 to 25 do Buffer[I] := Chr(I + Integer('A'));
    for I := 0 to 25 do Buffer[I + 26] := Chr(I + Integer('a'));
    GetTextExtentPoint(DC, Buffer, 52, TSize(DialogUnits));
    DialogUnits.X := (DialogUnits.X + 26) div 52;

    { calculate button size }
    GetTextExtentPoint32W(DC, PWideChar(SOKButtonText), Length(SOKButtonText), TSize(TextSz));
    BtnSize := TextSz;
    GetTextExtentPoint32W(DC, PWideChar(SCancelButtonText), Length(SCancelButtonText), TSize(TextSz));
    BtnSize.X := Max(BtnSize.X, TextSz.X);
    BtnSize.Y := Max(BtnSize.Y, TextSz.Y);
    Inc(BtnSize.X, 12);
    Inc(BtnSize.Y, 4);
    BtnSize.X := Max(BtnSize.X, (50 * DialogUnits.X + 2) div 4);
    BtnSize.Y := Max(BtnSize.Y, (14 * DialogUnits.Y + 4) div 8);
    BtnGap.X := (4 * DialogUnits.X + 2) div 4;
    BtnGap.Y := (7 * DialogUnits.Y + 4) div 8;

    { calculate window margins }
    Margins.X := (7 * DialogUnits.X + 2) div 4;
    Margins.Y := (7 * DialogUnits.Y + 4) div 8;

    SelectObject(DC, OldFont);

    { setup positions and sizes of child controls }
    GetClientRect(Handle, CR);
    InflateRect(CR, -Margins.X, -Margins.Y);

    R := CR;

    if FMultiLine then
    begin
      LabelSize.X := CR.Right - CR.Left;
      LabelSize.Y := DialogUnits.Y;
      SetWindowPos(FLabel, 0, R.Left, R.Top, LabelSize.X, LabelSize.Y, SWP_NOZORDER);
      Inc(LabelSize.Y, Margins.Y);
      Inc(R.Top, LabelSize.Y);
    end
    else LabelSize := Point(0, 0);

    if FMultiLine then Dec(R.Bottom, BtnSize.Y + BtnGap.Y)
    else R.Bottom := R.Top + BtnSize.Y;
    SetWindowPos(FEdit, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);

    R.Bottom := CR.Bottom;
    R.Top := CR.Bottom - BtnSize.Y;

    R.Right := R.Left + BtnSize.X;
    SetWindowPos(FLoadButton, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);

    R.Left := R.Right + BtnGap.X;
    R.Right := R.Left + BtnSize.X;
    SetWindowPos(FSaveButton, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);

    R.Right := CR.Right;
    R.Left := R.Right - BtnSize.X;
    SetWindowPos(FCancelButton, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);

    R.Right := R.Left - BtnGap.X;
    R.Left := R.Right - BtnSize.X;
    SetWindowPos(FOkButton, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);

    FMinTrackSize.X := BtnSize.X * 4 + BtnGap.X * 3;
    if FMultiLine then
      FMinTrackSize.Y := LabelSize.Y + BtnSize.Y * 4 + BtnGap.Y  // edit control is at least btnSize.Y * 3 high
    else
      FMinTrackSize.Y := BtnSize.Y * 2 + BtnGap.Y;
    Windows.GetWindowRect(Handle, R);
    Inc(FMinTrackSize.X, (R.Right - R.Left) - (CR.Right - CR.Left));
    Inc(FMinTrackSize.Y, (R.Bottom - R.Top) - (CR.Bottom - CR.Top));
  finally
    ReleaseDC(Handle, DC);
  end;
end;

constructor TTBXStrEditDlg.Create(AParentHandle: HWND; const ACaption: WideString; MultiLine: Boolean);
var
  W, H: Integer;
  NCSize: TPoint;

  function FitRectToScreen(const R: TRect): TRect;
  var
    W, H: Integer;
  begin
    Result := R;
    W := Result.Right - Result.Left;
    H := Result.Bottom - Result.Top;
    if W < 10 then W := 10;
    if H < 10 then H := 10;
    if W > Screen.Width then W := Screen.Width;
    if H > Screen.Height then H := Screen.Height;
    if Result.Left < 0 then Result.Left := 0;
    if Result.Top < 0 then Result.Top := 0;
    if Result.Left + W > Screen.Width then Result.Left := Screen.Width - W;
    if Result.Top + H > Screen.Height then Result.Top := Screen.Height - H;
    Result.Right := Result.Left + W;
    Result.Bottom := Result.Top + H;
  end;

begin
  FMultiLine := MultiLine;
  FCaption := ACaption;
  FParentHandle := AParentHandle;
  FLabelText := '0 lines';

  NCSize.X := GetSystemMetrics(SM_CXSIZEFRAME) * 2;
  NCSize.Y := GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYSIZEFRAME) * 2;

  if FMultiLine and IsMultiLineRectStored then
    FBoundsRect := FitRectToScreen(StoredMultiLineRect)
  else if not FMultiLine and IsSingleLineRectStored then
    FBoundsRect := FitRectToScreen(StoredSingleLineRect)
  else
  begin
    if FMultiLine then W := 500
    else W := 250;
    W := Min(W + NCSize.X, Screen.Width * 2 div 3);
    H := Min(300 + NCSize.Y, Screen.Height * 2 div 3);
    FBoundsRect := Bounds((Screen.Width - W) div 2, (Screen.Height - H) div 2, W, H);
  end;
  W := FBoundsRect.Right - FBoundsRect.Left;

  CreateHandle;
  CreateControls;
  Windows.SetFocus(FEdit);

  if not FMultiLine then
  begin
    H := FMinTrackSize.Y;
    FBoundsRect := Bounds((Screen.Width - W) div 2, (Screen.Height - H) div 2, W, H);
    with FBoundsRect do
      SetWindowPos(Handle, 0, Left, Top, Right - Left, Bottom - Top, SWP_NOZORDER);
  end;
end;

procedure TTBXStrEditDlg.CreateControls;
const
  MultiLines: array [Boolean] of Cardinal = (0, ES_MULTILINE or WS_VSCROLL or WS_HSCROLL);
var
  Font: HFONT;

  function CreateButton(const Caption: WideString; ID: Cardinal; Default: Boolean = False): HWND;
  const
    D: array [Boolean] of Cardinal = (BS_PUSHBUTTON, BS_DEFPUSHBUTTON);
  begin
    Result := CreateWindowW('BUTTON', PWideChar(Caption),
      WS_VISIBLE or WS_CHILD or WS_TABSTOP or D[Default] or BS_TEXT,
      0, 0, 0, 0, Handle, ID_EDIT, HInstance, nil);
    SendMessageW(Result, WM_SETFONT, Font, 0);
  end;

begin
  Font := GetStockObject(DEFAULT_GUI_FONT);
  SendMessage(Handle, WM_SETFONT, Font, 0);

  FEdit := CreateWindowExW(WS_EX_CLIENTEDGE, 'EDIT', nil,
    WS_CHILD or WS_VISIBLE or ES_AUTOVSCROLL or ES_AUTOHSCROLL or
    WS_TABSTOP or ES_WANTRETURN or MultiLines[FMultiLine],
    0, 0, 0, 0, Handle, ID_EDIT, HInstance, nil);
  SetWindowLong(FEdit, GWL_USERDATA, Integer(Self));
  FOldEditWndProc := Pointer(GetWindowLongW(FEdit, GWL_WNDPROC));
  SetWindowLongW(FEdit, GWL_WNDPROC, Integer(@EditProc));
  SendMessageW(FEdit, WM_SETFONT, Font, 0);

  if FMultiLine then
  begin
    FLabel := CreateWindowW('STATIC', PWideChar(FLabelText),
      WS_VISIBLE or WS_CHILD,
      0, 0, 0, 0, Handle, ID_LABEL, HInstance, nil);
    SendMessageW(FLabel, WM_SETFONT, Font, 0);
  end;

  FLoadButton := CreateButton(SLoadButtonText, ID_LOAD);
  FSaveButton := CreateButton(SSaveButtonText, ID_SAVE);
  FOkButton := CreateButton(SOKButtonText, ID_OK, True);
  FCancelButton := CreateButton(SCancelButtonText, ID_CANCEL);

  AlignControls;
  UpdateLabelText;
end;

procedure TTBXStrEditDlg.CreateHandle;
var
  ClassRegistered: Boolean;
  WindowClass, TempClass: TWndClassW;
  ExStyle, Style: Cardinal;
begin
  FWinClassName := ClassType.ClassName + '_CLASS';

  ClassRegistered := GetClassInfoW(HInstance, PWideChar(FWinClassName), TempClass);
  Assert((DlgWindowRefCounter > 0) = ClassRegistered);
  if not ClassRegistered then
  begin
    with WindowClass do
    begin
      style := CS_VREDRAW or CS_HREDRAW or CS_DBLCLKS;
      lpfnWndProc := @DlgWindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := SysInit.HInstance;
      hIcon := Application.Icon.Handle;
      hCursor := Screen.Cursors[crDefault];
      hbrBackground := GetSysColorBrush(COLOR_BTNFACE);
      lpszMenuName := nil;
      lpszClassName:= PWideChar(FWinClassName);
    end;
    RegisterClassW(WindowClass);
    ClassRegistered := GetClassInfoW(HInstance, PWideChar(FWinClassName), TempClass);
    if not ClassRegistered then RaiseLastOSError;
  end;
  Inc(DlgWindowRefCounter);

  Style := WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_POPUP or WS_CAPTION or
    WS_TABSTOP or WS_SYSMENU or WS_DLGFRAME or WS_THICKFRAME;
  ExStyle := WS_EX_CONTROLPARENT or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;

  FHandle := CreateWindowExW(ExStyle, PWideChar(FWinClassName), PWideChar(FCaption), Style,
    BoundsRect.Left, BoundsRect.Top,
    BoundsRect.Right - BoundsRect.Left, BoundsRect.Bottom - BoundsRect.Top,
    WndParent, 0, HInstance, nil);
  if FHandle = 0 then RaiseLastOSError;

  SetWindowLong(Handle, GWL_USERDATA, Integer(Self));
  SendMessageW(Handle, WM_SETICON, 1, 0);
end;

procedure TTBXStrEditDlg.DefaultHandler(var Message);
begin
  with TMessage(Message) do Result := DefWindowProcW(Handle, Msg, WParam, LParam);
end;

destructor TTBXStrEditDlg.Destroy;
begin
  DestroyControls;
  if FMultiLine then
  begin
    IsMultiLineRectStored := True;
    GetWindowRect(Handle, StoredMultiLineRect);
  end
  else
  begin
    IsSingleLineRectStored := True;
    GetWindowRect(Handle, StoredSingleLineRect);
  end;
  DestroyHandle;
  inherited;
end;

procedure TTBXStrEditDlg.DestroyControls;
begin
  DestroyWindow(FEdit);
  DestroyWindow(FLoadButton);
  DestroyWindow(FSaveButton);
  DestroyWindow(FOkButton);
  DestroyWindow(FCancelButton);
  if FMultiLine then DestroyWindow(FLabel);
end;

procedure TTBXStrEditDlg.DestroyHandle;
begin
  try
    if Handle <> 0 then
    begin
      if not Windows.DestroyWindow(FHandle) then RaiseLastOSError;
      FHandle := 0;
    end;
  finally
    Dec(DlgWindowRefCounter);
    if (DlgWindowRefCounter = 0) and
      not UnregisterClassW(PWideChar(FWinClassName), HInstance) then RaiseLastOSError;
  end;
end;

function TTBXStrEditDlg.GetEditText: WideString;
begin
  SetLength(Result, GetWindowTextLengthW(FEdit) + 1);
  GetWindowTextW(FEdit, PWideChar(Result), Length(Result));
  SetLength(Result, Length(Result) - 1);
end;

procedure TTBXStrEditDlg.LoadFile;
var
  OFN: OpenFileNameW;
  FileName: WideString;
  ActiveWindow: HWnd;
  WindowList: Pointer;
  FPUControlWord: Word;
  FocusState: TFocusState;
  Res: Boolean;
  Filters: WideString;
  S: WideString;
begin
  ZeroMemory(@OFN, SizeOf(OFN));
  OFN.lStructSize := SizeOf(OFN);
  OFN.hWndOwner := Handle;
  OFN.hInstance := SysInit.HInstance;
  SetLength(FileName, MAX_PATH + 2);
  FillChar(FileName[1], (MAX_PATH + 2) * SizeOf(WideChar), 0);
  OFN.lpstrFile := PWideChar(FileName);
  OFN.nMaxFile := MAX_PATH;
  Filters := 'Text files (*.txt)'#0'*.txt'#0'All files (*.*)'#0'*.*'#0;
  OFN.lpstrFilter := PWideChar(Filters);
  OFN.nFilterIndex := 2;
  OFN.lpstrInitialDir := '.';
  OFN.Flags := OFN_ENABLESIZING or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or OFN_SHOWHELP;

  ActiveWindow := {$IFDEF JR_D9}Application.ActiveFormHandle{$ELSE}GetActiveWindow{$ENDIF};
  WindowList := DisableTaskWindows(0);
  FocusState := SaveFocusState;
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Res := GetOpenFileNameW(OFN);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
  if Res then
  begin
    LoadWideString(FileName, S, WSF_AUTO);
    Self.SetEditText(S);
    UpdateLabelText;
  end;
end;

procedure TTBXStrEditDlg.SaveFile;
var
  OFN: OpenFileNameW;
  FileName: WideString;
  ActiveWindow: HWnd;
  WindowList: Pointer;
  FPUControlWord: Word;
  FocusState: TFocusState;
  Res: Boolean;
  Filters, S: WideString;
begin
  ZeroMemory(@OFN, SizeOf(OFN));
  OFN.lStructSize := SizeOf(OFN);
  OFN.hWndOwner := Handle;
  OFN.hInstance := SysInit.HInstance;
  SetLength(FileName, MAX_PATH + 2);
  FillChar(FileName[1], (MAX_PATH + 2) * SizeOf(WideChar), 0);
  OFN.lpstrFile := PWideChar(FileName);
  OFN.nMaxFile := MAX_PATH;
  OFN.lpstrInitialDir := '.';
  Filters := 'Text files (*.txt)'#0'*.txt'#0'All files (*.*)'#0'*.*'#0;
  OFN.lpstrFilter := PWideChar(Filters);
  OFN.nFilterIndex := 2;
  OFN.Flags := OFN_ENABLESIZING or OFN_CREATEPROMPT or OFN_OVERWRITEPROMPT or OFN_SHOWHELP;

  ActiveWindow := {$IFDEF JR_D9}Application.ActiveFormHandle{$ELSE}GetActiveWindow{$ENDIF};
  WindowList := DisableTaskWindows(0);
  FocusState := SaveFocusState;
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Res := GetSaveFileNameW(OFN);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
  if Res then
  begin
    S := GetEditText;
    if S = WideString(AnsiString(S)) then SaveWideString(FileName, S, WDF_ANSI)
    else SaveWideString(FileName, S, WDF_UTF8);
  end;
end;

procedure TTBXStrEditDlg.SetEditLimit(NewLimit: Integer);
begin

end;

procedure TTBXStrEditDlg.SetEditText(const Value: WideString);
begin
  SetWindowTextW(FEdit, PWideChar(Value));
  UpdateLabelText;
end;

function TTBXStrEditDlg.ShowModal: Integer;
var
  Msg: TMsg;
  WindowList: Pointer;
  SaveFocusState: TFocusState;
  SaveCursor: TCursor;
  ActiveWindow: HWnd;
  Unicode, MsgExists: Boolean;
begin
  Result := 0;
  if Handle = 0 then Exit;
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  ReleaseCapture;
  {$IFDEF JR_D7}
  Application.ModalStarted;
  {$ENDIF}
  try
    ActiveWindow := GetActiveWindow;
    SaveFocusState := Forms.SaveFocusState;
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crDefault;
    WindowList := DisableTaskWindows(0);
    try
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_SHOWWINDOW);
      try
        ModalResult := 0;

        repeat
          if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
          begin
            Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
            if Unicode then MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
            else MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE);
            if not MsgExists then Break;

            if not IsDialogmessageW(Handle, Msg) then
            begin
              TranslateMessage(Msg);
              if Unicode then DispatchMessageW(Msg)
              else DispatchMessage(Msg);
            end;
          end;
          if Application.Terminated then ModalResult := mrCancel;
        until ModalResult <> 0;
        Result := ModalResult;
        if GetActiveWindow <> Handle then ActiveWindow := 0;
      finally
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW);
      end;
    finally
      Screen.Cursor := SaveCursor;
      EnableTaskWindows(WindowList);
      if ActiveWindow <> 0 then SetActiveWindow(ActiveWindow);
      RestoreFocusState(SaveFocusState);
    end;
  finally
    {$IFDEF JR_D7}
    Application.ModalFinished;
    {$ENDIF}
  end;
end;

procedure TTBXStrEditDlg.UpdateLabelText;
var
  S: WideString;
  P: PWideChar;
  RowCount: Integer;
begin
  S := GetEditText;
  P := PWideChar(S);
  RowCount := 0;
  if P^ <> #0 then Inc(RowCount);
  while P^ <> #0 do
  begin
    if P^ = #13 then
    begin
      Inc(P);
      if P^ = #10 then Inc(P);
      if P^ <> #0 then Inc(RowCount);
    end
    else if P^ = #10 then
    begin
      Inc(P);
      if P^ = #13 then Inc(P);
      if P^ <> #0 then Inc(RowCount);
    end
    else Inc(P);
  end;
  FLabelText := IntToStr(RowCount) + ' line';
  if RowCount <> 1 then FLabelText := FLabelText + 's';
  SetWindowTextW(FLabel, PWideChar(FLabelText));
end;

procedure TTBXStrEditDlg.WMClose(var Message: TWMClose);
begin
  ModalResult := mrCancel;
end;

procedure TTBXStrEditDlg.WMCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    BN_CLICKED:
      if Message.Ctl = FOKButton then ModalResult := mrOk
      else if Message.Ctl = FCancelButton then ModalResult := mrCancel
      else if Message.Ctl = FLoadButton then LoadFile
      else if Message.Ctl = FSaveButton then SaveFile;
    EN_CHANGE:
      if (Message.Ctl = FEdit) and FMultiLine then
        UpdateLabelText;
  end;
  inherited;
end;

procedure TTBXStrEditDlg.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  Message.MinMaxInfo.ptMinTrackSize := FMinTrackSize;
  if not FMultiLine then
    Message.MinMaxInfo.ptMaxTrackSize.Y := FMinTrackSize.Y;
end;

procedure TTBXStrEditDlg.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
  Pt: TPoint;
begin
  inherited;
  GetClientRect(Handle, R);
  R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL);
  R.Top := R.Bottom - GetSystemMetrics(SM_CYVSCROLL);
  Pt := SmallPointToPoint(Message.Pos);
  Windows.ScreenToClient(Handle, Pt);
  if PtInRect(R, Pt) then
    Message.Result := HTBOTTOMRIGHT;
end;

procedure TTBXStrEditDlg.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
  R: TRect;
begin
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    GetClientRect(Handle, R);
    R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL);
    R.Top := R.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    DrawFrameControl(DC, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TTBXStrEditDlg.WMSize(var Message: TWMSize);
begin
  inherited;
  AlignControls;
end;

end.
