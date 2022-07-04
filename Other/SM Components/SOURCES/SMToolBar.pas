{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
}
unit SMToolBar;

interface

{$I SMVersion.inc}

uses Classes, Messages, Windows, Controls, ComCtrls;

type
  TSMToolbar = class(TToolbar)
  private
    FAdjustToolbar: Boolean;

    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CNNotify(var Message: TMessage); message CN_NOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    function GetButtonState(btn: TToolButton): Byte;
    function UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
    function UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
    procedure UpdateButtons;
  public
    procedure Customize;
  end;

procedure Register;

implementation

uses CommCtrl, SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMToolbar]);
end;

type
  TTBButtonInfo = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PAnsiChar;
    cchText: Integer;
  end;

const
  ComCtlDllName = 'comctl32.dll';

  ComCtlVersionIE3 = $00040046;
  ComCtlVersionIE4 = $00040047;
  ComCtlVersionIE401 = $00040048;
  ComCtlVersionIE5 = $00050050;

  TBIF_IMAGE       = $00000001;
  TBIF_TEXT        = $00000002;
  TBIF_STATE       = $00000004;
  TBIF_STYLE       = $00000008;
  TBIF_LPARAM      = $00000010;
  TBIF_COMMAND     = $00000020;
  TBIF_SIZE        = $00000040;

  TBSTATE_MARKED   = $80;
  TBSTYLE_AUTOSIZE = $0010; // automatically calculate the cx of the button

  TB_SETBUTTONINFO        = WM_USER + 65;

var
  ComCtlVersion: Integer;

const
  ButtonStates: array[TToolButtonState] of Word = (TBSTATE_CHECKED,
    TBSTATE_PRESSED, TBSTATE_ENABLED, TBSTATE_HIDDEN, TBSTATE_INDETERMINATE,
    TBSTATE_WRAP
    {$IFDEF SMForDelphi4}
    , TBSTATE_ELLIPSES, TBSTATE_MARKED
    {$ENDIF}
    );
  ButtonStyles: array[TToolButtonStyle] of Word = (TBSTYLE_BUTTON, TBSTYLE_CHECK,
    TBSTYLE_DROPDOWN, TBSTYLE_SEP, TBSTYLE_SEP {$IFDEF SMForDelphi2006}, BTNS_SHOWTEXT {$ENDIF});

function GetComCtlVersion: Integer;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ComCtlVersion = 0 then
  begin
    // GetFileVersionInfo modifies the filename parameter data while parsing.
    // Copy the string const into a local variable to create a writeable copy.
    FileName := ComCtlDllName;
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            ComCtlVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := ComCtlVersion;
end;

procedure TSMToolbar.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
    Style := Style or CCS_ADJUSTABLE;
end;

procedure TSMToolbar.WMNotify(var Message: TWMNotify);
var
  pnmTB: PNMToolBar;
begin
  inherited;

  case Message.NMHdr^.Code of
    TBN_GETBUTTONINFO:
      begin
       pnmTB := PNMToolBar(Message.NMHdr);
       if pnmTB^.iItem >= ButtonCount then exit;
       pnmTB^.tbButton.iBitmap := pnmTB^.iItem;
       pnmTB^.tbButton.idCommand := pnmTB^.iItem;
       pnmTB^.tbButton.dwData := LongInt(Buttons[pnmTB^.iItem]);
       pnmTB^.tbButton.fsState := TBSTATE_ENABLED;
       if not Buttons[pnmTB^.iItem].Visible then
         pnmTB^.tbButton.fsState := pnmTB^.tbButton.fsState or TB_ISBUTTONHIDDEN;

       pnmTB^.tbButton.fsStyle := TBSTYLE_BUTTON;
       pnmTB^.tbButton.bReserved[1] := 0;
       pnmTB^.tbButton.bReserved[2] := 0;
       pnmTB^.tbButton.iString := 0;
       Message.Result := 1;
      end;
    TBN_TOOLBARCHANGE:
      begin
//        SendMessage(Handle, TB_SAVERESTORE, 1, LongInt(@TBS));
        Message.Result:= 1;
      end;
    TBN_RESET:
      begin
//        SendMessage(Handle, TB_SAVERESTORE, 0, LongInt(@TBS));{???}
        Message.Result:= 1;
      end;
    TBN_BEGINADJUST, TBN_BEGINDRAG, TBN_ENDDRAG,
    TBN_QUERYDELETE, TBN_QUERYINSERT, TBN_ENDADJUST:
      begin
         Message.Result:= 1;
      end;
  end;
end;

procedure TSMToolbar.CNNotify(var Message: TMessage);
var
//  PButtonNotify: PTagTBNotify;
  pnmTB: PNMToolBar;
  Text: string;
begin
  inherited;

//exit;

  if Message.Result = 1 then exit;

  case PNMHdr(Message.LPARAM)^.Code of
    TBN_GETBUTTONINFO:
      begin
       pnmTB := PNMToolBar(PNMHdr(Message.LPARAM));
       if pnmTB^.iItem >= ButtonCount then exit;
       pnmTB^.tbButton.iBitmap := pnmTB^.iItem;
       pnmTB^.tbButton.idCommand := pnmTB^.iItem;
       pnmTB^.tbButton.dwData := LongInt(Buttons[pnmTB^.iItem]);
       pnmTB^.tbButton.fsState := TBSTATE_ENABLED;
       if not Buttons[pnmTB^.iItem].Visible then
         pnmTB^.tbButton.fsState := pnmTB^.tbButton.fsState or TB_ISBUTTONHIDDEN;

       pnmTB^.tbButton.fsStyle := TBSTYLE_BUTTON;
       pnmTB^.tbButton.bReserved[1] := 0;
       pnmTB^.tbButton.bReserved[2] := 0;
       pnmTB^.tbButton.iString := 0;
       Message.Result := 1;
      end;
    TBN_TOOLBARCHANGE:
      begin
//        SendMessage(Handle, TB_SAVERESTORE, 1, LongInt(@TBS));
        Message.Result:= 1;
      end;
    TBN_RESET:
      begin
//        SendMessage(Handle, TB_SAVERESTORE, 0, LongInt(@TBS));{???}
        Message.Result:= 1;
      end;
    TBN_BEGINADJUST, TBN_BEGINDRAG, TBN_ENDDRAG,
    TBN_QUERYDELETE, TBN_QUERYINSERT, TBN_ENDADJUST:
      begin
         Message.Result:= 1;
      end;
  end;
exit;

  Message.Result:=0;
  case PNMHdr(Message.LPARAM)^.Code of
    TBN_QUERYINSERT: Message.Result := 1;

    TBN_QUERYDELETE: begin
                       if //((FDrag = dr_None) or (FDrag = dr_Custom)) and
                          (not FAdjustToolbar) then
                         Message.Result := 0
                       else
                         Message.Result := 1;//Longint(CanDelete(PTagTBNotify(Message.LParam).Item));
                     end;

    TBN_GETBUTTONINFO: begin
{                         PButtonNotify := PTagTBNotify(Message.LParam);
                         if (PButtonNotify^.Item < FToolbarButtons.Count) then
                         begin
                           PButtonNotify^.TbButton := FToolbarButtons[PButtonNotify^.Item].Buttonrec;
                           if (PButtonNotify^.cchText <> 0) then
                             StrCopy(PButtonNotify^.pszText, PChar(FToolbarButtons.Tips[PButtonNotify^.TbButton.IdCommand]+'1'));
                           Message.Result := 1;
                         end
                         else
                         if (PButtonNotify^.Item - FToolbarButtons.Count < FAvailableButtons.Count) then
                         begin
                           PButtonNotify^.TBButton := FAvailableButtons[PButtonNotify^.Item - FToolbarButtons.Count].ButtonRec;
                           if (PButtonNotify^.cchText <> 0) then
                             StrCopy(PButtonNotify^.pszText, PChar(FAvailableButtons.Tips[PButtonNotify^.TbButton.IdCommand]+'2'));
                           Message.Result := 1;
                         end
                         else
}                           Message.Result := 0;
                       end;

    TBN_BEGINADJUST: begin
                       FAdjustToolbar := True;
//                       if Assigned(FOnBeginADJUST) then
//                         FOnBeginADJUST(Self);
                     end;

    TBN_ENDADJUST: begin
                     FAdjustToolbar:=False;
//                     UpdateButtons;
                     SendMessage(Handle, TB_AutoSize, 0, 0);
                     Parent.ReAlign;
//                     if Assigned(FOnENDADJUST) then
//                       FOnENDADJUST(Self);
                   end;

    TBN_BEGINDRAG: begin
                     Message.Result := 0;
{                     if Assigned(FOnBeginDrag) then
                       FOnBeginDrag(Self);
}                   end;

    TBN_ENDDRAG: begin
{                   if Assigned(FOnEndDrag) then
                     FOnEndDrag(Self);
}                   Message.Result := 0;
                 end;

    TBN_TOOLBARCHANGE: begin
                         if not FAdjustToolbar then
                           UpdateButtons;
                         SendMessage(Handle, TB_AutoSize, 0, 0);
                         Parent.ReAlign;
//                         if Assigned(FOnTOOLBARCHANGE) then
//                           FOnTOOLBARCHANGE(Self);
                       end;

    TBN_RESET: begin
{                 RestoreResetInfo;
                 if Assigned(FOnReset) then
                   FOnReset(Self);
}               end;

    TBN_CUSTHELP: begin
{                    if Assigned(FOnHelp) then
                      FOnHelp(Self);
}                  end;
  else
  end;
end;

function TSMToolBar.GetButtonState(btn: TToolButton): Byte;
begin
  Result := 0;
  if btn.Down then
    if btn.Style = tbsCheck then
      Result := Result or ButtonStates[tbsChecked]
    else
      Result := Result or ButtonStates[tbsPressed];
  if btn.Enabled {and ((ToolBar = nil) or ToolBar.Enabled) }then
    Result := Result or ButtonStates[tbsEnabled];
  if not btn.Visible and not (csDesigning in btn.ComponentState) then
    Result := Result or ButtonStates[tbsHidden];
//  if Indeterminate then Result := Result or ButtonStates[tbsIndeterminate];
//  if Wrap then Result := Result or ButtonStates[tbsWrap];
//  if Marked then Result := Result or ButtonStates[tbsMarked];
end;

function TSMToolBar.UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
var
  Control: TControl;
  Button: TTBButton;
  CaptionText: string;
  Buffer: array[0..4095] of Char;
begin
  Control := TControl(Buttons[FromIndex]);
  if Control is TToolButton then
    with TToolButton(Control) do
    begin
      FillChar(Button, SizeOf(Button), 0);
      if Style in [tbsSeparator, tbsDivider] then
      begin
        Button.iBitmap := Width;
        Button.idCommand := -1;
      end
      else
      begin
        if ImageIndex < 0 then
          Button.iBitmap := -2 else
          Button.iBitmap := ImageIndex;
        Button.idCommand := FromIndex;
      end;
      with Button do
      begin
        fsStyle := ButtonStyles[Style];
        if AutoSize and (GetComCtlVersion >= ComCtlVersionIE4) then
          fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
      end;
      Button.fsState := GetButtonState(TToolButton(Control));
      if Grouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
      Button.dwData := Longint(Control);
      if ShowCaptions then
      begin
        if Caption <> '' then
          CaptionText := Caption
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' ';
        StrPCopy(Buffer, CaptionText);
        { TB_ADDSTRING requires two null terminators }
        Buffer[Length(CaptionText) + 1] := #0;
        Button.iString := Self.Perform(TB_ADDSTRING, 0, Longint(@Buffer));
      end
      else
        Button.iString := -1;
    end
  else
  begin
    FillChar(Button, SizeOf(Button), 0);
    Button.fsStyle := ButtonStyles[tbsSeparator];
    Button.iBitmap := Control.Width;
    Button.idCommand := -1;
    if not Control.Visible and not (csDesigning in Control.ComponentState) then
      Button.fsState := Button.fsState or ButtonStates[tbsHidden];
    Button.dwData := Longint(Control);
    Button.iString := -1;
  end;
  Result := Self.Perform(Message, ToIndex, Integer(@Button)) <> 0;
end;

function TSMToolBar.UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
var
  Control: TControl;
  Button: TTBButtonInfo;
  CaptionText: string;
  Buffer: array[0..4095] of Char;
begin
  Control := TControl(Buttons[FromIndex]);
  FillChar(Button, SizeOf(Button), 0);
  Button.cbSize := SizeOf(Button);
  if Control is TToolButton then
    with TToolButton(Control) do
    begin
      Button.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or TBIF_COMMAND
        or TBIF_SIZE;
      if Style in [tbsSeparator, tbsDivider] then
      begin
        Button.idCommand := -1;
      end
      else
      begin
        Button.dwMask := Button.dwMask or TBIF_IMAGE;
        if ImageIndex < 0 then
          Button.iImage := -2 else
          Button.iImage := ImageIndex;
        Button.idCommand := FromIndex;
      end;
      with Button do
      begin
        cx := Width;
        fsStyle := ButtonStyles[Style];
        if AutoSize then fsStyle := fsStyle or TBSTYLE_AUTOSIZE;
        if Grouped then Button.fsStyle := Button.fsStyle or TBSTYLE_GROUP;
      end;
      Button.fsState := GetButtonState(TToolButton(Control));
      Button.lParam := Longint(Control);
      if ShowCaptions then
      begin
        if Caption <> '' then
          CaptionText := Caption
        else
          { Common control requries at least a space is used when showing button
            captions.  If any one button's caption is empty (-1) then none of
            the buttons' captions will not be displayed. }
          CaptionText := ' ';
        StrPCopy(Buffer, CaptionText);
        { TB_ADDSTRING requires two null terminators }
        Buffer[Length(CaptionText) + 1] := #0;
        //Button.iString := Self.Perform(TB_ADDSTRING, 0, Longint(@Buffer));
        Button.pszText := Buffer;
        Button.cchText := Length(CaptionText);
        Button.dwMask := Button.dwMask or TBIF_TEXT;
      end
      else
      begin
        Button.pszText := nil;
        Button.cchText := 0;
      end;

if Style in [tbsSeparator, tbsDivider] then
begin
  with Button do
  begin
    dwMask := TBIF_STYLE or TBIF_STATE or TBIF_LPARAM;
    fsState := TBSTATE_ENABLED or TBSTATE_WRAP;
    fsStyle := TBSTYLE_BUTTON;
  end;
end;

    end
  else
  begin
    Button.dwMask := TBIF_TEXT or TBIF_STATE or TBIF_STYLE or TBIF_LPARAM or
      TBIF_COMMAND or TBIF_SIZE;
    Button.fsStyle := ButtonStyles[tbsSeparator];
    Button.cx := Control.Width;
    Button.idCommand := -1;
    Button.lParam := Longint(Control);
    Button.pszText := nil;
    Button.cchText := 0;
  end;
  Result := Self.Perform(Message, ToIndex, Integer(@Button)) <> 0;
end;

procedure TSMToolBar.UpdateButtons;
const
  BlankButton: TTBButton = (iBitmap: 0; idCommand: 0; fsState: 0;
    fsStyle: TBSTYLE_BUTTON; dwData: 0; iString: 0);
var
  i: Integer;
  Count: Integer;
  Style: Longint;
begin
  try
    HandleNeeded;
    Style := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, Style and not WS_VISIBLE);
    try
      Count := ButtonCount;
      for i := 0 to ButtonCount - 1 do
      begin
        if i < Count then
          UpdateItem2(TB_SETBUTTONINFO, i, i)
        else
          UpdateItem(TB_INSERTBUTTON, i, i);
      end;
    finally
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  finally
  end;
  RepositionButtons(0);
end;

procedure TSMToolbar.Customize;
begin
  SendMessage(Self.Handle, TB_CUSTOMIZE, 0, 0);
end;

{Mab heeft geschreven in bericht <68rkgc$ict@Alf.ho.bunge.com.au>...
>Hi all,
>
>I've been experimenting with the TB_CUSTOMIZE message and TToolbar.
>Here's how far I got:
>
>1. Created a new component, descended from TToolbar, and added
>CCS_ADJUSTABLE to its create params.
>
>2. Added a WM_NOTIFY handler to my main form, and checked for
>TBN_QUERYINSERT, returning 1 all the time.
>
>Sweet! Now when I double click on the toolbar, I get a cute "customize
>toolbar" dialog pop up!
>
>Only problem is getting the buttons to appear in the dialog. Has
>anyone successfully handled the TBN_GETBUTTONINFO message? How do I do
>it?
>
>If you'd like to look at this yourself (it took me about an hour to
>figure out the above) then look up TB_CUSTOMIZE in the Win32 API help
>file.
>
>Email me if you'd like to create a customizable toolbar component with
>me!
}
end.
