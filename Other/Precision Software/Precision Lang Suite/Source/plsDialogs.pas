{------------------------------------------------------------------------------
  plsDialogs.pas

  Precision Language Suite

  Purpose:    Basic system dialogs (messages, input boxes) with localization support

  The initial author of the source code and the copyright holder is CodeGear,
  Copyright (c) 1995-2008 CodeGear

  The localization support enhancements are written by Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  The enhancements source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.

------------------------------------------------------------------------------
  History:

  - Version: 2.5.2
    * added: Support for Delphi XE6 and XE7

  - Version: 2.5.1
    * improved: Returning value of InputQueryLM with csDropDown style of ComboBox now matches appropriate item in the list
                 (ie. after typing a few characters to find an item and pressing Enter key)
    * improved: InputQueryLM dialog is now better adapted to the width of available items
    * improved: The unit has been redesigned to provide one source for all supported platforms (VCL, LCL, FMX)

  - Version: 2.2.4.19
    * added - Support for "elevation required" shields (contribution by ouiouioui, www.toutenvrac.org)
------------------------------------------------------------------------------}

{ Basic system dialogs (messages, input boxes) with localization support }

unit plsDialogs;

interface

uses
  {$IFDEF FPC} rtlconsts {$ELSE} Consts {$ENDIF},
  Classes,
  Dialogs;

var
  pls_Dialogs_VCLStylesBorder: Boolean = True;

  lng_MsgDlgCaptions:array[TMsgDlgType] of string = (
    SMsgDlgWarning,SMsgDlgError,SMsgDlgInformation,SMsgDlgConfirm,'');

  lng_ButtonCaptions: array[TMsgDlgBtn] of string = (
    SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp {$IFDEF FPC} , SMsgDlgOK {$ELSE} {$IF CompilerVersion>=20} , SMsgDlgClose {$IFEND} {$ENDIF} );

{ Procedure plsDialogs.LanguageChanged must be called every time the LanguageManager loads a new language.
  You can do it in BeforeLangChange event handler of TplsController component or in OnLanguageChanged event handler of TLanguageManager class.
  The following language identifiers should be defined in your language files:
    lng_MsgDlgWarning, lng_MsgDlgError, lng_MsgDlgInformation, lng_MsgDlgConfirm,
    lng_MsgBtnYes, lng_MsgBtnNo, lng_MsgBtnOK, lng_MsgBtnCancel, lng_MsgBtnAbort, lng_MsgBtnRetry,
    lng_MsgBtnIgnore, lng_MsgBtnAll, lng_MsgBtnNoToAll, lng_MsgBtnYesToAll, lng_MsgBtnHelp and lng_MsgBtnClose. }
procedure LanguageChanged;
{ function MessageDlgLM supports standard MessageDlg (WinXP/2000) and TaskMessageDlg(Vista/7) routines - localized,
  it is an alias for MessageDlg function }
function MessageDlgLM(const MsgCaption, Msg: string; DlgType: TMsgDlgType; Buttons:TMsgDlgButtons = [mbOK];
  DefaultButton:TMsgDlgBtn=mbOK; X:Integer=-1; Y:Integer=-1; HelpCtx:Longint=0; const HelpFileName:string=''
  {$IFNDEF FPC} ; BtnElevationRequired: TMsgDlgButtons = [] {$ENDIF} ): Integer;
// function MessageDlg supports standard MessageDlg (WinXP/2000) and TaskMessageDlg(Vista/7) routines - localized
function MessageDlg(const MsgCaption, Msg: string; DlgType: TMsgDlgType; Buttons:TMsgDlgButtons = [mbOK];
  DefaultButton:TMsgDlgBtn=mbOK; X:Integer=-1; Y:Integer=-1; HelpCtx:Longint=0; const HelpFileName:string=''
  {$IFNDEF FPC} ; BtnElevationRequired: TMsgDlgButtons = [] {$ENDIF} ): Integer; overload;

{ function InputQueryLM supports standard InputQuery routines - localized, it is an alias for InputQuery function }
function InputQueryLM(const ACaption, APrompt: string;
  var Value: string; sItems:TStrings=nil; Fixed:Boolean=False): Boolean;
{ function InputQueryLM supports standard InputQuery routines - localized }
function InputQuery(const ACaption, APrompt: string;
  var Value: string; sItems:TStrings=nil; Fixed:Boolean=False): Boolean; overload;
// function InputQueryMemoLM supports multiline text InputQuery - localized
function InputQueryMemoLM(const ACaption, APrompt: string;
  var Value: string; mWidth:Integer=232; mHeight:Integer=232): Boolean;

implementation

uses
  Windows, Graphics, Controls, Forms, StdCtrls, SysUtils,
  {$IFDEF FPC}
  buttons,
  {$ELSE}
  MultiMon, HelpIntfs, Themes,
  {$IF CompilerVersion>=23} Types, ExtCtrls, {$IFEND}
  {$ENDIF}
  plsLangMan;

{$IFNDEF FPC}
const
  _ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help' {$IFDEF FPC} , 'Close' {$ELSE} {$IF CompilerVersion>=20} , 'Close' {$IFEND} {$ENDIF}); // do not localize
{$ENDIF}

{$IFNDEF FPC}
{$IF CompilerVersion < 23.0 }
function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}
{$ENDIF}

procedure LanguageChanged;
begin
  with LanguageManager do
  begin
    lng_MsgDlgCaptions[mtWarning]:=LangText('lng_MsgDlgWarning',SMsgDlgWarning);
    lng_MsgDlgCaptions[mtError]:=LangText('lng_MsgDlgError',SMsgDlgError);
    lng_MsgDlgCaptions[mtInformation]:=LangText('lng_MsgDlgInformation',SMsgDlgInformation);
    lng_MsgDlgCaptions[mtConfirmation]:=LangText('lng_MsgDlgConfirm',SMsgDlgConfirm);

    lng_ButtonCaptions[mbYes]:=LangText('lng_MsgBtnYes',SMsgDlgYes);
    lng_ButtonCaptions[mbNo]:=LangText('lng_MsgBtnNo',SMsgDlgNo);
    lng_ButtonCaptions[mbOK]:=LangText('lng_MsgBtnOK',SMsgDlgOK);
    lng_ButtonCaptions[mbCancel]:=LangText('lng_MsgBtnCancel',SMsgDlgCancel);
    lng_ButtonCaptions[mbAbort]:=LangText('lng_MsgBtnAbort',SMsgDlgAbort);
    lng_ButtonCaptions[mbRetry]:=LangText('lng_MsgBtnRetry',SMsgDlgRetry);
    lng_ButtonCaptions[mbIgnore]:=LangText('lng_MsgBtnIgnore',SMsgDlgIgnore);
    lng_ButtonCaptions[mbAll]:=LangText('lng_MsgBtnAll',SMsgDlgAll);
    lng_ButtonCaptions[mbNoToAll]:=LangText('lng_MsgBtnNoToAll',SMsgDlgNoToAll);
    lng_ButtonCaptions[mbYesToAll]:=LangText('lng_MsgBtnYesToAll',SMsgDlgYesToAll);
    lng_ButtonCaptions[mbHelp]:=LangText('lng_MsgBtnHelp',SMsgDlgHelp);
    {$IFDEF FPC}
    lng_ButtonCaptions[mbClose]:=LangText('lng_MsgBtnClose',SMsgDlgOK);
    {$ELSE}
    {$IF CompilerVersion>=20}
    lng_ButtonCaptions[mbClose]:=LangText('lng_MsgBtnClose',SMsgDlgClose);
    {$IFEND}
    {$ENDIF}
  end;
end;

{$IFNDEF FPC}
{$IF CompilerVersion>=20}
{ TaskDialog based message dialog; requires Windows Vista or later }

type
  TSimpleTaskMessageDialog = class(TCustomTaskDialog)
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
  strict protected
    procedure DoOnButtonClicked(AModalResult: Integer; var CanClose: Boolean); override;
    procedure DoOnDialogCreated; override;
    procedure DoOnHelp; override;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Position: TPoint read FPosition write FPosition;
  end;

const
  tdbHelp = -1;

procedure TSimpleTaskMessageDialog.DoOnButtonClicked(AModalResult: Integer;
  var CanClose: Boolean);
begin
  if AModalResult = tdbHelp then
  begin
    CanClose := False;
    DoOnHelp;
  end;
end;

procedure TSimpleTaskMessageDialog.DoOnDialogCreated;
var
  Rect: TRect;
  LX, LY: Integer;
  LHandle: HMONITOR;
  LMonitorInfo: TMonitorInfo;
begin
  LX := Position.X;
  LY := Position.Y;
  LHandle := MonitorFromWindow(FParentWnd, MONITOR_DEFAULTTONEAREST);
  LMonitorInfo.cbSize := SizeOf(LMonitorInfo);
  if GetMonitorInfo(LHandle, @LMonitorInfo) then
    with LMonitorInfo do
    begin
      GetWindowRect(Handle, Rect);
      if LX < 0 then
        LX := ((rcWork.Right - rcWork.Left) - (Rect.Right - Rect.Left)) div 2;
      if LY < 0 then
        LY := ((rcWork.Bottom - rcWork.Top) - (Rect.Bottom - Rect.Top)) div 2;
      Inc(LX, rcWork.Left);
      Inc(LY, rcWork.Top);
      SetWindowPos(Handle, 0, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
    end;
end;

procedure TSimpleTaskMessageDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;
begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

function TSimpleTaskMessageDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;
{$IFEND}
{$ENDIF}

function MessageDlgLM(const MsgCaption, Msg: string; DlgType: TMsgDlgType; Buttons:TMsgDlgButtons = [mbOK];
  DefaultButton:TMsgDlgBtn=mbOK; X:Integer=-1; Y:Integer=-1; HelpCtx:Longint=0; const HelpFileName:string=''
  {$IFNDEF FPC} ; BtnElevationRequired: TMsgDlgButtons = [] {$ENDIF} ): Integer;
{$IFNDEF FPC}
{$IF CompilerVersion>=20}
const
  IconMap: array[TMsgDlgType] of TTaskDialogIcon = (tdiWarning, tdiError, tdiInformation, tdiInformation, tdiNone);
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, tdbHelp, mrClose);
var
  DlgBtn: TMsgDlgBtn;
  LTaskDialog: TSimpleTaskMessageDialog;
  Btn: TTaskDialogBaseButtonItem;
{$IFEND}
{$ENDIF}
var
  MessageDialog:TForm;
  offs:Integer;
  B:TMsgDlgBtn;
  LbMsg:TLabel;
  ml, oh, ph: Integer;
  {$IFDEF FPC}
  idx,i:integer;
  LButton, LDefBtn:TBitBtn;
  {$ELSE}
  LButton:TButton;
  {$ENDIF}
begin
  {$IFNDEF FPC}
  {$IF CompilerVersion>=20}
  if (Win32MajorVersion >= 6) and {$IF CompilerVersion >= 23} StyleServices.Enabled and pls_Dialogs_VCLStylesBorder {$ELSE} StyleServices.ThemesEnabled {$IFEND} then
  begin
    LTaskDialog := TSimpleTaskMessageDialog.Create(nil);
    try
      // Assign buttons
      for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if DlgBtn in Buttons then
        begin
          Btn := LTaskDialog.Buttons.Add;
          with TTaskDialogButtonItem(Btn) do
          begin
            Caption := lng_ButtonCaptions[DlgBtn];
            Default := DlgBtn = DefaultButton;
            ModalResult := LModalResults[DlgBtn];
            ElevationRequired := DlgBtn in BtnElevationRequired;
          end;
        end;
      // Set dialog properties
      with LTaskDialog do
      begin
        if DlgType <> mtCustom then
          Caption := lng_MsgDlgCaptions[DlgType]
        else
          Caption := Application.Title;
        CommonButtons := [];
        if Application.UseRightToLeftReading then
          Flags := Flags + [tfRtlLayout];
        HelpContext := HelpCtx;
        HelpFile := HelpFileName;
        if DlgType=mtConfirmation then
        begin
          CustomMainIcon.Handle := LoadIcon(0,IDI_QUESTION);
          if CustomMainIcon.Handle<>0 then
            LTaskDialog.Flags:=LTaskDialog.Flags+[tfUseHiconMain]
          else
            MainIcon :=  IconMap[DlgType]
        end
        else
          MainIcon :=  IconMap[DlgType];
        Position := Point(X, Y);
        Text := Msg;
        if Length(MsgCaption)=0 then
          Title := Application.Title
        else
          Title := MsgCaption;
      end;

      // Show dialog and return result
      Result := mrNone;
      if LTaskDialog.Execute then
        Result := LTaskDialog.ModalResult;
    finally
      LTaskDialog.Free;
    end;
  end
  else
  {$IFEND}
  {$ENDIF}
  begin
    MessageDialog:=CreateMessageDialog(Msg, DlgType, Buttons {$IFNDEF FPC} {$IF CompilerVersion>=20} , DefaultButton {$IFEND} {$ENDIF});
    try
      {$IFNDEF FPC}
      {$IF CompilerVersion >= 23}
      if not pls_Dialogs_VCLStylesBorder then
        MessageDialog.StyleElements := MessageDialog.StyleElements - [seBorder];
      {$IFEND}
      {$ENDIF}

      LbMsg := TLabel(MessageDialog.FindComponent('message'));
      if Assigned(LbMsg) then
        LbMsg.ShowAccelChar:=False;
      MessageDialog.HelpContext := HelpCtx;
      MessageDialog.HelpFile := HelpFileName;
      if X >= 0 then MessageDialog.Left := X;
      if Y >= 0 then MessageDialog.Top := Y;
      if (Y < 0) and (X < 0) then MessageDialog.Position := poScreenCenter;
      // localize Dialog caption
      if Length(MsgCaption)=0 then
        MessageDialog.Caption:=Application.Title
      else
        MessageDialog.Caption:=MsgCaption;
      if MessageDialog.ClientWidth<300 then
      begin
        offs:=150-(MessageDialog.ClientWidth div 2);
        MessageDialog.ClientWidth:=300;
      end
      else
        offs:=0;
      ml := 0; oh := 0; ph := -1;
      // localize buttons
      {$IFDEF FPC}
      idx:=-1;
      LDefBtn:=nil;
      for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if B in Buttons then
        begin
          inc(idx);
          for i:=0 to MessageDialog.ControlCount-1 do
              if i=idx then
              begin
                LButton := TBitBtn(MessageDialog.Components[i]);
                break;
              end;
          if Assigned(LButton) then
          begin
            if ph = -1 then
              ph := MessageDialog.ClientHeight - LButton.Top + MessageDialog.ClientHeight - LButton.BoundsRect.Bottom;
            LButton.Caption:=lng_ButtonCaptions[B];
            LButton.Left:=LButton.Left+offs;
            if (B = DefaultButton) or LButton.Default then
              LDefBtn := LButton;
            if LButton.Default then
              LButton.Default := False;
          end;
        end;
      if Assigned(LDefBtn) then
      begin
         LDefBtn.Default := True;
         MessageDialog.ActiveControl:=LDefBtn;
      end;
      {$ELSE}
      for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if B in Buttons then
        begin
          LButton := TButton(MessageDialog.FindComponent(_ButtonNames[B]));
          if Assigned(LButton) then
          begin
            if ph = -1 then
            begin
              oh := (MessageDialog.ClientHeight - LButton.BoundsRect.Bottom) div 2;
              ph := MessageDialog.ClientHeight - LButton.Top;
            end;
            LButton.Caption:=lng_ButtonCaptions[B];
            LButton.Left:=LButton.Left+offs;
            if LButton.BoundsRect.Right > ml then
              ml := LButton.BoundsRect.Right;
          end;
        end;
      {$ENDIF}

      {$IFNDEF FPC}
      {$IF CompilerVersion >= 23}
        if (not pls_Dialogs_VCLStylesBorder) and (ph >= 0) then
        begin
          with TPanel.Create(MessageDialog) do
          begin
            Parent := MessageDialog;
            Align := alBottom;
            Height := ph;
            BevelOuter := bvNone;
            BevelEdges := [beTop];
            BevelKind := bkTile;
            StyleElements := StyleElements - [seClient];
            Color := StyleServices.GetStyleColor(scPanel);
            SendToBack;
          end;

          ml := MessageDialog.ClientWidth - ml - oh;
          for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
            if B in Buttons then
            begin
              LButton := TButton(MessageDialog.FindComponent(_ButtonNames[B]));
              if Assigned(LButton) then
              begin
                LButton.Top := LButton.Top + oh;
                LButton.Left := LButton.Left + ml;
              end;
            end;
        end;
      {$IFEND}
      {$ENDIF}

      Result := MessageDialog.ShowModal;
    finally
      MessageDialog.Free;
    end;
  end;
end;

function MessageDlg(const MsgCaption, Msg: string; DlgType: TMsgDlgType; Buttons:TMsgDlgButtons = [mbOK];
  DefaultButton:TMsgDlgBtn=mbOK; X:Integer=-1; Y:Integer=-1; HelpCtx:Longint=0; const HelpFileName:string=''
  {$IFNDEF FPC} ; BtnElevationRequired: TMsgDlgButtons = [] {$ENDIF} ): Integer;
begin
  Result:=MessageDlgLM(MsgCaption, Msg, DlgType, Buttons, DefaultButton, X, Y, HelpCtx, HelpFileName {$IFNDEF FPC} , BtnElevationRequired {$ENDIF} );
end;

function _GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  {$IFDEF FPC}
  Result.x:=0; Result.y:=0;
  {$ENDIF}
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function InputQueryLM(const ACaption, APrompt: string;
  var Value: string; sItems:TStrings=nil; Fixed:Boolean=False): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  cEdit: TComboBox;
  i:Integer;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  dlgWidth:Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := _GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      if Assigned(sItems) and (sItems.Count>0) then
        dlgWidth:=Length(sItems[0])*DialogUnits.X
      else
        dlgWidth := Length(Value)*DialogUnits.X;
      if dlgWidth<248 then dlgWidth:=248
      else if dlgWidth>496 then dlgWidth:=496;
      ClientWidth := MulDiv(dlgWidth, DialogUnits.X, 4);
      {$IFDEF FPC}
      PopupMode := pmAuto;
      {$ELSE}
      {$IF CompilerVersion>=20}
      PopupMode := pmAuto;
      {$IFEND}
      {$ENDIF}
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(dlgWidth-16, DialogUnits.X, 4);
        ShowAccelChar:=False;
        WordWrap := True;
      end;
      cEdit:=nil;
      Edit:=nil;
      if Assigned(sItems) then
      begin
        cEdit := TComboBox.Create(Form);
        with cEdit do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := Prompt.Top + Prompt.Height + 5;
          Width := MulDiv(dlgWidth-16, DialogUnits.X, 4);
          MaxLength := 255;
          Items.Assign(sItems);
          if Fixed then
          begin
            Style:=csDropDownList;
            i:=Items.IndexOf(Value);
            if i>=0 then
              ItemIndex:=i
            else
            if Items.Count>0 then
              ItemIndex:=0;
          end
          else
            Text := Value;
          SelectAll;
        end;
        ButtonTop := cEdit.Top + cEdit.Height + 15;
      end
      else
      begin
        Edit := TEdit.Create(Form);
        with Edit do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := Prompt.Top + Prompt.Height + 5;
          Width := MulDiv(dlgWidth-16, DialogUnits.X, 4);
          MaxLength := 255;
          Text := Value;
          SelectAll;
        end;
        ButtonTop := Edit.Top + Edit.Height + 15;
      end;

      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := lng_ButtonCaptions[mbOK];
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv((dlgWidth div 2)-50-2, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := lng_ButtonCaptions[mbCancel];
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv((dlgWidth div 2)+2, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        if Assigned(sItems) and Assigned(cEdit) then
        begin
          if Fixed then
          begin
            if cEdit.ItemIndex>=0 then
              Value := cEdit.Items[cEdit.ItemIndex]
            else
              Value := '';
          end
          else
          if (cEdit.ItemIndex>=0) and AnsiSameText(cEdit.Text, cEdit.Items[cEdit.ItemIndex]) then
            Value := cEdit.Items[cEdit.ItemIndex]
          else
            Value := cEdit.Text
        end
        else
          Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function InputQuery(const ACaption, APrompt: string;
  var Value: string; sItems:TStrings=nil; Fixed:Boolean=False): Boolean;
begin
  Result:=InputQueryLM(ACaption, APrompt, Value, sItems, Fixed);
end;

function InputQueryMemoLM(const ACaption, APrompt: string;
  var Value: string; mWidth:Integer=232; mHeight:Integer=232): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := _GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      {$IFDEF FPC}
      PopupMode := pmAuto;
      {$ELSE}
      {$IF CompilerVersion>=20}
      PopupMode := pmAuto;
      {$IFEND}
      {$ENDIF}
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(mWidth, DialogUnits.X, 4);
        ShowAccelChar:=False;
        WordWrap := True;
      end;
      Edit := TMemo.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(mWidth, DialogUnits.X, 4);
        Height := MulDiv(trunc(mHeight/1.78), DialogUnits.Y, 8);
        Wordwrap := True;
        WantTabs := False;
        WantReturns := False;
        Lines.Text := Value;
        SelectAll;
      end;
      ClientWidth := Edit.Left + Edit.Width + 13;
      ButtonTop := Edit.Top + Edit.Height + 15;

      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := lng_ButtonCaptions[mbOK];
        ModalResult := mrOk;
        Default := True;
        SetBounds((Form.ClientWidth div 2)-ButtonWidth-2, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := lng_ButtonCaptions[mbCancel];
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds((Form.ClientWidth div 2)+2, ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Lines.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

end.
