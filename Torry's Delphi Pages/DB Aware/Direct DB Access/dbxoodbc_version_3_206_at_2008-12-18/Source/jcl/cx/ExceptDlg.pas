{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ExceptDlg.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Sample Application exception dialog replacement                                                  }
{                                                                                                  }
{ Last modified: $Date: 07-04-07 12:45 $                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit ExceptDlg;

{$I jcl.inc}

{$D+}

{+}

{$DEFINE _JCL_EXCEPTION_COMOBJ_}

{+.}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JclSysUtils, JclDebug, cxControls, cxContainer, cxEdit,
  cxImage, cxTextEdit, cxMemo, cxButtons, cxLabel;

const
  UM_CREATEDETAILS = WM_USER + $100;
  {+}
  UM_ADDREPORT = UM_CREATEDETAILS + 1;
  cMaxCycledErrors = 7;
  cMaxErrors = 100; // or = MaxInt
  {+.}

  ReportToLogEnabled   = $00000001; // TExceptionDialog.Tag property
  DisableTextScrollbar = $00000002; // TExceptionDialog.Tag property

type

  TExcDialogSystemInfo = (siStackList, siOsInfo, siModuleList, siActiveControls);
  TExcDialogSystemInfos = set of TExcDialogSystemInfo;
  {+}
  TExceptionDialog = class;
  TOnShowException = procedure(Sender: TExceptionDialog; E: Exception) of object;
  {+.}

  TExceptionDialog = class(TForm)
    Bevel1: TBevel;
    ShapeDetailsMemo: TShape;
    ShapeTextLabel: TShape;
    fImage: TcxImage;
    TextLabel: TcxMemo;
    DetailsMemo: TcxMemo;
    OkBtn: TcxButton;
    CopyButton: TcxButton;
    DetailsBtn: TcxButton;
    LErrorCount: TcxLabel;
    LErrorCountCaption: TcxLabel;
    KillBtn: TcxButton;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure KillBtnClick(Sender: TObject);
  private
    FDetailsVisible: Boolean;
    FThreadID: DWORD;
    FIsMainThead: Boolean;
    FLastActiveControl: TWinControl;
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
    FSimpleLog: TJclSimpleLog;

    FDetailsCalculated: Boolean;

    {+}
    fEMessage: string;
    fStackListCaption: string;
    fStackListFirst: TJclStackInfoList;
    fExceptAddrFirst: Pointer;
    fLastActiveControlFirst: TWinControl;
    {+.}

    procedure CreateDetails;
    function GetReportAsText: string;
    procedure ReportToLog;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
    {+}
    procedure MakeStackList(bCreateByExceptAddr: Boolean = False);
    procedure CleanStackList();
    procedure DoCalculateDetails();
    procedure UMAddReport(var Message: TMessage); message UM_ADDREPORT;
    procedure AddReport();
    procedure DoAddReport();
    {+.}
  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetailInfo; dynamic;
    procedure CreateReport(const SystemInfo: TExcDialogSystemInfos);
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: Char; virtual;
    procedure NextDetailBlock;
    {+}
    function DelimiterStr: string;
    {+.}
    procedure UpdateTextLabelScrollbars;

  {+}
  protected
    fErrorIcon: HICON;
    fSaveDetailsVisible: Boolean;
    fSaveHeight: Integer;
    fOnShowException: TOnShowException;
    iErrorCount: Integer;

    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure DoOnShowException(E: Exception; var Handled: Boolean); virtual;
    procedure PaintIcon(); virtual;
  {+.}
  public
    procedure CopyReportToClipboard();
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: TObject; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;
    property SimpleLog: TJclSimpleLog read FSimpleLog;
  end;

  TExceptionDialogClass = class of TExceptionDialog;

var
  ExceptionDialogClass: TExceptionDialogClass = TExceptionDialog;
  {+}
  FirstExceptionImmediateStackCalculate: Boolean = False;
  RawExceptionTrace: Boolean = True;
  ExcDialogSystemInfos: TExcDialogSystemInfos = [siStackList, siOsInfo, siModuleList, siActiveControls];
  SimpleExceptionHandler: Boolean = True;
  {+.}
  {+}
  {English:}
  LText_OK: string = '&OK';
  LText_CopyAll: string = '&Copy All';
  LText_Details: string = '&Details';
  LText_Kill: string = '&Kill App';
  {English.}

  {Russian:}
  {
  LText_OK: string = '&Закрыть';
  LText_CopyAll: string = '&Копировать всё';
  LText_Details: string = '&Детально';
  LText_Kill: string = '&Аварийный выход';
  {Russian.}

  {+.}

  {+}
  procedure InitializeHandler();
  procedure UnInitializeHandler();
  function IsInitialized(): Boolean;
  {+.}

implementation

{$R *.DFM}

uses
  ClipBrd, Math,
{+}
{$IFDEF _JCL_EXCEPTION_COMOBJ_}
  ComObj,
{$ENDIF _JCL_EXCEPTION_COMOBJ_}
{+.}
  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclStrings, JclSysInfo, JclWin32;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionMessage = 'Exception message: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'System   : %s %s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s, %s, %d MHz';
  RsMemory = 'Memory: %d; free %d';
  RsScreenRes = 'Display  : %dx%d pixels, %d bpp';
  RsActiveControl = 'Active Controls hierarchy:';
  {+}
  RsInvalidAddress = 'Invalid %s Address:';
  {+.}
  RsThread = 'Thread: %s';
  RsMissingVersionInfo = '(no version info)';

var
  ExceptionDialog: TExceptionDialog;

//==================================================================================================
// Helper routines
//==================================================================================================

// SortModulesListByAddressCompare
// sorts module by address
function SortModulesListByAddressCompare(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  Addr1, Addr2: Cardinal;
begin
  Addr1 := Cardinal(List.Objects[Index1]);
  Addr2 := Cardinal(List.Objects[Index2]);
  if Addr1 > Addr2 then
    Result := 1
  else if Addr1 < Addr2 then
    Result := -1
  else
    Result := 0;
end;

//==================================================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//==================================================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end else
//    SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

{
 TODO:
   Warning: Symbol 'EStackOverflow' is deprecated
}
{$warnings off}
procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  {todo: handle stack overflow in other thread}
  if (ExceptObject <> nil)
     and JclValidateModuleAddress(ExceptAddr) and (ExceptObject.InstanceSize >= Exception.InstanceSize)
     and (ExceptObject.ClassType <> EStackOverflow) then
  begin
    TExceptionDialog.ExceptionHandler(nil, Exception(ExceptObject));
  end
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;
{$warnings on}

//--------------------------------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;
type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;
var
  TApplicationHandleExceptionAddr, SysUtilsShowExceptionAddr: Pointer;
  CALLInstruction: TCALLInstruction;
  CallAddress: Pointer;
  WrittenBytes: Cardinal;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(Cardinal(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(Integer(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) - Integer(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;
  end;

begin
  TApplicationHandleExceptionAddr := PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr := PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
  if Result then
  begin
    CALLInstruction.Address := Integer(@HookShowException) - Integer(CallAddress) - SizeOf(CALLInstruction);
    Result := WriteProtectedMemory(CallAddress, @CallInstruction, SizeOf(CallInstruction), WrittenBytes);
  end;
end;

//==================================================================================================
// Exception dialog with Send
//==================================================================================================

var
  ExceptionShowing: Boolean;

{ TExceptionDialog }

procedure TExceptionDialog.AfterCreateDetails;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.BeforeCreateDetails;
begin
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.ReportMaxColumns: Integer;
begin
  Result := {+}100{+.};
end;


//----------------------------------------------------------------------------

procedure TExceptionDialog.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CreateDetailInfo;
begin
  {+}
  CreateReport(ExcDialogSystemInfos);
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  try
    CreateDetailInfo;
    {+}
    if iErrorCount = 1 then
      ReportToLog;
    {+.}
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    {+}
    CopyButton.Enabled := True;
    {+.}
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.AddReport();
var
  S: string;
begin
  DoCalculateDetails();
  if iErrorCount = 3 then
  begin
    S := DetailsMemo.Text;
    ClipBoard.AsText := S;
  end;
  S := DelimiterStr + #13#10 + LErrorCount.Caption + ')'#13#10 +
    TextLabel.Lines.Text +
    #13#10 + DelimiterStr + #13#10 +
    DetailsMemo.Lines.Text;
  Inc(iErrorCount);
  LErrorCount.Caption := IntToStr(iErrorCount);
  LErrorCount.Visible := True;
  LErrorCountCaption.Visible := True;
  DetailsMemo.Lines.BeginUpdate;
  TextLabel.Lines.BeginUpdate;
  try
    DetailsMemo.Lines.Clear;
    TextLabel.Lines.Clear;
    CreateReport([siStackList, siActiveControls]);
    TextLabel.Lines.Text := fEMessage;
    fEMessage := '';
    DetailsMemo.Lines.Text := DetailsMemo.Lines.Text + S;
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
  finally
    DetailsMemo.Lines.EndUpdate;
    TextLabel.Lines.EndUpdate;
  end;
end;

procedure TExceptionDialog.DoAddReport();
begin
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_ADDREPORT, 0, 0)
  else
    AddReport();
end;

procedure TExceptionDialog.MakeStackList(bCreateByExceptAddr: Boolean = False);
var
  i, j: Integer;
  vFirsrCallerAdr: Pointer;
  bRawTrace: Boolean;
  ptrAppHandleExBegin: DWORD;
  ptrAppHandleExEnd: DWORD;
  // ---
  function GetNativeAddr(Ptr: Pointer): DWORD;
  type
    PJumpInstruction = ^TJumpInstruction;
    TJumpInstruction = packed record
      case Byte of
        1: (
          Code: Byte; // jump instruction ($FF)
          Offset: DWord; // jump offset
          UnUsed: Byte;
          );
        2: (
          DW1: DWord;
          DW2: DWord;
          );
    end;
    PPointer = ^Pointer;
    PPPointer = ^PPointer;
  begin
    Result := DWORD(Ptr);
    if (PJumpInstruction(Result)^.Code = $FF) and // long jmp to package_address_5b
      (PJumpInstruction(Integer(Result) + 6)^.Code = $8B) and // mov
      (PJumpInstruction(Integer(Result) + 7)^.Code = $C0) {// eax,eax} then
      Result := DWORD(PPPointer(Integer(Result) + 2)^^);
  end;
  // ---
begin
  FreeAndNil(fStackListFirst);
  // ---
  fExceptAddrFirst := ExceptAddr;
  fLastActiveControlFirst := FLastActiveControl;
  if not bCreateByExceptAddr then
  begin
    fStackListFirst := JclGetExceptStackList(FThreadID);
    if fStackListFirst <> nil then
    begin
      JclRemoveExceptStackList(FThreadID);
      fStackListCaption := RsStackList;
      Exit;
    end;
  end;
  // ---
  fStackListCaption := '#Late ' + RsStackList;
  bRawTrace := RawExceptionTrace;
  if bRawTrace then
    fStackListFirst := JclCreateStackList({Raw=}True, {IgnoreLevels=}7, fExceptAddrFirst{nil})
  else
    fStackListFirst := JclCreateStackList({Raw=}False, {IgnoreLevels=}5, fExceptAddrFirst{nil});
  JclRemoveExceptStackList(fStackListFirst.ThreadID);
  CorrectExceptStackListTop(fStackListFirst, {SkipFirstItem:}True);
  //
  // remove stack items created after exception point
  //
  if (fStackListFirst <> nil) and (fStackListFirst.Count > 0) then
  begin
    if bRawTrace then
    begin
      vFirsrCallerAdr := Pointer(DWORD(fStackListFirst.Items[0].CallerAdr) - 5);
      for i := fStackListFirst.Count - 1 downto 1 do
      begin
        if vFirsrCallerAdr = fStackListFirst.Items[i].CallerAdr then
        begin
          fExceptAddrFirst := vFirsrCallerAdr;
          for j := i - 1 downto 0 do
            fStackListFirst.Delete(0);
          Exit;
        end;
      end;
      // ---
      ptrAppHandleExBegin := GetNativeAddr(@TApplication.HandleException);
      ptrAppHandleExEnd := GetNativeAddr(@TApplication.MessageBox);
      for i := 0 to fStackListFirst.Count - 1 do
      begin
        vFirsrCallerAdr := fStackListFirst.Items[i].CallerAdr;
        if ( DWORD(vFirsrCallerAdr) > ptrAppHandleExBegin )
          and ( DWORD(vFirsrCallerAdr) < ptrAppHandleExEnd ) then
        begin
          for j := i downto 0 do
            fStackListFirst.Delete(0);
          Exit;
        end;
      end;
      // ---
      ptrAppHandleExBegin := GetNativeAddr(@SysUtils.ShowException);
      ptrAppHandleExEnd := GetNativeAddr(@SysUtils.Abort);
      for i := 0 to fStackListFirst.Count - 1 do
      begin
        vFirsrCallerAdr := fStackListFirst.Items[i].CallerAdr;
        if ( DWORD(vFirsrCallerAdr) > ptrAppHandleExBegin )
          and ( DWORD(vFirsrCallerAdr) < ptrAppHandleExEnd ) then
        begin
          for j := i downto 0 do
            fStackListFirst.Delete(0);
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TExceptionDialog.CleanStackList();
begin
  if fStackListFirst <> nil then
  begin
    fStackListCaption := '';
    fLastActiveControlFirst := nil;
    fExceptAddrFirst := nil;
    FreeAndNil(fStackListFirst);
  end;
end;
{+.}

procedure TExceptionDialog.CreateReport(const SystemInfo: TExcDialogSystemInfos);
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  ModuleBase: Cardinal;
  ImageBaseStr: string;
  C: TWinControl;
  CpuInfo: TCpuInfo;
  ProcessorDetails: string;
  PETarget: TJclPeTarget;

  {+}
  function IsValidBlockAddr(Addr: Pointer; Size: DWord): boolean;
  begin
    Result := (FindHInstance(Pointer(Addr)) <> 0) and
      (not IsBadReadPtr(Pointer(Addr), Size));
  end;
  {+.}

begin
  SL := nil;
  try
    {+}
    DetailsMemo.Lines.BeginUpdate;
    {+.}
    // Stack list
    if siStackList in SystemInfo then
    begin
      {+}
      if fStackListFirst = nil then
        MakeStackList({CreateByExceptAddr:}True);
      if fStackListFirst <> nil then
      begin
        DetailsMemo.Lines.Add(Format(fStackListCaption, [DateTimeToStr(fStackListFirst.TimeStamp)]));
        //
        //procedure TJclStackInfoList.AddToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
        //  IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False;
        //  IncludeVAdress: Boolean = False);
        //
        fStackListFirst.AddToStrings(DetailsMemo.Lines, {Module}False, {AOffs}True, {POffs}True, {VAddr}True);
        NextDetailBlock;
      end
    end;
    {+}
    // Active controls
    if (siActiveControls in SystemInfo) and (FLastActiveControl <> nil) then
    begin
      DetailsMemo.Lines.Add(RsActiveControl);

      {+}
      if fStackListFirst <> nil then
        C := fLastActiveControlFirst
      else
        C := FLastActiveControl;
      {+.}

      while C <> nil do
      begin
        if IsValidBlockAddr(C, SizeOf(C)) then
        begin
          try
            DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
            C := C.Parent;
          except
            DetailsMemo.Lines.Add(
              Format(RsInvalidAddress, ['parent']) + IntToHex(Integer(C), 0)
            );
            C := nil;
          end;
        end
        else
        begin
          DetailsMemo.Lines.Add(
            Format(RsInvalidAddress, ['parent']) + IntToHex(Integer(C), 0)
          );
          C := nil;
        end;
      end;
      NextDetailBlock;
    end;
    {+.}
    // System and OS information
    if siOsInfo in SystemInfo then
    begin
      DetailsMemo.Lines.Add(Format(RsOSVersion, [GetWindowsVersionString, NtProductTypeString,
        Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
      GetCpuInfo(CpuInfo);
      with CpuInfo do
      begin
        ProcessorDetails := Format(RsProcessor, [Manufacturer, CpuName,
          RoundFrequency(FrequencyInfo.NormFreq)]);
        if not IsFDIVOK then
          ProcessorDetails := ProcessorDetails + ' [FDIV Bug]';
        if ExMMX then
          ProcessorDetails := ProcessorDetails + ' MMXex'
        else if MMX then
          ProcessorDetails := ProcessorDetails + ' MMX';
        if SSE > 0 then
          ProcessorDetails := Format('%s SSE%d', [ProcessorDetails, SSE]);
        if Ex3DNow then
          ProcessorDetails := ProcessorDetails + ' 3DNow!ex'
        else if _3DNow then
          ProcessorDetails := ProcessorDetails + ' 3DNow!';
        if Is64Bits then
          ProcessorDetails := ProcessorDetails + ' 64 bits';
        if DEPCapable then
          ProcessorDetails := ProcessorDetails + ' DEP';
      end;
      DetailsMemo.Lines.Add(ProcessorDetails);
      DetailsMemo.Lines.Add(Format(RsMemory, [GetTotalPhysicalMemory div 1024 div 1024,
      GetFreePhysicalMemory div 1024 div 1024]));
      DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
      NextDetailBlock;
    end;

    // Modules list
    if (siModuleList in SystemInfo) then
    begin
      SL := TStringList.Create;
      if LoadedModulesList(SL, GetCurrentProcessId) then
      begin
        DetailsMemo.Lines.Add(RsModulesList);
        SL.CustomSort(SortModulesListByAddressCompare);
        for I := 0 to SL.Count - 1 do
        begin
          ModuleName := SL[I];
          ModuleBase := Cardinal(SL.Objects[I]);
          DetailsMemo.Lines.Add(Format('[%.8x] %s', [ModuleBase, ModuleName]));
          PETarget := PeMapImgTarget(Pointer(ModuleBase));
          NtHeaders32 := nil;
          NtHeaders64 := nil;
          if PETarget = taWin32 then
            NtHeaders32 := PeMapImgNtHeaders32(Pointer(ModuleBase))
          else
          if PETarget = taWin64 then
            NtHeaders64 := PeMapImgNtHeaders64(Pointer(ModuleBase));
          if (NtHeaders32 <> nil) and (NtHeaders32^.OptionalHeader.ImageBase <> ModuleBase) then
            ImageBaseStr := Format('<%.8x> ', [NtHeaders32^.OptionalHeader.ImageBase])
          else
          if (NtHeaders64 <> nil) and (NtHeaders64^.OptionalHeader.ImageBase <> ModuleBase) then
            ImageBaseStr := Format('<%.8x> ', [NtHeaders64^.OptionalHeader.ImageBase])
          else
            ImageBaseStr := StrRepeat(' ', 11);
          if VersionResourceAvailable(ModuleName) then
            with TJclFileVersionInfo.Create(ModuleName) do
            try
              DetailsMemo.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
              if FileDescription <> '' then
                DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
            finally
              Free;
            end
          else
            DetailsMemo.Lines.Add(ImageBaseStr + RsMissingVersionInfo);
        end;
        NextDetailBlock;
      end;
    end;
  finally
    {+}
    CleanStackList();
    DetailsMemo.Lines.EndUpdate;
    {+.}
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

{+}

//--------------------------------------------------------------------------------------------------
procedure TExceptionDialog.KillBtnClick(Sender: TObject);
begin
  TerminateProcess(OpenProcess(PROCESS_TERMINATE, False, GetCurrentProcessID()), 0);
end;

{+.}

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CopyButtonClick(Sender: TObject);
begin
  {+}
  DoCalculateDetails();
  {+.}
  CopyReportToClipboard;
  MessageBeep(MB_OK);
end;
{+.}

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ExceptionHandler(Sender: TObject; E: Exception);
var
  S: string;
  {$IFDEF _JCL_EXCEPTION_COMOBJ_}
  eOle: EOleException absolute E;
  {$ENDIF _JCL_EXCEPTION_COMOBJ_}
begin
  if (E = nil) or (E.ClassType = nil) or IsIgnoredException(E.ClassType) then
    Exit;
  if ExceptionShowing and (ExceptionDialog <> nil) then
  begin
    //
    // OLD:
    //
    //{ Application.ShowException(E); }

    //
    // NEW:
    //
    if ExceptionDialog.iErrorCount <= cMaxCycledErrors then
    begin
      S := E.Message;
      {$IFDEF _JCL_EXCEPTION_COMOBJ_}
      if E is EOleException then
      begin
        S := S + '. OLE Error details (' +
          'ErrorCode: ' + IntToStr(eOle.ErrorCode) +
          '; Source: "' + eOle.Source + '")';
      end;
      {$ENDIF _JCL_EXCEPTION_COMOBJ_}
      ExceptionDialog.fEMessage := S;
      ExceptionDialog.DoAddReport();
    end
    else
    begin
      if ExceptionDialog.iErrorCount < cMaxErrors then
      begin
        Inc(ExceptionDialog.iErrorCount);
        if ExceptionDialog.iErrorCount < cMaxErrors then
          ExceptionDialog.LErrorCount.Caption := IntToStr(ExceptionDialog.iErrorCount)
        else
        begin
          ExceptionDialog.LErrorCount.Caption := '...';
          ExceptionDialog.LErrorCount.Visible := False;
          ExceptionDialog.KillBtn.Visible := True;
        end;
      end;
      //{ Windows.OutputDebugString( PChar('Exception cycled:' + E.Message) ); }
    end;
  end
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E, nil); //???
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ExceptionThreadHandler(Thread: TJclDebugThread);
begin
  if ExceptionShowing then
  begin
    if Thread.SyncException is Exception then
      Application.ShowException(Exception(Thread.SyncException));
  end
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(Thread.SyncException, Thread);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormCreate(Sender: TObject);
{+}
var
  iW, i, iDelta, iClientWidth: Integer;
{+.}
begin
  FSimpleLog := TJclSimpleLog.Create('filename.log');
  FFullHeight := ClientHeight;
  {+}
  iErrorCount := 1;
  Caption := Format(RsAppError, [Application.Title]);
  DetailsMemo.Lines.Clear;
  TextLabel.Lines.Clear;

  fErrorIcon := LoadIcon(0, IDI_ERROR);

  OkBtn.Caption := LText_OK;
  iW := Canvas.TextWidth(LText_OK);

  CopyButton.Caption := LText_CopyAll;
  i := Canvas.TextWidth(LText_CopyAll);
  if i > iW then
    iW := i;

  DetailsBtn.Caption := LText_Details;
  DetailsVisible := False;
  i := Canvas.TextWidth(DetailsBtn.Caption);
  if i > iW then
    iW := i;

  KillBtn.Caption := LText_Kill;
  DetailsVisible := False;
  i := Canvas.TextWidth(KillBtn.Caption);
  if i > iW then
    iW := i;

  with ClientRect do
  begin
    iClientWidth := (Right - Left) + 1;
    iDelta := (Self.Width - iClientWidth) div 2;
  end;

  Inc(iW, 4 + iDelta + iDelta);

  if iW < 75 then
    iW := 75;

  i := iClientWidth - iW - 3 - iDelta + 1;
  OkBtn.Left := i;
  CopyButton.Left := i;
  DetailsBtn.Left := i;
  KillBtn.Left := i;

  OkBtn.Width := iW;
  CopyButton.Width := iW;
  DetailsBtn.Width := iW;
  KillBtn.Width := iW;

  TextLabel.Width := OkBtn.Left - 4 - TextLabel.Left;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormDestroy(Sender: TObject);
begin
  {+}
  ExceptionShowing := False;
  if Self = ExceptionDialog then
    ExceptionDialog := nil;
  {+.}
  FreeAndNil(FSimpleLog);
  {+}
  CleanStackList();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {+}
  if (Key = VK_ESCAPE) and (Shift = []) then
    Close;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.PaintIcon();
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, fErrorIcon );
end;
{+.}

procedure TExceptionDialog.FormPaint(Sender: TObject);
begin
  {+}
  PaintIcon();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.DoCalculateDetails();
begin
  if FDetailsCalculated then
    Exit;
  FDetailsCalculated := True;
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) <> MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateDetails;
end;
{+.}

procedure TExceptionDialog.FormShow(Sender: TObject);
begin
  {+}
  OnShow := nil;
  {+.}
  {+}
  if FirstExceptionImmediateStackCalculate then
  begin
    DetailsBtn.Enabled := False;
    DoCalculateDetails();
  end
  else
    MakeStackList();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + AnsiCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

{+}
function TExceptionDialog.DelimiterStr: string;
begin
  Result := StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns);
end;
{+.}

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.ReportToLog;
begin
  if Tag and ReportToLogEnabled <> 0 then
  begin
    FSimpleLog.WriteStamp(ReportMaxColumns);
    try
      FSimpleLog.Write(ReportAsText);
    finally
      FSimpleLog.CloseLog;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  {+}
  if (FNonDetailsHeight > 0) and (FDetailsVisible = Value) then
  begin
    if not Value then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end;
    Exit;
  end;
  if Value then
    DoCalculateDetails();
  {+.}
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    Constraints.MinHeight := FNonDetailsHeight + 100;
    Constraints.MaxHeight := Screen.Height;
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
    DetailsMemo.Height := FFullHeight - DetailsMemo.Top - 3;
  end
  else
  begin
    FFullHeight := ClientHeight;
    DetailsCaption := DetailsCaption + ' >>';
    if FNonDetailsHeight = 0 then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
    Constraints.MinHeight := FNonDetailsHeight;
    Constraints.MaxHeight := FNonDetailsHeight
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.DoOnShowException(E: Exception; var Handled: Boolean);
begin
  if Assigned(fOnShowException) then
    fOnShowException(Self, E);
end;
{+.}

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ShowException(E: TObject; Thread: TJclDebugThread);
var
  S: string;
  bHandled, bIsException: Boolean;
  EObj: Exception absolute E;
  {$IFDEF _JCL_EXCEPTION_COMOBJ_}
  eOle: EOleException absolute E;
  {$ENDIF _JCL_EXCEPTION_COMOBJ_}
begin
  if (E = nil) or (E.ClassType = nil) then
    Exit;
  if ExceptionDialog = nil then
    ExceptionDialog := ExceptionDialogClass.Create(Application);
  try
    with ExceptionDialog do
    begin
      if Assigned(Thread) then
        FThreadID := Thread.ThreadID
      else
        FThreadID := MainThreadID;
      FIsMainThead := (GetCurrentThreadId = MainThreadID);
      FLastActiveControl := Screen.ActiveControl;
      {+}
      bIsException := E is Exception;
      if bIsException then
      begin
        S := EObj.Message;
        {$IFDEF _JCL_EXCEPTION_COMOBJ_}
        if E is EOleException then
        begin
          S := S + '. OLE Error details (' +
            'ErrorCode: ' + IntToStr(eOle.ErrorCode) +
            '; Source: "' + eOle.Source + '")';
        end;
        {$ENDIF _JCL_EXCEPTION_COMOBJ_}
      end
      else
        S := E.ClassName;
      TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', S));
      {+.}
      UpdateTextLabelScrollbars;
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      if Thread = nil then
        DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]))
      else
        DetailsMemo.Lines.Add(Format(RsThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      {+}
      if bIsException then
      begin
        bHandled := False;
        DoOnShowException(EObj, bHandled);
        if bHandled then
          Exit;
      end;
      {+.}
      ShowModal;
    end;
  finally
    FreeAndNil(ExceptionDialog);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

{+}
procedure TExceptionDialog.UMAddReport(var Message: TMessage);
begin
  Update();
  AddReport();
end;
{+.}

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UpdateTextLabelScrollbars;
begin
  if Tag and DisableTextScrollbar = 0 then
  begin
    Canvas.Font := TextLabel.Style.Font;
    if TextLabel.Lines.Count * Canvas.TextHeight('Wg') > TextLabel.ClientHeight then
      TextLabel.Properties.ScrollBars := ssVertical
    else
      TextLabel.Properties.ScrollBars := ssNone;
   end;
   {+}
   ShapeTextLabel.Left := TextLabel.Left - 1;
   ShapeTextLabel.Top := OkBtn.Top;
   TextLabel.Height := Bevel1.Top - TextLabel.Top - 1;
   ShapeTextLabel.Height := TextLabel.Top - ShapeTextLabel.Top + TextLabel.Height + 1;
   ShapeTextLabel.Width := TextLabel.Width + 2;

   ShapeDetailsMemo.Left := 1;
   ShapeDetailsMemo.Top := Bevel1.Top - 1;
   ShapeDetailsMemo.Width := ClientWidth - 2;
   ShapeDetailsMemo.Height := DetailsMemo.Top + DetailsMemo.Height - ShapeDetailsMemo.Top + 2;
   {+.}
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.WMSysCommand(var Message: TWMSysCommand);
begin
 case Message.CmdType of
   { not supported }
   //SC_MINIMIZE:
   //  begin
   //  end;
   SC_MAXIMIZE:
     begin
       DoCalculateDetails();
       if DetailsBtn.Enabled then
       begin
         fSaveDetailsVisible := DetailsVisible;
         fSaveHeight := FFullHeight;
         if not fSaveDetailsVisible then
           DetailsVisible := True;
         DetailsBtn.Enabled := False;
       end
       else
       begin
         Exit;
       end;
     end;
   SC_RESTORE:
     begin
       if not fSaveDetailsVisible then
       begin
         DetailsVisible := False;
       end;
       FFullHeight := fSaveHeight;
       DetailsBtn.Enabled := True;
     end;
  end;
 inherited;
end;
{+.}

//==================================================================================================
// Exception handler initialization code
//==================================================================================================

{+}
var
  bIsInitialized: Boolean = False;

function IsInitialized(): Boolean;
begin
  Result := bIsInitialized;
end;
{+.}

procedure InitializeHandler();
begin
  {+}
  if bIsInitialized then
    exit;
  {+.}
  {+}
  ExcDialogSystemInfos := [siStackList, siActiveControls];
  if not SimpleExceptionHandler then
  begin
    ExcDialogSystemInfos := ExcDialogSystemInfos + [siOsInfo, siModuleList];
  end
  else
  begin
    ExcDialogSystemInfos := ExcDialogSystemInfos + [siOsInfo];
    //RawExceptionTrace := False;
  end;
  if RawExceptionTrace then
    JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode];
  //else
  //  JclStackTrackingOptions := JclStackTrackingOptions + [stExceptFrame];
  {+.}
  {$IFNDEF HOOK_DLL_EXCEPTIONS}
  JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  JclStackTrackingOptions := JclStackTrackingOptions + [stDelayedTrace, stAllModules];
  JclDebugThreadList.OnSyncException := TExceptionDialog.ExceptionThreadHandler;
  {+}
  if not SimpleExceptionHandler then
    JclStartExceptionTracking;
  {+.}
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  if HookTApplicationHandleException then
    JclTrackExceptionsFromLibraries;
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  Application.OnException := TExceptionDialog.ExceptionHandler;
  {+}
  bIsInitialized := True;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure UnInitializeHandler();
begin
  {+}
  if not bIsInitialized then
    exit;
  {+.}
  Application.OnException := nil;
  JclDebugThreadList.OnSyncException := nil;
  JclUnhookExceptions;
  JclStopExceptionTracking;
  {+}
  bIsInitialized := False;
  {+.}
end;

//--------------------------------------------------------------------------------------------------


initialization
  InitializeHandler();

finalization
  UnInitializeHandler();

end.
