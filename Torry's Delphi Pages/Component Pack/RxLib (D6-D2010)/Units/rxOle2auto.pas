{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{         OLE2 Automation Controller                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxOle2Auto;

interface

{$I RX.INC}

uses
  Windows, SysUtils, {$IFDEF RX_D3} ActiveX, ComObj {$ELSE}
  Ole2, OleAuto, OleCtl {$ENDIF};

const { Maximum number of dispatch arguments }
{$IFDEF RX_D3}
  MaxDispArgs = 64;
{$ELSE}
  MaxDispArgs = 32;
{$ENDIF}

{$IFNDEF RX_D3}
type
  EPropReadOnly = class(EOleError);
  EPropWriteOnly = class(EOleError);
{$ENDIF}

type

{ OLE2 Automation Controller }

  TOleController = class(TObject)
  private
    FLocale: TLCID;
    FObject: Variant;
    FRetValue: Variant;
    function CallMethod(ID: TDispID; const Params: array of const;
      NeedResult: Boolean): PVariant;
    function CallMethodNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte; NeedResult: Boolean): PVariant;
    function CallMethodNoParams(ID: TDispID; NeedResult: Boolean): PVariant;
    function Invoke(dispidMember: TDispID; wFlags: Word;
      var pdispparams: TDispParams; Res: PVariant): PVariant;
    function NameToDispID(const AName: string): TDispID;
    function NameToDispIDs(const AName: string;
      const AParams: array of string; Dest: PDispIDList): PDispIDList;
  protected
    procedure ClearObject; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { create or assign OLE objects }
    procedure CreateObject(const ClassName: string); virtual;
    procedure AssignIDispatch(V: Variant); virtual;
    procedure GetActiveObject(const ClassName: string); virtual;
    { get/set properties of OLE object by ID }
    function GetPropertyByID(ID: TDispID): PVariant;
    procedure SetPropertyByID(ID: TDispID; const Prop: array of const);
    { get/set properties of OLE object }
    function GetProperty(const AName: string): PVariant;
    procedure SetProperty(const AName: string; const Prop: array of const);
    { call OLE functions by IDs }
    function CallFunctionByID(ID: TDispID; const Params: array of const): PVariant;
    function CallFunctionByIDsNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte): PVariant;
    function CallFunctionNoParamsByID(ID: TDispID): PVariant;
    { call OLE procedures by ID }
    procedure CallProcedureByID(ID: TDispID; const Params: array of const);
    procedure CallProcedureByIDsNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte);
    procedure CallProcedureNoParamsByID(ID: TDispID);
    { call OLE functions }
    function CallFunction(const AName: string; const Params: array of const): PVariant;
    function CallFunctionNamedParams(const AName: string; const Params: array of const;
      const ParamNames: array of string): PVariant;
    function CallFunctionNoParams(const AName: string): PVariant;
    { call OLE procedures }
    procedure CallProcedure(const AName: string; const Params: array of const);
    procedure CallProcedureNamedParams(const AName: string; const Params: array of const;
      const ParamNames: array of string);
    procedure CallProcedureNoParams(const AName: string);
    { locale }
    procedure SetLocale(PrimaryLangID, SubLangID: Word);
    property Locale: TLCID read FLocale write FLocale;
    property OleObject: Variant read FObject;
  end;

procedure InitOLE;
procedure DoneOLE;
function OleInitialized: Boolean;

function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
function MakeLCID(LangID: Word): TLCID;
function CreateLCID(PrimaryLangID, SubLangID: Word): TLCID;
function ExtractLangID(LCID: TLCID): Word;
function ExtractSubLangID(LCID: TLCID): Word;

implementation

uses
  Forms;

{$IFDEF RX_D3}
resourcestring
{$ELSE}
const
{$ENDIF}
  SOleInvalidVer   = 'Invalid OLE library version';
  SOleInitFailed   = 'OLE Library initialization failed. Error code: %.8xH';
  SOleNotInit      = 'OLE2 Library not initialized';
  SOleInvalidParam = 'Invalid parameter value';
  SOleNotSupport   = 'Method or property %s not supported by OLE object';
  SOleNotReference = 'Variant does not reference an OLE automation object';
{$IFNDEF RX_D3}
  SOleError        = 'OLE2 error occured. Error code: %.8xH';
{$ENDIF}

const
  FOleInitialized: Boolean = False;

const
{ OLE2 Version }
  RMJ =   0;
  RMM =  23;
  RUP = 639;

const
  DISPATCH_METHODNOPARAM = DISPATCH_METHOD or DISPATCH_PROPERTYGET;
  DISPATCH_METHODPARAMS = DISPATCH_METHOD
    or DISPATCH_PROPERTYGET;

function FailedHR(hr: HResult): Boolean;
begin
  Result := Failed(hr);
end;

{ Standard OLE Library initialization code }

procedure InitOLE;
var
  dwVer: Longint;
  HRes: HResult;
begin
  if FOleInitialized then
    Exit;
  dwVer := Longint(CoBuildVersion);
  if (RMM <> HiWord(dwVer)) or (RUP > LoWord(dwVer)) then
    raise EOleError.Create(SOleInvalidVer)
  else
  begin
    HRes := OleInitialize(nil);
    if FailedHR(HRes) then
      raise EOleError.CreateFmt(SOleInitFailed, [Longint(HRes)])
    else
      FOleInitialized := True;
  end;
end;

{ Standard OLE Library exit code }

procedure DoneOLE;
begin
  if FOleInitialized then
    OleUninitialize;
  FOleInitialized := False;
end;

function OleInitialized: Boolean;
begin
  Result := FOleInitialized;
end;

procedure CheckOleInitialized;
begin
  if not FOleInitialized then
    raise EOleError.Create(SOleNotInit);
end;

{$IFNDEF RX_D3}
function OleErrorMsg(ErrorCode: HResult): string;
begin
  FmtStr(Result, SOleError, [Longint(ErrorCode)]);
end;
{$ENDIF}

{ Raise exception given an OLE return code and TExcepInfo structure }

procedure DispInvokeError(Status: HResult; const ExcepInfo: TExcepInfo);
{$IFDEF RX_D3}
begin
  DispatchInvokeError(Status, ExcepInfo);
{$ELSE}
var
  EClass: ExceptClass;
  Message: string;
begin
  EClass := EOleError;
  if Longint(Status) <> DISP_E_EXCEPTION then
    Message := OleErrorMsg(Status)
  else
    with ExcepInfo do
    begin
      try
        if (scode = CTL_E_SETNOTSUPPORTED) or
          (scode = CTL_E_SETNOTSUPPORTEDATRUNTIME) then
            EClass := EPropReadOnly
        else
        if (scode = CTL_E_GETNOTSUPPORTED) or
          (scode = CTL_E_GETNOTSUPPORTEDATRUNTIME) then
            EClass := EPropWriteOnly;
        if bstrDescription <> nil then
        begin
          Message := OleStrToString(bstrDescription);
          while (Length(Message) > 0) and
            (Message[Length(Message)] in [#0..#32, '.']) do
            Delete(Message, Length(Message), 1);
        end;
      finally
        if bstrSource <> nil then
          SysFreeString(bstrSource);
        if bstrDescription <> nil then
          SysFreeString(bstrDescription);
        if bstrHelpFile <> nil then
          SysFreeString(bstrHelpFile);
      end;
    end;
  if Message = '' then
    Message := OleErrorMsg(Status);
  raise EClass.Create(Message);
{$ENDIF RX_D3}
end;

 {$IFDEF RX_D3}

{ Return OLE object stored in a variant }

function VarToInterface(const V: Variant): IDispatch;
begin
  Result := nil;
  if TVarData(V).VType = varDispatch then
    Result := IDispatch(TVarData(V).VDispatch)
  else
    if TVarData(V).VType = (varDispatch or varByRef) then
      Result := IDispatch(Pointer(TVarData(V).VPointer^));
  if Result = nil then
    raise EOleError.Create(SOleNotReference);
end;

 {$ENDIF}

{ Assign Variant }

procedure AssignVariant( var Dest: TVariantArg; const Value: TVarRec);
begin
    with Value do
      case VType of
        vtInteger:
          begin
            Dest.vt := VT_I4;
            Dest.lVal := VInteger;
          end;
        vtBoolean:
          begin
            Dest.vt := VT_BOOL;
            Dest.vbool := VBoolean;
          end;
        vtChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VChar);
          end;
        vtExtended:
          begin
            Dest.vt := VT_R8;
            Dest.dblVal := VExtended^;
          end;
        vtString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VString^);
          end;
        vtPointer:
          if VPointer = nil then begin
            Dest.vt := VT_NULL;
            Dest.byRef := nil;
          end
          else begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VPointer;
          end;
        vtPChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(StrPas(VPChar));
          end;
        vtObject:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VObject;
          end;
        vtClass:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VClass;
          end;
        vtWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := @VWideChar;
          end;
        vtPWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := VPWideChar;
          end;
        vtAnsiString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(string(VAnsiString));
          end;
        vtCurrency:
          begin
            Dest.vt := VT_CY;
            Dest.cyVal := VCurrency^;
          end;
        vtVariant:
          begin
            Dest.vt := VT_BYREF or VT_VARIANT;
            Dest.pvarVal := VVariant;
          end;
{$IFDEF RX_D4}
        vtInterface:
          begin
            Dest.vt := VT_UNKNOWN or VT_BYREF;
            Dest.byRef := VInterface;
          end;
        vtInt64:
          begin
            Dest.vt := VT_I8 or VT_BYREF;
            Dest.byRef := VInt64;
          end;
{$ENDIF RX_D4}
        else raise EOleError.Create(SOleInvalidParam);
      end;
end;

{ TOleController }

constructor TOleController.Create;
begin
  inherited Create;
  FLocale := GetThreadLocale;
  try
    InitOLE;
  except
    Application.HandleException(Self);
  end;
end;

destructor TOleController.Destroy;
begin
  if FOleInitialized then
    ClearObject;
  inherited Destroy;
end;

procedure TOleController.CreateObject(const ClassName: string);
begin
  CheckOleInitialized;
  ClearObject;
  FObject := CreateOleObject(ClassName);
end;

procedure TOleController.GetActiveObject(const ClassName: string);
begin
  CheckOleInitialized;
  ClearObject;
  FObject := GetActiveOleObject(ClassName);
end;

procedure TOleController.AssignIDispatch(V: Variant);
begin
  CheckOleInitialized;
  ClearObject;
  VarToInterface(V);
  VarCopy(FObject, V);
end;

procedure TOleController.ClearObject;
begin
  VarClear(FRetValue);
  VarClear(FObject);
end;

function TOleController.NameToDispID(const AName: string): TDispID;
var
  CharBuf: array[0..255] of WideChar;
  P: array[0..0] of PWideChar;
begin
  CheckOleInitialized;
  StringToWideChar(AName, @CharBuf, 256);
  P[0] := @CharBuf[0];
  if FailedHR(VarToInterface(FObject).GetIDsOfNames(GUID_NULL,
    @P, 1, FLocale, @Result)) then
    raise EOleError.CreateFmt(SOleNotSupport, [AName]);
end;

function TOleController.NameToDispIDs(const AName: string;
  const AParams: array of string; Dest: PDispIDList): PDispIDList;
var
  CharBuf: array[0..MaxDispArgs] of PWideChar;
  Size: Integer;
  I: Byte;
begin
  Result := Dest;
  CheckOleInitialized;
  Size := Length(AName) + 1;
  GetMem(CharBuf[0], Size * SizeOf(WideChar));
  StringToWideChar(AName, CharBuf[0], Size);
  for I := 0 to High(AParams) do
  begin
    Size := Length(AParams[I]) + 1;
    GetMem(CharBuf[I + 1], Size * SizeOf(WideChar));
    StringToWideChar(AParams[I], CharBuf[I + 1], Size);
  end;
  try
    if FailedHR(VarToInterface(FObject).GetIDsOfNames(GUID_NULL, @CharBuf,
      High(AParams) + 2, FLocale, @Result^[0])) then
      raise EOleError.CreateFmt(SOleNotSupport, [AName]);
  finally
    for I := 0 to High(AParams) + 1 do
      FreeMem(CharBuf[I]);
  end;
end;

function TOleController.Invoke(dispidMember: TDispID; wFlags: Word;
  var pdispparams: TDispParams; Res: PVariant): PVariant;
var
  pexcepinfo: TExcepInfo;
  puArgErr: Integer;
  HRes: HResult;
begin
  if Res <> nil then
    VarClear(Res^);
  try
    HRes := VarToInterface(FObject).Invoke(dispidMember, GUID_NULL,
      FLocale, wFlags, pdispparams, Res, @pexcepinfo, @puArgErr);
  except
    if Res <> nil then
      VarClear(Res^);
    raise;
  end;
  if FailedHR(HRes) then
    DispInvokeError(HRes, pexcepinfo);
  Result := Res;
end;

function TOleController.CallMethodNoParams(ID: TDispID;
  NeedResult: Boolean): PVariant;
const
  Disp: TDispParams = (rgvarg: nil; rgdispidNamedArgs: nil; cArgs: 0; cNamedArgs: 0);
begin
  CheckOleInitialized;
  if NeedResult then
    Result := Invoke(ID, DISPATCH_METHODNOPARAM, Disp, @FRetValue)
  else
    Result := Invoke(ID, DISPATCH_METHODNOPARAM, Disp, nil);
end;

function TOleController.CallMethod(ID: TDispID; const Params: array of const;
  NeedResult: Boolean): PVariant;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
begin
  CheckOleInitialized;
  ArgCnt := 0;
  for I := 0 to High(Params) do
  begin
    AssignVariant(Args[I], Params[I]);
    Inc(ArgCnt);
    if ArgCnt >= MaxDispArgs then
      Break;
  end;
  with Disp do
  begin
    if ArgCnt = 0 then
      rgvarg := nil
    else
      rgvarg := @Args;
    rgdispidNamedArgs := nil;
    cArgs := ArgCnt;
    cNamedArgs := 0;
  end;
  if NeedResult then
    Result := Invoke(ID, DISPATCH_METHODPARAMS, Disp, @FRetValue)
  else
    Result := Invoke(ID, DISPATCH_METHODPARAMS, Disp, nil);
end;

function TOleController.CallMethodNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte; NeedResult: Boolean): PVariant;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
begin
  CheckOleInitialized;
  ArgCnt := 0;
  for I := 0 to High(Params) do
  begin
    AssignVariant(Args[I], Params[I]);
    Inc(ArgCnt);
    if ArgCnt >= MaxDispArgs then
      Break;
  end;
  with Disp do
  begin
    if ArgCnt = 0 then
      rgvarg := nil
    else
      rgvarg := @Args;
    if Cnt = 0 then
      rgdispidNamedArgs := nil
    else
      rgdispidNamedArgs := @IDs[1];
    cArgs := ArgCnt;
    cNamedArgs := Cnt;
  end;
  if NeedResult then
    Result := Invoke(IDs[0], DISPATCH_METHODPARAMS, Disp, @FRetValue)
  else
    Result := Invoke(IDs[0], DISPATCH_METHODPARAMS, Disp, nil);
end;

procedure TOleController.SetPropertyByID(ID: TDispID; const Prop: array of const);
const
  NameArg: TDispID = DISPID_PROPERTYPUT;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
begin
  CheckOleInitialized;
  ArgCnt := 0;
  for I := 0 to High(Prop) do
  begin
    AssignVariant(Args[I], Prop[I]);
    Inc(ArgCnt);
    if ArgCnt >= MaxDispArgs then
      Break;
  end;
  with Disp do
  begin
    rgvarg := @Args;
    rgdispidNamedArgs := @NameArg;
    cArgs := ArgCnt;
    cNamedArgs := 1;
  end;
  Invoke(ID, DISPATCH_PROPERTYPUT, Disp, nil);
end;

function TOleController.GetPropertyByID(ID: TDispID): PVariant;
const
  Disp: TDispParams = (rgvarg: nil; rgdispidNamedArgs: nil;
    cArgs: 0; cNamedArgs: 0);
begin
  CheckOleInitialized;
  Result := Invoke(ID, DISPATCH_PROPERTYGET, Disp, @FRetValue);
end;

procedure TOleController.CallProcedureByID(ID: TDispID; const Params: array of const);
begin
  CallMethod(ID, Params, False);
end;

function TOleController.CallFunctionByID(ID: TDispID;
  const Params: array of const): PVariant;
begin
  Result := CallMethod(ID, Params, True);
end;

procedure TOleController.CallProcedureByIDsNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte);
begin
  CallMethodNamedParams(IDs, Params, Cnt, False);
end;

function TOleController.CallFunctionByIDsNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte): PVariant;
begin
  Result := CallMethodNamedParams(IDs, Params, Cnt, True);
end;

procedure TOleController.CallProcedureNoParamsByID(ID: TDispID);
begin
  CallMethodNoParams(ID, False);
end;

function TOleController.CallFunctionNoParamsByID(ID: TDispID): PVariant;
begin
  Result := CallMethodNoParams(ID, True);
end;

procedure TOleController.SetProperty(const AName: string;
  const Prop: array of const);
begin
  SetPropertyByID(NameToDispID(AName), Prop);
end;

function TOleController.GetProperty(const AName: string): PVariant;
begin
  Result := GetPropertyByID(NameToDispID(AName));
end;

procedure TOleController.CallProcedure(const AName: string;
  const Params: array of const);
begin
  CallProcedureByID(NameToDispID(AName), Params);
end;

function TOleController.CallFunction(const AName: string;
  const Params: array of const): PVariant;
begin
  Result := CallFunctionByID(NameToDispID(AName), Params);
end;

procedure TOleController.CallProcedureNamedParams(const AName: string;
  const Params: array of const; const ParamNames: array of string);
var
  DispIDs: array[0..MaxDispArgs] of TDispID;
begin
  CallProcedureByIDsNamedParams(NameToDispIDs(AName, ParamNames, @DispIDs)^,
    Params, High(ParamNames) + 1);
end;

function TOleController.CallFunctionNamedParams(const AName: string;
  const Params: array of const; const ParamNames: array of string): PVariant;
var
  DispIDs: array[0..MaxDispArgs] of TDispID;
begin
  Result := CallFunctionByIDsNamedParams(NameToDispIDs(AName, ParamNames,
    @DispIDs)^, Params, High(ParamNames) + 1);
end;

procedure TOleController.CallProcedureNoParams(const AName: string);
begin
  CallProcedureNoParamsByID(NameToDispID(AName));
end;

function TOleController.CallFunctionNoParams(const AName: string): PVariant;
begin
  Result := CallFunctionNoParamsByID(NameToDispID(AName));
end;

procedure TOleController.SetLocale(PrimaryLangID, SubLangID: Word);
begin
  FLocale := CreateLCID(PrimaryLangID, SubLangID);
end;

{ Utility routines }

function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
begin
  Result := (SubLangID shl 10) or PrimaryLangID;
end;

function MakeLCID(LangID: Word): TLCID;
begin
  Result := TLCID(LangID or (Longint(0) shl 16));
end;

function CreateLCID(PrimaryLangID, SubLangID: Word): TLCID;
begin
  Result := MakeLCID(MakeLangID(PrimaryLangID, SubLangID));
end;

function ExtractLangID(LCID: TLCID): Word;
begin
  Result := LCID and $FF;
end;

function ExtractSubLangID(LCID: TLCID): Word;
begin
  Result := LCID and ($FF shl 10) shr 10;
end;

initialization
finalization
  DoneOLE;
end.
