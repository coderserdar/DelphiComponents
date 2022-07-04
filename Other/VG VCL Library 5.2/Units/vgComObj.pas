{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         COM support                                   }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgComObj;

interface

uses Windows, StdVCL, ActiveX, VCLCom, Classes, vgSystem, ComObj, TypInfo;

{ TThreadedClassFactory, TApartmentThread are copied from Borland examples. }

type
{ TThreadedAutoObjectFactory }
  TThreadedAutoObjectFactory = class(TAutoObjectFactory, IClassFactory)
  protected
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
    function DoCreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
  end;

{ TThreadedClassFactory }
{$IFDEF _D5_}
  TThreadedClassFactory = class(TComponentFactory);
{$ELSE}
  TThreadedClassFactory = class(TComponentFactory, IClassFactory)
  protected
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
    function DoCreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;
{$ENDIF}

{ TApartmentThread }
  TCreateInstanceProc = function(const UnkOuter: IUnknown;
    const IID: TGUID; out Obj): HResult of object; stdcall;

  TApartmentThread = class(TThread)
  private
    FCreateInstanceProc: TCreateInstanceProc;
    FUnkOuter: IUnknown;
    FIID: TGuid;
    FSemaphore: THandle;
    FStream: Pointer;
    FCreateResult: HResult;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateInstanceProc: TCreateInstanceProc; UnkOuter: IUnknown; IID: TGuid);
    destructor Destroy; override;
    property Semaphore: THandle read FSemaphore;
    property CreateResult: HResult read FCreateResult;
    property ObjStream: Pointer read FStream;
  end;

{ TDispatchStrings }
  TDispatchStrings = class(TInterfacedObject, IStrings, ISupportErrorInfo)
  private
    FStrings: IStringsDisp;
  public
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;  stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { IStrings }
    function Get_ControlDefault(Index: Integer): OleVariant; safecall;
    procedure Set_ControlDefault(Index: Integer; Value: OleVariant); safecall;
    function Count: Integer; safecall;
    function Get_Item(Index: Integer): OleVariant; safecall;
    procedure Set_Item(Index: Integer; Value: OleVariant); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    function Add(Item: OleVariant): Integer; safecall;
    function _NewEnum: IUnknown; safecall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
  public
    constructor Create(const Strings: IStringsDisp);
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
  end;

{ TComPoolManager }
  TComPoolManager = class(TIntfPoolManager)
  private
    FClassID: TGUID;
  protected
    procedure CheckLocked(Instance: TCustomPoolInstance; var InUse: Boolean); override;
    function CreateUnknown(Instance: TCustomPoolInstance): IUnknown; override;
  public
    constructor Create(const AClassID: TGUID; AMaxCount: Integer; ATimeout: DWord);
    property ClassID: TGUID read FClassID;
  end;

{ TOlePoolManager }
  TOlePoolManager = class(TComPoolManager)
  protected
    function CreateUnknown(Instance: TCustomPoolInstance): IUnknown; override;
  end;

  EOleExceptionEx = class(EOleException);

function GetProperty(Obj: IDispatch; const Name: string): OleVariant;
{ Retrievies value of Name property from dispinterface }

procedure StringsFromIStrings(Strings: TStrings; StringsIntf: IStrings);

procedure StringsToIStrings(Strings: TStrings; StringsIntf: IStrings);

function HandleSafeCallExceptionEx(ExceptObject: TObject;
  ExceptAddr: Pointer; const ErrorIID: TGUID; const ProgID,
  HelpFileName: WideString; ARaiseExtended: Boolean): HResult;

implementation
uses Messages, SysUtils, vgUtils;

threadvar
  RaiseExtended: Boolean;

function HandleSafeCallExceptionEx(ExceptObject: TObject;
  ExceptAddr: Pointer; const ErrorIID: TGUID; const ProgID,
  HelpFileName: WideString; ARaiseExtended: Boolean): HResult;
begin
  RaiseExtended := ARaiseExtended;
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, ErrorIID, ProgID, HelpFileName);
end;

procedure SafeCallError(ErrorCode: Integer; ErrorAddr: Pointer);
var
  ErrorInfo: IErrorInfo;
  Source, Description, HelpFile: WideString;
  HelpContext: Longint;
begin
  HelpContext := 0;
  if GetErrorInfo(0, ErrorInfo) = S_OK then
  begin
    ErrorInfo.GetSource(Source);
    ErrorInfo.GetDescription(Description);
    ErrorInfo.GetHelpFile(HelpFile);
    ErrorInfo.GetHelpContext(HelpContext);
  end;
  if RaiseExtended then
  begin
    RaiseExtended := False;
    raise EOleExceptionEx.Create(Description, ErrorCode, Source,
      HelpFile, HelpContext) at ErrorAddr;
  end else
    raise EOleException.Create(Description, ErrorCode, Source,
      HelpFile, HelpContext) at ErrorAddr;
end;

function GetProperty(Obj: IDispatch; const Name: string): OleVariant;
var
  ID: Integer;
  WideName: WideString;
  DispParams: TDispParams;
  ExcepInfo: TExcepInfo;
  Status: Integer;
begin
  WideName := Name;
  OleCheck(Obj.GetIDsOfNames(GUID_NULL, @WideName, 1, 0, @ID));
  ZeroMem(@DispParams, SizeOf(DispParams));
  ZeroMem(@ExcepInfo, SizeOf(ExcepInfo));
  Status := Obj.Invoke(ID, GUID_NULL, 0, DISPATCH_PROPERTYGET, DispParams,
    @Result, @ExcepInfo, nil);
  if Status <> 0 then DispatchInvokeError(Status, ExcepInfo);
end;

procedure StringsFromIStrings(Strings: TStrings; StringsIntf: IStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for I := 0 to StringsIntf.Count - 1 do
      Strings.Add(StringsIntf.Item[I]);
  finally
    Strings.EndUpdate;
  end;
end;

procedure StringsToIStrings(Strings: TStrings; StringsIntf: IStrings);
var
  I: Integer;
begin
  StringsIntf.Clear;
  for I := 0 to Strings.Count - 1 do
    StringsIntf.Add(Strings[I]);
end;

{ TThreadedAutoObjectFactory }
function TThreadedAutoObjectFactory.DoCreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := inherited CreateInstance(UnkOuter, IID, Obj);
end;

function TThreadedAutoObjectFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  with TApartmentThread.Create(DoCreateInstance, UnkOuter, IID) do
  begin
    if WaitForSingleObject(Semaphore, INFINITE) = WAIT_OBJECT_0 then
    begin
      Result := CreateResult;
      if Result <> S_OK then Exit;
      Result := CoGetInterfaceAndReleaseStream(IStream(ObjStream), IID, Obj);
    end else
      Result := E_FAIL
  end;
end;

{$IFNDEF _D5_}
{ TThreadedClassFactory. }
function TThreadedClassFactory.DoCreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := inherited CreateInstance(UnkOuter, IID, Obj);
end;

function TThreadedClassFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  with TApartmentThread.Create(DoCreateInstance, UnkOuter, IID) do
  begin
    if WaitForSingleObject(Semaphore, INFINITE) = WAIT_OBJECT_0 then
    begin
      Result := CreateResult;
      if Result <> S_OK then Exit;
      Result := CoGetInterfaceAndReleaseStream(IStream(ObjStream), IID, Obj);
    end else
      Result := E_FAIL
  end;
end;

procedure TThreadedClassFactory.UpdateRegistry(Register: Boolean);
var
  ClsID: string;
begin
  ClsID := GUIDToString(ClassID);
  inherited UpdateRegistry(Register);
  if Register then
    CreateRegKey('CLSID\' + ClsID, 'ThreadingModel', 'Apartment') else
    DeleteRegKey('CLSID\' + ClsID);
end;
{$ENDIF}

{ TApartmentThread }
constructor TApartmentThread.Create(CreateInstanceProc: TCreateInstanceProc;
  UnkOuter: IUnknown; IID: TGuid);
begin
  FCreateInstanceProc := CreateInstanceProc;
  FUnkOuter := UnkOuter;
  FIID := IID;
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TApartmentThread.Destroy;
var
  Msg: TMsg;
begin
  FUnkOuter := nil;
  CloseHandle(FSemaphore);
  inherited Destroy;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
end;

procedure TApartmentThread.Execute;
var
  msg: TMsg;
  Unk: IUnknown;
begin
  CoInitialize(nil);
  FCreateResult := FCreateInstanceProc(FUnkOuter, FIID, Unk);
  if FCreateResult = S_OK then
    CoMarshalInterThreadInterfaceInStream(FIID, Unk, IStream(FStream));
  ReleaseSemaphore(FSemaphore, 1, nil);
  if FCreateResult = S_OK then
    while GetMessage(msg, 0, 0, 0) do
    begin
      DispatchMessage(msg);
      Unk._AddRef;
      if Unk._Release = 1 then break;
    end;
  Unk := nil;
  CoUninitialize;
end;

{ TDispatchStrings }
constructor TDispatchStrings.Create(const Strings: IStringsDisp);
begin
  FStrings := Strings;
end;

{ IDispatch }
function TDispatchStrings.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := IDispatch(FStrings).GetTypeInfoCount(Count);
end;

function TDispatchStrings.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := IDispatch(FStrings).GetTypeInfo(Index, LocaleID, TypeInfo);
end;

function TDispatchStrings.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := IDispatch(FStrings).GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs);
end;

function TDispatchStrings.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
begin
  Result := IDispatch(FStrings).Invoke(DispID, IID, LocaleID, Flags, Params, VarResult,
    ExcepInfo, ArgErr)
end;

{ IStrings }
function TDispatchStrings.Get_ControlDefault(Index: Integer): OleVariant;
begin
  Result := FStrings.ControlDefault[Index];
end;

procedure TDispatchStrings.Set_ControlDefault(Index: Integer; Value: OleVariant);
begin
  FStrings.ControlDefault[Index] := Value;
end;

function TDispatchStrings.Count: Integer;
begin
  Result := FStrings.Count;
end;

function TDispatchStrings.Get_Item(Index: Integer): OleVariant;
begin
  Result := FStrings.Item[Index];
end;

procedure TDispatchStrings.Set_Item(Index: Integer; Value: OleVariant);
begin
  FStrings.Item[Index] := Value;
end;

procedure TDispatchStrings.Remove(Index: Integer);
begin
  FStrings.Remove(Index);
end;

procedure TDispatchStrings.Clear;
begin
  FStrings.Clear;
end;

function TDispatchStrings.Add(Item: OleVariant): Integer;
begin
  FStrings.Add(Item);
end;

function TDispatchStrings._NewEnum: IUnknown;
begin
  Result := FStrings._NewEnum;
end;

{ ISupportErrorInfo}
function TDispatchStrings.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  if IsEqualGUID(IStrings, iid) then Result := S_OK else Result := S_FALSE;
end;

function TDispatchStrings.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IStrings, '', '');
end;

{ TComPoolManager }
constructor TComPoolManager.Create(const AClassID: TGUID;
  AMaxCount: Integer; ATimeout: DWord);
begin
  inherited Create(AMaxCount, ATimeout);
  FClassID := AClassID;
end;

procedure TComPoolManager.CheckLocked(Instance: TCustomPoolInstance;
  var InUse: Boolean);
begin
  if TIntfPoolInstance(Instance).AsUnknown = nil then
  begin
    TIntfPoolInstance(Instance).AsUnknown := CreateUnknown(Instance);
    InUse := False;
  end;
end;

function TComPoolManager.CreateUnknown(Instance: TCustomPoolInstance): IUnknown;
begin
  Result := CreateComObject(ClassID);
end;

{ TOlePoolManager }
function TOlePoolManager.CreateUnknown(Instance: TCustomPoolInstance): IUnknown;
begin
  Result := CreateOleObject(ClassIDToProgID(ClassID));
end;

initialization
  SafeCallErrorProc := @SafeCallError;

finalization
  SafeCallErrorProc := nil;

end.

