
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit OleTools;

interface

{$I STD.INC}

uses
  ActiveX, Classes, SysUtils, Windows, Registry, ComObj, Contnrs
  {$IFDEF D6_UP}, Variants{$ENDIF};

{ Error info support for stdcall COM methods }

procedure SafeCheck(const Obj; IID: TGUID; Result: HResult);

{ Guid routines }

function GuidToStr(const GUID: TGUID): string;
function StrToGuid(const S: string): TGUID;

{ GetClassFile }

function GetClassFile(const CLSID: TGUID): string;

{ GetInterfaceStrings }

procedure GetInterfaceStrings(Unknown: IUnknown; Strings: TStrings);

{ EnumerateVariant  }

type
  TVariantCallback = procedure(const V: OleVariant; Data: Pointer);

procedure EnumerateVariant(const V: OleVariant; Callback: TVariantCallback;
  Data: Pointer);

{ TEnumString class }

type
  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings: TStrings;
    FIndex: Integer;
    procedure SetStrings(Value: TStrings);
  protected
    { IEnumString }
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
  public
    property Strings: TStrings read FStrings write SetStrings;
  end;

{ The TEnumVariant class can be used as part of a collection automation object.
  All collections should support an enumerator interface exposed through a
  propget method named _NewEnum with a dispatch id of -4. }

{ TEnumVariant }

  TEnumVariant = class(TInterfacedObject, IEnumVariant)
  private
    FObjectList: TObjectList;
    FIndex: Integer;
  protected
    { IEnumVariant }
    function Next(celt: LongWord; var rgvar: OleVariant;
      out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    constructor Create(List: TObjectList);
  end;

{ The TAutoStorable class represents an object capable of data storage and
  retrieval through an OleVariant data property }

type
  TAutoIntfObjectClass = class of TAutoIntfObject;

  TAutoStorable = class(TAutoIntfObject)
  protected
    procedure InternalExecute(const Command: OleVariant); virtual;
    function InternalRetrieve(const Format: OleVariant): OleVariant; virtual;
    procedure InternalUpdate(const Data: OleVariant); virtual;
    { IStorable }
    procedure Execute(Command: OleVariant); safecall;
    function  Retrieve(Format: OleVariant): OleVariant; safecall;
    procedure Update(Data: OleVariant); safecall;
  end;

{ The TAutoCollection class can be used as a container for automation objects.
  The item class type and its' interface identifier are passed in the
  constructor. The first parameter of the constructor is the interface
  identifier of the collection itself. In order to support VB's "For Each"
  syntax and the default Item property, the typelibary must set the _NewEnum
  property dispid to -4 and the Item property dipsid to 0. All other
  properties are optional. }

  TAutoCollection = class(TAutoStorable)
  private
    FObjectList: TObjectList;
    FItemClass: TAutoIntfObjectClass;
    FItem: TGUID;
  protected
    property ObjectList: TObjectList read FObjectList;
    function InternalAdd(const Item: OleVariant): OleVariant; virtual;
    procedure InternalDelete(const Item: OleVariant); virtual;
    function InternalGetCount: Integer; virtual;
    function InternalGetItem(const Index: OleVariant): OleVariant; virtual;
    function InternalSearch(const Params: OleVariant): OleVariant; virtual;
    { ICollection }
    function Get__NewEnum: IUnknown; safecall;
    function Add(Item: OleVariant): OleVariant; safecall;
    procedure Delete(Item: OleVariant); safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    { ISearchCollection }
    function Search(Params: OleVariant): OleVariant; safecall;
  public
    constructor Create(Collection: TGUID; ItemClass: TAutoIntfObjectClass;
      Item: TGUID); virtual;
    destructor Destroy; override;
  end;

{ Scripting event object }

  TScriptEvents = class(TObject)
  private
    FEvents: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AttachEvent(const Name: string; Handler: IDispatch);
    procedure DetachEvent(const Name: string; Handler: IDispatch);
    procedure InvokeEvent(const Name: string);
  end;

{ EventSink helper routines }

function ConnectEvent(Instance: IUnknown; EventID: TGUID; Event: IUnknown): Integer;
procedure DisconnectEvent(Instance: IUnknown; EventID: TGUID; Cookie: Integer);

implementation

uses
  StrConst;

procedure SafeCheck(const Obj; IID: TGUID; Result: HResult);
var
  Unk: IUnknown absolute Obj;
  SupportErrorInfo: ISupportErrorInfo;
  ErrorInfo: IErrorInfo;
  Buffer: WideString;
  Description, Source, HelpFile: string;
  HelpContext: Integer;
begin
  if Result <> S_OK then
    if (Unk.QueryInterface(ISupportErrorInfo, SupportErrorInfo) = S_OK) and
      (SupportErrorInfo.InterfaceSupportsErrorInfo(IID) = S_OK) and
      (GetErrorInfo(0, ErrorInfo) = S_OK) then
    begin
      ErrorInfo.GetDescription(Buffer);
      Description := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetSource(Buffer);
      Source := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetHelpFile(Buffer);
      HelpFile := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetHelpContext(HelpContext);
      raise EOleException.Create(Description, Result, Source,
        HelpFile, HelpContext);
    end
    else
      OleError(Result);
end;

function GuidToStr(const GUID: TGUID): string;
var
  OleStr: PWideChar;
begin
  if StringFromIID(GUID, OleStr) = S_OK then
  begin
    Result := OleStr;
    CoTaskMemFree(OleStr);
  end
  else
    Result := '';
end;

function StrToGuid(const S: string): TGUID;
var
  OleStr: WideString;
begin
  OleStr := S;
  IIDFromString(PWideChar(OleStr), Result);
end;

function GetClassFile(const CLSID: TGUID): string;
var
  Reg: TRegistry;
  S: WideString;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    SetLength(S, 38);
    if (StringFromGUID2(CLSID, PWideChar(S), 39) > 0) and
      (Reg.KeyExists('\CLSID\' + S + '\InprocServer32')) then
    begin
      Reg.OpenKey('\CLSID\' + S + '\InprocServer32', False);
      Result := Reg.ReadString('');
    end;
  finally
    Reg.Free;
  end;
end;

procedure GuidClear(var Guid: TGUID);
begin
  FillChar(Guid, SizeOf(TGUID), #0);
end;

procedure GetInterfaceStrings(Unknown: IUnknown; Strings: TStrings);
const
  InterfaceMap: array[0..200] of record GUID: TGUID; Name: string end = (
    (GUID: '{51AAE3E0-7486-11CF-A0C2-00AA0062BE57}'; Name: 'IActiveDesigner'),
    (GUID: '{F490EB00-1240-11D1-9888-006097DEACF9}'; Name: 'IActiveDesktop'),
    (GUID: '{0000010F-0000-0000-C000-000000000046}'; Name: 'IAdviseSink'),
    (GUID: '{00000125-0000-0000-C000-000000000046}'; Name: 'IAdviseSink2'),
    (GUID: '{D97A6DA0-A866-11CF-83AE-10A0C90C2BD8}'; Name: 'IApplicationObject'),
    (GUID: '{7A8B9910-F33C-11D2-9EDD-00C04F6539EE}'; Name: 'IASPObject'),
    (GUID: '{79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IAuthenticate'),
    (GUID: '{0000000E-0000-0000-C000-000000000046}'; Name: 'IBindCtx'),
    (GUID: '{FC4801A1-2BA9-11CF-A229-00AA003D7352}'; Name: 'IBindHost'),
    (GUID: '{79EAC9C0-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IBinding'),
    (GUID: '{79EAC9CD-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IBindProtocol'),
    (GUID: '{79EAC9C1-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IBindStatusCallback'),
    (GUID: '{0002E013-0000-0000-C000-000000000046}'; Name: 'ICatInformation'),
    (GUID: '{0002E012-0000-0000-C000-000000000046}'; Name: 'ICatRegister'),
    (GUID: '{1008C4A0-7613-11CF-9AF1-0020AF6E72F4}'; Name: 'IChannelHook'),
    (GUID: '{00000001-0000-0000-C000-000000000046}'; Name: 'IClassFactory'),
    (GUID: '{B196B28F-BAB4-101A-B69C-00AA00341D07}'; Name: 'IClassFactory2'),
    (GUID: '{79EAC9D1-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'ICodeInstall'),
    (GUID: '{000214F1-0000-0000-C000-000000000046}'; Name: 'ICommDlgBrowser'),
    (GUID: '{B196B286-BAB4-101A-B69C-00AA00341D07}'; Name: 'IConnectionPoint'),
    (GUID: '{B196B284-BAB4-101A-B69C-00AA00341D07}'; Name: 'IConnectionPointContainer'),
    (GUID: '{000214E4-0000-0000-C000-000000000046}'; Name: 'IContextMenu'),
    (GUID: '{000214F4-0000-0000-C000-000000000046}'; Name: 'IContextMenu2'),
    (GUID: '{BCFCE0A0-EC17-11D0-8D10-00A0C90F2719}'; Name: 'IContextMenu3'),
    (GUID: '{B722BCCA-4E68-101B-A2BC-00AA00404770}'; Name: 'IContinueCallback'),
    (GUID: '{000214EF-0000-0000-C000-000000000046}'; Name: 'ICopyHookA'),
    (GUID: '{000214FC-0000-0000-C000-000000000046}'; Name: 'ICopyHookW'),
    (GUID: '{22F03340-547D-101B-8E65-08002B2BD119}'; Name: 'ICreateErrorInfo'),
    (GUID: '{00020405-0000-0000-C000-000000000046}'; Name: 'ICreateTypeInfo'),
    (GUID: '{00020412-0000-0000-C000-000000000046}'; Name: 'ICreateTypeInfo2'),
    (GUID: '{00020406-0000-0000-C000-000000000046}'; Name: 'ICreateTypeLib'),
    (GUID: '{0002040F-0000-0000-C000-000000000046}'; Name: 'ICreateTypeLib2'),
    (GUID: '{00000110-0000-0000-C000-000000000046}'; Name: 'IDataAdviseHolder'),
    (GUID: '{69D14C80-C18E-11D0-A9CE-006097942311}'; Name: 'IDataFilter'),
    (GUID: '{0000010E-0000-0000-C000-000000000046}'; Name: 'IDataObject'),
    (GUID: '{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}'; Name: 'IDeskBand'),
    (GUID: '{00020400-0000-0000-C000-000000000046}'; Name: 'IDispatch'),
    (GUID: '{012DD920-7B26-11D0-8CA9-00A0C92DBFE8}'; Name: 'IDockingWindow'),
    (GUID: '{47D2657A-7B27-11D0-8CA9-00A0C92DBFE8}'; Name: 'IDockingWindowFrame'),
    (GUID: '{2A342FC2-7B26-11D0-8CA9-00A0C92DBFE8}'; Name: 'IDockingWindowSite'),
    (GUID: '{00000121-0000-0000-C000-000000000046}'; Name: 'IDropSource'),
    (GUID: '{00000122-0000-0000-C000-000000000046}'; Name: 'IDropTarget'),
    (GUID: '{70BDDE00-C18E-11D0-A9CE-006097942311}'; Name: 'IEncodingFilterFactory'),
    (GUID: '{0002E011-0000-0000-C000-000000000046}'; Name: 'IEnumCATEGORYINFO'),
    (GUID: '{B196B285-BAB4-101A-B69C-00AA00341D07}'; Name: 'IEnumConnectionPoints'),
    (GUID: '{B196B287-BAB4-101A-B69C-00AA00341D07}'; Name: 'IEnumConnections'),
    (GUID: '{00000103-0000-0000-C000-000000000046}'; Name: 'IEnumFORMATETC'),
    (GUID: '{0002E000-0000-0000-C000-000000000046}'; Name: 'IEnumGUID'),
    (GUID: '{000214F2-0000-0000-C000-000000000046}'; Name: 'IEnumIDList'),
    (GUID: '{00000102-0000-0000-C000-000000000046}'; Name: 'IEnumMoniker'),
    (GUID: '{B722BCC8-4E68-101B-A2BC-00AA00404770}'; Name: 'IEnumOleDocumentViews'),
    (GUID: '{00000104-0000-0000-C000-000000000046}'; Name: 'IEnumOLEVERB'),
    (GUID: '{00000105-0000-0000-C000-000000000046}'; Name: 'IEnumSTATDATA'),
    (GUID: '{0000013B-0000-0000-C000-000000000046}'; Name: 'IEnumSTATPROPSETSTG'),
    (GUID: '{00000139-0000-0000-C000-000000000046}'; Name: 'IEnumSTATPROPSTG'),
    (GUID: '{0000000D-0000-0000-C000-000000000046}'; Name: 'IEnumStatStg'),
    (GUID: '{00000101-0000-0000-C000-000000000046}'; Name: 'IEnumString'),
    (GUID: '{00000100-0000-0000-C000-000000000046}'; Name: 'IEnumUnknown'),
    (GUID: '{00020404-0000-0000-C000-000000000046}'; Name: 'IEnumVariant'),
    (GUID: '{1CF2B120-547D-101B-8E65-08002B2BD119}'; Name: 'IErrorInfo'),
    (GUID: '{3127CA40-446E-11CE-8135-00AA004BB851}'; Name: 'IErrorLog'),
    (GUID: '{00000019-0000-0000-C000-000000000046}'; Name: 'IExternalConnection'),
    (GUID: '{000214EB-0000-0000-C000-000000000046}'; Name: 'IExtractIconA'),
    (GUID: '{000214FA-0000-0000-C000-000000000046}'; Name: 'IExtractIconW'),
    (GUID: '{000214F0-0000-0000-C000-000000000046}'; Name: 'IFileViewerA'),
    (GUID: '{000214F3-0000-0000-C000-000000000046}'; Name: 'IFileViewerSite'),
    (GUID: '{000214F8-0000-0000-C000-000000000046}'; Name: 'IFileViewerW'),
    (GUID: '{99CAF010-415E-11CF-8814-00AA00B569F5}'; Name: 'IFillLockBytes'),
    (GUID: '{BEF6E002-A874-101A-8BBA-00AA00300CAB}'; Name: 'IFont'),
    (GUID: '{BEF6E003-A874-101A-8BBA-00AA00300CAB}'; Name: 'IFontDisp'),
    (GUID: '{79EAC9D2-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IHttpNegotiate'),
    (GUID: '{79EAC9D7-BAFA-11CE-8C82-00AA004BA90B}'; Name: 'IHttpSecurity'),
    (GUID: '{68284FAA-6A48-11D0-8C78-00C04FD918B4}'; Name: 'IInputObject'),
    (GUID: '{F1DB8392-7331-11D0-8C99-00A0C92DBFE8}'; Name: 'IInputObjectSite'),
    (GUID: '{79EAC9E0-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternet'),
    (GUID: '{79EAC9E1-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetBindInfo'),
    (GUID: '{3AF280B6-CB3F-11D0-891E-00C04FB6BFC4}'; Name: 'IInternetHostSecurityManager'),
    (GUID: '{79EAC9EB-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetPriority'),
    (GUID: '{79EAC9E4-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetProtocol'),
    (GUID: '{79EAC9EC-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetProtocolInfo'),
    (GUID: '{79EAC9E3-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetProtocolRoot'),
    (GUID: '{79EAC9E5-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetProtocolSink'),
    (GUID: '{79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetSecurityManager'),
    (GUID: '{79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetSecurityMgrSite'),
    (GUID: '{79EAC9E7-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetSession'),
    (GUID: '{79EAC9E8-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetThreadSwitch'),
    (GUID: '{79EAC9EF-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IInternetZoneManager'),
    (GUID: '{0000000A-0000-0000-C000-000000000046}'; Name: 'ILockBytes'),
    (GUID: '{00000002-0000-0000-C000-000000000046}'; Name: 'IMalloc'),
    (GUID: '{0000001D-0000-0000-C000-000000000046}'; Name: 'IMallocSpy'),
    (GUID: '{00000003-0000-0000-C000-000000000046}'; Name: 'IMarshal'),
    (GUID: '{00000016-0000-0000-C000-000000000046}'; Name: 'IMessageFilter'),
    (GUID: '{0000000F-0000-0000-C000-000000000046}'; Name: 'IMoniker'),
    (GUID: '{000214E1-0000-0000-C000-000000000046}'; Name: 'INewShortcutHookA'),
    (GUID: '{000214F7-0000-0000-C000-000000000046}'; Name: 'INewShortcutHookW'),
    (GUID: '{CB5BDC81-93C1-11CF-8F20-00805F2CD064}'; Name: 'IObjectSafety'),
    (GUID: '{FC4801A3-2BA9-11CF-A229-00AA003D7352}'; Name: 'IObjectWithSite'),
    (GUID: '{00000111-0000-0000-C000-000000000046}'; Name: 'IOleAdviseHolder'),
    (GUID: '{0000011E-0000-0000-C000-000000000046}'; Name: 'IOleCache'),
    (GUID: '{00000128-0000-0000-C000-000000000046}'; Name: 'IOleCache2'),
    (GUID: '{00000129-0000-0000-C000-000000000046}'; Name: 'IOleCacheControl'),
    (GUID: '{00000118-0000-0000-C000-000000000046}'; Name: 'IOleClientSite'),
    (GUID: '{B722BCCB-4E68-101B-A2BC-00AA00404770}'; Name: 'IOleCommandTarget'),
    (GUID: '{0000011B-0000-0000-C000-000000000046}'; Name: 'IOleContainer'),
    (GUID: '{B196B288-BAB4-101A-B69C-00AA00341D07}'; Name: 'IOleControl'),
    (GUID: '{B196B289-BAB4-101A-B69C-00AA00341D07}'; Name: 'IOleControlSite'),
    (GUID: '{B722BCC5-4E68-101B-A2BC-00AA00404770}'; Name: 'IOleDocument'),
    (GUID: '{B722BCC7-4E68-101B-A2BC-00AA00404770}'; Name: 'IOleDocumentSite'),
    (GUID: '{B722BCC6-4E68-101B-A2BC-00AA00404770}'; Name: 'IOleDocumentView'),
    (GUID: '{00000117-0000-0000-C000-000000000046}'; Name: 'IOleInPlaceActiveObject'),
    (GUID: '{00000116-0000-0000-C000-000000000046}'; Name: 'IOleInPlaceFrame'),
    (GUID: '{00000113-0000-0000-C000-000000000046}'; Name: 'IOleInPlaceObject'),
    (GUID: '{00000119-0000-0000-C000-000000000046}'; Name: 'IOleInPlaceSite'),
    (GUID: '{00000115-0000-0000-C000-000000000046}'; Name: 'IOleInPlaceUIWindow'),
    (GUID: '{0000011C-0000-0000-C000-000000000046}'; Name: 'IOleItemContainer'),
    (GUID: '{0000011D-0000-0000-C000-000000000046}'; Name: 'IOleLink'),
    (GUID: '{00000112-0000-0000-C000-000000000046}'; Name: 'IOleObject'),
    (GUID: '{00000000-0000-0000-0000-000000000000}'; Name: 'IOleUILinkContainer'),
    (GUID: '{00000000-0000-0000-0000-000000000000}'; Name: 'IOleUILinkInfo'),
    (GUID: '{00000000-0000-0000-0000-000000000000}'; Name: 'IOleUIObjInfo'),
    (GUID: '{D001F200-EF97-11CE-9BC9-00AA00608E01}'; Name: 'IOleUndoManager'),
    (GUID: '{00000114-0000-0000-C000-000000000046}'; Name: 'IOleWindow'),
    (GUID: '{0000011A-0000-0000-C000-000000000046}'; Name: 'IParseDisplayName'),
    (GUID: '{376BD3AA-3845-101B-84ED-08002B2EC713}'; Name: 'IPerPropertyBrowsing'),
    (GUID: '{0000010C-0000-0000-C000-000000000046}'; Name: 'IPersist'),
    (GUID: '{0000010B-0000-0000-C000-000000000046}'; Name: 'IPersistFile'),
    (GUID: '{000214EA-0000-0000-C000-000000000046}'; Name: 'IPersistFolder'),
    (GUID: '{1AC3D9F0-175C-11D1-95BE-00609797EA4F}'; Name: 'IPersistFolder2'),
    (GUID: '{79EAC9C9-BAF9-11CE-8C82-00AA004BA90B}'; Name: 'IPersistMoniker'),
    (GUID: '{37D84F60-42CB-11CE-8135-00AA004BB851}'; Name: 'IPersistPropertyBag'),
    (GUID: '{0000010A-0000-0000-C000-000000000046}'; Name: 'IPersistStorage'),
    (GUID: '{00000109-0000-0000-C000-000000000046}'; Name: 'IPersistStream'),
    (GUID: '{7FD52380-4E07-101B-AE2D-08002B2EC713}'; Name: 'IPersistStreamInit'),
    (GUID: '{56223FE3-D397-11CF-A42E-00AA00C00940}'; Name: 'IPersistTextStream'),
    (GUID: '{7BF80980-BF32-101A-8BBB-00AA00300CAB}'; Name: 'IPicture'),
    (GUID: '{7BF80981-BF32-101A-8BBB-00AA00300CAB}'; Name: 'IPictureDisp'),
    (GUID: '{B722BCC9-4E68-101B-A2BC-00AA00404770}'; Name: 'IPrint'),
    (GUID: '{55272A00-42CB-11CE-8135-00AA004BB851}'; Name: 'IPropertyBag'),
    (GUID: '{9BFBBC02-EFF1-101A-84ED-00AA00341D07}'; Name: 'IPropertyNotifySink'),
    (GUID: '{B196B28D-BAB4-101A-B69C-00AA00341D07}'; Name: 'IPropertyPage'),
    (GUID: '{01E44665-24AC-101B-84ED-08002B2EC713}'; Name: 'IPropertyPage2'),
    (GUID: '{B196B28C-BAB4-101A-B69C-00AA00341D07}'; Name: 'IPropertyPageSite'),
    (GUID: '{0000013A-0000-0000-C000-000000000046}'; Name: 'IPropertySetStorage'),
    (GUID: '{00000138-0000-0000-C000-000000000046}'; Name: 'IPropertyStorage'),
    (GUID: '{B196B283-BAB4-101A-B69C-00AA00341D07}'; Name: 'IProvideClassInfo'),
    (GUID: '{56223FE1-D397-11CF-A42E-00AA00C00940}'; Name: 'IProvideRuntimeText'),
    (GUID: '{D5F569D0-593B-101A-B569-08002B2DBF7A}'; Name: 'IPSFactoryBuffer'),
    (GUID: '{00021500-0000-0000-C000-000000000046}'; Name: 'IQueryInfo'),
    (GUID: '{CF51ED10-62FE-11CF-BF86-00A0C9034836}'; Name: 'IQuickActivate'),
    (GUID: '{71EAF260-0CE0-11D0-A53E-00A0C90C2091}'; Name: 'IReadCookie'),
    (GUID: '{D97A6DA0-A861-11CF-93AE-00A0C90C2BD8}'; Name: 'IRequest'),
    (GUID: '{D97A6DA0-A85F-11DF-83AE-00A0C90C2BD8}'; Name: 'IRequestDictionary'),
    (GUID: '{D97A6DA0-A864-11CF-83BE-00A0C90C2BD8}'; Name: 'IResponse'),
    (GUID: '{00000012-0000-0000-C000-000000000046}'; Name: 'IRootStorage'),
    (GUID: '{D5F56B60-593B-101A-B569-08002B2DBF7A}'; Name: 'IRpcChannelBuffer'),
    (GUID: '{D5F56A34-593B-101A-B569-08002B2DBF7A}'; Name: 'IRpcProxyBuffer'),
    (GUID: '{D5F56AFC-593B-101A-B569-08002B2DBF7A}'; Name: 'IRpcStubBuffer'),
    (GUID: '{00000126-0000-0000-C000-000000000046}'; Name: 'IRunnableObject'),
    (GUID: '{00000010-0000-0000-C000-000000000046}'; Name: 'IRunningObjectTable'),
    (GUID: '{D97A6DA0-A868-11CF-83AE-00B0C90C2BD8}'; Name: 'IScriptingContext'),
    (GUID: '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}'; Name: 'ISequentialStream'),
    (GUID: '{D97A6DA0-A867-11CF-83AE-01A0C90C2BD8}'; Name: 'IServer'),
    (GUID: '{6D5140C1-7436-11CE-8034-00AA006009FA}'; Name: 'IServiceProvider'),
    (GUID: '{D97A6DA0-A865-11CF-83AF-00A0C90C2BD8}'; Name: 'ISessionObject'),
    (GUID: '{000214E2-0000-0000-C000-000000000046}'; Name: 'IShellBrowser'),
    (GUID: '{00000000-0000-0000-0000-000000000000}'; Name: 'IShellChangeNotify'),
    (GUID: '{000214F5-0000-0000-C000-000000000046}'; Name: 'IShellExecuteHookA'),
    (GUID: '{000214FB-0000-0000-C000-000000000046}'; Name: 'IShellExecuteHookW'),
    (GUID: '{000214E8-0000-0000-C000-000000000046}'; Name: 'IShellExtInit'),
    (GUID: '{000214E6-0000-0000-C000-000000000046}'; Name: 'IShellFolder'),
    (GUID: '{000214E5-0000-0000-C000-000000000046}'; Name: 'IShellIcon'),
    (GUID: '{7D688A70-C613-11D0-999B-00C04FD655E1}'; Name: 'IShellIconOverlay'),
    (GUID: '{0C6C4200-C589-11D0-999A-00C04FD655E1}'; Name: 'IShellIconOverlayIdentifier'),
    (GUID: '{000214EE-0000-0000-C000-000000000046}'; Name: 'IShellLinkA'),
    (GUID: '{000214F9-0000-0000-C000-000000000046}'; Name: 'IShellLinkW'),
    (GUID: '{000214E9-0000-0000-C000-000000000046}'; Name: 'IShellPropSheetExt'),
    (GUID: '{000214E3-0000-0000-C000-000000000046}'; Name: 'IShellView'),
    (GUID: '{88E39E80-3578-11CF-AE69-08002B2E1262}'; Name: 'IShellView2'),
    (GUID: '{742B0E01-14E6-101B-914E-00AA00300CAB}'; Name: 'ISimpleFrameSite'),
    (GUID: '{B15B8DC1-C7E1-11D0-8680-00AA00BDCB71}'; Name: 'ISoftDistExt'),
    (GUID: '{B196B28B-BAB4-101A-B69C-00AA00341D07}'; Name: 'ISpecifyPropertyPages'),
    (GUID: '{00000018-0000-0000-C000-000000000046}'; Name: 'IStdMarshalInfo'),
    (GUID: '{0000000B-0000-0000-C000-000000000046}'; Name: 'IStorage'),
    (GUID: '{0000000C-0000-0000-C000-000000000046}'; Name: 'IStream'),
    (GUID: '{D97A6DA0-A85D-11CF-83AE-00A0C90C2BD8}'; Name: 'IStringList'),
    (GUID: '{DF0B3D60-548F-101B-8E65-08002B2BD119}'; Name: 'ISupportErrorInfo'),
    (GUID: '{00020403-0000-0000-C000-000000000046}'; Name: 'ITypeComp'),
    (GUID: '{00020401-0000-0000-C000-000000000046}'; Name: 'ITypeInfo'),
    (GUID: '{00020412-0000-0000-C000-000000000046}'; Name: 'ITypeInfo2'),
    (GUID: '{00020402-0000-0000-C000-000000000046}'; Name: 'ITypeLib'),
    (GUID: '{00020411-0000-0000-C000-000000000046}'; Name: 'ITypeLib2'),
    (GUID: '{00000000-0000-0000-C000-000000000046}'; Name: 'IUnknown'),
    (GUID: '{AC60F6A0-0FD9-11D0-99CB-00C04FD64497}'; Name: 'IURLSearchHook'),
    (GUID: '{4A7DEB90-B069-11D0-B373-00A0C90C2BD8}'; Name: 'IVariantDictionary'),
    (GUID: '{0000010D-0000-0000-C000-000000000046}'; Name: 'IViewObject'),
    (GUID: '{00000127-0000-0000-C000-000000000046}'; Name: 'IViewObject2'),
    (GUID: '{A35E20C2-837D-11D0-9E9F-00A02457621F}'; Name: 'IWeakRef'),
    (GUID: '{79EAC9D5-BAFA-11CE-8C82-00AA004BA90B}'; Name: 'IWindowForBindingUI'),
    (GUID: '{79EAC9D8-BAFA-11CE-8C82-00AA004BA90B}'; Name: 'IWinInetHttpInfo'),
    (GUID: '{79EAC9D6-BAFA-11CE-8C82-00AA004BA90B}'; Name: 'IWinInetInfo'),
    (GUID: '{D97A6DA0-A862-11CF-84AE-00A0C90C2BD8}'; Name: 'IWriteCookie'));
var
  Obj: IUnknown;
  I: Integer;
begin
  Strings.BeginUpdate;
  with Unknown, Strings do
  try
    Clear;
    for I := Low(InterfaceMap) to High(InterfaceMap) do
      if QueryInterface(InterfaceMap[I].GUID, Obj) = S_OK then
        Add(InterfaceMap[I].Name);
  finally
    EndUpdate;
  end;
end;

procedure EnumerateVariant(const V: OleVariant; Callback: TVariantCallback;
  Data: Pointer);
var
  Unknown: IUnknown;
  EnumVariant: IEnumVariant;
  Variant: OleVariant;
  Dummy: Cardinal;
begin
  if VarType(V) = varDispatch then
  begin
    Unknown := V._NewEnum;
    EnumVariant := Unknown as IEnumVariant;
    while EnumVariant.Next(1, Variant, Dummy) = S_OK do
      Callback(Variant, Data);
  end;
end;

{ TEnumString }

procedure TEnumString.SetStrings(Value: TStrings);
begin
  if Value <> FStrings then
  begin
    FIndex := 0;
    FStrings := Value;
  end;
end;

{ TEnumString.IEnumString }

function TEnumString.Next(celt: Longint;
  out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  Result := S_FALSE;
  while (I < celt) and (FIndex < FStrings.Count) do
  begin
    TPointerList(elt)[I] := PWideChar(WideString(FStrings[FIndex]));
    Inc(I);
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK;
end;

function TEnumString.Skip(celt: Longint): HResult;
begin
  Result := S_FALSE;
  if FStrings <> nil then
    if (FIndex + celt) <= FStrings.Count then
    begin
      Inc(FIndex, celt);
      Result := S_OK;
    end
    else
      FIndex := FStrings.Count;
end;

function TEnumString.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumString.Clone(out enm: IEnumString): HResult;
var
  EnumString: TEnumString;
begin
  EnumString := TEnumString.Create;
  EnumString.Strings := FStrings;
  EnumString.FIndex := FIndex;
  enm := EnumString as IEnumString;
  Result := S_OK;
end;

{ TEnumVariant }

constructor TEnumVariant.Create(List: TObjectList);
begin
  inherited Create;
  FIndex := -1;
  FObjectList := List;
end;

{ TEnumVariant.IEnumVaraint }

function TEnumVariant.Next(celt: LongWord; var rgvar: OleVariant;
  out pceltFetched: LongWord): HResult;
type
  TVariants = array[0..High(Word) div 8] of OleVariant;
var
  Variants: TVariants absolute rgvar;
  Dispatch: IDispatch;
  I: LongWord;
begin
  Result := S_OK;
  for I := 0 to celt - 1 do
  begin
    Inc(FIndex);
    if FIndex > FObjectList.Count - 1 then
    begin
      FIndex := FObjectList.Count - 1;
      Result := S_FALSE;
      Break;
    end;
    if FObjectList[FIndex].GetInterface(IDispatch, Dispatch) then
      Variants[I] := Dispatch
    else
    begin
      Result := S_FALSE;
      Break;
    end;
  end;
   if @pceltFetched <> nil then
    pceltFetched := I;
end;

function TEnumVariant.Skip(celt: LongWord): HResult;
begin
  Result := S_FALSE;
  if FIndex + Integer(celt) > FObjectList.Count - 1 then
    FIndex := FObjectList.Count - 1
  else
  begin
    Inc(FIndex, celt);
    Result := S_OK;
  end;
end;

function TEnumVariant.Reset: HResult;
begin
  FIndex := -1;
  Result := S_OK;
end;

function TEnumVariant.Clone(out Enum: IEnumVariant): HResult;
var
  EnumVariant: TEnumVariant;
begin
  EnumVariant := TEnumVariant.Create(FObjectList);
  EnumVariant.FIndex := FIndex;
  Enum := EnumVariant as IEnumVariant;
  Result := S_OK;
end;

{ TAutoStorable }

procedure TAutoStorable.InternalExecute(const Command: OleVariant);
begin
end;

function TAutoStorable.InternalRetrieve(const Format: OleVariant): OleVariant;
begin
end;

procedure TAutoStorable.InternalUpdate(const Data: OleVariant);
begin
end;

{ TAutoStorable.IStorable }

procedure TAutoStorable.Execute(Command: OleVariant);
begin
  InternalExecute(Command);
end;

function TAutoStorable.Retrieve(Format: OleVariant): OleVariant;
begin
  Result := InternalRetrieve(Format);
end;

procedure TAutoStorable.Update(Data: OleVariant);
begin
  InternalUpdate(Data);
end;

{ TAutoCollection }

constructor TAutoCollection.Create(Collection: TGUID;
  ItemClass: TAutoIntfObjectClass; Item: TGUID);
begin
  FObjectList := TObjectList.Create(False);
  FItemClass := ItemClass;
  FItem := Item;
  { TODO: get typelib  }
  inherited Create(nil, Collection);
end;

destructor TAutoCollection.Destroy;
var
  I: Integer;
begin
  for I := 0 to FObjectList.Count - 1 do
    TAutoCollection(FObjectList[I])._Release;
  FObjectList.Free;
  inherited Destroy;
end;

function TAutoCollection.InternalAdd(const Item: OleVariant): OleVariant;
var
  AutoObject: TAutoIntfObject;
begin
  { TODO: get typelib }
  AutoObject := FItemClass.Create(nil, FItem);
  TAutoCollection(AutoObject)._AddRef;
  FObjectList.Add(AutoObject);
  Result := AutoObject as IDispatch;
end;

procedure TAutoCollection.InternalDelete(const Item: OleVariant);
var
  I: Integer;
begin
  I := Item;
  if I < FObjectList.Count - 1 then
  begin
    TAutoCollection(FObjectList[I])._Release;
    FObjectList.Delete(I);
  end;
end;

function TAutoCollection.InternalGetCount: Integer;
begin
  Result := FObjectList.Count;
end;

function TAutoCollection.InternalGetItem(const Index: OleVariant): OleVariant;
var
  AutoIntfObject: TAutoIntfObject;
  I: Integer;
begin
  I := Index;
  if (I > -1) and (I < FObjectList.Count) then
    AutoIntfObject := TAutoIntfObject(FObjectList[I])
  else
    AutoIntfObject := nil;
  if AutoIntfObject <> nil then
    Result := AutoIntfObject as IDispatch
  else
    VarClear(Result);
end;

function TAutoCollection.InternalSearch(const Params: OleVariant): OleVariant;
begin
  VarClear(Result);
end;

{ TAutoCollection.ICollection }

function TAutoCollection.Get__NewEnum: IUnknown;
begin
  Result := TEnumVariant.Create(FObjectList) as IUnknown;
end;

function TAutoCollection.Add(Item: OleVariant): OleVariant;
begin
  Result := InternalAdd(Item);
end;

procedure TAutoCollection.Delete(Item: OleVariant);
begin
  InternalDelete(Item);
end;

function TAutoCollection.Get_Count: Integer;
begin
  Result := InternalGetCount;
end;

function TAutoCollection.Get_Item(Index: OleVariant): OleVariant;
begin
  Result := InternalGetItem(Index);
end;

{ TAutoCollection.ISearchCollection }

function TAutoCollection.Search(Params: OleVariant): OleVariant;
begin
  Result := InternalSearch(Params);
end;

{ TScriptEvents }

constructor TScriptEvents.Create;
begin
  inherited Create;
  FEvents := TStringList.Create;
end;

destructor TScriptEvents.Destroy;
var
  I: Integer;
begin
  inherited Destroy;
  for I := 0 to FEvents.Count - 1 do
    IUnknown(Pointer(FEvents.Objects[I]))._Release;
  FEvents.Free;
end;

procedure TScriptEvents.AttachEvent(const Name: string; Handler: IDispatch);
begin
  if Name <> '' then
  begin
    Handler._AddRef;
    FEvents.AddObject(Name, TObject(Handler));
  end;
end;

procedure TScriptEvents.DetachEvent(const Name: string; Handler: IDispatch);
var
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if (FEvents.Objects[I] = TObject(Handler)) and (FEvents[I] = Name) then
    begin
      IDispatch(Pointer(FEvents.Objects[I]))._Release;
      FEvents.Delete(I);
      Break;
    end;
end;

procedure TScriptEvents.InvokeEvent(const Name: string);
const
  IID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
  DispArgs: array[0..3] of Pointer = (nil, nil, nil, nil);
var
  Dispatch: IDispatch;
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if FEvents[I] = Name then
    begin
      Dispatch := IDispatch(Pointer(FEvents.Objects[I]));
      Dispatch.Invoke(0, IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_METHOD,
        DispArgs, nil, nil, nil);
    end;
end;

{ EventSink helper routines }

function GetConnection(ConnectionPoints: IConnectionPointContainer;
  EventID: TGUID): IConnectionPoint;
begin
  Result := nil;
  if ConnectionPoints <> nil then
    OleCheck(ConnectionPoints.FindConnectionPoint(EventID, Result));
end;

function ConnectEvent(Instance: IUnknown; EventID: TGUID; Event: IUnknown): Integer;
var
  ConnectionPoints: IConnectionPointContainer;
  ConnectionPoint: IConnectionPoint;
begin
  Result := 0;
  if Supports(Instance, IConnectionPointContainer,
    ConnectionPoints) then
  begin
    ConnectionPoint := GetConnection(ConnectionPoints, EventID);
    if (ConnectionPoint <> nil) then
      OleCheck(ConnectionPoint.Advise(Event, Result));
  end;
end;

procedure DisconnectEvent(Instance: IUnknown; EventID: TGUID; Cookie: Integer);
var
  ConnectionPoints: IConnectionPointContainer;
  ConnectionPoint: IConnectionPoint;
begin
  if (Cookie <> 0) and Supports(Instance, IConnectionPointContainer,
    ConnectionPoints) then
  begin
    ConnectionPoint := GetConnection(ConnectionPoints, EventID);
    if (ConnectionPoint <> nil) then
      OleCheck(ConnectionPoint.Unadvise(Cookie));
  end;
end;

{ var
  Document: OleVariant;
  List: OleVariant;
  I: Integer;
begin
  Document := CreateOleObject('MSXML.DOMDocument');
  Document.Load('c:\temp\test.xml');
  List := Document.GetElementsByTagName('person');
  for I := 0 to List.Length - 1 do
    ShowMessage(List.Item(I).Text);
end; }

end.
