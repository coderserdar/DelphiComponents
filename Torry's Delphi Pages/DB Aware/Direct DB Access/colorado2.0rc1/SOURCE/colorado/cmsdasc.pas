unit cmsdasc;

// ************************************************************************ //
// WARNING                                                                  //
// -------                                                                  //
// The types declared in this file were generated from data read from a     //
// Type Library. If this type library is explicitly or indirectly (via      //
// another type library referring to this type library) re-imported, or the //
// 'Refresh' command of the Type Library Editor activated while editing the //
// Type Library, the contents of this file will be regenerated and all      //
// manual modifications will be lost.                                       //
// ************************************************************************ //

// PASTLWTR : $Revision:   1.11.1.75  $
// File generated on 13-08-2000 10:24:18 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\PROGRAM FILES\COMMON FILES\SYSTEM\OLE DB\OLEDB32.DLL
// IID\LCID: {2206CEB0-19C1-11D1-89E0-00C04FD7A829}\0
// Helpfile:
// HelpString: Microsoft OLE DB Service Component 1.0 Type Library
// Version:    1.0
// ************************************************************************ //

interface

uses Windows, ActiveX;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:      //
//   Type Libraries     : LIBID_xxxx                                    //
//   CoClasses          : CLASS_xxxx                                    //
//   DISPInterfaces     : DIID_xxxx                                     //
//   Non-DISP interfaces: IID_xxxx                                      //
// *********************************************************************//
const
  LIBID_MSDASC: TGUID = '{2206CEB0-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDataSourceLocator: TGUID = '{2206CCB2-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDataInitialize: TGUID = '{2206CCB1-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDBPromptInitialize: TGUID = '{2206CCB0-19C1-11D1-89E0-00C04FD7A829}';
  CLASS_DataLinks: TGUID = '{2206CDB2-19C1-11D1-89E0-00C04FD7A829}';
  CLASS_MSDAINITIALIZE: TGUID = '{2206CDB0-19C1-11D1-89E0-00C04FD7A829}';
  IID_IPersist: TGUID = '{0000010C-0000-0000-C000-000000000046}';
  IID_IPersistFile: TGUID = '{0000010B-0000-0000-C000-000000000046}';
  CLASS_PDPO: TGUID = '{CCB4EC60-B9DC-11D1-AC80-00A0C9034873}';
  IID_IMarshal: TGUID = '{00000003-0000-0000-C000-000000000046}';
  IID_IDBProperties: TGUID = '{0C733A8A-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IAuthenticate: TGUID = '{79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}';
  IID_IDBBinderProperties: TGUID = '{0C733AB3-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IRegisterProvider: TGUID = '{0C733AB9-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ICreateRow: TGUID = '{0C733AB2-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IBindResource: TGUID = '{0C733AB1-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IErrorInfo: TGUID = '{1CF2B120-547D-101B-8E65-08002B2BD119}';
  CLASS_RootBinder: TGUID = '{FF151822-B0BF-11D1-A80D-000000000000}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IDataSourceLocator = interface;
  IDataSourceLocatorDisp = dispinterface;
  IDataInitialize = interface;
  IDBPromptInitialize = interface;
  IPersist = interface;
  IPersistFile = interface;
  IMarshal = interface;
  IDBProperties = interface;
  IAuthenticate = interface;
  IDBBinderProperties = interface;
  IRegisterProvider = interface;
  ICreateRow = interface;
  IBindResource = interface;
  IErrorInfo = interface;
  ISequentialStream = interface;
  IStream = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to its Default Interface)            //
// *********************************************************************//
  DataLinks = IDataSourceLocator;
  MSDAINITIALIZE = IDataInitialize;
  PDPO = IPersistFile;
  RootBinder = IBindResource;


// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 
  PUINT1 = ^UINT; {*}
  PWord1 = ^Word; {*}
  PUserType1 = ^TGUID; {*}
  PUserType2 = ^_COSERVERINFO; {*}
  PPUserType1 = ^PUserType1; {*}
  PUserType3 = ^tagDBPROPIDSET; {*}
  PUserType4 = ^tagDBPROPSET; {*}
  PUserType5 = ^tagDBPROPINFOSET; {*}
  PByte1 = ^Byte; {*}

  LONG_PTR = Integer; 

  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  _COAUTHIDENTITY = packed record
    User: ^Word;
    UserLength: UINT;
    Domain: ^Word;
    DomainLength: UINT;
    Password: ^Word;
    PasswordLength: UINT;
    Flags: UINT;
  end;

  _COAUTHINFO = packed record
    dwAuthnSvc: UINT;
    dwAuthzSvc: UINT;
    pwszServerPrincName: PWideChar;
    dwAuthnLevel: UINT;
    dwImpersonationLevel: UINT;
    pAuthIdentityData: ^_COAUTHIDENTITY;
    dwCapabilities: UINT;
  end;

  _COSERVERINFO = packed record
    dwReserved1: UINT;
    pwszName: PWideChar;
    pAuthInfo: ^_COAUTHINFO;
    dwReserved2: UINT;
  end;

  tagDBPROPIDSET = packed record
    rgPropertyIDs: ^UINT;
    cPropertyIDs: UINT;
    guidPropertySet: TGUID;
  end;

  __MIDL_DBStructureDefinitions_0002 = record
    case Integer of
      0: (pwszName: PWideChar);
      1: (ulPropid: UINT);
  end;

  __MIDL_DBStructureDefinitions_0001 = record
    case Integer of
      0: (guid: TGUID);
      1: (pguid: ^TGUID);
  end;

  tagDBID = packed record
    uGuid: __MIDL_DBStructureDefinitions_0001;
    eKind: UINT;
    uName: __MIDL_DBStructureDefinitions_0002;
  end;

  tagDBPROP = packed record
    dwPropertyID: UINT;
    dwOptions: UINT;
    dwStatus: UINT;
    colid: tagDBID;
    vValue: OleVariant;
  end;

  tagDBPROPSET = packed record
    rgProperties: ^tagDBPROP;
    cProperties: UINT;
    guidPropertySet: TGUID;
  end;

  tagDBPROPINFO = packed record
    pwszDescription: PWideChar;
    dwPropertyID: UINT;
    dwFlags: UINT;
    vtType: Word;
    vValues: OleVariant;
  end;

  tagDBPROPINFOSET = packed record
    rgPropertyInfos: ^tagDBPROPINFO;
    cPropertyInfos: UINT;
    guidPropertySet: TGUID;
  end;


{  Uses Windows

  _LARGE_INTEGER = packed record
    QuadPart: Comp;
  end;

  ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  _FILETIME = packed record
    dwLowDateTime: UINT;
    dwHighDateTime: UINT;
  end; }

  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: UINT;
    cbSize: ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: UINT;
    grfLocksSupported: UINT;
    clsid: TGUID;
    grfStateBits: UINT;
    reserved: UINT;
  end;


// *********************************************************************//
// Interface: IDataSourceLocator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2206CCB2-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataSourceLocator = interface(IDispatch)
    ['{2206CCB2-19C1-11D1-89E0-00C04FD7A829}']
    function Get_hWnd: LONG_PTR; safecall;
    procedure Set_hWnd(phwndParent: LONG_PTR); safecall;
    function PromptNew: IDispatch; safecall;
    function PromptEdit(var ppADOConnection: IDispatch): WordBool; safecall;
    property hWnd: LONG_PTR read Get_hWnd write Set_hWnd;
  end;

// *********************************************************************//
// DispIntf:  IDataSourceLocatorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2206CCB2-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataSourceLocatorDisp = dispinterface
    ['{2206CCB2-19C1-11D1-89E0-00C04FD7A829}']
    property hWnd: LONG_PTR dispid 1610743808;
    function PromptNew: IDispatch; dispid 1610743810;
    function PromptEdit(var ppADOConnection: IDispatch): WordBool; dispid 1610743811;
  end;

// *********************************************************************//
// Interface: IDataInitialize
// Flags:     (0)
// GUID:      {2206CCB1-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(const pUnkOuter: IUnknown; dwClsCtx: UINT; 
                           pwszInitializationString: PWideChar; var riid: TGUID; 
                           var ppDataSource: IUnknown): HResult; stdcall;
    function GetInitializationString(const pDataSource: IUnknown; fIncludePassword: Shortint; 
                                     out ppwszInitString: PWideChar): HResult; stdcall;
    function CreateDBInstance(var clsidProvider: TGUID; const pUnkOuter: IUnknown; dwClsCtx: UINT; 
                              pwszReserved: PWideChar; var riid: TGUID; out ppDataSource: IUnknown): HResult; stdcall;
    function RemoteCreateDBInstanceEx(var clsidProvider: TGUID; const pUnkOuter: IUnknown; 
                                      dwClsCtx: UINT; pwszReserved: PWideChar; 
                                      var pServerInfo: _COSERVERINFO; cmq: UINT; 
                                      rgpIID: PPUserType1; out rgpItf: IUnknown; out rghr: HResult): HResult; stdcall;
    function LoadStringFromStorage(pwszFileName: PWideChar; out ppwszInitializationString: PWideChar): HResult; stdcall;
    function WriteStringToStorage(pwszFileName: PWideChar; pwszInitializationString: PWideChar; 
                                  dwCreationDisposition: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBPromptInitialize
// Flags:     (512) Restricted
// GUID:      {2206CCB0-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function RemotePromptDataSource(const pUnkOuter: IUnknown; var hWndParent: _RemotableHandle; 
                                    dwPromptOptions: UINT; cSourceTypeFilter: UINT; 
                                    var rgSourceTypeFilter: UINT; cchProviderFilter: UINT; 
                                    var pwszszzProviderFilter: Word; var riid: TGUID; 
                                    var ppDataSource: IUnknown): HResult; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: UINT; 
                            pwszInitialDirectory: PWideChar; pwszInitialFile: PWideChar; 
                            out ppwszSelectedFile: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPersist
// Flags:     (0)
// GUID:      {0000010C-0000-0000-C000-000000000046}
// *********************************************************************//
  IPersist = interface(IUnknown)
    ['{0000010C-0000-0000-C000-000000000046}']
    function GetClassID(out pClassID: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPersistFile
// Flags:     (0)
// GUID:      {0000010B-0000-0000-C000-000000000046}
// *********************************************************************//
  IPersistFile = interface(IPersist)
    ['{0000010B-0000-0000-C000-000000000046}']
    function IsDirty: HResult; stdcall;
    function Load(pszFileName: PWideChar; dwMode: UINT): HResult; stdcall;
    function Save(pszFileName: PWideChar; fRemember: Integer): HResult; stdcall;
    function SaveCompleted(pszFileName: PWideChar): HResult; stdcall;
    function GetCurFile(out ppszFileName: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMarshal
// Flags:     (0)
// GUID:      {00000003-0000-0000-C000-000000000046}
// *********************************************************************//
  IMarshal = interface(IUnknown)
    ['{00000003-0000-0000-C000-000000000046}']
    function GetUnmarshalClass(var riid: TGUID; var pv: Pointer; dwDestContext: UINT; 
                               var pvDestContext: Pointer; mshlflags: UINT; out pCid: TGUID): HResult; stdcall;
    function GetMarshalSizeMax(var riid: TGUID; var pv: Pointer; dwDestContext: UINT; 
                               var pvDestContext: Pointer; mshlflags: UINT; out pSize: UINT): HResult; stdcall;
    function MarshalInterface(const pstm: IStream; var riid: TGUID; var pv: Pointer; 
                              dwDestContext: UINT; var pvDestContext: Pointer; mshlflags: UINT): HResult; stdcall;
    function UnmarshalInterface(const pstm: IStream; var riid: TGUID; out ppv: Pointer): HResult; stdcall;
    function ReleaseMarshalData(const pstm: IStream): HResult; stdcall;
    function DisconnectObject(dwReserved: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBProperties
// Flags:     (0)
// GUID:      {0C733A8A-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBProperties = interface(IUnknown)
    ['{0C733A8A-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteGetProperties(cPropertyIDSets: UINT; var rgPropertyIDSets: tagDBPROPIDSET; 
                                 var pcPropertySets: UINT; out prgPropertySets: PUserType4; 
                                 out ppErrorInfoRem: IErrorInfo): HResult; stdcall;
    function RemoteGetPropertyInfo(cPropertyIDSets: UINT; var rgPropertyIDSets: tagDBPROPIDSET; 
                                   var pcPropertyInfoSets: UINT; 
                                   out prgPropertyInfoSets: PUserType5; var pcOffsets: UINT; 
                                   out prgDescOffsets: PUINT1; var pcbDescBuffer: UINT; 
                                   var ppDescBuffer: PWord1; out ppErrorInfoRem: IErrorInfo): HResult; stdcall;
    function RemoteSetProperties(cPropertySets: UINT; var rgPropertySets: tagDBPROPSET; 
                                 cTotalProps: UINT; out rgPropStatus: UINT; 
                                 out ppErrorInfoRem: IErrorInfo): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAuthenticate
// Flags:     (0)
// GUID:      {79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}
// *********************************************************************//
  IAuthenticate = interface(IUnknown)
    ['{79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}']
    function Authenticate(out phwnd: wireHWND; out pszUsername: PWideChar; 
                          out pszPassword: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBBinderProperties
// Flags:     (0)
// GUID:      {0C733AB3-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBBinderProperties = interface(IDBProperties)
    ['{0C733AB3-2A1C-11CE-ADE5-00AA0044773D}']
    function Reset: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRegisterProvider
// Flags:     (0)
// GUID:      {0C733AB9-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRegisterProvider = interface(IUnknown)
    ['{0C733AB9-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteGetURLMapping(pwszURL: PWideChar; dwReserved: UINT; out pclsidProvider: TGUID): HResult; stdcall;
    function SetURLMapping(pwszURL: PWideChar; dwReserved: UINT; var rclsidProvider: TGUID): HResult; stdcall;
    function UnregisterProvider(pwszURL: PWideChar; dwReserved: UINT; var rclsidProvider: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICreateRow
// Flags:     (0)
// GUID:      {0C733AB2-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICreateRow = interface(IUnknown)
    ['{0C733AB2-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteCreateRow(const pUnkOuter: IUnknown; pwszURL: PWideChar; dwBindURLFlags: UINT; 
                             var rguid: TGUID; var riid: TGUID; const pAuthenticate: IAuthenticate; 
                             const pSessionUnkOuter: IUnknown; var piid: TGUID; 
                             var ppSession: IUnknown; var pdwBindStatus: UINT; 
                             var ppwszNewURL: PWideChar; out ppUnk: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IBindResource
// Flags:     (0)
// GUID:      {0C733AB1-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IBindResource = interface(IUnknown)
    ['{0C733AB1-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteBind(const pUnkOuter: IUnknown; pwszURL: PWideChar; dwBindURLFlags: UINT; 
                        var rguid: TGUID; var riid: TGUID; const pAuthenticate: IAuthenticate; 
                        const pSessionUnkOuter: IUnknown; var piid: TGUID; var ppSession: IUnknown; 
                        var pdwBindStatus: UINT; out ppUnk: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IErrorInfo
// Flags:     (0)
// GUID:      {1CF2B120-547D-101B-8E65-08002B2BD119}
// *********************************************************************//
  IErrorInfo = interface(IUnknown)
    ['{1CF2B120-547D-101B-8E65-08002B2BD119}']
    function GetGUID(out pguid: TGUID): HResult; stdcall;
    function GetSource(out pBstrSource: WideString): HResult; stdcall;
    function GetDescription(out pBstrDescription: WideString): HResult; stdcall;
    function GetHelpFile(out pBstrHelpFile: WideString): HResult; stdcall;
    function GetHelpContext(out pdwHelpContext: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteRead(out pv: Byte; cb: UINT; out pcbRead: UINT): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: UINT; out pcbWritten: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: UINT; 
                        out plibNewPosition: ULARGE_INTEGER): HResult; stdcall;
    function SetSize(libNewSize: ULARGE_INTEGER): HResult; stdcall;
    function RemoteCopyTo(const pstm: IStream; cb: ULARGE_INTEGER; out pcbRead: ULARGE_INTEGER; 
                          out pcbWritten: ULARGE_INTEGER): HResult; stdcall;
    function Commit(grfCommitFlags: UINT): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: ULARGE_INTEGER; cb: ULARGE_INTEGER; dwLockType: UINT): HResult; stdcall;
    function UnlockRegion(libOffset: ULARGE_INTEGER; cb: ULARGE_INTEGER; dwLockType: UINT): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: UINT): HResult; stdcall;
    function Clone(out ppstm: IStream): HResult; stdcall;
  end;

  CoDataLinks = class
    class function Create: IDataSourceLocator;
    class function CreateRemote(const MachineName: string): IDataSourceLocator;
  end;

  CoMSDAINITIALIZE = class
    class function Create: IDataInitialize;
    class function CreateRemote(const MachineName: string): IDataInitialize;
  end;

  CoPDPO = class
    class function Create: IPersistFile;
    class function CreateRemote(const MachineName: string): IPersistFile;
  end;

  CoRootBinder = class
    class function Create: IBindResource;
    class function CreateRemote(const MachineName: string): IBindResource;
  end;

implementation

uses ComObj;

class function CoDataLinks.Create: IDataSourceLocator;
begin
  Result := CreateComObject(CLASS_DataLinks) as IDataSourceLocator;
end;

class function CoDataLinks.CreateRemote(const MachineName: string): IDataSourceLocator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DataLinks) as IDataSourceLocator;
end;

class function CoMSDAINITIALIZE.Create: IDataInitialize;
begin
  Result := CreateComObject(CLASS_MSDAINITIALIZE) as IDataInitialize;
end;

class function CoMSDAINITIALIZE.CreateRemote(const MachineName: string): IDataInitialize;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MSDAINITIALIZE) as IDataInitialize;
end;

class function CoPDPO.Create: IPersistFile;
begin
  Result := CreateComObject(CLASS_PDPO) as IPersistFile;
end;

class function CoPDPO.CreateRemote(const MachineName: string): IPersistFile;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PDPO) as IPersistFile;
end;

class function CoRootBinder.Create: IBindResource;
begin
  Result := CreateComObject(CLASS_RootBinder) as IBindResource;
end;

class function CoRootBinder.CreateRemote(const MachineName: string): IBindResource;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RootBinder) as IBindResource;
end;

end.
