unit ADOEditorApi;

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
// File generated on 08/28/1999 11:57:30 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Program Files\Common Files\SYSTEM\ole db\Oledb32.dll
// IID\LCID: {2206CEB0-19C1-11D1-89E0-00C04FD7A829}\0
// Helpfile: 
// HelpString: Microsoft OLE DB Service Component 1.0 Type Library
// Version:    1.0
// ************************************************************************ //

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

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

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to its Default Interface)            //
// *********************************************************************//
  DataLinks = IDataSourceLocator;
  MSDAINITIALIZE = IDataInitialize;
  PDPO = IPersistFile;


// *********************************************************************//
// Declaration of structures, unions and aliases.                       //
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 
  PUINT1 = ^UINT; {*}
  PSmallint1 = ^Smallint; {*}
  PUserType1 = ^TGUID; {*}
  PUserType2 = ^_COSERVERINFO; {*}
  PPUserType1 = ^PUserType1; {*}


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


// *********************************************************************//
// Interface: IDataSourceLocator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2206CCB2-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataSourceLocator = interface(IDispatch)
    ['{2206CCB2-19C1-11D1-89E0-00C04FD7A829}']
    function Get_hWnd: Integer; safecall;
    procedure Set_hWnd(phwndParent: Integer); safecall;
    function PromptNew: IDispatch; safecall;
    function PromptEdit(var ppADOConnection: IDispatch): WordBool; safecall;
    property hWnd: Integer read Get_hWnd write Set_hWnd;
  end;

// *********************************************************************//
// DispIntf:  IDataSourceLocatorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2206CCB2-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataSourceLocatorDisp = dispinterface
    ['{2206CCB2-19C1-11D1-89E0-00C04FD7A829}']
    property hWnd: Integer dispid 1610743808;
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
                                    var pwszszzProviderFilter: Smallint; var riid: TGUID; 
                                    var ppDataSource: IUnknown): HResult; stdcall;
    function PromptFileName(var hWndParent: _RemotableHandle; dwPromptOptions: UINT; 
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

end.
