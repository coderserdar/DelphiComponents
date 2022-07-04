unit Srv_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88  $
// File generated on 08/11/2000 6:27:38 from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: U:\VCL3\Demos\Midas\Srv.tlb (1)
// IID\LCID: {4881E391-6F8A-11D4-94F8-000000000000}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (D:\WINNT\System32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (D:\WINNT\System32\STDVCL40.DLL)
//   (3) v1.0 Midas, (D:\WINNT\System32\Midas.dll)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL, 
  MIDAS;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SrvMajorVersion = 1;
  SrvMinorVersion = 0;

  LIBID_Srv: TGUID = '{4881E391-6F8A-11D4-94F8-000000000000}';

  IID_INCOCI8MidasTest: TGUID = '{4881E392-6F8A-11D4-94F8-000000000000}';
  CLASS_NCOCI8MidasTest: TGUID = '{4881E394-6F8A-11D4-94F8-000000000000}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  INCOCI8MidasTest = interface;
  INCOCI8MidasTestDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  NCOCI8MidasTest = INCOCI8MidasTest;


// *********************************************************************//
// Interface: INCOCI8MidasTest
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4881E392-6F8A-11D4-94F8-000000000000}
// *********************************************************************//
  INCOCI8MidasTest = interface(IAppServer)
    ['{4881E392-6F8A-11D4-94F8-000000000000}']
  end;

// *********************************************************************//
// DispIntf:  INCOCI8MidasTestDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4881E392-6F8A-11D4-94F8-000000000000}
// *********************************************************************//
  INCOCI8MidasTestDisp = dispinterface
    ['{4881E392-6F8A-11D4-94F8-000000000000}']
    function  AS_ApplyUpdates(const ProviderName: WideString; Delta: OleVariant; 
                              MaxErrors: Integer; out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; dispid 20000000;
    function  AS_GetRecords(const ProviderName: WideString; Count: Integer; out RecsOut: Integer; 
                            Options: Integer; const CommandText: WideString; 
                            var Params: OleVariant; var OwnerData: OleVariant): OleVariant; dispid 20000001;
    function  AS_DataRequest(const ProviderName: WideString; Data: OleVariant): OleVariant; dispid 20000002;
    function  AS_GetProviderNames: OleVariant; dispid 20000003;
    function  AS_GetParams(const ProviderName: WideString; var OwnerData: OleVariant): OleVariant; dispid 20000004;
    function  AS_RowRequest(const ProviderName: WideString; Row: OleVariant; RequestType: Integer; 
                            var OwnerData: OleVariant): OleVariant; dispid 20000005;
    procedure AS_Execute(const ProviderName: WideString; const CommandText: WideString; 
                         var Params: OleVariant; var OwnerData: OleVariant); dispid 20000006;
  end;

// *********************************************************************//
// The Class CoNCOCI8MidasTest provides a Create and CreateRemote method to          
// create instances of the default interface INCOCI8MidasTest exposed by              
// the CoClass NCOCI8MidasTest. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNCOCI8MidasTest = class
    class function Create: INCOCI8MidasTest;
    class function CreateRemote(const MachineName: string): INCOCI8MidasTest;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNCOCI8MidasTest
// Help String      : NCOCI8MidasTest Object
// Default Interface: INCOCI8MidasTest
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNCOCI8MidasTestProperties= class;
{$ENDIF}
  TNCOCI8MidasTest = class(TOleServer)
  private
    FIntf:        INCOCI8MidasTest;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TNCOCI8MidasTestProperties;
    function      GetServerProperties: TNCOCI8MidasTestProperties;
{$ENDIF}
    function      GetDefaultInterface: INCOCI8MidasTest;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INCOCI8MidasTest);
    procedure Disconnect; override;
    function  AS_ApplyUpdates(const ProviderName: WideString; Delta: OleVariant; 
                              MaxErrors: Integer; out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant;
    function  AS_GetRecords(const ProviderName: WideString; Count: Integer; out RecsOut: Integer; 
                            Options: Integer; const CommandText: WideString; 
                            var Params: OleVariant; var OwnerData: OleVariant): OleVariant;
    function  AS_DataRequest(const ProviderName: WideString; Data: OleVariant): OleVariant;
    function  AS_GetProviderNames: OleVariant;
    function  AS_GetParams(const ProviderName: WideString; var OwnerData: OleVariant): OleVariant;
    function  AS_RowRequest(const ProviderName: WideString; Row: OleVariant; RequestType: Integer; 
                            var OwnerData: OleVariant): OleVariant;
    procedure AS_Execute(const ProviderName: WideString; const CommandText: WideString; 
                         var Params: OleVariant; var OwnerData: OleVariant);
    property  DefaultInterface: INCOCI8MidasTest read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNCOCI8MidasTestProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNCOCI8MidasTest
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNCOCI8MidasTestProperties = class(TPersistent)
  private
    FServer:    TNCOCI8MidasTest;
    function    GetDefaultInterface: INCOCI8MidasTest;
    constructor Create(AServer: TNCOCI8MidasTest);
  protected
  public
    property DefaultInterface: INCOCI8MidasTest read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoNCOCI8MidasTest.Create: INCOCI8MidasTest;
begin
  Result := CreateComObject(CLASS_NCOCI8MidasTest) as INCOCI8MidasTest;
end;

class function CoNCOCI8MidasTest.CreateRemote(const MachineName: string): INCOCI8MidasTest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NCOCI8MidasTest) as INCOCI8MidasTest;
end;

procedure TNCOCI8MidasTest.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4881E394-6F8A-11D4-94F8-000000000000}';
    IntfIID:   '{4881E392-6F8A-11D4-94F8-000000000000}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNCOCI8MidasTest.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INCOCI8MidasTest;
  end;
end;

procedure TNCOCI8MidasTest.ConnectTo(svrIntf: INCOCI8MidasTest);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNCOCI8MidasTest.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNCOCI8MidasTest.GetDefaultInterface: INCOCI8MidasTest;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TNCOCI8MidasTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNCOCI8MidasTestProperties.Create(Self);
{$ENDIF}
end;

destructor TNCOCI8MidasTest.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNCOCI8MidasTest.GetServerProperties: TNCOCI8MidasTestProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TNCOCI8MidasTest.AS_ApplyUpdates(const ProviderName: WideString; Delta: OleVariant; 
                                           MaxErrors: Integer; out ErrorCount: Integer; 
                                           var OwnerData: OleVariant): OleVariant;
begin
  Result := DefaultInterface.AS_ApplyUpdates(ProviderName, Delta, MaxErrors, ErrorCount, OwnerData);
end;

function  TNCOCI8MidasTest.AS_GetRecords(const ProviderName: WideString; Count: Integer; 
                                         out RecsOut: Integer; Options: Integer; 
                                         const CommandText: WideString; var Params: OleVariant; 
                                         var OwnerData: OleVariant): OleVariant;
begin
  Result := DefaultInterface.AS_GetRecords(ProviderName, Count, RecsOut, Options, CommandText, 
                                           Params, OwnerData);
end;

function  TNCOCI8MidasTest.AS_DataRequest(const ProviderName: WideString; Data: OleVariant): OleVariant;
begin
  Result := DefaultInterface.AS_DataRequest(ProviderName, Data);
end;

function  TNCOCI8MidasTest.AS_GetProviderNames: OleVariant;
begin
  Result := DefaultInterface.AS_GetProviderNames;
end;

function  TNCOCI8MidasTest.AS_GetParams(const ProviderName: WideString; var OwnerData: OleVariant): OleVariant;
begin
  Result := DefaultInterface.AS_GetParams(ProviderName, OwnerData);
end;

function  TNCOCI8MidasTest.AS_RowRequest(const ProviderName: WideString; Row: OleVariant; 
                                         RequestType: Integer; var OwnerData: OleVariant): OleVariant;
begin
  Result := DefaultInterface.AS_RowRequest(ProviderName, Row, RequestType, OwnerData);
end;

procedure TNCOCI8MidasTest.AS_Execute(const ProviderName: WideString; 
                                      const CommandText: WideString; var Params: OleVariant; 
                                      var OwnerData: OleVariant);
begin
  DefaultInterface.AS_Execute(ProviderName, CommandText, Params, OwnerData);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNCOCI8MidasTestProperties.Create(AServer: TNCOCI8MidasTest);
begin
  inherited Create;
  FServer := AServer;
end;

function TNCOCI8MidasTestProperties.GetDefaultInterface: INCOCI8MidasTest;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Servers',[TNCOCI8MidasTest]);
end;

end.
