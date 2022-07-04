{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Include file                                  }
{                                                       }
{         Copyright (c) 1997, 1998                      }
{                                                       }
{*******************************************************}

interface

uses Windows, ActiveX, Classes, StdVCL;

const
  LIBID_vgI:  TGUID      = '{36252182-F349-11D2-B616-4854E828D2EB}';

  LIBID_Midas: TGUID    = '{83F57D68-CA9A-11D2-9088-00C04FA35CFA}';
  IID_IAppServer: TGUID = '{1AEFCC20-7A24-11D2-98B0-C69BEB4B5B6D}';

  LIBDLL_vgI0           = 'VGI0.DLL';
  LIBDLL_vgI4           = 'VGI4.DLL';
  LIBDLL_vgI5           = 'VGI5.DLL';

{$IFDEF _D4_}
  {$IFDEF _D5_}
  LIBDLL_vgI        = LIBDLL_vgI5;
  LIBVER_vgI        = 5;
  LIBVER_STDVCL     = 4;
  {$ELSE}
  LIBDLL_vgI        = LIBDLL_vgI4;
  LIBVER_vgI        = 4;
  LIBVER_STDVCL     = 4;
  {$ENDIF}
{$ELSE}
  LIBDLL_vgI        = LIBDLL_vgI0;
  LIBVER_vgI        = 1;
  LIBVER_STDVCL     = 1;
{$ENDIF}

const

{ TxTransIsolation }

  tiDirtyRead = 0;
  tiReadCommited = 1;
  tiRepeatableRead = 2;

const

{ Component class GUIDs }
  Class_vgRemoteDataModule: TGUID = '{FB5F6406-DE19-11D2-B611-4854E828D2EB}';

type

{ Forward declarations: Interfaces }
  IvgRemoteDataModule = interface;
  IvgRemoteDataModuleDisp = dispinterface;
  IDatabase = interface;
  IDatabaseDisp = dispinterface;
  IvgDataSet = interface;
  IvgDataSetDisp = dispinterface;

{ Forward declarations: CoClasses }
  vgRemoteDataModule = IvgRemoteDataModule;

{$IFDEF _D4_}
  TOleEnum = type Integer;
{$ELSE}
  TOleEnum = type Smallint;
{$ENDIF}

{ Forward declarations: Enums }
  TxTransIsolation = TOleEnum;

  IvgRemoteDataModule = interface(IDataBroker)
    ['{36252183-F349-11D2-B616-4854E828D2EC}']
    procedure Idle; safecall;
    procedure Login(const Login, Password: WideString); safecall;
    procedure Logout; safecall;
    function CloneProvider(const ProviderName: WideString): WideString; safecall;
    function CreateProvider(Args: OleVariant): WideString; safecall;
    procedure DestroyProvider(const ProviderName: WideString); safecall;
    function GetProvider(const ProviderName: WideString): IProvider; safecall;
    function GetDatabase(const ADatabaseName: WideString): OleVariant; safecall;
    function GetDatabaseNames: OleVariant; safecall;
    function AppInfo(const ParamName: WideString): OleVariant; safecall;
    function Get_Logged: WordBool; safecall;
    property Logged: WordBool read Get_Logged;
  end;

{ DispInterface declaration for Dual Interface IvgRemoteDataModule }

  IvgRemoteDataModuleDisp = dispinterface
    ['{36252183-F349-11D2-B616-4854E828D2EC}']
    function GetProviderNames: OleVariant; dispid 22929905;
    procedure Idle; dispid 30300;
    procedure Login(const Login, Password: WideString); dispid 30301;
    procedure Logout; dispid 30302;
    function CloneProvider(const ProviderName: WideString): WideString; dispid 30303;
    function CreateProvider(Args: OleVariant): WideString; dispid 30304;
    procedure DestroyProvider(const ProviderName: WideString); dispid 30305;
    function GetProvider(const ProviderName: WideString): IProvider; dispid 30306;
    function GetDatabase(const ADatabaseName: WideString): OleVariant; dispid 30900;
    function GetDatabaseNames: OleVariant; dispid 30901;
    function AppInfo(const ParamName: WideString): OleVariant; dispid 30902;
    property Logged: WordBool readonly dispid 40301;
  end;

  IDatabase = interface(IDispatch)
    ['{A607E711-DC71-11D2-B611-4854E828D2EB}']
    procedure StartTransaction; safecall;
    procedure Commit; safecall;
    procedure Rollback; safecall;
    function GetTableNames(const Pattern: WideString; Extensions, SystemTables: WordBool): OleVariant; safecall;
    function GetStoredProcNames: OleVariant; safecall;
    function CreateTableProvider(const AName: WideString; Params: OleVariant): IProvider; safecall;
    function CreateQueryProvider(const AName: WideString; Params: OleVariant): IProvider; safecall;
    function CreateStoredProcProvider(const AName: WideString; Params: OleVariant): IProvider; safecall;
    procedure DestroyProvider(const AName: WideString); safecall;
    function UniqueName: WideString; safecall;
    function GetProvider(const AName: WideString): IProvider; safecall;
    function DataRequest(Input: OleVariant): OleVariant; safecall;
    function Get_Connected: WordBool; safecall;
    procedure Set_Connected(Value: WordBool); safecall;
    function Get_InTransaction: WordBool; safecall;
    function Get_Params: OleVariant; safecall;
    procedure Set_Params(Value: OleVariant); safecall;
    function Get_TransIsolation: TxTransIsolation; safecall;
    procedure Set_TransIsolation(Value: TxTransIsolation); safecall;
    function Get_IsSQLBased: WordBool; safecall;
    function Get_QuoteChar: WideString; safecall;
    property Connected: WordBool read Get_Connected write Set_Connected;
    property InTransaction: WordBool read Get_InTransaction;
    property Params: OleVariant read Get_Params write Set_Params;
    property TransIsolation: TxTransIsolation read Get_TransIsolation write Set_TransIsolation;
    property IsSQLBased: WordBool read Get_IsSQLBased;
    property QuoteChar: WideString read Get_QuoteChar;
  end;

{ DispInterface declaration for Dual Interface IDatabase }

  IDatabaseDisp = dispinterface
    ['{A607E711-DC71-11D2-B611-4854E828D2EB}']
    procedure StartTransaction; dispid 30900;
    procedure Commit; dispid 30901;
    procedure Rollback; dispid 30902;
    function GetTableNames(const Pattern: WideString; Extensions, SystemTables: WordBool): OleVariant; dispid 30903;
    function GetStoredProcNames: OleVariant; dispid 30904;
    function CreateTableProvider(const AName: WideString; Params: OleVariant): IProvider; dispid 30905;
    function CreateQueryProvider(const AName: WideString; Params: OleVariant): IProvider; dispid 30906;
    function CreateStoredProcProvider(const AName: WideString; Params: OleVariant): IProvider; dispid 30907;
    procedure DestroyProvider(const AName: WideString); dispid 30920;
    function UniqueName: WideString; dispid 30921;
    function GetProvider(const AName: WideString): IProvider; dispid 30922;
    function DataRequest(Input: OleVariant): OleVariant; dispid 30923;
    property Connected: WordBool dispid 41000;
    property InTransaction: WordBool readonly dispid 41001;
    property Params: OleVariant dispid 41002;
    property TransIsolation: TxTransIsolation dispid 41003;
    property IsSQLBased: WordBool readonly dispid 41004;
    property QuoteChar: WideString readonly dispid 41005;
  end;

  IvgDataSet = interface(IDispatch)
    ['{EB04E2B6-BF49-11D2-B60C-4854E828D2EB}']
    procedure First; safecall;
    procedure Last; safecall;
    procedure MoveBy(Offset: Integer); safecall;
    function BOF: WordBool; safecall;
    function EOF: WordBool; safecall;
    procedure Open; safecall;
    procedure Close; safecall;
    function Execute: Integer; safecall;
    function Locate(const FieldNames: WideString; FieldValues: OleVariant; CaseIns, PartialKey: WordBool): WordBool; safecall;
    procedure Insert; safecall;
    procedure Append; safecall;
    procedure Edit; safecall;
    procedure Post; safecall;
    procedure Cancel; safecall;
    procedure Delete; safecall;
    function GetFieldValue(const FieldName: WideString): OleVariant; safecall;
    procedure SetFieldValue(const FieldName: WideString; Value: OleVariant); safecall;
    function GetParamValue(const ParamName: WideString): OleVariant; safecall;
    procedure SetParamValue(const ParamName: WideString; Value: OleVariant); safecall;
    function Get_Active: WordBool; safecall;
    procedure Set_Active(Value: WordBool); safecall;
    function Get_Empty: WordBool; safecall;
    property Active: WordBool read Get_Active write Set_Active;
    property Empty: WordBool read Get_Empty;
  end;

{ DispInterface declaration for Dual Interface IvgDataSet }

  IvgDataSetDisp = dispinterface
    ['{EB04E2B6-BF49-11D2-B60C-4854E828D2EB}']
    procedure First; dispid 30800;
    procedure Last; dispid 30801;
    procedure MoveBy(Offset: Integer); dispid 30802;
    function BOF: WordBool; dispid 30803;
    function EOF: WordBool; dispid 30804;
    procedure Open; dispid 30805;
    procedure Close; dispid 30806;
    function Execute: Integer; dispid 30807;
    function Locate(const FieldNames: WideString; FieldValues: OleVariant; CaseIns, PartialKey: WordBool): WordBool; dispid 30808;
    procedure Insert; dispid 30809;
    procedure Append; dispid 30810;
    procedure Edit; dispid 30811;
    procedure Post; dispid 30812;
    procedure Cancel; dispid 30813;
    procedure Delete; dispid 30814;
    function GetFieldValue(const FieldName: WideString): OleVariant; dispid 30815;
    procedure SetFieldValue(const FieldName: WideString; Value: OleVariant); dispid 30816;
    function GetParamValue(const ParamName: WideString): OleVariant; dispid 30817;
    procedure SetParamValue(const ParamName: WideString; Value: OleVariant); dispid 30818;
    property Active: WordBool dispid 40823;
    property Empty: WordBool readonly dispid 40824;
  end;

  IAppServer = interface(IDispatch)
    ['{1AEFCC20-7A24-11D2-98B0-C69BEB4B5B6D}']
    function  AS_ApplyUpdates(const ProviderName: WideString; Delta: OleVariant;
                              MaxErrors: Integer; out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; safecall;
    function  AS_GetRecords(const ProviderName: WideString; Count: Integer; out RecsOut: Integer;
                            Options: Integer; const CommandText: WideString;
                            var Params: OleVariant; var OwnerData: OleVariant): OleVariant; safecall;
    function  AS_DataRequest(const ProviderName: WideString; Data: OleVariant): OleVariant; safecall;
    function  AS_GetProviderNames: OleVariant; safecall;
    function  AS_GetParams(const ProviderName: WideString; var OwnerData: OleVariant): OleVariant; safecall;
    function  AS_RowRequest(const ProviderName: WideString; Row: OleVariant; RequestType: Integer;
                            var OwnerData: OleVariant): OleVariant; safecall;
    procedure AS_Execute(const ProviderName: WideString; const CommandText: WideString;
                         var Params: OleVariant; var OwnerData: OleVariant); safecall;
  end;

{ DispInterface declaration for Dual Interface IAppServer }

  IAppServerDisp = dispinterface
    ['{1AEFCC20-7A24-11D2-98B0-C69BEB4B5B6D}']
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

  IvgAppServer = interface(IAppServer)
    ['{80D4D15C-77D8-11D3-B86D-00A0C993B292}']
    function  GetProviderNames: OleVariant; safecall;
    procedure Idle; safecall;
    procedure Login(const Login: WideString; const Password: WideString); safecall;
    procedure Logout; safecall;
    function  CloneProvider(const ProviderName: WideString): WideString; safecall;
    function  CreateProvider(Args: OleVariant): WideString; safecall;
    procedure DestroyProvider(const ProviderName: WideString); safecall;
    function  GetProvider(const ProviderName: WideString): IProvider; safecall;
    function  GetDatabase(const ADatabaseName: WideString): OleVariant; safecall;
    function  GetDatabaseNames: OleVariant; safecall;
    function  AppInfo(const ParamName: WideString): OleVariant; safecall;
    function  Get_Logged: WordBool; safecall;
    property Logged: WordBool read Get_Logged;
  end;

{ DispInterface declaration for Dual Interface IvgAppServer }

  IvgAppServerDisp = dispinterface
    ['{80D4D15C-77D8-11D3-B86D-00A0C993B292}']
    function  GetProviderNames: OleVariant; dispid 22929905;
    procedure Idle; dispid 30300;
    procedure Login(const Login: WideString; const Password: WideString); dispid 30301;
    procedure Logout; dispid 30302;
    function  CloneProvider(const ProviderName: WideString): WideString; dispid 30303;
    function  CreateProvider(Args: OleVariant): WideString; dispid 30304;
    procedure DestroyProvider(const ProviderName: WideString); dispid 30305;
    function  GetProvider(const ProviderName: WideString): IProvider; dispid 30306;
    function  GetDatabase(const ADatabaseName: WideString): OleVariant; dispid 30900;
    function  GetDatabaseNames: OleVariant; dispid 30901;
    function  AppInfo(const ParamName: WideString): OleVariant; dispid 30902;
    property Logged: WordBool readonly dispid 40301;
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

implementation
