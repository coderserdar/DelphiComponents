unit cadoxdb;

{ ************************************************************************ }
{ Microsoft ADO Ext. for DDL and Security (ADOX)                           }
{ Version:    2.5                                                          }
{ ************************************************************************ }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, cadodb;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:      //
//   Type Libraries     : LIBID_xxxx                                    //
//   CoClasses          : CLASS_xxxx                                    //
//   DISPInterfaces     : DIID_xxxx                                     //
//   Non-DISP interfaces: IID_xxxx                                      //
// *********************************************************************//
const
  LIBID_ADOX: TGUID = '{00000600-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXCatalog: TGUID = '{00000603-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXTables: TGUID = '{00000611-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXTable: TGUID = '{00000610-0000-0010-8000-00AA006D2EA4}';
  CLASS_Table: TGUID = '{00000609-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXColumns: TGUID = '{0000061D-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXColumn: TGUID = '{0000061C-0000-0010-8000-00AA006D2EA4}';
  CLASS_Column: TGUID = '{0000061B-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXIndexes: TGUID = '{00000620-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXIndex: TGUID = '{0000061F-0000-0010-8000-00AA006D2EA4}';
  CLASS_Index: TGUID = '{0000061E-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXKeys: TGUID = '{00000623-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXKey: TGUID = '{00000622-0000-0010-8000-00AA006D2EA4}';
  CLASS_Key: TGUID = '{00000621-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXProcedures: TGUID = '{00000626-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXProcedure: TGUID = '{00000625-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXViews: TGUID = '{00000614-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXView: TGUID = '{00000613-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXGroups: TGUID = '{00000617-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXGroup: TGUID = '{00000616-0000-0010-8000-00AA006D2EA4}';
  CLASS_Group: TGUID = '{00000615-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXUsers: TGUID = '{0000061A-0000-0010-8000-00AA006D2EA4}';
  IID_IADOXUser: TGUID = '{00000619-0000-0010-8000-00AA006D2EA4}';
  CLASS_User: TGUID = '{00000618-0000-0010-8000-00AA006D2EA4}';
  CLASS_Catalog: TGUID = '{00000602-0000-0010-8000-00AA006D2EA4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
// ColumnAttributesEnum constants
type
  ColumnAttributesEnum = TOleEnum;
const
  adColFixed = $00000001;
  adColNullable = $00000002;

// SortOrderEnum constants
type
  SortOrderEnum = TOleEnum;
const
  adSortAscending = $00000001;
  adSortDescending = $00000002;

// DataTypeEnum constants
type
  DataTypeEnum = TOleEnum;
const
  adEmpty = $00000000;
  adTinyInt = $00000010;
  adSmallInt = $00000002;
  adInteger = $00000003;
  adBigInt = $00000014;
  adUnsignedTinyInt = $00000011;
  adUnsignedSmallInt = $00000012;
  adUnsignedInt = $00000013;
  adUnsignedBigInt = $00000015;
  adSingle = $00000004;
  adDouble = $00000005;
  adCurrency = $00000006;
  adDecimal = $0000000E;
  adNumeric = $00000083;
  adBoolean = $0000000B;
  adError = $0000000A;
  adUserDefined = $00000084;
  adVariant = $0000000C;
  adIDispatch = $00000009;
  adIUnknown = $0000000D;
  adGUID = $00000048;
  adDate = $00000007;
  adDBDate = $00000085;
  adDBTime = $00000086;
  adDBTimeStamp = $00000087;
  adBSTR = $00000008;
  adChar = $00000081;
  adVarChar = $000000C8;
  adLongVarChar = $000000C9;
  adWChar = $00000082;
  adVarWChar = $000000CA;
  adLongVarWChar = $000000CB;
  adBinary = $00000080;
  adVarBinary = $000000CC;
  adLongVarBinary = $000000CD;
  adChapter = $00000088;
  adFileTime = $00000040;
  adPropVariant = $0000008A;
  adVarNumeric = $0000008B;

// AllowNullsEnum constants
type
  AllowNullsEnum = TOleEnum;
const
  adIndexNullsAllow = $00000000;
  adIndexNullsDisallow = $00000001;
  adIndexNullsIgnore = $00000002;
  adIndexNullsIgnoreAny = $00000004;

// RuleEnum constants
type
  RuleEnum = TOleEnum;
const
  adRINone = $00000000;
  adRICascade = $00000001;
  adRISetNull = $00000002;
  adRISetDefault = $00000003;

// KeyTypeEnum constants
type
  KeyTypeEnum = TOleEnum;
const
  adKeyPrimary = $00000001;
  adKeyForeign = $00000002;
  adKeyUnique = $00000003;

// ObjectTypeEnum constants
type
  ObjectTypeEnum = TOleEnum;
const
  adPermObjProviderSpecific = $FFFFFFFF;
  adPermObjTable = $00000001;
  adPermObjColumn = $00000002;
  adPermObjDatabase = $00000003;
  adPermObjProcedure = $00000004;
  adPermObjView = $00000005;

// RightsEnum constants
type
  RightsEnum = TOleEnum;
const
  adRightNone = $00000000;
  adRightDrop = $00000100;
  adRightExclusive = $00000200;
  adRightReadDesign = $00000400;
  adRightWriteDesign = $00000800;
  adRightWithGrant = $00001000;
  adRightReference = $00002000;
  adRightCreate = $00004000;
  adRightInsert = $00008000;
  adRightDelete = $00010000;
  adRightReadPermissions = $00020000;
  adRightWritePermissions = $00040000;
  adRightWriteOwner = $00080000;
  adRightMaximumAllowed = $02000000;
  adRightFull = $10000000;
  adRightExecute = $20000000;
  adRightUpdate = $40000000;
  adRightRead = $80000000;

// ActionEnum constants
type
  ActionEnum = TOleEnum;
const
  adAccessGrant = $00000001;
  adAccessSet = $00000002;
  adAccessDeny = $00000003;
  adAccessRevoke = $00000004;

// InheritTypeEnum constants
type
  InheritTypeEnum = TOleEnum;
const
  adInheritNone = $00000000;
  adInheritObjects = $00000001;
  adInheritContainers = $00000002;
  adInheritBoth = $00000003;
  adInheritNoPropogate = $00000004;

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IADOXCatalog = interface;
  IADOXCatalogDisp = dispinterface;
  IADOXTables = interface;
  IADOXTablesDisp = dispinterface;
  IADOXTable = interface;
  IADOXTableDisp = dispinterface;
  IADOXColumns = interface;
  IADOXColumnsDisp = dispinterface;
  IADOXColumn = interface;
  IADOXColumnDisp = dispinterface;
  IADOXIndexes = interface;
  IADOXIndexesDisp = dispinterface;
  IADOXIndex = interface;
  IADOXIndexDisp = dispinterface;
  IADOXKeys = interface;
  IADOXKeysDisp = dispinterface;
  IADOXKey = interface;
  IADOXKeyDisp = dispinterface;
  IADOXProcedures = interface;
  IADOXProceduresDisp = dispinterface;
  IADOXProcedure = interface;
  IADOXProcedureDisp = dispinterface;
  IADOXViews = interface;
  IADOXViewsDisp = dispinterface;
  IADOXView = interface;
  IADOXViewDisp = dispinterface;
  IADOXGroups = interface;
  IADOXGroupsDisp = dispinterface;
  IADOXGroup = interface;
  IADOXGroupDisp = dispinterface;
  IADOXUsers = interface;
  IADOXUsersDisp = dispinterface;
  IADOXUser = interface;
  IADOXUserDisp = dispinterface;


// *********************************************************************//
// Interface: IADOXCatalog
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000603-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXCatalog = interface(IDispatch)
    ['{00000603-0000-0010-8000-00AA006D2EA4}']
    function Get_Tables: IADOXTables; safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    procedure _Set_ActiveConnection(pVal: OleVariant); safecall;
    procedure Set_ActiveConnection(const pVal: IDispatch); safecall;
    function Get_Procedures: IADOXProcedures; safecall;
    function Get_Views: IADOXViews; safecall;
    function Get_Groups: IADOXGroups; safecall;
    function Get_Users: IADOXUsers; safecall;
    function Create(const ConnectString: WideString): OleVariant; safecall;
    function GetObjectOwner(const ObjectName: WideString; ObjectType: ObjectTypeEnum; 
                            ObjectTypeId: OleVariant): WideString; safecall;
    procedure SetObjectOwner(const ObjectName: WideString; ObjectType: ObjectTypeEnum; 
                             const UserName: WideString; ObjectTypeId: OleVariant); safecall;
    property Tables: IADOXTables read Get_Tables;
    property Procedures: IADOXProcedures read Get_Procedures;
    property Views: IADOXViews read Get_Views;
    property Groups: IADOXGroups read Get_Groups;
    property Users: IADOXUsers read Get_Users;
  end;

// *********************************************************************//
// DispIntf:  IADOXCatalogDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000603-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXCatalogDisp = dispinterface
    ['{00000603-0000-0010-8000-00AA006D2EA4}']
    property Tables: IADOXTables readonly dispid 0;
    function ActiveConnection: OleVariant; dispid 1;
    property Procedures: IADOXProcedures readonly dispid 2;
    property Views: IADOXViews readonly dispid 3;
    property Groups: IADOXGroups readonly dispid 4;
    property Users: IADOXUsers readonly dispid 5;
    function Create(const ConnectString: WideString): OleVariant; dispid 6;
    function GetObjectOwner(const ObjectName: WideString; ObjectType: ObjectTypeEnum;
                            ObjectTypeId: OleVariant): WideString; dispid 7;
    procedure SetObjectOwner(const ObjectName: WideString; ObjectType: ObjectTypeEnum;
                             const UserName: WideString; ObjectTypeId: OleVariant); dispid 8;
  end;

// *********************************************************************//
// Interface: IADOXTables
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000611-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXTables = interface(IADOCollection)
    ['{00000611-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXTable; safecall;
    procedure Append(Item: OleVariant); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXTable read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXTablesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000611-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXTablesDisp = dispinterface
    ['{00000611-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXTable readonly dispid 0; default;
    procedure Append(Item: OleVariant); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXTable
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000610-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXTable = interface(IDispatch)
    ['{00000610-0000-0010-8000-00AA006D2EA4}']
    function Get_Columns: IADOXColumns; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function Get_Type_: WideString; safecall;
    function Get_Indexes: IADOXIndexes; safecall;
    function Get_Keys: IADOXKeys; safecall;
    function Get_Properties: IADOProperties; safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_DateModified: OleVariant; safecall;
    function Get_ParentCatalog: IADOXCatalog; safecall;
    procedure _Set_ParentCatalog(const ppvObject: IADOXCatalog); safecall;
    procedure Set_ParentCatalog(const ppvObject: IADOXCatalog); safecall;
    property Columns: IADOXColumns read Get_Columns;
    property Name: WideString read Get_Name write Set_Name;
    property Type_: WideString read Get_Type_;
    property Indexes: IADOXIndexes read Get_Indexes;
    property Keys: IADOXKeys read Get_Keys;
    property Properties: IADOProperties read Get_Properties;
    property DateCreated: OleVariant read Get_DateCreated;
    property DateModified: OleVariant read Get_DateModified;
    property ParentCatalog: IADOXCatalog read Get_ParentCatalog write _Set_ParentCatalog;
  end;

// *********************************************************************//
// DispIntf:  IADOXTableDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000610-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXTableDisp = dispinterface
    ['{00000610-0000-0010-8000-00AA006D2EA4}']
    property Columns: IADOXColumns readonly dispid 0;
    property Name: WideString dispid 1;
    property Type_: WideString readonly dispid 2;
    property Indexes: IADOXIndexes readonly dispid 3;
    property Keys: IADOXKeys readonly dispid 4;
    property Properties: IADOProperties readonly dispid 5;
    property DateCreated: OleVariant readonly dispid 6;
    property DateModified: OleVariant readonly dispid 7;
    property ParentCatalog: IADOXCatalog dispid 8;
  end;

// *********************************************************************//
// Interface: IADOXColumns
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXColumns = interface(IADOCollection)
    ['{0000061D-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXColumn; safecall;
    procedure Append(Item: OleVariant; Type_: DataTypeEnum; DefinedSize: Integer); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXColumn read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXColumnsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061D-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXColumnsDisp = dispinterface
    ['{0000061D-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXColumn readonly dispid 0; default;
    procedure Append(Item: OleVariant; Type_: DataTypeEnum; DefinedSize: Integer); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXColumn
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXColumn = interface(IDispatch)
    ['{0000061C-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function Get_Attributes: ColumnAttributesEnum; safecall;
    procedure Set_Attributes(pVal: ColumnAttributesEnum); safecall;
    function Get_DefinedSize: Integer; safecall;
    procedure Set_DefinedSize(pVal: Integer); safecall;
    function Get_NumericScale: Byte; safecall;
    procedure Set_NumericScale(pVal: Byte); safecall;
    function Get_Precision: Integer; safecall;
    procedure Set_Precision(pVal: Integer); safecall;
    function Get_RelatedColumn: WideString; safecall;
    procedure Set_RelatedColumn(const pVal: WideString); safecall;
    function Get_SortOrder: SortOrderEnum; safecall;
    procedure Set_SortOrder(pVal: SortOrderEnum); safecall;
    function Get_Type_: DataTypeEnum; safecall;
    procedure Set_Type_(pVal: DataTypeEnum); safecall;
    function Get_Properties: IADOProperties; safecall;
    function Get_ParentCatalog: IADOXCatalog; safecall;
    procedure _Set_ParentCatalog(const ppvObject: IADOXCatalog); safecall;
    procedure Set_ParentCatalog(const ppvObject: IADOXCatalog); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Attributes: ColumnAttributesEnum read Get_Attributes write Set_Attributes;
    property DefinedSize: Integer read Get_DefinedSize write Set_DefinedSize;
    property NumericScale: Byte read Get_NumericScale write Set_NumericScale;
    property Precision: Integer read Get_Precision write Set_Precision;
    property RelatedColumn: WideString read Get_RelatedColumn write Set_RelatedColumn;
    property SortOrder: SortOrderEnum read Get_SortOrder write Set_SortOrder;
    property Type_: DataTypeEnum read Get_Type_ write Set_Type_;
    property Properties: IADOProperties read Get_Properties;
    property ParentCatalog: IADOXCatalog read Get_ParentCatalog write _Set_ParentCatalog;
  end;

// *********************************************************************//
// DispIntf:  IADOXColumnDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061C-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXColumnDisp = dispinterface
    ['{0000061C-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 0;
    property Attributes: ColumnAttributesEnum dispid 1;
    property DefinedSize: Integer dispid 3;
    property NumericScale: Byte dispid 4;
    property Precision: Integer dispid 5;
    property RelatedColumn: WideString dispid 6;
    property SortOrder: SortOrderEnum dispid 7;
    property Type_: DataTypeEnum dispid 8;
    property Properties: IADOProperties readonly dispid 9;
    property ParentCatalog: IADOXCatalog dispid 10;
  end;

// *********************************************************************//
// Interface: IADOXIndexes
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000620-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXIndexes = interface(IADOCollection)
    ['{00000620-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXIndex; safecall;
    procedure Append(Item: OleVariant; Columns: OleVariant); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXIndex read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXIndexesDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000620-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXIndexesDisp = dispinterface
    ['{00000620-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXIndex readonly dispid 0; default;
    procedure Append(Item: OleVariant; Columns: OleVariant); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXIndex
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXIndex = interface(IDispatch)
    ['{0000061F-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function Get_Clustered: WordBool; safecall;
    procedure Set_Clustered(pVal: WordBool); safecall;
    function Get_IndexNulls: AllowNullsEnum; safecall;
    procedure Set_IndexNulls(pVal: AllowNullsEnum); safecall;
    function Get_PrimaryKey: WordBool; safecall;
    procedure Set_PrimaryKey(pVal: WordBool); safecall;
    function Get_Unique: WordBool; safecall;
    procedure Set_Unique(pVal: WordBool); safecall;
    function Get_Columns: IADOXColumns; safecall;
    function Get_Properties: IADOProperties; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Clustered: WordBool read Get_Clustered write Set_Clustered;
    property IndexNulls: AllowNullsEnum read Get_IndexNulls write Set_IndexNulls;
    property PrimaryKey: WordBool read Get_PrimaryKey write Set_PrimaryKey;
    property Unique: WordBool read Get_Unique write Set_Unique;
    property Columns: IADOXColumns read Get_Columns;
    property Properties: IADOProperties read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  IADOXIndexDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061F-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXIndexDisp = dispinterface
    ['{0000061F-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 0;
    property Clustered: WordBool dispid 1;
    property IndexNulls: AllowNullsEnum dispid 2;
    property PrimaryKey: WordBool dispid 3;
    property Unique: WordBool dispid 4;
    property Columns: IADOXColumns readonly dispid 5;
    property Properties: IADOProperties readonly dispid 6;
  end;

// *********************************************************************//
// Interface: IADOXKeys
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000623-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXKeys = interface(IADOCollection)
    ['{00000623-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXKey; safecall;
    procedure Append(Item: OleVariant; Type_: KeyTypeEnum; Column: OleVariant;
                     const RelatedTable: WideString; const RelatedColumn: WideString); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXKey read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXKeysDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000623-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXKeysDisp = dispinterface
    ['{00000623-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXKey readonly dispid 0; default;
    procedure Append(Item: OleVariant; Type_: KeyTypeEnum; Column: OleVariant;
                     const RelatedTable: WideString; const RelatedColumn: WideString); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXKey
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000622-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXKey = interface(IDispatch)
    ['{00000622-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function Get_DeleteRule: RuleEnum; safecall;
    procedure Set_DeleteRule(pVal: RuleEnum); safecall;
    function Get_Type_: KeyTypeEnum; safecall;
    procedure Set_Type_(pVal: KeyTypeEnum); safecall;
    function Get_RelatedTable: WideString; safecall;
    procedure Set_RelatedTable(const pVal: WideString); safecall;
    function Get_UpdateRule: RuleEnum; safecall;
    procedure Set_UpdateRule(pVal: RuleEnum); safecall;
    function Get_Columns: IADOXColumns; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property DeleteRule: RuleEnum read Get_DeleteRule write Set_DeleteRule;
    property Type_: KeyTypeEnum read Get_Type_ write Set_Type_;
    property RelatedTable: WideString read Get_RelatedTable write Set_RelatedTable;
    property UpdateRule: RuleEnum read Get_UpdateRule write Set_UpdateRule;
    property Columns: IADOXColumns read Get_Columns;
  end;

// *********************************************************************//
// DispIntf:  IADOXKeyDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000622-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXKeyDisp = dispinterface
    ['{00000622-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 0;
    property DeleteRule: RuleEnum dispid 1;
    property Type_: KeyTypeEnum dispid 2;
    property RelatedTable: WideString dispid 3;
    property UpdateRule: RuleEnum dispid 4;
    property Columns: IADOXColumns readonly dispid 5;
  end;

// *********************************************************************//
// Interface: IADOXProcedures
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000626-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXProcedures = interface(IADOCollection)
    ['{00000626-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXProcedure; safecall;
    procedure Append(const Name: WideString; const Command: IDispatch); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXProcedure read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXProceduresDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000626-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXProceduresDisp = dispinterface
    ['{00000626-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXProcedure readonly dispid 0; default;
    procedure Append(const Name: WideString; const Command: IDispatch); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXProcedure
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000625-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXProcedure = interface(IDispatch)
    ['{00000625-0000-0010-8000-00AA006D2EA4}']
    function Get_Command: OleVariant; safecall;
    procedure _Set_Command(pVar: OleVariant); safecall;
    procedure Set_Command(const pVar: IDispatch); safecall;
    function Get_Name: WideString; safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_DateModified: OleVariant; safecall;
    property Name: WideString read Get_Name;
    property DateCreated: OleVariant read Get_DateCreated;
    property DateModified: OleVariant read Get_DateModified;
  end;

// *********************************************************************//
// DispIntf:  IADOXProcedureDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000625-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXProcedureDisp = dispinterface
    ['{00000625-0000-0010-8000-00AA006D2EA4}']
    function Command: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property DateCreated: OleVariant readonly dispid 2;
    property DateModified: OleVariant readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IADOXViews
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000614-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXViews = interface(IADOCollection)
    ['{00000614-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXView; safecall;
    procedure Append(const Name: WideString; const Command: IDispatch); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXView read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXViewsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000614-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXViewsDisp = dispinterface
    ['{00000614-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXView readonly dispid 0; default;
    procedure Append(const Name: WideString; const Command: IDispatch); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXView
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000613-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXView = interface(IDispatch)
    ['{00000613-0000-0010-8000-00AA006D2EA4}']
    function Get_Command: OleVariant; safecall;
    procedure _Set_Command(pVal: OleVariant); safecall;
    procedure Set_Command(const pVal: IDispatch); safecall;
    function Get_Name: WideString; safecall;
    function Get_DateCreated: OleVariant; safecall;
    function Get_DateModified: OleVariant; safecall;
    property Name: WideString read Get_Name;
    property DateCreated: OleVariant read Get_DateCreated;
    property DateModified: OleVariant read Get_DateModified;
  end;

// *********************************************************************//
// DispIntf:  IADOXViewDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000613-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXViewDisp = dispinterface
    ['{00000613-0000-0010-8000-00AA006D2EA4}']
    function Command: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property DateCreated: OleVariant readonly dispid 2;
    property DateModified: OleVariant readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IADOXGroups
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000617-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXGroups = interface(IADOCollection)
    ['{00000617-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXGroup; safecall;
    procedure Append(Item: OleVariant); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXGroup read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXGroupsDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000617-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXGroupsDisp = dispinterface
    ['{00000617-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXGroup readonly dispid 0; default;
    procedure Append(Item: OleVariant); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: _Group
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000616-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXGroup = interface(IDispatch)
    ['{00000616-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function GetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; ObjectTypeId: OleVariant): RightsEnum; safecall;
    procedure SetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; Action: ActionEnum;
                             Rights: RightsEnum; Inherit: InheritTypeEnum; ObjectTypeId: OleVariant); safecall;
    function Get_Users: IADOXUsers; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Users: IADOXUsers read Get_Users;
  end;

// *********************************************************************//
// DispIntf:  IADOXGroupDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000616-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXGroupDisp = dispinterface
    ['{00000616-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 0;
    function GetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; ObjectTypeId: OleVariant): RightsEnum; dispid 2;
    procedure SetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; Action: ActionEnum;
                             Rights: RightsEnum; Inherit: InheritTypeEnum; ObjectTypeId: OleVariant); dispid 3;
    property Users: IADOXUsers readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IADOXUsers
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061A-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXUsers = interface(IADOCollection)
    ['{0000061A-0000-0010-8000-00AA006D2EA4}']
    function Get_Item(Item: OleVariant): IADOXUser; safecall;
    procedure Append(Item: OleVariant; const Password: WideString); safecall;
    procedure Delete(Item: OleVariant); safecall;
    property Item[Item: OleVariant]: IADOXUser read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  IADOXUsersDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {0000061A-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXUsersDisp = dispinterface
    ['{0000061A-0000-0010-8000-00AA006D2EA4}']
    property Item[Item: OleVariant]: IADOXUser readonly dispid 0; default;
    procedure Append(Item: OleVariant; const Password: WideString); dispid 1610809345;
    procedure Delete(Item: OleVariant); dispid 1610809346;
    property Count: Integer readonly dispid 1610743808;
    function _NewEnum: IUnknown; dispid -4;
    procedure Refresh; dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IADOXUser
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000619-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXUser = interface(IDispatch)
    ['{00000619-0000-0010-8000-00AA006D2EA4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const pVal: WideString); safecall;
    function GetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; ObjectTypeId: OleVariant): RightsEnum; safecall;
    procedure SetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; Action: ActionEnum;
                             Rights: RightsEnum; Inherit: InheritTypeEnum; ObjectTypeId: OleVariant); safecall;
    procedure ChangePassword(const OldPassword: WideString; const NewPassord: WideString); safecall;
    function Get_Groups: IADOXGroups; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Groups: IADOXGroups read Get_Groups;
  end;

// *********************************************************************//
// DispIntf:  IADOXUserDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {00000619-0000-0010-8000-00AA006D2EA4}
// *********************************************************************//
  IADOXUserDisp = dispinterface
    ['{00000619-0000-0010-8000-00AA006D2EA4}']
    property Name: WideString dispid 0;
    function GetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; ObjectTypeId: OleVariant): RightsEnum; dispid 2;
    procedure SetPermissions(Name: OleVariant; ObjectType: ObjectTypeEnum; Action: ActionEnum;
                             Rights: RightsEnum; Inherit: InheritTypeEnum; ObjectTypeId: OleVariant); dispid 3;
    procedure ChangePassword(const OldPassword: WideString; const NewPassord: WideString); dispid 4;
    property Groups: IADOXGroups readonly dispid 5;
  end;

  CoTable = class
    class function Create: IADOXTable;
    class function CreateRemote(const MachineName: string): IADOXTable;
  end;

  CoColumn = class
    class function Create: IADOXColumn;
    class function CreateRemote(const MachineName: string): IADOXColumn;
  end;

  CoIndex = class
    class function Create: IADOXIndex;
    class function CreateRemote(const MachineName: string): IADOXIndex;
  end;

  CoKey = class
    class function Create: IADOXKey;
    class function CreateRemote(const MachineName: string): IADOXKey;
  end;

  CoGroup = class
    class function Create: IADOXGroup;
    class function CreateRemote(const MachineName: string): IADOXGroup;
  end;

  CoUser = class
    class function Create: IADOXUser;
    class function CreateRemote(const MachineName: string): IADOXUser;
  end;

  CoCatalog = class
    class function Create: IADOXCatalog;
    class function CreateRemote(const MachineName: string): IADOXCatalog;
  end;

implementation

uses ComObj;

class function CoTable.Create: IADOXTable;
begin
  Result := CreateComObject(CLASS_Table) as IADOXTable;
end;

class function CoTable.CreateRemote(const MachineName: string): IADOXTable;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Table) as IADOXTable;
end;

class function CoColumn.Create: IADOXColumn;
begin
  Result := CreateComObject(CLASS_Column) as IADOXColumn;
end;

class function CoColumn.CreateRemote(const MachineName: string): IADOXColumn;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Column) as IADOXColumn;
end;

class function CoIndex.Create: IADOXIndex;
begin
  Result := CreateComObject(CLASS_Index) as IADOXIndex;
end;

class function CoIndex.CreateRemote(const MachineName: string): IADOXIndex;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Index) as IADOXIndex;
end;

class function CoKey.Create: IADOXKey;
begin
  Result := CreateComObject(CLASS_Key) as IADOXKey;
end;

class function CoKey.CreateRemote(const MachineName: string): IADOXKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Key) as IADOXKey;
end;

class function CoGroup.Create: IADOXGroup;
begin
  Result := CreateComObject(CLASS_Group) as IADOXGroup;
end;

class function CoGroup.CreateRemote(const MachineName: string): IADOXGroup;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Group) as IADOXGroup;
end;

class function CoUser.Create: IADOXUser;
begin
  Result := CreateComObject(CLASS_User) as IADOXUser;
end;

class function CoUser.CreateRemote(const MachineName: string): IADOXUser;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_User) as IADOXUser;
end;

class function CoCatalog.Create: IADOXCatalog;
begin
  Result := CreateComObject(CLASS_Catalog) as IADOXCatalog;
end;

class function CoCatalog.CreateRemote(const MachineName: string): IADOXCatalog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Catalog) as IADOXCatalog;
end;

end.
