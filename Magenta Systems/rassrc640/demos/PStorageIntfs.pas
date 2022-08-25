unit PStorageIntfs;

// http://www.cobans.net/pslib.php

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

// PASTLWTR : $Revision:   1.130  $
// File generated on 02.07.2003 23:17:55 from Type Library described below.

// ************************************************************************  //
// Type Lib: pstorec.dll (1)
// LIBID: {5A6F1EBD-2DB1-11D0-8C39-00C04FD9126B}
// LCID: 0
// Helpfile:
// DepndLst:
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Error creating palette bitmap of (TCPStore) : Server C:\pstorec.dll contains no icons
//   Error creating palette bitmap of (TCEnumTypes) : Server C:\pstorec.dll contains no icons
//   Error creating palette bitmap of (TCEnumItems) : Server C:\pstorec.dll contains no icons
// ************************************************************************ //
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
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
//{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  PSTORECLibMajorVersion = 1;
  PSTORECLibMinorVersion = 0;

  LIBID_PSTORECLib: TGUID = '{5A6F1EBD-2DB1-11D0-8C39-00C04FD9126B}';

  IID_IEnumPStoreProviders: TGUID = '{5A6F1EBF-2DB1-11D0-8C39-00C04FD9126B}';
  IID_IPStore: TGUID = '{5A6F1EC0-2DB1-11D0-8C39-00C04FD9126B}';
  IID_IEnumPStoreTypes: TGUID = '{789C1CBF-31EE-11D0-8C39-00C04FD9126B}';
  IID_IEnumPStoreItems: TGUID = '{5A6F1EC1-2DB1-11D0-8C39-00C04FD9126B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IEnumPStoreProviders = interface;
  IPStore = interface;
  IEnumPStoreTypes = interface;
  IEnumPStoreItems = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  CPStore = IEnumPStoreProviders;
  CEnumTypes = IEnumPStoreTypes;
  CEnumItems = IEnumPStoreItems;


// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  PUserType1 = ^_PST_PROVIDERINFO; {*}
  PByte1 = ^Byte; {*}
  PUserType2 = ^TGUID; {*}
  PUserType3 = ^_PST_TYPEINFO; {*}
  PUserType4 = ^_PST_ACCESSRULESET; {*}
  PPUserType1 = ^IEnumPStoreTypes; {*}
  PUserType5 = ^_PST_PROMPTINFO; {*}
  PPUserType2 = ^IEnumPStoreItems; {*}

  _PST_PROVIDERINFO = packed record
    cbSize: LongWord;
    ID: TGUID;
    Capabilities: LongWord;
    szProviderName: PWideChar;
  end;

  _PST_TYPEINFO = packed record
    cbSize: LongWord;
    szDisplayName: PWideChar;
  end;

  _PST_ACCESSCLAUSE = packed record
    cbSize: LongWord;
    ClauseType: LongWord;
    cbClauseData: LongWord;
    pbClauseData: ^Byte;
  end;

  _PST_ACCESSRULE = packed record
    cbSize: LongWord;
    AccessModeFlags: LongWord;
    cClauses: LongWord;
    rgClauses: ^_PST_ACCESSCLAUSE;
  end;

  _PST_ACCESSRULESET = packed record
    cbSize: LongWord;
    cRules: LongWord;
    rgRules: ^_PST_ACCESSRULE;
  end;

  _PST_PROMPTINFO = packed record
    cbSize: LongWord;
    dwPromptFlags: LongWord;
    hwndApp: LongWord;
    szPrompt: PWideChar;
  end;


// *********************************************************************//
// Interface: IEnumPStoreProviders
// Flags:     (0)
// GUID:      {5A6F1EBF-2DB1-11D0-8C39-00C04FD9126B}
// *********************************************************************//
  IEnumPStoreProviders = interface(IUnknown)
    ['{5A6F1EBF-2DB1-11D0-8C39-00C04FD9126B}']
    function  Next(celt: LongWord; out rgelt: PUserType1; var pceltFetched: LongWord): HResult; stdcall;
    function  Skip(celt: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Clone(out ppenum: IEnumPStoreProviders): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPStore
// Flags:     (0)
// GUID:      {5A6F1EC0-2DB1-11D0-8C39-00C04FD9126B}
// *********************************************************************//
  IPStore = interface(IUnknown)
    ['{5A6F1EC0-2DB1-11D0-8C39-00C04FD9126B}']
    function  GetInfo(out ppProperties: PUserType1): HResult; stdcall;
    function  GetProvParam(dwParam: LongWord; out pcbData: LongWord; out ppbData: PByte1;
                           dwFlags: LongWord): HResult; stdcall;
    function  SetProvParam(dwParam: LongWord; cbData: LongWord; var pbData: Byte; dwFlags: LongWord): HResult; stdcall;
    function  CreateType(Key: LongWord; var pType: TGUID; var pInfo: _PST_TYPEINFO;
                         dwFlags: LongWord): HResult; stdcall;
    function  GetTypeInfo(Key: LongWord; var pType: TGUID; out ppInfo: PUserType3; dwFlags: LongWord): HResult; stdcall;
    function  DeleteType(Key: LongWord; var pType: TGUID; dwFlags: LongWord): HResult; stdcall;
    function  CreateSubtype(Key: LongWord; var pType: TGUID; var pSubtype: TGUID;
                            var pInfo: _PST_TYPEINFO; var pRules: _PST_ACCESSRULESET;
                            dwFlags: LongWord): HResult; stdcall;
    function  GetSubtypeInfo(Key: LongWord; var pType: TGUID; var pSubtype: TGUID;
                             out ppInfo: PUserType3; dwFlags: LongWord): HResult; stdcall;
    function  DeleteSubtype(Key: LongWord; var pType: TGUID; var pSubtype: TGUID; dwFlags: LongWord): HResult; stdcall;
    function  ReadAccessRuleset(Key: LongWord; var pType: TGUID; var pSubtype: TGUID;
                                out ppRules: PUserType4; dwFlags: LongWord): HResult; stdcall;
    function  WriteAccessRuleset(Key: LongWord; var pType: TGUID; var pSubtype: TGUID;
                                 var pRules: _PST_ACCESSRULESET; dwFlags: LongWord): HResult; stdcall;
    function  EnumTypes(Key: LongWord; dwFlags: LongWord; var ppenum: IEnumPStoreTypes): HResult; stdcall;
    function  EnumSubtypes(Key: LongWord; var pType: TGUID; dwFlags: LongWord;
                           var ppenum: IEnumPStoreTypes): HResult; stdcall;
    function  DeleteItem(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                         szItemName: PWideChar; var pPromptInfo: _PST_PROMPTINFO; dwFlags: LongWord): HResult; stdcall;
    function  ReadItem(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                       szItemName: PWideChar; out pcbData: LongWord; out ppbData: Pointer;
                       var pPromptInfo: _PST_PROMPTINFO; dwFlags: LongWord): HResult; stdcall;
    function  WriteItem(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                        szItemName: PWideChar; cbData: LongWord; var pbData: Byte;
                        var pPromptInfo: _PST_PROMPTINFO; dwDefaultConfirmationStyle: LongWord;
                        dwFlags: LongWord): HResult; stdcall;
    function  OpenItem(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                       szItemName: PWideChar; ModeFlags: LongWord;
                       var pPromptInfo: _PST_PROMPTINFO; dwFlags: LongWord): HResult; stdcall;
    function  CloseItem(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                        szItemName: PWideChar; dwFlags: LongWord): HResult; stdcall;
    function  EnumItems(Key: LongWord; var pItemType: TGUID; var pItemSubtype: TGUID;
                        dwFlags: LongWord; var ppenum: IEnumPStoreItems): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumPStoreTypes
// Flags:     (0)
// GUID:      {789C1CBF-31EE-11D0-8C39-00C04FD9126B}
// *********************************************************************//
  IEnumPStoreTypes = interface(IUnknown)
    ['{789C1CBF-31EE-11D0-8C39-00C04FD9126B}']
    function  Next(celt: LongWord; out rgelt: TGUID; var pceltFetched: LongWord): HResult; stdcall;
    function  Skip(celt: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Clone(out ppenum: IEnumPStoreTypes): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumPStoreItems
// Flags:     (0)
// GUID:      {5A6F1EC1-2DB1-11D0-8C39-00C04FD9126B}
// *********************************************************************//
  IEnumPStoreItems = interface(IUnknown)
    ['{5A6F1EC1-2DB1-11D0-8C39-00C04FD9126B}']
    function  Next(celt: LongWord; out rgelt: PWideChar; var pceltFetched: LongWord): HResult; stdcall;
    function  Skip(celt: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Clone(out ppenum: IEnumPStoreItems): HResult; stdcall;
  end;

implementation

end.
