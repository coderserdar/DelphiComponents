unit oledbadm;

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
// File generated on 09-01-2001 21:14:38 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\WINDOWS\SYSTEM\OLEDBADM.DLL
// IID\LCID: {7A28E540-3649-11D4-98B3-E352F1564F07}\0
// Helpfile: 
// HelpString: OLEDB Administrator Library
// Version:    1.5
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
  LIBID_oledbadm: TGUID = '{7A28E540-3649-11D4-98B3-E352F1564F07}';
  IID_IOLEDBAdmin: TGUID = '{7A28E541-3649-11D4-98B3-E352F1564F07}';
  CLASS_OLEDBAdmin: TGUID = '{7A28E543-3649-11D4-98B3-E352F1564F07}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                  //
// *********************************************************************//
// DSNTypeEnum constants
type
  DSNTypeEnum = TOleEnum;
const
  adDSNUser = $00000345;
  adDSNSystem = $00000346;

type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IOLEDBAdmin = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to its Default Interface)            //
// *********************************************************************//
  OLEDBAdmin = IOLEDBAdmin;


// *********************************************************************//
// Interface: IOLEDBAdmin
// Flags:     (0)
// GUID:      {7A28E541-3649-11D4-98B3-E352F1564F07}
// *********************************************************************//
  IOLEDBAdmin = interface(IUnknown)
    ['{7A28E541-3649-11D4-98B3-E352F1564F07}']
    function Open(DSNType: DSNTypeEnum): Integer; stdcall;
    procedure Close; stdcall;
    function GetDSN(Index: Integer; var Buffer: WideString): Integer; stdcall;
    function GetConnectionString(const DSN: WideString; var ConnectionString: WideString): Integer; stdcall;
    function GetDSNCount: Integer; stdcall;
    function SetConnectionString(const DSN: WideString; const ConnectionString: WideString): Integer; stdcall;
  end;

  CoOLEDBAdmin = class
    class function Create: IOLEDBAdmin;
    class function CreateRemote(const MachineName: string): IOLEDBAdmin;
  end;

implementation

uses ComObj;

class function CoOLEDBAdmin.Create: IOLEDBAdmin;
begin
  Result := CreateComObject(CLASS_OLEDBAdmin) as IOLEDBAdmin;
end;

class function CoOLEDBAdmin.CreateRemote(const MachineName: string): IOLEDBAdmin;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OLEDBAdmin) as IOLEDBAdmin;
end;

end.
