unit cmdacver;

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
// File generated on 28-08-2000 00:09:29 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\WINDOWS\SYSTEM\ODBCCONF.DLL
// IID\LCID: {54AF9343-1923-11D3-9CA4-00C04F72C514}\0
// Helpfile: 
// HelpString: Microsoft Data Access Components Installed Version
// Version:    2.50
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
  LIBID_MDACVer: TGUID = '{54AF9343-1923-11D3-9CA4-00C04F72C514}';
  IID_IVersion: TGUID = '{54AF934F-1923-11D3-9CA4-00C04F72C514}';
  CLASS_Version: TGUID = '{54AF9350-1923-11D3-9CA4-00C04F72C514}';
type

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
  IVersion = interface;
  IVersionDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to its Default Interface)            //
// *********************************************************************//
  Version = IVersion;


// *********************************************************************//
// Interface: IVersion
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {54AF934F-1923-11D3-9CA4-00C04F72C514}
// *********************************************************************//
  IVersion = interface(IDispatch)
    ['{54AF934F-1923-11D3-9CA4-00C04F72C514}']
    function Get_Major: OleVariant; safecall;
    function Get_Minor: OleVariant; safecall;
    function Get_Build: OleVariant; safecall;
    function Get_Qfe: OleVariant; safecall;
    function Get_String_: OleVariant; safecall;
    property Major: OleVariant read Get_Major;
    property Minor: OleVariant read Get_Minor;
    property Build: OleVariant read Get_Build;
    property Qfe: OleVariant read Get_Qfe;
    property String_: OleVariant read Get_String_;
  end;

// *********************************************************************//
// DispIntf:  IVersionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {54AF934F-1923-11D3-9CA4-00C04F72C514}
// *********************************************************************//
  IVersionDisp = dispinterface
    ['{54AF934F-1923-11D3-9CA4-00C04F72C514}']
    property Major: OleVariant readonly dispid 1;
    property Minor: OleVariant readonly dispid 2;
    property Build: OleVariant readonly dispid 3;
    property Qfe: OleVariant readonly dispid 4;
    property String_: OleVariant readonly dispid 5;
  end;

  CoVersion = class
    class function Create: IVersion;
    class function CreateRemote(const MachineName: string): IVersion;
  end;

implementation

uses ComObj;

class function CoVersion.Create: IVersion;
begin
  Result := CreateComObject(CLASS_Version) as IVersion;
end;

class function CoVersion.CreateRemote(const MachineName: string): IVersion;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Version) as IVersion;
end;

end.
