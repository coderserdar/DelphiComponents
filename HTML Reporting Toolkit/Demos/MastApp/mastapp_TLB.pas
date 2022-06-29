unit mastapp_TLB;

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

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 06.09.2000 14:35:01 from Type Library described below.

// ************************************************************************ //
// Type Lib: D:\Work\delphi5\webpack\Demos\MastApp\mastapp.tlb (1)
// IID\LCID: {CA13E9F0-6969-4138-BA51-2B72C0A8B205}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  mastappMajorVersion = 1;
  mastappMinorVersion = 0;

  LIBID_mastapp: TGUID = '{CA13E9F0-6969-4138-BA51-2B72C0A8B205}';

  IID_IWindowExternal: TGUID = '{9F87AA84-19A0-44D0-8000-0C66954F4EC0}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IWindowExternal = interface;
  IWindowExternalDisp = dispinterface;

// *********************************************************************//
// Interface: IWindowExternal
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F87AA84-19A0-44D0-8000-0C66954F4EC0}
// *********************************************************************//
  IWindowExternal = interface(IDispatch)
    ['{9F87AA84-19A0-44D0-8000-0C66954F4EC0}']
    function  Get_FromDate: TDateTime; safecall;
    procedure Set_FromDate(Value: TDateTime); safecall;
    function  Get_ToDate: TDateTime; safecall;
    procedure Set_ToDate(Value: TDateTime); safecall;
    function  Get_DateTimeTickerEnabled: WordBool; safecall;
    procedure Set_DateTimeTickerEnabled(Value: WordBool); safecall;
    function  Get_SelectedNode: Integer; safecall;
    procedure Set_SelectedNode(Value: Integer); safecall;
    procedure EditOrder(OrderNo: Double); safecall;
    property FromDate: TDateTime read Get_FromDate write Set_FromDate;
    property ToDate: TDateTime read Get_ToDate write Set_ToDate;
    property DateTimeTickerEnabled: WordBool read Get_DateTimeTickerEnabled write Set_DateTimeTickerEnabled;
    property SelectedNode: Integer read Get_SelectedNode write Set_SelectedNode;
  end;

// *********************************************************************//
// DispIntf:  IWindowExternalDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F87AA84-19A0-44D0-8000-0C66954F4EC0}
// *********************************************************************//
  IWindowExternalDisp = dispinterface
    ['{9F87AA84-19A0-44D0-8000-0C66954F4EC0}']
    property FromDate: TDateTime dispid 1;
    property ToDate: TDateTime dispid 2;
    property DateTimeTickerEnabled: WordBool dispid 3;
    property SelectedNode: Integer dispid 4;
    procedure EditOrder(OrderNo: Double); dispid 5;
  end;

implementation

uses ComObj;

end.
