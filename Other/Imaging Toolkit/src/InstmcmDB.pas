// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  23594: InstmcmDB.pas
//
//    Rev 1.2    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.1    03-01-2005 18:26:32  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.0    01-09-2004 18:30:58  mcm    Version: IMG 2.6
// Initial revision. Included to support the toolkit in general (except the
// TmcmImageDB control) in Delphi/C++Builder Standard edition.

unit InstmcmDB;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF DCB2_5}
  uses Dsgnintf;
{$ELSE}
 uses DesignIntf, DesignEditors;
{$ENDIF}

procedure Register;

implementation

uses {$IFNDEF GE_DXE2}
      Classes, Windows, SysUtils, Dialogs, TypInfo,
     {$ELSE}
      WinApi.Windows, System.SysUtils, System.Classes, Vcl.Dialogs, System.TypInfo,
     {$ENDIF}
     mcmImageDB, InstmcmImaging;


procedure Register;
begin
  RegisterComponents('Imaging Toolbox', [TmcmImageDB]);
  RegisterComponentEditor(TmcmImageDB, TmcmPropMenu);
end; // Register.

end.
