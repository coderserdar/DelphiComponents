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
// $Log:  15880: mcmTWAINDS.dpr 
//
//    Rev 1.7    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.6    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.5    03-01-2005 18:35:20  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.4    04-11-2003 23:45:34  mcm    Version: DT3.0
// Updated version info.

//
//   Rev 1.3    06-07-2003 11:07:52  mcm    Version: DT 2.5
// Release version 2.5

//
//   Rev 1.2    16-05-2003 20:55:06  mcm    Version: DT 2.4

//
//   Rev 1.1    06-03-2003 11:39:24  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.0    04-12-2001 16:49:06  mcm    Version: DT 2.0

Library mcmTWAINDS;

//------------------------------------------------------------------------------
// This Library is the "program file".
//
// Compile this file, and rename it to twdsrcd.ds
// Then place it in the directory
//
//   C:\Windows\TWAIN_32\
//
// or in a sub-directory,
//
//   C:\Windows\TWAIN_32\MY_DIR\
//
// which makes it more clear which files belong to which TWAIN driver.
//
// Note: It's not possible to add a Title to this project in
//         Project|Options|Application
//       because the Application.CreateForm is missing.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// RESOURCE FILE
//
// The version information used by a TWAIN driver is stored in "driver32.res",
// and included below as {$R DRIVER32.RES}.
// Modify the "driver32.res" or "driver32.rc" data, though without changing the
// entries.
// The "driver32.rc" can be edited in Notepad or Delphi. To create a ".res" file
// that can be included in a Delphi Project convert the ".RC" file using
// brcc32.exe program included with Delphi.
//
//     c:\program files\borland\delphi X\bin\brcc32.exe -r driver32.rc
//
// This creates the driver32.res file included below. In the driver32.rc file
// look for
//     "String Table - Info used by TWAIN !!!!!!!!"
// The data in this section is the primary data to change, and the entries are:
//
// STRINGTABLE DISCARDABLE
// BEGIN
//     0  "MCM DESIGN"    <- This is the company name
//     1  "TWAIN Drivers" <- This is the product family name
//     2  "MCM DESIGN Delphi Data Source" <- The name listed in "Select Source" dialogue
//     3  "Version 2.4.0" <- The version number of your TWAIN driver
// END
//
// Additionally you might want to change the information in the section
// "VERSION". The information in this section is used by Windows.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEBUGGING
//
// Set Conditional Defines to TWNDEBUG, in Project, Options.
// This will make the TWAIN source generate a new LOG file each time the source
// is started.
// The LOG file contains information about function calls in twd_port.pas, incl
// message
//------------------------------------------------------------------------------

{$INCLUDE mcmDefines.pas}

uses
  {$IFNDEF GE_DXE2}
     Windows,
     uDSUserInterface in 'uDSUserInterface.PAS' {FormSource},
     Usabout in 'USABOUT.PAS' {FormAbout},
     mcmTWAINDSEntry in 'mcmTWAINDSEntry.pas',
     mcmTWAINDSLayer in 'mcmTWAINDSLayer.pas',
     uTwnDlgAppInfo in 'uTwnDlgAppInfo.pas' {FormAppInfo},
     uTwnDlgSimEvents in 'uTwnDlgSimEvents.pas' {FormEvents},
     mcmTWAINLog in 'mcmTWAINLog.pas',
     uTwnDlgHandle in 'uTwnDlgHandle.pas';
  {$ELSE}
     WinApi.Windows,
     uDSUserInterface in 'uDSUserInterface.pas' {FormSource},
     Usabout in 'USABOUT.PAS' {FormAbout},
     mcmTWAINDSEntry in 'mcmTWAINDSEntry.pas',
     mcmTWAINDSLayer in 'mcmTWAINDSLayer.pas',
     uTwnDlgAppInfo in 'uTwnDlgAppInfo.pas' {FormAppInfo},
     uTwnDlgSimEvents in 'uTwnDlgSimEvents.pas' {FormEvents},
     mcmTWAINLog in 'mcmTWAINLog.pas',
     uTwnDlgHandle in 'uTwnDlgHandle.pas';
  {$ENDIF}

{$E ds}

{$R DRIVER32.RES}

{$R *.RES}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

exports DS_Entry index 1;

var SaveExit : Pointer;

procedure LibExit;
begin
  // library exit code.
  ExitProc := SaveExit;  // restore exit procedure chain
end; { End LibExit.                                                            }


begin
  // library initialization code
  SaveExit := ExitProc;  // save exit procedure chain
  ExitProc := @LibExit;  // install LibExit exit procedure

  DLLProc := @mcmTWAINDSEntry.LibMain;
  mcmTWAINDSEntry.LibMain(DLL_PROCESS_ATTACH);
end.


