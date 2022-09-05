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
// $Log:  15904: twainproc.pas 
//
//    Rev 1.13    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.12    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.11    2013-12-04 23:16:16  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.10    2013-11-25 21:30:14  mcm
// Added XE2 About dialogue.
//
//    Rev 1.9    01-03-2011 20:41:06  mcm    Version: DT 3.11
//
//    Rev 1.8    25-10-2009 16:44:28  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//    Rev 1.7    11-01-2009 14:03:56  mcm    Version: DT 3.8
// Delphi 2009 Support
//
//    Rev 1.6    11-08-2007 10:59:12  mcm    Version: DT 3.7
// Added support for Delphi 2007
//
//    Rev 1.5    22-12-2005 20:53:08  mcm    Version: DT 3.6
// Delphi 2006 support.
//
//   Rev 1.4    03-01-2005 18:35:40  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.3    04-11-2003 20:15:46  mcm    Version: DT3.0
// Modified demo entrance

//
//   Rev 1.2    06-03-2003 10:58:42  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.1    09-10-02 01:36:24  mcm    Version: DT2.1

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

unit twainproc;

{$INCLUDE mcmDefines.pas}

interface

uses twain;


implementation // TWAIN
{$IFDEF MCMDEMO}
uses {$IFDEF GE_DXE2}
     WinApi.Windows, Vcl.Dialogs,
     {$ELSE}
     Dialogs, Windows,
     {$ENDIF}
     uTwnAbout;

{$ENDIF} // MCMDEMO

{$IFDEF MCMDEMO}
begin
    mcmAboutBox := TmcmAboutBox.Create(Nil);
    mcmAboutBox.DemoVersion := True;
{    if ((FindWindow('T'+mcmAppxName, mcmDelphixName+' 2') = 0) and
        (FindWindow('T'+mcmAppxName, mcmDelphixName+' 3') = 0) and
        (FindWindow('T'+mcmAppxName, mcmDelphixName+' 4') = 0) and
        (FindWindow('T'+mcmAppxName, mcmDelphixName+' 5') = 0) and
        (FindWindow('T'+mcmAppxName, mcmDelphixName+' 6') = 0) and
        (FindWindow('T'+mcmAppxName, mcmDelphixName+' 7') = 0) and
        (FindWindow('T'+mcmAppxName, mcmCxName+mcmBuilderxName) = 0) and
        (FindWindow('T'+mcmAppxName, mcmCxName+mcmBuilderxName+' 4') = 0) and
        (FindWindow('T'+mcmAppxName, mcmCxName+mcmBuilderxName+' 5') = 0) and
        (FindWindow('T'+mcmAppxName, mcmCxName+mcmBuilderxName+' 6') = 0)) or
       (FindWindow('T'+mcmAlignxName+mcmPalettexName, nil) = 0) or
       (FindWindow('T'+mcmBuildAppxName, nil) = 0)
    then begin }
         mcmAboutBox.ShowModal;
{    end; }
    mcmAboutBox.Free;

    mcmAboutBox := Nil;
{$ENDIF}

end.
