{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}
{}
{ $Log:  15876: d32twain.dpr 
//
//    Rev 1.16    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.15    2013-12-05 21:43:36  mcm
//
//    Rev 1.14    2013-12-04 23:16:10  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.13    30-12-2008 17:51:10  mcm    Version: DT 3.8
//
//    Rev 1.12    19-11-2007 19:29:54  mcm
// Added manifest.
//
//   Rev 1.11    15-05-2005 22:00:54  mcm    Version: DT 3.5
// New version

//
//   Rev 1.10    23-02-2005 23:43:32  mcm

//
//   Rev 1.9    19-02-2005 00:34:02  mcm    Version: DT 3.4

//
//   Rev 1.8    03-01-2005 18:35:18  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.7    12-11-2003 13:54:20  mcm    Version: DT3.0

//
//   Rev 1.6    06-11-2003 09:52:04  mcm    Version: DT3.0
// Updated version info.

//
//   Rev 1.5    06-07-2003 11:08:22  mcm    Version: DT 2.5
// Release version 2.5

//
//   Rev 1.4    14-06-2003 11:08:56  mcm    Version: DT 2.4
// Updated version information.

//
//   Rev 1.3    15-04-2003 10:57:22  mcm    Version: DT 2.3
// Updated sample project.

//
//   Rev 1.2    12-03-2003 15:40:42  mcm    Version: DT 2.2
// Release 2.2

//
//   Rev 1.1    06-03-2003 11:08:42  mcm    Version: DT 2.2

//
//   Rev 1.0    04-12-2001 16:49:04  mcm    Version: DT 2.0

{
{   Rev 1.3    24-01-01 08:40:20  mcm
{ Updated version info.
}
{
{   Rev 1.2    24-10-00 00:23:09  mcm    Version: 1.9.1
{ Updated version 1.9.1.
}
{
{   Rev 1.1    09-04-00 15:17:15  mcm    Version: 1.8.4
}
{
{   Rev 1.0    15-02-00 12:01:20  mcm    Version: 1.8.3
{ Initial Revision
}
{
{   Rev 1.1    14-02-00 02:30:56  mcm
}
{
{   Rev 1.0    30-12-99 00:19:06  Marc Martin    Version: 1.8.2
{ TWAIN Toolkit for Delphi
}
{}
program d32twain;

{$INCLUDE mcmDefines.pas}

uses
  {$IFNDEF GE_DXE2}
  Forms,
  uTwainForm in 'uTwainForm.pas' {FormTWAIN},
  uTwnDlgSrcInfo in 'uTwnDlgSrcInfo.pas' {FormSrcInfo};
  {$ELSE}
  Vcl.Forms,
  uTwainForm in 'uTwainForm.pas' {FormTWAIN},
  uTwnDlgSrcInfo in 'uTwnDlgSrcInfo.pas' {FormSrcInfo};
  {$ENDIF}

{$R d32twain.RES}
{ $R d32Twain_Manifest.RES}


begin
  Application.Initialize;
  Application.Title := 'TWAIN Toolkit for Delphi';
  Application.CreateForm(TFormTWAIN, FormTWAIN);
  Application.Run;
end.
