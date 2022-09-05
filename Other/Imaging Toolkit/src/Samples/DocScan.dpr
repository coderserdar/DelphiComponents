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
// $Log:  21054: DocScan.dpr 
//
//    Rev 1.9    26-08-2009 22:46:44  mcm    Version: IMG 3.2
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.8    22-05-2006 20:21:56  mcm
//
//   Rev 1.7    23-08-2005 22:28:02  mcm    Version: IMG 2.9

//
//   Rev 1.6    11-06-2005 10:06:38  mcm    Version: IMG 2.9

//
//   Rev 1.5    15-05-2005 23:43:38  mcm    Version: IMG 2.9

//
//   Rev 1.4    09-04-2005 17:34:32  mcm    Version: IMG 2.9
// Corrected a problem with data sources being unable to negotiate resolution.
// Enabled scan of multiple pages from non ADF scanners (Correction).

//
//   Rev 1.3    01-06-2004 00:06:30  mcm
// Added manifest resource, providing XP look.

//
//   Rev 1.2    06-11-2003 16:07:08  mcm    Version: IMG 2.0
// Updated for the new TmcmTWAIN component using threads.

//
//   Rev 1.1    04-11-2003 23:35:06  mcm

//
//   Rev 1.0    29-07-2003 11:42:38  mcm

program DocScan;

uses
  Forms,
  DocScanForm in 'DocScanForm.pas' {FormDocScan};

{$R *.RES}
{$R DocScan_Manifest.RES}

begin
  Application.Initialize;
  Application.Title := 'DocScan';
  Application.CreateForm(TFormDocScan, FormDocScan);
  Application.Run;
end.
