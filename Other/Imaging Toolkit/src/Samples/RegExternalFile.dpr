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
// $Log:  26067: RegExternalFile.dpr 
//
//    Rev 1.1    31-10-2007 20:19:12  mcm    Version: IMG 3.2
// Moved WMF file import filter to mcmWMFFile.pas.
//
//   Rev 1.0    23-08-2005 18:59:50  mcm    Version: IMG 2.9

program RegExternalFile;

uses
  Forms,
  uFormRegExternalFile in 'uFormRegExternalFile.pas' {FormMain},
  uFormBrowse in 'uFormBrowse.pas' {FormBrowse},
  mcmWMFFile in 'mcmWMFFile.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
