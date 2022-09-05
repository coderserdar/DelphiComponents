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
// $Log:  22699: ScanIn72dpi.dpr 
//
//   Rev 1.1    03-01-2005 18:35:20  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.0    20-01-2004 21:05:52  mcm    Version: DT3.0
// Initial revision.

//
//   Rev 1.0    03-01-2004 13:02:58  mcm
// Initial edition.

program ScanIn72dpi;

uses
  Forms,
  uScanIn72dpi in 'uScanIn72dpi.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
