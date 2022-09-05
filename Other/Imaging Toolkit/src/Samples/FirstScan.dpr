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
// $Log:  22648: FirstScan.dpr 
//
//    Rev 1.2    2013-12-04 23:16:10  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.1    20-01-2004 21:05:12  mcm    Version: DT3.0

//
//   Rev 1.0    03-01-2004 13:02:58  mcm
// Initial edition.

program FirstScan;

uses
  Forms,
  uFirstScan in 'uFirstScan.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
