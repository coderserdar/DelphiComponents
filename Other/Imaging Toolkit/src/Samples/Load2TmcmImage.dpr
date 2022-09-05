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
// $Log:  25398: Load2TmcmImage.dpr 
//
//    Rev 1.4    18-03-2006 18:34:04  mcm    Version: IMG 2.16
// Corrected sample to use UnregisterFileFormat
//
//   Rev 1.3    24-07-2005 18:48:02  mcm    Version: IMG 2.9

//
//   Rev 1.2    09-04-2005 17:30:24  mcm    Version: IMG 2.9

//
//   Rev 1.1    29-03-2005 20:20:38  mcm

//
//   Rev 1.0    13-02-2005 19:49:36  mcm    Version: IMG 2.8

program Load2TmcmImage;

uses
  Forms,
  uFormLoad2TmcmImage in 'uFormLoad2TmcmImage.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
