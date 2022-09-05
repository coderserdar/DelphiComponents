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
// $Log:  22606: ReadMultiTIFF.dpr 
//
//    Rev 1.1    02-10-2005 18:06:44  mcm
//
//   Rev 1.0    12-12-2003 19:57:24  mcm

program ReadMultiTIFF;

uses
  Forms,
  uFormTIFFRead in 'uFormTIFFRead.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
