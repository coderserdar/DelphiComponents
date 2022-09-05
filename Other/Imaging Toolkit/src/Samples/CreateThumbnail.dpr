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
// $Log:  26853: CreateThumbnail.dpr 
//
//    Rev 1.1    27-08-2007 19:00:56  mcm
//
//    Rev 1.0    06-12-2005 19:37:02  mcm
program CreateThumbnail;

uses
  Forms,
  uCreateThumbnail in 'uCreateThumbnail.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
