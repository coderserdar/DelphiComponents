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
// $Log:  19193: ImageMath2TImage.dpr 
//
//    Rev 1.2    02-10-2005 18:06:06  mcm
//
//   Rev 1.1    12-03-2003 15:38:40  mcm    Version: IMG 1.3.2

//
//   Rev 1.0    10-02-2003 18:45:50  mcm    Version: IMG 1.3

program ImageMath2TImage;

uses
  Forms,
  uFormImageMath2TImage in 'uFormImageMath2TImage.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ImageMath2TImage';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
