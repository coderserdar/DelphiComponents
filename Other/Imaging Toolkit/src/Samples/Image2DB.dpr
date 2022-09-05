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
// $Log:  22339: Image2DB.dpr 
//
//    Rev 1.1    02-10-2005 18:05:44  mcm
//
//   Rev 1.0    24-11-2003 20:29:56  mcm
// Initial edition.

program Image2DB;

uses
  Forms,
  UFormImage2DB in 'UFormImage2DB.pas' {FormImageDB};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Image2DB';
  Application.CreateForm(TFormImageDB, FormImageDB);
  Application.Run;
end.
