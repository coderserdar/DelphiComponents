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
// $Log:  22733: FishImage2DB.dpr 
//
//    Rev 1.1    02-10-2005 18:05:34  mcm
//
//   Rev 1.0    24-01-2004 13:00:00  mcm    Version: IMG 2.3
// Initial edition

program FishImage2DB;

uses
  Forms,
  UFormFishImage2DB in 'UFormFishImage2DB.pas' {FormImageDB};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FishImage2DB';
  Application.CreateForm(TFormImageDB, FormImageDB);
  Application.Run;
end.
