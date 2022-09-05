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
// $Log:  22197: FirstTmcmImageCtrl.dpr 
//
//    Rev 1.1    02-10-2005 18:05:22  mcm
//
//   Rev 1.0    17-11-2003 00:47:10  mcm    Version: IMG 2.0
// Initial edition

program FirstTmcmImageCtrl;

uses
  Forms,
  uFirstTmcmImageCtrl in 'uFirstTmcmImageCtrl.pas' {FormImageCtrl};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormImageCtrl, FormImageCtrl);
  Application.Run;
end.
