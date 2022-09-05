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
// $Log:  21503: TImage2Geometric.dpr 
//
//    Rev 1.1    02-10-2005 18:06:44  mcm
//
//   Rev 1.0    25-09-2003 23:25:46  mcm    Version: IMG 1.5

program TImage2Geometric;

uses
  Forms,
  uFormTImageGeometric in 'uFormTImageGeometric.pas' {FormGeoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGeoMain, FormGeoMain);
  Application.Run;
end.
