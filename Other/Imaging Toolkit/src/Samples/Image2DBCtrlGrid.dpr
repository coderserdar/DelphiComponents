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
// $Log:  25394: Image2DBCtrlGrid.dpr 
//
//   Rev 1.0    13-02-2005 19:48:38  mcm    Version: IMG 2.8

program Image2DBCtrlGrid;

uses
  Forms,
  uFormImage2DBCtrlGrid in 'uFormImage2DBCtrlGrid.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
