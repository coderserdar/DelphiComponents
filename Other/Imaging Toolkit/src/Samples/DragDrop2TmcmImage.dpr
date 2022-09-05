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
// $Log:  25954: DragDrop2TmcmImage.dpr 
//
//    Rev 1.1    20-08-2007 20:24:06  mcm    Version: IMG 3.1
//
//   Rev 1.0    24-07-2005 18:50:28  mcm    Version: IMG 2.9
// Drag & Drop sample project.

//
//   Rev 1.2    09-04-2005 17:30:24  mcm    Version: IMG 2.9

//
//   Rev 1.1    29-03-2005 20:20:38  mcm

//
//   Rev 1.0    13-02-2005 19:49:36  mcm    Version: IMG 2.8

program DragDrop2TmcmImage;

uses
  Forms,
  uFormDragDrop2TmcmImage in 'uFormDragDrop2TmcmImage.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
