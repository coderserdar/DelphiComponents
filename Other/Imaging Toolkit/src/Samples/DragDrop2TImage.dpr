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
// $Log:  25952: DragDrop2TImage.dpr 
//
//    Rev 1.1    20-08-2007 20:22:34  mcm
//
//   Rev 1.0    24-07-2005 18:50:28  mcm    Version: IMG 2.9
// Drag & Drop sample project.

//
//   Rev 1.2    21-07-2005 23:19:14  mcm

//
//   Rev 1.1    12-03-2003 15:39:04  mcm    Version: IMG 1.3.2

//
//   Rev 1.0    10-02-2003 18:45:50  mcm    Version: IMG 1.3

program DragDrop2TImage;

uses
  Forms,
  uFormDragDrop2TImage in 'uFormDragDrop2TImage.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Load2TImage';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
