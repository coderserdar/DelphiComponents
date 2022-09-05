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
// $Log:  27788: TmcmImage2Stream.dpr 
//
//    Rev 1.1    01-06-2006 20:06:18  mcm
//
//    Rev 1.0    01-06-2006 19:59:52  mcm
//
//    Rev 1.2    02-10-2005 18:06:44  mcm
//
//   Rev 1.1    26-03-2003 10:20:06  mcm

//
//   Rev 1.0    18-03-2003 16:34:48  mcm    Version: IMG 1.3.3

program TmcmImage2Stream;

uses
  Forms,
  uFormTmcmImage2Stream in 'uFormTmcmImage2Stream.pas' {FormStreaming};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TImage2Stream';
  Application.CreateForm(TFormStreaming, FormStreaming);
  Application.Run;
end.