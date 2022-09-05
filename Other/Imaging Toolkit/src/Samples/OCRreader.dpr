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
// $Log:  21515: OCRreader.dpr 
//
//    Rev 1.7    05-03-2006 10:54:06  mcm    Version: IMG 2.16
// Added check for image being 8 bit grey scale.
//
//    Rev 1.6    02-10-2005 18:06:44  mcm
//
//   Rev 1.5    20-07-2004 20:36:28  mcm

//
//   Rev 1.4    08-07-2004 23:30:14  mcm    Version: IMG 2.5

//
//   Rev 1.3    30-01-2004 19:36:20  mcm    Version: IMG 2.3

//
//   Rev 1.2    17-11-2003 10:09:36  mcm    Version: OCR 1.0

//
//   Rev 1.1    16-10-2003 11:32:56  mcm    Version: OCR 1.0

//
//   Rev 1.0    25-09-2003 23:27:10  mcm    Version: IMG 1.5

program OCRreader;

uses
  Forms,
  uFormMainOCR in 'uFormMainOCR.pas' {FormMain},
  uchildwin in 'uchildwin.pas' {FormChild},
  uFormBrowse in 'uFormBrowse.pas' {FormBrowse},
  uFormImageInfo in 'uFormImageInfo.pas' {FormImageInfo},
  uFormStretch in 'uFormStretch.pas' {FormResize},
  uFormThreshold in 'uFormThreshold.pas' {FormThreshold},
  uFormRotate in 'uFormRotate.pas' {FormRotate},
  uFormOCRLearn in 'uFormOCRLearn.pas' {FormOCRLearn},
  uFormOCRText in 'uFormOCRText.pas' {FormOCRText};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'OCR Reader';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
