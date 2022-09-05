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
// $Log:  15878: d32twainLI.dpr 
//
//    Rev 1.8    2014-01-15 13:41:56  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}

program d32twainLI;

{$INCLUDE mcmDefines.pas}

uses
    {$IFNDEF GE_DXE2}
     Forms,
     uTwainFormLI in 'uTwainFormLI.pas' {FormTWAIN};
     {$ELSE}
     Vcl.Forms,
     uTwainFormLI in 'uTwainFormLI.pas' {FormTWAIN};
     {$ENDIF}

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TWAIN Toolkit for Delphi';
  Application.CreateForm(TFormTWAIN, FormTWAIN);
  Application.Run;
end.
