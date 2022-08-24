{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2010
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Localize from scratch demo

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

program LocalizeFromScratch;

uses
  Forms, LResources, Interfaces,
  Main in 'Main.pas' {Form1},
  Child in 'Child.pas' {frmChild},
  plsLangMan in '..\..\Source\plsLangMan.pas',
  plsDialogs in '..\..\Source\plsDialogs.pas',
  plsEmbedded in '..\..\Source\plsEmbedded.pas' {frmEmbeddedEditor};

{$IFDEF WINDOWS}{$R LocalizeFromScratch.rc}{$ENDIF}

begin
  {$I LocalizeFromScratch.lrs}
  Application.Initialize;
  Application.Title:='Localize from scratch demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
