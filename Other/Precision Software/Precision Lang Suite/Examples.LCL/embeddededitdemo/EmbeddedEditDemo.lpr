{-----------------------------------------------------------------------------
  Precision Language Suite for VCL

  written by Precision software & consulting
            copyright ©  2008 - 2010
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Embedded localization demo

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.
------------------------------------------------------------------------------}

program EmbeddedEditDemo;



uses
  Forms, LResources, Interfaces,
  Main in 'Main.pas' {Form1},
  Child in 'Child.pas' {frmChild},
  plsLangMan in '..\..\Source\plsLangMan.pas',
  plsDialogs in '..\..\Source\plsDialogs.pas',
  plsEmbedded in '..\..\Source\plsEmbedded.pas' {frmEmbeddedEditor};

{$IFDEF WINDOWS}{$R EmbeddedEditDemo.rc}{$ENDIF}

begin
  {$I EmbeddedEditDemo.lrs}
  Application.Initialize;

  { LanguageManager Initialization }
  // LanguageManager.FileNames.CommaText:='DemoMain,Comm*';   // set this, if you want to filter language files to those names (masks supported)
  // LanguageManager.RecursiveFolder:=True;                   // set this, if you want to get language files from subfolders too
  // LanguageManager.Folder:='..\..\Langs';                   // set this, if your language files are in different folder then default (AppExe\Langs)
  LanguageManager.LanguageCode:=GetDefaultLangCode;           // instead of default language code you can set here a language code loaded from your ini/config
  if LanguageManager.LanguageCode='' then
    LanguageManager.LanguageCode:='en';
  { End LanguageManager Initialization }

  Application.Title:='Embedded Edit Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
