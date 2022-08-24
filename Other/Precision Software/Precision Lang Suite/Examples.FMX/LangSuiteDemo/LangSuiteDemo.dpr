program LangSuiteDemo;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Child in 'Child.pas' {frmChild},
  FMX.plsLangMan in '..\..\Source\FMX.plsLangMan.pas';

{$R *.res}

begin
  { LanguageManager Initialization }
  // LanguageManager.FileNames.CommaText:='DemoMain,Comm*';   // set this, if you want to filter language files to those names (masks supported)
  // LanguageManager.RecursiveFolder:=True;                   // set this, if you want to get language files from subfolders too
  // LanguageManager.Folder:='..\..\Langs';                   // set this, if your language files are in different folder then default (AppExe\Langs)
  LanguageManager.LanguageCode:=GetDefaultLangCode;           // instead of default language code you can set here a language code loaded from your ini/config
  if LanguageManager.LanguageCode='' then
    LanguageManager.LanguageCode:='en';
  { End LanguageManager Initialization }

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
