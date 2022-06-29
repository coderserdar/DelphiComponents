{************************************************}
{                                                }
{  Project: Universal Viewer                     }
{  Copyright (C) 2006-09 Alexey Torgashin        }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

{$MAXSTACKSIZE $00200000} //Recommended for RegEx library.
{$I ViewerOptions.inc} //UV options.

program Viewer;

uses
  Windows,
  Forms,
  UFormView in 'UFormView.pas' {FormViewUV},
  UFormViewFindText in 'UFormViewFindText.pas' {FormViewFindText},
  UFormViewOptions in 'UFormViewOptions.pas' {FormViewOptions},
  UFormViewOptionsText in 'UFormViewOptionsText.pas' {FormViewOptionsText},
  UFormViewOptionsImages in 'UFormViewOptionsImages.pas' {FormViewOptionsImages},
  UFormViewOptionsGutter in 'UFormViewOptionsGutter.pas' {FormViewOptionsGutter},
  UFormViewGoto in 'UFormViewGoto.pas' {FormViewGoto},
  UFormPluginsOptions in 'UFormPluginsOptions.pas' {FormPluginsOptions},
  UFormPluginsAdd in 'UFormPluginsAdd.pas' {FormPluginsAdd},
  UFormPluginsEdit in 'UFormPluginsEdit.pas' {FormPluginsEdit},
  UFormViewFindProgress in 'UFormViewFindProgress.pas' {FormViewFindProgress},
  UFormViewToolParams in 'UFormViewToolParams.pas' {FormViewToolParams},
  UFormViewToolbar in 'UFormViewToolbar.pas' {FormViewToolbar},
  UFormViewToolList in 'UFormViewToolList.pas' {FormViewToolList},
  UFormViewRename in 'UFormViewRename.pas' {FormViewRename},
  {$ifdef EXIF}
  UFormViewEXIF in 'UFormViewEXIF.pas' {FormViewEXIF},
  {$endif}
  UFormViewAbout in 'UFormViewAbout.pas' {FormViewAbout},
  UFormViewEdit in 'UFormViewEdit.pas' {FormViewEdit};

{$R *.RES}
{$R ViewerRes.RES}

begin
  Application.Initialize;
  Application.Title := 'Universal Viewer';
  Application.CreateForm(TFormViewUV, FormViewUV);
  Application.Run;
end.
