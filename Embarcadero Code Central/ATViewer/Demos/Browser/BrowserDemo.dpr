{************************************************}
{                                                }
{  Project: ATViewer Browser Demo                }
{  Copyright (C) 2006-2007 Alexey Torgashin      }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

program BrowserDemo;

uses
  Forms,
  UFormView in 'UFormView.pas' {FormView};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATViewer Demo';
  Application.CreateForm(TFormView, FormView);
  Application.Run;
end.
