{*******************************************************}
{                                                       }
{       Extension Library example of                    }
{       TELInstanceChecker                              }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

program Project1;

uses
  Forms,
  Controls,
  DIalogs,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TELInstanceChecker demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
