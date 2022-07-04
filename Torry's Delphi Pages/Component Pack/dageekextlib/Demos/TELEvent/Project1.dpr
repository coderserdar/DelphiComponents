{*******************************************************}
{                                                       }
{       Extension Library example of                    }
{       TELEvent, TELEventSender                        }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2},
  Unit3 in 'Unit3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TELEvent, TELEventSource demo';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm3, Form3);
  Form1.Start;
  Application.Run;
end.
