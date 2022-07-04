{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELInstanceChecker                                      }
{                                                          }
{  Copyright (c) 2001, Balabuyev Yevgeny                   }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

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
