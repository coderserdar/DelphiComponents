{**********************************************************}
{                                                          }
{  CD Player                                               }
{  Devrace Extension Library example of                    }
{  TELTrayIcon, TELInstanceChecker                         }
{  ELPackStrings, ELUnpackStrings                          }
{  See Readme.txt for comments                             }
{                                                          }
{  Copyright (c) 2000 - 2001, Balabuyev Yevgeny            }
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

unit dlgExitWindowsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type
  TdlgExitWindows = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    Button1: TButton;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TdlgExitWindows.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := ProgressBar1.Min;
  Timer1.Enabled := True;
  if Button1.CanFocus then Button1.SetFocus;
end;

procedure TdlgExitWindows.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Position := ProgressBar1.Position + 1;
  if ProgressBar1.Position = ProgressBar1.Max then
  begin
    ProgressBar1.Position := ProgressBar1.Min;
    Timer1.Enabled := False;
    ModalResult := mrOk;
  end;
end;

procedure TdlgExitWindows.Button1Click(Sender: TObject);
begin
  ProgressBar1.Position := ProgressBar1.Min;
  Timer1.Enabled := False;
end;

procedure TdlgExitWindows.FormHide(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

end.
