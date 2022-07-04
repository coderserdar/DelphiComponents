{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       QBuilder dialog component                       }
{                                                       }
{       Copyright (c) 1996-2003 Sergey Orlik            }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Borland Moscow office                           }
{       Internet:  support@fast-report.com,             }
{                  sorlik@borland.com                   }
{                  http://www.fast-report.com           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBAboutForm = class(TForm)
    Bevel1: TBevel;
    Ok: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    procedure WMNCHitTest(var Message :TWMNCHitTest); message WM_NCHITTEST;
  end;

implementation

{$R *.DFM}

procedure TOQBAboutForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
     inherited;
     if  Message.Result = htClient then
      Message.Result := htCaption;
end;

end.
