{*******************************************************}
{File:      O8AboutFrm.PAS                              }
{Revision:  0.01 / 27.10.1999                           }
{Comment:   NC OCI8 Demo: NC O8 Console, about form     }
{Copyright: (c) 1999, Dmitry Arefiev                    }
{Author:    Dmitry Arefiev, diman@ncom.ru               }
{*******************************************************}

unit O8AboutFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TNCO8AboutFrm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Image2: TImage;
    Memo1: TMemo;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute;
  end;

var
  NCO8AboutFrm: TNCO8AboutFrm;

implementation

{$R *.DFM}

procedure TNCO8AboutFrm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    Close;
end;

procedure TNCO8AboutFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    Close;
end;

class procedure TNCO8AboutFrm.Execute;
begin
    if NCO8AboutFrm = nil then
        Application.CreateForm(TNCO8AboutFrm, NCO8AboutFrm);
    NCO8AboutFrm.ShowModal;
end;

end.
