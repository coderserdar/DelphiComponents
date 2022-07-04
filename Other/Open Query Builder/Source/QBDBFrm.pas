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

unit QBDBFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBDBForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Bevel1: TBevel;
    ComboDB: TComboBox;
    CheckDB: TCheckBox;
    EdtDir: TEdit;
    btnDir: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnDirClick(Sender: TObject);
  end;


implementation

{$R *.DFM}

uses
  QBDirFrm;


procedure TOQBDBForm.btnDirClick(Sender: TObject);
var
  QBDirForm: TOQBDirForm;
  s: string;
begin
  QBDirForm := TOQBDirForm.Create(Application);
  GetDir(0, s);
  QBDirForm.Directory := s;
  if QBDirForm.ShowModal = mrOk then
    EdtDir.Text := QBDirForm.Directory;
  QBDirForm.Free;
end;

end.
