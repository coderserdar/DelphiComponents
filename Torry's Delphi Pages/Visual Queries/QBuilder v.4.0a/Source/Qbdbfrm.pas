{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       QBuilder dialog component                       }
{                                                       }
{       Copyright (c) 1996-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit QBdbFrm;

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
    DlgOpen: TOpenDialog;
    procedure btnDirClick(Sender: TObject);
  end;

implementation
{$R *.DFM}

uses
  QBDirFrm;
  

procedure TOQBDBForm.btnDirClick(Sender: TObject);
var
  QBDirForm : TOQBDirForm;
  s : string;
begin
  QBDirForm:=TOQBDirForm.Create(Application);
  GetDir(0,s);
  QBDirForm.Directory:=s;
  if QBDirForm.ShowModal=mrOk then
  begin
    EdtDir.Text:=QBDirForm.Directory;
  end;
  QBDirForm.Free;
end;

end.
