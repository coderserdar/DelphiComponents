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

unit QBdbFrm2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBDBForm2 = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Bevel1: TBevel;
    CheckDB: TCheckBox;
    EdtDB: TEdit;
    btnDir: TButton;
    Label1: TLabel;
    DlgSelect: TOpenDialog;
    CheckView: TCheckBox;
    procedure btnDbClick(Sender: TObject);
  end;

implementation
{$R *.DFM}

procedure TOQBDBForm2.btnDbClick(Sender: TObject);
var
  s : string;
begin
  GetDir(0,s);
  DlgSelect.InitialDir:=s;
  if DlgSelect.Execute then
  begin
    EdtDB.Text:=DlgSelect.FileName;
  end;
end;

end.
