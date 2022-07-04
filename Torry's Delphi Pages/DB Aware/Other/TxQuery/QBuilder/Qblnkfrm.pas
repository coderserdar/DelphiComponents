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

{$HINTS OFF}
{$WARNINGS OFF}

unit QBLnkFrm;

{$I xq_flag.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBLinkForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Panel1: TPanel;
    txtTable1: TLabel;
    txtCol1: TLabel;
    Panel2: TPanel;
    txtTable2: TLabel;
    txtCol2: TLabel;
    Image1: TImage;
    CboOpt: TComboBox;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image2: TImage;
    Bevel1: TBevel;
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    JoinType: Integer;
  end;

implementation

{$R *.DFM}

uses
   xqmiscel, xqconsts;

procedure TOQBLinkForm.Label2Click(Sender: TObject);
begin
   Image2.Top := Label2.Top;
   JoinType := 0;
end;

procedure TOQBLinkForm.Label3Click(Sender: TObject);
begin
   Image2.Top := Label3.Top;
   JoinType := 1;
end;

procedure TOQBLinkForm.FormCreate(Sender: TObject);
begin
   Label3.Caption := Format(Label3.Caption,[txtTable1.Caption,txtTable2.Caption]);
end;

end.
