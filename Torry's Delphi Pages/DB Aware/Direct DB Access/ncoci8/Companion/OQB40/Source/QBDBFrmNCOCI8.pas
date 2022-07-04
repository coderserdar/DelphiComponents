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

unit QBDBFrmNCOCI8;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBDBFormNCOCI8 = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Bevel1: TBevel;
    CheckDB: TCheckBox;
    CheckView: TCheckBox;
  end;

implementation
{$R *.DFM}

end.
