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

unit QBLnkFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOQBLinkForm = class(TForm)
    RadioOpt: TRadioGroup;
    RadioType: TRadioGroup;
    BtnOk: TButton;
    BtnCancel: TButton;
    txtTable1: TStaticText;
    txtTable2: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtCol1: TStaticText;
    Label4: TLabel;
    txtCol2: TStaticText;
  end;

implementation

{$R *.DFM}

end.
