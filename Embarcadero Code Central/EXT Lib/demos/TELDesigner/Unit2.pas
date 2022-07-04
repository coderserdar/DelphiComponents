{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELDesigner                                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
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

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Grids, ToolWin, ELDsgnr;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    CheckBox1: TCheckBox;
    Button3: TButton;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button2: TButton;
    UpDown1: TUpDown;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Image1: TImage;
    Image2: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
