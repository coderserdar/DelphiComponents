{*******************************************************}
{                                                       }
{       Extension Library example of                    }
{       TELDesigner                                     }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit Unit2;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, ComCtrls, StdCtrls, ExtCtrls, Grids, ToolWin, ELDsgnr, Menus,
  ActnList, ImgList;

type
    TForm2 = class (TForm)
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
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    ActionList1: TActionList;
    ImageList1: TImageList;
    Action1: TAction;
    Action2: TAction;
    Action11: TMenuItem;
    Action21: TMenuItem;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Action1Execute(Sender: TObject);
begin
	ShowMessage('1');
end;

procedure TForm2.Action2Execute(Sender: TObject);
begin
	ShowMessage('2');
end;

end.

