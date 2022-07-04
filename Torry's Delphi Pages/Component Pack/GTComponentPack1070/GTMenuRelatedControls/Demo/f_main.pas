unit f_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, o_MenuTreeView, StdCtrls, Menus, jpeg, ExtCtrls,XpMan;

type
  TForm1 = class(TForm)
    gtMenuTreeView1: TgtMenuTreeView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    MainMenu: TMainMenu;
    btnPopulateMenu: TButton;
    btnCopyToTreeView: TButton;
    Button1: TButton;
    chkMapEvents: TCheckBox;
    Image1: TImage;
    procedure btnPopulateMenuClick(Sender: TObject);
    procedure btnCopyToTreeViewClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure MenuItemClick(Sender : TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{------------------------------------------------------------------------------}
procedure TForm1.btnPopulateMenuClick(Sender: TObject);
var
   i,j,x   : integer;
   MnuItem : TMenuItem;
   SubItem : TMenuItem;
   SubSubItem : TMenuItem;
begin
  for i:= 0 to 10 do
  begin
    MnuItem := TMenuItem.Create(MainMenu);
    MnuItem.Caption := Format('MenuItem %d',[i]);
    MainMenu.Items.Add(MnuItem);
    for j:= 0 to 20 do
    begin
      SubItem := TMenuItem.Create(MainMenu);
      SubItem.Caption := Format('SubMenuItem %d',[j]);
      SubItem.OnClick := MenuItemClick;
      MnuItem.Add(SubItem);
      if odd(j) then
        for x:=0 to 10 do
        begin
          SubSubItem := TMenuItem.Create(MainMenu);
          SubSubItem.Caption := Format('SubSubMenuItem %d',[x]);
          SubSubItem.OnClick := MenuItemClick;
          SubItem.Add(SubSubItem);
          SubItem.OnClick := nil;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.MenuItemClick(Sender: TObject);
begin
  ShowMessage(TMenuItem(Sender).Caption);
end;
{------------------------------------------------------------------------------}
procedure TForm1.btnCopyToTreeViewClick(Sender: TObject);
begin
  gtMenuTreeView1.MapEvents := chkMapEvents.Checked;
  gtMenuTreeView1.PopulateMenu(MainMenu);
end;
{------------------------------------------------------------------------------}
procedure TForm1.Button1Click(Sender: TObject);
begin
  gtMenuTreeView1.Items.Clear;
end;
{------------------------------------------------------------------------------}
end.
