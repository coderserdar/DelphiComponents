{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELTrayIcon                                             }
{                                                          }
{  Copyright (c) 2001 - 2002, Balabuyev Yevgeny            }
{  Contact: yebalabuev@devrace.com                         }
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

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELControls, ImgList, Menus, ExtCtrls;

type
  TForm1 = class(TForm)
    ELTrayIcon1: TELTrayIcon;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ImageList1: TImageList;
    Button6: TButton;
    ImageList2: TImageList;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Button7: TButton;
    Label2: TLabel;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ImageList3: TImageList;
    Label3: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    PopupMenu1: TPopupMenu;
    Item11: TMenuItem;
    Item21: TMenuItem;
    Item31: TMenuItem;
    Bevel1: TBevel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ELTrayIcon1Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateEnableds;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ELTrayIcon1.Active := not ELTrayIcon1.Active;

  if ELTrayIcon1.Active then
    Button1.Caption := 'Deactivate'
  else
    Button1.Caption := 'Activate';
  UpdateEnableds;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ELTrayIcon1.HideTaskBarButton := not ELTrayIcon1.HideTaskBarButton;
  if ELTrayIcon1.HideTaskBarButton then
    Button2.Caption := 'Show task bar button'
  else
    Button2.Caption := 'Hide task bar button';
end;

procedure TForm1.UpdateEnableds;
var
  LE: Boolean;
begin
  LE := ELTrayIcon1.Active;
  Button2.Enabled := LE;
  Button3.Enabled := LE;
  Button4.Enabled := LE;
  Button5.Enabled := LE;
  Button6.Enabled := LE;

  Label3.Enabled := LE;
  Edit1.Enabled := LE;

  Label1.Enabled := LE;
  Button7.Enabled := LE;
  Label2.Enabled := LE;
  Button8.Enabled := LE;
  Button9.Enabled := LE;
  CheckBox1.Enabled := LE;
  CheckBox2.Enabled := LE;
  CheckBox3.Enabled := LE;
  CheckBox4.Enabled := LE;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateEnableds;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ImageList1.GetIcon(0, ELTrayIcon1.Icon);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ImageList1.GetIcon(1, ELTrayIcon1.Icon);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ImageList1.GetIcon(2, ELTrayIcon1.Icon);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ELTrayIcon1.Icon := Application.Icon;

  // You can use code:
  //   ELTrayIcon1.Icon.ReleaseHandle;
  // to set application icon becouse
  // clearing icon always set Application.Icon
  // into tray
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[0].Start;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[1].Start;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  if ELTrayIcon1.Animations.ActiveItem <> nil then
    ELTrayIcon1.Animations.ActiveItem.Stop;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[0].Circular := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[1].Circular := CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[0].RestoreOldIcon := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  ELTrayIcon1.Animations[1].RestoreOldIcon := CheckBox4.Checked;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ELTrayIcon1.Hint := Edit1.Text;
  
  // You can use Application.Hint as tray icon hint
  // by assigning empty string to hint property
end;

procedure TForm1.ELTrayIcon1Click(Sender: TObject);
begin
  Visible := not Visible;
end;

end.
