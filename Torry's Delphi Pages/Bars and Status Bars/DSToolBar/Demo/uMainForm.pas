unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, ImgList, ExtCtrls, StdCtrls, Menus, ActnList,
  DsDockSite, DsToolBar, DsManager, CheckLst;


type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    DockTop: TDsDockSite;
    DockLeft: TDsDockSite;
    DockRight: TDsDockSite;
    DockBottom: TDsDockSite;
    ToolBar1: TDSToolBar;
    ToolBar8: TDSToolBar;
    ToolBar4: TDSToolBar;
    ToolBar5: TDSToolBar;
    ToolBar3: TDSToolBar;
    ToolBar6: TDSToolBar;
    ToolBar10: TDSToolBar;
    ToolBar11: TDSToolBar;
    ToolBar12: TDSToolBar;
    ToolButton13: TDSToolButton;
    ToolButton14: TDSToolButton;
    ToolButton15: TDSToolButton;
    ToolButton16: TDSToolButton;
    ToolButton17: TDSToolButton;
    ToolButton18: TDSToolButton;
    ToolButton19: TDSToolButton;
    ToolButton20: TDSToolButton;
    ToolButton21: TDSToolButton;
    ToolButton22: TDSToolButton;
    ToolButton23: TDSToolButton;
    ToolButton24: TDSToolButton;
    ToolButton25: TDSToolButton;
    ToolButton26: TDSToolButton;
    ToolButton27: TDSToolButton;
    ToolButton28: TDSToolButton;
    ToolButton29: TDSToolButton;
    ToolButton30: TDSToolButton;
    ToolButton31: TDSToolButton;
    ToolButton32: TDSToolButton;
    ToolButton33: TDSToolButton;
    ToolButton34: TDSToolButton;
    ToolButton35: TDSToolButton;
    ToolButton36: TDSToolButton;
    ToolButton37: TDSToolButton;
   ToolButton38: TDSToolButton;
    ToolButton39: TDSToolButton;
    ToolButton40: TDSToolButton;
    ToolButton41: TDSToolButton;
    ToolButton42: TDSToolButton;
    ToolButton43: TDSToolButton;
    ToolButton44: TDSToolButton;
    ToolButton45: TDSToolButton;
    ToolButton46: TDSToolButton;
    ToolButton47: TDSToolButton;
    ToolButton48: TDSToolButton;
    PopupMenu1: TPopupMenu;
    miTest1: TMenuItem;
    miTest2: TMenuItem;
    miTest3: TMenuItem;
    MdToolBar1: TDSToolBar;
    MdToolButton1: TDSToolButton;
    MdToolButton2: TDSToolButton;
    MdToolBar2: TDSToolBar;
    MdToolButton3: TDSToolButton;
    MdToolButton4: TDSToolButton;
    MdToolButton5: TDSToolButton;
    MdToolButton6: TDSToolButton;
    ToolBar2: TDSToolBar;
    ToolButton9: TDSToolButton;
    ToolButton12: TDSToolButton;
    ToolButton10: TDSToolButton;
    MyToolButton1: TDSToolButton;
    ToolBar7: TDSToolBar;
    ToolButton4: TDSToolButton;
    ToolButton5: TDSToolButton;
    ToolButton7: TDSToolButton;
    ToolButton6: TDSToolButton;
    MdToolButton7: TDSToolButton;
    MdToolButton8: TDSToolButton;
    MdToolButton9: TDSToolButton;
    MdToolButton10: TDSToolButton;
    MdToolButton11: TDSToolButton;
    MdToolButton13: TDSToolButton;
    MdToolButton12: TDSToolButton;
    MdToolBar3: TDSToolBar;
    MdToolButton14: TDSToolButton;
    ComboBox3: TComboBox;
    MdToolButton15: TDSToolButton;
    MdToolBar4: TDSToolBar;
    ProgressBar1: TProgressBar;
    barMenu: TDSToolBar;
    MdToolBar6: TDSToolBar;
    MdToolButton18: TDSToolButton;
    MdToolButton19: TDSToolButton;
    MdToolButton23: TDSToolButton;
    MdToolButton24: TDSToolButton;
    MdToolButton25: TDSToolButton;
    ActionList1: TActionList;
    actNew: TAction;
    DsToolButtonGroup1: TDSToolButtonGroup;
    PopupMenu2: TPopupMenu;
    miLockTop: TMenuItem;
    miLockLeft: TMenuItem;
    miLockRight: TMenuItem;
    miLockBottom: TMenuItem;
    N1: TMenuItem;
    miLockall: TMenuItem;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    miEdit: TMenuItem;
    asdfasdf1: TMenuItem;
    asdfasfd1: TMenuItem;
    asdfasdf2: TMenuItem;
    miSearch: TMenuItem;
    Find1: TMenuItem;
    miView: TMenuItem;
    Hide1: TMenuItem;
    miAbout: TMenuItem;
    Help1: TMenuItem;
    miUnlockAll: TMenuItem;
    pnlMain: TPanel;
    actNew1: TMenuItem;
    actCheck: TAction;
    btnSave: TButton;
    DsManager1: TDsManager;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure miTest1Click(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure miLockTopClick(Sender: TObject);
    procedure miLockLeftClick(Sender: TObject);
    procedure miLockRightClick(Sender: TObject);
    procedure miLockBottomClick(Sender: TObject);
    procedure miLockallClick(Sender: TObject);
    procedure btnButtonSizeClick(Sender: TObject);
    procedure btnToolBarHeightClick(Sender: TObject);
    procedure miUnlockAllClick(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure actCheckExecute(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

  private
    { Private declarations }
    procedure MenuClick(Sender: TObject);
  public
    { Public declarations }
    procedure UpdateDockMenu;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateDockMenu;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
  vMenu: TMenuItem;
begin
  DsManager1.fileName := ChangeFileExt(ParamStr(0), '.INI');
  DsManager1.loadFromFile;
  for i := DsManager1.Count - 1 downto 0  do begin
    vMenu := TMenuItem.Create(MainMenu1);
    vMenu.Caption := TControl(DsManager1.Toolbars[i]).Name;
    vMenu.Checked := TControl(DsManager1.Toolbars[i]).Visible;
    vMenu.OnClick := MenuClick;
    vMenu.Tag := Integer(Pointer(DsManager1.Toolbars[i]));
    PopupMenu2.Items.Insert(0, vMenu);
  end;

end;

procedure TForm1.MenuClick(Sender: TObject);
var
  vToolBar: TDSToolBar;
begin
  vToolBar := TDSToolBar(TMenuItem(Sender).Tag);
  vToolBar.Visible := not vToolBar.Visible;
  TMenuItem(Sender).Checked := vToolBar.Visible;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if barMenu.Menu <> nil then
    barMenu.Menu := nil
  else
    barMenu.Menu := MainMenu1;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  ShowMessage('Clicked');
end;

procedure TForm1.miTest1Click(Sender: TObject);
begin
  ShowMessage('MenuClick');
end;

procedure TForm1.actNewExecute(Sender: TObject);
var
  vChecked: Boolean;
begin
  vChecked := actNew.Checked;
  if vChecked then
    ShowMessage('Action: Checked')
  else
    ShowMessage('Action: Not checked')
end;

procedure TForm1.N11Click(Sender: TObject);
begin
  ShowMessage('Main Menu Click');
end;

procedure TForm1.miLockTopClick(Sender: TObject);
begin
  DockTop.Locked := not DockTop.Locked;
  UpdateDockMenu;
end;

procedure TForm1.miLockLeftClick(Sender: TObject);
begin
  DockLeft.Locked := not DockLeft.Locked;
  UpdateDockMenu;
end;

procedure TForm1.miLockRightClick(Sender: TObject);
begin
  DockRight.Locked := not DockRight.Locked;
  UpdateDockMenu;
end;

procedure TForm1.miLockBottomClick(Sender: TObject);
begin
  DockBottom.Locked := not DockBottom.Locked;
  UpdateDockMenu;
end;

procedure TForm1.miLockallClick(Sender: TObject);
begin
  DockTop.Locked := True;
  DockLeft.Locked := True;
  DockRight.Locked := True;
  DockBottom.Locked := True;

  UpdateDockMenu;
end;

procedure TForm1.miUnlockAllClick(Sender: TObject);
begin
  DockTop.Locked := False;
  DockLeft.Locked := False;
  DockRight.Locked := False;
  DockBottom.Locked := False;

  UpdateDockMenu;
end;

procedure TForm1.UpdateDockMenu;
begin
  miLockTop.Checked := DockTop.Locked;
  miLockLeft.Checked := DockLeft.Locked;
  miLockRight.Checked := DockRight.Locked;
  miLockBottom.Checked := DockBottom.Locked;

  miUnlockAll.Enabled := DockTop.Locked or DockLeft.Locked or DockRight.Locked or DockBottom.Locked;
  miLockall.Enabled := not (DockTop.Locked and DockLeft.Locked and DockRight.Locked and DockBottom.Locked);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MdToolBar1.Width := MdToolBar1.Width div 2;
end;

procedure TForm1.btnButtonSizeClick(Sender: TObject);
begin
  MdToolButton2.Width := MdToolButton2.Width + 5;
end;

procedure TForm1.btnToolBarHeightClick(Sender: TObject);
begin
  MdToolBar1.Height := MdToolBar1.Height - 5;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
var
  vClr, vClrTo: TColor;
begin
  case ComboBox3.ItemIndex of
  1:
  begin
    vClr := $00ADB8BE;
    vClrTo := $00D9E9EB;
  end
  else
  begin
    vClr := $00F5B789;
    vClrTo := $00FFDEDE;
  end
  end;

  DockTop.Color := vClr;
  DockLeft.Color := vClr;
  DockRight.Color := vClrTo;
  DockBottom.Color := vClrTo;


  DockTop.ColorTo := vClrTo;
  DockLeft.ColorTo := vClrTo;
  DockRight.ColorTo := vClr;
  DockBottom.ColorTo := vClr;
end;

procedure TForm1.actCheckExecute(Sender: TObject);
begin
  if actCheck.Checked then
    ShowMessage('Checked')
  else
    ShowMessage('not Checked');
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
   DsManager1.saveToFile();
end;

end.
