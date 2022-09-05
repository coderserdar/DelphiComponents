unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, Menus, ComCtrls,
  TB2Dock, TB2Toolbar, TB2Item, TB2ExtItems,
  SpTBXSkins, SpTBXItem, SpTBXControls, SpTBXEditors, SpTBXFormPopupMenu,
  SpTBXDkPanels;

type
  TForm1 = class(TForm)
    SpTBXDock2: TSpTBXDock;
    SpTBXToolbar2: TSpTBXToolbar;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXStatusBar1: TSpTBXStatusBar;
    SpTBXTitleBar1: TSpTBXTitleBar;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXMultiDock1: TSpTBXMultiDock;
    SpTBXDockablePanel1: TSpTBXDockablePanel;
    SpTBXRadioGroup1: TSpTBXRadioGroup;
    SpTBXCheckBox1: TSpTBXCheckBox;
    SpTBXGroupBox1: TSpTBXGroupBox;
    SpTBXGroupBox2: TSpTBXGroupBox;
    SpTBXButtonEdit1: TSpTBXButtonEdit;
    SpTBXButton1: TSpTBXButton;
    SpTBXButtonEdit2: TSpTBXButtonEdit;
    SpTBXButton2: TSpTBXButton;
    SpTBXFormPopupMenu1: TSpTBXFormPopupMenu;
    SpTBXFormPopupMenu2: TSpTBXFormPopupMenu;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXSkinGroupItem1: TSpTBXSkinGroupItem;
    SpTBXLabelItem1: TSpTBXLabelItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXLabelItem2: TSpTBXLabelItem;
    procedure SpTBXSubmenuItem2DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure SpTBXCheckBox1Click(Sender: TObject);
    procedure SpTBXRadioGroup1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpTBXFormPopupMenu1BeforeClosePopup(Sender: TObject;
      Selected: Boolean);
    procedure SpTBXFormPopupMenu2BeforeClosePopup(Sender: TObject;
      Selected: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Unit2, Unit3;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  SpTBXFormPopupMenu1.PopupForm := Form2;
  SpTBXFormPopupMenu2.PopupForm := Form3;
end;

procedure TForm1.SpTBXCheckBox1Click(Sender: TObject);
begin
  SpTBXFormPopupMenu1.PopupFocus := SpTBXCheckBox1.Checked;
  SpTBXFormPopupMenu2.PopupFocus := SpTBXCheckBox1.Checked;
end;

procedure TForm1.SpTBXRadioGroup1Click(Sender: TObject);
var
  I: Integer;
begin
  I := SpTBXRadioGroup1.ItemIndex;
  if I > -1 then begin
    SpTBXFormPopupMenu1.BorderStyle := TSpTBXPopupBorderStyleType(I);
    SpTBXFormPopupMenu2.BorderStyle := TSpTBXPopupBorderStyleType(I);
  end;
end;

procedure TForm1.SpTBXSubmenuItem2DrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  // Don't draw the items background
  if (PaintStage = pstPrePaint) and (CurrentSkin.SkinName = 'Default') then
    PaintDefault := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Popups }

procedure TForm1.SpTBXFormPopupMenu1BeforeClosePopup(Sender: TObject;
  Selected: Boolean);
begin
  if Selected then begin
    SpTBXButton1.Caption := DateToStr(Form2.MonthCalendar1.Date);
    SpTBXButtonEdit1.Text := DateToStr(Form2.MonthCalendar1.Date);
  end;
end;

procedure TForm1.SpTBXFormPopupMenu2BeforeClosePopup(Sender: TObject;
  Selected: Boolean);
begin
  if Selected then begin
    SpTBXButton2.Caption := Form3.TreeView1.Selected.Text;
    SpTBXButtonEdit2.Text := Form3.TreeView1.Selected.Text;
  end;
end;

end.

