unit Demo1;

{
  Toolbar2000 demo project

  $jrsoftware: tb2k/DemoProj/Demo1.pas,v 1.9 2006/03/12 23:11:58 jr Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, ActnList, ImgList, ComCtrls,
  TB2Item, TB2ExtItems, TB2Dock, TB2Toolbar, TB2MRU;

type
  TForm1 = class(TForm)
    ActionList: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actPageSetup: TAction;
    actPrint: TAction;
    actExit: TAction;
    actUndo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actDelete: TAction;
    actSelectAll: TAction;
    actTimeDate: TAction;
    actWordWrap: TAction;
    actSetFont: TAction;
    actFind: TAction;
    actFindNext: TAction;
    actReplace: TAction;
    actHelpTopics: TAction;
    actAbout: TAction;
    actTestButton: TAction;
    actTestEdit: TTBEditAction;
    DockTop: TTBDock;
    MenuToolbar: TTBToolbar;
    ImageList: TTBImageList;
    FMenu: TTBSubmenuItem;
    FNew: TTBItem;
    FOpen: TTBItem;
    FSave: TTBItem;
    FSaveAs: TTBItem;
    FPageSetup: TTBItem;
    FPrint: TTBItem;
    FExit: TTBItem;
    EMenu: TTBSubmenuItem;
    EUndo: TTBItem;
    ECut: TTBItem;
    ECopy: TTBItem;
    EPaste: TTBItem;
    EDelete: TTBItem;
    ESelectAll: TTBItem;
    ETimeDate: TTBItem;
    EWordWrap: TTBItem;
    ESetFont: TTBItem;
    SMenu: TTBSubmenuItem;
    SFind: TTBItem;
    SFindNext: TTBItem;
    SReplace: TTBItem;
    HMenu: TTBSubmenuItem;
    HHelpTopics: TTBItem;
    HAbout: TTBItem;
    FMRU: TTBMRUListItem;
    NBSeparatorItem3: TTBSeparatorItem;
    NBSeparatorItem4: TTBSeparatorItem;
    TestEdit: TTBEditItem;
    MainToolbar: TTBToolbar;
    NewButton: TTBItem;
    OpenButton: TTBItem;
    SaveButton: TTBItem;
    DockBottom: TTBDock;
    DockLeft: TTBDock;
    DockRight: TTBDock;
    BottomToolbar: TTBToolbar;
    PopupMenuBarButton: TTBItem;
    PopupToolbarButton: TTBItem;
    Memo1: TMemo;
    ContextPopupMenu: TTBPopupMenu;
    StatusBar: TStatusBar;
    ToolbarPopupMenu: TTBPopupMenu;
    MRUList: TTBMRUList;
    PrintButton: TTBItem;
    CutButton: TTBItem;
    CopyButton: TTBItem;
    PasteButton: TTBItem;
    UndoButton: TTBSubmenuItem;
    actPrintPreview: TAction;
    TBItem1: TTBItem;
    FormatToolbar: TTBToolbar;
    BoldItem: TTBItem;
    ItalicItem: TTBItem;
    UnderlineItem: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    AlignLeftItem: TTBItem;
    CenterItem: TTBItem;
    AlignRightItem: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    BulletsItem: TTBItem;
    NBItem34: TTBItem;
    NBSeparatorItem6: TTBSeparatorItem;
    NBItem35: TTBItem;
    NBItem36: TTBItem;
    NBItem37: TTBItem;
    NBItem38: TTBItem;
    NBSeparatorItem7: TTBSeparatorItem;
    NBItem39: TTBItem;
    ComboBox1: TComboBox;
    TBControlItem1: TTBControlItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBControlItem2: TTBControlItem;
    ComboBox2: TComboBox;
    actBold: TAction;
    actItalic: TAction;
    actUnderline: TAction;
    actAlignLeft: TAction;
    actCenter: TAction;
    actAlignRight: TAction;
    TBSeparatorItem4: TTBSeparatorItem;
    actRedo: TAction;
    RedoButton: TTBSubmenuItem;
    TBItem2: TTBItem;
    TBItem3: TTBItem;
    TBItem4: TTBItem;
    ERedo: TTBItem;
    VMenu: TTBSubmenuItem;
    VToolbars: TTBSubmenuItem;
    VTStandard: TTBVisibilityToggleItem;
    VTFormatting: TTBVisibilityToggleItem;
    VTBottom: TTBVisibilityToggleItem;
    VStatusBar: TTBItem;
    actBullets: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ItemClick(Sender: TObject);
    procedure PopupMenuBarButtonClick(Sender: TObject);
    procedure PopupToolbarButtonClick(Sender: TObject);
    procedure MRUListClick(Sender: TObject; const Filename: String);
    procedure AlignClick(Sender: TObject);
    procedure BulletsItemClick(Sender: TObject);
    procedure BoldItemClick(Sender: TObject);
    procedure ItalicItemClick(Sender: TObject);
    procedure UnderlineItemClick(Sender: TObject);
    procedure VStatusBarClick(Sender: TObject);
    procedure VMenuClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnHint(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  {$IFDEF CLR} Types, {$ENDIF}
  CommCtrl, TB2Version;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Text := 'Running on ' + Toolbar2000VersionPropText;
  Application.OnHint := OnHint;
end;

procedure TForm1.OnHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TForm1.ItemClick(Sender: TObject);
begin
  Application.MessageBox( {$IFNDEF CLR} PChar {$ENDIF}
    ('You selected:'#13#10#13#10 + (Sender as TComponent).Name),
    'OnClick handler', MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.MRUListClick(Sender: TObject; const Filename: String);
begin
  Application.MessageBox( {$IFNDEF CLR} PChar {$ENDIF}
    (Format('You selected "%s" from the MRU list.', [Filename])),
    'MRUListClick', MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.VMenuClick(Sender: TObject);
begin
  VStatusBar.Checked := StatusBar.Visible;
end;

procedure TForm1.VStatusBarClick(Sender: TObject);
begin
  { Force the StatusBar to always be at the bottom of the form. Without this
    line of code, the status bar sometimes may appear above the bottom dock.
    This is not a bug in Toolbar2000, but rather is due to the design of the
    VCL's alignment system. }
  StatusBar.Top := ClientHeight;

  { Toggle the status bar's visibility }
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TForm1.AlignClick(Sender: TObject);
begin
  AlignLeftItem.Checked := (Sender = actAlignLeft);
  CenterItem.Checked := (Sender = actCenter);
  AlignRightItem.Checked := (Sender = actAlignRight);
end;

procedure TForm1.BulletsItemClick(Sender: TObject);
begin
  BulletsItem.Checked := not BulletsItem.Checked;
end;

procedure TForm1.BoldItemClick(Sender: TObject);
begin
  BoldItem.Checked := not BoldItem.Checked;
end;

procedure TForm1.ItalicItemClick(Sender: TObject);
begin
  ItalicItem.Checked := not ItalicItem.Checked;
end;

procedure TForm1.UnderlineItemClick(Sender: TObject);
begin
  UnderlineItem.Checked := not UnderlineItem.Checked;
end;

procedure TForm1.PopupMenuBarButtonClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Memo1.ClientToScreen(Point(8, 8));
  MenuToolbar.Items.Popup(P.X, P.Y, True);
end;

procedure TForm1.PopupToolbarButtonClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Memo1.ClientToScreen(Point(8, 8));
  MainToolbar.Items.Popup(P.X, P.Y, True);
end;

end.
