unit Main;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QCLXToolbar, QComCtrls, QExtCtrls, QImgList, QMenus, QTypes;

type
  TFormMain = class(TForm)
    CLXDockTop: TCLXDock;
    CLXDockLeft: TCLXDock;
    ToolFile: TCLXToolbar;
    ToolFont: TCLXToolbar;
    ToolParagraph: TCLXToolbar;
    DockingMemo: TCLXDockingControl;
    DockingTreeView: TCLXDockingControl;
    BOpen: TCLXToolButton;
    BNew: TCLXToolButton;
    BPrint: TCLXToolButton;
    BSave: TCLXToolButton;
    BPreview: TCLXToolButton;
    CLXSeparator1: TCLXSeparator;
    BBulett: TCLXToolButton;
    BRight: TCLXToolButton;
    BLeft: TCLXToolButton;
    BCenter: TCLXToolButton;
    BFontName: TComboBox;
    BFontSize: TSpinEdit;
    BFont: TCLXToolButton;
    ImageListE: TImageList;
    ImageListD: TImageList;
    CLXDockRight: TCLXDock;
    CLXDockBottom: TCLXDock;
    BParagraph: TCLXToolButton;
    ToolEdit: TCLXToolbar;
    BCopy: TCLXToolButton;
    BPaste: TCLXToolButton;
    BCut: TCLXToolButton;
    BUndo: TCLXToolButton;
    Memo1: TMemo;
    TreeView1: TTreeView;
    ImageListFileE: TImageList;
    ImageListFileD: TImageList;
    Memo2: TMemo;
    PopupMenu1: TPopupMenu;
    BPOpen: TMenuItem;
    History1: TMenuItem;
    N1: TMenuItem;
    History2: TMenuItem;
    History3: TMenuItem;
    History4: TMenuItem;
    MainMenu1: TMainMenu;
    MFile: TMenuItem;
    MNew: TMenuItem;
    MOpen: TMenuItem;
    MSave: TMenuItem;
    N2: TMenuItem;
    MPreview: TMenuItem;
    MPrint: TMenuItem;
    N3: TMenuItem;
    MExit: TMenuItem;
    MParagraph: TMenuItem;
    MCenter: TMenuItem;
    MLeft: TMenuItem;
    MRight: TMenuItem;
    MBulett: TMenuItem;
    MParagraph1: TMenuItem;
    MEdit: TMenuItem;
    MCut: TMenuItem;
    MCopy: TMenuItem;
    MPaste: TMenuItem;
    MUndo: TMenuItem;
    MView: TMenuItem;
    MToolFile: TMenuItem;
    MToolParagraph: TMenuItem;
    MToolEdit: TMenuItem;
    MToolFont: TMenuItem;
    MTreeView: TMenuItem;
    MDockingMemo: TMenuItem;
    Panel1: TPanel;
    MLeftPage: TMenuItem;
    MRightPage: TMenuItem;
    PopupMenu2: TPopupMenu;
    op1: TMenuItem;
    Bottom1: TMenuItem;
    Left1: TMenuItem;
    Right1: TMenuItem;
    N4: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure MViewClick(Sender: TObject);
    procedure MToolFileClick(Sender: TObject);
    procedure ToolFileAfterDocking(Sender: TCLXCustomControl;
      ADock: TCLXDock);
    procedure ToolFileAfterUndock(Sender: TCLXCustomControl;
      ADock: TCLXDock);
    procedure ToolFileBeforeDocking(Sender: TCLXCustomControl;
      ADock: TCLXDock; var Accept: Boolean);
    procedure ToolFileBeforeUndock(Sender: TCLXCustomControl;
      ADock: TCLXDock; var Accept: Boolean);
    procedure ToolFileClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolFileShow(Sender: TObject);
    procedure CLXDockLeftAfterDocking(Sender: TCLXDock;
      ADock: TCLXCustomControl);
    procedure CLXDockLeftBeforeDocking(Sender: TCLXDock;
      ADock: TCLXCustomControl; var Accept: Boolean);
    procedure CLXDockLeftAfterUndock(Sender: TCLXDock;
      ADock: TCLXCustomControl);
    procedure CLXDockLeftBeforeUndock(Sender: TCLXDock;
      ADock: TCLXCustomControl; var Accept: Boolean);
    procedure MToolParagraphClick(Sender: TObject);
    procedure MToolEditClick(Sender: TObject);
    procedure MToolFontClick(Sender: TObject);
    procedure MTreeViewClick(Sender: TObject);
    procedure MDockingMemoClick(Sender: TObject);
    procedure ToolFileCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MLeftPageClick(Sender: TObject);
    procedure op1Click(Sender: TObject);
    procedure Bottom1Click(Sender: TObject);
    procedure Left1Click(Sender: TObject);
    procedure Right1Click(Sender: TObject);
    procedure MRightPageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.xfm}
//{$R buttons.res}

procedure TFormMain.FormCreate(Sender: TObject);
//var Tmp: TBitmap;
begin
//  Tmp := TBitmap.Create;
//  Tmp.LoadFromResourceName(hInstance, 'FILES');
{  ImageListFileE.Clear;
  ImageListFileE.Add(Tmp, nil);}
end;

procedure TFormMain.BNewClick(Sender: TObject);
begin
  Memo2.Lines.Add('Click: ' + (Sender as TComponent).Name);
end;

procedure TFormMain.MExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.MViewClick(Sender: TObject);
begin
  MToolFile.Checked := ToolFile.Visible;
  MToolParagraph.Checked := ToolParagraph.Visible;
  MToolEdit.Checked := ToolEdit.Visible;
  MToolFont.Checked := ToolFont.Visible;
  MTreeView.Checked := DockingTreeView.Visible;
  MDockingMemo.Checked := DockingMemo.Visible;
  
end;

procedure TFormMain.MToolFileClick(Sender: TObject);
begin
  ToolFile.Visible := not ToolFile.Visible;
end;

procedure TFormMain.ToolFileAfterDocking(Sender: TCLXCustomControl;
  ADock: TCLXDock);
begin
  Memo2.Lines.Add('Control.AfterDocking. Sender: ' +
    Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.ToolFileAfterUndock(Sender: TCLXCustomControl;
  ADock: TCLXDock);
begin
  Memo2.Lines.Add('Control.AfterUndock. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.ToolFileBeforeDocking(Sender: TCLXCustomControl;
  ADock: TCLXDock; var Accept: Boolean);
begin
  Memo2.Lines.Add('Control.BeforeDocking. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.ToolFileBeforeUndock(Sender: TCLXCustomControl;
  ADock: TCLXDock; var Accept: Boolean);
begin
  Memo2.Lines.Add('Control.BeforeUndock. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.ToolFileClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Memo2.Lines.Add('Close: ' + (Sender as TComponent).Name);
end;

procedure TFormMain.ToolFileShow(Sender: TObject);
begin
  Memo2.Lines.Add('Show: ' + (Sender as TComponent).Name);
end;

procedure TFormMain.CLXDockLeftAfterDocking(Sender: TCLXDock;
  ADock: TCLXCustomControl);
begin
  Memo2.Lines.Add('Dock.AfterDocking. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.CLXDockLeftBeforeDocking(Sender: TCLXDock;
  ADock: TCLXCustomControl; var Accept: Boolean);
begin
  Memo2.Lines.Add('Dock.BeforeDocking. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.CLXDockLeftAfterUndock(Sender: TCLXDock;
  ADock: TCLXCustomControl);
begin
  Memo2.Lines.Add('Dock.AfterUndock. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.CLXDockLeftBeforeUndock(Sender: TCLXDock;
  ADock: TCLXCustomControl; var Accept: Boolean);
begin
  Memo2.Lines.Add('Dock.BeforeUndock. Sender: ' +
   Sender.Name + '. Dock: ' + ADock.Name);
end;

procedure TFormMain.MToolParagraphClick(Sender: TObject);
begin
  ToolParagraph.Visible := not ToolParagraph.Visible;
end;

procedure TFormMain.MToolEditClick(Sender: TObject);
begin
  ToolEdit.Visible := not ToolEdit.Visible;
end;

procedure TFormMain.MToolFontClick(Sender: TObject);
begin
  ToolFont.Visible := not ToolFont.Visible;
end;

procedure TFormMain.MTreeViewClick(Sender: TObject);
begin
  DockingTreeView.Visible := not DockingTreeView.Visible;
end;

procedure TFormMain.MDockingMemoClick(Sender: TObject);
begin
  DockingMemo.Visible := not DockingMemo.Visible;
end;

procedure TFormMain.ToolFileCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Memo2.Lines.Add('CloseQuery: ' + (Sender as TComponent).Name);
  CanClose := MessageDlg('Would you like to close Component ' +
  (Sender as TComponent).Name + '?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

procedure TFormMain.MLeftPageClick(Sender: TObject);
begin
  if CLXDockLeft.Mode = dmSimple then
  begin
    MLeftPage.Caption := 'Set left simple';
    CLXDockLeft.Mode := dmPage;
  end
  else
  begin
    MLeftPage.Caption := 'Set left page';
    CLXDockLeft.Mode := dmSimple;
  end;
end;

procedure TFormMain.op1Click(Sender: TObject);
begin
  ToolFile.ActiveDock := CLXDockTop;
end;

procedure TFormMain.Bottom1Click(Sender: TObject);
begin
ToolFile.ActiveDock := CLXDockBottom;
end;

procedure TFormMain.Left1Click(Sender: TObject);
begin
ToolFile.ActiveDock := CLXDockLeft;
end;

procedure TFormMain.Right1Click(Sender: TObject);
begin
ToolFile.ActiveDock := CLXDockRight;
end;

procedure TFormMain.MRightPageClick(Sender: TObject);
begin
  if CLXDockRight.Mode = dmSimple then
  begin
    MRightPage.Caption := 'Set right simple';
    CLXDockRight.Mode := dmPage;
  end
  else
  begin
    MRightPage.Caption := 'Set right page';
    CLXDockRight.Mode := dmSimple;
  end;
end;

end.
