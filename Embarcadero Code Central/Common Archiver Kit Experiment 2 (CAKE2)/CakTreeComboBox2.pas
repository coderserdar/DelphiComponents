unit CakTreeComboBox2;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, CakTreeView2,StdCtrls, CakListView2, Cakdir2, ExtCtrls;

type
  TCakTreeComboBox2 = class;
  TDropDownForm = class(TForm)
    CAKTreeView21: TCAKTreeView2;
    procedure CAKTreeView21Change(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    TreeCombo : TCakTreeComboBox2;
    procedure WMACTIVATE(var Msg: TMessage); message WM_ACTIVATE;
  public
    { Public declarations }
  end;

  TCakTreeComboBox2 = class(TPanel)
  private
    { Private declarations }
    aPanel : TPanel;
    folderBitmap : TBitmap;
    DropDownForm   : TDropDownForm;
    Canvas :  TControlCanvas;
    DropDownFormShown : boolean;
    procedure SetCakdir(aCakdir : TCakdir2);
    procedure SetSelectPath(value : string);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    { Protected declarations }
  public
    { Public declarations }
    CakDir: TCakDir2;
    sPath : string;
    Passive, UpdCak : Boolean;
    TreeViewHeight : integer;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure DropDownClick(Sender : TObject);

    procedure PaintWindow(DC: HDC); override;
    procedure Paint; dynamic;
    procedure ReloadCAK;
  published
    { Published declarations }
    property CakDir2 : TCakDir2 read CakDir write SetCakDir;
    property SelectPath : string read sPath write SetSelectPath;
    property PassiveMode : boolean read Passive write Passive;
    property UpdateCak : boolean read UpdCak write UpdCak;
  end;

  procedure Register;

var
  DropDownForm: TDropDownForm;

implementation

{$R *.dfm}

procedure Register;
begin
  RegisterComponents('CAKE', [TCakTreeComboBox2]);
end;

constructor TCakTreeComboBox2.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Height := 22;
  Canvas := TControlCanvas.Create;
  Canvas.Control := Self;
  TreeViewHeight := 150;

  aPanel := TPanel.Create(Self);
  aPanel.Parent := Self;
  aPanel.Align := alRight;
  aPanel.Width := 14;
  aPanel.BorderWidth := 0;
  aPanel.Font.Name := 'Wingdings 3';
  aPanel.Font.Size := 6;
  aPanel.caption := chr(113);
  aPanel.BevelInner := bvNone;
  aPanel.BevelOuter := bvSpace;
  aPanel.BevelWidth := 1;
  aPanel.OnClick := DropDownClick;

//  aImage := TImage.Create(Self);
//  aImage.Parent := Self;
//  aImage.Align := alClient;
//  aImage.Color := clWindow;
//  aLabel.BevelInner := bvNone;
//  aLabel.BevelOuter := bvNone;
//  aLabel.BorderStyle := bsNone;

  Self.BevelInner  := bvNone;
  Self.BevelOuter  := bvSpace;
  Self.BorderStyle := bsSingle;
  Self.BevelWidth  := 1;
  Self.Color       := clWindow;
  Self.Caption     := '';
  DropDownForm     := TDropDownForm.Create(Self);

  Passive := false;
  UpdCak := true;
  DropDownFormShown := false;

  folderBitmap := TBitmap.Create;
  folderBitmap.LoadFromResourceName(HInstance,'FOLDER_OPEN_16');
end;

destructor TCakTreeComboBox2.Destroy;
begin
  DropDownForm.Free;
  Canvas.Free;
  folderBitmap.Free;
  inherited Destroy;
end;

procedure TCakTreeComboBox2.DropDownClick(Sender : TObject);
begin
  DropDownForm.Top   := Self.ClientOrigin.Y + Self.Height - 4;
  DropDownForm.Left  := Self.ClientOrigin.X - 2;
  DropDownForm.Width := Self.Width;
  DropDownForm.Height := TreeViewHeight;
  DropDownForm.TreeCombo := Self;
  DropDownForm.CAKTreeView21.UpdateCak := UpdateCak;
  DropDownForm.CAKTreeView21.CakDir := Cakdir2;
  DropDownForm.CAKTreeView21.ReloadCAK;
  DropDownForm.CAKTreeView21.setselectpath(sPath);
  DropDownForm.Show;
  DropDownFormShown := true;
end;

procedure TCakTreeComboBox2.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> DropDownForm) and
     (DropDownForm.Visible) then
  DropDownForm.Close;
end;

procedure TDropDownForm.WMACTIVATE(var Msg: TMessage);
begin
  if LOWORD(Msg.wParam) = WA_INACTIVE then
    Close;
end;

procedure TCakTreeComboBox2.SetSelectPath(value : string);
begin
  sPath := value;
  Invalidate;
end;

procedure TCakTreeComboBox2.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;


procedure TCakTreeComboBox2.PaintWindow(DC: HDC);
begin
  inherited paintWindow(DC);
  Paint;
end;


procedure TCakTreeComboBox2.paint;
begin
  Canvas.FillRect(Rect(0,0,Width-15,Height));
  Canvas.Draw(2,0,FolderBitmap);
  if (Assigned(Cakdir)) and not Passive then
  Canvas.TextOut(20,2,'\' + Cakdir.BaseDir) else
  if DropDownFormShown and (Extractfilepath(DropDownForm.CAKTreeView21.getselectedpath) <> '') then
  Canvas.TextOut(20,2,'\' + Extractfilepath(DropDownForm.CAKTreeView21.getselectedpath)) else
  Canvas.TextOut(20,2,'\')
end;

procedure TCakTreeComboBox2.SetCakdir(aCakdir : TCakdir2);
begin
   Cakdir := aCakdir;
   if assigned(Cakdir) then
   Cakdir._SetTreeComboBox(Self);
end;

procedure TCakTreeComboBox2.ReloadCAK;
begin
  Invalidate;
end;

procedure TDropDownForm.CAKTreeView21Change(Sender: TObject;
  Node: TTreeNode);
begin
 TreeCombo.SelectPath := CakTreeview21.getselectedpath;
 Close;
end;

end.
