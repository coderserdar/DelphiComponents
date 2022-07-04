unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgCtrls, ImgList, vgStndrt, ComCtrls;

type
  TMainForm = class(TForm)
    ilImages: TImageList;
    GroupBox3: TGroupBox;
    chRowSelect: TCheckBox;
    chIndent: TCheckBox;    
    chImages: TCheckBox;
    chFont: TCheckBox;
    chBkgnd: TCheckBox;
    AppIniFile1: TAppIniFile;
    PropStorage1: TPropStorage;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    lb: TvgListBox;
    GroupBox2: TGroupBox;
    cb: TvgComboBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    tv: TvgTreeView;
    tc: TvgTreeViewCombo;
    lv: TvgListView;
    procedure FormCreate(Sender: TObject);
    procedure lbGetItemIndent(Sender: TObject; Index: Integer;
      var Indent: Integer);
    procedure lbGetItemParams(Sender: TObject; Index: Integer;
      State: TOwnerDrawState; AFont: TFont; var Background: TColor;
      var ImageIndex, StateIndex, OverlayIndex: Integer);
    procedure chRowSelectClick(Sender: TObject);
    procedure chIndentClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvGetItemParams(Sender: TObject; Node: TTreeNode;
      AFont: TFont; var Background: TColor; var State: TCustomDrawState);
    procedure lvGetItemParams(Sender: TObject; Item: TListItem;
      AFont: TFont; var Background: TColor; var State: TCustomDrawState);
    procedure tcTVOnGetItemParams(Sender: TObject; Node: TTreeNode;
      AFont: TFont; var Background: TColor; var State: TCustomDrawState);
  private
    { Private declarations }
    procedure GetItemParams(Sender: TObject; Index: Integer;
      AFont: TFont; var Background: TColor; var State: TCustomDrawState);
  public
    { Public declarations }
    procedure UpdateControls;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  S: string;
  Node: TTreeNode;
begin
  lb.Items.BeginUpdate;
  cb.Items.BeginUpdate;
  tv.Items.BeginUpdate;
  try
    for I := 0 to ilImages.Count - 1 do
    begin
      S := Format('Image number %d', [I]);
      lb.Items.Add(S);
      cb.Items.Add(S);

      with lv.Items.Add do
      begin
        Caption := S;
        ImageIndex := I;
      end;

      Node := tv.Items.AddChild(nil, S);
      Node.ImageIndex := I;
      Node.SelectedIndex := I;
    end;
  finally
    lb.Items.EndUpdate;
    cb.Items.EndUpdate;
    tv.Items.EndUpdate;
  end;
  tc.Items := tv.Items;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMainForm.UpdateControls;
begin
  lb.RowSelect := chRowSelect.Checked;
  cb.RowSelect := chRowSelect.Checked;
  lv.RowSelect := chRowSelect.Checked;
  tv.RowSelect := chRowSelect.Checked;
  tc.TVRowSelect := chRowSelect.Checked;
  if chImages.Checked then
  begin
    lb.Images := ilImages;
    cb.Images := ilImages;
    lv.SmallImages := ilImages;
    tv.Images := ilImages;
    tc.Images := ilImages;
  end else begin
    lb.Images := nil;
    cb.Images := nil;
    lv.SmallImages := nil;
    tv.Images := nil;
    tc.Images := nil;
  end;
end;

procedure TMainForm.lbGetItemIndent(Sender: TObject; Index: Integer;
  var Indent: Integer);
begin
  if chIndent.Checked then
    Indent := Index * 10 mod 50;
end;


type
  TControlHack = class(TControl);

procedure TMainForm.lbGetItemParams(Sender: TObject; Index: Integer;
  State: TOwnerDrawState; AFont: TFont; var Background: TColor;
  var ImageIndex, StateIndex, OverlayIndex: Integer);

const
  Colors : array[0..10] of TColor = (
   clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clLtGray, clDkGray);

begin
  if chFont.Checked and (odSelected in State) then
    AFont.Style := AFont.Style + [fsBold] else
    AFont.Style := AFont.Style - [fsBold];
  if chBkgnd.Checked then
  begin
    if odSelected in State then
    begin
      Background := clHighlight;
    end else
      Background := Colors[Index mod (High(Colors) + 1)];
  end else begin
    if odSelected in State then
      Background := clHighlight else
      Background := TControlHack(Sender).Color;
  end;
end;

procedure TMainForm.chRowSelectClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMainForm.GetItemParams(Sender: TObject; Index: Integer;
  AFont: TFont; var Background: TColor; var State: TCustomDrawState);
var
  AState: TOwnerDrawState;
  ImageIndex, StateIndex, OverlayIndex: Integer;
begin
  if cdsSelected in State then
    AState := [odSelected] else
    AState := [];

  if TWinControl(Sender).Focused then
    Include(AState, odFocused);

  lbGetItemParams(Sender, Index,
    AState, AFont, Background, ImageIndex, StateIndex, OverlayIndex);
end;

procedure TMainForm.chIndentClick(Sender: TObject);
begin
  lb.Invalidate;
  cb.Invalidate;
  lv.Invalidate;
  tv.Invalidate;
  tc.Invalidate;
end;

procedure TMainForm.tvGetItemParams(Sender: TObject; Node: TTreeNode;
  AFont: TFont; var Background: TColor; var State: TCustomDrawState);
begin
  GetItemParams(Sender, Node.Index, AFont, Background, State);
end;

procedure TMainForm.tcTVOnGetItemParams(Sender: TObject; Node: TTreeNode;
  AFont: TFont; var Background: TColor; var State: TCustomDrawState);
begin
  GetItemParams(tc, Node.Index, AFont, Background, State);
end;

procedure TMainForm.lvGetItemParams(Sender: TObject; Item: TListItem;
  AFont: TFont; var Background: TColor; var State: TCustomDrawState);
begin
  GetItemParams(Sender, Item.Index, AFont, Background, State);
end;

end.
