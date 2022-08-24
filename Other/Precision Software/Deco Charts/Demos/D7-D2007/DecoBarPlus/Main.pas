unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  DecoBarPlus, Menus, Grids, DecoBar;

type
  TForm3 = class(TForm)
    pOptions: TPanel;
    dlgColor: TColorDialog;
    gbBackground: TGroupBox;
    Label3: TLabel;
    shpBackColor: TShape;
    gbStyle: TGroupBox;
    cbRounded: TCheckBox;
    edRoundCorners: TEdit;
    udRoundCorners: TUpDown;
    cbGradient: TCheckBox;
    Label4: TLabel;
    edStripSize: TEdit;
    udStripSize: TUpDown;
    Label5: TLabel;
    udBarSize: TUpDown;
    edBarSize: TEdit;
    edTitle: TEdit;
    rgAlignment: TRadioGroup;
    dlgFont: TFontDialog;
    Label7: TLabel;
    sbFont: TButton;
    Bevel1: TBevel;
    Label8: TLabel;
    edItemTextFormat: TEdit;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    shpTreeLinesColor: TShape;
    Label10: TLabel;
    cbTreeLinesStyle: TComboBox;
    cbCutExpanded: TCheckBox;
    cbDarkenCollapsed: TCheckBox;
    Label11: TLabel;
    Label1: TLabel;
    shpFrame: TShape;
    Label2: TLabel;
    edFrameWidth: TEdit;
    udFrameWidth: TUpDown;
    Label12: TLabel;
    edCaptionFormat: TEdit;
    Label13: TLabel;
    sbCaptionFont: TButton;
    Label6: TLabel;
    pumDecoBar: TPopupMenu;
    DecoBarpopupmenu1: TMenuItem;
    N1: TMenuItem;
    miEditValue: TMenuItem;
    N2: TMenuItem;
    miCollapseall: TMenuItem;
    pMain: TPanel;
    cbShowLegend: TCheckBox;
    cbShowCaption: TCheckBox;
    lvData: TListView;
    sbSaveAs: TButton;
    cbAutoColors: TCheckBox;
    cbAutoCalc: TCheckBox;
    DecoBar1: TDecoBarPlus;
    miDeleteitem: TMenuItem;
    miEditcaption: TMenuItem;
    miAdditem: TMenuItem;
    cbTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbRoundedClick(Sender: TObject);
    procedure shpFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure udFrameWidthChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure udRoundCornersChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure cbGradientClick(Sender: TObject);
    procedure udBarSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure udStripSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure edTitleChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure dlgFontApply(Sender: TObject; Wnd: HWND);
    procedure edItemTextFormatChange(Sender: TObject);
    procedure shpTreeLinesColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbTreeLinesStyleSelect(Sender: TObject);
    procedure cbCutExpandedClick(Sender: TObject);
    procedure cbDarkenCollapsedClick(Sender: TObject);
    procedure edCaptionFormatChange(Sender: TObject);
    procedure sbCaptionFontClick(Sender: TObject);
    procedure cbShowCaptionClick(Sender: TObject);
    procedure cbShowLegendClick(Sender: TObject);
    procedure miCollapseallClick(Sender: TObject);
    procedure DecoBar1Click(Sender: TObject);
    procedure pumDecoBarPopup(Sender: TObject);
    procedure miEditValueClick(Sender: TObject);
    procedure DecoBar1Expanding(Sender:TObject; Item:TDecoBarItem; var Allow:boolean);
    procedure DecoBar1Expanded(Sender:TObject; Item:TDecoBarItem);
    procedure DecoBar1Collapsed(Sender:TObject; Item:TDecoBarItem);
    procedure lvDataKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sbSaveAsClick(Sender: TObject);
    procedure cbAutoColorsClick(Sender: TObject);
    procedure cbAutoCalcClick(Sender: TObject);
    procedure miDeleteitemClick(Sender: TObject);
    procedure miEditcaptionClick(Sender: TObject);
    procedure miAdditemClick(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowItemDetailedData(Item:TDecoBarItem);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  FileCtrl;

{$R *.dfm}

procedure TForm3.cbAutoCalcClick(Sender: TObject);
begin
  if cbAutoCalc.Checked then
  begin
    DecoBar1.AutoCalcValues:=True;
    DecoBar1.RecalcAll;
  end
  else
    DecoBar1.AutoCalcValues:=False;
end;

procedure TForm3.cbAutoColorsClick(Sender: TObject);
begin
  DecoBar1.AutoColors:=cbAutoColors.Checked;
end;

procedure TForm3.cbCutExpandedClick(Sender: TObject);
begin
  DecoBar1.CutExpandedBar:=cbCutExpanded.Checked;
end;

procedure TForm3.cbDarkenCollapsedClick(Sender: TObject);
begin
  DecoBar1.DarkenCollapsedBars:=cbDarkenCollapsed.Checked;
end;

procedure TForm3.cbGradientClick(Sender: TObject);
begin
  DecoBar1.Gradient:=cbGradient.Checked;
end;

procedure TForm3.shpFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpFrame.Brush.Color;
  if dlgColor.Execute then
  begin
    shpFrame.Brush.Color:=dlgColor.Color;
    DecoBar1.FrameColor:=shpFrame.Brush.Color;
  end;
end;

procedure TForm3.udFrameWidthChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoBar1.FrameWidth:=NewValue;
end;

procedure TForm3.shpTreeLinesColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpTreeLinesColor.Brush.Color;
  if dlgColor.Execute then
  begin
    shpTreeLinesColor.Brush.Color:=dlgColor.Color;
    DecoBar1.TreeLinesColor:=shpTreeLinesColor.Brush.Color;
  end;
end;

procedure TForm3.sbCaptionFontClick(Sender: TObject);
begin
  dlgFont.Tag:=0;
  dlgFont.Font.Assign(DecoBar1.CaptionFont);
  if dlgFont.Execute then
    dlgFontApply(Sender,0);
end;

procedure TForm3.sbFontClick(Sender: TObject);
begin
  dlgFont.Tag:=1;
  dlgFont.Font.Assign(DecoBar1.Font);
  if dlgFont.Execute then
    dlgFontApply(Sender,0);
end;

procedure TForm3.dlgFontApply(Sender: TObject; Wnd: HWND);
begin
  case dlgFont.Tag of
    0:begin
        DecoBar1.CaptionFont.Assign(dlgFont.Font);
        sbCaptionFont.Caption:=DecoBar1.CaptionFont.Name+', '+IntToStr(DecoBar1.CaptionFont.Size)+'pt';
      end;
    1:begin
        DecoBar1.Font.Assign(dlgFont.Font);
        lvData.Font.Assign(dlgFont.Font);
        sbFont.Caption:=DecoBar1.Font.Name+', '+IntToStr(DecoBar1.Font.Size)+'pt';
      end;
  end;
end;

procedure TForm3.edCaptionFormatChange(Sender: TObject);
begin
  DecoBar1.CaptionFormat:=edCaptionFormat.Text;
end;

procedure TForm3.shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpBackColor.Brush.Color;
  if dlgColor.Execute then
  begin
    shpBackColor.Brush.Color:=dlgColor.Color;
    if not DecoBar1.ParentBackground then
      pMain.Color:=shpBackColor.Brush.Color;
    //DecoBar1.Color:=shpBackColor.Brush.Color;
  end;
end;

procedure TForm3.cbRoundedClick(Sender: TObject);
begin
  DecoBar1.Rounded:=cbRounded.Checked;
end;

procedure TForm3.cbTransparentClick(Sender: TObject);
begin
  pMain.ParentBackground:=cbTransparent.Checked;
  DecoBar1.ParentBackground:=cbTransparent.Checked;
  if DecoBar1.ParentBackground then
    pMain.Color:=Self.Color
  else
    pMain.Color:=shpBackColor.Brush.Color;
end;

procedure TForm3.cbTreeLinesStyleSelect(Sender: TObject);
begin
  DecoBar1.TreeLinesStyle:=TDecoBarTreeLinesStyle(cbTreeLinesStyle.ItemIndex);
end;

procedure TForm3.cbShowLegendClick(Sender: TObject);
begin
  DecoBar1.ShowLegend:=cbShowLegend.Checked;
end;

procedure TForm3.cbShowCaptionClick(Sender: TObject);
begin
  DecoBar1.ShowCaption:=cbShowCaption.Checked;
end;

procedure TForm3.edItemTextFormatChange(Sender: TObject);
begin
  DecoBar1.ItemTextFormat:=edItemTextFormat.Text;
end;

procedure TForm3.edTitleChange(Sender: TObject);
begin
  DecoBar1.Caption:=edTitle.Text;
  DecoBar1.ShowCaption:=Length(DecoBar1.Caption)>0;
end;

procedure TForm3.udRoundCornersChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoBar1.RoundCorners:=NewValue;
end;

procedure TForm3.udBarSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoBar1.BarSize:=NewValue;
end;

procedure TForm3.udStripSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoBar1.StripSize:=NewValue;
end;

procedure TForm3.rgAlignmentClick(Sender: TObject);
begin
  case rgAlignment.ItemIndex of
    0:DecoBar1.Alignment:=taLeftJustify;
    2:DecoBar1.Alignment:=taRightJustify;
  else
      DecoBar1.Alignment:=taCenter;
  end;
end;

procedure TForm3.miCollapseallClick(Sender: TObject);
begin
  DecoBar1.FullCollapse;
end;

procedure TForm3.pumDecoBarPopup(Sender: TObject);
begin
  miEditCaption.Enabled:=DecoBar1.SelectedItem<>nil;
  miEditValue.Enabled:=DecoBar1.SelectedItem<>nil;
  miDeleteItem.Enabled:=DecoBar1.SelectedItem<>nil;
end;

procedure TForm3.miAdditemClick(Sender: TObject);
var
  c,v:string;
begin
  c:=''; v:='1000';
  if InputQuery('New Item','New item caption:',c) then
  begin
    if InputQuery(c,'Enter value for a new item:',v) then
    begin
      if DecoBar1.SelectedItem=nil then
        DecoBar1.Items.AddItem(c,StrToFloatDef(v,0))
      else
      if DecoBar1.SelectedItem.Collection=DecoBar1.ExpandedItems then
        DecoBar1.ExpandedItems.AddItem(c,StrToFloatDef(v,0))
      else
      begin
        DecoBar1.SelectedItem.Items.AddItem(c,StrToFloatDef(v,0));
        DecoBar1.SelectedItem.Expanded:=True;
      end;
    end;
  end;
end;

procedure TForm3.miDeleteitemClick(Sender: TObject);
begin
  DecoBar1.SelectedItem.Expanded:=False;
  DecoBar1.SelectedItem.Free;
end;

procedure TForm3.miEditcaptionClick(Sender: TObject);
var
  v:string;
begin
  v:=DecoBar1.SelectedItem.Caption;
  if InputQuery(DecoBar1.SelectedItem.Caption,'Enter a new caption:',v) then
    DecoBar1.SelectedItem.Caption:=v;
end;

procedure TForm3.miEditValueClick(Sender: TObject);
var
  v:string;
begin
  v:=FloatToStr(DecoBar1.SelectedItem.Value);
  if InputQuery(DecoBar1.SelectedItem.Caption,'Enter a new value:',v) then
    DecoBar1.SelectedItem.Value:=StrToFloatDef(v,DecoBar1.SelectedItem.Value);
end;

procedure TForm3.DecoBar1Click(Sender: TObject);
begin
  if DecoBar1.IsCaptionHovered then
    ShowMessage('DecoBar caption has been clicked.');
end;

procedure TForm3.DecoBar1Expanding(Sender:TObject; Item:TDecoBarItem; var Allow:boolean);
begin
  if Item.Tag=1 then
  begin
    ShowMessage('This item cannot be expanded. See OnExpanding event in the demo main form.');
    Allow:=False;
  end;
end;

procedure TForm3.DecoBar1Expanded(Sender:TObject; Item:TDecoBarItem);
begin
  if Item.Items.Count=0 then
    ShowItemDetailedData(Item)
  else
  if lvData.Visible then
    lvData.Visible:=False;
end;

procedure TForm3.DecoBar1Collapsed(Sender:TObject; Item:TDecoBarItem);
begin
  if lvData.Visible then
    lvData.Visible:=False;
end;

procedure TForm3.ShowItemDetailedData(Item:TDecoBarItem);
var
  LI:TListItem;
  i,c,v:Integer;
begin
  lvData.Items.BeginUpdate;
  try
    lvData.Items.Clear;
    LI:=lvData.Items.Add;
    LI.Caption:='This is a "linked" ListView component (with random values)';
    LI.SubItems.Add('');
    LI.SubItems.Add('');
    lvData.ItemFocused:=LI;

    c:=Random(20);
    for i:=1 To c do
    begin
      LI:=lvData.Items.Add;
      LI.Caption:='Item '+IntToStr(i);
      v:=Random(50000);
      LI.SubItems.Add(IntToStr(v));
      if v>25000 then
        LI.SubItems.Add('Note for an item')
      else
        LI.SubItems.Add('');
    end;
  finally
    lvData.Items.EndUpdate;
  end;
  if not lvData.Visible then
    lvData.Visible:=True;
  if lvData.CanFocus then
    lvData.SetFocus;
end;

procedure TForm3.lvDataKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        DecoBar1.SelectedItem.Expanded:=False;
        if DecoBar1.CanFocus then
          DecoBar1.SetFocus;
      end;
    VK_UP:
      if (lvData.Items.Count=0) or (lvData.ItemFocused=lvData.Items[0]) then
      begin
        if DecoBar1.CanFocus then
          DecoBar1.SetFocus;
      end;
  end;
end;

procedure TForm3.sbSaveAsClick(Sender: TObject);
var
  MF:TMetafile;
  MFC:TMetafileCanvas;
  fn,fext:string;
  B:TBitmap;
begin
  fn:='DecoBar';
  if PromptForFileName(fn,'Enhanced metafiles (*.emf)|*.emf|Bitmap files (*.bmp)|*.bmp','emf','Save as ...','',True) then
  begin
    fext:=ansilowercase(extractfileext(fn));
    if fext='.emf' then
    begin
      MF:=TMetafile.Create;
      try
        MF.Enhanced:=True;
        MF.Width:=DecoBar1.Width;
        MF.Height:=DecoBar1.Height;
        MFC:=TMetafileCanvas.Create(MF,0);
        try
          DecoBar1.PrintTo(MFC,Rect(0,0,DecoBar1.Width,DecoBar1.Height),True);
        finally
          MFC.Free;
        end;
        MF.SaveToFile(fn);
      finally
        MF.Free;
      end;
    end
    else
    begin
      B:=TBitmap.Create;
      try
        B.PixelFormat:=pf24bit;
        B.Width:=DecoBar1.Width;
        B.Height:=DecoBar1.Height;
        DecoBar1.PrintTo(B.Canvas,Rect(0,0,DecoBar1.Width,DecoBar1.Height));
        B.SaveToFile(fn);
      finally
        B.Free;
      end;
    end;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  Self.DesktopFont:=True;
  DecoBar1.CaptionFont.Size:=DecoBar1.CaptionFont.Size+2;
  sbCaptionFont.Caption:=DecoBar1.CaptionFont.Name+', '+IntToStr(DecoBar1.CaptionFont.Size)+'pt';
  sbFont.Caption:=DecoBar1.Font.Name+', '+IntToStr(DecoBar1.Font.Size)+'pt';
  Randomize;
end;

end.
