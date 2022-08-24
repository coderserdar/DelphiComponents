unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus,
  DecoProgressGridPlus, DecoProgressGrid, DecoCommon;

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
    shpGridLineColor: TShape;
    Label10: TLabel;
    cbGridLineStyle: TComboBox;
    Label11: TLabel;
    Label1: TLabel;
    shpFrame: TShape;
    Label2: TLabel;
    edFrameWidth: TEdit;
    udFrameWidth: TUpDown;
    Label13: TLabel;
    sbCaptionFont: TButton;
    Label6: TLabel;
    pumGrid: TPopupMenu;
    miMenuTitle: TMenuItem;
    N1: TMenuItem;
    miEditValue: TMenuItem;
    cbShowColumnsHeader: TCheckBox;
    cbShowCaption: TCheckBox;
    sbSaveAs: TButton;
    miDeleteitem: TMenuItem;
    miEditcaption: TMenuItem;
    miAdditem: TMenuItem;
    cbShowHorzGrid: TCheckBox;
    cbShowFirstGridLine: TCheckBox;
    cbShowLastGridLine: TCheckBox;
    cbShowVertGrid: TCheckBox;
    Label9: TLabel;
    Label14: TLabel;
    cbGradient: TComboBox;
    cbFrameProgress: TCheckBox;
    Label4: TLabel;
    cbBarPosition: TComboBox;
    pMain: TPanel;
    DecoProgressGrid1: TDecoProgressGridPlus;
    Splitter1: TSplitter;
    DecoProgressGrid2: TDecoProgressGridPlus;
    Label12: TLabel;
    cbValueInBar: TComboBox;
    shpBBackColor: TShape;
    Label15: TLabel;
    cbTransparent: TCheckBox;
    shpBackNone: TShape;
    N2: TMenuItem;
    miSortAlpha: TMenuItem;
    miSortPercent: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure cbRoundedClick(Sender: TObject);
    procedure shpFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure udFrameWidthChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure udRoundCornersChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udBarSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
    procedure edTitleChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure dlgFontApply(Sender: TObject; Wnd: HWND);
    procedure edItemTextFormatChange(Sender: TObject);
    procedure shpGridLineColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbGridLineStyleSelect(Sender: TObject);
    procedure sbCaptionFontClick(Sender: TObject);
    procedure cbShowCaptionClick(Sender: TObject);
    procedure cbShowColumnsHeaderClick(Sender: TObject);
    procedure pumGridPopup(Sender: TObject);
    procedure miEditValueClick(Sender: TObject);
    procedure sbSaveAsClick(Sender: TObject);
    procedure miDeleteitemClick(Sender: TObject);
    procedure miEditcaptionClick(Sender: TObject);
    procedure miAdditemClick(Sender: TObject);
    procedure DecoProgressGrid1Click(Sender: TObject);
    procedure cbShowHorzGridClick(Sender: TObject);
    procedure cbShowVertGridClick(Sender: TObject);
    procedure cbShowFirstGridLineClick(Sender: TObject);
    procedure cbShowLastGridLineClick(Sender: TObject);
    procedure cbGradientSelect(Sender: TObject);
    procedure cbFrameProgressClick(Sender: TObject);
    procedure cbBarPositionSelect(Sender: TObject);
    procedure DecoProgressGrid1DrawCell(Sender: TObject; TargetCanvas: TCanvas; Item: TDecoProgressGridItem;
      ColumnIndex: Integer; CellRect: TRect; var Text: string);
    procedure cbValueInBarSelect(Sender: TObject);
    procedure shpBBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DecoProgressGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DecoProgressGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DecoProgressGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DecoProgressGrid1MouseLeave(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure shpBackNoneMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure miSortAlphaClick(Sender: TObject);
    procedure miSortPercentClick(Sender: TObject);
    function DecoProgressGrid1CompareItems(Sender: TObject; Item1, Item2: TDecoProgressGridItem): Integer;
  private
    { Private declarations }
    modItem:TDecoProgressGridItem;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  FileCtrl;

{$R *.dfm}

procedure TForm3.shpFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpFrame.Brush.Color;
  if dlgColor.Execute then
  begin
    shpFrame.Brush.Color:=dlgColor.Color;
    DecoProgressGrid1.FrameColor:=shpFrame.Brush.Color;
    DecoProgressGrid2.FrameColor:=shpFrame.Brush.Color;
  end;
end;

procedure TForm3.udFrameWidthChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  if NewValue>=0 then
  begin
    DecoProgressGrid1.FrameWidth:=NewValue;
    DecoProgressGrid2.FrameWidth:=NewValue;
  end;
end;

procedure TForm3.shpGridLineColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpGridLineColor.Brush.Color;
  if dlgColor.Execute then
  begin
    shpGridLineColor.Brush.Color:=dlgColor.Color;
    DecoProgressGrid1.GridLineColor:=shpGridLineColor.Brush.Color;
    DecoProgressGrid2.GridLineColor:=shpGridLineColor.Brush.Color;
  end;
end;

procedure TForm3.sbCaptionFontClick(Sender: TObject);
begin
  dlgFont.Tag:=0;
  dlgFont.Font.Assign(DecoProgressGrid1.CaptionFont);
  if dlgFont.Execute then
    dlgFontApply(Sender,0);
end;

procedure TForm3.sbFontClick(Sender: TObject);
begin
  dlgFont.Tag:=1;
  dlgFont.Font.Assign(DecoProgressGrid1.Font);
  if dlgFont.Execute then
    dlgFontApply(Sender,0);
end;

procedure TForm3.dlgFontApply(Sender: TObject; Wnd: HWND);
begin
  case dlgFont.Tag of
    0:begin
        DecoProgressGrid1.CaptionFont.Assign(dlgFont.Font);
        DecoProgressGrid2.CaptionFont.Assign(dlgFont.Font);
        sbCaptionFont.Caption:=DecoProgressGrid1.CaptionFont.Name+', '+IntToStr(DecoProgressGrid1.CaptionFont.Size)+'pt';
      end;
    1:begin
        DecoProgressGrid1.Font.Assign(dlgFont.Font);
        DecoProgressGrid2.Font.Assign(dlgFont.Font);
        sbFont.Caption:=DecoProgressGrid1.Font.Name+', '+IntToStr(DecoProgressGrid1.Font.Size)+'pt';
      end;
  end;
end;

procedure TForm3.shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpBackColor.Brush.Color;
  if dlgColor.Execute then
  begin
    shpBackColor.Brush.Color:=dlgColor.Color;
    pMain.Color:=shpBackColor.Brush.Color;
  end;
end;

procedure TForm3.shpBackNoneMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DecoProgressGrid1.BackColor:=clNone;
  DecoProgressGrid2.BackColor:=clNone;
end;

procedure TForm3.shpBBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color:=shpBBackColor.Brush.Color;
  if dlgColor.Execute then
  begin
    shpBBackColor.Brush.Color:=dlgColor.Color;
    DecoProgressGrid1.BackColor:=shpBBackColor.Brush.Color;
    DecoProgressGrid2.BackColor:=shpBBackColor.Brush.Color;
  end;
end;

procedure TForm3.cbRoundedClick(Sender: TObject);
begin
  DecoProgressGrid1.Rounded:=cbRounded.Checked;
  DecoProgressGrid2.Rounded:=cbRounded.Checked;
end;

procedure TForm3.cbBarPositionSelect(Sender: TObject);
begin
  DecoProgressGrid1.BarPosition:=TPGBarPosition(cbBarPosition.ItemIndex);
  DecoProgressGrid2.BarPosition:=TPGBarPosition(cbBarPosition.ItemIndex);
end;

procedure TForm3.cbFrameProgressClick(Sender: TObject);
begin
  DecoProgressGrid1.FrameProgress:=cbFrameProgress.Checked;
  DecoProgressGrid2.FrameProgress:=cbFrameProgress.Checked;
end;

procedure TForm3.cbGradientSelect(Sender: TObject);
begin
  DecoProgressGrid1.Gradient:=TPGGradientStyle(cbGradient.ItemIndex);
  DecoProgressGrid2.Gradient:=TPGGradientStyle(cbGradient.ItemIndex);
end;

procedure TForm3.cbGridLineStyleSelect(Sender: TObject);
begin
  DecoProgressGrid1.GridLineStyle:=TPenStyle(cbGridLineStyle.ItemIndex);
  DecoProgressGrid2.GridLineStyle:=TPenStyle(cbGridLineStyle.ItemIndex);
end;

procedure TForm3.cbShowLastGridLineClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowLastGridLine:=cbShowLastGridLine.Checked;
  DecoProgressGrid2.ShowLastGridLine:=cbShowLastGridLine.Checked;
end;

procedure TForm3.cbShowColumnsHeaderClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowColumnsHeader:=cbShowColumnsHeader.Checked;
  DecoProgressGrid2.ShowColumnsHeader:=cbShowColumnsHeader.Checked;
end;

procedure TForm3.cbShowVertGridClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowVertGrid:=cbShowVertGrid.Checked;
  DecoProgressGrid2.ShowVertGrid:=cbShowVertGrid.Checked;
end;

procedure TForm3.cbTransparentClick(Sender: TObject);
begin
  pMain.ParentBackground:=cbTransparent.Checked;
  DecoProgressGrid1.ParentBackground:=cbTransparent.Checked;
  DecoProgressGrid2.ParentBackground:=cbTransparent.Checked;
  if DecoProgressGrid1.ParentBackground then
    pMain.Color:=Self.Color
  else
    pMain.Color:=shpBackColor.Brush.Color;
end;

procedure TForm3.cbValueInBarSelect(Sender: TObject);
begin
  DecoProgressGrid1.ShowValueInBar:=TShowValueInBar(cbValueInBar.ItemIndex);
  DecoProgressGrid2.ShowValueInBar:=TShowValueInBar(cbValueInBar.ItemIndex);
end;

procedure TForm3.cbShowCaptionClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowCaption:=cbShowCaption.Checked;
  DecoProgressGrid2.ShowCaption:=cbShowCaption.Checked;
end;

procedure TForm3.cbShowFirstGridLineClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowFirstGridLine:=cbShowFirstGridLine.Checked;
  DecoProgressGrid2.ShowFirstGridLine:=cbShowFirstGridLine.Checked;
end;

procedure TForm3.cbShowHorzGridClick(Sender: TObject);
begin
  DecoProgressGrid1.ShowHorzGrid:=cbShowHorzGrid.Checked;
  DecoProgressGrid2.ShowHorzGrid:=cbShowHorzGrid.Checked;
end;

procedure TForm3.edItemTextFormatChange(Sender: TObject);
begin
  DecoProgressGrid1.ValueTextFormat:=edItemTextFormat.Text;
  DecoProgressGrid2.ValueTextFormat:=edItemTextFormat.Text;
end;

procedure TForm3.edTitleChange(Sender: TObject);
begin
  DecoProgressGrid1.Caption:=edTitle.Text;
  DecoProgressGrid2.Caption:=edTitle.Text;
  DecoProgressGrid1.ShowCaption:=Length(DecoProgressGrid1.Caption)>0;
  DecoProgressGrid2.ShowCaption:=Length(DecoProgressGrid2.Caption)>0;
end;

procedure TForm3.udRoundCornersChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoProgressGrid1.RoundCorners:=NewValue;
  DecoProgressGrid2.RoundCorners:=NewValue;
end;

procedure TForm3.udBarSizeChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt;
  Direction: TUpDownDirection);
begin
  DecoProgressGrid1.RowHeight:=NewValue;
  DecoProgressGrid2.RowHeight:=NewValue;
end;

procedure TForm3.rgAlignmentClick(Sender: TObject);
begin
  case rgAlignment.ItemIndex of
    0:DecoProgressGrid1.Alignment:=taLeftJustify;
    2:DecoProgressGrid1.Alignment:=taRightJustify;
  else
      DecoProgressGrid1.Alignment:=taCenter;
  end;
  DecoProgressGrid2.Alignment:=DecoProgressGrid1.Alignment;
end;

procedure TForm3.pumGridPopup(Sender: TObject);
begin
  if pumGrid.PopupComponent=DecoProgressGrid1 then
  begin
    miEditCaption.Enabled:=DecoProgressGrid1.SelectedItem<>nil;
    miEditValue.Enabled:=DecoProgressGrid1.SelectedItem<>nil;
    miDeleteItem.Enabled:=DecoProgressGrid1.SelectedItem<>nil;
  end
  else
  begin
    miEditCaption.Enabled:=DecoProgressGrid2.SelectedItem<>nil;
    miEditValue.Enabled:=DecoProgressGrid2.SelectedItem<>nil;
    miDeleteItem.Enabled:=DecoProgressGrid2.SelectedItem<>nil;
  end;
end;

procedure TForm3.miAdditemClick(Sender: TObject);
var
  c,v:string;
begin
  c:=''; v:='50';
  if InputQuery('New Item','New item caption:',c) then
  begin
    if InputQuery(c,'Enter value for a new item:',v) then
    begin
      if pumGrid.PopupComponent=DecoProgressGrid1 then
        DecoProgressGrid1.Items.AddItem(c,StrToFloatDef(v,0))
      else
        DecoProgressGrid2.Items.AddItem(c,StrToFloatDef(v,0))
    end;
  end;
end;

procedure TForm3.miDeleteitemClick(Sender: TObject);
begin
  if pumGrid.PopupComponent=DecoProgressGrid1 then
    DecoProgressGrid1.SelectedItem.Free
  else
    DecoProgressGrid2.SelectedItem.Free
end;

procedure TForm3.miEditcaptionClick(Sender: TObject);
var
  v:string;
begin
  if pumGrid.PopupComponent=DecoProgressGrid1 then
  begin
    v:=DecoProgressGrid1.SelectedItem.Caption;
    if InputQuery(DecoProgressGrid1.SelectedItem.Caption,'Enter a new caption:',v) then
      DecoProgressGrid1.SelectedItem.Caption:=v;
  end
  else
  begin
    v:=DecoProgressGrid2.SelectedItem.Caption;
    if InputQuery(DecoProgressGrid2.SelectedItem.Caption,'Enter a new caption:',v) then
      DecoProgressGrid2.SelectedItem.Caption:=v;
  end;
end;

procedure TForm3.miEditValueClick(Sender: TObject);
var
  v:string;
begin
  if pumGrid.PopupComponent=DecoProgressGrid1 then
  begin
    v:=FloatToStr(DecoProgressGrid1.SelectedItem.Value);
    if InputQuery(DecoProgressGrid1.SelectedItem.Caption,'Enter a new value:',v) then
      DecoProgressGrid1.SelectedItem.Value:=StrToFloatDef(v,DecoProgressGrid1.SelectedItem.Value);
  end
  else
  begin
    v:=FloatToStr(DecoProgressGrid2.SelectedItem.Value);
    if InputQuery(DecoProgressGrid2.SelectedItem.Caption,'Enter a new value:',v) then
      DecoProgressGrid2.SelectedItem.Value:=StrToFloatDef(v,DecoProgressGrid2.SelectedItem.Value);
  end;
end;

procedure TForm3.miSortAlphaClick(Sender: TObject);
begin
  DecoProgressGrid1.OnCompareItems:=nil;
  DecoProgressGrid2.OnCompareItems:=nil;
  DecoProgressGrid1.Sort;
  DecoProgressGrid2.Sort;
  DecoProgressGrid1.OnCompareItems:=DecoProgressGrid1CompareItems;
  DecoProgressGrid2.OnCompareItems:=DecoProgressGrid1CompareItems;
end;

procedure TForm3.miSortPercentClick(Sender: TObject);
begin
  DecoProgressGrid1.Sort;
  DecoProgressGrid2.Sort;
end;

procedure TForm3.DecoProgressGrid1Click(Sender: TObject);
begin
  if TDecoProgressGridPlus(Sender).IsCaptionHovered then
    ShowMessage('Grid caption has been clicked.')
  else
  if TDecoProgressGridPlus(Sender).IsColumnsHeaderHovered then
    ShowMessage('Grid columns header has been clicked.');
end;

function TForm3.DecoProgressGrid1CompareItems(Sender: TObject; Item1, Item2: TDecoProgressGridItem): Integer;
begin
  if Item1.Value/Item1.MaxValue<Item2.Value/Item2.MaxValue then Result:=-1
  else
  if Item1.Value/Item1.MaxValue>Item2.Value/Item2.MaxValue then Result:=1
  else
    Result:=AnsiCompareText(Item1.Caption,Item2.Caption);
end;

procedure TForm3.DecoProgressGrid1DrawCell(Sender: TObject; TargetCanvas: TCanvas; Item: TDecoProgressGridItem;
  ColumnIndex: Integer; CellRect: TRect; var Text: string);
begin
  case ColumnIndex of
    0:begin
        TargetCanvas.Font.Color:=$A26B2D;
        TargetCanvas.Font.Style:=[fsBold];
      end;
    3:if Item.Value>Item.MaxValue then
      TargetCanvas.Font.Color:=TDecoProgressGridPlus(Sender).Level3Color;
  end;
end;

procedure TForm3.DecoProgressGrid1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HInfo:TDecoPGHitTestInfo;
begin
  if button=mbLeft then
  begin
    HInfo:=DecoProgressGrid1.GetHitTestInfoAt(x,y);
    if HInfo.Kind in [pgiBar,pgiValueInBar] then
      modItem:=HInfo.Item;
  end;
end;

procedure TForm3.DecoProgressGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  HInfo:TDecoPGHitTestInfo;
  newCursor:TCursor;
begin
  if modItem<>nil then
  begin
    HInfo:=DecoProgressGrid1.GetHitTestInfoAt(x,y);
    if HInfo.Item<>modItem then
      modItem:=nil
    else
    if HInfo.Kind in [pgiBar,pgiValueInBar] then
      modItem.Value:=HInfo.BarValue
  end
  else
  if Shift=[] then
  begin
    HInfo:=DecoProgressGrid1.GetHitTestInfoAt(x,y);
    if HInfo.Kind=pgiValueInBar then
      newCursor:=crSizeWE
    else
      newCursor:=crDefault;
    if Screen.Cursor<>newCursor then
      Screen.Cursor:=newCursor;
  end;
end;

procedure TForm3.DecoProgressGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HInfo:TDecoPGHitTestInfo;
begin
  if modItem<>nil then
  begin
    HInfo:=DecoProgressGrid1.GetHitTestInfoAt(x,y);
    if HInfo.Kind in [pgiBar,pgiValueInBar] then
      modItem.Value:=HInfo.BarValue;
    modItem:=nil;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm3.DecoProgressGrid1MouseLeave(Sender: TObject);
begin
  if modItem<>nil then
    modItem:=nil;
  if Screen.Cursor<>crDefault then
    Screen.Cursor:=crDefault;
end;

procedure TForm3.sbSaveAsClick(Sender: TObject);
var
  MF:TMetafile;
  MFC:TMetafileCanvas;
  fn,fext:string;
  B:TBitmap;
begin
  fn:='DecoProgressGrid';
  if PromptForFileName(fn,'Enhanced metafiles (*.emf)|*.emf|Bitmap files (*.bmp)|*.bmp','emf','Save as ...','',True) then
  begin
    fext:=ansilowercase(extractfileext(fn));
    if fext='.emf' then
    begin
      MF:=TMetafile.Create;
      try
        MF.Enhanced:=True;
        MF.Width:=DecoProgressGrid1.Width;
        MF.Height:=DecoProgressGrid1.Height;
        MFC:=TMetafileCanvas.Create(MF,0);
        try
          DecoProgressGrid1.PrintTo(MFC,Rect(0,0,DecoProgressGrid1.Width,DecoProgressGrid1.Height),True);
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
        B.Width:=DecoProgressGrid1.Width;
        B.Height:=DecoProgressGrid1.Height;
        DecoProgressGrid1.PrintTo(B.Canvas,Rect(0,0,DecoProgressGrid1.Width,DecoProgressGrid1.Height));
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
  DecoProgressGrid1.CaptionFont.Size:=DecoProgressGrid1.CaptionFont.Size+2;
  DecoProgressGrid2.CaptionFont.Size:=DecoProgressGrid2.CaptionFont.Size+2;
  DecoProgressGrid1.CaptionFont.Style:=[fsBold];
  DecoProgressGrid2.Font.Style:=[fsBold];
  DecoProgressGrid2.CaptionFont.Style:=[fsBold];
  sbCaptionFont.Caption:=DecoProgressGrid1.CaptionFont.Name+', '+IntToStr(DecoProgressGrid1.CaptionFont.Size)+'pt';
  sbFont.Caption:=DecoProgressGrid1.Font.Name+', '+IntToStr(DecoProgressGrid1.Font.Size)+'pt';
  modItem:=nil;
end;

initialization

  DecoUseSpeedSort := True;

end.
