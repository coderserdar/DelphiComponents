unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, StdCtrls, DecoChart, DecoCompareGrid, DecoCompareGridPlus, ComCtrls;

type
  TfrmMain = class(TForm)
    pOptions: TPanel;
    ilPictures: TImageList;
    ilChecks: TImageList;
    ilStars: TImageList;
    StatusBar1: TStatusBar;
    cbFirstRowHeight: TComboBox;
    Label1: TLabel;
    cbFirstColWidth: TComboBox;
    Label2: TLabel;
    cbRowHeight: TComboBox;
    cbColWidth: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    sbFont: TButton;
    dlgFont: TFontDialog;
    Label5: TLabel;
    edFrameWidth: TEdit;
    udFrameWidth: TUpDown;
    Label6: TLabel;
    edFrameRoundCorners: TEdit;
    udFrameRoundCorners: TUpDown;
    sbCaptionFont: TButton;
    sbItemFont: TButton;
    sbCriteriaFont: TButton;
    sbSetParentFont: TButton;
    Label7: TLabel;
    Label8: TLabel;
    cbItemImageLayout: TComboBox;
    cbItemTextLayout: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    edGridLineWidth: TEdit;
    udGridLineWidth: TUpDown;
    Label11: TLabel;
    cbGridLineStyle: TComboBox;
    Panel1: TPanel;
    cbCustomDraw: TCheckBox;
    cbShowVertGrid: TCheckBox;
    cbShowHorzGrid: TCheckBox;
    cbHeadGridLine: TCheckBox;
    cbImages: TCheckBox;
    cbShowCaption: TCheckBox;
    cbShowFocusRect: TCheckBox;
    cbHover: TCheckBox;
    cbShowGuides: TCheckBox;
    sbFixedColor: TButton;
    dlgColor: TColorDialog;
    cbShowCategories: TCheckBox;
    sbGridColor: TButton;
    sbFrameColor: TButton;
    sbColor: TButton;
    rgStripe: TRadioGroup;
    cbRatingImages: TCheckBox;
    sbFillValues: TButton;
    sbSaveEMF: TButton;
    cbTransparent: TCheckBox;
    pMain: TPanel;
    compTable: TDecoCompareGridPlus;
    procedure FormCreate(Sender: TObject);
    procedure compTableMouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sbFillValuesClick(Sender: TObject);
    procedure CompTableDrawCell(Sender:TObject; TargetCanvas:TCanvas; Item:TDecoCompareGridItem;
        Criterium: TDecoCompareGridCriterium; Value:TDecoCompareGridValue; CellRect:TRect; IsSpecial:Boolean; var Text:string);
    procedure CompTableSelectCell(Sender:TObject; Item:TDecoChartItem);
    procedure cbCustomDrawClick(Sender: TObject);
    procedure OtherOptionsChange(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure sbSetParentFontClick(Sender: TObject);
    procedure sbFixedColorClick(Sender: TObject);
    procedure sbSaveEMFClick(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillDemoData;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DecoGDI, DwmApi {$IF CompilerVersion >= 24}, System.Types, System.UITypes{$IFEND};

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  compTable.ItemFont.Style:=[fsBold];
  compTable.ItemFont.Color:=clNavy;
  compTable.CaptionFont.Color:=clPurple;
  compTable.CaptionFont.Style:=[fsBold];
  Randomize;
  FillDemoData;
end;

procedure TfrmMain.FillDemoData;
var
  I:TDecoCompareGridItem;
begin
  compTable.BeginUpdate;
  try
    compTable.Items.Clear;
    compTable.Criterias.Clear;

    compTable.AddCriterium('Overall Score','','Overall Score',cgsProgressBar,'%.1f').Tag:=1;
    compTable.AddCriterium('Price','General criterias','Price',cgsNumber,'$ %.1n').Tag:=2;
    compTable.AddCriterium('User Rating','General criterias','User Rating',cgsRating,'%.1f',0,5);
    compTable.AddCriterium('Total Downloads','General criterias','Total Downloads',cgsNumber,'%.0n');
    compTable.AddCriterium('VCL Component','Miscellaneous factors','VCL Component',cgsCheck,'|Yes');

    I:=compTable.AddItem('Precision Helper',0); I.Hint := I.Caption;
    compTable.AddValue(I,[Random(101),'Free',Random(51)/10,Random(100000),False]);

    I:=compTable.AddItem('Precision Language Suite',1); I.Hint := I.Caption;
    compTable.AddValue(I,[Random(101),Random(51),Random(51)/10,Random(100000),True]);

    I:=compTable.AddItem('pdScript IDE',3); I.Hint := I.Caption;
    compTable.AddValue(I,[Random(101),Random(51),Random(51)/10,Random(100000),False]);

    I:=compTable.AddItem('Deco Charts for VCL',2); I.Hint := I.Caption;
    compTable.AddValue(I,[Random(101),Random(51),Random(51)/10,Random(100000),True]);
  finally
    compTable.EndUpdate;
  end;
end;

procedure TfrmMain.sbFillValuesClick(Sender: TObject);
begin
  FillDemoData;
end;

procedure TfrmMain.compTableMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta>0 then
  begin
    if compTable.OffsetY-(WheelDelta div 6)<0 then
      compTable.OffsetY:=0
    else
    begin
      compTable.OffsetY:=compTable.OffsetY-(WheelDelta div 6);
      Handled:=True;
    end;
  end
  else
  if (compTable.GridRect.Bottom-compTable.GridRect.Top)>(compTable.ClientRect.Bottom-compTable.ClientRect.Top-compTable.Padding.Bottom-compTable.Padding.Top) then
  begin
    if compTable.OffsetY-(WheelDelta div 6)>(compTable.GridRect.Bottom-compTable.GridRect.Top)-(compTable.ClientRect.Bottom-compTable.ClientRect.Top-compTable.Padding.Bottom-compTable.Padding.Top) then
      compTable.OffsetY:=(compTable.GridRect.Bottom-compTable.GridRect.Top)-(compTable.ClientRect.Bottom-compTable.ClientRect.Top-compTable.Padding.Bottom-compTable.Padding.Top)
    else
    begin
      compTable.OffsetY:=compTable.OffsetY-(WheelDelta div 6);
      Handled:=True;
    end;
  end;

  if (not Handled) then
  begin
    if WheelDelta>0 then
    begin
      if compTable.OffsetX-(WheelDelta div 6)<0 then
        compTable.OffsetX:=0
      else
        compTable.OffsetX:=compTable.OffsetX-(WheelDelta div 6);
    end
    else
    if compTable.GridRect.Right-compTable.GridRect.Left>compTable.ClientRect.Right-compTable.ClientRect.Left-compTable.Padding.Right-compTable.Padding.Left then
    begin
      if compTable.OffsetX-(WheelDelta div 6)>(compTable.GridRect.Right-compTable.GridRect.Left)-(compTable.ClientRect.Right-compTable.ClientRect.Left-compTable.Padding.Right-compTable.Padding.Left) then
        compTable.OffsetX:=(compTable.GridRect.Right-compTable.GridRect.Left)-(compTable.ClientRect.Right-compTable.ClientRect.Left-compTable.Padding.Right-compTable.Padding.Left)
      else
        compTable.OffsetX:=compTable.OffsetX-(WheelDelta div 6);
      Handled:=True;
    end;
  end;
end;

procedure TfrmMain.sbFixedColorClick(Sender: TObject);
begin
  if Sender=sbFixedColor then dlgColor.Color:=compTable.FixedColor
  else if Sender=sbGridColor then dlgColor.Color:=compTable.GridLineColor
  else if Sender=sbFrameColor then dlgColor.Color:=compTable.FrameColor
  else if Sender=sbColor then dlgColor.Color:=compTable.Color;
  if dlgColor.Execute then
  begin
    if Sender=sbFixedColor then compTable.FixedColor:=dlgColor.Color
    else if Sender=sbGridColor then compTable.GridLineColor:=dlgColor.Color
    else if Sender=sbFrameColor then compTable.FrameColor:=dlgColor.Color
    else if Sender=sbColor then compTable.Color:=dlgColor.Color;
  end;
end;

procedure TfrmMain.sbFontClick(Sender: TObject);
begin
  if Sender=sbFont then dlgFont.Font.Assign(compTable.Font)
  else if Sender=sbItemFont then dlgFont.Font.Assign(compTable.ItemFont)
  else if Sender=sbCriteriaFont then dlgFont.Font.Assign(compTable.CriteriaFont)
  else if Sender=sbCaptionFont then dlgFont.Font.Assign(compTable.CaptionFont);
  if dlgFont.Execute then
  begin
    if Sender=sbFont then compTable.Font:=dlgFont.Font
    else if Sender=sbItemFont then compTable.ItemFont:=dlgFont.Font
    else if Sender=sbCriteriaFont then compTable.CriteriaFont:=dlgFont.Font
    else if Sender=sbCaptionFont then compTable.CaptionFont:=dlgFont.Font
  end;
end;

procedure TfrmMain.sbSetParentFontClick(Sender: TObject);
begin
  compTable.ParentFont := False;
  compTable.ParentFont := True;
end;

procedure TfrmMain.cbCustomDrawClick(Sender: TObject);
begin
  if cbCustomDraw.Checked then
    compTable.OnDrawCell:=CompTableDrawCell
  else
    compTable.OnDrawCell:=nil;
end;

procedure TfrmMain.OtherOptionsChange(Sender: TObject);
var
  tmpInt:Integer;
begin
  compTable.BeginUpdate;
  try
    compTable.ShowCaption:=cbShowCaption.Checked;
    compTable.ShowFocusRect:=cbShowFocusRect.Checked;

    tmpInt:=StrToIntDef(cbFirstColWidth.Text,0);
    compTable.FixedColWidth:=tmpInt;
    tmpInt:=StrToIntDef(cbFirstRowHeight.Text,0);
    compTable.FixedRowHeight:=tmpInt;

    tmpInt:=StrToIntDef(cbRowHeight.Text,0);
    compTable.RowHeight:=tmpInt;
    tmpInt:=StrToIntDef(cbColWidth.Text,0);
    compTable.ColWidth:=tmpInt;

    compTable.FrameWidth:=udFrameWidth.Position;
    compTable.GridLineWidth:=udGridLineWidth.Position;
    compTable.ShowVertGrid:=cbShowVertGrid.Checked;
    compTable.ShowHorzGrid:=cbShowHorzGrid.Checked;
    compTable.HeadGridLine:=cbHeadGridLine.Checked;
    compTable.FrameRoundCorners := udFrameRoundCorners.Position;

    compTable.GridLineStyle:=TPenStyle(cbGridLineStyle.ItemIndex);
    compTable.ShowCategories:=cbShowCategories.Checked;

    compTable.ShowGuideHeader:=cbShowGuides.Checked;
    compTable.ShowGuideFooter:=cbShowGuides.Checked;

    if cbImages.Checked then
      compTable.Images:=ilPictures
    else
      compTable.Images:=nil;
    if cbRatingImages.Checked then
      compTable.RatingImages:=ilStars
    else
      compTable.RatingImages:=nil;

    case cbItemImageLayout.ItemIndex of
      0:compTable.ItemImageLayout:=iilTop;
      2:compTable.ItemImageLayout:=iilLeft;
      3:compTable.ItemImageLayout:=iilRight;
    else
      compTable.ItemImageLayout:=iilBottom;
    end;

    case cbItemTextLayout.ItemIndex of
      0:compTable.ItemTextLayout:=taAlignTop;
      1:compTable.ItemTextLayout:=taAlignBottom;
    else
      compTable.ItemTextLayout:=taVerticalCenter;
    end;

    if cbHover.Checked then
    begin
      compTable.HoverCursor := crHandPoint;
      compTable.HoverStyle := [fsUnderline];
    end
    else
    begin
      compTable.HoverCursor := crDefault;
      compTable.HoverStyle := [];
    end;

    compTable.StripeColumns:=rgStripe.ItemIndex=0;
    compTable.StripeRows:=rgStripe.ItemIndex=1;

  finally
    compTable.EndUpdate;
  end;
end;

procedure TfrmMain.cbTransparentClick(Sender: TObject);
begin
  pMain.ParentBackground:=cbTransparent.Checked;
  compTable.ParentBackground:=cbTransparent.Checked;
  if compTable.ParentBackground then
    compTable.Color:=Self.Color
  else
    compTable.Color:=clWhite;
  if compTable.ParentBackground and DwmCompositionEnabled then
  begin
    Self.GlassFrame.SheetOfGlass:=True;
    Self.GlassFrame.Enabled:=True;
  end
  else
  begin
    Self.GlassFrame.SheetOfGlass:=False;
    Self.GlassFrame.Enabled:=False;
  end;
end;

procedure TfrmMain.CompTableDrawCell(Sender:TObject; TargetCanvas:TCanvas; Item:TDecoCompareGridItem;
      Criterium: TDecoCompareGridCriterium; Value:TDecoCompareGridValue; CellRect:TRect; IsSpecial:Boolean; var Text:string);
begin
  if IsSpecial then
  begin
    if Criterium=nil then
    begin
      TargetCanvas.Brush.Style:=bsSolid;
      TargetCanvas.Brush.Color:=clBtnFace;
      TargetCanvas.FillRect(CellRect);
      TargetCanvas.Brush.Style:=bsClear;
      TargetCanvas.Font.Color:=clHighlight;
      if Item=nil then
        DoDrawNormalText(TargetCanvas.Handle, '#', CellRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
    end;
  end
  else
  begin
    if Value<>nil then
    begin
      case Criterium.Style of
        cgsRating:
          if CellRect.Right-CellRect.Left>ilStars.Width*5+44 then
          begin
            Dec(CellRect.Right,6);
            TargetCanvas.Font.Size:=7;
            DoDrawNormalText(TargetCanvas.Handle, TDecoCompareGrid(Sender).GetValueFormatted(Criterium,Value), CellRect, DT_SINGLELINE or DT_RIGHT or DT_VCENTER);
            TargetCanvas.Font.Size:=compTable.Font.Size;
          end;
        cgsCheck:
          if not Value.AsBoolean then
          begin
            TargetCanvas.Font.Color:=clMaroon;
            DoDrawNormalText(TargetCanvas.Handle, 'No', CellRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER, clMaroon);
          end;
      end;
    end
    else
    if Item<>nil then
    begin
      if rgStripe.ItemIndex<>0 then
      begin
        if Assigned(compTable.Images) and (CellRect.Bottom - (compTable.Images.Height + compTable.Spacing*2)>CellRect.Top) then
          CellRect.Top:=CellRect.Bottom - (compTable.Images.Height + compTable.Spacing*2);
        TargetCanvas.Brush.Style:=bsSolid;
        TargetCanvas.Brush.Color:=compTable.Color;
        TargetCanvas.FillRect(CellRect);
        TargetCanvas.Brush.Style:=bsClear;
        TargetCanvas.Pen.Mode := pmCopy;
        TargetCanvas.Pen.Width:= compTable.GridLineWidth;
        TargetCanvas.Pen.Color := compTable.GridLineColor;
        TargetCanvas.Polyline([point(CellRect.Left,CellRect.Top),point(CellRect.Right,CellRect.Top)]);
      end;
    end;
    if Criterium<>nil then
    begin
      if Criterium.Tag=1 then // Overall score row
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
      else
      if Criterium.Tag=2 then // Price row
      begin
        TargetCanvas.Font.Color:=clRed;
        if Value<>nil then
          TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      end
    end;
  end;
end;

procedure TfrmMain.CompTableSelectCell(Sender:TObject; Item:TDecoChartItem);
var
  I:TDecoCompareGridItem;
  C:TDecoCompareGridCriterium;
  V:TDecoCompareGridValue;
begin
  if Item=nil then
    StatusBar1.SimpleText:=''
  else
  begin
    if Item is TDecoCompareGridValue then
    begin
      if compTable.GetHitTestInfoAt(Item,I,C,V) then
        StatusBar1.SimpleText:=I.Caption+' - '+C.Caption+' : '+compTable.GetValueFormatted(C,V)
      else
        StatusBar1.SimpleText:='';
    end
    else
      StatusBar1.SimpleText:=Item.Caption;
  end;
end;

procedure TfrmMain.sbSaveEMFClick(Sender: TObject);
var
  MF:TMetafile;
  MFC:TMetafileCanvas;
  fn,fext:string;
  B:TBitmap;
  R:TRect;
begin
  fn:=compTable.Name;
  if PromptForFileName(fn,'Enhanced metafiles (*.emf)|*.emf|Bitmap files (*.bmp)|*.bmp','emf','Save as ...','',True) then
  begin
    compTable.SetOffsetXY(0,0);
    R:=compTable.GridRect;
    Dec(R.Left,compTable.Padding.Left);
    Dec(R.Top,compTable.Padding.Top);
    Inc(R.Right,compTable.Padding.Right);
    Inc(R.Bottom,compTable.Padding.Bottom);
    fext:=ansilowercase(extractfileext(fn));
    if fext='.emf' then
    begin
      MF:=TMetafile.Create;
      try
        MF.Enhanced:=True;
        MF.Width:=R.Right-R.Left;
        MF.Height:=R.Bottom-R.Top;
        MFC:=TMetafileCanvas.Create(MF,0);
        try
          compTable.PrintTo(MFC,R,True);
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
        B.Width:=R.Right-R.Left;
        B.Height:=R.Bottom-R.Top;
        compTable.PrintTo(B.Canvas,R);
        B.SaveToFile(fn);
      finally
        B.Free;
      end;
    end;
  end;
end;

end.
