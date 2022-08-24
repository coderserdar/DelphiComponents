unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TagCloud, ComCtrls, ExtCtrls, StdCtrls, Tabs;

type
  TfrmMain = class(TForm)
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    Panel3: TPanel;
    Panel4: TPanel;
    edFilter: TEdit;
    lbSearch: TLabel;
    Bevel1: TBevel;
    tsPages: TTabSet;
    ScrollBox: TScrollBox;
    TagCloud: TTagCloud;
    pcMain: TPageControl;
    TabSheet3: TTabSheet;
    Label4: TLabel;
    cbLogScale: TCheckBox;
    cbColorLevels: TCheckBox;
    edIncr: TEdit;
    cbIncr: TCheckBox;
    cbAutoSize: TCheckBox;
    cbAutoShrinkRows: TCheckBox;
    cbFixedColWidth: TCheckBox;
    edFixedColWidth: TEdit;
    cbCustomDraw: TCheckBox;
    cbAlignment: TComboBox;
    cbEnabled: TCheckBox;
    cbOpacity: TCheckBox;
    cbSortByValue: TCheckBox;
    cbSpeedSort: TCheckBox;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    sbLoadCountries: TButton;
    sbLoadCities: TButton;
    sbHugeCSV: TButton;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    mText: TMemo;
    Panel5: TPanel;
    Label3: TLabel;
    sbParse: TButton;
    mExceptWords: TMemo;
    cbLowerCase: TCheckBox;
    cbNativeFrames: TCheckBox;
    procedure statusItemInfo(Item: TTagCloudItem);
    procedure TagCloudHoverChange(Sender: TObject; Item: TTagCloudItem);
    procedure TagCloudTagHint(Sender: TObject; Item: TTagCloudItem; var HintText: string);
    procedure TagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure sbLoadCountriesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbLoadCitiesClick(Sender: TObject);
    procedure sbParseClick(Sender: TObject);
    procedure cbColorLevelsClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure TagCloudPageCountChanged(Sender: TObject);
    procedure tsPagesChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure sbHugeCSVClick(Sender: TObject);
    procedure TagCloudShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure cbSortByValueClick(Sender: TObject);
    function TagCloudCompareItems(Sender: TObject; Item1, Item2: TTagCloudItem): Integer;
    procedure TagCloudAdvancedCustomDrawItem(Sender: TObject; TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect;
      var FrameRect, ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw: Boolean);
    procedure SetOptions(Sender:TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  tagCloud.UseSpeedSort:=True;
  cbColorLevelsClick(nil);
  SetOptions(nil);
  sbLoadCountriesClick(nil);
end;

procedure TfrmMain.SetOptions(Sender:TObject);
begin
  if tagCloud.Items.UpdateCount>0 then
    Exit;
  tagCloud.Items.BeginUpdate;
  try
    tagCloud.AutoSize:=cbAutoSize.Checked;
    if cbAutoSize.Checked then
      tagCloud.Align:=alTop
    else
      tagCloud.Align:=alClient;

    if cbFixedColWidth.Checked then
      tagCloud.FixedColWidth:=StrToIntDef(edFixedColWidth.Text,0)
    else
      tagCloud.FixedColWidth:=0;
    if tagCloud.FixedColWidth>0 then
    begin
      tagCloud.HoverEnlarge:=False;
      tagCloud.Font.Size:=11;
      tagCloud.MaxFontSize:=11;
    end
    else
    begin
      tagCloud.HoverEnlarge:=True;
      tagCloud.Font.Size:=8;
      tagCloud.MaxFontSize:=42;
    end;

    tagCloud.Alignment:=TAlignment(cbAlignment.ItemIndex);
    tagCloud.AutoShrinkRows:=cbAutoShrinkRows.Checked;

    if cbCustomDraw.Checked then
      tagCloud.OnAdvancedCustomDrawItem:=TagCloudAdvancedCustomDrawItem
    else
      tagCloud.OnAdvancedCustomDrawItem:=nil;

    if cbNativeFrames.Checked then
    begin
      tagCloud.ItemFrame.FrameSize:=1;
      tagCloud.HoverFrame.FrameSize:=1;
      tagCloud.SelectedFrame.FrameSize:=1;
      tagCloud.ItemFrame.FrameMargin:=2;
      tagCloud.HoverFrame.FrameMargin:=2;
      tagCloud.SelectedFrame.FrameMargin:=2;
      if cbCustomDraw.Checked then
      begin
        tagCloud.ItemFrame.BackColor:=clNone;
        tagCloud.HoverFrame.BackColor:=clNone;
        tagCloud.SelectedFrame.BackColor:=clNone;
      end
      else
      begin
        tagCloud.ItemFrame.BackColor:=clNone;
        tagCloud.HoverFrame.BackColor:=$00A7D6FC;
        tagCloud.SelectedFrame.BackColor:=clInfoBk;
      end;
      if tagCloud.FixedColWidth>0 then
      begin
        tagCloud.ItemFrame.RoundedSize:=0;
        tagCloud.HoverFrame.RoundedSize:=0;
        tagCloud.SelectedFrame.RoundedSize:=0;
      end
      else
      begin
        tagCloud.ItemFrame.RoundedSize:=3;
        tagCloud.HoverFrame.RoundedSize:=3;
        tagCloud.SelectedFrame.RoundedSize:=3;
      end;
    end
    else
    begin
      tagCloud.ItemFrame.FrameSize:=0;
      tagCloud.HoverFrame.FrameSize:=0;
      tagCloud.SelectedFrame.FrameSize:=0;
      tagCloud.ItemFrame.BackColor:=clNone;
      tagCloud.HoverFrame.BackColor:=clNone;
      tagCloud.SelectedFrame.BackColor:=clNone;
      if cbCustomDraw.Checked then
      begin
        tagCloud.ItemFrame.FrameMargin:=2;
        tagCloud.HoverFrame.FrameMargin:=2;
        tagCloud.SelectedFrame.FrameMargin:=2;
      end
      else
      begin
        tagCloud.ItemFrame.FrameMargin:=0;
        tagCloud.HoverFrame.FrameMargin:=0;
        tagCloud.SelectedFrame.FrameMargin:=0;
      end;
    end;

    if cbNativeFrames.Checked or (tagCloud.FixedColWidth>0) then
    begin
      tagCloud.HoverStyle:=tagCloud.Font.Style;
      tagCloud.ShrinkDiacritic:=False;
    end
    else
    begin
      tagCloud.HoverStyle:=[fsBold,fsUnderline];
      tagCloud.ShrinkDiacritic:=True;
    end;
    tagCloud.LogScale:=cbLogScale.Checked;
    tagCloud.UseSpeedSort := cbSpeedSort.Checked;
    tagCloud.Enabled:=cbEnabled.Checked;
  finally
    tagCloud.Items.EndUpdate;
  end;
end;

procedure TfrmMain.cbColorLevelsClick(Sender: TObject);
begin
  TagCloud.Colors.BeginUpdate;
  try
    if (Sender=cbOpacity) and cbOpacity.Checked then
    begin
      cbColorLevels.Checked:=False;
      cbCustomDraw.Checked:=False;
    end
    else
    if (Sender=cbColorLevels) and (cbColorLevels.Checked) then
      cbOpacity.Checked:=False;

    TagCloud.Colors.Clear;
    if cbOpacity.Checked then
    begin
      TagCloud.Colors.Add.Color:=$00D7BFA8;
      TagCloud.Colors.Add.Color:=$00CF9F70;
      TagCloud.Colors.Add.Color:=$00D38A41;
      TagCloud.Colors.Add.Color:=$00AF6010;
      TagCloud.Colors.Add.Color:=$00804000;
    end
    else
    if cbColorLevels.Checked then
    begin
      TagCloud.Colors.Add.Color:=clWindowText;
      TagCloud.Colors.Add.Color:=clTeal;
      TagCloud.Colors.Add.Color:=$00FFA448;
      TagCloud.Colors.Add.Color:=$004080FF;
    end
  finally
    TagCloud.Colors.EndUpdate;
  end;
end;

procedure TfrmMain.cbSortByValueClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    if Assigned(tagCloud.OnCompareItems) then
      tagCloud.OnCompareItems:=nil
    else
      tagCloud.OnCompareItems:=tagCloudCompareItems;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.sbLoadCountriesClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    tagCloud.LoadingOptions.AlphaSort:=not Assigned(tagCloud.OnCompareItems);
    tagCloud.LoadingOptions.LowerCase:=True;
    tagCloud.LoadingOptions.SkipFirstRow:=True;
    tagCloud.LoadingOptions.MaxItemsCount:=-1;
    tagCloud.LoadingOptions.ColCaption:=0;
    tagCloud.LoadingOptions.ColValue:=1;
    tagCloud.LoadingOptions.ValuesGreaterThan:=0;
    tagCloud.LoadingOptions.ColHint:=2;
    tagCloud.LoadingOptions.ColTag:=-1;
    tagCloud.LoadingOptions.ColData:=-1;
    tagCloud.LoadingOptions.Separator:=';';
    tagCloud.LoadFromFile(ExtractFilePath(ParamStr(0))+'Country.csv');
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.sbLoadCitiesClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    tagCloud.LoadingOptions.AlphaSort:=not Assigned(tagCloud.OnCompareItems);
    tagCloud.LoadingOptions.LowerCase:=True;
    tagCloud.LoadingOptions.SkipFirstRow:=True;
    tagCloud.LoadingOptions.MaxItemsCount:=-1;
    tagCloud.LoadingOptions.ColCaption:=1;
    tagCloud.LoadingOptions.ColValue:=2;
    tagCloud.LoadingOptions.ValuesGreaterThan:=0;
    tagCloud.LoadingOptions.ColHint:=3;
    tagCloud.LoadingOptions.ColTag:=-1;
    tagCloud.LoadingOptions.ColData:=-1;
    tagCloud.LoadingOptions.Separator:=';';
    tagCloud.LoadFromFile(ExtractFilePath(ParamStr(0))+'Cities.csv');
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.sbHugeCSVClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    tagCloud.LoadingOptions.AlphaSort:=not Assigned(tagCloud.OnCompareItems);
    tagCloud.LoadingOptions.LowerCase:=False;
    tagCloud.LoadingOptions.SkipFirstRow:=False;
    tagCloud.LoadingOptions.MaxItemsCount:=-1;
    tagCloud.LoadingOptions.ColCaption:=0;
    tagCloud.LoadingOptions.ColValue:=1;
    tagCloud.LoadingOptions.ValuesGreaterThan:=0;
    tagCloud.LoadingOptions.ColHint:=-1;
    tagCloud.LoadingOptions.ColTag:=-1;
    tagCloud.LoadingOptions.ColData:=-1;
    tagCloud.LoadingOptions.Separator:=';';
    tagCloud.LoadFromFile(ExtractFilePath(ParamStr(0))+'AllCities.csv');
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.sbParseClick(Sender: TObject);
var
  startpos, endpos: Integer;
  S:string;
  c:string;
begin
  Screen.Cursor:=crHourGlass;
  tagCloud.Items.BeginUpdate;
  try
    mExceptWords.Lines.Text:=AnsiLowerCase(mExceptWords.Lines.Text);
    tagCloud.Items.Clear;
    S:=mText.Lines.Text;
    startpos := 1;
    while startpos <= Length(S) do
    begin
      while (startpos <= Length(S)) and not IsCharAlpha(S[startpos]) do
        Inc(startpos);
      if startpos <= Length(S) then
      begin
        endpos := startpos + 1;
        while (endpos <= Length(S)) and IsCharAlpha(S[endpos]) do
          Inc(endpos);

        c:=Copy(S, startpos, endpos - startpos);
        if mExceptWords.Lines.IndexOf(AnsiLowerCase(c))=-1 then
        begin
          if cbLowerCase.Checked then
            TagCloud.Items.IncreaseValue(AnsiLowerCase(c))
          else
            TagCloud.Items.IncreaseValue(c);
        end;

        startpos := endpos + 1;
      end;
    end;
  finally
    tagCloud.Items.EndUpdate;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.TagCloudAdvancedCustomDrawItem(Sender: TObject; TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect;
  var FrameRect, ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw: Boolean);
var
  R:TRect;
  oldclr:TColor;
begin
  DefaultDraw:=True;
  TargetCanvas.Brush.Style := bsSolid;

  if TTagCloud(Sender).FixedColWidth>0 then
  begin
    if Item=TTagCloud(Sender).HoverItem then
    begin
      TargetCanvas.Pen.Color:=clGray;
      TargetCanvas.Brush.Color:=$00A7D6FC
    end
    else
    begin
      TargetCanvas.Pen.Color:=clSilver;
      TargetCanvas.Brush.Color:=clInfoBk;
    end;
    TargetCanvas.Rectangle(FrameRect);
    if cbNativeFrames.Checked then
    begin
      TargetCanvas.Brush.Style := bsClear;
      R:=ItemRect;
      R.Left:=FrameRect.Right+8;
      Windows.DrawTextW(TargetCanvas.Handle, '- '+IntToStr(Item.Value), -1, R, TextFlags);
    end;
  end
  else
  begin
    if not cbNativeFrames.Checked then
    begin
      if Item=TTagCloud(Sender).HoverItem then
      begin
        TargetCanvas.Pen.Color:=$00C27127;
        TargetCanvas.Brush.Color:=$00C27127;
        TargetCanvas.Font.Color:=clHighlightText;
      end
      else
      begin
        TargetCanvas.Pen.Color:=$00FFEFDA;
        TargetCanvas.Brush.Color:=$00FFEFDA;
      end;
      TargetCanvas.RoundRect(FrameRect,10,10);
    end;
    if cbNativeFrames.Checked or (Item<>TTagCloud(Sender).HoverItem) then
    begin
      R:=ItemRect;
      OffsetRect(R,1,1);
      oldClr:=TargetCanvas.Font.Color;
      TargetCanvas.Font.Color:=clSilver;
      TargetCanvas.Brush.Style := bsClear;
      Windows.DrawTextW(TargetCanvas.Handle, Item.Caption, -1, R, TextFlags);
      TargetCanvas.Font.Color:=oldClr;
    end;
  end;
end;

function TfrmMain.TagCloudCompareItems(Sender: TObject; Item1, Item2: TTagCloudItem): Integer;
begin
  if Item1.Value<Item2.Value then
    Result:=1
  else
  if Item1.Value>Item2.Value then
    Result:=-1
  else
    Result:=CompareText(Item1.Caption,Item2.Caption);
end;

procedure TfrmMain.statusItemInfo(Item: TTagCloudItem);
begin
  if Assigned(Item) then
    StatusBar.Panels[4].Text:=Format('%s: %d',[Item.Caption,Item.Value])
  else
    StatusBar.Panels[4].Text:='';
end;

procedure TfrmMain.TagCloudHoverChange(Sender: TObject; Item: TTagCloudItem);
begin
  statusItemInfo(Item);
end;

procedure TfrmMain.tsPagesChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  tagCloud.PageIndex:=NewTab;
end;

procedure TfrmMain.TagCloudPageCountChanged(Sender: TObject);
var
  i,oi:Integer;
begin
  oi:=tagCloud.PageIndex;
  tsPages.OnChange:=nil;
  tsPages.Tabs.BeginUpdate;
  try
    tsPages.Tabs.Clear;
    for i:=1 To TagCloud.PageCount do
      tsPages.Tabs.Add(' '+IntToStr(i)+' ');
  finally
    tsPages.Tabs.EndUpdate;
    tsPages.TabIndex:=oi;
    tsPages.OnChange:=tsPagesChange;
  end;
end;

procedure TfrmMain.edFilterChange(Sender: TObject);
begin
  tagCloud.Filter:=edFilter.Text;
end;

procedure TfrmMain.TagCloudShow(Sender: TObject);
begin
  StatusBar.Panels[0].Text:=format('Page: %d/%d, Row count: %d/%d',[tagCloud.PageIndex+1,tagCloud.PageCount,tagCloud.VisibleRowCount,tagCloud.RowCount]);

  if tagCloud.DisplayCount=0 then
    StatusBar.Panels[1].Text:='This view did not match any item'
  else
  if Length(tagCloud.Filter)=0 then
    StatusBar.Panels[1].Text:=format('Showing %d item(s), from %d to %d',
      [tagCloud.DisplayCount,tagCloud.FirstDisplayIndex+1,tagCloud.LastDisplayIndex+1])
  else
    StatusBar.Panels[1].Text:=format('Showing %d item(s)',[tagCloud.DisplayCount]);

  if Length(tagCloud.Filter)=0 then
    StatusBar.Panels[2].Text:=''
  else
    StatusBar.Panels[2].Text:='Items found: '+IntToStr(tagCloud.VisibleCount);

  StatusBar.Panels[3].Text:='Items total: '+IntToStr(tagCloud.Items.Count);
end;

procedure TfrmMain.TagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
begin
  if Assigned(Item) and cbIncr.Checked then
  begin
    Item.Value:=Item.Value+strtointdef(edIncr.Text,0);
    statusItemInfo(Item);
  end;
end;

procedure TfrmMain.TagCloudTagHint(Sender: TObject; Item: TTagCloudItem; var HintText: string);
begin
  if Assigned(Item) then
  begin
    HintText:=
      Item.Caption+#13#10+
      'Value: '+IntToStr(Item.Value)+#13#10+
      'Hint: '+Item.Hint+#13#10+
      'Tag: '+IntToStr(Item.Tag);
  end;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if PtInRect(tagCloud.ClientRect,tagCloud.ScreenToClient(MousePos)) then
  begin
    if tagCloud.AutoSize and ScrollBox.VertScrollBar.IsScrollBarVisible then
    begin
      ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position - WheelDelta div 5;
      Handled := True;
    end
    else
    if cbIncr.Checked then
    begin
      if Assigned(tagCloud.HoverItem) then
      begin
        if WheelDelta>0 then
          tagCloud.HoverItem.Value:=tagCloud.HoverItem.Value+strtointdef(edIncr.Text,0)
        else
          tagCloud.HoverItem.Value:=tagCloud.HoverItem.Value-strtointdef(edIncr.Text,0);
        statusItemInfo(tagCloud.HoverItem);
        Handled := True
      end;
    end
    else
    begin
      if WheelDelta<0 then
        tagCloud.PageIndex:=tagCloud.PageIndex+1
      else
        tagCloud.PageIndex:=tagCloud.PageIndex-1;
      tsPages.TabIndex:=tagCloud.PageIndex;
      Handled := True;
    end;
  end
end;

end.
