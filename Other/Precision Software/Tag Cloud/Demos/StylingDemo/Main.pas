unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, TagCloud, TagIndex, TagCloudPrmStyler, Menus;

type
  TfrmMain = class(TForm)
    pLeft: TPanel;
    splLeft: TSplitter;
    Panel1: TPanel;
    tagCloud: TTagCloud;
    tcStyler: TTagCloudPrmStyler;
    pBottom: TPanel;
    splBottom: TSplitter;
    tagCloudSmall: TTagCloud;
    Splitter1: TSplitter;
    tagCloudFixed: TTagCloud;
    pOptions: TPanel;
    cbIgnoreDims: TCheckBox;
    tiStyles: TTagIndex;
    Bevel1: TBevel;
    cbMultiSelect: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbIgnoreDimsClick(Sender: TObject);
    procedure tiStylesTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure tiStylesDblClick(Sender: TObject);
    procedure tcStylerStyleApplied(Sender: TObject);
    procedure tagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure tagCloudMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    procedure LoadStylesList;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  tagCloud.LoadingOptions.MaxItemsCount:=0;
  tagCloudSmall.LoadingOptions.MaxItemsCount:=0;
  tagCloudFixed.LoadingOptions.MaxItemsCount:=0;
  tagCloud.LoadFromFile(extractfilepath(ParamStr(0))+'tags.csv');
  tagCloudSmall.LoadFromFile(extractfilepath(ParamStr(0))+'tags.csv');
  tagCloudFixed.LoadFromFile(extractfilepath(ParamStr(0))+'tags.csv');
  LoadStylesList;
end;

procedure TfrmMain.LoadStylesList;
var
  i:Integer;
  SL,FL:TStringList;
  tmp:string;
  T:TTagCloudItem;

  procedure _GetStyleFiles(Dir:string;const SL:TStrings);
  var
    Found:integer;
    SR:TSearchRec;
  begin
    {$WARNINGS OFF}
    Found := FindFirst(Dir+'*.prm', faAnyFile-faDirectory-faVolumeID, SR);
    while Found = 0 do
      begin
        SL.Add(dir+sr.Name);
        Found := FindNext(SR);
      end;
    SysUtils.FindClose(SR);
    {$WARNINGS ON}
  end;

begin
  tiStyles.Items.BeginUpdate;
  SL:=TStringList.create;
  FL:=TStringList.Create;
  try
    tiStyles.Items.Clear;
    tiStyles.SelectedLabel:='';
    _GetStyleFiles(extractfilepath(ParamStr(0))+'Styles\',SL);
    for i:=0 To SL.Count-1 do
    begin
      FL.LoadFromFile(SL[i]);
      tmp:=trim(FL.Values['_stylename']);
      if Length(tmp)=0 then
        tmp:=changefileext(extractfilename(SL[i]),'');
      tiStyles.Items.AddItem(tmp,0,SL[i]);
    end;
  finally
    FL.Free;
    SL.Free;
    tiStyles.Items.EndUpdate;
    tiStyles.SelectedLabel:='All styles';
  end;
  if tiStyles.Items.Count>0 then
  begin
    T:=tiStyles.Items.Find('Mustang');
    if T=nil then
      tiStylesTagClick(tiStyles,tiStyles.Items[0])
    else
      tiStylesTagClick(tiStyles,T)
  end;
end;

procedure TfrmMain.cbIgnoreDimsClick(Sender: TObject);
begin
  tcStyler.IgnoreDimensions:=cbIgnoreDims.Checked;
  if tcStyler.IgnoreDimensions then
  begin
    // set the default dimensional properties
    tagCloud.Font.Size:=8;
    tagCloud.MaxFontSize:=26;
    tagCloud.AutoShrinkRows:=True;
    tagCloud.Alignment:=taCenter;
    tagCloud.ColSpacing:=6;
    tagCloud.RowSpacing:=0;
    tagCloud.FixedColCount:=0;
    tagCloud.FixedColWidth:=0;
    tagCloud.HoverEnlarge:=False;
    tagCloud.HoverStyle:=[fsUnderline];
    tagCloud.LogScale:=False;
    tagCloud.Padding.Left:=4;
    tagCloud.Padding.Top:=4;
    tagCloud.Padding.Right:=4;
    tagCloud.Padding.Bottom:=4;
    tagCloud.ShrinkDiacritic:=False;

    tagCloudSmall.Font.Size:=6;
    tagCloudSmall.MaxFontSize:=13;
    tagCloudSmall.AutoShrinkRows:=True;
    tagCloudSmall.Alignment:=taCenter;
    tagCloudSmall.ColSpacing:=3;
    tagCloudSmall.RowSpacing:=0;
    tagCloudSmall.FixedColCount:=0;
    tagCloudSmall.FixedColWidth:=0;
    tagCloudSmall.HoverEnlarge:=False;
    tagCloudSmall.HoverStyle:=[fsUnderline];
    tagCloudSmall.LogScale:=False;
    tagCloudSmall.Padding.Left:=4;
    tagCloudSmall.Padding.Top:=4;
    tagCloudSmall.Padding.Right:=4;
    tagCloudSmall.Padding.Bottom:=4;
    tagCloudSmall.ShrinkDiacritic:=False;

    tagCloudFixed.Font.Size:=9;
    tagCloudFixed.MaxFontSize:=9;
    tagCloudFixed.AutoShrinkRows:=True;
    tagCloudFixed.Alignment:=taLeftJustify;
    tagCloudFixed.ColSpacing:=2;
    tagCloudFixed.RowSpacing:=2;
    tagCloudFixed.FixedColCount:=0;
    tagCloudFixed.FixedColWidth:=110;
    tagCloudFixed.HoverEnlarge:=False;
    tagCloudFixed.HoverStyle:=[fsUnderline];
    tagCloudFixed.LogScale:=False;
    tagCloudFixed.Padding.Left:=4;
    tagCloudFixed.Padding.Top:=4;
    tagCloudFixed.Padding.Right:=4;
    tagCloudFixed.Padding.Bottom:=4;
    tagCloudFixed.ShrinkDiacritic:=False;

  end;
  tiStylesTagClick(tiStyles, tiStyles.SelectedItem);
end;


procedure TfrmMain.tagCloudMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and cbMultiSelect.Checked then
  begin
    if TCustomTagCloud(Sender).GetItemAt(X,Y)=nil then
      TCustomTagCloud(Sender).ClearSelection;
  end;
end;

procedure TfrmMain.tagCloudTagClick(Sender: TObject; Item: TTagCloudItem);
begin
  if cbMultiSelect.Checked then
  begin
    Item.Selected:=not Item.Selected;
    if Item.Selected then
      TCustomTagCloud(Sender).SelectedItem:=Item
    else
      TCustomTagCloud(Sender).SelectedItem:=nil;
  end
  else
    TCustomTagCloud(Sender).SelectedItem:=Item;
end;

procedure TfrmMain.tcStylerStyleApplied(Sender: TObject);
begin
  if Sender=tagCloud then
  begin
    tiStyles.Color:=tagCloud.Color;
    tiStyles.Font.Color:=tagCloud.Font.Color;
    if tagCloud.Colors.Count>0 then
      tiStyles.LabelFont.Color:=tagCloud.Colors[tagCloud.Colors.Count-1].Color
    else
      tiStyles.LabelFont.Color:=tagCloud.Font.Color;
    tiStyles.HoverColor:=tiStyles.LabelFont.Color;
    tiStyles.SelLabelFrame.FrameColor:=tiStyles.LabelFont.Color;
    tiStyles.SelLabelFrame.RoundedSize:=tagCloud.HoverFrame.RoundedSize;
    tiStyles.HoverFrame.RoundedSize:=tagCloud.HoverFrame.RoundedSize;
    tiStyles.SelectedFrame.RoundedSize:=tagCloud.HoverFrame.RoundedSize;
  end;
end;

procedure TfrmMain.tiStylesDblClick(Sender: TObject);
begin
  LoadStylesList;
end;

procedure TfrmMain.tiStylesTagClick(Sender: TObject; Item: TTagCloudItem);
begin
  if Item<>nil then
    tcStyler.LoadFromFile(Item.Hint);
  tiStyles.SelectedItem:=Item;
end;

end.
