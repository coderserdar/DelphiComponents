unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TagCloud, TagIndex, ComCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    pLeft: TPanel;
    splLeft: TSplitter;
    tiABC: TTagIndex;
    pTop: TPanel;
    sbOpenFile: TButton;
    reDoc: TRichEdit;
    cbCustomLabels: TCheckBox;
    odFile: TOpenDialog;
    cbShowPageNumbs: TCheckBox;
    cbPageBtns: TCheckBox;
    sbParse: TButton;
    Label1: TLabel;
    rgLabelsPos: TRadioGroup;
    ScrollBox: TScrollBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    tagCloud: TTagCloud;
    splRight: TSplitter;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cbCustomLabelsClick(Sender: TObject);
    procedure tiABCTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure sbOpenFileClick(Sender: TObject);
    procedure cbShowPageNumbsClick(Sender: TObject);
    procedure cbPageBtnsClick(Sender: TObject);
    procedure ParseDocument(Sender: TObject);
    procedure pLeftResize(Sender: TObject);
    procedure rgLabelsPosClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBoxResize(Sender: TObject);
    procedure tagCloudTagPositioning(Sender: TObject; Item: TTagCloudItem; CWidth, CHeight: Integer; const PageRect: TRect;
      var x, y: Integer; var breakKind: TTagItemBreakKind);
    procedure tagCloudAdvancedCustomDrawItem(Sender: TObject; TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect;
      var FrameRect, ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OpenDoc(FileName:string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  tagCloud.AutoSize:=True;
  tagCloud.UseSpeedSort:=True;
  tiABC.UseSpeedSort:=True;
  OpenDoc(extractfilepath(ParamStr(0))+'TagCloud for VCL.rtf');
  cbCustomLabels.Checked:=True;
end;

procedure TfrmMain.OpenDoc(FileName:string);
begin
  if fileexists(FileName) then
  begin
    reDoc.Lines.LoadFromFile(FileName);
    odFile.FileName:=FileName;
    ParseDocument(nil);
  end;
end;

procedure TfrmMain.ParseDocument(Sender: TObject);
var
  startpos, endpos: Integer;
  S:string;
  c:string;
  SL:string;
  ExceptWords:TStringList;

  i:Integer;
  FLastIDXLbl:string;
  Item:TTagCloudItem;
begin
  Screen.Cursor:=crHourGlass;
  ExceptWords:=TStringList.Create;
  tiABC.Items.BeginUpdate;
  tagCloud.Items.BeginUpdate;
  FLastIDXLbl:='';
  SL:=tiABC.SelectedLabel;
  try
    tiABC.SelectedLabel:='';
    tiABC.Items.Clear;
    tagCloud.Items.Clear;
    ExceptWords.CommaText:='a,an,and,are,as,be,can,for,from,has,in,into,of,or,that,the,to,what,where,which';
    S:=reDoc.Lines.Text;
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
        if (Length(c)>1) and (ExceptWords.IndexOf(AnsiLowerCase(c))=-1) then
          tiABC.Items.IncreaseValue(AnsiLowerCase(c));

        startpos := endpos + 1;
      end;
    end;
    tagCloud.Items.Assign(tiABC.Items);
    tagCloud.Sort;
    FLastIDXLbl:='';
    for i:=0 To tagCloud.Items.Count-1 do
    begin
      Item:=tagCloud.Items[i];
      if AnsiUpperCase(Copy(Item.Caption,1,1))<>FLastIDXLbl then
      begin
        Item.Tag:=1; // markup the item as the beginning of the next character group
        FLastIDXLbl:=AnsiUpperCase(Copy(Item.Caption,1,1));
      end;
    end;
  finally
    tiABC.Items.EndUpdate;
    tagCloud.Items.EndUpdate;
    tiABC.SelectedLabel:=SL;
    ExceptWords.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.pLeftResize(Sender: TObject);
begin
  if tiABC.Width<128 then
    tiABC.FixedColCount:=1
  else
  if tiABC.FixedColCount>0 then
  begin
    tiABC.Items.BeginUpdate;
    try
      tiABC.FixedColCount:=0;
      tiABC.FixedColWidth:=120;
    finally
      tiABC.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmMain.ScrollBoxResize(Sender: TObject);
begin
  if tagCloud.Width<128 then
    tagCloud.FixedColCount:=1
  else
  if tagCloud.FixedColCount>0 then
  begin
    tagCloud.Items.BeginUpdate;
    try
      tagCloud.FixedColCount:=0;
      tagCloud.FixedColWidth:=120;
    finally
      tagCloud.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmMain.rgLabelsPosClick(Sender: TObject);
begin
  case rgLabelsPos.ItemIndex of
    1:tiABC.LabelsPosition:=tlpLeft;
    2:tiABC.LabelsPosition:=tlpRight;
  else
    tiABC.LabelsPosition:=tlpTop;
  end;
end;

procedure TfrmMain.sbOpenFileClick(Sender: TObject);
begin
  if odFile.Execute then
    OpenDoc(odFile.FileName);
end;

procedure TfrmMain.tagCloudTagPositioning(Sender: TObject; Item: TTagCloudItem; CWidth, CHeight: Integer; const PageRect: TRect;
  var x, y: Integer; var breakKind: TTagItemBreakKind);
begin
  if Item.Tag=1 then // item is marked up as a beginning of the characters group
  begin
    breakKind:=tibSpacer;
    y:=y+20;
  end;
end;

procedure TfrmMain.tagCloudAdvancedCustomDrawItem(Sender: TObject; TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect;
  var FrameRect, ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw: Boolean);
var
  R:TRect;
  fh:Integer;
  fs:TFontStyles;
  fc:TColor;
begin
  if Item.Tag=1 then // item is marked up as a beginning of the characters group
  begin
    R:=ItemRect;
    OffsetRect(R,0,-17);
    fh:=TargetCanvas.Font.Size;
    fs:=TargetCanvas.Font.Style;
    fc:=TargetCanvas.Font.Color;
    TargetCanvas.Font.Size:=12;
    TargetCanvas.Font.Color:=clNavy;
    TargetCanvas.Font.Style:=TargetCanvas.Font.Style+[fsBold];
    DrawText(TargetCanvas.Handle,PChar(AnsiUpperCase(Copy(Item.Caption,1,1))),1,R,TextFlags);
    TargetCanvas.Font.Size:=fh;
    TargetCanvas.Font.Style:=fs;
    TargetCanvas.Font.Color:=fc;
  end;
end;

procedure TfrmMain.tiABCTagClick(Sender: TObject; Item: TTagCloudItem);
var
  l:Integer;
  i,j:Integer;
begin
  l:=Length(reDoc.Lines.Text);
  j:=reDoc.SelStart+reDoc.SelLength;
  i:=reDoc.FindText(Item.Caption,j,l-j,[stWholeWord]);
  if (i<0) and (j>0) then
    i:=reDoc.FindText(Item.Caption,0,l,[stWholeWord]);
  if i>-1 then
  begin
    reDoc.SelStart:=i;
    reDoc.SelLength:=Length(Item.Caption);
  end;
end;

procedure TfrmMain.cbCustomLabelsClick(Sender: TObject);
var
  i:Integer;
begin
  if cbCustomLabels.Checked then
  begin
    tiABC.CustomLabels.BeginUpdate;
    try
      tiABC.CustomLabels.Clear;
      tiABC.CustomLabels.Add('All words|'); // no string after | (pipe) defines an empty filter - all tags will be shown
      tiABC.CustomLabels.Add('  ||');        // | (pipe) as a filter defines the separator. A string before the first pipe is displayed as a label
      for i:=Ord('A') To Ord('Z') do
        tiABC.CustomLabels.Add(Chr(i));
      { Other examples of defining custom filter. You can also handle the filters in OnFilterLabel event.
      tiABC.CustomLabels.Add('|||');       // ||| (three pipes) defines the separator with | displayed
      tiABC.CustomLabels.Add('123|?');     // label '123', filter '?'
      }
    finally
      tiABC.CustomLabels.EndUpdate;
    end;
    tiABC.SelectedLabel:='All words';
  end
  else
  begin
    tiABC.CustomLabels.Clear;
    tiABC.SelectedLabel:='A';
  end;
end;

procedure TfrmMain.cbPageBtnsClick(Sender: TObject);
begin
  if cbPageBtns.Checked then
    tiABC.PageNumberStyle:=pnsButtons
  else
    tiABC.PageNumberStyle:=pnsTabs
end;

procedure TfrmMain.cbShowPageNumbsClick(Sender: TObject);
begin
  tiABC.ShowPageNumbers:=cbShowPageNumbs.Checked;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if PtInRect(tiABC.ClientRect,tiABC.ScreenToClient(MousePos)) then
  begin
    if tiABC.PageCount>1 then
    begin
      if WheelDelta<0 then
        tiABC.PageIndex:=tiABC.PageIndex+1
      else
        tiABC.PageIndex:=tiABC.PageIndex-1;
      Handled := True;
    end;
  end
  else
  if PtInRect(ScrollBox.ClientRect,ScrollBox.ScreenToClient(MousePos)) then
  begin
    if tagCloud.AutoSize and ScrollBox.HorzScrollBar.IsScrollBarVisible then
    begin
      ScrollBox.HorzScrollBar.Position := ScrollBox.HorzScrollBar.Position - WheelDelta div 4;
      Handled := True;
    end
  end
end;

end.
