unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TagCloud, StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    Splitter1: TSplitter;
    pLeft: TPanel;
    tags: TTagCloud;
    rgVAlign: TRadioGroup;
    sbRand: TButton;
    cbAutoScaleFont: TCheckBox;
    rgAlignment: TRadioGroup;
    Label1: TLabel;
    tbMinFontSize: TTrackBar;
    Label2: TLabel;
    tbMaxFontSize: TTrackBar;
    DblClickTimer: TTimer;
    procedure sbRandClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgVAlignClick(Sender: TObject);
    procedure cbAutoScaleFontClick(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure tbMaxFontSizeChange(Sender: TObject);
    procedure tbMinFontSizeChange(Sender: TObject);
    procedure tagsTagClick(Sender: TObject; Item: TTagCloudItem);
    procedure DblClickTimerTimer(Sender: TObject);
    procedure tagsTagDblClick(Sender: TObject; Item: TTagCloudItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.cbAutoScaleFontClick(Sender: TObject);
begin
  tags.AutoScaleFont:=cbAutoScaleFont.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DesktopFont:=True;
  sbRandClick(nil);
  DblClickTimer.Interval:=GetDoubleClickTime;
  if DblClickTimer.Interval=0 then
    DblClickTimer.Interval:=200;
end;

procedure TfrmMain.rgAlignmentClick(Sender: TObject);
begin
  case rgAlignment.ItemIndex of
    0:tags.Alignment:=taLeftJustify;
    2:tags.Alignment:=taRightJustify;
  else
    tags.Alignment:=taCenter;
  end;
end;

procedure TfrmMain.rgVAlignClick(Sender: TObject);
begin
  case rgVAlign.ItemIndex of
    1:tags.VerticalAlignment:=taVerticalCenter;
    2:tags.VerticalAlignment:=taAlignBottom;
  else
    tags.VerticalAlignment:=taAlignTop;
  end;
end;

procedure TfrmMain.sbRandClick(Sender: TObject);
var
  i,c:Integer;
begin
  tags.Items.BeginUpdate;
  try
    tags.Items.Clear;
    c:=10+Random(50);
    for i:=1 To c do
      tags.Items.AddItem('item-'+IntToStr(i),Random(3000));
  finally
    tags.Items.EndUpdate;
  end;
end;

procedure TfrmMain.tbMaxFontSizeChange(Sender: TObject);
begin
  tags.MaxFontSize:=tbMaxFontSize.Position;
end;

procedure TfrmMain.tbMinFontSizeChange(Sender: TObject);
begin
  tags.Font.Size:=tbMinFontSize.Position;
end;

procedure TfrmMain.tagsTagClick(Sender: TObject; Item: TTagCloudItem);
begin
  if Assigned(tags.OnTagDblClick) then
    DblClickTimer.Enabled:=true
  else
    DblClickTimerTimer(nil);
end;

procedure TfrmMain.tagsTagDblClick(Sender: TObject; Item: TTagCloudItem);
begin
  DblClickTimer.Enabled:=False;
  ShowMessage('Double click');
end;

procedure TfrmMain.DblClickTimerTimer(Sender: TObject);
begin
  DblClickTimer.Enabled:=False;
  ShowMessage('Single click');
end;

initialization

  Randomize;

end.
