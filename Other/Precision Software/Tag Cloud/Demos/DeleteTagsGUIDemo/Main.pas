unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, TagCloud, ExtCtrls;

type
  TForm3 = class(TForm)
    imgDel: TImage;
    Panel1: TPanel;
    tgcEdit: TTagCloud;
    procedure tgcEditAdvancedCustomDrawItem(Sender: TObject;
      TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect;
      var FrameRect, ItemRect: TRect; var TextFlags: Cardinal;
      var DefaultDraw: Boolean);
    procedure tgcEditTagClick(Sender: TObject; Item: TTagCloudItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.tgcEditAdvancedCustomDrawItem(Sender: TObject;
  TargetCanvas: TCanvas; Item: TTagCloudItem; TextRect: TRect; var FrameRect,
  ItemRect: TRect; var TextFlags: Cardinal; var DefaultDraw: Boolean);
var
  R:TRect;
begin
  with TCustomTagCloud(Sender) do
  begin
    if HoverColSpace then
    begin
      R:=FrameRect;
      if ValidateItemHoverColSpaceRect(Item, R) then
      begin
        TargetCanvas.Brush.Style:=bsSolid;
        TargetCanvas.Brush.Color:=HoverFrame.BackColor;
        TargetCanvas.Pen.Color:=HoverFrame.FrameColor;
        TargetCanvas.Pen.Width:=HoverFrame.FrameSize;
        TargetCanvas.Pen.Style:=HoverFrame.FrameStyle;
        TargetCanvas.RoundRect(R, HoverFrame.RoundedSize, HoverFrame.RoundedSize);

        if R.Right>FrameRect.Right then
          R.Left:=FrameRect.Right-HoverFrame.FrameMargin-2
        else
          R.Right:=FrameRect.Left+HoverFrame.FrameMargin+2;

        TargetCanvas.MoveTo(R.Left,R.Top);
        TargetCanvas.LineTo(R.Left,R.Bottom);
        TargetCanvas.Draw(R.Left+(R.Right-R.Left-imgDel.Picture.Graphic.Width) div 2,
          R.Top+(R.Bottom-R.Top-imgDel.Picture.Graphic.Height) div 2, imgDel.Picture.Graphic);

        TargetCanvas.Pen.Style:=psClear;
        TargetCanvas.Brush.Style:=bsClear;
      end;
    end;
  end;
end;

procedure TForm3.tgcEditTagClick(Sender: TObject; Item: TTagCloudItem);
var
  m:TTagCloudItemMetrics;
  p:TPoint;
  R:TRect;
begin
  TCustomTagCloud(Sender).GetItemMetrics(Item, m);
  R:=m.FrameRectHovered;
  inflateRect(R,-TCustomTagCloud(Sender).HoverFrame.FrameMargin,0);
  GetCursorPos(p);
  p:=TCustomTagCloud(Sender).ScreenToClient(p);
  if PtInRect(R, p) then
    ShowMessage('Item "'+Item.Caption+'" has been clicked')
  else
    TCustomTagCloud(Sender).Items.Delete(Item.Index);
end;

end.
