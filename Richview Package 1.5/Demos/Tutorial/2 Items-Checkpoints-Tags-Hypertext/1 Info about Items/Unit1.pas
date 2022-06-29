unit Unit1;

{==============================================================================}
{ This demo shows how to read information about items in RichView              }
{ Key methods and properties:                                                  }
{ - ItemCount,                                                                 }
{ - GetItemStyle,                                                              }
{ - GetTextInfo, GetBreakInfo, GetPictureInfo, GetControlInfo, GetBulletInfo,  }
{   GetHotspotInfo                                                             }
{ This demo also shows "hotspots"                                              }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVScroll, RichView, RVStyle, ImgList;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichView1: TRichView;
    RichView2: TRichView;
    Label1: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure FillLeftRichView;
    procedure FillRightRichView;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
{------------------------------------------------------------------------------}
procedure TForm1.FillLeftRichView;
var bmp: TBitmap;
    btn: TButton;
begin
  with RichView1 do  begin
    AddNL('Reading information about items',1,1);

    AddBreakEx(2, rvbsLine, clRed);

    AddNL('Image: ',0,0);
    bmp := TBitmap.Create;
    bmp.Width  := 60;
    bmp.Height := 40;
    bmp.Canvas.Pen.Color := clRed;
    bmp.Canvas.Brush.Color := clYellow;
    bmp.Canvas.Rectangle(0,0,60,40);
    AddPictureEx('Yellow box', bmp, -1, rvvaMiddle);

    AddBreak;

    AddNL('Bullet: ',0,0);
    AddBulletEx('Printer image',0, ImageList1, -1);

    AddBreak;

    AddNL('Hotspot: ',0,0);
    AddHotspotEx('Active printer image',1, 0, ImageList1, -1);
    AddNL('(move mouse to hotspot and you see picture changes;'+
          ' more information about hotspots will be in hypertext description)',
          0,-1);

    AddBreak;

    AddNL('Button: ',0,0);
    btn := TButton.Create(nil);
    btn.Width := 100;
    btn.Caption := 'Out of order';
    AddControlEx('Button example', btn, -1, rvvaBaseline);

    Format;
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.FillRightRichView;
var
    i, ItemStyle: Integer;
    s: String;
    Tag: Integer;
    {.................................................................}
    procedure GetBreakInfo(ItemNo: Integer);
    var Width: Byte;
        Color: TColor;
        Style: TRVBreakStyle;
        Tag: Integer;
        ColorString: String;
    begin
      RichView1.GetBreakInfo(ItemNo, Width, Style, Color, Tag);
      if Color = clNone then
        ColorString := 'line has color of the 0-th text style'
      else
        ColorString := ColorToString(Color);
      RichView2.AddFmt('Item #%d: break. Color=%s; Width=%d',
                       [ItemNo, ColorString, Integer(Width)], 0, 0);
      // Style is always rvbsLine in current version
    end;
    {.................................................................}
    procedure GetPictureInfo(ItemNo: Integer);
    var gr: TGraphic;
        VAlign: TRVValign;
        VAString: String;
        Name: String;
        Tag: Integer;
    begin
      RichView1.GetPictureInfo(ItemNo, Name, gr, VAlign, Tag);
      case VAlign of
        rvvaBaseline:
          VAString := 'text base line';
        rvvaMiddle:
          VAString := 'text middle';
        else
          VAString := '?'
      end;
      RichView2.AddFmt('Item #%d: image (%s, %dx%d); Vertical alignment: %s; Name="%s"',
                       [ItemNo, gr.ClassName, gr.Width, gr.Height, VAString, Name],
                       0, 0);
    end;
    {.................................................................}
    procedure GetControlInfo(ItemNo: Integer);
    var ctrl: TControl;
        VAlign: TRVValign;
        VAString: String;
        Name: String;
        Tag: Integer;
    begin
      RichView1.GetControlInfo(ItemNo, Name, ctrl, VAlign, Tag);
      case VAlign of
        rvvaBaseline:
          VAString := 'text base line';
        rvvaMiddle:
          VAString := 'text middle';
        else
          VAString := '?'
      end;
      RichView2.AddFmt('Item #%d: control (%s, %dx%d); Vertical alignment: %s; Name="%s"',
                       [ItemNo, ctrl.ClassName, ctrl.Width, ctrl.Height, VAString, Name],
                       0, 0);
    end;
    {.................................................................}
    procedure GetBulletInfo(ItemNo: Integer);
    var ImageList: TImageList;
        Name: String;
        ImageIndex, Tag: Integer;
    begin
      RichView1.GetBulletInfo(ItemNo, Name, ImageIndex, ImageList, Tag);
      RichView2.AddFmt('Item #%d: bullet; ImageList="%s"; Image index=%d; Name="%s"',
                       [ItemNo, ImageList.Name, ImageIndex, Name],
                       0, 0);
    end;
    {.................................................................}
    procedure GetHotspotInfo(ItemNo: Integer);
    var ImageList: TImageList;
        Name: String;
        ImageIndex, HotImageIndex, Tag: Integer;
    begin
      RichView1.GetHotspotInfo(ItemNo, Name, ImageIndex, HotImageIndex, ImageList, Tag);
      RichView2.AddFmt('Item #%d: bullet; ImageList="%s"; Image index=%d, Hot image index=%d; Name="%s"',
                       [ItemNo, ImageList.Name, ImageIndex, HotImageIndex, Name],
                       0, 0);
    end;
    {.................................................................}
begin

  // 2. Reading information about items
  for i := 0 to RichView1.ItemCount-1 do begin
    ItemStyle := RichView1.GetItemStyle(i);
    if ItemStyle>=0 then begin
      // Parameter of GetItemStyle - index of item (0..ItemCount).
      // If GetItemStyle returns zero or positive value,
      // this item is text, and returned value is an index in
      // collection of styles (RVStyle.TextStyles)
      RichView1.GetTextInfo(i, s, Tag);
      RichView2.AddFmt('Item #%d: text. Value="%s". Style of text is "%s"',
                       [i, s, RVStyle1.TextStyles[ItemStyle].StyleName],0,0);
      end
    else
      // If GetItemStyle returns negative value, this item is a non-text item
      case ItemStyle of
        rvsBreak:
          GetBreakInfo(i);
        rvsPicture:
          GetPictureInfo(i);
        rvsComponent:
          GetControlInfo(i);
        rvsBullet:
          GetBulletInfo(i);
        rvsHotspot:
          GetHotspotInfo(i);
        else
          RichView2.AddNL('Unknown item type', 0, 0);
      end;
    RichView2.AddBreak;
  end;
  RichView2.Format;
end;
{------------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FillLeftRichView;
  FillRightRichView;
end;

end.
