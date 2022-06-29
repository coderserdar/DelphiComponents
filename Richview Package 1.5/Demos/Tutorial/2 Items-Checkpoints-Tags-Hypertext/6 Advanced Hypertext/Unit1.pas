unit Unit1;

interface

{==============================================================================}
{ This demo show how to use information about hypertext item.                  }
{------------------------------------------------------------------------------}
{ Key methods:                                                                 }
{ - GetJumpPointItemNo;                                                        }
{ - methods for obtaining information about items (see first demo in this      }
{   group.                                                                     }
{ Useful if text is self-describing (for example, for WWW addresses).          }
{==============================================================================}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, ExtCtrls, RVScroll, RichView, StdCtrls, ImgList;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    Panel1: TPanel;
    RVStyle1: TRVStyle;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure RichView1Jump(Sender: TObject; id: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichView1.AddNL('Hypertext',1,1);
  RichView1.AddNL('Some text styles can be chosen as hypertext styles. ',0,0);
  RichView1.AddNL('Like this one.',4,-1);
  RichView1.AddNL(' You can have as many hypertext styles as you want.  ',0,-1);
  RichView1.AddNL('Here is one more.',5,-1);
  RichView1.AddNL('Images from Image Lists also can be hypertext: ',0,0);
  RichView1.AddHotspotEx('Pen Image', 0,1, ImageList1, -1);
  RichView1.AddNL(' Such images are called "hotspots".',0,-1);  
  RichView1.Format;
end;

procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
var ItemNo, ItemStyle, A,B: Integer;
    ImageList: TImageList;
    s: String;
    Tag: Integer;
begin
  ItemNo := RichView1.GetJumpPointItemNo(id);
  ItemStyle := RichView1.GetItemStyle(ItemNo);
  if ItemStyle>=0 then begin
    RichView1.GetTextInfo(ItemNo, s, Tag);
    Panel1.Caption := 'Clicked: text "'+s+'"';
    end
  else if ItemStyle=rvsHotspot then begin
    RichView1.GetHotspotInfo(ItemNo, s, A,B, ImageList, Tag);
    Panel1.Caption := 'Clicked: hotspot "'+s+'"';
  end;
end;

end.
