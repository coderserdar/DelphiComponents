unit Unit1;

interface

{==============================================================================}
{ This demo shows how to use Tags of items.                                    }
{ Each item has associated integer value - Tag. Tags are used to store         }
{ additional information, which RichView does not display and does not use.    }
{------------------------------------------------------------------------------}
{ All AddXXX methods (except from obsolete ones) has AddXXXTag version.        }
{ AddXXX(...) == AddXXXTag(...,0) (Tags=0 by default)                          }
{ For example, look AddNLTag and AddHotpotExTag in this demo.                  }
{ Tag of item can be obtained with GetItemTag method (or GetXXXInfo methods)   }
{ and modified with SetItemTag (or SetXXXInfo methods).                        }
{------------------------------------------------------------------------------}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, RVStyle, ExtCtrls, RVScroll, RichView;

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
  RichView1.AddNL('Some text styles can be chosen as hypertext styles. ',0,0);
  RichView1.AddNLTag('Like this one.',4,-1, 100);
  RichView1.AddNL(' You can have as many hypertext styles as you want.  ',0,-1);
  RichView1.AddNLTag('Here is one more.',5,-1,  200);
  RichView1.AddNL('Images from Image Lists also can be hypertext: ',0,0);
  RichView1.AddHotspotExTag('Pen Image', 0,1, ImageList1, -1, 300);
  RichView1.AddNL(' Such images are called "hotspots".',0,-1);
  RichView1.Format;
end;

procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
var ItemNo: Integer;
    Tag: Integer;
begin
  ItemNo := RichView1.GetJumpPointItemNo(id);
  Tag := RichView1.GetItemTag(ItemNo);
  Panel1.Caption := 'Clicked: Item with Tag='+IntToStr(Tag);
end;

end.
