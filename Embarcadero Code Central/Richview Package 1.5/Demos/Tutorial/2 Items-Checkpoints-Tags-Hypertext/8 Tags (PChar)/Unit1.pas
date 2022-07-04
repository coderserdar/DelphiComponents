unit Unit1;

interface

{==============================================================================}
{ This demo shows how to use Tags of items in mode, when Tags considered as    }
{ pointers to dynamically allocated strings (PChar).                           }
{ In this demo, rvoTagsArePChars set to Options (IMPORTANT!)                   }
{ In this mode, you need to allocate memory for tag strings with StrNew.       }
{ RichView will free this memory itself when needed.                           }
{------------------------------------------------------------------------------}
{ The key methods are the same as in previous demo.                            }
{ You still can use AddXXX, which set Tags to 0 (i.e. nil - empty string).     }
{------------------------------------------------------------------------------}
{ This is the most powerfull method for organizing hypertext, because you can  }
{ code any information that you need in string.                                }
{------------------------------------------------------------------------------}
{ IMPORTANT: Do not use spaces in Tag strings (you can use them, but you       }
{ will not be able to save such tags in RVF files)                             }
{ IMPORTANT: Do not use #0 in Tag strings (except from character closing       }
{ the string)                                                                  }
{==============================================================================}

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
  RichView1.AddNLTag('Like this one.',4,-1, Integer(StrNew('First_jump')));
  RichView1.AddNL(' You can have as many hypertext styles as you want.  ',0,-1);
  RichView1.AddNLTag('Here is one more.',5,-1,  Integer(StrNew('Second_jump')));
  RichView1.AddNL('Images from Image Lists also can be hypertext: ',0,0);
  RichView1.AddHotspotExTag('Pen Image', 0,1, ImageList1, -1, Integer(StrNew('Third_jump')));
  RichView1.AddNL(' Such images are called "hotspots".',0,-1);
  RichView1.Format;
end;

procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
var ItemNo: Integer;
    Tag: PChar;
begin
  ItemNo := RichView1.GetJumpPointItemNo(id);
  Tag := PChar(RichView1.GetItemTag(ItemNo));
  Panel1.Caption := 'Clicked: Item with Tag='+Tag;
end;

end.
