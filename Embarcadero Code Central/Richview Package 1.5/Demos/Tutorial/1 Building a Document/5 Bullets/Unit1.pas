unit Unit1;
{==============================================================================}
{ This demo shows how to add controls to RichView                              }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichView1: TRichView;
    ImageList1: TImageList;
    ImageList2: TImageList;
    procedure FormCreate(Sender: TObject);
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
var i: Integer;
const crlf = #13#10;
begin
  RichView1.Clear;
  RichView1.AddNL('Example of adding bullets', 1, 1);

  // AddTextNL methods can add several paragraphs of text.
  // Paragraphs can be separated with #13#10, #13 or #10 characters.
  RichView1.AddTextNL('"Bullets" are the images from ImageLists. '+crlf+
                  '"Bullets" provide an efficient way to store graphics in RichView, '+
                  'because only link to ImageList and index of image are stored '+
                  'per "bullet" in memory.'+crlf+
                  '"Bullets" are useful when you need to add large number of the '+
                  'same picture in RichView: ', 0, 0, 0);
  for i := 0 to 9 do
    RichView1.AddBulletEx('', 0, ImageList1, -1);
  RichView1.AddNL('You can use as many ImageList, as you wish:',0,0);
  RichView1.AddBulletEx('', 0, ImageList1, -1);
  RichView1.AddBulletEx('', 0, ImageList2, -1);
  RichView1.AddBulletEx('', 1, ImageList1, -1);
  RichView1.AddBulletEx('', 1, ImageList2, -1);
  RichView1.Format;

  // About AddBulletEx:
  // Parameters of this method are similar with parameters of
  // AddPictureEx and AddControlEx:
  // 1st parameter: name of bullet. Allows to hold additional text information
  //  together with bullet. There is no predefined meaning of this
  //  parameter. May be it will be used to display hints in future.
  // 2nd parameter: index of image
  // 3rd parameter: ImageList; RichView holds only link to this image list, not
  // a copy of it;
  // 4th parameter: index of paragraph style (-1 to continue paragraph)

end;

end.
