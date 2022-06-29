unit Unit1;
{==============================================================================}
{ This demo shows how to add pictures and horizontal lines into RichView.      }
{                                                                              }
{ This demo also shows how to use background image.                            }
{ RichView1.BackgroundBitmap is assigned to some image, and                    }
{ RichView1.BackgroundStyle is set to bsTiled                                  }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichView1: TRichView;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
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
var ico: TIcon;
    bmp: TBitmap;
begin

  RichView1.Clear;
  RichView1.AddNL('Example of adding images', 1, 1);

  // Adding "break" - horizontal line
  RichView1.AddBreak;

  RichView1.AddNL('Adding icon:', 0, 0);
  // RichView frees inserted graphics when needed.
  // So RichView1.AddPictureEx('', Image1.Picture.Graphic, -1, rvvaBaseline)
  // will cause error. So we need to create copy of graphics.

  ico := TIcon.Create;
  ico.Assign(Image1.Picture.Graphic);
  RichView1.AddPictureEx('', ico, -1, rvvaBaseline);

  RichView1.AddNL('Adding bitmap:', 0, 0);

  // Adding bitmap from file:
  bmp := TBitmap.Create;
  bmp.LoadFromFile(ExtractFilePath(Application.ExeName)+'bars.bmp');
  RichView1.AddPictureEx('', bmp, -1, rvvaMiddle);

  RichView1.AddBreak;

  RichView1.Format;

  // About AddPictureEx:
  // 1st parameter: name of picture. Allows to hold additional text information
  //  together with image. There is no predefined meaning of this
  //  parameter. May be in future this string will be shown as hints.
  // 2nd parameter: image. TBitmap, TIcon, TMetafile, etc.
  // 3rd parameter: index of paragraph style (-1 to continue paragraph)
  // 4th parameter: vertical align of image.
  //  In current version RichView understands two options:
  //  - rvvaBaseline: align bottom of image to base line of text;
  //  - rvvaMiddle: align middle of image to base line of text;
end;

end.
