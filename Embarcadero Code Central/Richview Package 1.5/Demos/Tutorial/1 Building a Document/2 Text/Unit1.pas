unit Unit1;
{==============================================================================}
{ This demo shows how to add text in RichView at run-time.                     }
{ See comments in TForm1.FormCreate                                            }
{==============================================================================}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    RichView1: TRichView;
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
begin
  // Clear deletes all items of RichView.
  // There are not items at the beginning, so this call of Clear is
  // just for demonstration.
  RichView1.Clear;

  // Adding first paragraph.
  // This paragraph has the 1-th style (centered by default; you can view/modify
  // it in RVStyle1.ParaStyles property).
  // This paragraph consists of one item of the 1-th text style
  RichView1.AddNL('Adding Text', 1, 1);

  // Adding second paragraph.
  // This paragraph has the 0-th style (style is defined by first item in paragraph).
  RichView1.AddNL('This demo shows how to add text in ', 0, 0);
  // Continuing to add second paragraph.
  // Note: -1 is passed as index of paragraph style.
  // This means that this item will be added to last paragraph.
  RichView1.AddNL('RichView', 3, -1);
  // Continuing to add second paragraph...
  RichView1.AddNL('. There are two paragraphs in this document - '+
                  'first one with centered alignment, and second one - '+
                  'with left alignment. First paragraph consist of one text item, '+
                  'and second paragraph consists of three items. '+
                  'Each item is added with one call of AddNL method.', 0, -1);
  // But text will not displayed yet. You need to call Format method after adding
  // all contents to RichView:
  RichView1.Format;
end;

end.
