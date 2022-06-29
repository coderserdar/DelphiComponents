unit Unit1;
{==============================================================================}
{ The most simple application with RichView Package - "Hello World!"           }
{ There are two components on the form:                                        }
{ RichView1: TRichView; - component for displaying text                        }
{ RVStyle1: TRVStyle;   - components for customizing appearance of RichView;   }
{ RichView1.Style is set to RVStyle1;                                          }
{ See more comments in TForm1.FormCreate                                       }
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
  // This line adds one line of text in RichView.
  RichView1.AddNL('Hello World!', 0, 0);
  // But text will not displayed yet. You need to call Format method after adding
  // all contents to RichView:
  RichView1.Format;

  // More about AddNL method:
    // First parameter of method ('Hello World!') is a text to display
    // Second parameter defines text attributes of added text;
    //  It is index in collection of text styles (RVStyle1.TextStyles)
    //  You can customize collection of styles using Object Inspector
    //  (for Delphi 3+, CB 3+)
    // Third parameter defined paragraph attributes of added text;
    //  It is index in collection of paragraph styles (RVStyle1.ParaStyles)
  // AddNL is one of methods for adding contents to RichView (AddXXX methods)
  // See "Building RichView Document" topic of help file.
  // These methods appends items to RichView. But component is not prepared for
  // displaying data until Format method is called.
end;

end.
