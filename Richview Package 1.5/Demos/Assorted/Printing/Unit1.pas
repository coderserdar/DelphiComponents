unit Unit1;
{$I RV_Defs.inc}
{==============================================================================}
{ RichView Printing Demo.                                                      }
{------------------------------------------------------------------------------}
{ Note:                                                                        }
{ This demo does not show how to create user interface to setup margins.       }
{ Windows has special dialog for this task, but it is unsupported by Delphi    }
{ (may be it will be in Delphi 6?)                                             }
{ http://www.delphifreestuff.com has free component to show standard           }
{ Page Setup dialog.                                                           }
{ Generally, you need to execute such dialog, and assign RVPrint1.XXXMarginMM  }
{ properties.                                                                  }
{------------------------------------------------------------------------------}
{ Note:                                                                        }
{ This demo does not show how to implement custom scaling of print preview.    }
{ Look at example of  RichViewEdit based editor.                               }
{==============================================================================}

interface

{$I RV_Defs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, ExtCtrls, ComCtrls, PtblRV, CRVPP, RVPP, RVStyle,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  StdCtrls, CtrlImg;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    RichView1: TRichView;
    RVPrintPreview1: TRVPrintPreview;
    RVPrint1: TRVPrint;
    Image1: TImage;
    Panel2: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    RVStyle1: TRVStyle;
    Label1: TLabel;
    PrintDialog1: TPrintDialog;
    btnPrint: TButton;
    Image2: TImage;
    StatusBar1: TStatusBar;
    Panel3: TPanel;
    ScrollBar1: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure RVPrint1PrintComponent(Sender: TRVPrint; PrintMe: TControl;
      var ComponentImage: TBitmap);
    procedure btnPrintClick(Sender: TObject);
    procedure RVPrint1Formatting(Sender: TRichView; PageCompleted: Integer;
      Step: TRVPrintingStep);
    procedure RVPrint1SendingToPrinter(Sender: TRichView;
      PageCompleted: Integer; Step: TRVPrintingStep);
    procedure RVPrint1PagePrepaint(Sender: TRVPrint; PageNo: Integer;
      Canvas: TCanvas; Preview: Boolean; PageRect, PrintAreaRect: TRect);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
  private
    { Private declarations }
    PreviewCreated: Boolean;
    procedure UpdatePreview;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
const longtext:String = 'A simple way of describing Delphi is a sophisticated Pascal compiler.'+
               ' Delphi’s roots lie in Borland’s Turbo Pascal, introduced in the mid-1980s.'+
               ' This view of Delphi, however, doesn’t capture the real power of Delphi.'+
               ' Object Pascal, the object-oriented extensions to Pascal, is the underlying'+
               ' language of Delphi. The Visual Component Library, or VCL, is a hierarchy of '+
               'Object Pascal objects that allow you to design programs. A better way of describing '+
               'Delphi is an Object Pascal-based visual development environment.'#13+

               'The VCL is intimately tied to the Delphi IDE, and is what gives you the ability '+
               'to quickly develop applications. The Component palette and Object Inspector allow '+
               'you to drop VCL components on forms and then manipulate the properties and events of '+
               'those controls without having to write a single line of code.'#13+

               'Despite its name, the VCL is not entirely made up of visual components. '+
               'In fact, of the over 600 objects in the VCL, most are not visual. '+
               'The Delphi IDE allows you to visually add some nonvisual components to '+
               'your programs. For example, if you wanted to write a database application '+
               'that connected to a table, you would drop a TDataSource component on your '+
               'form. TDataSource is a nonvisual component, but is represented on the form by '+
               'an icon (which doesn’t show up at runtime), and you can manipulate the properties '+
               'and events of TDataSource in the Object Inspector just as you would a visual control.'#13+

               'All VCL objects, and in fact all objects in Object Pascal, are derived from TObject. '+
               'TObject is unique in that it is an abstract object that has no properties or events, '+
               'only methods that allow you to derive objects from this base class. Use TObject as the '+
               'immediate base class when writing simple objects that are not components. Components are '+
               'objects that you can manipulate at design time. All components in the VCL are derived '+
               'from the abstract component type TComponent. The VCL components you will likely use the '+
               'most are the VCL’s controls, such as TForm or TSpeedButton. Controls are visual components'+
               ' derived from the abstract component type TControl.'#13+

               'You can use Delphi to create Object Pascal objects without using the VCL, '+
               'although by creating any objects in Object Pascal, both your objects and VCL '+
               'objects will share a common ancestor in TObject. However, by deriving new objects '+
               'from VCL object, much of the work in designing applications is done for you by Delphi. '+
               'For example, if you wanted to use a progress bar in your application but didn’t like '+
               'TProgressBar, Delphi’s control that creates a progress bar, you could create a new '+
               'object based on TProgressBar and override its properties, events, or methods.';
var gr: TGraphic;
begin

  // Creating sample document
  RichView1.AddNL('Printing Demo',1,2);
  RichView1.AddTextNL(longtext,0,0,0);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,1,1);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,2,2);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,3,3);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,4,4);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,5,5);
  RichView1.AddBreak;
  RichView1.AddTextNL(longtext,0,7,7);

  RichView1.AddControlEx('', Panel2, 2, rvvaBaseline);

  gr := TIcon.Create;
  gr.Assign(Image1.Picture);
  RichView1.AddPictureEx( '', gr, -1,  rvvaBaseLine);

  gr := TMetafile.Create;
  gr.Assign(Image2.Picture);
  RichView1.AddPictureEx( '', gr, -1,  rvvaBaseLine);

  // Created...
  RichView1.Format;

  // Assigning margins: 20 mm.
  RVPrint1.LeftMarginMM   := 20;
  RVPrint1.RightMarginMM  := 20;
  RVPrint1.BottomMarginMM := 20;
  // Top margin: 25 mm.
  RVPrint1.TopMarginMM    := 25;

  // Making printable area on preview visible...
  RVPrintPreview1.MarginsPen.Style := psDot;
end;
{------------------------------------------------------------------------------}
{ Switching page to "Preview"                                                  }
{------------------------------------------------------------------------------}
procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if (PageControl1.ActivePage.PageIndex=1) and not PreviewCreated then begin
    PreviewCreated := True;
    UpdatePreview;
  end;
  RVPrintPreview1.ZoomMode := rvzmFullPage;
end;
{------------------------------------------------------------------------------}
procedure TForm1.UpdatePreview;
begin
  Screen.Cursor := crHourglass;
  // Assigning document for printing:
  RVPrint1.AssignSource(RichView1);
  // Formatting pages:
  RVPrint1.FormatPages(rvdoAll);
  // Updating user interface for preview:
  Scrollbar1.Min := 1;
  Scrollbar1.Position := 1;
  {$IFDEF RICHVIEWDEF4}
  Scrollbar1.PageSize := 1;
  {$ENDIF}
  Scrollbar1.Max := RVPrint1.PagesCount;
  // Preview will show full page:
  RVPrintPreview1.ZoomMode := rvzmFullPage;
  // Preview will show 1st page:
  RVPrintPreview1.First;
  Screen.Cursor := crDefault;
end;
{------------------------------------------------------------------------------}
{ Page turning:                                                                }
{------------------------------------------------------------------------------}
procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  if Scrollbar1.Position>0 then begin
    RVPrintPreview1.PageNo := Scrollbar1.Position;
    StatusBar1.SimpleText := Format('Page %d of %d',
      [RVPrintPreview1.PageNo, RVPrint1.PagesCount]);
  end;
end;
{------------------------------------------------------------------------------}
{ Event: printing inserted components.                                         }
{ We need to create bitmap, draw component onto it,                            }
{ and assign this bitmap to ComponentImage parameter.                          }
{ Bitmap should have the same size as component.                               }
{ CtrlImg.pas from RichView package has useful function DrawControl.           }
{------------------------------------------------------------------------------}
procedure TForm1.RVPrint1PrintComponent(Sender: TRVPrint;
  PrintMe: TControl; var ComponentImage: TBitmap);
begin
  ComponentImage := DrawControl(PrintMe);
end;
{------------------------------------------------------------------------------}
{ Printing...                                                                  }
{------------------------------------------------------------------------------}
procedure TForm1.btnPrintClick(Sender: TObject);
begin
  if not PreviewCreated then begin
    PreviewCreated := True;
    UpdatePreview;
  end;
  // do not print empty document!
  if RichView1.ItemCount=0 then
    exit;
  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := RVPrint1.PagesCount;
  PrintDialog1.FromPage := 1;
  PrintDialog1.ToPage := RVPrint1.PagesCount;
  // we can pring whole document or specified pages:
  if PrintDialog1.Execute then begin
    // it's possible that current printer was changed.
    // so we need to reformat document and update preview:
    UpdatePreview;
    case PrintDialog1.PrintRange of
      prAllPages:
        RVPrint1.Print( 'Test', PrintDialog1.Copies, PrintDialog1.Collate);
      prPageNums:
        RVPrint1.PrintPages(PrintDialog1.FromPage, PrintDialog1.ToPage,
              'Test', PrintDialog1.Copies, PrintDialog1.Collate);
    end;
  end;
end;
{------------------------------------------------------------------------------}
{ Event: displaying formatting progress...                                     }
{------------------------------------------------------------------------------}
procedure TForm1.RVPrint1Formatting(Sender: TRichView;
  PageCompleted: Integer; Step: TRVPrintingStep);
begin
  case Step of
    rvpsStarting:
      StatusBar1.SimpleText := 'Repaginating...';
    rvpsProceeding:
      StatusBar1.SimpleText := Format('Repaginating (%d)',[PageCompleted]);
    rvpsFinished:
      StatusBar1.SimpleText := '';
  end;
end;
{------------------------------------------------------------------------------}
{ Event: displaying printing (spooling) progress...                                     }
{------------------------------------------------------------------------------}
procedure TForm1.RVPrint1SendingToPrinter(Sender: TRichView;
  PageCompleted: Integer; Step: TRVPrintingStep);
begin
  case Step of
    rvpsStarting:
      StatusBar1.SimpleText := 'Starting...';
    rvpsProceeding:
      StatusBar1.SimpleText := Format('Printing (%d)',[PageCompleted]);
    rvpsFinished:
      StatusBar1.SimpleText := '';
  end;
end;
{------------------------------------------------------------------------------}
{ (NEW) Event: prepaint on page                                                }
{------------------------------------------------------------------------------}
procedure TForm1.RVPrint1PagePrepaint(Sender: TRVPrint; PageNo: Integer;
  Canvas: TCanvas; Preview: Boolean; PageRect, PrintAreaRect: TRect);
var w,h: Integer;
    s: String;
begin
  // This is a temporary solution for drawing page numbers and similalar stuff

  // This example output string just above RichView contents

  s := Format ('-- Page %d of %d --', [PageNo, Sender.PagesCount]);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(RVStyle1.TextStyles[0]);
  w := Canvas.TextWidth(s);
  h := Canvas.TextHeight(s);
  TextOut(Canvas.Handle, (PrintAreaRect.Right+PrintAreaRect.Left-w) div 2,
                  PrintAreaRect.Top - h - 10, PChar(s), Length(s));
end;

procedure TForm1.Panel3Resize(Sender: TObject);
begin
  // In earlier versions of Delphi scrollbars do not have Align property...
  // Aligning to the right side of panel
  ScrollBar1.SetBounds(Panel3.ClientWidth-ScrollBar1.Width, 0,
                       ScrollBar1.Width, Panel3.ClientHeight);
end;

end.
