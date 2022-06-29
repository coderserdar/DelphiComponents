unit Unit1;

interface
{==============================================================================}
{ Demo: how to load RVF file saved in demo editor.                             }
{ Sergey Tkachenko                                                             }
{------------------------------------------------------------------------------}
{ Providing pictures and controls on request from RichView is not supported in }
{ this demo.                                                                   }                                                 
{==============================================================================}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, StdCtrls, ExtCtrls, RVStyle, OleCtnrs;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    ImageList1: TImageList;
    RVStyle1: TRVStyle;
    procedure Button1Click(Sender: TObject);
    procedure RichView1RVFImageListNeeded(Sender: TRichView;
      ImageListTag: Integer; var il: TImageList);
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

{
Notes about loading from RVF files:
1. In simplest cases you can just write: RichView1.LoadRVF(<file name>);
2. If file contains inserted Delphi Controls, these controls must be registered
   with RegisterClasses functions before loading (see FormCreate below)
3. If file contains images from image lists, you need to process
   OnRVFImageListNeeded event (see RichView1RVFImageListNeeded below)
   If you have several image lists, you can distinguish them using
   ImageListTag parameter of this event.
4. You must have the same (or compatible) TRVStyle object assigned to
   RichView1.Style as in editor.
   Otherwise you can use RVStyle.LoadINI and SaveINI methods for loading
   and saving styles.
5. If some items in RVF file have character strings associated as items' tags
   (rvoTagsArePChars was in editor's Options), you need also set rvoTagsArePChars
   in RichView1.Options.
}


procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterClasses([TButton, TEdit, TOleContainer]);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    if not RichView1.LoadRVF(OpenDialog1.FileName) then
      Application.MessageBox('Error Loading File', nil, MB_OK);
    RichView1.Format;
  end;
end;

procedure TForm1.RichView1RVFImageListNeeded(Sender: TRichView;
  ImageListTag: Integer; var il: TImageList);
begin
  il := ImageList1;
end;

end.
