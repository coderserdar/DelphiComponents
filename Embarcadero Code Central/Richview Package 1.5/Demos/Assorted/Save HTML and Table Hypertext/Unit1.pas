unit Unit1;

interface
{==============================================================================}
{ Demo:                                                                        }
{ 1. how to load RVF file saved in demo editor.                                }
{ 2. HTML export with custom saving of images                                  }
{    (uses free Anders Melander's TGifImage,                                   }
{     http://www.melander.dk/delphi/gifimage/,                                 }
{     but can be modified to work with other GIF implementations               }
{     supporting assignment from other graphic formats)                        }
{ Sergey Tkachenko                                                             }
{------------------------------------------------------------------------------}
{ Providing pictures and controls on request from RichView is not supported in }
{ this demo.                                                                   }
{==============================================================================}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, StdCtrls, ExtCtrls, RVStyle, OleCtnrs, ImgList,
  ComCtrls, CRVData, CRVFData, RVTable, GifImage;

type
  TForm1 = class(TForm)
    RichView1: TRichView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    Button2: TButton;
    CheckBox1: TCheckBox;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    RVStyle1: TRVStyle;
    ImageList1: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure RichView1RVFImageListNeeded(Sender: TRichView;
      ImageListTag: Integer; var il: TImageList);
    procedure FormCreate(Sender: TObject);
    procedure RichView1RVMouseMove(Sender: TObject; id: Integer);
    procedure RichView1Jump(Sender: TObject; id: Integer);
    procedure RichView1HTMLSaveImage(Sender: TRichView;
      RVData: TCustomRVData; ItemNo: Integer; const Path: String;
      BackgroundColor: TColor; var Location: String;
      var DoDefault: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure RichView1URLNeeded(Sender: TRichView; id: Integer;
      var url: String);
    procedure RichView1SaveComponentToFile(Sender: TRichView; Path: string;
      SaveMe: TPersistent; SaveFormat: TRVSaveFormat; var OutStr: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{==================== Notes about loading from RVF files:=======================
1. In simplest cases you can just write: RichView1.LoadRVF(<file name>);
2. If file contains inserted Delphi controls, these controls must be registered
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
===============================================================================}
{===================== Notes about HTML export =================================
1. There are 2 methods for saving HTML files:
   a) SaveHTML - saving HTML file, where formatting is made by <FONT>,<B>,
     <I> tags, etc.
   b) SaveHTMLEx - saving HTML file, where formatting is made by Cascading
     Style Sheet
2. Images are saved in separate files in JPG-files (or in BMP-files for Delphi2,
   or if RVDONOTUSEJPEGIMAGE directive is defined)
3. By default, images are saved in the same directory as HTML file, and have
   names built as Prefix + Number + .JPG.
   You can specify your own prefix as parameter of SaveHTML[Ex].
   You can include subdirectory in prefix (such as 'images\img'), but this
   subdirectory will NOT be created automatically.
4. JPEG/BMP do not support transparency. Transparent color (of metafiles,
   icons, imagelist images) is replaced with current background color
   (of RichView or table cell or paragraph background)
5. By default, images from imagelists (bullets and hotspot) are saved like
   other images, but the same image saved only one time (next occurrences
   point to the same image file, if they have the same background color)
6. You can save images yourself using OnHTMLSaveImage event (new in v1.4).
   You need to store image to file and return its location in 'Location'
   parameter.
   This demo shows
   a) how to save images in GIF-files
   b) how to save bullets in a way allowing to use the same image files for
      all HTML document generated by your application.
7. By default hypertext is not saved.
   You can specify destinations of [some/all] hypertext jumps
   using OnURLNeeded event.
8. By default inserted controls are not saved.
   You can save them using OnSaveComponentToFile event
===============================================================================}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterClasses([TButton, TEdit, TOleContainer]);
end;

{============================== RVF loading ===================================}
procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    if not RichView1.LoadRVF(OpenDialog1.FileName) then
      Application.MessageBox('Error Loading File', nil, MB_OK);
    RichView1.Format;
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.RichView1RVFImageListNeeded(Sender: TRichView;
  ImageListTag: Integer; var il: TImageList);
begin
  il := ImageList1;
end;
{============================ Hypertext testing ===============================}
procedure TForm1.RichView1RVMouseMove(Sender: TObject; id: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo: Integer;
begin
  if id=-1 then
    StatusBar1.SimpleText := ''
  else begin
    RichView1.GetJumpPointLocation(id, RVData, ItemNo);
    StatusBar1.SimpleText := Format('Mouse is over jump #%d      (the %d-th item in %s object)',[id, ItemNo,RVData.ClassName]);
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo: Integer;
begin
  RichView1.GetJumpPointLocation(id, RVData, ItemNo);
  StatusBar1.SimpleText := Format('CLICK: jump #%d      (the %d-th item in %s object)',[id, ItemNo,RVData.ClassName]);
  Application.MessageBox(PChar(StatusBar1.SimpleText),'Click', 0);
end;
{============================ SAVING TO HTML ==================================}
procedure TForm1.Button2Click(Sender: TObject);
var SaveOptions: TRVSaveOptions;
var r: Boolean;
begin
  if SaveDialog1.Execute then begin
    Screen.Cursor := crHourglass;
    if Checkbox1.Checked then
      SaveOptions := [rvsoOverrideImages]
    else
      SaveOptions := [];
    case SaveDialog1.FilterIndex of
      1:
        r := RichView1.SaveHTML(SaveDialog1.FileName,'Demo File',Edit2.Text, SaveOptions);
      2:
        r := RichView1.SaveHTMLEx(SaveDialog1.FileName,'Demo File',Edit1.Text,
                                      '','','',SaveOptions);
      else
        r := False;
    end;
    Screen.Cursor := crDefault;
    if not r then
      Application.MessageBox('Error during saving', 'Error', 0);
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.RichView1HTMLSaveImage(Sender: TRichView;
  RVData: TCustomRVData; ItemNo: Integer; const Path: String;
  BackgroundColor: TColor; var Location: String; var DoDefault: Boolean);
var gif: TGifImage;
    wmf: TMetafile;
    gr: TGraphic;
    s: String;
    AVAlign: TRVVAlign;
    ATag: Integer;
    ImageList: TImageList;
    ImageIndex: Integer;
    Canvas: TMetafileCanvas;
begin
  // Parameters:
  // Saving item is defined by pair (RVData, ItemNo).
  // It is the ItemNo-th item in RVData object.
  // RVData may be RichView.RVData, or cell, or RVData of cell inplace editor.
  // Path - destination directory of HTML file.
  // BackgroundColor - color of background under this item. Not used here
  // because GIFs support true transparency. 
  // Location - output parameter to specify filename of image file
  // DoDefault - set to false if you save this item as image yourself.

  case RVData.GetItemStyle(ItemNo) of
    rvsPicture:
      begin
        // Assigning image to GIF and saving
        // (metafiles and icons will be saved with transparency)
        gif := TGifImage.Create;
        try
          RVData.GetPictureInfo(ItemNo, s, gr, AVAlign, ATag);
          gif.Assign(gr);
          Location := RVData.GetNextFileName(Edit2.Text, Path, '.gif', RichView1.imgSaveNo, Checkbox1.Checked);
          gif.SaveToFile(Location);
          DoDefault := False;
        finally
          gif.Free;
        end;
      end;
    rvsBullet, rvsHotspot:
      begin
        // This is not efficient way, because the same image will be
        // saved many times. In your application you can save bullets
        // before saving HTMLs, and here only return file name.
        RVData.GetBulletInfo(ItemNo, s, ImageIndex, ImageList, ATag);
        wmf := TMetafile.Create;
        gif := TGifImage.Create;
        try
          // Drawing image from imagelist to metafile
          // This method allows to save transparency
          wmf.Width := ImageList.Width;
          wmf.Height := ImageList.Height;
          Canvas := TMetafileCanvas.Create(wmf, 0);
          ImageList.Draw(Canvas,0,0, ImageIndex);
          Canvas.Free;
          // Assigning metafile to GIF and saving
          gif.Assign(wmf);
          // Saving to Path + Bullets Prefix + ImageIndex + .gif
          Location := Format('%s%s%d.gif', [Path, Edit1.Text, ImageIndex]);
          gif.SaveToFile(Location);
          DoDefault := False;
        finally
          wmf.Free;
          gif.Free;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TForm1.RichView1URLNeeded(Sender: TRichView; id: Integer;
  var url: String);
begin
  // Just for demo
  url := '#'+IntToStr(id);
end;
{------------------------------------------------------------------------------}
procedure TForm1.RichView1SaveComponentToFile(Sender: TRichView;
  Path: string; SaveMe: TPersistent; SaveFormat: TRVSaveFormat;
  var OutStr: string);
begin
  case SaveFormat of
   rvsfHTML:
       begin
         if SaveMe is TButton then begin
           OutStr := '<INPUT type="button" value="'+TButton(SaveMe).Caption+'" '+
                     'onClick="alert(''Just a demo'')">';
           exit;
         end;
         if SaveMe is TEdit then begin
           OutStr := '<INPUT type="text" value="'+TEdit(SaveMe).Text+'">';
           exit;
         end;
       end;
   end;
end;

end.
