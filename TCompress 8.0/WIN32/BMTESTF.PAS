unit bmtestf;
(*
  Compressed-Bitmap-As-Resources example for TCompress 3.0 -- no change from v2.5
  This provides instructions and working code examples for:
    a) Creating compressed bitmaps
    b) Turning them into compressed resources
    c) Loading/decompressing resources using LoadExpandedResource example program for tCompres
    d) Loading bitmaps from the decompressed resource

  Note:
  Although this example uses bitmaps, you can compress and
  store/load ANY kind of resource. In fact, an entire compressed archive
  (as created by COMPDEMO) can be stored as a resource -- as is done in
  the self-extracting EXE examples, SELFEXTR.DPR and SELFXSML.DPR

  INSTRUCTIONS:
  =============
  Part A: Making a compressed bitmap file using the CompressABitmap method:
  1. Choose a bitmap you like (e.g. FIREFISH.BMP) and put it in this directory
  2. Alter the MyBitmapName constant to name it properly (note: NO extension)
  3. Compile and run this program and push the Create button to make a compressed
     copy of the bitmap file (FIREFISH.ARC)

  Part B: Turning the compressed bitmap into a resource, and loading it into the EXE:
  4. Create a text file called BITMAPS.RC file containing the following (where
     any reference to FireFish is replaced with the correct name of your bitmap):
/* Compile this file with (16-bit): bin\brc -r BITMAPS.RC
   or                     (32-bit): bin\brc32 -r BITMAPS.RC
   That will create a BITMAPS.RES file which should be included in the main
   unit of your project.
*/
Firefish TCOMPRESS "Firefish.arc"
   (you can have as many lines as you have compressed bitmaps)

  5. Compile the resource file per the instructions in its header.
     Then remove the "-" which is currently in the {$-R BITMAPS.RES} line
     at the start of the implementation section of this file. This line
     loads your new resource into the EXE when it links the program.

  Part C: Loading and decompressing a named bitmap with LoadABitmap:
  6. Build and run the program (the compressed resource should now be in it)
  7. Push the Load button to load from the resource into the TImage component
     which is on the form.
  8. Congratulations -- your bitmap should now be displayed!

*)
interface

uses
  Wintypes, Winprocs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Compress, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Compress1: TCompress;
    Memo1: TMemo;
    MyBitmap: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CompressABitmap(infilename, outfilename: string);
    procedure LoadABitmap(bm: TBitmap;Resname: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const MyBitmapName = 'FireFish';

implementation

{$R *.DFM}
{-$R BITMAPS.RES} { Remove the "-" before recompiling and pushing the Load button }

{ outfilename is assumed to be infilename but with an ARC extension,
  but doesn't HAVE to be }
procedure TForm1.CompressABitmap(infilename, outfilename: string);
var infile, outfile: TFileStream;
begin
  infile := TFileStream.create(infilename,fmOpenRead);
  outfile := TFileStream.create(outfilename,fmCreate);
  try
     Compress1.Compress(outfile,infile,coLZH);
  finally
    infile.free;
    outfile.free;
  end;
end;

{ Loads a bitmap from the stream created by TCompress'
  LoadExpandedResource function }
procedure TForm1.LoadABitmap(bm: TBitmap;Resname: string);
var ResourceStream: TStream;
begin
   ResourceStream := Compress1.LoadExpandedResource(Resname,'');
   try
     bm.LoadFromStream(ResourceStream);
   finally
     ResourceStream.free;  { MUST make sure it gets freed }
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   CompressABitmap(MyBitmap.text+'.bmp',MyBitmap.Text+'.arc');
   Showmessage('Created '+MyBItmap.Text+'.arc'#13#10+
      'Please continue from Step #4 of the instructions...');
   Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     Memo1.Visible := False;
     LoadABitmap(Image1.Picture.Bitmap,Mybitmap.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     MyBitmap.Text := MyBitMapName;
end;

end.
