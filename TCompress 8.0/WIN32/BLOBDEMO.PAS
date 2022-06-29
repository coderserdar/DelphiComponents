(* BLOBDEMO.PAS for TCompress V3.0 (only comments changed from V2.5)

   This unit provides example code for two things:
   1. On-the-fly creation of compressed blob fields
   2. Writing and reading of any kind of data (e.g. arrays, AVI, WAV etc)
      to/from a compressed blob field.

   Before running the program, you need to use the Database Desktop to create
   a table called BLOB.DB in the DBDEMOS alias which contains a 'Name' field (A10)
   and a 'Data' field (B0). For a full test, you'd also want to add routines
   to put meaningful data into the OurData array, and display it
   before/after blob reads.  We're just compressing a bunch of zeros...

*)
unit Blobdemo;

interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Grids, DBGrids, ExtCtrls, DBCtrls, DB, DBTables,
  Compress, CompCtrl;       { <--- CompCtrl for the TCBlobField/TCBlobstream types }


type
  TForm1 = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    SaveArray: TButton;
    LoadArray: TButton;
    Compress1: TCompress;
    Table1Name: TStringField;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveArrayClick(Sender: TObject);
    procedure LoadArrayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  TestCount: Smallint; { just for making field names }

implementation

{$R *.DFM}

var  BlobDataField:TCBlobField;  { compressed field we'll set up at runtime }
     OurData: Array[1..8000] of byte; { Data to go to/from the field }

const SizeOfOurData = 8000;

{ Here, we create our compressed field BEFORE opening the table. Note that
  the field ('Data' in this case) should *not* appear in the list of fields
  you see when you double-click on the Grid or on Table1. If it is, remove it.
  If the list is blank, add whatever you need, but NOT 'Data'.            }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BlobDataField := TCBlobField.Create(Self);
  BlobDataField.CompressSource := Compress1; { Our TCompress component     }
  BlobDataField.CompressionMethod := coRLE; { RLE compression, for example }
  BlobDataField.FieldName:='Data'; { or whatever you call it in the table }
  BlobDataField.Size := 0;         { The # of bytes stored WITHIN the table }
  BlobDataField.DataSet := Table1;
  try
    Table1.Open;  { Note: In Delphi 2.0+, we've edited our grid (double-click) to ensure
                  that the new field will NOT be added to the grid. This is
                  because the V2.0 grids try to detail with (and display) it as
                  a TBlobfield instead of a TCBlobfield, which causes spurious
                   "Blob not open" errors. Please adopt the same tactic if you
                   are working with custom blob fields like this in Delphi 2.
                 }
  except

(* This code here would be FINE except that for a Delphi VCL/BDE bug,
   -- telling it to create a table with a ftBlob field size zero
   (something perfectly possible in Database Desktop) results
   in a table with a ftBlob ('B') field size 1. This is a baaad
   thing and doesn't work for our purposes. So alas, we have to ask you
   to create the demo table manually. Sorry about that. It's a pain.

UPDATE April 97: This commented-out code should now work for Delphi 1, but
only by exploiting another bug which arises from setting TableType to ttDefault
instead of ttParadox. However, the GOOD news is that there is a VCL patch
available to remove the original bug -- for all Delphi versions. If you want
to know how to create a table with a 0-length Blob field in it, see
http://www.spis.co.nz/blobfix.htm

    with Table1 do
    begin
      DatabaseName := 'DBDEMOS';
      TableName := 'BLOB.DB';
      TableType := ttDefault; { if ttParadox, Delphi 1 will misbehave again }
      with FieldDefs do
      begin
        Clear;
        Add('Name', ftString, 10,True);
        Add('Data', ftBlob, 0,False);  { bzzzt -- wrong -- makes size 1, dammit! }
      end;
      IndexDefs.Clear;
      CreateTable;
      FieldDefs.Clear;
      temptable.free;
      Table1.Open; { open should work this time... but won't, due to above "spec" }
    end;
*)      { so instead, we: }
     showMessage('Please create a table called BLOB.DB in the'+#13+
                 'DBDEMOS alias, according to the specs in BLOBDEMO.PAS.'+#13+#13+
                 'THEN run this program again.');

  end;
end;



{ After all that, here's the enjoyable bits.... }

{ How to write/compress our array data to the blob }
procedure TForm1.SaveArrayClick(Sender: TObject);
var cbs: TCBlobstream;
begin
     { this is cosmetic stuff }
  Inc(TestCount);
  Table1.append; { put a new record in, what the heck (or you could just Edit) }
  Table1.FieldByName('Name').asstring := 'Test '+IntToStr(TestCount);

    { this is the IMPORTANT bit }
  cbs:= TCBlobstream.create(BlobDataField,bmWrite); { will save our data to it }
  cbs.writeBuffer(OurData,SizeOfOurData);         { standard stream method   }
  showmessage('Compressed and wrote '+IntToStr(cbs.size)+' bytes');
  cbs.free;    { done! (compression occurs just here...) }
  Table1.Post;
end;
{ Note: If the data was coming from a file or another object, you could use the
        CopyFrom method from the applicable filestream/memorystream/blobstream }


{ How to read/expand our data (of whatever nature) back into our array }
procedure TForm1.LoadArrayClick(Sender: TObject);
var cbs: TCBlobstream;
    bs: TBlobStream;
begin
  cbs:= TCBlobstream.create(BlobDataField,bmRead); { will read our data from it }
  cbs.readBuffer(OurData,sizeof(OurData));         { standard stream method   }
  showmessage('Expanded and read '+IntToStr(cbs.size)+' bytes');
  cbs.free;

  { Oh, and by the way... }
  bs := TBlobStream.Create(BlobDataField,bmread); { a handle on our RAW (compressed) data }
  showmessage('By the way, that was stored in only '+IntToStr(bs.size)+' bytes');
  bs.free;
end;

{ Cleanup code }
procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Table1.Active then Table1.close;
  BlobDataField.free;
end;

end.
