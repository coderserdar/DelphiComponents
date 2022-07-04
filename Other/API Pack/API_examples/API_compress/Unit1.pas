unit Unit1;

//------------------------------------------------------------------------------
// API_Compress example
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_compress, API_base;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    tAPI_compress1: tAPI_compress;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit3: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  api_files;

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
begin
  opendialog1.Filter:='All files (*.*)|*.*';
  if opendialog1.execute then                     // if file was selected
  begin
    edit1.text:= opendialog1.FileName;            // show filename on edit
    edit2.text:= edit1.Text + '.compressed';      // add extension
  end;
end;

//------------------------------------------------------------------------------
// example to compress file stream to another
procedure TForm1.Button2Click(Sender: TObject);
var
  fs_in: tfilestream;     // source file
  fs_out: tfilestream;    // target
begin
  fs_in:= tfilestream.create(edit1.text, fmopenread);                       // open source file
  try
    label3.Caption:= 'Input Size = '+api_files.bytestostr(fs_in.size);      // show input file size
    fs_out:= tfilestream.create(edit2.Text, fmcreate);                      // open output file
    try
      fs_out.Seek(int64(0), soFromBeginning);                               // seek to beginning
      api_compress.CompressStream(fs_in, fs_out);                           // compress to target
      label4.Caption:= 'Output Size = '+api_files.BytesToStr(fs_out.size);  // show output size
      label5.caption:= 'Compression Ratio = '+                              // show result..
        formatfloat('0.0',(fs_in.size-fs_out.size)/fs_in.size*100)+'%';     // as percentages
    finally
      fs_out.free;                                                          // free ioutput stream
    end;
  finally
    fs_in.free;                                                             // free input stream
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button5Click(Sender: TObject);
var
  fs_in: tfilestream;     // compressed file
  fs_out: tfilestream;    // target
begin
  (*
      this es exactly as above, but instead of
      api_compress.compressstream
      you must use --- for practice, you could try
      to finsih this function :)
  *)
  api_compress.DecompressStream(fs_in, fs_out);
end;

//------------------------------------------------------------------------------
procedure TForm1.Button3Click(Sender: TObject);
var
  sl: tstringlist;
  ms_in, ms_out: tmemorystream;
  outstring: string;
begin
  sl:= tstringlist.create;
  try
    sl.Text:= edit3.text;

    // do compression
    ms_in:= tmemorystream.create;             // create source stream
    try
      sl.SaveToStream(ms_in);                 // save stringlist to stream
      ms_out:= tmemorystream.create;          // create output stream
      try
        api_compress.compressstream(ms_in, ms_out);

        // show result
        label7.Caption:= 'Source length = '+inttostr(ms_in.Size);
        label8.caption:= 'Output length = '+inttostr(ms_out.size);

      finally
        ms_out.free;
      end;
    finally
      ms_in.free;
    end;

  finally
    sl.free;
  end;
end;

end.
