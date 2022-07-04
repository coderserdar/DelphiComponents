unit makeexef;

{ MakeEXE -- example self-extracting archive builder for TCompLHA

  Before using this tool, be sure to load and build the SFX.DPR
  project according to the instructions in its comment block.

  To make this application simple to use, we have taken the
  approach of doing the compression here, including everything
  in (and optionally) below a source directory. Another approach
  would be to build the archive first with a visual tool like
  LHADemo, then this utility simply needs to bolt the already-created
  archive onto the end of a copy of the SFX.EXE "header" file.

  Note: before compiling with Delphi 1.0, change the following line in the TForm1 declaration:
    procedure CompLHACheckFile(const originalFilePath: String; var newFilePath: string;
  to:
    procedure CompLHACheckFile(const originalFilePath: String; var newFilePath: OPENstring;
}

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CompLHA;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SourcePath: TEdit;
    Label1: TLabel;
    CompLHA: TCompLHA;
    HeaderFilePath: TEdit;
    Label2: TLabel;
    EXEFilePath: TEdit;
    Label3: TLabel;
    CreateButton: TButton;
    Recurse: TCheckBox;
    Status: TPanel;
    procedure BuildTree(const SourceDir: string;recurse: Boolean;whichfiles: TStrings);
    procedure HeaderFilePathExit(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
{ before compiling with Delphi 1.0, change newFilePath: String;
  to newFilePath: OPENString; in the following line: }
    procedure CompLHACheckFile(const originalFilePath: string;
      var newFilePath: string; dateTime: Longint;
      mode: TCompLHAProcessMode);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ MakeCopyOfHeader
  Copies the file in HeaderFilePath to EXEFilePath, returning a stream pointer
  which is at the end of the EXEFilePath file, nil if no good.
  Deletes EXEFilePath if existing to begin with.
}
function MakeCopyOfHeader(const HeaderFilePath,EXEFilePath: string): TStream;
var headerfs : TFileStream;
begin
   headerfs := TFileStream.create(HeaderFilePath,fmOpenRead or fmShareDenyNone);
   try
     if FileExists(EXEFilePath) then
        DeleteFile(EXEFilePath);
     result :=TFileStream.create(EXEFilePath,fmCreate or fmShareExclusive);
     result.copyfrom(headerfs,headerfs.size);
   finally
     headerfs.free;
   end;
end;

{ BuildTree
  A handy utility which uses the CompLHA GetAllFilesInDir method
  to recursively build a tree of all files in a directory tree
}
procedure TForm1.BuildTree(const SourceDir: string;recurse: Boolean;whichfiles: TStrings);
var thisdir: TStringlist;
    thisfile: string;
    count : Integer;
begin
   thisdir := TStringList.create;
   try
     CompLHA.GetAllFilesInDir(thisdir, SourceDir,'*.*',True); { get EVERYTHING }
     for count := 0 to pred(thisdir.count) do
     begin
       thisfile :=thisdir[count];
       if (FileGetAttr(thisfile) and faDirectory)=0 then
         whichfiles.add(thisfile) { we'll take ALL non-DIR files }
       else if recurse then { go get this subdirectory's files too... }
         BuildTree(thisfile,recurse,whichfiles);
     end;
   finally
     thisdir.free;
   end;
end;

{ Here is where everything comes together.
  Logic:
  Create a SFX file consisting of two sections bolted together:
   a) The SFX.EXE header, which will do the extraction
   b) A compressed archive of all the files we want to deliver
  See SFX.DPR for the extraction part of the process
}
procedure TForm1.CreateButtonClick(Sender: TObject);
var whichFiles: TStringList;
    fs: TStream;
begin
 fs := MakeCopyOfHeader(HeaderFilePath.Text,EXEFilePath.Text); { leaves stream pointer at the end }
 whichFiles := TStringList.create;
 createButton.enabled := False;
 try
  BuildTree(SourcePath.Text,recurse.checked,whichFiles); { get a full list }
  { we now have a list of files which are to be packed }
  with CompLHA do
  begin
    if recurse.checked then { tweak so we store paths BELOW the SourcePath root }
    begin
      StorePath := True;
      TargetPath := SourcePath.Text; { which will be pruned off the front of the stored paths }
    end else
     StorePath := False; { don't bother }
    screen.cursor := crHourGlass;
    CompressFilesToStream(fs,TStrings(whichfiles),coLH5); { compress everything onto the end of the new exe }
  end; { with }
 finally
    whichFiles.free;
    fs.free;
    status.caption := '';
    screen.cursor := crDefault;
    createButton.enabled := True;
 end;
 Showmessage('Completed. Now run '+EXEFilePath.Text+
      ' C:\OURPATH'#13#10' -- the OURPATH parameter indicates a test directory '+
      'into which it should be extracted.');
end;

procedure TForm1.HeaderFilePathExit(Sender: TObject);
begin
   if not FileExists(HeaderFilePath.Text) then
   begin
      showmessage('Incorrect header filename');
      HeaderFilePath.setfocus;
   end;
end;

{ before compiling with Delphi 1.0, change newFilePath: String;
  to newFilePath: OPENString; in the following line: }
procedure TForm1.CompLHACheckFile(const originalFilePath: string;
  var newFilePath: string; dateTime: Longint; mode: TCompLHAProcessMode);
begin
   Status.caption := 'Compressing '+originalFilePath + ' as '+newFilePath;
   application.Processmessages;
end;

end.
