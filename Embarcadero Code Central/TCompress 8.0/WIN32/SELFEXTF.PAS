unit Selfextf;
(*
  Self-Extracting-EXE example program for TCompress 3.0 -- no change from V2.5
  This provides instructions and working code examples for:
    a) Creating a compressed archive as a resource
    b) Loading the compressed resource using LoadCompressedResource
    c) Expanding the compressed archive into a directory with ExpandFilesFromStream

  Notes:
  a) For an example which compresses and loads individual bitmaps, see BMTEST.DPR
  b) For an example with NO forms overhead (but hence no user interface), see SELFXSML.DPR
  c) This code has been tested satisfactorily with Delphi 2.0/3.0 and C++ Builder. With
     Delphi 1.02 we encountered exceptions on ONE of our test systems
     (Windows95 with Service Pack 1 and SP1 patches installed) which we have not
     been able to eliminate after substantial tracing and testing.
     Your mileage may vary.

  INSTRUCTIONS
  ============
  Part A: Creating a compressed archive as a resource:
  1. Use COMPDEMO to make a new compressed archive (e.g. MYARCHIV.ARC) containing
     all the files you want to automatically install (try NOTEPAD.EXE and a
     simple README.TXT for example)
  2. Create a text file called COMP_RES.RC containing the following (where
     any reference to MYARCHIV is replaced with the correct name of your archive):
/* Compile this file with (16-bit): bin\brc -r COMP_RES.RC
   or                     (32-bit): bin\brc32 -r COMP_RES.RC
   That will create a COMP_RES.RES file which should be included in the main
   unit of your project.
*/
MyArchiv TCOMPRESS "MyArchiv.arc"
   (you can have additional lines for other resources if necessary)

  3. Compile the resource file per the instructions in its header.
     Note the {$R COMP_RES.RES} reference to it in this file (just after
     the start of the 'implementation' section.
  4. Alter the PROG_NAME and README_NAME constants below to
     refer to the correct files (they're optional, so blank is ok)

  Part B: Loading the compressed resource using LoadCompressedResource
  5. Refer to the code in DOINSTAL.PAS -- it is called when the INSTALL button
  on this form is pushed. Change the RESOURCE_NAME constant to be the correct
  name of your resource (e.g. MyArchiv).
  6. The resource loading (in compressed form) is accomplished with the line:
        TempStream := LoadCompressedResource(RESOURCE_NAME,'');

  Part C: Expanding the compressed archive into a directory:
  7. Once the resource is correctly loaded, the TCompress TargetPath
     and MakeDirectories properties are set, then the full archive
     expansion is handled by this line:
        ExpandFilesFromStream(TempStream,nil); { get the lot }

  8. Build this program, run it and give it a safe temporary path --
     the files should be expanded to there and, if you've checked the
     boxes, the extracted program should be run and/or the readme file
     viewed with notepad.
*)
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, DoInstal;

type
  TForm1 = class(TForm)
    TargetDirectory: TEdit;
    RunProgram: TCheckBox;
    ViewReadMe: TCheckBox;
    Install: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure TargetDirectoryExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
{$R COMP_RES.RES}


const PROG_NAME =   'notepad.exe';
      README_NAME = 'readme.txt';

procedure TForm1.FormCreate(Sender: TObject);
begin
     TargetDirectory.text := ExtractFilePath(paramstr(0));
end;

procedure TForm1.InstallClick(Sender: TObject);
var ReadMeFile, ExeFile: String;
begin
     ExeFile := '';
     ReadMeFile := '';
     if RunProgram.checked then
        ExeFile := PROG_NAME;
     if ViewReadMe.checked then
        ReadMeFile := README_NAME;
     DoInstall(TargetDirectory.text,ExeFile, ReadMeFile);
     ShowMessage('Installation is complete...');
     close;
end;

procedure TForm1.TargetDirectoryExit(Sender: TObject);
var path: string;
begin
     path :=TargetDirectory.text;
     if path<>'' then
        if copy(path,Length(path),1)<>'\' then
           TargetDirectory.text := path+'\';
end;

end.

