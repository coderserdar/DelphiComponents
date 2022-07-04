unit u_createpackage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_files, StdCtrls, ExtCtrls, API_base, API_listbox, Zip;

const
  DEFAULTARCHIVEEXTENSION = '.zip';

type
  Tf_createpackage = class(TForm)
    API_files1: TAPI_files;
    SaveDialog1: TSaveDialog;
    Button4: TButton;
    Label3: TLabel;
    Edit2: TEdit;
    Button5: TButton;
    Button2: TButton;
    Zip1: TZip;
    Label1: TLabel;
    Edit1: TEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure API_files1FindFileReady(sender: TObject;
      const count: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    break: boolean;
  end;

var
  f_createpackage: Tf_createpackage;

function MakeFilenameFromDirectory(const Dirname, Extension: string): string;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
function MakeFilenameFromDirectory(const Dirname, Extension: string): string;
var
  tmpS: string;
begin
  tmpS:= Dirname;
  if tmpS<>'' then
  begin
    if tmpS[length(tmpS)]='\' then delete(tmpS, length(tmpS), 1);
    tmpS:= tmpS + Extension;
    tmpS:= extractfilename(tmpS);
  end;
  result:= Dirname;
  if result<>'' then
    if result[length(result)]<>'\' then result:= result + '\';
  result:= result + tmpS;
end;

//------------------------------------------------------------------------------
procedure Tf_createpackage.Button2Click(Sender: TObject);
begin
  close;
end;

//------------------------------------------------------------------------------
// by clicking delete button, all files listed to the visible list will be
// tried to be deleted :)
procedure Tf_createpackage.Button4Click(Sender: TObject);
var
  res: integer;
begin
  //zip1.DllPath:= extractfiledir(application.exename)+'\API_misc\tzip\Dlls';   // dll folder

  // delete old zip if exists
  if fileexists(zip1.Filename) then
  begin
    windows.DeleteFileW(pwidechar(zip1.filename));
  end;
  //
  // create new archive
  zip1.Filename:= edit1.text;
  zip1.AddPath:= edit2.text;
  zip1.FileSpecList.Add(ansistring('*.*'));
  zip1.AddOptions:= [aoRecursive, aoFolderEntries];
  //
  // add to archive, and release files
  self.Cursor:= crHourGlass;
  try
    res:= zip1.add;
  finally
    self.Cursor:= crDefault;
    zip1.Filename:= '';
    zip1.FileSpecList.clear;
  end;
  //
  // check result
  if not zip1.Cancelled then
  begin
    button2.SetFocus;
  end else
  begin
    messagedlg('Aborted by user.', mtwarning, [mbok], 0);
    button4.setfocus;
  end;
end;

//------------------------------------------------------------------------------
procedure Tf_createpackage.Button5Click(Sender: TObject);
begin
  if edit2.text='' then edit2.text:= extractfiledir(application.exename);
  edit2.text:= api_files.BrowseFolderDialog('Select folder', edit2.text, true );
  edit1.text:= MakeFilenameFromDirectory(edit2.text, DEFAULTARCHIVEEXTENSION);
end;

//------------------------------------------------------------------------------
procedure Tf_createpackage.API_files1FindFileReady(sender: TObject;
  const count: Integer);
begin
  messagedlg('File search completed.'+#13+'Found '+inttostr(count)+' file(s).', mtinformation, [mbok], 0);
end;

//------------------------------------------------------------------------------
procedure Tf_createpackage.FormCreate(Sender: TObject);
begin
  edit2.Text:= extractfiledir(application.exename);
  edit1.text:= MakeFilenameFromDirectory(edit2.text, DEFAULTARCHIVEEXTENSION);
end;

//------------------------------------------------------------------------------
procedure Tf_createpackage.FormActivate(Sender: TObject);
begin
  button4.SetFocus;
end;

end.
