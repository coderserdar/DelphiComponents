unit Unit1;

//------------------------------------------------------------------------------
// API_STORAGEFILE EXAMPLE
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_StorageFile, StdCtrls, API_listbox, ExtCtrls, API_grbutton,
  API_edit, API_label, API_files, API_base, ComCtrls;

type
  TForm1 = class(TForm)
    API_StorageFile1: TAPI_StorageFile;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_listbox1: TAPI_listbox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    API_listbox2: TAPI_listbox;
    API_label1: TAPI_label;
    API_label2: TAPI_label;
    API_grbutton4: TAPI_grbutton;
    API_files1: TAPI_files;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_StorageFile1AddFiles(Sender: TComponent;
      const AFilename: String; const AIndex, ATotalAmount: Integer);
    procedure API_StorageFile1FileProgress(Sender: TComponent;
      const AFilename: String; const AFileSize, APosition: Int64);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton1Click(Sender: TObject);
var
  i: integer;
begin
  opendialog1.filename:= '';
  opendialog1.Filter:= 'All Files (*.*)|*.*';
  opendialog1.InitialDir:= extractfiledir(application.exename);
  if opendialog1.Execute then
    for i:=0 to opendialog1.Files.Count-1 do
      api_listbox1.Items.add(opendialog1.Files[i]);
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton2Click(Sender: TObject);
var
  sl: tstringlist;
begin
  if opendialog1.execute then
  begin
    // set file name to component
    api_storagefile1.Filename:= opendialog1.FileName;
    // list files inside the storage file
    sl:= tstringlist.create;
    try
      api_storagefile1.FileList(sl);
      api_listbox2.Items.text:= sl.text;
    finally
      sl.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton3Click(Sender: TObject);
var
  sl: tstringlist;
begin
  savedialog1.filename:= '';
  savedialog1.InitialDir:= extractfiledir(application.exename);
  savedialog1.Filter:= 'Storage Files (*.sfi)|*.sfi';
  savedialog1.DefaultExt:= '.sfi';
  if savedialog1.Execute then
  begin
    api_storagefile1.Filename:= savedialog1.FileName;
    sl:= tstringlist.create;
    try
      sl.clear;
      sl.AddStrings(api_listbox1.items);
      if api_storagefile1.AddFiles(sl) then
        messagedlg('Storage file created succesfully.', mtinformation, [mbok], 0)
        else messagedlg('Failed to create storage file.', mterror, [mbok], 0);
      progressbar1.Position:= 0;
    finally
      sl.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton4Click(Sender: TObject);
var
  filename, folder: string;
  i: integer;
  count, total: integer;
begin
  if api_listbox2.itemindex>-1 then
  begin
    // request folder to extract selected files to
    folder:= api_files1.BrowseFolderDialog('Select folder..',extractfiledir(application.exename),false);

    // go trough all selected files
    count:= 0;
    total:= 0;
    for i:=0 to api_listbox2.items.count-1 do
      if api_listbox2.Selected[i] then
      begin
        total:= total + 1;
        filename:= api_listbox2.items[i];
        if api_storagefile1.ExtractFile(filename, folder) then
          count:= count + 1;
      end;

    progressbar2.position:= 0; // make sure this looks nice =)

    // show result
    if (count = total) then
      messagedlg('File(s) extracted succesfully.', mtinformation, [mbok], 0)
      else messagedlg(inttostr(count)+' of '+inttostr(total)+' files extracted', mtwarning, [mbok], 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_StorageFile1AddFiles(Sender: TComponent;
  const AFilename: String; const AIndex, ATotalAmount: Integer);
begin
  progressbar1.Position:= round((Aindex/AtotalAmount)*100);
  application.ProcessMessages;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_StorageFile1FileProgress(Sender: TComponent;
  const AFilename: String; const AFileSize, APosition: Int64);
begin
  progressbar2.Position:= round(Aposition/afilesize*100);
  application.ProcessMessages;  
end;

end.
