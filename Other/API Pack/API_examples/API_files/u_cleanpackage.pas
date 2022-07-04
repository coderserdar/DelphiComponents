unit u_cleanpackage;

// 19062008, ari pikivirta
//  * updated to show proper use of fundfiles function 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_files, StdCtrls, ExtCtrls, API_base, API_listbox;

const
  STR1 = 'CleanPackage.exe';
  STR2 = 'CreatePackage.exe';

type
  Tf_cleanpackage = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    API_files1: TAPI_files;
    SaveDialog1: TSaveDialog;
    Button4: TButton;
    Label3: TLabel;
    Edit2: TEdit;
    Button5: TButton;
    Button2: TButton;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    break: boolean;
  end;

var
  f_cleanpackage: Tf_cleanpackage;

implementation

{$R *.dfm}

uses
  shellapi, api_strings;

//------------------------------------------------------------------------------
// start search function and assign break variable to it. function will fill up
// the filelist stringlist with the filenames found - in this case it's not
// used to anything because we'll add all found files to the visible list trough
// the event. break function is assigned to be able to terminate search by
// pressing the button on the form.
procedure Tf_cleanpackage.Button1Click(Sender: TObject);
var
  filelist: tstringlist;
  pos: integer;
  sizeondisk: int64;
begin
  // STR1 = 'CleanPackage.exe';
  // STR2 = 'CreatePackage.exe';
  listbox1.Clear;
  sizeondisk:= 0;

  filelist:= tstringlist.create;
  try

    // search files on selected folder, except the executables if checkbox was checked
    if not checkbox1.checked then
    begin
      filelist.clear;
      sizeondisk:= api_files.FindFiles(filelist, edit2.text, '*.exe', '', true, true, true);
      pos:= filelist.IndexOf(addbackslash(edit2.text)+STR1);
      if pos>-1 then filelist.Delete(pos);
      pos:= filelist.IndexOf(addbackslash(edit2.text)+STR2);
      if pos>-1 then filelist.Delete(pos);
      listbox1.items.AddStrings( filelist );
    end;

    // look for backup files
    filelist.clear;
    sizeondisk:= sizeondisk + api_files.findfiles(filelist, edit2.text, '*.~*', '', true, true, true);
    listbox1.items.addstrings( filelist );
    filelist.clear;
    sizeondisk:= sizeondisk + api_files.findfiles(filelist, edit2.text, '*.bak', '', true, true, true);
    listbox1.items.addstrings( filelist );

    // IDE specific project files (not needed)
    if not checkbox2.checked then
    begin
      filelist.clear;
      sizeondisk:= sizeondisk + api_files.findfiles(filelist, edit2.text, '*.dproj*', '', true, true, true);
      listbox1.items.addstrings( filelist );
      filelist.clear;
      sizeondisk:= sizeondisk + api_files.findfiles(filelist, edit2.text, '*.bdsproj*', '', true, true, true);
      listbox1.items.addstrings( filelist );
    end;

  finally
    freeandnil(filelist);
  end;

  // show size on bytes
  label1.caption:= bytestostr(sizeondisk);

  // focus to next button
  button4.SetFocus;
end;

//------------------------------------------------------------------------------
procedure Tf_cleanpackage.Button2Click(Sender: TObject);
begin
  close;
end;

//------------------------------------------------------------------------------
// here we'll just check if the key pressed was enter..
procedure Tf_cleanpackage.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then
    button1click(self);
end;

//------------------------------------------------------------------------------
// by clicking delete button, all files listed to the visible list will be
// tried to be deleted :)
procedure Tf_cleanpackage.Button4Click(Sender: TObject);
var
  i, cnt: integer;
  sl: tstringlist;
begin
  if listbox1.items.count>0 then
  begin
    i:=0;
    cnt:= 0;
    sl:= tstringlist.create;
    try
      sl.clear;
      while i<listbox1.items.count do
      begin
        if bool(windows.DeleteFileA(pansichar(listbox1.items[i]))) then cnt:= cnt + 1
          else sl.add(listbox1.items[i]); // add item that failed
        i:= i + 1;
      end;
      //
      // show result
      if sl.count>0 then
      begin
        messagedlg(inttostr(cnt)+' files deleted.'+#13+
          inttostr(sl.count)+' failed to delete:'+#13+
          sl.text, mtWarning, [mbok], 0);
      end else
        messagedlg(inttostr(cnt)+' files deleted succesfully.', mtinformation, [mbok], 0);
    finally
      freeandnil(sl);
    end;
    button2.SetFocus;
  end else
  begin
    messagedlg('No files found to delete.', mtinformation, [mbok], 0);
    button2.setfocus;
  end;
end;

//------------------------------------------------------------------------------
procedure Tf_cleanpackage.Button5Click(Sender: TObject);
var
  s: string;
begin
  s:= api_files.BrowseFolderDialog('Select folder', edit2.text, true );
  if s<>'' then edit2.text:= s;
end;

//------------------------------------------------------------------------------
procedure Tf_cleanpackage.FormCreate(Sender: TObject);
begin
  edit2.Text:= extractfiledir(application.exename);
end;

//------------------------------------------------------------------------------
procedure Tf_cleanpackage.FormDestroy(Sender: TObject);
var
  tmpS: string;
begin
  tmpS:= addbackslash(extractfiledir(application.exename))+'CreatePackage.exe';
  if fileexists(tmpS) then
    if messagedlg('Do you want to execute CreatePackage.exe now?', mtconfirmation, [mbyes, mbno], 0)=mrYes then
      ShellExecute(Handle, 'open', pchar(tmpS), '', nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------
procedure Tf_cleanpackage.FormActivate(Sender: TObject);
begin
  button1.SetFocus;
end;

end.
