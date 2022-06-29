unit cakdir2cmd;

interface
uses Windows,Controls,Dialogs,Cakdir2,SysUtils, Classes, {HttpGet,} Forms,QzRegistry,StdCtrls,PBFolderDialog;

var Cakdir2 : TCakdir2;

function Param : Tstrings;
procedure New(name : string);
procedure Open(name : string);
procedure Close;
procedure Test;
procedure Extract(what, too : string);
procedure Add(what : string);
procedure DoAdd;
procedure Del(what : string);  //and Delete
procedure Ren(what,too : string); //and Rename
procedure Convert(what,totype : string);
procedure Password(what : string);
procedure Useextrpath(toggle : boolean);
procedure Useextroverwrite(toggle : boolean);
procedure Useaddpath(toggle : boolean);
procedure UseVerCtrl(toggle : boolean);
procedure Userelativepath(toggle : boolean);
procedure Usesubdir(toggle : boolean);
procedure RunFile(what,param : string);
procedure RunAndwait(ProgramPath, ProgramParam: string);
procedure MoveFile(what, too : string);
procedure RenFile(what, too : string);
procedure DelFile(what : string);
procedure CopyFile(what,too : string);
procedure MakeDir(what : string);
procedure DelDir(what : string);
procedure OpenFolder(what : string);
procedure Sort(index : integer; accending : boolean);

//Read Archive_Contents
function Total_Contents : integer;
function Contents_Filename(which : integer) : String;
function Contents_FileDefPath(which : integer) : String;
function Contents_FileSize(which : integer) : Integer;
function Contents_FileCompSize(which : integer) : Integer;
function Contents_FileSelected(which : integer) : Boolean;
function Contents_FileType(which : integer) : String;
function Contents_FileDateTime(which : integer) : TDatetime;

//Select file manually e.g.
//Selectfile(0);
//Extract('','C:\temp');
procedure Select_File(which : integer);
procedure DeSelect_File(which : integer);
function Mask_Select_File(mask : string) : integer;
procedure Clear_select;

//Locate Paths
function TempPath : String;
function MydocuPath : string;
function DesktopPath : string;
function SystemPath : string;
function GetFileSize(const FileName: String): Integer;
function GetFileDateTime(const FileName: String): TDatetime;

function appendSlash(input : string) : string;
function pollfilelist(maskedname : string;subdir : boolean) : tstrings;

//function download(www,too : string) : boolean;

function New_Form(Left, Top, Width, Height : integer; Caption : string) : TForm;
function New_Label(Parent : TWinControl; Left, Top : integer; Caption : String) : TLabel;
function New_Edit(Parent : TWinControl; Left, Top, Width, Height : integer; Text : String) : TEdit;
function New_Button(Parent : TWinControl; Left, Top, Width, Height : integer; Caption : String) : TButton;
function New_Memo(Parent : TWinControl; Left, Top, Width, Height : integer) : TMemo;
function AskDirDialog(Default : String) : String;
function AskFilenameDialog(Default,filter : String) : String;
procedure Show_Form(aForm : TForm);


implementation
uses CakUtils2, CakDefs2;
//uses Editor;
function Param : Tstrings;
begin
  Result := QzRegistry.ReadList(nil,'Param')
end;

procedure New(name : string);
var l : integer;
    x : string;
begin
  if not fileexists(name) then
    Cakdir2.New(name) else
      begin
        l := 0;
        x := Format('%s%d%s',[removefileext(name), l,extractfileext(name)]);
        While (l <= 99) and fileexists(x) do
                begin
                inc(l);
                x := Format('%s%d%s',[removefileext(name), l,extractfileext(name)]);
                end;
        if not fileexists(x) then
                Cakdir2.New(x) else
                begin
                  Cakdir2.ArchiveName := name;
                end;
      end;
end;
procedure Open(name : string);
begin
  Cakdir2.ArchiveName := name;
end;
procedure Close;
begin
  Cakdir2.Close;
end;
procedure Test;
begin
  Cakdir2.Test;
end;
procedure Extract(what, too : string);
begin
  if what <> '' then
  begin
  Cakdir2.UnSelectALL;
  Cakdir2.Select(what,'');
  end;
  if too <> '' then
  Cakdir2.Extractoptions.extr_to := too;
  Cakdir2.Extract;
end;
procedure Add(what : string);
begin
  Cakdir2.AddOptions.add_files.Add(what);
end;
procedure DoAdd;
begin
  Cakdir2.Add;      
end;
procedure Del(what : string);
begin
  if what <> '' then
  begin
  Cakdir2.UnSelectALL;
  Cakdir2.Select(what,'');
  end;
  Cakdir2.Delete;
end;
procedure Ren(what,too : string);
begin
//  If Cakdir2.ArchiveType = _ZIP then
//         Cakdir2.Zipdirrename(what,too);
end;
procedure Convert(what,totype : string);
begin
  Archive_Convert(Cakdir2,what,Cakdir2.GetArchiveType('xyz.'+totype));
end;
procedure Password(what : string);
begin
  Cakdir2.password := what;
  Cakdir2.AddOptions.add_encrypt := what;
  Cakdir2.AddOptions.add_useencrypt := (what <> '');
end;
procedure Useextrpath(toggle : boolean);
begin
  Cakdir2.Extractoptions.extr_DirNames := toggle;
end;
procedure Useextroverwrite(toggle : boolean);
begin
  Cakdir2.Extractoptions.extr_OverWrite := toggle;
end;
procedure Useaddpath(toggle : boolean);
begin
  Cakdir2.AddOptions.add_usepath := toggle;
end;
procedure UseVerCtrl(toggle : boolean);
begin
  Cakdir2.versioncontrol := toggle;
end;
procedure Userelativepath(toggle : boolean);
begin
  Cakdir2.AddOptions.add_relative := toggle;
end;
procedure Usesubdir(toggle : boolean);
begin
  Cakdir2.AddOptions.add_SubDir := toggle;
end;
procedure RunFile(what,Param : string);
begin
  run(what,Param);
end;

procedure RunAndwait(ProgramPath, ProgramParam: string);
begin
  RunAndWait(ProgramPath,ProgramParam);
end;

procedure MoveFile(what, too : string);
begin
  if not directoryexists(extractfilepath(too)) then
    MakeDirectory(extractfilepath(too));
  Movefile(PCHAR(what),PCHAR(too));
end;
procedure RenFile(what, too : string);
begin
  if not directoryexists(extractfilepath(too)) then
    MakeDirectory(extractfilepath(too));
  Renamefile(what,too);
end;
procedure DelFile(what : string);
begin
  Deletefile(PCHAR(what));
end;
procedure CopyFile(what,too : string);
begin
  if not directoryexists(extractfilepath(too)) then
    MakeDirectory(extractfilepath(too));
  Windows.Copyfile(PCHAR(what),Pchar(too),False);
end;

procedure MakeDir(what : string);
begin
  MakeDirectory(what);
end;
procedure DelDir(what : string);
begin
  Deletedir(what);
end;
procedure OpenFolder(what : string);
begin
  Explorefolder(what);
end;
function Total_Contents : integer;
begin
    Result := Cakdir2.Total_Contents;
end;
function Contents_Filename(which : integer) : String;
begin
    Result := Cakdir2.archive_Contents[which]._Filename;    
end;
function Contents_FileDefPath(which : integer) : String;
begin
    Result := Cakdir2.archive_Contents[which]._Filedefpath;
end;
function Contents_FileSize(which : integer) : Integer;
begin
    Result := Cakdir2.archive_Contents[which]._FileSize;
end;
function Contents_FileCompSize(which : integer) : Integer;
begin
    Result := Cakdir2.archive_Contents[which]._FilePackedSize ;
end;
function Contents_FileSelected(which : integer) : Boolean;
begin
    Result := Cakdir2.archive_Contents[which]._Selected;
end;
function Contents_FileType(which : integer) : String;
begin
    Result := Cakdir2.archive_Contents[which]._FileType;
end;
function Contents_FileDateTime(which : integer) : TDatetime;
begin
    Result := Cakdir2.archive_Contents[which]._FileTime;
end;
function appendSlash(input : string) : string;
begin
    if length(input) > 0 then
    if input[Length(input)] = '\' then
       result := input else
    result := input + '\' else
    result := input;
end;
function pollfilelist(maskedname : string;subdir : boolean) : tstrings;
var sr : TSearchRec;
    astrings : tstrings;
    k : string;
begin
    astrings := tstringlist.create();
    k := Appendslash(extractfilepath(maskedname));

    if FindFirst(maskedname,faAnyfile and faHidden,sr) = 0 then
        begin
        if (sr.name <> '.') and (sr.name <> '..') then
                if fileexists(k + sr.Name) then
                astrings.Add(k + sr.Name);
        while FindNext(sr) = 0 do
        if (sr.name <> '.') and (sr.name <> '..') then
                if fileexists(k + sr.Name) then
                astrings.Add(k + sr.Name);

        end;
        FindClose(sr);

    if subdir then
        if pos('*',maskedname) <> 0 then
        begin
        if FindFirst(Appendslash(extractfilepath(maskedname)) + '*',faDirectory + faHidden ,sr) = 0 then
        begin

        if (sr.name <> '.') and (sr.name <> '..') then
        if directoryexists(k + sr.name) then
        astrings.addstrings(pollfilelist(appendslash(k + sr.name) +  Extractfilename(maskedname) ,subdir));

        While FindNext(sr) = 0 do
                if (sr.name <> '.') and (sr.name <> '..') then
                if directoryexists(k + sr.name) then
                astrings.addstrings(pollfilelist(appendslash(k + sr.name) +  Extractfilename(maskedname) ,subdir));

        end;
        FindClose(sr);
        end;

    result := astrings;
end;
procedure Select_File(which : integer);
begin
  Cakdir2.Archive_Contents[which]._Selected := true;
end;
procedure DeSelect_File(which : integer);
begin
  Cakdir2.Archive_Contents[which]._Selected := false;
end;
function Mask_Select_File(mask : string) : integer;
begin
  Cakdir2.Select(mask,'');
  Result := Cakdir2.Get_Selected_Count;
end;
procedure Clear_select;
begin
  Cakdir2.UnSelectALL;
end;
function TempPath : String;
begin
  Result := GrabTempPath;
end;
function MydocuPath : string;
begin
  Result := GrabMydocuPath;
end;
function DesktopPath : string;
begin
  Result := GrabDesktopPath;
end;
function SystemPath : string;
begin
  Result := GrabSystemPath;
end;

{function download(www,too : string) : boolean;
var ahttpget : THTTPGet;
begin
  aHttpGet := THttpget.Create(Application.MainForm);
  aHttpget.URL := www;
  aHttpget.BinaryData := true;
  //aHttpget.UserName := 'anonymous';
  //aHttpget.Password := 'email@domain.com';
  aHttpget.FileName  := too;
  aHttpget.WaitThread := True;
  aHttpget.GetFile;
  result := fileexists(too);
end;
}
function GetFileSize(const FileName: String): Integer;
var
  myFile: THandle;
  myFindData: TWin32FindData;
begin
  Result := 0;
  myFile := FindFirstFile(PChar(FileName), myFindData);
  if myFile <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(myFile);
    Result := Int64(myFindData.nFileSizeHigh) shl Int64(32) +
Int64(myFindData.nFileSizeLow);
  end;
end;

function GetFileDateTime(const FileName: String): TDatetime;
begin
    Result := FileDateToDateTime(FileAge(Filename));

end;

function New_Form(Left, Top, Width, Height : integer; Caption : string) : TForm;
var aForm : TForm;
begin
  aForm := Tform.Create(nil);
  aForm.Left := Left;
  aForm.Top := Top;
  aForm.Width := Width;
  aForm.Height := Height;
  aForm.Caption := Caption;
  Result := aForm;
end;

function New_Label(Parent : TWinControl; Left, Top : integer; Caption : String) : TLabel;
var aLabel : TLabel;
begin
  aLabel := TLabel.Create(Parent);
  aLabel.Parent := Parent;
  aLabel.Left := Left;
  aLabel.Top := Top;
  aLabel.Caption := Caption;
  Result := aLabel;
end;

function New_Edit(Parent : TWinControl; Left, Top, Width, Height : integer; Text : String) : TEdit;
var aEdit : TEdit;
begin
  aEdit := TEdit.Create(Parent);
  aEdit.Parent := Parent;
  aEdit.Left := Left;
  aEdit.Top := Top;
  aEdit.Width := Width;
  aEdit.Height := Height;
  aEdit.Text := Text;
  Result := aEdit;
end;
function New_Button(Parent : TWinControl; Left, Top, Width, Height : integer; Caption : String) : TButton;
var aButton : TButton;
begin
  aButton := TButton.Create(Parent);
  aButton.Parent := Parent;
  aButton.Left := Left;
  aButton.Top := Top;
  aButton.Width := Width;
  aButton.Height := Height;
  aButton.Caption := Caption;
  Result := aButton;
end;

function New_Memo(Parent : TWinControl; Left, Top, Width, Height : integer) : TMemo;
var aMemo : TMemo;
begin
  aMemo := TMemo.Create(Parent);
  aMemo.Parent := Parent;
  aMemo.Left := Left;
  aMemo.Top := Top;
  aMemo.Width := Width;
  aMemo.Height := Height;
  Result := aMemo;
end;
procedure Show_Form(aForm : TForm);
begin
  aForm.Show;
  while aForm.Visible  do
  begin
    Application.HandleMessage;
  end;
end;
function AskDirDialog(Default : String) : String;
var FolderDialog : TPBFolderDialog;
begin
  FolderDialog := TPBFolderDialog.Create(nil);
  FolderDialog.Folder := Default;
  if FolderDialog.Execute then
     Result := FolderDialog.Folder else
     Result := '';
  FolderDialog.free;
end;
function AskFilenameDialog(Default,filter : String) : String;
var OpenDialog : TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  OpenDialog.Filename := Default;
  OpenDialog.Filter := filter;
  if OpenDialog.Execute then
     Result := OpenDialog.Filename else
     Result := '';
  OpenDialog.Free;
end;

procedure Sort(index : integer; accending : boolean);
begin
  Case Index of
  0 : Cakdir2.Sort(accending,_FName);
  1 : Cakdir2.Sort(accending,_FType);
  2 : Cakdir2.Sort(accending,_FSize);
  3 : Cakdir2.Sort(accending,_FPSize);
  4 : Cakdir2.Sort(accending,_FCRC);
  5 : Cakdir2.Sort(accending,_FRatio);
  6 : Cakdir2.Sort(accending,_FDefPath);
  7 : Cakdir2.Sort(accending,_FTime);
  end;
end;

initialization
finalization
end.
