unit ifpi_cakdir2;
{$I ifps3_def.inc}
interface
uses
  Dialogs, IFPS3CompExec, ifps3, ifps3common, ifps3utl, ifpiclassruntime, Cakdir2, ifpscomp,
  ifpiclass, ifpi_cakcmd, QzLib;

procedure AddFunc_Cakdir(Sender: TIFPS3CompExec);  
procedure RIRegister_Cakdir(Cl: TIFPSRuntimeClassImporter);
procedure SIRegister_Cakdir(Cl: TIFPSCompileTimeClassesImporter);
implementation
uses SysUtils;
function sizeinK(size: integer): string;
var
  j: real;
  k : string;
begin
  if size = 0 then
    Result := '0 kb'
  else
  begin
    j := (size / 1000);
    if j <= 999.99 then
      k := FormatFloat('##0.00', j)
    else
      k := FormatFloat('###,###,###,##0', j);
    Result := k + ' kb';
  end;
end;

procedure AddFunc_Cakdir(Sender: TIFPS3CompExec);
begin
    Sender.AddFunction(@SizeinK, 'function sizeinK(size: integer): string;');
    Sender.AddFunction(@ifpi_cakcmd.Param, 'function Param : Tstrings;');
    Sender.AddFunction(@ifpi_cakcmd.New, 'procedure New(name : string);');
    Sender.AddFunction(@ifpi_cakcmd.Open, 'procedure Open(name : string);');
    Sender.AddFunction(@ifpi_cakcmd.Close, 'procedure Close;');
    Sender.AddFunction(@ifpi_cakcmd.Test, 'procedure Test;');
    Sender.AddFunction(@ifpi_cakcmd.Extract, 'procedure Extract(what, too : string)');
    Sender.AddFunction(@ifpi_cakcmd.Add, 'procedure Add(name : string);');
    Sender.AddFunction(@ifpi_cakcmd.DoAdd, 'procedure DoAdd;');
    Sender.AddFunction(@ifpi_cakcmd.Del, 'procedure Del(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.Ren, 'procedure Ren(what,too : string);');
    Sender.AddFunction(@ifpi_cakcmd.Convert, 'procedure Convert(what,totype : string);');
    Sender.AddFunction(@ifpi_cakcmd.Password, 'procedure Password(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.UseVerCtrl, 'procedure UseVerCtrl(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.UseExtrPath, 'procedure Useextrpath(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.UseExtrOverWrite, 'procedure Useextroverwrite(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.UseAddPath, 'procedure Useaddpath(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.UseRelativePath, 'procedure Userelativepath(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.UseSubdir, 'procedure Usesubdir(toggle : boolean);');
    Sender.AddFunction(@ifpi_cakcmd.RunFile, 'procedure RunFile(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.MoveFile, 'procedure MoveFile(what, too : string);');
    Sender.AddFunction(@ifpi_cakcmd.RenFile, 'procedure RenFile(what, too : string);');
    Sender.AddFunction(@ifpi_cakcmd.Delfile, 'procedure DelFile(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.Copyfile, 'procedure CopyFile(what,too : string);');
    Sender.AddFunction(@ifpi_cakcmd.MakeDir, 'procedure MakeDir(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.DelDir, 'procedure DelDir(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.Sort, 'procedure Sort(index : integer; accending : boolean);');

    Sender.AddFunction(@ifpi_cakcmd.OpenFolder, 'procedure OpenFolder(what : string);');
    Sender.AddFunction(@ifpi_cakcmd.OpenFolder, 'procedure OpenDir(what : string);');


    Sender.AddFunction(@ifpi_cakcmd.Total_Contents, 'function Total_Contents : integer;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_Filename, 'function Contents_Filename(which : integer) : String;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileDefPath, 'function Contents_FileDefPath(which : integer) : String;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileSize, 'function Contents_FileSize(which : integer) : Integer;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileCompSize, 'function Contents_FileCompSize(which : integer) : Integer;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileSelected, 'function Contents_FileSelected(which : integer) : Boolean;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileType, 'function Contents_FileType(which : integer) : String;');
    Sender.AddFunction(@ifpi_cakcmd.Contents_FileDateTime, 'function Contents_FileDateTime(which : integer) : TDatetime;');

    Sender.AddFunction(@ifpi_cakcmd.Select_File, 'procedure Select_File(which : integer);');
    Sender.AddFunction(@ifpi_cakcmd.DeSelect_File, 'procedure DeSelect_File(which : integer);');
    Sender.AddFunction(@ifpi_cakcmd.Mask_Select_File, 'function Mask_Select_File(mask : string) : integer;');
    Sender.AddFunction(@ifpi_cakcmd.Clear_Select, 'procedure Clear_select;');

    Sender.AddFunction(@ifpi_cakcmd.TempPath, 'function TempPath : String;');
    Sender.AddFunction(@ifpi_cakcmd.MyDocuPath, 'function MydocuPath : string;');
    Sender.AddFunction(@ifpi_cakcmd.DesktopPath, 'function DesktopPath : string;');
    Sender.AddFunction(@ifpi_cakcmd.SystemPath, 'function SystemPath : string;');
    Sender.AddFunction(@QzLib.GrabProgramPath, 'function ProgramPath : string;');
    Sender.AddFunction(@QzLib.GrabCurrentPath, 'function CurrentPath : string;');

    Sender.AddFunction(@ifpi_cakcmd.Getfilesize, 'function GetFileSize(const FileName: String): Integer;');
    Sender.AddFunction(@ifpi_cakcmd.GetfileDatetime, 'function GetFileDateTime(const FileName: String): TDatetime;');

    Sender.AddFunction(@ifpi_cakcmd.Appendslash,'function appendSlash(input : string) : string;');
    Sender.AddFunction(@ifpi_cakcmd.Pollfilelist,'function pollfilelist(maskedname : string; subdir : boolean) : tstrings;');
    Sender.AddFunction(@ifpi_cakcmd.Download,'function download(www,too : string) : boolean;');

    Sender.AddFunction(@ifpi_cakcmd.New_Form, 'function New_Form(Left, Top, Width, Height : integer; Caption : string) : TForm;');
    Sender.AddFunction(@ifpi_cakcmd.New_Label, 'function New_Label(Form : TForm; Left, Top : integer; Caption : String) : TLabel;');
    Sender.AddFunction(@ifpi_cakcmd.New_Edit, 'function New_Edit(Form : TForm; Left, Top, Width, Height : integer; Text : String) : TEdit;');
    Sender.AddFunction(@ifpi_cakcmd.New_Button, 'function New_Button(Form : TForm; Left, Top, Width, Height : integer; Caption : String) : TButton;');
    Sender.AddFunction(@ifpi_cakcmd.New_Memo, 'function New_Memo(Form : TForm; Left, Top, Width, Height : integer) : TMemo;');
    Sender.AddFunction(@ifpi_cakcmd.AskDirDialog, 'function AskDirDialog(Default : String) : String;');
    Sender.AddFunction(@ifpi_cakcmd.AskFilenameDialog, 'function AskFilenameDialog(Default,filter : String) : String;');

    Sender.AddFunction(@ifpi_cakcmd.Show_Form, 'procedure Show_Form(aForm : TForm);');


    Sender.AddFunction(@SysUtils.Now, 'function Now: TDateTime;');
    Sender.AddFunction(@SysUtils.Directoryexists, 'function DirectoryExists(const FileName: string): Boolean;');
    Sender.AddFunction(@SysUtils.Fileexists, 'function FileExists(const FileName: string): Boolean;');
    Sender.AddFunction(@SysUtils.ExtractFilename, 'function ExtractFileName(const FileName: string): string;');
    Sender.AddFunction(@SysUtils.ExtractFilepath, 'function ExtractFilePath(const FileName: string): string;');
    Sender.AddFunction(@SysUtils.ExtractFileExt, 'function ExtractFileExt(const FileName: string): string;');

    Sender.AddFunction(@Dialogs.ShowMessage, 'procedure ShowMessage(const Msg: string);');

    //Sender.AddFunction(@SysUtils.Format, 'function Format(const Format: string; const Args: array of const): string;');

end;

procedure RIRegister_Cakdir(Cl: TIFPSRuntimeClassImporter);
begin
    with CL.Add(TCakdir) do
    begin

      RegisterMethod(@TCakdir.Test, 'TEST');
      RegisterMethod(@TCakdir.Extract, 'EXTRACT');
      RegisterMethod(@TCakdir.Add, 'ADD');
      RegisterMethod(@TCakdir.Delete, 'DELETE');

      RegisterMethod(@TCakdir.Run, 'RUN');
      RegisterMethod(@TCakdir.Runwww, 'RUNWWW');
      RegisterMethod(@TCakdir.Runandwait, 'RUNANDWAIT');

      RegisterMethod(@TCakdir.CreateShortcut, 'CREATESHORTCUT');
      RegisterMethod(@TCakdir.DeleteDir, 'DELETEDIR');
      RegisterMethod(@TCakdir.Explorefolder, 'EXPLOREFOLDER');

      RegisterMethod(@TCakdir.GrabArchivePath, 'GRABARCHIVEPATH');
      RegisterMethod(@TCakdir.GrabDesktopPath, 'GRABDESKTOPPATH');
      RegisterMethod(@TCakdir.GrabProgramPath, 'GRABPROGRAMPATH');
      RegisterMethod(@TCakdir.GrabCurrentPath, 'GRABCURRENTPATH');
      RegisterMethod(@TCakdir.GrabTempPath, 'GRABTEMPPATH');
      RegisterMethod(@TCakdir.GrabSystemPath, 'GRABSYSTEMPATH');
      RegisterMethod(@TCakdir.GrabWindowPath, 'GRABWINDOWEPATH');
      RegisterMethod(@TCakdir.GrabMydocuPath, 'GRABMYDOCUPATH');

      RegisterMethod(@TCakdir.Add_All_Selected_List, 'ADD_ALL_SELECTED_LIST');
      RegisterMethod(@TCakdir.Mask_Add_Selected_List, 'MASK_ADD_SELECTED_LIST');
      RegisterMethod(@TCakdir.Clear_Selected_List, 'CLEAR_SELECTED_LIST');

      RegisterMethod(@TCakdir.Canadd, 'CANADD');
      RegisterMethod(@TCakdir.Canextract, 'CANEXTRACT');

      RegisterMethod(@TCakdir.Get_Total_Size, 'GET_TOTAL_SIZE');


    end;
end;

procedure SIRegister_Cakdir(Cl: TIFPSCompileTimeClassesImporter);
begin
    CL.se.AddTypeS('TDatetime','Double');
    CL.se.AddTypeS('Contenttype','record _FileRatio : Word;_FileIcon, _Tag : Word;_FileSize,_FilePackedSize : Longword;_FileTime : TDatetime;_FileCRC : string;_Filename,_Filetype,_FileFullPath,_FileDefPath,_FileArchive : String;_Encrypted, _Selected : boolean;end;');
    CL.se.AddTypeS('ExtractOptionstype','record extr_to : string; extr_DirNames : boolean; extr_OverWrite : boolean; extr_ArcINArc : boolean; extr_Extractall : boolean; end;');
    with CL.Add(CL.FindClass('TCOMPONENT'), TCAKDIR) do
    begin
      RegisterPublishedProperties;

      //RegisterPublishedProperty('Archive_Contents');
      RegisterMethod('function ArcContents(var recordno : integer) : contenttype;');
      RegisterMethod('procedure Test;');
      RegisterMethod('procedure Extract;');
      RegisterMethod('procedure Add;');

      RegisterMethod('procedure run(programpath,Programparam : string);');
      RegisterMethod('procedure runwww(wwwpath : string);');
      RegisterMethod('procedure runandwait(programpath,Programparam : string);');

      RegisterMethod('function CreateShortcut(linkfilename,filepath : string) : boolean;');
      RegisterMethod('procedure DeleteDir(aDir: string);');
      RegisterMethod('procedure Explorefolder(folder : string);');

      RegisterMethod('function GrabArchivePath : string;');
      RegisterMethod('function GrabDesktopPath : string;');
      RegisterMethod('function GrabProgramPath : string;');
      RegisterMethod('function GrabCurrentPath : string;');
      RegisterMethod('function GrabTempPath : string;');
      RegisterMethod('function GrabSystemPath : string;');
      RegisterMethod('function GrabWindowPath : string;');
      RegisterMethod('function GrabMydocuPath : string;');

      RegisterMethod('procedure Add_All_Selected_List;');
      RegisterMethod('procedure Mask_Add_Selected_List(FileMasks, Filearchive: string);');
      RegisterMethod('procedure Clear_Selected_List;');

      RegisterMethod('function CanAdd : boolean;');
      RegisterMethod('function CanExtract : boolean;');
      RegisterMethod('procedure Delete;');

      RegisterMethod('function Get_Total_Size : Longint;');
    end;
end;


end.
