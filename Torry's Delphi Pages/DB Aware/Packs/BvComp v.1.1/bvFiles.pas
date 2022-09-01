unit bvFiles;

interface

uses
{$ifndef LINUX}
   classes,
   Windows,
   Forms;
{$else}
   Libc,
   QForms;
{$endif}

//function DirectoryExists(const Name: string): Boolean;

function GetUniqueFileName(Directory :string='?';FileName:string=''):string;
//function GetUniqueTableName(Directory :string='?';EXT:string='db'):string;

function GetDirSize(Directory:string;Recurse:boolean):longint;

function CheckPath(const path:string):string;

function ExtractFileNameNotExt(FileName:string):string;

function GetTempDir: string;

function getFileList(Directory:string;List:TStrings):boolean;

//function IsDLL:boolean;


function DeleteFiles(const FileMask: string;BreakOnError:boolean = true): Boolean;

implementation

uses sysutils,bvStringUtils;

{
function GetUniqueTableName(Directory :string='?';EXT:string='db'):string;
begin
  Result:=GetUniqueFileName(Directory,'.'+ext);
end;
}

function GetUniqueFileName(Directory :string='?';FileName:string=''):string;
Var Name:string;
    FileExt:string;
    ThisNum:integer;
begin
    directory:=trim(Directory);
    if Directory='?' then Directory:=ExtractFilePath(Application.ExeName);
    FileName:=trim(FileName);
    FileExt:=extractfileext(FileName);
    FileName:=ExtractfileNameNotExt(FileName);
    FileName:=StringReplace( FileName,'*','',[rfReplaceAll]);
    ThisNum:=1;
    if (FileName='') or FileExists(Directory+FileName+FileExt) then
      while true do begin
         Randomize;
         if FileName='' then Name:='bv'
         else Name:=FileName+'_';
         Name:=Name+'_'+trim(inttostr({random(60000)} ThisNUm));
         if not FileExists(Directory+Name+FileExt) then break;
         inc(thisNum);
      end
    else Name:=fileName;
    Result:=Directory+Name+Fileext;
end;

{
function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
}

function GetDirSize(Directory:string;Recurse:boolean):longint;
var
  SearchRec: TSearchRec;
  FullSize:longint;
begin
   FindFirst(Directory, faAnyFile, SearchRec);
   FullSize:=SearchRec.Size;

   while (FindNext(SearchRec) = 0) do begin
      if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
        if ((faDirectory and SearchRec.Attr)>0) and Recurse
        then
           inc(FullSize,GetDirSize(extractfilepath(Directory)+
               SearchRec.Name+'\'+extractfileName(Directory),Recurse))
        else
           FullSize:=FullSize+SearchRec.Size;
      end;
   end;
   FindClose(SearchRec);
   Result:=FullSize;
end;

function CheckPath(const path:string):string;
var Len:integer;
begin
  result:=trim(path);
  len:=length(Result);
  if (len>0) then Result:=
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}
       (Result);
end;


function ExtractFileNameNotExt(FileName:string):string;
var Ext:string;
begin
  FileName:=ExtractFileName(FileName);
  ext:=extractfileext(FileName);

  if ext<>'' then  Delete(FileName,length(FileName)-length(Ext)+1,length(ext));
  result:=FileName;
end;

function GetTempDir: string;
{$ifndef LINUX}
var
  Buffer: array[0..1023] of Char;
{$endif}
begin
{$ifndef LINUX}
  SetString(Result, Buffer, GetTempPath(SizeOf(Buffer), Buffer));
{$else}
  Result:=extractfilepath(tempnam(nil,nil));
{$endif}
end;


function getFileList(Directory:string;List:TStrings):boolean;
var thRec:TSearchRec;
begin
  try
      List.clear;
      if FindFirst( includetrailingbackslash(Directory)+'*',faAnyFile,thRec)=0 then
      repeat
        if not ((thRec.name='.') or (thRec.name='..'))
        then List.add(thRec.Name);
      until FindNext(thRec)<>0;
      FindClose(thREc);
      Result:=true;
  except
      Result:=false;
  end;

end;

function DeleteFiles(const FileMask: string; BreakOnError:boolean): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := FindFirst(ExpandFileName(FileMask), faAnyFile, SearchRec) = 0;
  try
    if Result then
      repeat
        if (SearchRec.Name[1] <> '.') and
          (SearchRec.Attr and faVolumeID <> faVolumeID) and
          (SearchRec.Attr and faDirectory <> faDirectory) then
        begin
          Result := DeleteFile(ExtractFilePath(FileMask) + SearchRec.Name);
          if not Result and BreakOnError then Break;
        end;
      until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

end.
