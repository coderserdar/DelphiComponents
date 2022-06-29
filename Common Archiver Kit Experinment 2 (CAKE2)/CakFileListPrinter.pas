unit CakFileListPrinter;
{*********************************************************}
{*                 CakFileListPrinter.pas                *}
{*                   File List Printer                   *}
{*        Copyright (c) 2003 Joseph Leung Yat Chun       *}
{*                 All rights reserved.                  *}
{*********************************************************}
interface

uses
  SysUtils, Classes, CakDir2, CakDefs2, CakUtils2, QzMiniHtml2, Graphics;

const
TextView =
'<form name="form1" method="post" action="">'+LineBreak+
'<textarea name="textview" cols="%d" rows="%d"  wrap="OFF"></textarea>'+LineBreak+
'</form>';
ScriptConst =
'<script language="JavaScript">'+LineBreak+
'<!--'+LineBreak+
'function ShowMessage(msg) { alert(msg); }'+ LineBreak +
'function MM_findObj(n, d) { //v4.0'+LineBreak+
'  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {'+LineBreak+
'    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}'+LineBreak+
'  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];'+LineBreak+
'  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);'+LineBreak+
'  if(!x && document.getElementById) x=document.getElementById(n); return x;'+LineBreak+
'}'+LineBreak+
'function MM_setTextOfTextfield(objName,x,newText) { //v3.0'+LineBreak+
'  var obj = MM_findObj(objName); if (obj) obj.value = newText;'+LineBreak+
'}'+LineBreak+
'//-->'+LineBreak+
'</script>';
ExtTableHTML =
'<table border="0" width="28" height="10" bordercolor="#000000" cellspacing="1"><tr><td bgcolor="%s" bordercolor="#000000"><font color="#FFFFFF"><b>%s</b></font></td></tr></table>';
type
  FListOptionsType = record
                      headbgcolor, flistbgColor, flistbgcolor2 : TColor;
                      headfontcolor, flistfontcolor : TColor;
                      Border : boolean;
                      IncludeTxtContent : Boolean;
                      MaxTxtViewLines, TxtviewRow,TxtviewCol : integer;
                      Template,Thumbnail : string;
                     end;
type
  TCakFileListPrinter = class(TComponent)
  private
    { Private declarations }
    FListType : FileListType;
    FListShowItems : FileListItemTypes;
    SrtByType,GrpByType : SortByType;
    SortAccending : boolean;
    FListName : string;
    HtmlTemp : TStrings;
    Cakdir : TCakdir2;
    procedure SetHtmlTemp(value : TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    FListOptions : FListOptionsType;
    CodePage : string;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function GetCellData(col : FileListItemType; index : integer) : string;
    function GetColType(index : integer) : FileListItemType; overload;
    function GetColType(aSrtByType : SortByType) : FileListItemType; overload;

    procedure HTMLFileList;
    procedure CsvFileList;
    procedure TxtFileList;
    procedure CreateFileList; dynamic;
    procedure LoadTemplate;
    function  ListTemplate(directory : string) : TStrings;
    procedure InitFListOptions;    
    procedure LoadSettings(filename : string);
    procedure SaveSettings(filename : string);

  published
    { Published declarations }
    property Cakdir2 : TCakdir2 read Cakdir write Cakdir;
    property OutputType : FileListType read FListType write FListType;
    property FLGroupByType : SortByType read GrpByType write GrpByType;
    property FLSortByType : SortByType read SrtByType write SrtByType;
    property FLSortAccend : boolean read SortAccending write SortAccending;
    property ItemToShow : FileListItemTypes read FListShowItems write FListShowItems;
    property FileName : string read FListName write FListName;
    property HtmlTemplate : TStrings read HtmlTemp write SetHtmlTemp;
  end;

procedure Register;

implementation
uses IniFiles, Dialogs;

constructor TCakFileListPrinter.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  HtmlTemp := TStringList.Create;
  HtmlTemp.Text := '<Html><Head><dynamic id="ArchiveName>' + LineBreak +
                   '<dynamic id="codepage"></Head>' + LineBreak +
                   '<dynamic id="ArchiveFileList"></Html>';
  FListShowItems := [Fl_fname,Fl_ftype,Fl_fsize,Fl_fdate,Fl_fPsize,Fl_fCRC,Fl_fratio,Fl_fdefpath];
  SrtByType := _Fname;
  GrpByType := _Fname;
  CodePage := HtmlCodePageMeta[1];
  InitFListOptions;
end;

destructor TCakFileListPrinter.Destroy;
begin
  HtmlTemp.Free;
  inherited Destroy;
end;

procedure TCakFileListPrinter.InitFListOptions;
begin
  with FListOptions do
  begin
  headbgcolor := clNavy;
  headfontcolor := clWhite;
  flistbgcolor := clWhite;
  flistbgcolor2 := clSilver;
  flistfontcolor := clBlack;
  Border := True;
  IncludeTxtContent := False;
  TxtviewRow := 20;
  TxtviewCol := 80;
  MaxTxtViewLines := 20;
  Template := '';
  Thumbnail := '';
  end;
end;

function TCakFileListPrinter.GetCellData(col : FileListItemType; index : integer) : string;
 function Extractext(value : string) : string;
 begin
  Result := Extractfileext(Value);
  if (Result <> '') and (Result[1] = '.') then
    Result := Copy(Result,2,Length(Result)-1);
 end;
begin
    with Cakdir2.Archive_Contents[index] do
    Case col of
    fl_fext    : Result := Extractext(_Filename);
    Fl_fname   : Result := _Filename;
    Fl_ftype   : Result := _Filetype;
    Fl_fsize   : Result := SizeinK(_Filesize);
    Fl_fdate   : Result := Datetimetostr(_Filetime);
    Fl_fPsize  : Result := SizeinK(_FilePackedsize);
    Fl_fCRC    : Result := _FileCRC;
    Fl_fratio  : Result := Inttostr(_Fileratio) + '%';
    Fl_fdefpath: Result := _FileDefPath;
    end;
end;

function TCakFileListPrinter.GetColType(index : integer) : FileListItemType;
begin
  Case index of
      1 : Result := fl_fext;
      2 : Result := fl_fname;
      3 : Result := fl_ftype;
      4 : Result := fl_fsize;
      5 : Result := fl_fdate;
      6 : Result := fl_fpsize;
      7 : Result := fl_fratio;
      8 : Result := fl_fcrc;
      9 : Result := fl_fdefpath;
  else Result := fl_fname;
  end;
end;

function TCakFileListPrinter.GetColType(aSrtByType : SortByType) : FileListItemType;
begin
  Case aSrtByType of
      _fname   : Result := fl_fname;
      _ftype   : Result := fl_ftype;
      _fsize   : Result := fl_fsize;
      _ftime   : Result := fl_fdate;
      _fpsize  : Result := fl_fpsize;
      _fratio  : Result := fl_fratio;
      _fcrc    : Result := fl_fcrc;
      _fdefpath: Result := fl_fdefpath;
  else Result := fl_fname;
  end;
end;


procedure TCakFileListPrinter.SetHtmlTemp(value : TStrings);
begin
  HtmlTemp.Assign(Value)
end;

procedure TCakFileListPrinter.CsvFileList;
var i,j : integer;
    k : string;
    Output : TStrings;
procedure AddToOutput(value : string); begin if (k = '') then k := value else
                                       k := k + ',' + value; end;
procedure AddCol(i : integer); begin AddToOutput(columns[i]); end;
begin
  k := '';
  Output := TStringList.Create;
  try
  for i := 1 to totalcolumns do
     if GetColType(i) in FListShowItems then AddCol(i);
  Output.Add(k);
  for i := 0 to Cakdir2.Total_Contents -1 do
  begin
  k := '';
  for j := 1 to totalcolumns do
     if GetColType(j) in FListShowItems then AddtoOutput(GetCellData(GetColType(j),i));
  Output.Add(k);
  end;

  Output.SaveToFile(FileName);
  finally
    Output.Free;
  end;
end;

procedure TCakFileListPrinter.TxtFileList;
var Maxsize : array[1..Totalcolumns] of integer;
    Output : TStrings;
    i : integer;
    prevItem : string;
  procedure CalculateMaxSize;
  var i,j : integer;
  begin
  for j := 1 to TotalColumns do MaxSize[j] := Length(Columns[j]);
  for i := 0 to Cakdir2.Total_Contents - 1 do for j := 1 to TotalColumns do
   if MaxSize[j] < Length(GetCellData(GetColType(j),i))
    then MaxSize[j] := Length(GetCellData(GetColType(j),i));
  end;
  function GetFormatStr(index : integer) : String;
  begin
    Result := '%'+Inttostr(Maxsize[index])+'s';
  end;
  function MakeLineBreak(Fill,Sep : Char) : WideString;
  var i,j : integer;
      k : WideString;
  begin
    k := '';
    for i := 1 to TotalColumns do
      if GetColType(i) in FListShowItems then
        begin
        if k = '' then
        k := k + '|' else
        k := k + Sep;
        for j := 1 to Maxsize[i] do
          k := k + Fill;
        end;
    Result := k + '|' ;
  end;
  function CellText(Value : string) : WideString;
  var i, totalsize : integer;
  begin
    TotalSize := 0;
    for i := 1 to TotalColumns do
      if GetColType(i) in FListShowItems then
        Inc(totalsize,Maxsize[i] + 1);
    Result := Format('|%-'+ Inttostr(TotalSize-1) +'s|',[Value])
  end;
  function MakeHeaderRow : WideString;
  var k : WideString;
      i : integer;
    procedure AddCol(i : integer);
    begin k := k + '|' + Format(GetFormatStr(i),[columns[i]]); end;
  begin
    k := '';
    for i := 1 to TotalColumns do
    if GetColType(i) in FListShowItems then AddCol(i);
    Result := k + '|';
  end;
  function MakeFileListRow(index : integer) : WideString;
  var k : WideString;
      i : integer;
  begin
    k := '';
    for i := 1 to TotalColumns do
    if GetColType(i) in FListShowItems then
      k := k + '|' + Format(GetFormatStr(i),[GetCellData(GetColType(i),index)]);
     Result := k + '|';
  end;
  function MakeGrpRow(index : integer) : WideString;
  var k : WideString;
  begin
    k := GetCellData(GetColType(GrpByType),index);
    k := '|'+k + Format('%'+inttostr(length(MakeFileListRow(index))-length(k)-2)+'s',['']);
    Result := k + '|';
  end;
begin
  Output := TStringList.Create;
  try
    CalculateMaxSize;
    Output.Text := MakeLineBreak('=','=');
    Output.Text := Output.Text + CellText(Format('Archive contents for : %s',[Extractfilename(Cakdir2.ArchiveName)]));
    Output.Text := Output.Text + CellText(Format('Archive size : %s',[SizeinK(GetFileSize(Cakdir2.ArchiveName))]));
    Output.Text := Output.Text + CellText(Format('Total %d file(s), %d folder(s) ',[Cakdir2.Total_Contents, cakdir2.DirectoryList.Count]));
    Output.Text := Output.Text + CellText(Format('Printed on : %s',[DateTimetoStr(Now)]));
//    Output.Text := Output.Text + CellText(Format(' : %s',[]));
    Output.Text := Output.Text + MakeLineBreak('=','|');
    Output.Text := Output.Text + MakeHeaderRow;
    Output.Text := Output.Text + MakeLineBreak('=','|');
    previtem := '';

    for i := 0 to Cakdir2.Total_Contents -1 do
     begin
     if (GrpByType <> _fname) then
      if GetCellData(GetColType(GrpByType),i) <> previtem then
       begin
        Output.Text := Output.Text + MakeLineBreak('-','|');
        Output.Text := Output.Text + MakeGrpRow(i);
        Output.Text := Output.Text + MakeLineBreak('-','|');
       end;
     previtem := GetCellData(GetColType(GrpByType),i);
     Output.Text := Output.Text + MakeFileListRow(i);
     end;
     
    Output.Text := Output.Text + MakeLineBreak('=','=');
    Output.SaveToFile(FileName);
  finally
  Output.free;
  end;
end;

procedure TCakFileListPrinter.HTMLFileList;
var Output : TStrings;
    Processing : WideString;
    i : integer;
function MakeHeaderRow : Widestring;
var i : integer;
    k : WideString;
  procedure AddCol(i : integer);
  begin k := k + columns[i]; end;
begin
    k := k + Format('<TR Bgcolor="%s" Align="Centre">',
               [Tcolor2webcolor(FListOptions.headbgcolor)]);
    for i := 1 to totalcolumns do
    begin

      if GetColType(i) in FListShowItems then
        begin
        k := k + '<TD><Div Align="Center">' + Format('<B><FONT Color="%s">',
           [Tcolor2webcolor(FListOptions.headfontcolor)]);
        AddCol(i);
        k := k + '</DIV></FONT></B></TD>';
        end;
    end;
    k := k + '</TR>' + LineBreak;
    Result := k;
end;

function MakeFileListRow(index : integer) : WideString;
var i: integer;
    k : WideString;

function isTxt : boolean;
var ext : string;
begin
  ext := lowercase(Extractfileext(Cakdir2.archive_Contents[index]._FileName));
  Result := pos(ext,'.txt .pas') <> 0;;
end;

function GetText : string;
var input : TStrings;
    i : integer;
begin
  Result := Cakdir2.archive_Contents[index]._FileName;

  If isTxt then
    begin
      Cakdir2.Extractoptions.Extr_to := GrabTempPath;
      Cakdir2.Extractoptions.Extr_Overwrite := true;
      Cakdir2.Extractoptions.Extr_DirNames := false;
      Cakdir2.Select(index);
      Cakdir2.Extract;
      input := TStringList.Create;
      input.LoadFromFile(GrabTempPath+Cakdir2.archive_Contents[index]._FileName);
      for i := 0 to input.Count -1 do
        if (i < FListOptions.MaxTxtViewLines) then
          Result := Result + ' \r ' + Replace(Replace(input.Strings[i],'''',' '),'"',' ');
      Input.Free;
    end;
end;

begin
  if (index = 0) or (index mod 2 = 0) then
  k := Format('<TR bgcolor="%s" align="Right">',
       [tcolor2webcolor(FListOptions.flistbgcolor2)]) else
  k := Format('<TR bgcolor="%s" align="Right">',
       [tcolor2webcolor(FListOptions.flistbgcolor)]);
  for i := 1 to totalcolumns do
    begin
      if GetColType(i) in FListShowItems then
        begin
          if GetColType(i) = Fl_fname then
          k := k + '<TD align="Left">' else
          k := k + '<TD>';

          k := k + Format('<FONT Color="%s"',
                 [tcolor2webcolor(FListOptions.flistfontcolor)]);;
          if (GetColType(i) = fl_fname) and (FListOptions.IncludeTxtContent) and (isTxt) then
          k := k + Format(' onClick="MM_setTextOfTextfield(''textview'','''',''%s'')">',[GetText]) else
          k := k + '>';

          if GetColType(i) = fl_fext then
          k := k + Format(ExtTableHTML,[tcolor2webcolor(FListOptions.headbgcolor),GetCellData(GetColType(i),index)]) else
          k := k + GetCellData(GetColType(i),index);

          k := k + '</FONT></TD>';
        end;
    end;
    k := k + '</TR>' + LineBreak;
    Result := k;
end;

  function MakeGrpRow(index : integer) : WideString;
  var k : WideString;
      i, colcount : integer;
  begin
    colCount := 0;
    for i := 1 to totalcolumns do
      if GetColType(i) in FListShowItems then
       Inc(colcount);

    k := k + Format('<TR bgcolor="%s"><TD align="Left" colspan="%d">',
                 [tcolor2webcolor(FListOptions.headbgcolor),colcount]);
    k := k + Format('<U><B><FONT Color="%s">',
                 [tcolor2webcolor(FListOptions.headfontcolor)]);
    k := k + GetCellData(GetColType(GrpByType),index);
    k := k + '</FONT></U></B</TD></TR>';
    Result := k;
  end;


function MakeFileList : WideString;
var i : integer;
    previtem : string;
begin
  Result := MakeHeaderRow;
  previtem := '';
  for i := 0 to Cakdir2.Total_Contents -1 do
   begin
    if (GrpByType <> _fname) then
      if GetCellData(GetColType(GrpByType),i) <> previtem then
       Result := Result + MakeGrpRow(i);
    previtem := GetCellData(GetColType(GrpByType),i);
    Result := Result + MakeFileListRow(i);
   end;
  if FListOptions.Border then
  Result := Format('<Table Width="100%%" Cellpadding="6" Cellspacing="1" Bgcolor="%s">',
                   [tcolor2webcolor(FListOptions.flistbgcolor2)])
            +LineBreak+Result+LineBreak+'</Table>' else
  Result := '<Table Width="100%">'+LineBreak+Result+LineBreak+'</Table>';
end;

begin
  Output := TStringList.Create;
  try
    for i := 0 to HtmlTemp.Count -1 do
    begin
    Processing := HtmlTemp.Strings[i];
    Processing := Replace(Processing,'<dynamic id="codepage">',
      format('<meta http-equiv="Content-Type" content="text/html; charset="%s">',[CodePage]));
    Processing := Replace(Processing,'<dynamic id="today">',DateToStr(now));
    Processing := Replace(Processing,'<dynamic id="now">',DateTimeToStr(now));
    if Pos('<dynamic id="archivefilelist">',lowercase(Processing)) > 0 then
    Processing := Replace(Processing,'<dynamic id="archivefilelist">',MakeFileList);
    Processing := Replace(Processing,'<dynamic id="archivename">',ExtractFileName(Cakdir2.ArchiveName));
    Processing := Replace(Processing,'<dynamic id="archivepath">',ExtractFilePath(Cakdir2.ArchiveName));
    Processing := Replace(Processing,'<dynamic id="archivepackedsize">',SizeinK(Cakdir2.Get_Total_Packed_Size));
    Processing := Replace(Processing,'<dynamic id="archivesize">',SizeinK(Cakdir2.Get_Total_Size));
    Processing := Replace(Processing,'<dynamic id="archivefilecount">',Inttostr(Cakdir2.Total_Contents));
    if Cakdir2.DirectoryList.Count = 0 then
    Processing := Replace(Processing,'<dynamic id="archivedircount">','1') else
    Processing := Replace(Processing,'<dynamic id="archivedircount">',Inttostr(Cakdir2.DirectoryList.Count));

    Processing := Replace(Processing,'<dynamic id="scriptconst">',ScriptConst);
    with FlistOptions do
    Processing := Replace(Processing,'<dynamic id="textview">',Format(Textview,[TxtviewCol,TxtviewRow]));

    //'<dynamic id="ArchiveFileComment">';
    Output.Text := Output.Text + Processing;
    end;
    Output.SaveToFile(FileName);
  finally
  Output.free;
  end;
end;

procedure TCakFileListPrinter.CreateFileList;
begin
  Cakdir2.Sort(SortAccending,SrtByType);
  if (GrpByType <> _FName) then
   begin
    Cakdir2.Sort(SortAccending,GrpByType);
    ItemToShow := ItemToShow - [GetColType(GrpByType)];
   end;

  if Cakdir2.DirectoryList.Count = 0 then
    FListShowItems := FListShowItems - [fl_fdefpath];
  Case FListType of
  _Txt : TxtFileList;
  _Htm : HtmlFileList;
  _Csv : CsvFileList;
  else ShowMessage('Not Supported');
  end;
end;

function ReturnListItemName(FileListItem : FileListItemType) : string;
begin
  Case FileListItem of
  Fl_fext  : Result := 'Ext';
  Fl_fname : Result := 'name';
  Fl_ftype : Result := 'Type';
  Fl_fsize : Result := 'Size';
  Fl_fdate : Result := 'Date';
 Fl_fPsize : Result := 'PSize';
   Fl_fCRC : Result := 'CRC';
 Fl_fratio : Result := 'Ratio';
Fl_fdefpath: Result := 'Path';
  end;
end;

procedure TCakFileListPrinter.SaveSettings(filename : string);
var f : FileListItemType;
begin
  with TIniFile.Create(filename) do
   with FListOptions do
    try
      WriteString('FileList','headbgcolor',TColor2WebColor(headbgcolor));
      WriteString('FileList','flistbgColor',TColor2WebColor(flistbgColor));
      WriteString('FileList','flistbgColor2',TColor2WebColor(flistbgColor2));
      WriteString('FileList','headfontcolor',TColor2WebColor(headfontcolor));
      WriteString('FileList','flistfontColor',TColor2WebColor(flistfontColor));
      WriteString('FileList','template',Template);
      WriteString('FileList','thumbnail',Thumbnail);
        WriteBool('FileList','IncludeTxtContent',IncludeTxtContent);
        WriteBool('FileList','Border',Border);
      for f := Fl_fext to Fl_fdefpath do
        if f in FListShowItems then
          WriteBool('ShowItems',ReturnListItemName(f),true) else
          WriteBool('ShowItems',ReturnListItemName(f),false);
    finally
      free;
    end;
end;

procedure TCakFileListPrinter.LoadSettings(filename : string);
var f : FileListItemType;
begin
  InitFListOptions;
  with TIniFile.Create(filename) do
   with FListOptions do
    try
      headbgcolor   :=WebColor2TColor(ReadString('FileList','headbgcolor',TColor2WebColor(headbgcolor)));
      flistbgColor  :=WebColor2TColor(ReadString('FileList','flistbgColor',TColor2WebColor(flistbgColor)));
      flistbgColor2 :=WebColor2TColor(ReadString('FileList','flistbgColor2',TColor2WebColor(flistbgColor2)));
      headfontcolor :=WebColor2TColor(ReadString('FileList','headfontcolor',TColor2WebColor(headfontcolor)));
      flistfontcolor:=WebColor2TColor(ReadString('FileList','flistfontColor',TColor2WebColor(flistfontColor)));
      IncludeTxtContent:=ReadBool('FileList','IncludeTxtContent',IncludeTxtContent);
      Border := ReadBool('FileList','Border',Border);
      Template      :=ReadString('FileList','template',Template);
      Thumbnail     :=ReadString('FileList','thumbnail',Thumbnail);

      FListShowItems := [];
      for f := Fl_fext to Fl_fdefpath do
        if ReadBool('ShowItems',ReturnListItemName(f),true) then
          FListShowItems := FListShowItems + [f]; 

      LoadTemplate;
    finally
      free;
    end;
end;

procedure TCakFileListPrinter.LoadTemplate;
begin
  with FlistOptions do
  if Template <> '' then
    if fileexists(FileListPath+Template) then
      HtmlTemp.LoadFromFile(FileListPath+Template);
end;

function TCakFileListPrinter.ListTemplate(directory : string) : TStrings;
var i : integer;
begin
  Result := Pollfilelist(appendslash(directory)+'*.ini',false);
  for i := 0 to Result.Count -1 do
    Result.Strings[i] := Removefileext(Extractfilename(Result.Strings[i]));
end;

procedure Register;
begin
  RegisterComponents('CAKE', [TCakFileListPrinter]);
end;

end.
