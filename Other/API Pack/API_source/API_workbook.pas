unit API_workbook;

// workbook, that will replace excel almost competely. this is fast and works;
// so it already has all the features excel doesn't. and also this is made
// threadsafe :).
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// 2.11.2009, r1.28, ari pikivirta
//  * added function SwapRows to use at least in sortbycolumn function
//  * minor changes on sortby column function (might be even faster now too)
//  * added clearrow function to use in insert row function
//  * page change no longer sets modification time stamp
//  * adding page does not set creation time anymore
//  * minor changes as optimizing for the performance
//
// 5.6.2009, r1.27, ari pikivirta
//  * fixed bug on indexofrow() function
//
// 1.6.2009, r1.26, ari pikivirta
//  * added option parameter AsDatetimeString to SortByColumn function
//
// 26052009, r1.25, ari pikivirta
//  * removed property use lock file
//  * removed color from the cell records
//
// 18052009, r1.24, ari pikivirta
//  * added funclock to apply critical section during function operations
//  * replaced many of the for loops with while loops
//
// 16012009, r1.23, ari pikivirta
//  * get and set cell functions rewrite
//  * memory for current column and row for easier data type access with
//    first, next, previous and last functions
//  * IgnoreCase option added to FindText function
//  * couple of boundary check bugs fixed
//
// 15012009, r1.22, ari pikivirta
//  * minor changes on use of lock file on saving the workbook to file
//  * header changed, since all empty lines removed from the result
//
// 05102008, r1.20, ari pikivirta
//  * added FindMaskedText function (text with wildcards * and ?)
//  * removed addbackslash because it's also in api_files
//
// 27092008, r1.19, ari pikivirta
//  * removed both float and int64 fields from cells (text is enough)
//
// 26092008, r1.18, ari pikivirta
//  * overloaded get & set cell value with normal integer
//
// 22092008, r1.17, aripikivirta
//  * removed compression as zlib is not supplied with d2009 anymore (?)
//  * updated all strings inside the file to be encoded/decoded 64
//  * old / previos revision files can be loaded with openworkbook_o(fname) function
//  * alignment removed removed from both default and individual cells
//  * added int64 value to the cells, one cell can hold either integer or float but not both 
//
// 03072008, r1.16, ari pikivirta
//  * changed saveworkbook function to not to necessarily need filename as param
//
// 19062008, r1.15, ari pikivirta
//  * fixed bug on findtext function
//  * changed use of lock file (does not cause warning anymore on build)
//
// 03062008, r1.14, ari pikivirta
//  * added FindText function
//
// 16052008, r1.13, ari pikivirta
//  * fixed bug in indexofrow function(s)
//
// 06052008, r1.12, ari pikivirta
//  * changed initialization to not to reset values on custom program startup
//
// 20012008, r1.11, ari pikivirta
//  * rewrote SortByColumn function to much faster than it was (it was doing
//    lots of unnecessary work)
//  * added possibility to compress files
//
// 07012008, r1.10, ari pikivirta
//  * removed individual font setting per record, replaced with index of font record
//    which are managed on font change of the cell
//  * went back to critical sections
//  * old workbook format is load automatically if detected
//  * added possibility to use crypting on open and save (api_strings)
//
// 23052007, r1.09, ari pikivirta
//  * added InsertRow function
//
// 09052007, r1.08, ari pikivirta
//  * removed limitation of amount of columns and rows
//  * added lock file to be used if multiple applications are using the
//    same workbook (wb file time stamp must be checked by other applications
//    before applying any modifications)
//
// 16022007, r1.07, ari pikivirta
//  * added some more code for error checking onto the import functions
//  * bug fixed on setcellvalue (string), resulted always true as function result
//    event there were axception
//
// 21092006, r1.06, ari pikivirta
//  * added getrangemax and getrangemin functions
//  * added deleterow(index) and deletecolumn(index) functions
//
// 26082006, r1.05, ari pikivirta
//  * fixed csv export to file (columns were on separate lines before)
//
// 18082006, r1.04, ari pikivirta
//  * added SwapCells and SortByColumn functions
//  * uses tmultireadexclusivewritesynchronizer instead of critical sections
//
// 15082006, r1.03, ari pikivirta
//  * added new functions: ImportStringGrid, ExportStringListColumn,
//    ExportStringListRow, ImportStringlistColumn, ImportStringlistRow,
//    ImportCSVText
//  * added priority setting to involve also setcellvalue as string also
//  * fixed problem on creating workbook pages runtime (columns was must to
//    define before rows, otherwise rise access violation)
//
// 10072006, r1.02, ari pikivirta
// * fixed bug in export functions with columns and rows swapped
//
// 12102004, r1.01, ari pikivirta
// * added csv export to file and as string with customizable separator
// * added export priority property to be set to text or to value
// * fixed missing value export when exporting to html
//
// 16072004, r1.00, ari pikivirta
// * changed to component
// * added individual cell fonts, colors and alignments
// * added document default font, color, and alignment properties
// * added several "document" properties
// * fixed the not working font styles..
// * modified open and save > now even faster
//
// 15072004, unit, ari pikivirta
// * created
// * basicly all items are working and ready to move into component..
//   just i have to make sure (via demo of course)

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Grids, Graphics, Forms, API_stringgrid;

const
  WB_maxpages = 512;

  WB_lockfile = 'TAPIWORKBOOKLOCK.LOCK';            // lock file for writing
  WB_fileheader_1007 = 'TAPIWORKBOOKHEADER1007';         // base64 header - colors
  WB_fileheader_old = 'TAPIWORKBOOKHEADER1006';         // base64 header
  WB_fileheader_old2 = 'TAPIWORKBOOKHEADER1005';     // base64 header

type
  TWBPriority = (TWB_text, TWB_double);

  TWBCellRecord = record
    text: string;                               // text value
  end;

  TWBSheetRecord = record
    columns: integer;
    rows: integer;
    cell: array of array of TWBCellRecord;
  end;

  TWBWorkbook = record
    lock: tcriticalsection;
    filename: string;
    author: string;
    created: tdatetime;
    editor: string;
    modified: tdatetime;
    currentpage: integer;
    pages: integer;                       // number of pages
    page: array of TWBSheetrecord;        // page contents
  end;

  TAPI_workbook = class(TComponent)
  private
    fversion: string;
    fwb: tWBworkbook;
    fwbpriority: TWBPriority;
    fcol, frow: integer;
    funclock: tcriticalsection;           // lock during function operations

    procedure dummys(s: string);

    procedure setpage ( pageindex: integer );
    function  getpage: integer;

    procedure setpages ( i: integer);
    procedure setcolumns ( i: integer );
    procedure setrows ( i: integer );
    function  getpages: integer;
    function  getcolumns: integer;
    function  getrows: integer;

    function  getdocument: string;
    function  getauthor: string;
    procedure setauthor( s: string );
    function  getcreated: tdatetime;
    procedure setcreated( t: tdatetime );
    function  geteditor: string;
    procedure seteditor( s: string );
    function  getmodified: tdatetime;
    procedure setmodified( t: tdatetime );

    // internal use only functions (to use from lock)
    function  Int_ClearPage(Const pageIndex: integer): boolean;
    function  Int_SetColumns(Const I: Integer): boolean;
    function  Int_SetRows(Const I: Integer): boolean;
    function  Int_SwapCells( fcol, frow, tcol, trow: integer ): boolean;
    function  int_getcolumns: integer;
    function  int_getrows: integer;

  protected

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    function  OpenWorkbook ( filename: string; cryptkey: integer = 0  ): boolean;
    function  OpenWorkbook_O ( filename: string; cryptkey: integer = 0 ): boolean; // load old
    function  OpenWorkbook_O2( filename: string; cryptkey: integer = 0 ): boolean; // load old 2
    function  SaveWorkbook ( filename: string = ''; cryptkey: integer = 0 ): boolean;
    function  SaveWorkbook_O ( filename: string = ''; cryptkey: integer = 0 ): boolean;

    procedure ClearAllPages;
    function  ClearPage ( pageindex: integer ): boolean;
    function  ClearRow(Const ARowIndex: Integer; Const AZeroContent: string): Boolean;

    function  SetCellValue ( value: string ): boolean; overload;
    function  SetCellValue ( value: int64 ): boolean; overload;
    function  SetCellValue ( value: double ): boolean; overload;
    function  SetCellValue ( column, row: integer; value: string ): boolean; overload;
    function  SetCellValue ( column, row: integer; value: double ): boolean; overload;
    function  SetCellValue ( column, row: integer; value: int64 ): boolean; overload;
    function  SetCellValue ( column, row: integer; value: integer ): boolean; overload;

    function  GetCellValueAsString: string; overload;
    function  GetCellValueAsInt: int64; overload;
    function  GetCellValueAsFloat: double; overload;
    function  GetCellValue ( column, row: integer; var value: string ): boolean; overload;
    function  GetCellValue ( column, row: integer; var value: double ): boolean; overload;
    function  GetCellValue ( column, row: integer; var value: int64 ): boolean; overload;
    function  GetCellValue ( column, row: integer; var value: integer ): boolean; overload;

    function  Row: integer;
    function  Col: integer;
    function  FirstRow: integer;
    function  PrevRow: boolean;
    function  NextRow: boolean;
    function  LastRow: integer;
    function  FirstCol: integer;
    function  PrevCol: boolean;
    function  NextCol: boolean;
    function  LastCol: integer;

    function  ExportStringGrid (var sg: tstringgrid): boolean; overload;
    function  ExportStringGrid (var sg: tapi_stringgrid): boolean; overload;
    function  ImportStringGrid (var sg: tstringgrid): boolean;
    function  ExportHTMLFile ( filename: string ): boolean;
    function  ExportHTMLText ( var s: string ): boolean;
    function  ExportCSVFile ( filename: string; separator: char ): boolean;
    function  ExportCSVText ( var s: string; separator: char ): boolean;
    function  ImportCSVText ( s: string; separator: char ): boolean;
    function  ExportStringListColumn (var sl: tstringlist; col: integer ): boolean;
    function  ExportStringListRow (var sl: tstringlist; row: integer ): boolean;
    function  ImportStringlistColumn (sl: tstringlist; col: integer ): boolean;
    function  ImportStringlistRow (sl: tstringlist; row: integer ): boolean;

    function  InsertRow( index: integer ): boolean;
    function  DeleteRow( index: integer ): boolean;
    function  DeleteColumn( index: integer ): boolean;

    function  IndexOfRow( column: integer; value: string; IgnoreCase: boolean = TRUE; StartRow: integer = 0 ): integer; overload;
    function  IndexOfRow( column: integer; value: integer; StartRow: integer = 0 ): integer; overload;
    function  IndexOfRow( column: integer; value: double; StartRow: integer = 0 ): integer; overload;

    function  FindText(TextPartToFind: string; var col, row: integer; ColRangeStart: integer = -1; ColRangeEnd: integer = -1; AIgnoreCase: boolean = TRUE): boolean;
    function  FindMaskedText(MaskedTextToFind: string; var col, row: integer; StartRow: integer = 0; StartCol: integer = 0; EndCol: integer = -1): boolean;

    function  GetRangeAverage ( column, startrow, endrow: integer; var average: double ): boolean;
    function  GetRangeSum ( column, startrow, endrow: integer; var sum: double ): boolean;
    function  GetRangeMin ( column, startrow, endrow: integer; var min: double ): boolean;
    function  GetRangeMax ( column, startrow, endrow: integer; var max: double ): boolean;
    function  SortByColumn (Const AColumn: Integer; Const Ascending: Boolean = TRUE; Const AsDatetimeString: Boolean = FALSE ): Boolean;
    function  SwapCells( fcol, frow, tcol, trow: integer ): boolean;
    function  SwapRows( Const ARow1, ARow2: Integer): Boolean;

    function  FileModified(Const Afilename: string): tdatetime;

  published
    property Version: string read fversion write dummys stored false;
    property Document: string read getdocument write dummys stored false;
    property Author: string read getauthor write setauthor;
    property Created: tdatetime read getcreated write setcreated;
    property Editor: string read geteditor write seteditor;
    property Modified: tdatetime read getmodified write setmodified;
    property Pages: integer read getpages write setpages;
    property Page: integer read getpage write setpage;
    property Columns: integer read getcolumns write setcolumns;
    property Rows: integer read getrows write setrows;
    property Priority: TWBpriority read fwbpriority write fwbpriority;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  api_files, dialogs, api_strings, math;

const
  versioninfostring: string = 'r1.28/ari>at<pikivirta.fi';

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_workbook]);
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_workbook.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;

  fwb.lock:= tcriticalsection.create;
  funclock:= tcriticalsection.create;

  fwbpriority:=       twb_text;
  fwb.filename:=      '';
  fwb.author:=        'nobody';
  fwb.created:=       now;
  fwb.editor:=        'unknown';
  fwb.modified:=      now;

  fcol:=              0;
  frow:=              0; // current row

  setpages(1);
  setpage(0);
  setcolumns(1);
  setrows(0);
  clearallpages;
end;

//------------------------------------------------------------------------------
destructor TAPI_workbook.destroy;
begin
  ClearAllPages;
  fwb.lock.Free;
  funclock.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.Int_ClearPage(Const pageIndex: integer): boolean;
var
  row: integer;
  column: integer;
begin
  result:= FALSE;
  if (pageindex<0) or (pageindex>fwb.pages-1) then exit;
  for row:= 0 to fwb.page[pageindex].rows-1 do
    for column:=0 to fwb.page[pageindex].columns-1 do
    begin
      fwb.page[pageindex].cell[column, row].text:= '';
    end;
  fwb.modified:= now;
  result:= TRUE;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.ClearRow(Const ARowIndex: Integer; Const AZeroContent: string): Boolean;
var
  c: integer;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    if (ARowIndex<0) or (ARowIndex>int_getrows-1) then exit;
    for c:= 0 to int_getcolumns-1 do
    begin
      fwb.page[fwb.currentpage].cell[c, ARowIndex].text:= AZeroContent;
    end;
    fwb.modified:= NOW;
    result:= TRUE;
  finally
    fwb.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.clearallpages;
var
  page: integer;
begin
  fwb.lock.Acquire;
  try
    for page:= 0 to fwb.pages-1 do
      Int_ClearPage(page);
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.clearpage ( pageindex: integer ): boolean;
begin
  fwb.lock.Acquire;
  try
    result:= int_clearpage(pageindex);
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.OpenWorkbook_O2( filename: string; cryptkey: integer = 0 ): boolean;
var
  i: integer;
  j: integer;
  k: integer;
  m: integer;
  pos: integer;
  sl: tstringlist;
begin
  result:=false;
  fwb.lock.Acquire;
  try
    sl:=tstringlist.create;
    try
      try
        // open uncompressed file
        if not api_strings.OpenFileToStrings(FileName, sl) then exit;

        // uncrypt content
        if cryptkey<>0 then sl.text:= string( api_strings.Decrypt(ansistring(sl.text), 12, 45, cryptkey) );

        // header check
        pos:=0;
        if (sl[pos]<>WB_fileheader_old2) then exit; // header differs from defined constant!

        // load contents
        pos:=pos+1; fwb.author:= string(decode64(ansistring(sl[pos])));
        pos:=pos+1; trystrtofloat(sl[pos], double(fwb.created));
        pos:=pos+1; fwb.editor:= string(decode64(ansistring(sl[pos])));
        pos:=pos+1; trystrtofloat(sl[pos], double(fwb.modified));
        pos:=pos+1; trystrtoint(sl[pos], fwb.pages);
        pos:=pos+1; // trystrtoint(sl[pos], i); setalignment(i);
        pos:=pos+1; //trystrtoint(sl[pos], integer(fwb.defaultcolor));
        pos:=pos+1; //trystrtoint(sl[pos], fwb.defaultfont);
        pos:=pos+1; trystrtoint(sl[pos], fwb.currentpage);

        setlength(fwb.page, fwb.pages);
        for i:=0 to (fwb.pages-1) do
        begin
          pos:=pos+1; trystrtoint(sl[pos], m);
          if m<>i then exit; // page number differs from what was expected!
          pos:=pos+1; trystrtoint(sl[pos], fwb.page[i].columns);
          setlength(fwb.page[i].cell, fwb.page[i].columns);
          pos:=pos+1; trystrtoint(sl[pos], fwb.page[i].rows);
          for j:=0 to (fwb.page[i].columns-1) do
          begin
            setlength(fwb.page[i].cell[j], fwb.page[i].rows);
            for k:=0 to (fwb.page[i].rows-1) do
            begin
              pos:=pos+1; //trystrtofloat(sl[pos], fwb.page[i].cell[j,k].value_f);
              pos:=pos+1; fwb.page[i].cell[j,k].text:= string(decode64(ansistring(sl[pos])));
              pos:=pos+1; //trystrtoint64(sl[pos], fwb.page[i].cell[j,k].value_i);
              pos:=pos+1; //trystrtoint(sl[pos], integer(fwb.page[i].cell[j,k].color));
              pos:=pos+1; //trystrtoint(sl[pos], fwb.page[i].cell[j,k].font);
            end;
          end;
        end;

        fwb.filename:= filename;
        result:=true;
      except
      end;
    finally
      freeandnil(sl);
    end;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.OpenWorkbook_O( filename: string; cryptkey: integer = 0 ): boolean;
var
  i, j, k: integer;
  pos: integer;
  sl: tstringlist;

  function GetNextLine: string;
  begin
    result:= sl[pos];
    pos:= pos + 1;
  end;

begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    pos:=0;

    sl:= tstringlist.create;
    try
      try
        // open uncompressed file
        if not api_strings.OpenFileToStrings(FileName, sl) then exit;
        // uncrypt content
        if (cryptkey<>0) then sl.text:= string(api_strings.Decrypt(ansistring(sl.text), 12, 45, cryptkey));
        // header check
        if (GetNextLine<>WB_fileheader_old) then exit; // header differs from defined constant!
        // load contents
        fwb.author:= string(decode64(ansistring(GetNextLine)));
        fwb.created:= strtofloat(GetNextLine);
        fwb.editor:= string(decode64(ansistring(GetNextLine)));
        fwb.modified:= strtofloat(GetNextLine);
        fwb.pages:= strtoint(GetNextLine);
        GetNextLine; //fwb.defaultcolor:= strtoint(GetNextLine);
        fwb.currentpage:= strtoint(GetNextLine);
        setlength(fwb.page, fwb.pages); // set page array size
        for i:=0 to (fwb.pages-1) do // load all pages from file
        begin
          fwb.page[i].columns:= strtoint(GetNextLine);
          fwb.page[i].rows:= strtoint(GetNextLine);
          setlength(fwb.page[i].cell, fwb.page[i].columns); // set page column count
          for j:=0 to (fwb.page[i].columns-1) do
          begin
            setlength(fwb.page[i].cell[j], fwb.page[i].rows); // set page row count for each column
            for k:=0 to (fwb.page[i].rows-1) do
            begin
              fwb.page[i].cell[j,k].text:= string(decode64(ansistring(GetNextLine)));
              GetNextLine; //fwb.page[i].cell[j,k].color:= strtoint(GetNextLine);
            end;
          end;
        end;
        fwb.filename:= filename; // set last opened filename to this just opened
        result:= TRUE; // we managed =)
      except
        // exception while opening workbook
        result:= FALSE;
      end;
    finally
      freeandnil(sl);
    end;

  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.OpenWorkbook( filename: string; cryptkey: integer = 0 ): boolean;
var
  i, j, k: integer;
  pos: integer;
  sl: tstringlist;

  function GetNextLine: string;
  begin
    if pos<sl.count then result:= sl[pos];
    pos:= pos + 1;
  end;

begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    pos:= 0;
    sl:= tstringlist.create;
    try
      try
        if not api_strings.OpenFileToStrings(FileName, sl) then exit;
        if (cryptkey<>0) then sl.text:= string(api_strings.Decrypt(ansistring(sl.text), 12, 45, cryptkey));
        if (GetNextLine<>WB_fileheader_1007) then exit; // header differs from defined constant!
        fwb.author:= string(decode64(ansistring(GetNextLine)));
        fwb.created:= strtofloat(GetNextLine);
        fwb.editor:= string(decode64(ansistring(GetNextLine)));
        fwb.modified:= strtofloat(GetNextLine);
        fwb.pages:= strtoint(GetNextLine);
        fwb.currentpage:= strtoint(GetNextLine);
        setlength(fwb.page, fwb.pages); // set page array size
        for i:=0 to (fwb.pages-1) do // load all pages from file
        begin
          fwb.page[i].columns:= strtoint(GetNextLine);
          fwb.page[i].rows:= strtoint(GetNextLine);
          setlength(fwb.page[i].cell, fwb.page[i].columns); // set page column count
          for j:=0 to (fwb.page[i].columns-1) do
          begin
            setlength(fwb.page[i].cell[j], fwb.page[i].rows); // set page row count for each column
            for k:=0 to (fwb.page[i].rows-1) do
            begin
              fwb.page[i].cell[j,k].text:= string(decode64(ansistring(GetNextLine)));
            end;
          end;
        end;
        fwb.filename:= filename; // set last opened filename to this just opened
        result:= TRUE; // we managed =)
      except
        result:= FALSE;
      end;
    finally
      freeandnil(sl);
    end;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.saveworkbook ( filename: string = ''; cryptkey: integer = 0 ): boolean;
var
  i, j, k: integer;
  sl: tstringlist;
  fname: string;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    // check filename
    if filename<>'' then fname:= filename else fname:= fwb.filename;
    if fname='' then exit; // no name defined
    // create contents to stringlist
    sl:= tstringlist.create;
    try
      sl.clear;
      sl.add(WB_fileheader_1007);                                                    // header
      sl.add(string(encode64(ansistring(fwb.author))));                                             // author
      sl.add(floattostr(fwb.created));                                          // creation date
      sl.add(string(encode64(ansistring(fwb.editor))));                                             // modified by
      sl.add(floattostr(fwb.modified));                                         // modification date
      sl.add(inttostr(fwb.pages));                                              // amount of pages
      sl.add(inttostr(fwb.currentpage));                                        // current page
      for i:=0 to (fwb.pages-1) do
      begin
        sl.add(inttostr(fwb.page[i].columns));                                  // amount of columns
        sl.add(inttostr(fwb.page[i].rows));                                     // amount of rows
        for j:=0 to fwb.page[i].columns-1 do
        begin
          for k:=0 to fwb.page[i].rows-1 do
          begin
            sl.add(string(encode64(ansistring(fwb.page[i].cell[j,k].text))));                       // cell text
          end;
        end;
      end;
      if (cryptkey<>0) then sl.text:= string(api_strings.Encrypt(ansistring(sl.text), 12, 45, cryptkey));
      if api_strings.SaveStringsToFile(FName, sl) then
      begin
        fwb.filename:= fname; // file name is now defined
        fwb.modified:= 0;     // now; should be 0 instead of now! 05102008
        result:= TRUE;        // success on saving!
      end;
    finally
      freeandnil(sl);
    end;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.saveworkbook_O ( filename: string = ''; cryptkey: integer = 0 ): boolean;
var
  i: integer;
  j: integer;
  k: integer;
  sl: tstringlist;
  fname: string;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;

    // check filename
    if filename<>'' then fname:= filename
      else fname:= fwb.filename;
    if fname='' then exit; // no name defined

      // create contents to stringlist
      sl:= tstringlist.create;
      try
        sl.clear;
        sl.add(WB_fileheader_old);                                                    // header
        sl.add(string(encode64(ansistring(fwb.author))));                                             // author
        sl.add(floattostr(fwb.created));                                          // creation date
        sl.add(string(encode64(ansistring(fwb.editor))));                                             // modified by
        sl.add(floattostr(fwb.modified));                                         // modification date
        sl.add(inttostr(fwb.pages));                                              // amount of pages
        sl.add('0'); //inttostr(fwb.defaultcolor));                                       // default color
        sl.add(inttostr(fwb.currentpage));                                        // current page
        for i:=0 to (fwb.pages-1) do
        begin
          sl.add(inttostr(fwb.page[i].columns));                                  // amount of columns
          sl.add(inttostr(fwb.page[i].rows));                                     // amount of rows
          for j:=0 to fwb.page[i].columns-1 do
          begin
            for k:=0 to fwb.page[i].rows-1 do
            begin
              sl.add(string(encode64(ansistring(fwb.page[i].cell[j,k].text))));                       // cell text
              sl.add('0'); //sl.add(inttostr(fwb.page[i].cell[j,k].color));                      // cell color
            end;
          end;
        end;

        // crypt content
        if (cryptkey<>0) then sl.text:= string(api_strings.Encrypt(ansistring(sl.text), 12, 45, cryptkey));

        // save the stringlist
        if api_strings.SaveStringsToFile(FName, sl) then
        begin
          fwb.filename:= fname; // file name is now defined
          fwb.modified:= 0;     // now; should be 0 instead of now! 05102008
          result:= TRUE;        // success on saving!
        end;

      finally
        freeandnil(sl);
      end;

  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.FileModified(Const Afilename: string): tdatetime;
begin
  result:= api_files.FileDateTime(Afilename);
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.setpages ( i: integer );
var
  j: integer;
begin
  fwb.lock.Acquire;
  try
    if (i<0) then i:= 0;
    if (i>WB_maxpages-1) or (i=fwb.pages) then exit;

    // set new pages amount
    fwb.pages:= i;
    setlength( fwb.page, fwb.pages );

    // set new column and row amount
    for j:=0 to fwb.pages-1 do
    begin
      Int_SetRows(fwb.page[j].rows);
      Int_SetColumns(fwb.page[j].columns);
    end;

    // moved after creating new pages 02112009
    for j:=i to fwb.pages-1 do clearpage(j);

    if (fwb.currentpage>i-1) then fwb.currentpage:= i-1; // use last index
    if (fwb.currentpage<0) and (i>0) then fwb.currentpage:= 0; // index to zero

    if (fwb.created<1) then fwb.created:= now; // set created time
    fwb.modified:= now;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.Int_SetColumns(Const I: Integer): boolean;
var
  j: integer;
begin
  result:= false;
  if (fwb.currentpage<0)
    or (fwb.currentpage>fwb.pages-1)
    or (fwb.pages<1)
    or (i<0) or (i=fwb.page[fwb.currentpage].columns)
    then exit;
  // free leaving rows (length to zero)
  for j:=i+1 to fwb.page[fwb.currentpage].columns-1 do
    setlength(fwb.page[fwb.currentpage].cell, 0);
  // set new columns
  setlength( fwb.page[fwb.currentpage].cell, i );
  // reserve memory for new rows
  for j:=fwb.page[ fwb.currentpage ].columns to i-1 do
    setlength(fwb.page[fwb.currentpage].cell[j], fwb.page[fwb.currentpage].rows);
  // store new column count
  fwb.page[ fwb.currentpage ].columns:= i;
  fwb.modified:= now;
  result:= TRUE;
end;

procedure TAPI_workbook.setcolumns ( i: integer );
begin
  fwb.lock.Acquire;
  try
    Int_setcolumns(i);
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_workbook.Int_SetRows(Const I: Integer): boolean;
var
  j: integer;
begin
  result:= false;
  if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1) or (fwb.pages<1)
    or (i=int_getrows)
    or (int_getcolumns<1) // no columns (y axis)
    or (i<0)
    then exit;
  for j:=0 to int_getcolumns-1 do
    setlength( fwb.page[fwb.currentpage].cell[j], i );
  fwb.page[fwb.currentpage].rows:= i;
  fwb.modified:= now;
  result:= TRUE;
end;

procedure TAPI_workbook.setrows ( i: integer );
begin
  fwb.lock.Acquire;
  try
    Int_SetRows(i);
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.Int_SwapCells( fcol, frow, tcol, trow: integer ): boolean;
var
  rec: TWBCellRecord;
begin
  // called inside lock!
  result:= FALSE;
  if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
    or (fcol<0) or (fcol>int_getcolumns-1)
    or (frow<0) or (frow>int_getrows-1)
    or (tcol<0) or (tcol>int_getcolumns-1)
    or (trow<0) or (trow>int_getrows-1)
    then exit;
  rec:= fwb.page[ fwb.currentpage ].cell[fcol, frow];
  fwb.page[ fwb.currentpage ].cell[fcol, frow]:= fwb.page[ fwb.currentpage ].cell[tcol, trow];
  fwb.page[ fwb.currentpage ].cell[tcol, trow]:= rec;
  fwb.modified:= now;
  result:= TRUE;
end;

function TAPI_workbook.SwapCells( fcol, frow, tcol, trow: integer ): boolean;
begin
  fwb.lock.Acquire;
  try
    result:= int_swapcells(fcol, frow, tcol, trow);
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.SwapRows( Const ARow1, ARow2: Integer): Boolean;
var
  k: integer;
  rec: TWBCellRecord;
begin
  fwb.lock.acquire;
  try
    for k:=0 to int_getcolumns-1 do
    begin
      rec:= fwb.page[ fwb.currentpage ].cell[k, ARow1];
      fwb.page[ fwb.currentpage ].cell[k, ARow1]:= fwb.page[ fwb.currentpage ].cell[k, ARow2];
      fwb.page[ fwb.currentpage ].cell[k, ARow2]:= rec;
    end;
    fwb.modified:= now;
    result:= TRUE;
  finally
    fwb.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.SortByColumn (Const AColumn: Integer; Const Ascending: Boolean = TRUE; Const AsDatetimeString: Boolean = FALSE ): Boolean;
var
  i, j: integer;
  value1, value2: double;
  text1, text2: string;
begin
  funclock.acquire;
  try
    result:= FALSE;
    if (Acolumn<0) or (Acolumn>getcolumns-1) then exit;
    //
    i:= 0;
    while (i<getrows) do
    begin
      getcellvalue(Acolumn, i, text1); // as string
      getcellvalue(Acolumn, i, value1); // as float
      j:= 0;
      while (j<i) do
      begin
        getcellvalue(Acolumn, j, text2); // as string
        getcellvalue(Acolumn, j, value2); // as float
        //
        if (AsDateTimeString) and (trystrtodatetime(text1, tdatetime(value1))) and (trystrtodatetime(text2, tdatetime(value2))) then
        begin
          text1:= FloatToStr(value1); // convert float to string for compare
          text2:= FloatToStr(value2); // convert float to string for compare
        end;
        //
        if ((ascending) and (text1<text2)) or ((not ascending) and (text1>text2)) then
        begin
          SwapRows(i, j);
        end;
        j:= j + 1; // increase j position (compare to row)
      end;
      i:= i + 1; // increase i position (source row)
    end;
    //
    // operation completed
    result:= TRUE;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.FindText(TextPartToFind: string; var col, row: integer; ColRangeStart: integer = -1; ColRangeEnd: integer = -1; AIgnoreCase: boolean = TRUE): boolean;
var
  c, r, cr1, cr2: integer;
  t: string;
begin
  funclock.acquire;
  try
    col:= -1;
    row:= -1;
    result:= false;
    if colrangestart>-1 then cr1:= colrangestart else cr1:= 0;
    if colrangeend>-1 then cr2:= colrangeend else cr2:= self.columns;
    //
    c:= cr1;
    while (c<cr2) do
    begin
      r:= 0;
      while (r<rows) do
      begin
        if getcellvalue(c, r, t) then
          if (pos(ansistring(TextPartToFind), ansistring(t), AIgnoreCase)>0) then
          begin
            col:= c;
            row:= r;
            result:= TRUE;
            exit;
          end;
        r:= r + 1;
      end;
      c:= c + 1;
    end;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Workbook.FindMaskedText(MaskedTextToFind: string; var col, row: integer; StartRow: integer = 0; StartCol: integer = 0; EndCol: integer = -1): boolean;
var
  ec: integer;
  c, r: integer;
  temps: string;
begin
  funclock.acquire;
  try
    // range check
    if endcol<0 then ec:= columns else ec:= endcol;
    if endcol>columns-1 then ec:= endcol;
    //
    col:= -1;
    row:= -1;
    result:= false;
    //
    // start find loop
    c:= Startcol;
    while (c<ec) do
    begin
      r:= startrow;
      while r<rows do
      begin
        if getcellvalue(c, r, temps) then
          if api_strings.match(ansistring(maskedtexttofind), ansistring(temps)) then
          begin
            col:= c;
            row:= r;
            result:= true;
            exit;
          end;
        r:= r + 1;
      end;
      c:= c + 1;
    end;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.InsertRow( index: integer ): boolean;
var
  r: integer;
begin
  funclock.acquire;
  try
    result:= FALSE;
    if (index>-1) then
    begin
      // add new row to the end
      rows:= rows + 1;
      // set beginning
      r:= (rows-1);
      // move all one pos up from index
      while (r>index+1) do
      begin
        SwapRows(r, r-1);
        r:= r - 1;
      end;
      // clear all columns on inserted?
      ClearRow(index, '');
      //
      result:= TRUE;
    end;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.DeleteRow( index: integer ): boolean;
var
  r: integer;
begin
  funclock.acquire;
  try
    result:= FALSE;
    if (index>-1) and (index<rows) then
    begin
      // move all one pos down
      r:= index;
      while (r<rows-1) do
      begin
        SwapRows(r, r+1);
        r:= r + 1;
      end;
      // decrease rows
      rows:= rows - 1;
      result:= TRUE;
    end;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
// 16.2.2007, ari pikivirta
//  * added true as function result (was always false earlier)
function TAPI_workbook.DeleteColumn( index: integer ): boolean;
var
  r,c: integer;
begin
  funclock.acquire;
  try
    result:= false;
    if (index>-1) and (index<columns) then
    begin
      // move all one pos down
      c:= index;
      while c<columns-1 do
      begin
        r:= 0;
        while r<rows do
        begin
          SwapCells(c,r,c+1,r);
          r:= r + 1;
        end;
        c:= c + 1;
      end;
      // decrease columns
      columns:= columns - 1;
      result:= TRUE;
    end;
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getpages: integer;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.pages;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.int_getcolumns: integer;
begin
  if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1) then
  begin
    result:= -1;
  end else
    result:= fwb.page[ fwb.currentpage ].columns;
end;

function TAPI_workbook.getcolumns: integer;
begin
  fwb.lock.Acquire;
  try
    result:= int_getcolumns;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.int_getrows: integer;
begin
  if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1) then
  begin
    result:= -1;
  end else
    result:= fwb.page[ fwb.currentpage ].Rows;
end;

function TAPI_workbook.getrows: integer;
begin
  fwb.lock.Acquire;
  try
    result:= int_getrows;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.setpage ( pageindex: integer );
begin
  fwb.lock.Acquire;
  try
    if (pageindex<0) or (pageindex>fwb.pages-1) then exit;
    fwb.currentpage:= pageindex;
    //fwb.modified:= now; 02112009
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getpage: integer;
begin
  result:= fwb.currentpage;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.setcellvalue ( column, row: integer; value: string ): boolean;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (column<0) or (column>int_getcolumns-1)
      or (row<0) or (row>int_getrows-1)
      then exit;
    fwb.page[fwb.currentpage].cell[column, row].text:= value;
    fwb.modified:= now;
    result:= TRUE;
  finally
    fwb.lock.Release;
  end;
end;

function TAPI_workbook.setcellvalue (column, row: integer; value: int64): boolean;
begin
  result:= setcellvalue(column, row, inttostr(value))
end;

function TAPI_workbook.setcellvalue (column, row: integer; value: integer): boolean;
begin
  result:= setcellvalue(column, row, inttostr(value));
end;

function TAPI_workbook.setcellvalue ( column, row: integer; value: double ): boolean;
begin
  result:= setcellvalue(column, row, floattostr(value));
end;

function TAPI_workbook.SetCellValue(value: string): boolean;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (fcol<0) or (fcol>int_getcolumns-1)
      or (frow<0) or (frow>int_getrows-1)
      then exit;
    fwb.page[ fwb.currentpage ].cell[fcol, frow].text := value;
    fwb.modified:= now;
    result:= TRUE;
  finally
    fwb.lock.Release;
  end;
end;

function TAPI_workbook.SetCellValue(value: Int64): boolean;
begin
  result:= SetCellValue(inttostr(value));
end;

function TAPI_workbook.SetCellValue(value: Double): boolean;
begin
  result:= SetCellValue(floattostr(value));
end;

//------------------------------------------------------------------------------

function TAPI_workbook.getcellvalue ( column, row: integer; var value: string ): boolean;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (column<0) or (column>int_getcolumns-1)
      or (row<0) or (row>int_getrows-1)
      then exit;
    value:= fwb.page[fwb.currentpage].cell[column, row].text;
    result:= TRUE;
  finally
    fwb.lock.Release;
  end;
end;

function TAPI_workbook.getcellvalue ( column, row: integer; var value: int64 ): boolean;
var
  temps: string;
begin
  if not getcellvalue( column, row, temps ) then result:= FALSE
    else result:= trystrtoint64(temps, value);
  if not result then value:= 0;
end;

function TAPI_workbook.getcellvalue ( column, row: integer; var value: integer ): boolean;
var
  temps: string;
begin
  if not getcellvalue(column, row, temps) then result:= FALSE
    else result:= trystrtoint(temps, value);
  if not result then value:= 0;
end;

function TAPI_workbook.getcellvalue ( column, row: integer; var value: double ): boolean;
var
  temps: string;
begin
  if not getcellvalue(column, row, temps) then result:= FALSE
    else result:= trystrtofloat(temps, value);
  if not result then value:= 0;
end;

function TAPI_workbook.GetCellValueAsString: string;
begin
  fwb.lock.Acquire;
  try
    result:= '';
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (fcol<0) or (fcol>int_getcolumns-1)
      or (frow<0) or (frow>int_getrows-1)
      then exit;
    result:= fwb.page[ fwb.currentpage ].cell[fcol, frow].text;
  finally
    fwb.lock.Release;
  end;
end;

function TAPI_workbook.GetCellValueAsInt: int64;
begin
  if not trystrtoint64(getcellvalueasstring, result) then result:= 0;
end;

function TAPI_workbook.GetCellValueAsFloat: double;
begin
  if not trystrtofloat(getcellvalueasstring, result) then result:= 0;
end;

//------------------------------------------------------------------------------
function  TAPI_workbook.Row: integer;
begin
  fwb.lock.Acquire;
  try
    result:= frow;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.Col: integer;
begin
  fwb.lock.Acquire;
  try
    result:= fcol;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.FirstRow: integer;
begin
  fwb.lock.Acquire;
  try
    frow:= 0;
    result:= frow;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.PrevRow: boolean;
begin
  fwb.lock.Acquire;
  try
    if frow>0 then
    begin
      frow:= frow - 1;
      result:= true;
    end else
      result:= false;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.NextRow: boolean;
begin
  fwb.lock.Acquire;
  try
    if frow<fwb.page[fwb.currentpage].rows-1 then
    begin
      frow:= frow + 1;
      result:= true;
    end else
      result:= FALSE;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.LastRow: integer;
begin
  fwb.lock.Acquire;
  try
    frow:= fwb.page[fwb.currentpage].rows-1;
    result:= frow;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.FirstCol: integer;
begin
  fwb.lock.Acquire;
  try
    fcol:= 0;
    result:= fcol;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.PrevCol: boolean;
begin
  fwb.lock.Acquire;
  try
    if fcol>0 then
    begin
      fcol:= fcol - 1;
      result:= TRUE;
    end else
      result:= false;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.NextCol: boolean;
begin
  fwb.lock.Acquire;
  try
    if fcol<fwb.page[fwb.currentpage].columns-1 then
    begin
      fcol:= fcol + 1;
      result:= true;
    end else
      result:= false;
  finally
    fwb.lock.releasE;
  end;
end;

function  TAPI_workbook.LastCol: integer;
begin
  fwb.lock.Acquire;
  try
    fcol:= fwb.page[fwb.currentpage].columns-1;
    result:= fcol;
  finally
    fwb.lock.releasE;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.IndexOfRow( column: integer; value: string; IgnoreCase: boolean = TRUE; StartRow: integer = 0 ): integer;
var
  i: integer;
  temp: string;
begin
  result:= -1;
  i:= startrow;
  while (i<rows) do
  begin
    if getcellvalue( column, i, temp ) then
      if pos(ansistring(value), ansistring(temp), IgnoreCase)>0 then
      begin
        result:= i;
        break;
      end;
    i:= i + 1;
  end;
end;

function TAPI_workbook.IndexOfRow( column: integer; value: integer; StartRow: integer = 0 ): integer;
begin
  result:= IndexOfRow( column, inttostr(value) );
end;

function TAPI_workbook.IndexOfRow( column: integer; value: double; StartRow: integer = 0 ): integer;
begin
  result:= IndexOfRow( column, floattostr(value) );
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exportstringgrid(var sg: tstringgrid): boolean;
var
  i: integer;
  j: integer;
  text: string;
begin
  result:= FALSE;
  if not assigned(sg) then exit;
  //
  sg.RowCount:= sg.FixedRows + getrows;
  sg.ColCount:= sg.FixedCols + getcolumns;
  //
  i:= 0;
  while i<columns do
  begin
    j:= 0;
    while j<rows do
    begin
      getcellvalue(i,j,text);
      sg.Cells[i+sg.FixedCols,j+sg.FixedRows]:= text;
      j:= j + 1;
    end;
    i:= i + 1;
  end;
  //
  result:= TRUE;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exportstringgrid (var sg: tapi_stringgrid): boolean;
var
  i: integer;
  j: integer;
  text: string;
  value: double;
begin
  result:=false;
  if not assigned(sg) then exit;
  //
  sg.RowCount:= sg.FixedRows + rows;
  sg.ColCount:= sg.FixedCols + columns;
  i:= 0;
  while i<columns do
  begin
    j:= 0;
    while j<rows do
    begin
      getcellvalue(i,j,value);
      getcellvalue(i,j,text);
      if fwbpriority = twb_text then
      begin
        if (text<>'') then sg.Cells[i+sg.FixedCols,j+sg.FixedRows]:= text
          else sg.cells[i+sg.fixedcols,j+sg.FixedRows]:= floattostr(value);
      end else
      begin
        if (value<>0) or (text='') then sg.cells[i+sg.fixedcols,j+sg.FixedRows]:= floattostr(value)
          else sg.Cells[i+sg.FixedCols,j+sg.FixedRows]:= text;
      end;
      j:= j + 1;
    end;
    i:= i + 1;
  end;
  result:=true;
end;

//------------------------------------------------------------------------------
// 16.2.2007, ari pikivirta
//  * added setcellvalue checking, will exit with function result false if
//    there any problems occur
function TAPI_workbook.ImportStringGrid (var sg: tstringgrid): boolean;
var
  i, j: integer;
begin
  result:= false;
  if not assigned(sg) then exit;
  setrows( sg.FixedRows + sg.RowCount );
  setcolumns( sg.FixedCols + sg.ColCount );
  // fixed things
  for i:=0 to sg.FixedCols-1 do
    for j:=0 to sg.rowcount-1 do
      if not setcellvalue(i,j,sg.Cells[i,j]) then
        exit;
  // fixed things
  for i:=0 to sg.Colcount-1 do
    for j:=0 to sg.FixedRows-1 do
      if not setcellvalue(i,j,sg.Cells[i,j]) then
        exit;
  // values
  for i:=sg.FixedCols to sg.colcount-1 do
    for j:=sg.FixedRows to sg.rowcount-1 do
      if not setcellvalue(i,j,sg.Cells[i,j]) then
        exit;
  result:= TRUE;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exporthtmlfile ( filename: string ): boolean;
var
  i: integer;
  j: integer;
  value: double;
  text: string;
  f: textfile;
begin
  result:=false;
  {$i-}
  assignfile(f, filename);
  rewrite(f);
  {$i+}
  if ioresult=0 then
  begin
    writeln(f, '<html>');
    writeln(f, '<body background=#cccccc>');
    writeln(f, '<table width=100% border=0>');
    j:= 0;
    while j<rows do
    begin
      writeln(f, '<tr>');
      i:= 0;
      while i<columns do
      begin
        getcellvalue(i,j,value);
        getcellvalue(i,j,text);
        if fwbpriority = twb_text then
        begin
          if (text<>'') then writeln(f,'<td bgcolor=#ffffff><font size=2 color=#000000>' + text + '</td>')
            else writeln(f,'<td bgcolor=#ffffff><font size=2 color=#000000>' + floattostr(value) + '</td>');
        end else
        begin
          if (value<>0) or (text='') then writeln(f,'<td bgcolor=#ffffff><font size=2 color=#000000>' + floattostr(value) + '</td>')
            else writeln(f,'<td bgcolor=#ffffff><font size=2 color=#000000>' + text + '</td>');
        end;
        i:= i + 1;
      end;
      writeln(f, '</tr>');
      j:= j + 1;
    end;
    writeln(f, '</table>');
    writeln(f, '</body>');
    writeln(f, '</html>');
    closefile(f);
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exporthtmltext (var s: string): boolean; // this is always true
var
  i: integer;
  j: integer;
  value: double;
  text: string;
begin
  s:='<html>'+#13#10
    +'<body background=#cccccc>'+#13#10
    +'<table width=100% border=0>'+#13#10;
  j:= 0;
  while j<rows do
  begin
    s:=s+'<tr>'+#13#10;
    i:= 0;
    while i<columns do
    begin
      getcellvalue(i,j,value);
      getcellvalue(i,j,text);
      if fwbpriority = twb_text then
      begin
        if (text<>'') then s:=s+'<td width=100% bgcolor=#ffffff><font size=2 color=#000000>' + text + '</td>'
          else s:=s+'<td width=100% bgcolor=#ffffff><font size=2 color=#000000>' + floattostr(value) + '</td>';
      end else
      begin
        if (value<>0) or (text='') then s:=s+'<td width=100% bgcolor=#ffffff><font size=2 color=#000000>' + floattostr(value) + '</td>'
          else s:=s+'<td width=100% bgcolor=#ffffff><font size=2 color=#000000>' + text + '</td>';
      end;
      i:= i + 1;
    end;
    s:=s+'</tr>'+#13#10;
    j:= j + 1;
  end;
  s:=s+'</table>'+#13#10
    +'</body>'+#13#10
    +'</html>';
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exportcsvfile ( filename: string; separator: char ): boolean;
var
  i: integer;
  j: integer;
  value: double;
  text: string;
  f: textfile;
begin
  result:= false;
  {$i-}
  assignfile( f, filename );
  rewrite( f );
  {$i+}
  if ioresult=0 then
  begin
    j:= 0;
    while j<rows do
    begin
      i:= 0;
      while i<columns do
      begin
        getcellvalue(i,j,value);
        getcellvalue(i,j,text);
        if fwbpriority = twb_text then
        begin
          if (text<>'') then write(f, text + separator)
            else write(f, floattostr(value) + separator);
        end else
        begin
          if (value<>0) or (text='') then write(f, floattostr(value) + separator)
            else write(f, text + separator);
        end;
        i:= i + 1;
      end;
      writeln(f);
      j:= j + 1;
    end;
    closefile( f );
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.exportcsvtext ( var s: string; separator: char ): boolean;
var
  i: integer;
  j: integer;
  value: double;
  text: string;
begin
  s:='';
  j:= 0;
  while j<rows do
  begin
    i:= 0;
    while i<columns do
    begin
      getcellvalue(i,j,value);
      getcellvalue(i,j,text);
      if fwbpriority = twb_text then
      begin
        if (text<>'') then s:=s+text + separator
          else s:=s+ floattostr(value) + separator;
      end else
      begin
        if (value<>0) or (text='') then s:=s+ floattostr(value) + separator
          else s:=s+ text + separator;
      end;
      i:= i + 1;
    end;
    s:= s + #13#10; //CRLF anyway on end of line
    j:= j + 1;
  end;
  result:= true;
end;

//------------------------------------------------------------------------------
// 16.2.2007, ari pikivirta
//  * added setcell return value checking, will exit with function result
//    false if any problems occur
function TAPI_workbook.ImportCSVText ( s: string; separator: char ): boolean;
var
  col: integer;
  p: integer;
  tempdoc: ansistring;
  line: ansistring;
  value: string;
begin
  result:= false;
  setcolumns(0);                            // set current page columns to zero
  setrows(0);                               // set page rows to zero
  tempdoc:= ansistring(s);                  // copy string to tempdoc
  while (tempdoc<>'') do                    // while tempdoc is not empty
  begin
    p:= pos(#13#10, tempdoc);               // try to find CRLNs
    if p>0 then                             // great! found one..
    begin
      setrows( getrows + 1 );               // add new row
      col:= 0;
      line:= copy(tempdoc,1,p-1);           // get new line contents
      delete(tempdoc,1,p+1);                // delete line from tempdoc
      p:= pos(ansichar(separator), line);             // get first separator position
      while p>0 do
      begin
        col:= col + 1;                      // count next column
        if getcolumns<col then              // if more columns needed
          setcolumns(col);                  // increase column count
        value:= string(copy(line,1,p-1));           // copy value from line
        delete(line,1,p);                   // delete line up to separator
        if not setcellvalue(col-1, getrows-1, value) then    // set cell value
          exit;
        p:= pos(ansistring(separator), line);           // get first separator position
      end;
    end else                                // no CRLNs found
      exit;                                 // exit whole import
  end;
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.ExportStringlistColumn (var sl: tstringlist; col: integer ): boolean;
var
  cols,rows: integer;
  j: integer;
  value: double;
  text: string;
begin
  result:= false;
  cols:= getcolumns;
  rows:= getrows;
  if (col>-1) and (col<cols) and (assigned(sl)) then
  begin
    sl.BeginUpdate;
    try
      sl.Clear;
      j:= 0;
      while j<rows do
      begin
        getcellvalue(col,j,value);
        getcellvalue(col,j,text);
        if fwbpriority = twb_text then
        begin
          if (text<>'') then sl.add(text)
            else sl.add(floattostr(value));
        end else
        begin
          if (value<>0) or (text='') then sl.add(floattostr(value))
            else sl.Add(text);
        end;
        j:= j + 1;
      end;
    finally
      sl.EndUpdate;
    end;
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.ExportStringlistRow (var sl: tstringlist; row: integer ): boolean;
var
  rows, cols: integer;
  i: integer;
  value: double;
  text: string;
begin
  result:= false;
  rows:= getrows;
  cols:= getcolumns;
  if (row>-1) and (row<rows) and (assigned(sl)) then
  begin
    sl.BeginUpdate;
    try
      sl.Clear;
      i:= 0;
      while i<cols do
      begin
        getcellvalue(i,row,value);
        getcellvalue(i,row,text);
        if fwbpriority = twb_text then
        begin
          if (text<>'') then sl.add(text)
            else sl.add(floattostr(value));
        end else
        begin
          if (value<>0) or (text='') then sl.add(floattostr(value))
            else sl.Add(text);
        end;
        i:= i + 1;
      end;
    finally
      sl.EndUpdate;
    end;
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
// 16.2.2007, ari pikivirta
//  * added setcell return value checking, will result false if problems occur
function TAPI_workbook.ImportStringlistColumn (sl: tstringlist; col: integer ): boolean;
var
  rows, cols: integer;
  j: integer;
begin
  result:= FALSE;
  rows:= getrows;
  cols:= getcolumns;
  if (col>-1) and (col<cols) and (assigned(sl)) then
  begin
    if (sl.count>rows) then                               // if there is not anough rows
      setrows(sl.count);                                  // increase row count
    for j:=0 to sl.count-1 do                             // go trough all rows
      if not setcellvalue(col,j,sl[j]) then               // add value to the wb cell
        exit;
    result:= TRUE;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.ImportStringlistRow (sl: tstringlist; row: integer ): boolean;
var
  rows, cols: integer;
  j: integer;
begin
  result:= false;
  rows:= getrows;
  cols:= getcolumns;
  if (row>-1) and (row<rows) and (assigned(sl)) then
  begin
    if (sl.count>cols) then                               // if there is not anough rows
      setcolumns(sl.count);                               // try to increase row count
    for j:=0 to sl.count-1 do                             // go trough all rows
      setcellvalue(j,row,sl[j]);                          // add value to the wb cell
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getrangeaverage ( column, startrow, endrow: integer; var average: double ): boolean;
var
  count: integer;
  total, temp: double;
  i: integer;
begin
  fwb.lock.Acquire;
  try
    result:= FALSE;
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (column<0) or (column>int_getcolumns-1)
      or (startrow<0) or (startrow>int_getrows-1)
      or (endrow<0) or (endrow>int_getrows-1)
      or (startrow>endrow)
      then exit;
    total:=0;
    count:=0;
    for i:=startrow to endrow do
    begin
      trystrtofloat( fwb.page[ page ].cell[ column, i ].text, temp);
      total:= total + temp;
      count:= count + 1;
    end;
    if (total<>0) and (count<>0) then average:= total / count
      else average:=0;
    result:=true;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
// 18052009, ari pikivirta
//  * uses min(double) instead of written number
//  * while loop instead of for..
function TAPI_workbook.getrangesum ( column, startrow, endrow: integer; var sum: double ): boolean;
var
  i: integer;
  temp: double;
begin
  fwb.lock.Acquire;
  try
    result:=false;
    if (fwb.currentpage<0) or (fwb.currentpage>fwb.pages-1)
      or (column<0) or (column>int_getcolumns-1)
      or (startrow<0) or (startrow>int_getrows-1)
      or (endrow<0) or (endrow>int_getrows-1)
      or (startrow>endrow)
      then exit;
    sum:=0;
    i:= startrow;
    while i<endrow do
    begin
      trystrtofloat(fwb.page[ page ].cell[ column, i ].text, temp);
      sum:=sum + temp;
      i:= i + 1;
    end;
    result:= TRUE;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
// 18052009, ari pikivirta
//  * uses min(double) instead of written number
//  * while loop instead of for..
function TAPI_workbook.GetRangeMin ( column, startrow, endrow: integer; var min: double ): boolean;
const
  SVAL = MAXDOUBLE; //max(double);
var
  value: double;
  i: integer;
begin
  funclock.acquire;
  try
    min:= SVAL;
    for i:=startrow to endrow do
    begin
      getcellvalue(column, i, value);
      if value<min then min:= value;
    end;
    result:= (min<>SVAL);
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
// 18052009, ari pikivirta
//  * uses min(double) instead of written number
//  * while loop instead of for..
function TAPI_workbook.GetRangeMax ( column, startrow, endrow: integer; var max: double ): boolean;
const
  SVAL = MINDOUBLE; //min(double);
var
  value: double;
  i: integer;
begin
  funclock.acquire;
  try
    max:= SVAL;
    i:= startrow;
    while i<endrow do
    begin
      getcellvalue(column, i, value);
      if value>max then max:= value;
      i:= i + 1;
    end;
    result:= (max<>SVAL);
  finally
    funclock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getdocument: string;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.filename;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getauthor: string;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.author;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.setauthor( s: string );
begin
  fwb.lock.Acquire;
  try
    fwb.author:= s;
    fwb.modified:= now;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getcreated: tdatetime;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.created;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.setcreated( t: tdatetime );
begin
  fwb.lock.Acquire;
  try
    fwb.created:= t;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.geteditor: string;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.editor;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.seteditor( s: string );
begin
  fwb.lock.Acquire;
  try
    fwb.editor:= s;
    fwb.modified:= now;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_workbook.getmodified: tdatetime;
begin
  fwb.lock.Acquire;
  try
    result:= fwb.modified;
  finally
    fwb.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_workbook.setmodified( t: tdatetime );
begin
  fwb.lock.Acquire;
  try
    fwb.modified:= t;
  finally
    fwb.lock.Release;
  end;
end;

end.
