unit API_msexcel;

//------------------------------------------------------------------------------
// excel ole automation made easy
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
// r1.05, 28052007, ari pikivirta
//  * added GetCellText function
//
// r1.04, 22052007, ari pikivirta
//  * ExportToCsv function added (to text)
//  * added LocateLastRow
//  * added ExportToCsvFile
//
// r1.03, 24082006, ari pikivirta
//  * openworkbook returned always false, now fixed
//
// r1.02, ari pikivirta
// * made fexcel variant (object) public - so all rest of the functions are available
//   that are currently missing from the component :)
// * separated display alerts parameter as a function (to avoid replace all things
//   in saving file
// * rename sheet now only changes the name of active sheet
//
// r1.01, ari pikivirta
// * changed excel close handle to also use try..except
// * added variable assigned checking to all routines implemented

interface

uses
  SysUtils, Classes, Variants;

const
  xlCSV             = 6;
  xlExcel9795       = 43;
  xlHTML            = 44;
  xlTextMSDOS       = 21;
  xlTextWindows     = 20;
  xlUnicodeText     = 42;
  xlWorkbookNormal  = -4143;

type
  TAPIexcelevent = procedure (sender: tobject; const errortext: string) of object;

  TAPI_msexcel = class(TComponent)
  private
    fversion: string;
    factive: boolean;
    ferror: string;
    fonerror: tapiexcelevent;
    procedure dummys(s: string);
    procedure dummyb(b: boolean);

  protected
  public
    fexcel: variant;

    constructor Create ( aowner: tcomponent ); override;
    destructor Destroy; override;

    function Open: boolean;
    function Close: boolean;

    function ExcelVersion: string;
    function SetVisible(IsVisible: Boolean): boolean;
    function SetDisplayAlerts( IsVisible: boolean ): boolean;
    function AddNewWorkBook: boolean;
    function OpenWorkBook(FileName : String): boolean;
    function CloseWorkBooks(SaveAll: Boolean): boolean;
    function SaveWorkBookAs(Filename: string): boolean;
    function SaveActiveWorkBookAs(Filename: string): boolean;

    function GetActiveSheetName: string;
    function AddWorkSheet: boolean;
    function DeleteWorkSheet(SheetName : string): boolean;
    function RenameSheet(NewName: string): boolean;
    function SelectSheetByName(SheetName: String): boolean;

    function SelectCell(RowNum, ColNum: Integer): boolean;

    function GetCellText(RowNum, ColNum: Integer): string;
    function GetCellValue(RowNum, ColNum: Integer): string; overload;
    function GetCellValue: string; overload;

    function SetCellValue(RowNum, ColNum: Integer; Value : string): boolean; overload;
    function SetCellValue(Value: string): boolean; overload;
    function SetCellValue(Value: integer): boolean; overload;
    function SetCellValue(Value: double): boolean; overload;
    function GetCellFormula(RowNum, ColNum: Integer): string;
    function SetCellFormula(FormulaString : string; RowNum, ColNum: Integer): boolean;

    //function GetRange(RowNum, ColNum, RowCount, ColCount: integer): tstringlist;
    //function SetRange(RowNum, ColNum: integer; list: Tstringlist): boolean;

    function GetRow: integer;
    function GetCol: integer;
    function SetRowHeight(RowNum, RowHeight: Double): boolean;
    function SetColumnWidth(ColNum, ColumnWidth: Integer): boolean;
    function SetDisplayGridLines(Visible: boolean): boolean;

    function LocateLastRow( column: integer): integer;
    function ExportToCsv(var csvtext: string; const separator: string ): boolean;
    function ExportToCsvFile( const filename, separator: string ): boolean;

  published
    property Version: string read fversion write dummys stored false;
    property Active: boolean read factive write dummyb stored false;
    property Error: string read ferror write ferror stored false;
    property OnError: tapiexcelevent read fonerror write fonerror;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  comobj;

const
  versioninfostring: string = 'r1.05/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
procedure TAPI_msexcel.dummys(s: string); begin end;
procedure TAPI_msexcel.dummyb(b: boolean); begin end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_msexcel]);
end;

//------------------------------------------------------------------------------
constructor TAPI_msexcel.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  ferror:= '';
  factive:= false;
  fexcel:= unassigned;
end;

//------------------------------------------------------------------------------
destructor TAPI_msexcel.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.Open: boolean;
begin
  result:=false;
  if varisempty(fexcel) then
  begin
    try
      fexcel:= GetActiveOleObject('Excel.Application');
      result:=True;
    except
    end;

    if not result then
    try
      fexcel:= CreateOleObject('Excel.Application');
      result:=true;
    except
    end;

    if not result then
    begin
      fexcel:=unassigned;
      ferror:='Unable to get Excel object handle';
      if assigned(fonerror) then
        fonerror(self, ferror);
    end;

    // set active state
    factive:= result;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.AddNewWorkBook: boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.workbooks.add;
    result:=True;
  except
    ferror:='Failed to add new WorkBook';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetDisplayAlerts( IsVisible: boolean ): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.DisplayAlerts := isvisible;
    result:=true;
  except
    ferror:='Failed to set display alerts paremeter';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.Close: boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  begin
    setdisplayalerts(false);
    try
      fexcel.Quit;
      fexcel:= unassigned;
      result:=true;
    except
      ferror:='failed to close excel handle';
      if assigned(fonerror) then
        fonerror(self, ferror);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.AddWorkSheet: boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Worksheets.Add;
    result:=true;
  except
    ferror:='Unable to add a new worksheet';
    if assigned(fonerror) then
      fonerror(self,ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetVisible(IsVisible: Boolean): boolean;
begin
  result:=falsE;
  if not varisempty(fexcel) then
  try
    fexcel.Visible:=IsVisible;
    result:=true;
  except
    ferror:='Unable to set Excel Visibility';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.CloseWorkBooks(SaveAll: Boolean): boolean;
var
  loop: byte;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    for loop := 1 to fexcel.Workbooks.Count do
      fexcel.Workbooks[1].Close[SaveAll];
  except
    ferror:='Failed to close Excel WorkBooks';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SelectSheetByName(SheetName: String): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Sheets[SheetName].Select;
    result:=true;
  except
    ferror:='Failed to select sheet '+sheetname;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SelectCell(RowNum, ColNum: Integer): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.ActiveSheet.Cells[RowNum, ColNum].Select;
  except
    ferror:='Failed to select Cell';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.GetCellText(RowNum, ColNum: Integer): string;
begin
  result:='';
  if (not varisempty(fexcel)) and (rownum>0) and (colnum>0) then
  try
    result := fexcel.Cells[RowNum, ColNum].Text;
  except
    ferror:= 'Failed to get Cell Text';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

function TAPI_msexcel.GetCellValue(RowNum, ColNum: Integer): string;
begin
  result:='';
  if (not varisempty(fexcel)) and (rownum>0) and (colnum>0) then
  try
    result := fexcel.Cells[RowNum, ColNum].Value;
  except
    ferror:= 'Failed to get Cell Value';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

function TAPI_msexcel.GetCellValue: string;
begin
  result:= getcellvalue(getrow, getcol);
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.LocateLastRow( column: integer): integer;
begin
  result:= 1;
  if varisempty(fexcel) then exit;
  while getcellvalue( result, column )<>'' do
    result:= result + 1;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.ExportToCsv(var csvtext: string; const separator: string ): boolean;
var
  maxcol: integer;
  row, col: integer;
  value, line, linetemp: string;
  empty: boolean;
  sl: tstringlist;
begin
  result:= false;
  csvtext:= '';
  if varisempty(fexcel) then exit;

  maxcol:= 0;
  col:= 1;
  row:= 1;
  line:= '';
  linetemp:= '';
  sl:= tstringlist.create;
  try
    sl.Clear;

    empty:= false;
    while not empty do
    begin
      value:= getcelltext(row, col);

      if (value<>'') or (col<maxcol) then
      begin
        col:= col + 1;
        line:= line + value + separator;
        linetemp:= linetemp+value;
        if col>maxcol then maxcol:= col;
      end else

      begin
        row:= row + 1;
        if linetemp='' then empty:= true
          else sl.add(line);
        line:= '';
        linetemp:= '';
        col:= 1;
      end;

    end;

    csvtext:= sl.text;
    result:= true;

  finally
    sl.free;
  end;
end;

function TAPI_msexcel.ExportToCsvFile( const filename, separator: string ): boolean;
var
  s: string;
  sl: tstringlist;
begin
  result:= exporttocsv( s, separator );
  if result then
  begin
    sl:= tstringlist.create;
    try
      sl.Text:= s;
      try
        sl.SaveToFile(filename);
        result:= true;
      except
        // don't care
      end;
    finally
      sl.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.GetRow: integer;
begin
  result:=1;
  if not varisempty(fexcel) then
  try
    result:=fexcel.ActiveCell.Row;
  except
    ferror:='Failed to get row number';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.GetCol: integer;
begin
  result:=1;
  if not varisempty(fexcel) then
  try
    result:=fexcel.ActiveCell.Column;
  except
    ferror:='Failed to get column number';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetCellFormula(FormulaString : string; RowNum, ColNum: Integer): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.ActiveSheet.Cells[RowNum, ColNum].Formula := FormulaString;
    result:=true;
  except
    ferror:='Failed to set Formula';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetCellValue(RowNum, ColNum: Integer; Value : string): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Cells[RowNum, ColNum].Value := Value;
    result := True;
  except
    ferror:='Failed to set Cell Value';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

function TAPI_msexcel.SetCellValue(Value: string): boolean;
begin
  result:= setcellvalue(getrow, getcol, value);
end;

function TAPI_msexcel.SetCellValue(Value: integer): boolean;
begin
  result:= setcellvalue(getrow, getcol, inttostr(value));
end;

function TAPI_msexcel.SetCellValue(Value: double): boolean;
begin
  result:= setcellvalue(getrow, getcol, floattostr(value));
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.OpenWorkbook(FileName : String): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Workbooks.Open[FileName];
    result:= true;
  except
    ferror:='Unable to open '+FileName;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetColumnWidth(ColNum, ColumnWidth: Integer): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Columns[colNum].columnwidth := ColumnWidth;
    result:=true;
  except
    ferror:='Failed to set Column width';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetRowHeight(RowNum, RowHeight: Double): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.Rows[rowNum].rowheight := rowheight;
    result:=true;
  except
    ferror:='Failed to set Row height';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.RenameSheet(NewName: string): boolean;
begin
  result:=falsE;
  if not varisempty(fexcel) then
  try
    fexcel.ActiveSheet.Name := NewName;
    result:=true;
  except
    ferror:='Failed to Rename Sheet to '+newname;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.DeleteWorkSheet(SheetName : string): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    if not SelectSheetByName(SheetName) Then
    begin
      ferror:='Unable to select WorkSheet '+SheetName;
      if assigned(fonerror) then
        fonerror(self, ferror);
      exit;
    end;
    fexcel.ActiveWindow.SelectedSheets.Delete;
    result:=true;
  except
    ferror:='Failed to delete worksheet '+sheetname;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.GetActiveSheetName: string;
begin
  result:='';
  if not varisempty(fexcel) then
  try
    result := fexcel.ActiveSheet.Name;
  except
    ferror:='Failed to get active sheet name';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.GetCellFormula(RowNum, ColNum: Integer): string;
begin
  result:='';
  if not varisempty(fexcel) then
  try
    result:=fexcel.ActiveSheet.Cells[RowNum, ColNum].Formula;
  except
    ferror:='Failed to get Formula';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.ExcelVersion: string;
begin
  result:='';
  if not varisempty(fexcel) then
  try
    result:= fexcel.Version;
  except
    ferror:='Failed to get Excel version';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SetDisplayGridLines(Visible: boolean): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.ActiveWindow.DisplayGridlines := visible;
    result:=true;
  except
    ferror:='Failed to set Display Grid Lines';
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SaveWorkBookAs(Filename: string): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.SaveAs(filename);
    result:=true;
  except
    ferror:='Failed to save WorkBook as '+filename;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_msexcel.SaveActiveWorkBookAs(Filename: string): boolean;
begin
  result:=false;
  if not varisempty(fexcel) then
  try
    fexcel.ActiveWorkBook.SaveAs(filename);
    result:=true;
  except
    ferror:='Failed to save Active Workbook as '+filename;
    if assigned(fonerror) then
      fonerror(self, ferror);
  end;
end;

end.
