unit Unit1;

// workbook unit demo
// 15072004, ari pikivirta
// * everything you see here is free to use in any application or purpose,
//   just author is not responsible of any damages this might cause.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, API_edit, API_label, API_listbox,
  API_grbutton, Grids, API_stringgrid, TeEngine, Series, TeeProcs, Chart,
  jpeg, API_workbook, Menus, API_base;

type
  TForm1 = class(TForm)
    API_label1: TAPI_label;
    API_label2: TAPI_label;
    API_edit1: TAPI_edit;
    API_label3: TAPI_label;
    API_edit2: TAPI_edit;
    API_label4: TAPI_label;
    API_edit3: TAPI_edit;
    Bevel1: TBevel;
    API_grbutton1: TAPI_grbutton;
    API_label6: TAPI_label;
    API_label7: TAPI_label;
    API_label8: TAPI_label;
    API_edit5: TAPI_edit;
    API_edit6: TAPI_edit;
    API_edit7: TAPI_edit;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_label9: TAPI_label;
    API_edit8: TAPI_edit;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    API_grbutton7: TAPI_grbutton;
    API_grbutton8: TAPI_grbutton;
    API_grbutton9: TAPI_grbutton;
    API_grbutton10: TAPI_grbutton;
    API_grbutton11: TAPI_grbutton;
    Chart1: TChart;
    Series1: TLineSeries;
    API_grbutton12: TAPI_grbutton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    API_label5: TAPI_label;
    API_label10: TAPI_label;
    Label2: TLabel;
    API_label11: TAPI_label;
    Label3: TLabel;
    API_label12: TAPI_label;
    Label4: TLabel;
    API_label13: TAPI_label;
    Label5: TLabel;
    Timer1: TTimer;
    API_grbutton13: TAPI_grbutton;
    PopupMenu1: TPopupMenu;
    Cancel1: TMenuItem;
    ExportCurrentRowasStringList1: TMenuItem;
    ExportCurrentColasStringList1: TMenuItem;
    API_workbook1: TAPI_workbook;
    API_stringgrid1: TAPI_stringgrid;
    procedure ExportCurrentColasStringList1Click(Sender: TObject);
    procedure ExportCurrentRowasStringList1Click(Sender: TObject);
    procedure API_grbutton13Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure API_stringgrid1Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton6Click(Sender: TObject);
    procedure API_grbutton7Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure API_grbutton8Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton10Click(Sender: TObject);
    procedure API_grbutton11Click(Sender: TObject);
    procedure API_grbutton12Click(Sender: TObject);
    procedure API_stringgrid1GetEditText(Sender: TObject; ACol,
      ARow: Integer; var Value: String);
    procedure API_stringgrid1SetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure Timer1Timer(Sender: TObject);
    procedure API_stringgrid1TopLeftChanged(Sender: TObject);
  private
  public
    procedure updatetable;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// regenerate table creates whole new table with set parameters.
procedure TForm1.API_grbutton1Click(Sender: TObject);
var
  pages: integer;
  columns: integer;
  rows: integer;
  i: integer;
begin
  pages:= api_edit1.asInteger;
  columns:= api_edit2.asInteger;
  rows:= api_edit3.asinteger;
  // these are called from TAPI_workbook unit..
  api_workbook1.Pages:= pages;
  for i:=0 to pages-1 do
  begin
    api_workbook1.Page:= i;
    api_workbook1.Columns:= columns;
    api_workbook1.Rows:= rows;
  end;
  api_workbook1.clearallpages;
  api_workbook1.page:= 0;
  // and listbox is updated here..
  updatetable;
end;

//------------------------------------------------------------------------------
// next routine will draw the workbook values into the api listbox visible
// on this example application.
procedure TForm1.updatetable;
var
  topindex: integer;
  leftindex: integer;
  i: integer;
begin
  topindex:= api_stringgrid1.TopRow;
  leftindex:= api_stringgrid1.LeftCol;
  // show current page vs. max pages
  api_grbutton9.Caption:= inttostr( api_workbook1.Page+1 ) + ' / '+ inttostr( api_workbook1.Pages );
  // export workbook contents to stringgrid
  api_workbook1.exportstringgrid( api_stringgrid1 );
  // draw row and col numbering
  for i:=0 to api_stringgrid1.ColCount-1 do
    api_stringgrid1.Cells[i,0]:= inttostr(i-1);
  for i:=1 to api_stringgrid1.rowcount-1 do
    api_stringgrid1.Cells[0,i]:= inttostr(i-1);
  // return to position we started updating
  api_stringgrid1.TopRow:= topindex;
  api_stringgrid1.LeftCol:= leftindex;
end;

//------------------------------------------------------------------------------
// read defined cells value as string.. in this demo we only allowed writing
// floating point values into the workbook. to get out floating point values
// from workbook, we have to define variable as floating point to store
// get value in.
procedure TForm1.API_grbutton2Click(Sender: TObject);
var
  value: double;
begin
  api_workbook1.getcellvalue ( api_edit5.asinteger, api_edit6.asinteger, value );
  api_edit7.Text:= floattostr(value);
  updatetable;
end;

//------------------------------------------------------------------------------
// Writes defined cells value as string.. to store floating point value is
// differs from storing strings. both text and number value can exist
// in each workbook cell - so we just store the floating point.
procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  api_workbook1.setcellvalue ( api_edit5.asinteger, api_edit6.asInteger, api_edit7.asFloat );
  updatetable;
  api_edit7.text:='0';
  api_edit7.SetFocus;
end;

//------------------------------------------------------------------------------
// last page
procedure TForm1.API_grbutton6Click(Sender: TObject);
begin
  api_workbook1.Page:= api_workbook1.page -1;
  updatetable;
end;

//------------------------------------------------------------------------------
// next page
procedure TForm1.API_grbutton7Click(Sender: TObject);
begin
  api_workbook1.page:= api_workbook1.page +1;
  updatetable;
end;

//------------------------------------------------------------------------------
// first page
procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  api_workbook1.Page:= 0;
  updatetable;
end;

//------------------------------------------------------------------------------
// previous page
procedure TForm1.API_grbutton8Click(Sender: TObject);
begin
  api_workbook1.page:= api_workbook1.pages-1;
  updatetable;
end;

//------------------------------------------------------------------------------
// draw line chart routine for one column that is selected.
procedure TForm1.API_grbutton4Click(Sender: TObject);
var
  col: integer;
  value: double;
  i: integer;
begin
  col:= api_edit8.asInteger;
  series1.Clear;
  for i:=1 to api_workbook1.rows do
  begin
    api_workbook1.getcellvalue( col, i-1, value );
    series1.AddXY( i-1, value );
  end;
end;

//------------------------------------------------------------------------------
// saving of workbookfile. see whole function in workbook unit to see also
// the rest of document's data.
procedure TForm1.API_grbutton10Click(Sender: TObject);
begin
  // initialize save dialog settings
  savedialog1.filename:= '';
  savedialog1.Filter:= 'Workbooks (*.awb)|*.awb';
  savedialog1.FilterIndex:= 1;
  savedialog1.DefaultExt:='.awb';

  // execute save dialog
  if savedialog1.Execute then
  begin
    //api_workbook1.CompressFile:= (savedialog1.filterindex<>1);
    if api_workbook1.saveworkbook(savedialog1.FileName) then
    begin
      messagedlg('Workbook saved to file', mtinformation, [mbok], 0);
    end else
      messagedlg('Failed to save workbook', mterror, [mbok], 0);
  end;
end;

//------------------------------------------------------------------------------
// opening of workbook files through the units internal open method.
procedure TForm1.API_grbutton11Click(Sender: TObject);
begin
  // init open dialog
  opendialog1.filename:= '';
  opendialog1.Filter:= 'Workbooks (*.awb)|*.awb';
  opendialog1.FilterIndex:= 0;

  // execute dialog
  if opendialog1.Execute then
  begin
    //api_workbook1.CompressFile:= (opendialog1.filterindex=1);
    if api_workbook1.openworkbook(opendialog1.filename) then
    begin
      api_edit1.Text:= inttostr(api_workbook1.pages);
      api_edit2.text:= inttostr(api_workbook1.Columns);
      api_edit3.text:= inttostr(api_workbook1.Rows);
      api_workbook1.Page:= 0;
      updatetable;
    end else
      messagedlg('Failed to open workbook', mterror, [mbok], 0);
  end;
end;

//------------------------------------------------------------------------------
// capture chart1 image and store it into jpeg file. This look ugly but is
// very fast and after all consumes quite nothing cpu time.
procedure TForm1.API_grbutton12Click(Sender: TObject);
var
  bmp: tbitmap;
  jpg: tjpegimage;
begin
  // prepare save dialog
  savedialog1.Filter:= 'Bitmap (*.bmp)|*.bmp|Jpeg (*.jpg)|*.jpg';
  savedialog1.FilterIndex:= 1; // jpeg
  savedialog1.DefaultExt:='.jpg';

  // execute dialog
  if savedialog1.Execute then
  begin

    // create bitmap
    bmp:= chart1.TeeCreateBitmap(clblack, rect(0,0,chart1.Width,chart1.Height));
    try

      // save bitmap
      if savedialog1.FilterIndex=0 then
      try
        bmp.SaveToFile(savedialog1.filename);
        messagedlg(savedialog1.filename+#13+'saved succesfully', mtinformation, [mbok], 0);
      except
        messagedlg('Failed to save'+#13+savedialog1.filename, mterror, [mbok], 0);
      end else

      // create and save jpeg
      if savedialog1.filterindex=1 then
      begin
        jpg:= tjpegimage.Create;
        try
          jpg.Assign(bmp);
          try
            jpg.SaveToFile(savedialog1.filename);
            messagedlg(savedialog1.filename+#13+'saved succesfully', mtinformation, [mbok], 0);
          except
            messagedlg('Failed to save'+#13+savedialog1.filename, mterror, [mbok], 0);
          end;
        finally
          jpg.free;
        end;
      end else

      // invalid filter selection
        messagedlg('Invalid file format selected', mterror, [mbok], 0);

    finally
      bmp.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_grbutton13Click(Sender: TObject);
begin
  savedialog1.filename:= '';
  savedialog1.Filter:='Html files (*.html)|*.html';
  savedialog1.DefaultExt:='.html';
  if savedialog1.Execute then
  begin
    if api_workbook1.exporthtmlfile( savedialog1.FileName ) then
      showmessage('html file saved succesfully')
      else showmessage('failed to save html file');
    updatetable;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_stringgrid1Click(Sender: TObject);
begin
  api_edit5.text:= inttostr(api_Stringgrid1.SelectedCol-1);
  api_edit6.text:= inttostr(api_stringgrid1.SelectedRow-1);
  api_grbutton2click(self);
end;

//------------------------------------------------------------------------------
// this time the input is only for floating point numbers, so the
// workbook value is get as double too.
procedure TForm1.API_stringgrid1GetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
var
  valu: double;
begin
  api_workbook1.getcellvalue ( acol-1, arow-1, valu );
  value:= floattostr( valu );
end;

//------------------------------------------------------------------------------
// check that entered value is double.. otherwise - this will crear out
// the whole cell..
procedure TForm1.API_stringgrid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  valu: double;
begin
  if value<>'' then
  try
    valu:= strtofloat( value );
    api_workbook1.setcellvalue( acol-1, arow-1, valu );
  except
    api_workbook1.getcellvalue( acol-1, arow-1, valu );
    api_stringgrid1.Cells[arow,acol]:= floattostr(valu);
  end;
end;

procedure TForm1.API_stringgrid1TopLeftChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TForm1.ExportCurrentColasStringList1Click(Sender: TObject);
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    if api_workbook1.ExportStringListColumn(sl, api_stringgrid1.SelectedCol - api_stringgrid1.FixedCols) then
      showmessage(sl.text);
  finally
    sl.free;
  end;
end;

procedure TForm1.ExportCurrentRowasStringList1Click(Sender: TObject);
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    if api_workbook1.ExportStringListRow(sl, api_stringgrid1.SelectedRow - api_stringgrid1.FixedRows) then
      showmessage(sl.text);
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  api_grbutton1click(self);
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  label1.Caption:= api_workbook1.Document;
  label2.caption:= api_workbook1.Author;
  label3.caption:= datetostr( api_workbook1.Created ) + ' ' + timetostr( api_workbook1.Created );
  label4.caption:= api_workbook1.Editor;
  label5.caption:= datetostr( api_workbook1.Modified ) + ' ' + timetostr( api_workbook1.Modified );
end;

end.
