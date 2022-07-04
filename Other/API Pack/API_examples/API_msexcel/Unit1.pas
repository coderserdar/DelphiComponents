unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_edit, ExtCtrls, API_grbutton, API_msexcel,
  API_msgdlg;

type
  TForm1 = class(TForm)
    API_msexcel1: TAPI_msexcel;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    API_grbutton6: TAPI_grbutton;
    API_grbutton7: TAPI_grbutton;
    API_msgdlg1: TAPI_msgdlg;
    API_grbutton8: TAPI_grbutton;
    ListBox1: TListBox;
    API_grbutton9: TAPI_grbutton;
    API_grbutton10: TAPI_grbutton;
    API_grbutton11: TAPI_grbutton;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure API_grbutton6Click(Sender: TObject);
    procedure API_grbutton7Click(Sender: TObject);
    procedure API_msexcel1Error(sender: TObject; const errortext: String);
    procedure API_grbutton8Click(Sender: TObject);
    procedure API_grbutton9Click(Sender: TObject);
    procedure API_grbutton10Click(Sender: TObject);
    procedure API_grbutton11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton10Click(Sender: TObject);
var
  s: string;
begin
  api_msexcel1.ExportToCsv(s, ';');
  showmessage(s);
end;

procedure TForm1.API_grbutton11Click(Sender: TObject);
var
  i: integer;
begin
  i:= api_msexcel1.LocateLastRow(1);
  showmessage('Last row is '+inttostr(i));
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_msexcel1.Open;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  api_msexcel1.close;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  api_msexcel1.SetVisible( not api_grbutton3.LedState ); 
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  opendialog1.Filter:='Excel WorkBooks (*.xls)|*.xls';
  if opendialog1.Execute then
  begin
    api_msexcel1.OpenWorkBook( opendialog1.FileName );
  end;
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  savedialog1.Filter:='Excel WorkBooks (*.xls)|*.xls';
  savedialog1.FileName:= api_msexcel1.GetActiveSheetName + '.xls';
  if savedialog1.Execute then
  begin
    api_msexcel1.SaveWorkBookAs( savedialog1.FileName );
  end;
end;

procedure TForm1.API_grbutton6Click(Sender: TObject);
var
  row: integer;
  col: integer;
  value: string;
begin
  row:= api_edit2.asInteger;
  col:= api_edit1.asInteger;
  value:= api_edit3.text;

  api_msexcel1.SetCellValue( row, col, value );
end;

procedure TForm1.API_grbutton7Click(Sender: TObject);
var
  row, col: integer;
  value: string;
begin
  row:= api_edit2.asinteger;
  col:= api_edit1.asinteger;
  value:= api_msexcel1.GetCellValue(row, col);
  api_edit3.Text:= value;
end;

procedure TForm1.API_msexcel1Error(sender: TObject;
  const errortext: String);
begin
  api_msgdlg1.Caption:='API_msexcel Error';
  api_msgdlg1.Msg.Text:= errortext;
  api_msgdlg1.Execute;
end;

procedure TForm1.API_grbutton8Click(Sender: TObject);
begin
  api_msexcel1.AddNewWorkbook;
end;

procedure TForm1.API_grbutton9Click(Sender: TObject);
var
  worksheet1: string;
  worksheet2: string;
begin

  // clear logbook (listbox)
  listbox1.Clear;

  // open excel object
  if api_msexcel1.Open then
  try
    listbox1.items.add('excel object opened');
    application.ProcessMessages;

    // add new workbook
    if api_msexcel1.AddNewWorkBook then
    begin
      listbox1.items.add('new workbook added');
      application.ProcessMessages;

      // get current worksheets name
      worksheet1:= api_msexcel1.GetActiveSheetName;
      listbox1.Items.add( 'Current worksheet = '+ worksheet1 );
      application.ProcessMessages;

      // insert some values in
      if api_msexcel1.SetCellValue(10, 10, 'testing this thing oout..') then
        listbox1.items.add('some values set into worksheet')
        else listbox1.items.add('error: failed to set cellvalues');
      application.ProcessMessages;

      // add new worksheet
      if api_msexcel1.AddWorkSheet then
        listbox1.Items.add('new worksheet added')
        else listbox1.items.add('failed to add new worksheet');
      application.processmessages;

      // get current worksheet name
      worksheet2:= api_msexcel1.GetActiveSheetName;
      listbox1.Items.add( 'Current worksheet = '+worksheet2 );
      application.ProcessMessages;

      // change new sheets name
      if api_msexcel1.RenameSheet( 'newsheet' ) then
        listbox1.items.add('New sheet changed from '+worksheet2+' to "newsheet"')
        else listbox1.items.add('error: failed to rename sheet '+worksheet2);
      application.ProcessMessages;

      // insert some values in
      if api_msexcel1.SetCellValue(1, 1, 'now this is placed to the new worksheet') then
        listbox1.items.add('some values set into workbook')
        else listbox1.items.add('error: failed to set cellvalues');
      application.ProcessMessages;

      // switch back to the original one
      if api_msexcel1.SelectSheetByName( worksheet1 ) then
        listbox1.items.add('switched back to the '+worksheet1)
        else listbox1.items.add('error: failed to switch to '+worksheet1);
      application.ProcessMessages;

      // set display alerts to false
      if api_msexcel1.SetDisplayAlerts(false) then
        listbox1.items.add('Excel displaying alerts is set to false')
        else listbox1.items.add('error: failed to set display alerts to false');

      // save the document
      if api_msexcel1.SaveActiveWorkBookAs('c:\testi.xls') then
        listbox1.items.add('document saved to c:\testi.xls')
        else listbox1.items.add('error: failed to save workbook');
      application.ProcessMessages;

      // save copy as html
      try
        api_msexcel1.fexcel.activeworkbook.saveas('c:\testi.html', xlHTML); // note that not all of these "internal" constants are set in API_msexcel component
        listbox1.items.add('save as html copy succeeded');
      except
        listbox1.items.add('failed to save copy as html');
      end;
      application.ProcessMessages;

    end else
    begin
      listbox1.Items.add('error: failed to add new workbook');
      application.ProcessMessages;
    end;

  // and finally close the workbook
  finally
    if api_msexcel1.Close then
      listbox1.items.add('excel object closed')
      else begin
             listbox1.items.add('error: failed to close excel object');
             api_msexcel1.SetVisible(true);
           end;

  // failed to open excel object
  end else
  begin
    listbox1.items.add('error: failed to open excel');
  end;
end;

end.
