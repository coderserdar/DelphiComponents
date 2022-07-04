unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ActnList, jvuib, FBCustomDataSet, DBGrids, DB, ExtCtrls, DBCtrls, LCLType,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    actCustNew: TAction;
    actCustEdit: TAction;
    actCustDelete: TAction;
    actCustFilter: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    dbGrid2: TdbGrid;
    dsSprCustomer: TDatasource;
    dsSprCountry: TDatasource;
    dbGrid1: TdbGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl3: TPageControl;
    Panel1: TPanel;
    quSprCustomer: TFBDataSet;
    quSprCountry: TFBDataSet;
    JvUIBDataBase1: TJvUIBDataBase;
    tabCustomer: TTabSheet;
    tabAbout: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    trRead: TJvUIBTransaction;
    trWrite: TJvUIBTransaction;
    MainMenu1: TMainMenu;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    StatusBar1: TStatusBar;
    tabMain: TTabSheet;
    tabDirectories: TTabSheet;
    tabSprCountry: TTabSheet;
    procedure ApplicationProperties1Hint(Sender: TObject);
    procedure MainFormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure actCustEditExecute(Sender: TObject);
    procedure actCustDeleteExecute(Sender: TObject);
    procedure actCustFilterExecute(Sender: TObject);
    procedure dbGrid1TitleClick(Column: TColumn);
    procedure dbGrid2TitleClick(Column: TColumn);
    procedure quSprCountryAfterClose(DataSet: TDataSet);
    procedure quSprCustomerAfterClose(DataSet: TDataSet);
  private
    { sort engine }
    //Country
    FSFSprCountry:string;
    FSFSprCountryOrder:boolean;
    //Customers
    FSFSprCustomer:string;
    FSFSprCustomerOrder:boolean;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation
uses sprCustomerEditUnit, sprCustomerFilterUnit;

{ TMainForm }

procedure TMainForm.PageControl2Change(Sender: TObject);
begin
  quSprCountry.Active:=(PageControl2.ActivePage = tabSprCountry) and (PageControl1.ActivePage = tabDirectories);
  quSprCustomer.Active:=(PageControl2.ActivePage = tabCustomer) and (PageControl1.ActivePage = tabDirectories);
end;

procedure TMainForm.actCustEditExecute(Sender: TObject);
begin
  sprCustomerEditForm:=TsprCustomerEditForm.Create(Application);
  sprCustomerEditForm.Datasource1.DataSet:=quSprCustomer;
  if (Sender as TComponent).Tag=1 then  quSprCustomer.Append
  else quSprCustomer.Edit;
  sprCustomerEditForm.CheckBox1.Checked:=quSprCustomer.FieldByName('ON_HOLD').AsString = '*';
  sprCustomerEditForm.DBComboBox1.Text:=quSprCustomer.FieldByName('COUNTRY').AsString;
  if sprCustomerEditForm.ShowModal=mrOk then
  begin
    quSprCustomer.FieldByName('COUNTRY').AsString:=sprCustomerEditForm.DBComboBox1.Text;
    if sprCustomerEditForm.CheckBox1.Checked then
      quSprCustomer.FieldByName('ON_HOLD').AsString := '*'
    else
      quSprCustomer.FieldByName('ON_HOLD').Clear;
    quSprCustomer.Post
  end
  else
    quSprCustomer.Cancel;
  sprCustomerEditForm.Free;
end;

procedure TMainForm.actCustDeleteExecute(Sender: TObject);
begin
  if Application.MessageBox('Delete this record?', 'Warning',MB_ICONQUESTION + MB_YESNO)=id_Yes then
    quSprCustomer.Delete;
end;

procedure TMainForm.actCustFilterExecute(Sender: TObject);
var
  SMacro:string;

procedure AddMacro(S:string);
begin
  if SMacro<>'' then SMacro:=SMacro + ' and ';
  SMacro:=SMacro+'('+S+')';
end;

begin
  sprCustomerFilterForm:=TsprCustomerFilterForm.Create(Application);
  try
    if sprCustomerFilterForm.ShowModal=mrOk then
    begin
      quSprCustomer.Close;
      SMacro:='';
      if sprCustomerFilterForm.Edit1.Text<>'' then
        AddMacro('CUSTOMER.CUSTOMER like ''%'+sprCustomerFilterForm.Edit1.Text+'%''');
      if sprCustomerFilterForm.Edit2.Text<>'' then
        AddMacro('CUSTOMER.PHONE_NO = '''+sprCustomerFilterForm.Edit2.Text+'''');
      if SMacro<>'' then
        SMacro:=' where ' + SMacro;
      quSprCustomer.MacroByName('MacroFilter').Value:=SMacro;
      quSprCustomer.Open;
      actCustFilter.Checked:=SMacro<>'';
    end;
  finally
    sprCustomerFilterForm.Free;
  end;
end;


procedure TMainForm.dbGrid1TitleClick(Column: TColumn);
begin
  if quSprCountry.Active then
  begin
    if Column.Field.FieldName = FSFSprCountry then
      FSFSprCountryOrder:=not FSFSprCountryOrder
    else
    begin
      FSFSprCountryOrder:=true;
      FSFSprCountry := Column.Field.FieldName;
    end;
    quSprCountry.SortOnField(FSFSprCountry, FSFSprCountryOrder);
  end;
end;

procedure TMainForm.dbGrid2TitleClick(Column: TColumn);
begin
  if quSprCustomer.Active then
  begin
    if Column.Field.FieldName = FSFSprCustomer then
      FSFSprCustomerOrder:=not FSFSprCustomerOrder
    else
    begin
      FSFSprCustomerOrder:=true;
      FSFSprCustomer := Column.Field.FieldName;
    end;
    quSprCustomer.SortOnField(FSFSprCustomer, FSFSprCustomerOrder);
  end;
end;


procedure TMainForm.quSprCountryAfterClose(DataSet: TDataSet);
begin
  FSFSprCountry:='';
end;

procedure TMainForm.quSprCustomerAfterClose(DataSet: TDataSet);
begin
  FSFSprCustomer:='';
end;

procedure TMainForm.MainFormCreate(Sender: TObject);
begin
  JvUIBDataBase1.Connected:=true;
  PageControl1Change(nil);
end;

procedure TMainForm.ApplicationProperties1Hint(Sender: TObject);
begin
  StatusBar1.SimpleText:=Application.Hint;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  PageControl2Change(nil);
end;

initialization
  {$I MainUnit.lrs}

end.

