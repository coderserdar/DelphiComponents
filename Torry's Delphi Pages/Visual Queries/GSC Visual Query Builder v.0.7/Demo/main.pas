{*******************************************************************************
* GSC Query Builder v0.7 - Demo application                                    *
*------------------------------------------------------------------------------*
* Please read the top of GSCQBWorkArea.pas!!!                                  * 
*------------------------------------------------------------------------------*
* Contact:                                                                     *
*   Web: http://www.gsc.hu                                                     *
*   E-mail: info@gsc.hu (Subject: TGSCQBuilder)                                *
********************************************************************************}
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GSCQBWorkArea, StdCtrls, Grids, ExtCtrls;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    GSCQBWorkArea: TGSCQBWorkArea;
    SQLCommand: TMemo;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    GSCQBGrid: TGSCQBGrid;
    Button1: TButton;
    Button2: TButton;
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GSCQBWorkAreaGetTableFields(Sender: TObject;
      const ATableName: String; Fields: TStrings);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GSCQBWorkAreaChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ListBox1.BeginDrag(false);
end;

procedure TMainForm.GSCQBWorkAreaGetTableFields(Sender: TObject;
  const ATableName: String; Fields: TStrings);
const
  TableFields : array[0..4,0..5] of string = (
    ('id'   ,'name'  ,'address','phone','email' ,''       ),//Customers (2 fields)
    ('id'   ,'ordno' ,'custid' ,'date' ,'total' ,'comment'),//Orders
    ('ordid','prodid','tax'    ,'price','amount',''       ),//OrderItems
    ('id'   ,'catid' ,'prodno' ,'name' ,'price' ,''       ),//Products
    ('id'   ,'name'  ,''       ,''     ,''      ,''));//Categories
begin
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),0]);
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),1]);
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),2]);
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),3]);
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),4]);
  Fields.Add(TableFields[ListBox1.Items.IndexOf(ATableName),5]);
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  GSCQBGrid.UseGroupBy := CheckBox1.Checked;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(GSCQBWorkArea.TableCount));
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ShowMessage(IntToStr(GSCQBWorkArea.LinkCount));
end;

procedure TMainForm.GSCQBWorkAreaChange(Sender: TObject);
var
  SQLCmd : TStrings;
begin
  SQLCmd := GSCQBWorkArea.SQL;
  try
    SQLCommand.Lines.Assign(SQLCmd);
  finally
    SQLCmd.Free;
  end;
end;

end.
