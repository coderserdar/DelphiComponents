unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  jvuib, jvuibdataset, DB, StdCtrls, FBCustomDataSet, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Datasource1: TDatasource;
    dbGrid1: TdbGrid;
    Edit1: TEdit;
    FBDataSet1: TFBDataSet;
    JvUIBDataBase1: TJvUIBDataBase;
    JvUIBTransaction1: TJvUIBTransaction;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FBDataSet1.Locate(dbGrid1.SelectedField.FieldName, Edit1.Text, [loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  JvUIBDataBase1.Connected:=true;
  FBDataSet1.Open;
end;

initialization
  {$I Unit1.lrs}

end.

