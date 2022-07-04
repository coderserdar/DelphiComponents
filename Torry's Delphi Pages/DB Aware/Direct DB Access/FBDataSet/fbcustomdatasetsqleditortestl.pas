unit FBCustomDataSetSQLEditorTestL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, Buttons, FBCustomDataSet, DB, ComCtrls, SynEdit;

type

  { TFBCustomDataSetSQLEditorTestForm }

  TFBCustomDataSetSQLEditorTestForm = class(TForm)
    Button1: TButton;
    Datasource1: TDatasource;
    dbGrid1: TdbGrid;
    FBDataSetTest: TFBDataSet;
    PageControl1: TPageControl;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FBCustomDataSetSQLEditorTestForm: TFBCustomDataSetSQLEditorTestForm;

implementation

initialization
  {$I fbcustomdatasetsqleditortestl.lrs}

end.

