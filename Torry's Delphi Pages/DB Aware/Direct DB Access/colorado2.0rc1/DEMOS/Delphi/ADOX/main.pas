unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ComCtrls, Grids, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, colorado;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    DBGrid1: TDBGrid;
    DBTables1_DS: TDataSource;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    Splitter1: TSplitter;
    DBColumns1_DS: TDataSource;
    DBGrid2: TDBGrid;
    Panel1: TPanel;
    Splitter2: TSplitter;
    Label1: TLabel;
    DBGrid3: TDBGrid;
    ColumnProperties_DS: TDataSource;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Splitter3: TSplitter;
    DBGrid4: TDBGrid;
    DBIndexes1_DS: TDataSource;
    PageControl3: TPageControl;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    DBGrid5: TDBGrid;
    IndexColumns1_DS: TDataSource;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    IndexProperties_DS: TDataSource;
    DBGrid6: TDBGrid;
    DBGrid7: TDBGrid;
    PageControl4: TPageControl;
    TabSheet11: TTabSheet;
    DBGrid8: TDBGrid;
    DBKeys1_DS: TDataSource;
    KeyColumns_DS: TDataSource;
    DBGrid10: TDBGrid;
    Splitter4: TSplitter;
    DBViews1_DS: TDataSource;
    DBNavigator1: TDBNavigator;
    DBMemo1: TDBMemo;
    DBGrid9: TDBGrid;
    DBNavigator2: TDBNavigator;
    DBProcedures1_DS: TDataSource;
    Splitter5: TSplitter;
    DBGroups1_DS: TDataSource;
    DBGrid11: TDBGrid;
    Splitter6: TSplitter;
    PageControl5: TPageControl;
    TabSheet12: TTabSheet;
    GroupUsers_DS: TDataSource;
    DBGrid12: TDBGrid;
    DBGrid13: TDBGrid;
    Splitter7: TSplitter;
    PageControl6: TPageControl;
    TabSheet13: TTabSheet;
    DBGrid14: TDBGrid;
    DBUsers1_DS: TDataSource;
    UserGroups_DS: TDataSource;
    TabSheet14: TTabSheet;
    GroupPermissions_DS: TDataSource;
    DBGrid15: TDBGrid;
    TabSheet15: TTabSheet;
    DBGrid16: TDBGrid;
    UserPermissions_DS: TDataSource;
    DBParameters1_DS: TDataSource;
    Panel2: TPanel;
    DBMemo2: TDBMemo;
    Splitter8: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    DBGrid17: TDBGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses dm;

{$R *.DFM}

end.
