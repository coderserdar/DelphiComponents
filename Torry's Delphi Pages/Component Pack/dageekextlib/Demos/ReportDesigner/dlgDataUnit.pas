{*******************************************************}
{                                                       }
{       Report Designer                                 }
{       Extension Library example of                    }
{       TELDesigner, TELDesignPanel                     }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit dlgDataUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids;

type
  TdlgData = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label2: TLabel;
    Label3: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label4: TLabel;
    ListBox2: TListBox;
    Label5: TLabel;
    Memo1: TMemo;
    Panel6: TPanel;
    Label6: TLabel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TBDEDataSet;
    FStepNum: Integer;
    FAliasName: string;
    FIsUseTable: Boolean;
    FOwner: TWinControl;
    procedure Step;
    procedure UpdateButtons;
    procedure ShowStepPanel;
  public
    { If function return True, then it create new ADataSet object, but NOT
      free old one. You mast explicitly free old object}
    function Execute(var ADataSet: TBDEDataSet; AOwner: TWinControl): Boolean;
  end;

var
  dlgData: TdlgData;

const
  MAXSTEPNUM = 3;

implementation

{$R *.dfm}

{ TForm2 }

function TdlgData.Execute(var ADataSet: TBDEDataSet; AOwner: TWinControl): Boolean;
begin
  FDataSet := nil;
  FOwner := AOwner;
  FStepNum := -1;
  ShowStepPanel;
  FStepNum := 0;
  UpdateButtons;
  Memo1.Lines.Clear;
  Result := (ShowModal = mrOk);
  if not Result and (FDataset <> nil) then
  begin
    FDataSet.Free;
    FDataSet := nil;
  end;
  ADataSet := FDataSet;
  DataSource1.DataSet := nil;
end;

procedure TdlgData.FormShow(Sender: TObject);
begin
  Step;
end;

procedure TdlgData.Step;
begin
  UpdateButtons;
  ShowStepPanel;
  case FStepNum of
    0:
      begin
        if not Session.Active then Session.Open;
        ListBox1.Items.Clear;
        Session.GetAliasNames(ListBox1.Items);
        if ListBox1.Items.Count > 0 then
          ListBox1.ItemIndex := 0;
        Button3.Enabled := (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex <> -1);
      end;
    1:
      FAliasName := ListBox1.Items[ListBox1.ItemIndex];
    2:
      begin
        FIsUseTable := RadioButton1.Checked;
        Panel4.Visible := FIsUseTable;
        Panel5.Visible := not FIsUseTable;
        if FIsUseTable then
        begin
          ListBox2.Items.Clear;
          Session.GetTableNames(FAliasName, '', False, False, ListBox2.Items);
          if ListBox2.Items.Count > 0 then
            ListBox2.ItemIndex := 0;
          Button3.Enabled := (ListBox2.Items.Count > 0) and (ListBox2.ItemIndex <> -1);
        end;
      end;
    3:
      begin
        if FDataSet <> nil then
        begin
          FDataSet.Free;
          FDataSet := nil;
        end;
        if FIsUseTable then
        begin
          FDataSet := TTable.Create(FOwner);
          with TTable(FDataSet) do
          begin
            DatabaseName := FAliasName;
            TableName := ListBox2.Items[ListBox2.ItemIndex];
          end;
        end
        else
        begin
          FDataSet := TQuery.Create(FOwner);
          with TQuery(FDataSet) do
          begin
            DatabaseName := FAliasName;
            SQL := Memo1.Lines;
          end;
        end;
        try
          DataSource1.DataSet := FDataSet;
          FDataSet.Open;
        except
          Button3.Enabled := False;
          FDataSet.Free;
          FDataSet := nil;
          raise;
        end;
      end;
    MAXSTEPNUM + 1:
      ModalResult := mrOk;
  end;
end;

procedure TdlgData.Button2Click(Sender: TObject);
begin
  Dec(FStepNum);
  Step;
end;

procedure TdlgData.Button3Click(Sender: TObject);
begin
  Inc(FStepNum);
  Step;
end;

procedure TdlgData.UpdateButtons;
begin
  Button2.Enabled := FStepNum > 0;
  Button3.Enabled := True;
  if FStepNum >= MAXSTEPNUM then
    Button3.Caption := 'Ok'
  else
    Button3.Caption := 'Next >>';
end;

procedure TdlgData.ShowStepPanel;
begin
  Panel1.Visible := (FStepNum = 0);
  Panel2.Visible := (FStepNum = 1);
  Panel3.Visible := (FStepNum = 2);
  Panel6.Visible := (FStepNum = 3);
end;

procedure TdlgData.ListBox1Click(Sender: TObject);
begin
  Button3.Enabled := (ListBox1.Items.Count > 0) and (ListBox1.ItemIndex <> -1);
end;

procedure TdlgData.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then Button3Click(nil);
end;

procedure TdlgData.ListBox2Click(Sender: TObject);
begin
  Button3.Enabled := (ListBox2.Items.Count > 0) and (ListBox2.ItemIndex <> -1);
end;

procedure TdlgData.ListBox2DblClick(Sender: TObject);
begin
  if ListBox2.ItemIndex <> -1 then Button3Click(nil);
end;

end.
