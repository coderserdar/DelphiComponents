{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TConverterEditor helper                       }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }

unit ConvAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, {$IFDEF _D3_}DBTables, {$ENDIF} vgTools, vgDBConv, DsgnIntf;

type
{$IFDEF _D4_}
  TFormDesigner = IFormDesigner;
{$ENDIF}

  TConvertItemsAddForm = class(TForm)
    rgFilter: TRadioGroup;
    lbTables: TListBox;
    Label1: TLabel;
    cmAdd: TButton;
    cmClose: TButton;
    cbAliases: TComboBox;
    Label2: TLabel;
    cmConnect: TButton;
    db: TDatabase;
    Label3: TLabel;
    cbClasses: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure cmConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TableListChanged(Sender: TObject);
    procedure cmAddClick(Sender: TObject);
  private
    { Private declarations }
    FConverter: TDBConverter;
    FTables: TStrings;
    procedure SetDatabase(Value: String);
  protected
    procedure Loaded; override;
  public
    { Public declarations }
  end;

procedure EditConverter(Converter: TDBConverter; ADesigner: TFormDesigner);

implementation

uses vgConvEd;

{$R *.DFM}
var
  ConvertItems: TStrings = nil;

procedure EditConverter(Converter: TDBConverter; ADesigner: TFormDesigner);
begin
  with TConvertItemsAddForm.Create(nil) do
  try
    FConverter := Converter;
    Designer := ADesigner;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TConvertItemsAddForm.FormCreate(Sender: TObject);
begin
  FTables := TStringList.Create;
  TStringList(FTables).OnChange := TableListChanged;
end;

procedure TConvertItemsAddForm.FormDestroy(Sender: TObject);
begin
  FTables.Free;
end;

procedure TConvertItemsAddForm.Loaded;
const
  I: Integer = 0;
var
  S: String;
begin
  repeat
    S := Format('A%d', [I]);
    Inc(I);
  until (Session.FindDatabase(S) = nil);
  db.DatabaseName := S;
  inherited;
end;

procedure TConvertItemsAddForm.FormShow(Sender: TObject);
begin
  Caption := '';
  cbClasses.Items.Assign(ConvertItems);
  cbClasses.ItemIndex := 0;
  Session.ConfigMode := cmPersistent;
  Session.GetAliasNames(cbAliases.Items);
end;

procedure TConvertItemsAddForm.cmConnectClick(Sender: TObject);
begin
  SetDatabase(cbAliases.Text);
end;

procedure TConvertItemsAddForm.SetDatabase(Value: String);
begin
  if AnsiCompareText(Caption, Value) <> 0 then
  begin
    if db.Connected then
    begin
      db.Close;
      Caption := '';
    end;
    if Value <> '' then
    begin
      db.DatabaseName := Value;
      db.Open;
      Caption := Value;
      FTables.BeginUpdate;
      try
        FTables.Clear;
        Session.GetTableNames(db.DatabaseName, '*.*', True, True, FTables);
      finally
        FTables.EndUpdate;
      end;
    end;
  end;
end;

procedure TConvertItemsAddForm.TableListChanged(Sender: TObject);
  function FindItem(TableName: String; Source: Boolean): Boolean;
  var
    I: Integer;
    Item: TDBConvertItem;
  begin
    Result := False;
    for I := 0 to FConverter.Count - 1 do
    begin
      Item := FConverter.Items[I];
      case Source of
        False:
          Result := AnsiCompareText(Item.TableNameDest, TableName) = 0;
        True:
          Result := AnsiCompareText(Item.TableNameSource, TableName) = 0;
      end;
      if Result then Break;
    end;
  end;
var
  I, J: Integer;
  S: String;
  Found: Boolean;
begin
  lbTables.Items.BeginUpdate;
  try
    lbTables.Items.Clear;
    J := rgFilter.ItemIndex;
    for I := 0 to FTables.Count - 1 do
    begin
      S := FTables[I];
      Found := False;
      case J of
        1: Found := FindItem(S, True);
        2: Found := FindItem(S, False);
      end;
      if not Found then lbTables.Items.Add(S);
    end;
  finally
    lbTables.Items.EndUpdate;
  end;
end;

procedure TConvertItemsAddForm.cmAddClick(Sender: TObject);
  function GetValidName(const Value: String): String;
  var
    I: Integer;
    C: Char;
  const
    Alpha = ['A'..'Z', 'a'..'z', '_'];
    AlphaNumeric = Alpha + ['0'..'9'];
  begin
    Result := Value;
    for I := 1 to Length(Result) do
    begin
      C := Result[I];
      if not (C in AlphaNumeric) then Result[I] := '_';
    end;
  end;
var
  I, J: Integer;
  S, Tmp: String;
  ItemClass: TDBConvertItemClass;
  Item: TDBConvertItem;
begin
  if cbClasses.ItemIndex < 0 then Exit;
  ItemClass := TDBConvertItemClass(cbClasses.Items.Objects[cbClasses.ItemIndex]);
  for I := 0 to lbTables.Items.Count - 1 do
  if lbTables.Selected[I] then
  begin
    S := lbTables.Items[I];
    Item := ItemClass.Create(FConverter.Owner);
    try
      Item.Converter := FConverter;
      Tmp := GetValidName(FConverter.Name + S);
      J := 0;
      while FConverter.Owner.FindComponent(Tmp) <> nil do
      begin
        Tmp := GetValidName(Format('%s%s%d', [FConverter.Name, S, J]));
        Inc(J);
      end;
      Item.Name := Tmp;
      Item.TableNameSource := S;
      Item.TableNameDest := S;
      Designer.Modified;
    except
      Item.Free;
      raise;
    end;
  end;
  TableListChanged(nil);
end;

procedure RegisterConvertClasses(const ConvertClasses: array of TDBConvertItemClass);
var
  I: Integer;
begin
  for I := Low(ConvertClasses) to High(ConvertClasses) do
    ConvertItems.AddObject(ConvertClasses[I].ClassName, TObject(ConvertClasses[I]));
end;

initialization
  ConvertItems := TStringList.Create;
  RegisterConvertItemsProc := RegisterConvertClasses;

finalization
  ConvertItems.Free;

end.
