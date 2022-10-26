{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{*******************************************************}

unit rxSelDSFrm;

{$I RX.INC}

interface

{$IFDEF DCS}

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DB, StdCtrls,
  {$IFDEF RX_D6} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF}; // Polaris

type

{ TSelectDataSetForm }

  TSelectDataSetForm = class(TForm)
    GroupBox: TGroupBox;
    DataSetList: TListBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure DataSetListDblClick(Sender: TObject);
    procedure DataSetListKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
{$IFDEF RX_D6}     // Polaris
    FDesigner: IDesigner;
{$ELSE}
    FDesigner: IFormDesigner;
{$ENDIF}
    FExclude: string;
    procedure FillDataSetList(ExcludeDataSet: TDataSet);
    procedure AddDataSet(const S: string);
  public
    { Public declarations }
  end;

{ TMemDataSetEditor }

  TMemDataSetEditor = class(TComponentEditor)
  private
    function UniqueName(Field: TField): string;
    procedure BorrowStructure;
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$IFDEF RX_D6}     // Polaris
function SelectDataSet(ADesigner: IDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
{$ELSE}
function SelectDataSet(ADesigner: IFormDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
{$ENDIF}

{$ENDIF DCS}

implementation

{$IFDEF DCS}

uses
  DbConsts, TypInfo, rxVclUtils, {StrUtils,} RxLConst,   // Polaris
  {$IFDEF RX_D3}{$IFDEF RX_D5} DsnDbCst, {$ELSE} BdeConst, {$ENDIF}{$ENDIF}
  DSDesign;

{$R *.DFM}

{$IFDEF RX_D6}     // Polaris
function SelectDataSet(ADesigner: IDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
{$ELSE}
function SelectDataSet(ADesigner: IFormDesigner; const ACaption: string;
  ExcludeDataSet: TDataSet): TDataSet;
{$ENDIF}
begin
  Result := nil;
  with TSelectDataSetForm.Create(Application) do
  try
    if ACaption <> '' then
      Caption := ACaption;
    FDesigner := ADesigner;
    FillDataSetList(ExcludeDataSet);
    if ShowModal = mrOk then
      if DataSetList.ItemIndex >= 0 then
        with DataSetList do
          Result := FDesigner.GetComponent(Items[ItemIndex]) as TDataSet;
  finally
    Free;
  end;
end;

{ TMemDataSetEditor }

procedure TMemDataSetEditor.BorrowStructure;
var
  DataSet: TDataSet;
  I: Integer;
  Caption: string;
begin
  Caption := Component.Name;
  if (Component.Owner <> nil) and (Component.Owner.Name <> '') then
    Caption := Format({$IFDEF CBUILDER} '%s->%s' {$ELSE} '%s.%s' {$ENDIF},
      [Component.Owner.Name, Caption]);
  DataSet := SelectDataSet(Designer, Caption, TDataSet(Component));
  if DataSet <> nil then
  begin
    StartWait;
    try
      if not CopyStructure(DataSet, Component as TDataSet) then
        Exit;
      with TDataSet(Component) do
      begin
        for I := 0 to FieldCount - 1 do
          if Fields[I].Name = '' then
            Fields[I].Name := UniqueName(Fields[I]);
      end;
    finally
      StopWait;
    end;
    Designer.Modified;
  end;
end;

function TMemDataSetEditor.UniqueName(Field: TField): string;
const
  AlphaNumeric = ['A'..'Z', 'a'..'z', '_'] + ['0'..'9'];
var
  Temp: string;
  Comp: TComponent;
  I: Integer;
begin
  Result := '';
  if (Field <> nil) then
  begin
    Temp := Field.FieldName;
    for I := Length(Temp) downto 1 do
      if not (Temp[I] in AlphaNumeric) then
        System.Delete(Temp, I, 1);
    if (Temp = '') or not IsValidIdent(Temp) then
    begin
      Temp := Field.ClassName;
      if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
        System.Delete(Temp, 1, 1);
    end;
  end
  else
    Exit;
  Temp := Component.Name + Temp;
  Comp := Designer.GetComponent(Temp);
  if (Comp = nil) or (Comp = Field) then
    Result := Temp
  else
    Result := Designer.UniqueName(Temp);
end;

procedure TMemDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
{$IFDEF RX_D5}
    0: ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
{$ELSE}
    0: ShowDatasetDesigner(Designer, TDataSet(Component));
{$ENDIF}
    1: BorrowStructure;
  end;
end;

function TMemDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := ResStr(SDatasetDesigner);
    1: Result := LoadStr(srBorrowStructure);
  end;
end;

function TMemDataSetEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSelectDataSetForm }

procedure TSelectDataSetForm.AddDataSet(const S: string);
begin
  if (S <> '') and (S <> FExclude) then
    DataSetList.Items.Add(S);
end;

procedure TSelectDataSetForm.FillDataSetList(ExcludeDataSet: TDataSet);
begin
  DataSetList.Items.BeginUpdate;
  try
    DataSetList.Clear;
    FExclude := '';
    if ExcludeDataSet <> nil then
      FExclude := ExcludeDataSet.Name;
    FDesigner.GetComponentNames(GetTypeData(TypeInfo(TDataSet)), AddDataSet);
    with DataSetList do
    begin
      if Items.Count > 0 then
        ItemIndex := 0;
      Enabled := Items.Count > 0;
      OkBtn.Enabled := (ItemIndex >= 0);
    end;
  finally
    DataSetList.Items.EndUpdate;
  end;
end;

procedure TSelectDataSetForm.DataSetListDblClick(Sender: TObject);
begin
  if DataSetList.ItemIndex >= 0 then
    ModalResult := mrOk;
end;

procedure TSelectDataSetForm.DataSetListKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) and (DataSetList.ItemIndex >= 0) then
    ModalResult := mrOk;
end;

{$ENDIF DCS}

end.