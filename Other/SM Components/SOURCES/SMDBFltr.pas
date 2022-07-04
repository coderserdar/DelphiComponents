{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com

  In this unit I described the visual dialog for Dataset's Filter/Where clause property editing.
}
unit SMDBFltr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, DB, IniFiles, Buttons;

type
  TSMFilterMode = (fmFilterDataset, fmExpression);
  TSMFilterOption = (foLoadFrom, foSaveAs);
  TSMFilterOptions = set of TSMFilterOption;

  TSMDBFilterItem = class
    FieldIndex: Integer;
    FieldName: string;
    FieldCaption: string;
  end;

  TfrmFilterDialog = class;

  TSMDBFExpression = procedure(Sender: TObject; IsFirstRow, boolCondition: Boolean; intField, intCondition: Integer; Value: string; var Expression: string) of object;
  TSMDBFValue = procedure(Sender: TObject; intField, intCondition: Integer; var Value: string) of object;

  TSMDBFilterDialog = class(TComponent)
  private
    { Private declarations }
    FCaption: TCaption;
    FDataset: TDataset;
    FAllowedFields: TStrings;
    FCaptionsFields: TStrings;

    FFilterMode: TSMFilterMode;
    FExpression: string;
    FOptions: TSMFilterOptions;

    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;
    FOnShow: TNotifyEvent;

    FOnLoadFilter: TNotifyEvent;
    FOnSaveFilter: TNotifyEvent;

    FOnExpression: TSMDBFExpression;
    FOnValue: TSMDBFValue;
    FWildCard: string;
    FIsSQLBased: Boolean;

    frmFilterDialog: TfrmFilterDialog;

    procedure SetAllowedFields(Value: TStrings);
    procedure SetCaptionsFields(Value: TStrings);
  protected
    { Protected declarations }
    function GetItemCaption(item: TSMDBFilterItem): string; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCurrentFilter: string;
    procedure SetCurrentFilter(Value: string);
    function Execute: Boolean;
  published
    { Published declarations }
    property Caption: TCaption read FCaption write FCaption;
    property Dataset: TDataset read FDataset write FDataset;
    property AllowedFields: TStrings read FAllowedFields write SetAllowedFields;
    property CaptionsFields: TStrings read FCaptionsFields write SetCaptionsFields;

    property FilterMode: TSMFilterMode read FFilterMode write FFilterMode;
    property Expression: string read FExpression write FExpression;

    property Options: TSMFilterOptions read FOptions write FOptions default [foLoadFrom, foSaveAs];
    property WildCard: string read FWildCard write FWildCard;
    property IsSQLBased: Boolean read FIsSQLBased write FIsSQLBased default True;

    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;

    property OnExpression: TSMDBFExpression read FOnExpression write FOnExpression;
    property OnValue: TSMDBFValue read FOnValue write FOnValue;

    property OnLoadFilter: TNotifyEvent read FOnLoadFilter write FOnLoadFilter;
    property OnSaveFilter: TNotifyEvent read FOnSaveFilter write FOnSaveFilter;
  end;

  TfrmFilterDialog = class(TForm)
    lblFilter: TLabel;
    lbFilter: TListView;
    btnFilterAdd: TButton;
    btnFilterDelete: TButton;
    lblAddFilter: TLabel;
    rbAND: TRadioButton;
    rbOR: TRadioButton;
    cbFilterField: TComboBox;
    cbFilterCondition: TComboBox;
    edFilterValue: TEdit;
    lblFilterValue: TLabel;
    lblFilterCondition: TLabel;
    lblFilterField: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    bvlButton: TBevel;
    btnClear: TButton;
    btnFilterLoad: TSpeedButton;
    btnFilterSave: TSpeedButton;
    btnFilterEdit: TButton;
    procedure btnFilterAddClick(Sender: TObject);
    procedure btnFilterDeleteClick(Sender: TObject);
    procedure lbFilterChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure cbFilterFieldChange(Sender: TObject);
    procedure btnFilterSaveClick(Sender: TObject);
    procedure btnFilterLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure lbFilterClick(Sender: TObject);
  private
    { Private declarations }
    fltFile: string;
    procedure AddFilter(EditLI: TListItem; IsUserTyped, boolCondition: Boolean; intField, intCondition: Integer; Value: string);
  public
    { Public declarations }
    OnExpression: TSMDBFExpression;
    OnValue: TSMDBFValue;
    FDefWildCard: string;
    FDefIsSQLBased: Boolean;
  end;

procedure Register;

implementation

{$R *.DFM}
//{$R SMDBFltr.Dcr}
uses SMCnst, SMDBFltrFile;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBFilterDialog]);
end;

var
  ASignStr: array[0..10] of string = ('', '', '', '', '', '', '', '', '', '', '');
  ASign: array[0..10] of string = ('=', '<>',
                                   '<=', '>=',
                                   '<', '>',
                                   'IS NULL', 'IS NOT NULL',
                                   'IN', '', '='{'LIKE'});
  ACondition: array[Boolean, Boolean] of string = ((' OR  ', 'OR'),
                                                   (' AND ', 'AND'));

{ TSMDBFilterDialog }
constructor TSMDBFilterDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaption := 'Filter setup dialog';
  FDataset := nil;
  FAllowedFields := TStringList.Create;
  FCaptionsFields := TStringList.Create;
  FOptions := [foLoadFrom, foSaveAs];
  FWildCard := '%';
  FIsSQLBased := True;
end;

destructor TSMDBFilterDialog.Destroy;
begin
  FAllowedFields.Free;
  FCaptionsFields.Free;

  inherited Destroy;
end;

procedure TSMDBFilterDialog.SetAllowedFields(Value: TStrings);
begin
  FAllowedFields.Assign(Value);
end;

procedure TSMDBFilterDialog.SetCaptionsFields(Value: TStrings);
begin
  FCaptionsFields.Assign(Value);
end;

function TSMDBFilterDialog.GetItemCaption(item: TSMDBFilterItem): string;
begin
  Result := {item.FieldName + '  :  ' + }item.FieldCaption;
end;

function GetIndexBySignStr(str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  str := Trim(str);
  for i := 0 to High(ASignStr) do
    if ASignStr[i] = str then
    begin
      Result := i;
      break
    end;
end;

function GetIndexByName(str: string; lst: TStrings): Integer;
var
  i, Code: Integer;
begin
  Result := -1;
  Val(str, i, Code);
  if Code = 0 then
  begin
    for i := 0 to lst.Count-1 do
      if (TSMDBFilterItem(lst.Objects[i]).FieldIndex = Result) then
      begin
        Result := i;
        break
      end
  end
  else
  begin
    for i := 0 to lst.Count-1 do
      if (TSMDBFilterItem(lst.Objects[i]).FieldName = str) then
      begin
        Result := i;
        break
      end;
  end;
end;

function TSMDBFilterDialog.GetCurrentFilter: string;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(frmFilterDialog) then exit;

  for i := 0 to frmFilterDialog.lbFilter.Items.Count-1 do
    Result := Result + PString(frmFilterDialog.lbFilter.Items[i].Data)^;
  if (Result <> '') then
    Result := ' AND ' + Result;

  if (Result <> '') then
    Result := Copy(Result, 6, Length(Result)-5);
end;

procedure TSMDBFilterDialog.SetCurrentFilter(Value: string);

  function GetIndexBySign(str, AValue: string): Integer;
  var
    i, j: Integer;
    IsFirst: Boolean;
  begin
    str := Trim(str);
    Result := 0;
    IsFirst := True;
    for i := 0 to High(ASign) do
      if (ASign[i] = str) then
      begin
        Result := i;
        j := Pos(FWildCard, AValue);
        if (j < 1) then
          break
        else
        if IsFirst then
          IsFirst := False
        else
          break
      end;
  end;

  function FindSign(str: string): Integer;
  var
    i, j: Integer;
  begin
    Result := 0;
    for i := 0 to High(ASign) do
    begin
      j := Length(ASign[i])+1;
      if (ASign[i] + ' ' = UpperCase(Copy(str, 1, j))) then
      begin
        Result := j+1;
        break
      end
    end;
  end;

var
  i, j, k: Integer;
  str, strExtracted, strSign: string;
  boolANDOR: Boolean;
begin
  if not Assigned(frmFilterDialog) or (Value = '') then exit;

  while (Value <> '') do
  begin
    Value := Trim(Value);
    if (Copy(Value, 1, 4) = 'OR  ') then
    begin
      Delete(Value, 1, 4);
      boolANDOR := False;
    end
    else
    begin
      if (Copy(Value, 1, 4) = 'AND ') then
        Delete(Value, 1, 4);
      boolANDOR := True;
    end;

    j := Pos(' AND ', Value);
    k := Pos(' OR  ', Value);
    if (j = 0) and (k = 0) then
      j := Length(Value)+1
    else
      if ((k < j) or (j = 0)) and (k > 0) then
        j := k;
    if j > 0 then
    begin
      str := Copy(Value, 1, j-1);
      Delete(Value, 1, j);

      {extract a fieldname}
      j := Pos(' ', str);
      strExtracted := Copy(str, 2, j-2);
      Delete(str, 1, j);
      i := GetIndexByName(strExtracted, frmFilterDialog.cbFilterField.Items);

      {extract a condition sign}
//      j := Pos(' ', str);
      j := FindSign(str);
      strSign := Copy(str, 1, j-1);
      str := Copy(str, j, Length(str)-j{-1});
      if (str <> '') then
      begin
        {remove quote in start and end of value}
        if (str[1] = '''') then
          Delete(str, 1, 1);
        j := Length(str);
        if (str[j] = '''') then
          Delete(str, j, 1);

        {remove () in start and end of value}
        if (str[1] = '(') then
          Delete(str, 1, 1);
        j := Length(str);
        if (str[j] = ')') then
          Delete(str, j, 1);

        {Tomasz <tomcmok@polbox.com>:
         remove wildcards in start and end of value}
        if (str[1] = WildCard) then
          Delete(str, 1, 1);
        j := Length(str);
        if (str[j] = WildCard) then
          Delete(str, j, 1);
      end;
//      str := Copy(str, j+2, Length(str)-j-3);

      frmFilterDialog.AddFilter(nil, False, boolANDOR, i, GetIndexBySign(strSign, str), str);
    end
  end;
end;

function TSMDBFilterDialog.Execute: Boolean;
var
  i, j: Integer;
  item: TSMDBFilterItem;
  strFilter: string;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);

//  Result := False;
  if not Assigned(frmFilterDialog) then
    frmFilterDialog := TfrmFilterDialog.Create(Self{Application});
  with frmFilterDialog do
    try
      OnExpression := Self.OnExpression;
      OnValue := Self.OnValue;

      Caption := FCaption;
      FDefWildCard := FWildCard;
      FDefIsSQLBased := IsSQLBased;

      if IsSQLBased then
        ASign[10] := 'LIKE'
      else
        ASign[10] := '=';

      {fill the native strings}
      ASignStr[0] := strEqual;
      ASignStr[1] := strNonEqual;
      ASignStr[2] := strNonMore;
      ASignStr[3] := strNonLess;
      ASignStr[4] := strLessThan;
      ASignStr[5] := strLargeThan;
      ASignStr[6] := strExist;
      ASignStr[7] := strNonExist;
      ASignStr[8] := strIn;
      ASignStr[9] := strBetween;
      ASignStr[10] := strLike;

      for i := 0 to 10 do
        cbFilterCondition.Items.Add(ASignStr[i]);

      ACondition[False, True] := strOR;
      rbOR.Caption := strOR;
      ACondition[True, True] := strAND;
      rbAND.Caption := strAND;

      lblFilterField.Caption := strField;
      lbFilter.Columns.Items[1].Caption := strField;
      lblFilterCondition.Caption := strCondition;
      lbFilter.Columns.Items[2].Caption := strCondition;
      lblFilterValue.Caption := strValue;
      lbFilter.Columns.Items[3].Caption := strValue;

      btnFilterAdd.Caption := strAddToList;
      btnFilterEdit.Caption := strEditInList;
      btnFilterDelete.Caption := strDeleteFromList;
      lblAddFilter.Caption := strAddCondition;
      lblFilter.Caption := strSelection;

      {New constants by Arpad Toth}
      btnFilterSave.Hint := strFSaveAs;
      btnFilterLoad.Hint := strFLoadFrom;
      btnClear.Caption  := SBtnClear;

      btnOk.Caption := SBtnOk;
      btnCancel.Caption := SBtnCancel;

      if Assigned(FOnShow) then
        FOnShow(Self);

      {fill the field list}
      for i := 0 to DataSet.FieldCount - 1 do
        if (AllowedFields.Count = 0) or
           (AllowedFields.IndexOf(DataSet.Fields[i].FieldName) > -1) then
        begin
          item := TSMDBFilterItem.Create;
          item.FieldIndex := cbFilterField.Items.Count;
          item.FieldName := DataSet.Fields[i].FieldName;
          item.FieldCaption := FCaptionsFields.Values[item.FieldName];
          if item.FieldCaption = '' then
            item.FieldCaption := DataSet.Fields[i].DisplayLabel;

          cbFilterField.Items.AddObject(GetItemCaption(item), item);
        end;


      {fill the filtered conditions}
      case FilterMode of
        fmFilterDataset: strFilter := DataSet.Filter;
        fmExpression: strFilter := Expression;
      end;
      SetCurrentFilter(strFilter);

      btnFilterSave.Enabled := (lbFilter.Items.Count>0);

      j := 0;
      if foLoadFrom in Options then
      begin
        btnFilterLoad.Visible := True;
        Inc(j);

        if Assigned(OnLoadFilter) then
          btnFilterLoad.OnClick := OnLoadFilter
      end;
      if foSaveAs in Options then
      begin
        btnFilterSave.Visible := True;
        Inc(j);

        if Assigned(OnSaveFilter) then
          btnFilterSave.OnClick := OnSaveFilter
      end
      else
        if foLoadFrom in Options then
          btnFilterLoad.Left := btnFilterSave.Left;
      lblFilter.Width := lblFilter.Width - j*btnFilterLoad.Width;
      if j > 0 then
        lblFilter.Width := lblFilter.Width - 5;


      Result := (ShowModal = mrOk);
      if Result then
      begin
        {fill a filter string}
        strFilter := GetCurrentFilter();

        case FilterMode of
          fmFilterDataset: begin
                             DataSet.Filtered := False;
                             DataSet.Filter := strFilter;
                             DataSet.Filtered := True;
                           end;
          fmExpression: Expression := strFilter;
        end;

      end;

    finally
      {release field items}
      for i := cbFilterField.Items.Count-1 downto 0 do
      begin
        if Assigned(cbFilterField.Items.Objects[i]) then
          cbFilterField.Items.Objects[i].Free;
      end;

      Free;
      frmFilterDialog := nil;
    end;

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

procedure TfrmFilterDialog.btnFilterAddClick(Sender: TObject);
begin
  if (TComponent(Sender).Tag = 1) then
  begin
    {Tomasz: List index out of bounds fixed}
    if (lbFilter.Selected.Index > -1) and (lbFilter.Items.Count > 0) then
//    if (lbFilter.Selected.Index = 0) and (lbFilter.Items.Count > 0) then
    begin
      AddFilter(lbFilter.Selected, True, rbAND.Checked, cbFilterField.ItemIndex, cbFilterCondition.ItemIndex, edFilterValue.Text);
      btnFilterSave.Enabled := (lbFilter.Items.Count>0);
    end;
  end
  else
    AddFilter(nil, True, rbAND.Checked, cbFilterField.ItemIndex, cbFilterCondition.ItemIndex, edFilterValue.Text);

  rbAND.Checked := True;
  cbFilterField.ItemIndex := -1;
  cbFilterCondition.ItemIndex := -1;
  edFilterValue.Text := '';
end;

procedure TfrmFilterDialog.btnFilterDeleteClick(Sender: TObject);
var
  strWhere: string;
  IsChanged: Boolean;
  intLen: Integer;
begin
  with lbFilter do
  begin
    if (Selected.Index = 0) and (Items.Count > 1) then
    begin
      {we need remove AND/OR clause from WHERE in second item (that will be a first after delete)}
      strWhere := PString(Items[1].Data)^;
      IsChanged := False;

      intLen := Length(ACondition[True, False]);
      if (Copy(strWhere, 1, intLen) = ACondition[True, False]) then
      begin
        IsChanged := True;
        Delete(strWhere, 1, intLen)
      end
      else
      begin
        intLen := Length(ACondition[False, False]);
        if (Copy(strWhere, 1, intLen) = ACondition[False, False]) then
        begin
          IsChanged := True;
          Delete(strWhere, 1, intLen)
        end
      end;

      if IsChanged then
      begin
        {free a memory for previous string}
        DisposeStr(PAnsiString(Items[1].Data));
        {add a new string}
        Items[1].Data := TObject(LongInt(NewStr(strWhere)));
      end;
    end;

    Items.Delete(Selected.Index);
  end;
  btnFilterSave.Enabled := (lbFilter.Items.Count>0);
end;

procedure TfrmFilterDialog.lbFilterChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  btnFilterEdit.Enabled := Assigned(lbFilter.Selected);
  btnFilterDelete.Enabled := Assigned(lbFilter.Selected);
end;

procedure TfrmFilterDialog.cbFilterFieldChange(Sender: TObject);
begin
  btnFilterAdd.Enabled := (cbFilterField.ItemIndex > -1) and
                          (cbFilterCondition.ItemIndex > -1) and
                          ((edFilterValue.Text <> '') or (cbFilterCondition.ItemIndex in [6, 7]));
  btnFilterSave.Enabled := (lbFilter.Items.Count>0);
end;

procedure TfrmFilterDialog.AddFilter(EditLI: TListItem; IsUserTyped, boolCondition: Boolean; intField, intCondition: Integer; Value: string);
var
  ListItem: TListItem;
  strFieldName, strCondition, s1, s2: string;
  intPos: Integer;
begin
  if (intField < 0) then exit;
  
  strFieldName := TSMDBFilterItem(cbFilterField.Items.Objects[intField]).FieldName;
  strCondition := '(' + strFieldName + ' ' + ASign[intCondition];
  if (Assigned(EditLI) and (EditLI.Index > 0)) or
     (not Assigned(EditLI) and (lbFilter.Items.Count > 0)) then
    strCondition := ACondition[boolCondition, False] + strCondition;

  if {IsUserTyped and }Assigned(OnValue) then
    OnValue(Owner, intField, intCondition, Value);

  case intCondition of
    {'NOT IS NULL', 'IS NULL'}
    6, 7: strCondition := strCondition + ' )';
//    6, 7: strCondition := strCondition + ')';
    {'IN'}
    8: strCondition := strCondition + ' (' + Value + '))';
    {'BETWEEN'}
    9: begin
         intPos := Pos(' ', Value);
         if intPos > 0 then
         begin
           s1 := Copy(Value, 1, intPos-1);
           s2 := Copy(Value, intPos+1, Length(Value)- intPos);
         end
         else
         begin
           s1 := Value;
           s2 := '" "';
         end;
         strCondition := strCondition + '>= ' + s1 + ') AND (' + strFieldName + ' <= ' + s2 + ')';
       end;
    10: begin //LIKE
          if FDefIsSQLBased then
            strCondition := strCondition + ' ''' + FDefWildCard + Value + FDefWildCard + ''')'
          else
            strCondition := strCondition + ' ''' + Value + FDefWildCard + ''')';
        end
  else
    strCondition := strCondition + ' ''' + Value + ''')';
  end;

  if {IsUserTyped and }Assigned(OnExpression) then
    OnExpression(Owner, (lbFilter.Items.Count < 1), boolCondition, intField, intCondition, Value, strCondition);

  if Assigned(EditLI) then
  begin
    {delete previos assigned string}
    DisposeStr(PAnsiString(EditLI.Data));

    EditLI.ImageIndex := intField;
    EditLI.Caption := ACondition[boolCondition, True];
    EditLI.SubItems[0] := cbFilterField.Items[intField];
    EditLI.SubItems[1] := cbFilterCondition.Items[intCondition];
    EditLI.SubItems[2] := Value;

    EditLI.Data := TObject(LongInt(NewStr(strCondition)));
    EditLI.Update
  end
  else
  begin
    ListItem := lbFilter.Items.Add;
    ListItem.ImageIndex := intField;
    ListItem.Caption := ACondition[boolCondition, True];
    ListItem.SubItems.Add(cbFilterField.Items[intField]);
    ListItem.SubItems.Add(cbFilterCondition.Items[intCondition]);
    ListItem.SubItems.Add(Value);

    ListItem.Data := TObject(LongInt(NewStr(strCondition)));
  end
end;

procedure TfrmFilterDialog.btnFilterSaveClick(Sender: TObject);
var
  i: Integer;
  s, strIndex: string;
begin
  if frmFilterFileDialog.Execute(fltFile, True) then
    with TIniFile.Create(fltFile) do
      try
        EraseSection('Filter');
        for i := 0 to lbFilter.Items.Count-1 do
        begin
          s := lbFilter.Items[i].Caption;
          if s = strAND then
            s := 'AND'
          else
            s := 'OR';
          strIndex := IntToStr(i);
          WriteString('Filter', 'AndOr' + strIndex, s);
          s := TSMDBFilterItem(cbFilterField.Items.Objects[lbFilter.Items[i].ImageIndex]).FieldName;
          WriteString('Filter', 'Field' + strIndex, s);
          WriteInteger('Filter','Condition' + strIndex, GetIndexBySignStr(lbFilter.Items[i].SubItems[1]));
          WriteString('Filter', 'Value' + strIndex, lbFilter.Items[i].SubItems[2]);
        end;
      finally
        Free;
      end;
end;

procedure TfrmFilterDialog.btnFilterLoadClick(Sender: TObject);
var
  i: Integer;
  lst: TStrings;
  AndOr: Boolean;
begin
  if frmFilterFileDialog.Execute(fltFile, False) then
  begin
    lst := TStringList.Create;
    with TIniFile.Create(fltFile) do
      try
        ReadSection('Filter', lst);
        lbFilter.Items.Clear;
        i := 0;
        while i < (lst.Count-1) do
        begin
          AndOr := (ReadString('Filter', lst[i], '') = 'AND');
          AddFilter(nil, False, AndOr,
                    GetIndexByName(ReadString('Filter', lst[i+1], ''), cbFilterField.Items),
                    GetIndexBySignStr(ASignStr[ReadInteger('Filter', lst[i+2], 0)]),
                    ReadString('Filter', lst[i+3], ''));
          i := i + 4;
        end;
        lbFilter.Refresh;
      finally
        lst.Free;
        Free;
        btnFilterSave.Enabled := (lbFilter.Items.Count>0);
      end;
  end
end;

procedure TfrmFilterDialog.btnClearClick(Sender: TObject);
begin
  lbFilter.Items.Clear;
end;

procedure TfrmFilterDialog.lbFilterClick(Sender: TObject);
begin
  {load selected row to controls}
  if Assigned(lbFilter.Selected) then
  begin
    cbFilterField.ItemIndex := lbFilter.Selected.ImageIndex;
    cbFilterCondition.ItemIndex := cbFilterCondition.Items.IndexOf(lbFilter.Selected.SubItems[1]);
    edFilterValue.Text := lbFilter.Selected.SubItems[2];
    if (lbFilter.Selected.Caption = ACondition[True, True]) then
    begin
      rbAND.Checked := True;
      rbOR.Checked := False;
    end
    else
    begin
      rbAND.Checked := False;
      rbOR.Checked := True;
    end
  end;
end;

end.
