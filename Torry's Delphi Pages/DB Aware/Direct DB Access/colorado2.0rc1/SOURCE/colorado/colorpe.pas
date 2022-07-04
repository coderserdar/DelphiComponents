{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit colorpe;

interface

{$I CDEFINES.INC}

uses Windows, SysUtils, Classes, Dialogs, Graphics,
     Registry,
{$IFDEF VCL60}
     Variants,
     DesignIntf,
     DesignEditors,
     VCLEditors,
{$ELSE}
     DsgnIntf,
{$ENDIF}
     Controls, Forms, TypInfo, Db;

procedure Register;

implementation
uses colorado, cdlink, cadodb, cschema, cfields;


type

{$IFDEF VCL50}

{:-- TComponentRequiredProperty category}

{$IFDEF VCL60}
  TRequiredComponentProperty = class(TComponentProperty, ICustomPropertyDrawing)
{$ELSE}
  TRequiredComponentProperty = class(TComponentProperty)
{$ENDIF}
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); {$IFNDEF VCL60} override; {$ENDIF}
    {$IFDEF VCL60}
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    {$ENDIF}
  end;

{:-- TStringRequiredProperty category}

{$IFDEF VCL60}
  TRequiredStringProperty = class(TStringProperty, ICustomPropertyDrawing)
{$ELSE}
  TRequiredStringProperty = class(TStringProperty)
{$ENDIF}
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); {$IFNDEF VCL60} override; {$ENDIF}
    {$IFDEF VCL60}
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    {$ENDIF}
  end;

{$IFNDEF VCL60}
{:-- Cursor Services category}

  TCursorServicesCategory = class(TPropertyCategory)
  public
    class function Name: String; override;
  end;

{$ENDIF VCL60}

{$ELSE}
  TRequiredStringProperty = class(TStringProperty);
{$ENDIF}


{:-- TConnectionStringProperty}

  TConnectionStringProperty = class(TRequiredStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{:-- TSchemaItemsProperty}

  TSchemaItemsProperty = class(TRequiredStringProperty)
  private
{$IFDEF VCL60}
    FStringsPropertyEditor: IProperty;
    procedure FindPropertyEditor(const Prop: IProperty);
{$ELSE}
    FStringsPropertyEditor: TPropertyEditor;
    procedure FindPropertyEditor(Prop: TPropertyEditor);
{$ENDIF}
  public
    function AutoFill: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{:-- TProviderProperty}

  TProviderProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{:-- TFilterProperty}

  TFilterProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue (const Value: string); override;
  end;

{:-- TTimeoutProperty}

  TTimeoutProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue (const Value: string); override;
  end;


function GetSourceType(Source: TObject): TSourceType;
begin
  Result := stTable;
  if Source is TCTable then
     if TCTable(Source).TableDirect
        then Result := stTableDirect
        else Result := stTable;
  if Source is TCQuery then Result := stText;
  if Source is TCStoredProc then Result := stStoredProc;
  if Source is TCSchema then Result := stSchema;
  if Source is TCPersistedRecordset then Result := stPersistedRecordset;
  if Source is TRecordset then Result := TRecordset(Source).SourceType; 
end;

{:-- TConnectionStringProperty}

function TConnectionStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TConnectionStringProperty.Edit;
var FCDataLink : TCDataLink;
    FDSNType   : string;
begin
  FCDataLink := TCDataLink.Create(Application);
  Screen.Cursor := crHourGlass;
  if Pos('FILE NAME', AnsiUpperCase(GetStrValue)) > 0 then
     begin
       FCDataLink.FILENAME.Checked := TRUE;
       FCDataLink.FILENAMEEDIT.Text := GetConnectionStringParam(GetStrValue, 'FILE NAME');
     end else
           if Pos('OLEDBDSN', AnsiUpperCase(GetStrValue)) > 0 then
              begin
                FCDataLink.DSN.Checked := TRUE;
                FCDataLink.DSNCOMBO.Text := GetConnectionStringParam(GetStrValue, 'OLEDBDSN');
                if AnsiUpperCase(GetConnectionStringParam(GetStrValue, 'OLEDBDSNTYPE')) = 'SYSTEM'
                   then FCDataLink.DSNTYPE.ItemIndex := 1;
              end else
                    FCDataLink.CONNECTIONSTRINGEDIT.Text := GetStrValue;
  Screen.Cursor := crArrow;
  if FCDataLink.ShowModal = mrOK then
     begin
       if FCDataLink.FILENAME.Checked
          then SetStrValue('FILE NAME=' + FCDataLink.FILENAMEEDIT.Text)
          else if FCDataLink.DSN.Checked then
                  begin
                    if FCDataLink.DSNType.ItemIndex = 0
                       then FDSNType := 'USER'
                       else FDSNType := 'SYSTEM';
                    SetStrValue('OLEDBDSN=' + FCDataLink.DSNCOMBO.Text + ';OLEDBDSNTYPE=' + FDSNType);
                  end else SetStrValue(FCDataLink.CONNECTIONSTRINGEDIT.Text);
     end;
  FCDataLink.Free;
end;

{:-- TInternalPropertyComponent}

type TInternalPropertyComponent = class(TComponent)
     private
       FModified: Boolean;
       FStrings: TStringList;
       procedure SetStrings(Strings: TStringList);
       procedure OnChange(Sender: TObject);
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
     published
       property Strings: TStringList read FStrings write SetStrings;
     end;

procedure TInternalPropertyComponent.SetStrings(Strings: TStringList);
begin
  FStrings.Assign(Strings);
end;

procedure TInternalPropertyComponent.OnChange(Sender: TObject);
begin
  FModified := TRUE;
end;

constructor TInternalPropertyComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FStrings.OnChange := OnChange;
end;

destructor TInternalPropertyComponent.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

{:-- TSchemaItemsProperty}

function TSchemaItemsProperty.AutoFill: Boolean;
begin
  Result := FALSE;
end;

function TSchemaItemsProperty.GetAttributes: TPropertyAttributes;
begin
  if (GetSourceType(GetComponent(0)) = stText)
     or (GetSourceType(GetComponent(0)) = stPersistedRecordset)
     then Result := [paDialog, paReadOnly]
     else Result := [paValueList, paSortList];
  if (UpperCase(GetName) = 'INDEXNAME') then
     if  (GetSourceType(GetComponent(0)) = stTable)
     or  (GetSourceType(GetComponent(0)) = stTableDirect)
         then Result := [paValueList] else Result := [];
end;

{$IFDEF VCL60}
procedure TSchemaItemsProperty.FindPropertyEditor(const Prop: IProperty);
begin
  if Prop.GetPropInfo.Name = 'Strings'
     then FStringsPropertyEditor := Prop;
end;
{$ELSE}
procedure TSchemaItemsProperty.FindPropertyEditor(Prop: TPropertyEditor);
begin
  if Prop.ClassName = 'TStringListProperty'
     then FStringsPropertyEditor := Prop
     else Prop.Free;
end;
{$ENDIF}

procedure TSchemaItemsProperty.Edit;
var FPropEditComp  : TInternalPropertyComponent;
    FOpenDialog    : TOpenDialog;
    {$IFDEF VCL60}
       FComponentList : TDesignerSelections;
    {$ELSE}
       {$IFDEF VCL50}
          FComponentList : TDesignerSelectionList;
       {$ELSE}
          FComponentList : TComponentList;
       {$ENDIF}
    {$ENDIF}
begin
  if (UpperCase(GetName) = 'INDEXNAME') then Exit;
  with TCustomRecordset(GetComponent(0)) do
  begin
    if GetSourceType(GetComponent(0)) = stText then
       begin
         FStringsPropertyEditor := nil;
         {$IFDEF VCL60}
            FComponentList := TDesignerSelections.Create;
         {$ELSE}
            {$IFDEF VCL50}
               FComponentList := TDesignerSelectionList.Create;
            {$ELSE}
               FComponentList := TComponentList.Create;
            {$ENDIF}
         {$ENDIF}
         FPropEditComp := TInternalPropertyComponent.Create(Application);
         {$IFDEF VCL60}
            IDesignerSelections(FComponentList).Add(FPropEditComp);
         {$ELSE}
            FComponentList.Add(FPropEditComp);
         {$ENDIF}
         GetComponentProperties(FComponentList, tkAny, Self.Designer, FindPropertyEditor);
         FPropEditComp.FStrings.Assign(SourceAsStringLines);
         FPropEditComp.FModified := FALSE;
         if FStringsPropertyEditor <> nil then
            begin
              FStringsPropertyEditor.Edit;
              {$IFNDEF VCL60}
              FStringsPropertyEditor.Free;
              {$ENDIF}
            end;
         FComponentList.Free;
         if FPropEditComp.FModified then
            begin
              SourceAsStringLines.Assign(FPropEditComp.FStrings);
              if Owner is TCustomForm then TCustomForm(Owner).Designer.Modified;
            end;
         FPropEditComp.Free;
       end;
    if GetSourceType(GetComponent(0)) = stPersistedRecordset then
       begin
         FOpenDialog := TOpenDialog.Create(Application);
         FOpenDialog.Filter := 'XML format|*.xml|Advanced Data Tablegram format|*.adtg|Any file|*.*';
         FOpenDialog.Options := FOpenDialog.Options + [ofPathMustExist, ofFileMustExist];
         FOpenDialog.FileName := GetStrValue;
         try
           if FOpenDialog.Execute then SetStrValue(FOpenDialog.FileName);
         finally
           FOpenDialog.Free;
         end;
       end;
  end;
end;

function TSchemaItemsProperty.GetValue: string;
begin
  if (GetSourceType(GetComponent(0)) = stText)
     and
     (UpperCase(GetName) <> 'INDEXNAME')
     then Result := '[SQL Script]'
     else Result := inherited GetValue;
end;

procedure TSchemaItemsProperty.GetValues(Proc: TGetStrProc);
var FSchema       : Variant;
    i             : Integer;
    FCValues      : Variant;
    LastIndexItem : WideString;
begin
 if TCustomRecordset(GetComponent(0)).Connection <> nil
    then if GetSourceType(GetComponent(0)) <> stSchema
         then with TCustomRecordset(GetComponent(0)).Connection do
              begin
                try
                  if not Active then Exit;
                  if GetSourceType(GetComponent(0)) = stStoredProc
                     then
                       if (UpperCase(GetName) <> 'INDEXNAME')
                          then FSchema := Variant(ConnectionObject).OpenSchema(adSchemaProcedures)
                          else Exit
                     else
                       if (UpperCase(GetName) = 'INDEXNAME') then
                          begin
                            FCValues := VarArrayCreate([0, 4], varVariant);
                            FCValues [0] := Unassigned;
                            FCValues [1] := Unassigned;
                            FCValues [2] := Unassigned;
                            FCValues [3] := Unassigned;
                            if GetComponent(0) is TRecordset
                               then FCValues [4] := TRecordset(GetComponent(0)).Source;
                            if GetComponent(0) is TCTable
                               then FCValues [4] := TCTable(GetComponent(0)).TableName;
                            FSchema := Variant(ConnectionObject).OpenSchema(adSchemaIndexes, FCValues);
                            FCValues := Unassigned;
                          end
                            else FSchema := Variant(ConnectionObject).OpenSchema(adSchemaTables);
                except
                  FSchema := Unassigned;
                  Exit;
                end;
                LastIndexItem := '';
                while not FSchema.EOF do
                      begin
                        if GetSourceType(GetComponent(0)) = stStoredProc
                           then begin
                                  if VarType(FSchema.Fields ['PROCEDURE_NAME'].Value) > 1 then
                                     begin
                                       Proc(FSchema.Fields ['PROCEDURE_NAME'].Value);
                                     end;
                                end
                           else
                             if (UpperCase(GetName) = 'INDEXNAME')
                                then
                                  begin
                                     if VarType(FSchema.Fields ['INDEX_NAME'].Value) > 1 then
                                        begin
                                          if FSchema.Fields ['INDEX_NAME'].Value <> OleVariant(LastIndexItem)
                                             then Proc(FSchema.Fields ['INDEX_NAME'].Value);
                                          LastIndexItem := FSchema.Fields ['INDEX_NAME'].Value;
                                        end
                                  end else if VarType(FSchema.Fields ['TABLE_NAME'].Value) > 1 then
                                              if (VarType(FSchema.Fields ['TABLE_TYPE'].Value) > 1)
                                                 and
                                                 ( (FSchema.Fields ['TABLE_TYPE'].Value = 'TABLE') or (FSchema.Fields ['TABLE_TYPE'].Value = 'VIEW'))
                                                 then begin
                                                        Proc(FSchema.Fields ['TABLE_NAME'].Value);
                                                      end;
                        FSchema.MoveNext;
                      end;
                FSchema.Close;
                FSchema := Unassigned;
              end
              else begin
                     if (UpperCase(GetName) = 'INDEXNAME') then Exit;
                     for i := 0 to SchemaCount - 1 do Proc(SchemaItems [i]._Type);
                   end;
end;



{:-- TProviderProperty}

function TProviderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TProviderProperty.GetValues(Proc: TGetStrProc);
var key: HKEY;
    buffer: array [0..254] of char;
    ProviderName: array [0..254] of char;
    dwsize: DWORD;
    lsize: LongInt;
    subkey: HKEY;
    subkey1: HKEY;
    n: Integer;
begin
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'CLSID', 0, KEY_READ, key) = ERROR_SUCCESS then
     begin
       dwsize := 255;
       n := 0;
       while RegEnumKeyEx(key, n, buffer, dwsize, nil, nil, nil, nil) = ERROR_SUCCESS do
             begin
               inc(n);
               dwsize := 255;
               if RegOpenKeyEx(key, buffer, 0, KEY_READ, subkey) = ERROR_SUCCESS then
                  begin
                    if RegOpenKeyEx(subkey, 'OLE DB Provider', 0, KEY_READ, subkey1) = ERROR_SUCCESS then
                       begin
                         lsize := 255;
                         if RegQueryValue(subkey, nil, ProviderName, lsize) = ERROR_SUCCESS
                            then Proc(StrPas(ProviderName));
                         RegCloseKey(subkey1);
                       end;
                    RegCloseKey(subkey);
                  end
             end;
       RegCloseKey(key);
     end;
end;

{:-- TFilterProperty}

function TFilterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TFilterProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('ftFilterNone');
  Proc('ftFilterPendingRecords');
  Proc('ftFilterAffectedRecords');
  Proc('ftFilterFetchedRecords');
  Proc('ftFilterConflictingRecords');
end;

function TFilterProperty.GetValue: string;
  function FilterAsInteger(Value: string): Integer;
  begin
    Result := -1;
    if Value = ftFilterNone then Result := 0;
    if Value = ftFilterPendingRecords then Result := 1;
    if Value = ftFilterAffectedRecords then Result := 2;
    if Value = ftFilterFetchedRecords then Result := 3;
    if Value = ftFilterConflictingRecords then Result := 4;
  end;
begin
  if FilterAsInteger(GetStrValue) <> -1  then
     begin
       Result := 'ftFilterNone';
       if GetStrValue = ftFilterPendingRecords then Result := 'ftFilterPendingRecords';
       if GetStrValue = ftFilterAffectedRecords then Result := 'ftFilterAffectedRecords';
       if GetStrValue = ftFilterFetchedRecords then Result := 'ftFilterFetchedRecords';
       if GetStrValue = ftFilterConflictingRecords then Result := 'ftFilterConflictingRecords';
     end else Result := GetStrValue;
end;

procedure TFilterProperty.SetValue (const Value: string);
begin
  if (LowerCase(Value) = 'ftfilterpendingrecords')
     then SetStrValue(ftFilterPendingRecords)
     else
       if (LowerCase(Value) = 'ftfilteraffectedrecords')
          then SetStrValue(ftFilterAffectedRecords)
          else
            if (LowerCase(Value) = 'ftfilterfetchedrecords')
            then SetStrValue(ftFilterFetchedRecords)
            else
              if (LowerCase(Value) = 'ftfilterconflictingrecords')
              then SetStrValue(ftFilterConflictingRecords)
              else
                if (LowerCase(Value) = 'ftfilternone')
                then SetStrValue(ftFilterNone)
                else
                  if Value <> ''
                  then SetStrValue(Value)
                  else
                    SetStrValue(ftFilterNone);
end;

{:-- TTimeoutProperty}

function TTimeoutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TTimeoutProperty.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TCustomRecordset then Proc('tmSuperior');
  Proc('tmEndless');
end;

function TTimeoutProperty.GetValue: string;
var v: Integer;
begin
  v := GetOrdValue;
  case v of
   -1: Result := 'tmSuperior';
    0: Result := 'tmEndless';
  else Result := IntToStr(v);
  end;
  if (GetComponent(0) is TConnection)
     and (v = -1)
     then Result := '-1';
end;

procedure TTimeoutProperty.SetValue (const Value: string);
begin
  if (GetComponent(0) is TConnection)
     and (UpperCase(Value) = 'TMSUPERIOR')
     then Exit;
  if UpperCase(Value) = 'TMSUPERIOR'
     then SetOrdValue(-1)
     else if UpperCase(Value) = 'TMENDLESS'
             then SetOrdValue(0)
             else SetOrdValue(StrToInt(Value));
end;


{$IFDEF VCL50}

procedure DrawRequiredProp(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean; AName: string);
var PrevFontStyle: TFontStyles;
begin
  PrevFontStyle := ACanvas.Font.Style;
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, AName);
  ACanvas.Font.Style := PrevFontStyle;
end;

procedure DrawStandardProp(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean; AName: string);
begin
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, AName);
end;

{:-- TComponentRequiredProperty}

procedure TRequiredComponentProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DrawRequiredProp(ACanvas, ARect, ASelected, GetName);
end;

{$IFDEF VCL60}
procedure TRequiredComponentProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DrawStandardProp(ACanvas, ARect, ASelected, GetValue);
end;
{$ENDIF}

{:-- TStringRequiredProperty}

procedure TRequiredStringProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if UpperCase(GetName) = 'INDEXNAME'
     then {$IFNDEF VCL60} inherited DrawPropName(ACanvas, ARect, ASelected)
          {$ELSE} DrawStandardProp(ACanvas, ARect, ASelected, GetName) {$ENDIF}
     else DrawRequiredProp(ACanvas, ARect, ASelected, GetName);
end;

{$IFDEF VCL60}
procedure TRequiredStringProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DrawStandardProp(ACanvas, ARect, ASelected, GetValue);
end;
{$ENDIF}

{$IFNDEF VCL60}
class function TCursorServicesCategory.Name: string;
begin
  Result := 'Cursor Services';
end;
{$ENDIF VCL60}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('colorADO', [TConnection,
                                  TRecordset,
                                  TCTable,
                                  TCQuery,
                                  TCStoredProc,
                                  TCSchema,
                                  TCPersistedRecordset]);
  RegisterPropertyEditor(TypeInfo(string), TCustomRecordset, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomRecordset, 'CommandTimeout', TTimeoutProperty);
  RegisterPropertyEditor(TypeInfo(string), TRecordset, 'Source', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCTable, 'TableName', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCPersistedRecordset, 'FileName', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCStoredProc, 'StoredProcName', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCSchema, 'SchemaType', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRecordset, 'IndexName', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCTable, 'IndexName', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TCQuery, 'SQL', TSchemaItemsProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TConnection, 'ConnectionString', TConnectionStringProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TConnection, 'Provider', TProviderProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TConnection, 'CommandTimeout', TTimeoutProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TConnection, 'ConnectionTimeout', TTimeoutProperty);
  {$IFDEF VCL50}
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomRecordset, 'Connection', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomRecordset, 'Database', TRequiredComponentProperty);
  {$IFDEF VCL60}
     RegisterPropertiesInCategory('Cursor Services', TCustomRecordset, ['CursorLocation', 'CursorType']);
     RegisterPropertiesInCategory('Database', TCustomRecordset, ['Connection', 'Database']);
  {$ELSE}
     RegisterPropertiesInCategory(TCursorServicesCategory, TCustomRecordset, ['CursorLocation', 'CursorType']);
     RegisterPropertiesInCategory(TDatabaseCategory, TCustomRecordset, ['Connection', 'Database']);
  {$ENDIF}
  {$ENDIF}
end;


end.

