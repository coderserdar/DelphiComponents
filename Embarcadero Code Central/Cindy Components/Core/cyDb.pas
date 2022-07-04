{   Unit cyDB

    Description:
    Unit with functions to use for Database.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyDB;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, SysUtils, Forms, DB, DBClient, Provider, DBGrids, Dialogs, cyStrUtils;

type
  TClientDatasetOpenMode = (omClientDatasetCreated, omClientDatasetOpened, omClientDatasetUpdated);
  TFilterArgumentMode = (faAnd, faOr);

function Open(aClientDataset: TClientDataset; const Filename: String = ''; const SavedFormat: TDataPacketFormat = dfBinary): TClientDatasetOpenMode;
procedure Save(aDataset: TDataSet);
procedure Cancel(aDataset: TDataSet);
function IsEmpty(aDataset: TDataSet): Boolean;
function Edit(aDataset: TDataset; const ErrorMsg: String = ''): Boolean;
function Delete(aDataset: TDataSet; const ErrorMsg: String = ''): Boolean;
function DeleteCurrentRecords(aDataset: TDataSet): Boolean;
procedure DuplicateRecord(aDataset: TDataSet);
procedure SaveAsNewRecord(aDataset: TDataSet);
function CloneFieldValues(DatasetSrc, DatasetDest: TDataSet; const Src_FieldTag: Integer = 0; const Only_FieldTag: Boolean = false): Boolean;
function FindKey(aClientDataset: TClientDataset; Value: String): Boolean;
procedure FindNearest(aClientDataset: TClientDataset; Value: String);

function CloneField(SourceField: TField; aOwner: TComponent): TField;
procedure CopyFieldProperties(Source, Destination: TField);
procedure CopyDatasetProperties(Source, Destination: TDataset; var RsltFields: String);
function ValidFieldForClientDatasetIndex(Field: TField): Boolean;


function GetFilter_FromKeywordsInput(const OnField, aKeywordsInput: String): string;
procedure AddToFilter(var CurrentFilter: String; AddArgument: String; const ArgumentMode: TFilterArgumentMode = faAnd);

implementation

// Does not handle field size changes for now ...
function Open(aClientDataset: TClientDataset; const Filename: String = ''; const SavedFormat: TDataPacketFormat = dfBinary): TClientDatasetOpenMode;
var
  LoadedFileDS, RebuildDS: TClientDataset;
  Stream: TMemoryStream;
  f: Integer;
  aField: TField;
  FieldChanges: Boolean;

      function CloneField(SourceField: TField; Destination: TDataset): TField;
      begin
        Result := TFieldClass(SourceField.ClassType).Create(LoadedFileDS);
        Result.FieldName := SourceField.FieldName;
        Result.FieldKind := SourceField.FieldKind;
        Result.Size := SourceField.Size;

        if Destination <> Nil then
          Result.DataSet := Destination;        // Can' t do on open dataset ...
      end;

begin
  Result := omClientDatasetCreated;

  if Filename = '' then
    aClientDataset.CreateDataSet
  else
    if not FileExists(Filename) then
      aClientDataset.CreateDataSet
    else begin
      Result := omClientDatasetOpened;

      // * Create temporary Clientdataset with current file data * //
      LoadedFileDS := TClientDataset.Create(aClientDataset.Owner);
      Stream := TMemoryStream.Create;

      try
        LoadedFileDS.LoadFromFile(Filename);
        FieldChanges := false;

        // if any field.FieldKind is fkData not exists on LoadedFileDS, we will have an error !
        for f := 0 to aClientDataset.Fields.Count-1 do
          if aClientDataset.Fields[f].FieldKind = fkData then
            if LoadedFileDS.FindField(aClientDataset.Fields[f].FieldName) = Nil then
            begin
              FieldChanges := true;

              aField := CloneField(aClientDataset.Fields[f], Nil);
              LoadedFileDS.Fields.Add(aField);

              // Not needed :
              // LoadedFileDS.FieldDefs.Add(aClientDataset.Fields[f].FieldName, aClientDataset.Fields[f].DataType, aClientDataset.Fields[f].Size, aClientDataset.Fields[f].Required);
              // LoadedFileDS.FieldDefs.Update;
            end
            else
              if LoadedFileDS.FindField(aClientDataset.Fields[f].FieldName).Size <> aClientDataset.Fields[f].Size then
                FieldChanges := true;

        if FieldChanges then
        begin
          // !!! Only first record loaded if LoadedFileDS as dataset provider, so we need to recreate xml file !!!
          Result := omClientDatasetUpdated;

          // * Recreate XML File with new fields * //
          RebuildDS := TClientDataset.Create(aClientDataset.Owner);

          for f := 0 to LoadedFileDS.Fields.Count-1 do
            if aClientDataset.FindField(LoadedFileDS.Fields[f].FieldName) <> Nil then // Field deleted ?
            begin
              aField := CloneField(LoadedFileDS.Fields[f], RebuildDS);
              aField.Size := aClientDataset.FindField(LoadedFileDS.Fields[f].FieldName).Size;
            end;

          RebuildDS.CreateDataSet;

          // Reset LoadedFileDS fields from file :
          LoadedFileDS.Active := false;
          LoadedFileDS.LoadFromFile(Filename);

          // Copy fields :
          while not LoadedFileDS.Eof do
          begin
            RebuildDS.Append;

            {$IFDEF UNICODE}
            RebuildDS.CopyFields(LoadedFileDS);
            {$ELSE}
            CloneFieldValues(LoadedFileDS, RebuildDS);
            {$ENDIF}

            RebuildDS.Post;

            LoadedFileDS.Next;
          end;

          RebuildDS.SaveToStream(Stream, SavedFormat);
        end
        else
          LoadedFileDS.SaveToStream(Stream, SavedFormat);

        Stream.Position := 0;
        aClientDataset.LoadFromStream(Stream);
      finally
        if not aClientDataset.Active then
          aClientDataset.CreateDataSet;

        Stream.Free;
        LoadedFileDS.Free;
      end;
    end;
end;

procedure Save(aDataset: TDataSet);
begin
  if aDataset.State in [DsEdit, DsInsert] then
    aDataset.Post;
end;

procedure Cancel(aDataset: TDataSet);
begin
  if aDataset.State in [DsEdit, DsInsert] then
    aDataset.Cancel;
end;

function IsEmpty(aDataset: TDataSet): Boolean;
begin
  if aDataset.State = dsInsert
  then Result := false
  else Result := aDataset.EOF and aDataset.BOF;
end;

function Edit(aDataset: TDataset; const ErrorMsg: String = ''): Boolean;
begin
  Result := true;

  if (aDataset.State <> dsInsert) and (aDataset.State <> dsEdit) then
    try
      aDataset.Edit;
    except
      Result := false;

      if ErrorMsg <> '' then
        MessageDlg(ErrorMsg, mtError, [mbOk], 0);
    end;
end;

function Delete(aDataset: TDataSet; const ErrorMsg: String = ''): Boolean;
begin
  Result := False;
  if IsEmpty(aDataset) then Exit;

  try
    if aDataset.State = dsInsert
    then aDataset.Cancel
    else aDataset.Delete;

    Result := True;
  except
    if ErrorMsg <> '' then
      MessageDlg(ErrorMsg, mtError, [mbOk], 0);
  end;
end;

function DeleteCurrentRecords(aDataset: TDataSet): Boolean;
var
  Cont: Boolean;
begin
  Result := True;
  Cont   := (not IsEmpty(aDataset));

  while Cont do
    if not Delete(aDataset, '') then
    begin
      Cont   := False;
      Result := False;
    end
    else
      Cont := not IsEmpty(aDataset);
end;

procedure DuplicateRecord(aDataset: TDataSet);
var
  Variants: array Of Variant;
  i : Integer;
begin
  SetLength(Variants, aDataset.Fields.Count);

  for i := 0 to aDataset.Fields.Count-1 do
    Variants[i] := aDataset.Fields[i].Value;

  aDataset.Append;

  for i:= 0 to aDataset.Fields.Count-1 do
    aDataset.Fields[i].Value := Variants[i];
end;

procedure SaveAsNewRecord(aDataset: TDataSet);
var
  Variants : array Of Variant;
  i : Integer;
begin
  SetLength(Variants, aDataset.Fields.Count);

  for i := 0 to aDataset.Fields.Count-1 do
    Variants[i] := aDataset.Fields[i].Value;

  Cancel(aDataset);
  aDataset.Append;

  for i:= 0 to aDataset.Fields.Count-1 do
    aDataset.Fields[i].Value := Variants[i];
end;

function CloneFieldValues(DatasetSrc, DatasetDest: TDataSet; const Src_FieldTag: Integer = 0; const Only_FieldTag: Boolean = false): Boolean;
var
  i :Integer;
  DestField: TField;
begin
  Result := True;

  for i := 0 to DatasetSrc.Fields.Count-1 do
    if (not Only_FieldTag) or (DatasetSrc.Fields[i].Tag = Src_FieldTag) then
      try
        DestField := DatasetDest.FindField(DatasetSrc.Fields[i].FieldName);

        if DestField <> nil then
          if not DestField.ReadOnly then
            if DestField.FieldKind = fkData then
              case DestField.DataType of
                ftBoolean :
                  DestField.AsBoolean := DatasetSrc.Fields[i].AsBoolean;
                else
                  DestField.AsString := DatasetSrc.Fields[i].AsString;
              end;
      except
        Result := False;
      end;
end;

function FindKey(aClientDataset: TClientDataset; Value: String): Boolean;
begin
  try
    Result := aClientDataset.FindKey([Value]);
  except
    Result := false;
  end;
end;

procedure FindNearest(aClientDataset: TClientDataset; Value: String);
begin
  try
    aClientDataset.FindNearest([Value]);
  except
  end;
end;

function CloneField(SourceField: TField; aOwner: TComponent): TField;
begin
  Result := TFieldClass(SourceField.ClassType).Create(aOwner);
  Result.FieldName := SourceField.FieldName;
  Result.FieldKind := SourceField.FieldKind;
  Result.Size := SourceField.Size;
end;

procedure CopyFieldProperties(Source, Destination: TField);
var
  MinValue, MaxValue: Extended;
  Precision: Integer;
begin
  // * Properties * //
  Destination.Alignment := Source.Alignment;
  Destination.DisplayLabel := Source.DisplayLabel;
  Destination.DisplayWidth := Source.DisplayWidth;
  Destination.ReadOnly := Source.ReadOnly;
  Destination.Required := Source.Required;
  Destination.Visible := Source.Visible;

  // * Properties by FieldType* //
  if Source is TStringField then  // TWideStringField descends from TStringField
    if Destination is TStringField then
      TStringField(Destination).EditMask := TStringField(Source).EditMask;

  if Source is TNumericField then // TIntegerField, TFloatField, TBCDField, TFMTBCDField etc ... descends from TNumericField
    if Destination is TNumericField then
    begin
      TNumericField(Destination).EditMask := TNumericField(Source).EditMask;
      TNumericField(Destination).EditFormat := TNumericField(Source).EditFormat;
      TNumericField(Destination).DisplayFormat := TNumericField(Source).DisplayFormat;
    end;

  MinValue  := 0;
  MaxValue  := 0;
  Precision := 0;

  if Source is TIntegerField then         // TSmallintField, TShortintField, TByteField, TWordField and TAutoIncField descends from TIntegerField
  begin
    MinValue := TIntegerField(Source).MinValue;
    MaxValue := TIntegerField(Source).MaxValue;
  end;

  {$IFDEF UNICODE}
  if Source is TLongWordField then
  begin
    MinValue := TLongWordField(Source).MinValue;
    MaxValue := TLongWordField(Source).MaxValue;
  end;
  {$ENDIF}

  if Source is TLargeintField then
  begin
    MinValue := TLargeintField(Source).MinValue;
    MaxValue := TLargeintField(Source).MaxValue;
  end;

  if Source is TFloatField then           // TCurrencyField descends from TFloatField
  begin
    MinValue := TFloatField(Source).MinValue;
    MaxValue := TFloatField(Source).MaxValue;
    Precision := TFloatField(Source).Precision;
  end;

  // ***

  if Destination is TIntegerField then
  begin
    TIntegerField(Destination).MinValue := Trunc(MinValue);
    TIntegerField(Destination).MaxValue := Trunc(MaxValue);
  end;

  {$IFDEF UNICODE}
  if Destination is TLongWordField then
  begin
    TLongWordField(Destination).MinValue := Trunc(MinValue);
    TLongWordField(Destination).MaxValue := Trunc(MaxValue);
  end;
  {$ENDIF}

  if Destination is TLargeintField then
  begin
    TLargeintField(Destination).MinValue := Trunc(MinValue);
    TLargeintField(Destination).MaxValue := Trunc(MaxValue);
  end;

  if Destination is TFloatField then
  begin
    TFloatField(Destination).MinValue := MinValue;
    TFloatField(Destination).MaxValue := MaxValue;
    TFloatField(Destination).Precision := Precision;
  end;

  if Source is TBooleanField then  // TWideStringField descends from TStringField
    if Destination is TBooleanField then
      TBooleanField(Destination).DisplayValues := TBooleanField(Source).DisplayValues;

  if Source is TDateTimeField then  // TDateField, TTimeField  descends from TDateTimeField
    if Destination is TDateTimeField then
    begin
      TDateTimeField(Destination).DisplayFormat := TDateTimeField(Source).DisplayFormat;
      TDateTimeField(Destination).EditMask := TDateTimeField(Source).EditMask;
    end;

  if Source is TSQLTimeStampField then
    if Destination is TSQLTimeStampField then
    begin
      TSQLTimeStampField(Destination).DisplayFormat := TSQLTimeStampField(Source).DisplayFormat;
      TSQLTimeStampField(Destination).EditMask := TSQLTimeStampField(Source).EditMask;
    end;

  // * Events * //
  Destination.OnChange := Source.OnChange;
  Destination.OnGetText := Source.OnGetText;
  Destination.OnSetText := Source.OnSetText;
  Destination.OnValidate := Source.OnValidate;
end;

procedure CopyDatasetProperties(Source, Destination: TDataset; var RsltFields: String);
var
  f: Integer;
  SourceField, DestinationField: TField;
begin
  RsltFields := '';

  // * Properties * //
  Destination.Filter := Source.Filter;
  Destination.Filtered := Source.Filtered;
  Destination.FilterOptions := Source.FilterOptions;

  {$IFDEF UNICODE}
  if Source is TCustomClientDataSet then
    if Destination is TCustomClientDataSet then
    begin
      TCustomClientDataSet(Destination).IndexFieldNames := TCustomClientDataSet(Source).IndexFieldNames;
    end;
  {$ENDIF}

  // * Field properties * //
  for f := 0 to Source.Fields.Count-1 do
  begin
    SourceField := Source.Fields[f];

    case SourceField.FieldKind of
      fkData:
      begin
        if RsltFields <> '' then
          RsltFields := RsltFields + ', ';

        RsltFields := RsltFields + SourceField.FieldName;
      end;

      fkCalculated:            // Add calculated field !
        if not Destination.Active then
          if Destination.FindField(SourceField.FieldName) = Nil then
          begin
            DestinationField := CloneField(SourceField, Destination.Owner);
            DestinationField.DataSet := Destination;
            DestinationField.Name := Destination.Name + DestinationField.FieldName;
          end;

      fkLookup:
      begin
        if not Destination.Active then
          if Destination.FindField(SourceField.FieldName) = Nil then
          begin
            DestinationField := CloneField(SourceField, Destination.Owner);
            DestinationField.DataSet := Destination;
            DestinationField.Name := Destination.Name + DestinationField.FieldName;

            DestinationField.LookupCache := SourceField.LookupCache;
            DestinationField.LookupDataSet := SourceField.LookupDataSet;
            DestinationField.LookupKeyFields := SourceField.LookupKeyFields;
            DestinationField.LookupResultField := SourceField.LookupResultField;
            DestinationField.KeyFields := SourceField.KeyFields;
          end;
      end;
    end;


    DestinationField := Destination.FindField(SourceField.FieldName);

    if Assigned(DestinationField) then
      CopyFieldProperties(SourceField, DestinationField);
  end;

  // * Events * //
  if not Assigned(Source.BeforeCancel)   then Destination.BeforeCancel   := Source.BeforeCancel;
  if not Assigned(Source.BeforeClose)    then Destination.BeforeClose    := Source.BeforeClose;
  if not Assigned(Source.BeforeDelete)   then Destination.BeforeDelete   := Source.BeforeDelete;
  if not Assigned(Source.BeforeEdit)     then Destination.BeforeEdit     := Source.BeforeEdit;
  if not Assigned(Source.BeforeInsert)   then Destination.BeforeInsert   := Source.BeforeInsert;
  if not Assigned(Source.BeforeOpen)     then Destination.BeforeOpen     := Source.BeforeOpen;
  if not Assigned(Source.BeforePost)     then Destination.BeforePost     := Source.BeforePost;
  if not Assigned(Source.BeforeRefresh)  then Destination.BeforeRefresh  := Source.BeforeRefresh;
  if not Assigned(Source.BeforeScroll)   then Destination.BeforeScroll   := Source.BeforeScroll;
  if not Assigned(Source.AfterCancel)    then Destination.AfterCancel    := Source.AfterCancel;
  if not Assigned(Source.AfterClose)     then Destination.AfterClose     := Source.AfterClose;
  if not Assigned(Source.AfterDelete)    then Destination.AfterDelete    := Source.AfterDelete;
  if not Assigned(Source.AfterEdit)      then Destination.AfterEdit      := Source.AfterEdit;
  if not Assigned(Source.AfterInsert)    then Destination.AfterInsert    := Source.AfterInsert;
  if not Assigned(Source.AfterOpen)      then Destination.AfterOpen      := Source.AfterOpen;
  if not Assigned(Source.AfterPost)      then Destination.AfterPost      := Source.AfterPost;
  if not Assigned(Source.AfterRefresh)   then Destination.AfterRefresh   := Source.AfterRefresh;
  if not Assigned(Source.AfterScroll)    then Destination.AfterScroll    := Source.AfterScroll;
  if not Assigned(Source.OnCalcFields)   then Destination.OnCalcFields   := Source.OnCalcFields;
  if not Assigned(Source.OnDeleteError)  then Destination.OnDeleteError  := Source.OnDeleteError;
  if not Assigned(Source.OnEditError)    then Destination.OnEditError    := Source.OnEditError;
  if not Assigned(Source.OnFilterRecord) then Destination.OnFilterRecord := Source.OnFilterRecord;
  if not Assigned(Source.OnNewRecord)    then Destination.OnNewRecord    := Source.OnNewRecord;
  if not Assigned(Source.OnPostError)    then Destination.OnPostError    := Source.OnPostError;
end;

function ValidFieldForClientDatasetIndex(Field: TField): Boolean;
begin
  Result := false;

  if Assigned(Field) then
    if Field.FieldKind = fkData then
      if not (Field is TBlobField) then
        Result := true;
end;

function GetFilter_FromKeywordsInput(const OnField, aKeywordsInput: String): string;
var
  i, LengthaKeywordsInput: Integer;
  CurrentWord: string;
  CurrentWordIsExpression, CurrentWordTerminated: Boolean;
begin
  Result := '';

  if aKeywordsInput = '' then Exit;

  CurrentWord := '';
  CurrentWordIsExpression := false;
  LengthaKeywordsInput := Length(aKeywordsInput);

  // Add filter for each word / expression :
  for i := 1 to Length(aKeywordsInput) do
  begin
    if CurrentWordIsExpression
    then CurrentWordTerminated := aKeywordsInput[i] = '"'
    else CurrentWordTerminated := aKeywordsInput[i] = ' ';

    if not CurrentWordTerminated then
    begin
      if aKeywordsInput[i] = '"'
      then CurrentWordIsExpression := true
      else CurrentWord := CurrentWord + aKeywordsInput[i];

      if i = LengthaKeywordsInput then
        CurrentWordTerminated := true;
    end;

    if CurrentWordTerminated then
    begin
      CurrentWordIsExpression := false;

      if CurrentWord <> '' then
      begin
        AddToFilter(Result,  OnField + ' LIKE ' + QuotedStr('%' + CurrentWord + '%'), faAnd);
        CurrentWord := '';
      end;
    end;
  end;
end;

procedure AddToFilter(var CurrentFilter: String; AddArgument: String; const ArgumentMode: TFilterArgumentMode = faAnd);
begin
  if AddArgument = '' then Exit;

  if CurrentFilter <> '' then
  begin
    if AddArgument[1] <> '(' then
      AddArgument := '(' + AddArgument + ')';

    if CurrentFilter[1] <> '(' then
      CurrentFilter := '(' + CurrentFilter + ')';

    if ArgumentMode = faAnd
    then CurrentFilter := CurrentFilter + ' AND ' + AddArgument
    else CurrentFilter := CurrentFilter + ' OR ' + AddArgument;
  end
  else
    CurrentFilter := AddArgument;
end;

end.
