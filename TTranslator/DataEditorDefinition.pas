{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: DataEditorDefinition.pas,v 1.20 2002/12/30 09:32:10 laa Exp $ }

{-------------------------------------------------------------------------
  DataEditorDefinition

  What

  Company          Polycon
  Authors          MVJ
-------------------------------------------------------------------------}
unit DataEditorDefinition;

interface

uses
  Classes, CommonLib,
  DataElements, Criteria, StandardView;

type
  TListType = (ltFirst, ltLast, ltRunning, ltTab, ltOther, ltDistribute);

  TFieldOrganizer = class
  private
    FLists : TList;
    FSortedList : TList;
    FSortedFieldList : TFieldList;
    FFirst : TFieldList;
    FLast : TFieldList;
    FRunning : TFieldList;
    FTab : TFieldList;
    FOther : TFieldList;
    FDistibute : TFieldList;
    function GetCount : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetList : TList;
    function GetFieldList : TFieldList;
    function ContainsField(AField : TDataField; var AList : TFieldList; var idx : Integer) : Boolean;
//    procedure AddField(AField : TDataField; AListType : TListType; RemoveExistent : Boolean);
    procedure AddField(AField : TDataField; AListType : TListType; RemoveExistent : Boolean);

    property Count : Integer read GetCount;
    property Lists : TList read FLists;
  end;

  TDataEditorDefinition = class
  private
  protected
    FReadOnly : Boolean;
    FOpenCriteria : TCondition;
    FInternalCriteria  : TCriteria;
    FKeys : TFieldOrganizer;// TFieldList;
    FDefaultStandardView : TSingletonStandardView;
    FViewCommonKeys : Boolean;
    FDisabledFieldList : TDefaultValueFieldList;
    FEditSubtotals : Boolean;
    FConstantBufferEdit : Boolean;
    FConstantBufferKey : TKeyField;

    FHideKeyList : TFieldList;
    FHideFieldList : TFieldList;
    FOwnDisabledFieldList : TDefaultValueFieldList;
    FSaveToAuxtableFields : TDataFieldSet;

    procedure SetOpenCriteria(ACriteria : TCondition);
    procedure SetInternalCriteria(ACriteria : TCriteria);
    function GetSaveToAuxTableFields : TDataFieldSet;
    procedure SetSaveToAuxTableFields(Fields : TDataFieldSet);
  public
    constructor Create;
    destructor Destroy; override;
    {/** Add a key to the key list for this tabel */}
//    procedure AddKey(aKey : TKeyField);
    {/** Insert a key to the key list for this tabel */}
//    procedure InsertKey(idxKey : Integer; aKey : TKeyField);
    {/** Pointer to a list decideing in what order the keys should appear onscreen */}
    property Keys : TFieldOrganizer read FKeys;
    {/** Pointer to the list of keys not to be shown at all */}
    property HideKeyList : TFieldList read FHideKeyList;
    {/** Pointer to the list of fields (non keys) not to be shown at all */}
    property HideFieldList : TFieldList read FHideFieldList;
    {/** Pointer to the list of keys not to be shown at all */}
    property DisabledFieldList : TDefaultValueFieldList read FDisabledFieldList write FDisabledFieldList;
    {/** Pointer to the list of fields that should be saved directly to the auxtable */}
    property SaveToAuxTableFields : TDataFieldSet read GetSaveToAuxTableFields write SetSaveToAuxTableFields;
    {/** Choose if the budget should be read-only or read-write */}
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    {/** Pointer to the selection criteria for the budget to be opened */}
    property OpenCriteria : TCondition read FOpenCriteria write SetOpenCriteria;
    {/** Pointer to the criteria containing only the users choises for the budget to be opened */}
    property InternalCriteria : TCriteria read FInternalCriteria write SetInternalCriteria;
    {/** Pointer to object decribing the default screen contents on page and row level */}
    property DefaultStandardView : TSingletonStandardView read FDefaultStandardView write FDefaultStandardView;
    {/** Should keys that are common for all rows in the opened table be shown */}
    property ViewCommonKeys : Boolean read FViewCommonKeys write FViewCommonKeys;
    property ConstantBufferEdit : Boolean read FConstantBufferEdit write FConstantBufferEdit;
    property ConstantBufferKey : TKeyField read FConstantBufferKey write FConstantBufferKey;
    {/** Should we allow edit of subtotals */}
    property EditSubtotals : Boolean read FEditSubtotals write FEditSubtotals;
  end;

implementation

uses
  SysUtils;

{------------------------ TFieldOrganizer -------------------------------------}

constructor TFieldOrganizer.Create;
begin
  Inherited Create;

  FSortedList := TList.Create;
  FSortedFieldList := TFieldList.Create;
  FLists := TList.Create;
  FFirst := TFieldList.Create;
  FLast := TFieldList.Create;
  FRunning := TFieldList.Create;
  FTab := TFieldList.Create;
  FOther := TFieldList.Create;
  FDistibute := TFieldList.Create;

  FFirst.Duplicates := dupIgnore;
  FLast.Duplicates := dupIgnore;
  FRunning.Duplicates := dupIgnore;
  FTab.Duplicates := dupIgnore;
  FOther.Duplicates := dupIgnore;
  FDistibute.Duplicates := dupIgnore;

  Lists.Add(FFirst);
  Lists.Add(FLast);
  Lists.Add(FRunning);
  Lists.Add(FTab);
  Lists.Add(FOther);
  Lists.Add(FDistibute);
end;

destructor TFieldOrganizer.Destroy;
begin
  fFirst.Free; // LAA-tillsatt
  FLast.Free;
  FRunning.Free;
  FTab.Free;
  FOther.Free;
  FDistibute.Free;
  FLists.Free;
  FSortedList.Free;
  FSortedFieldList.Free;

  inherited Destroy;
end;

procedure TFieldOrganizer.AddField(AField : TDataField; AListType : TListType; RemoveExistent : Boolean);
var
  idx : Integer;
  AList : TFieldList;
  Contains : Boolean;
begin
  if AField.IsRunningNumber then
    AListType := ltRunning;

  Contains := ContainsField(AField, AList, idx);
  if not Contains or RemoveExistent then
  begin
    if Contains then
      AList.Delete(idx);
    case AListType of
      ltFirst      : FFirst.Add(AField);
      ltLast       : FLast.Add(AField);
      ltRunning    :
        begin
          if not AField.IsRunningNumber then
            raise Exception.Create(Self.ClassName + '.AddField: Field ' + AField.FieldName + ' is not a running number!')
          else
            FRunning.Add(AField);
        end;
      ltTab        : FTab.Add(AField);
      ltOther      : FOther.Add(AField);
      ltDistribute : FDistibute.Add(AField);
    end;
  end;
end;

function TFieldOrganizer.GetCount : Integer;
var
  iList : Integer;
begin
  Result := 0;
  for iList := 0 to Lists.Count -1 do
    Result := Result + TFieldList(Lists[iList]).Count;
end;

function TFieldOrganizer.ContainsField(AField : TDataField; var AList : TFieldList; var idx : Integer) : Boolean;
var
  iList : Integer;
begin
  idx := -1;
  for iList := 0 to Lists.Count -1 do
  begin
    idx := TFieldList(Lists[iList]).IndexOf(AField);
    if idx >= 0 then
    begin
      AList := TFieldList(Lists[iList]);
      Break;
    end;
  end;
  Result := idx >= 0;
end;

function TFieldOrganizer.GetList : TList;

  procedure AddToList(Src : array of TFieldList; Dest : TList);
  var
    iField, iList : Integer;
  begin
    for iList := Low(Src) to High(Src) do
      for iField := 0 to Src[iList].Count -1 do
        Dest.Add(Src[iList][iField]);
  end;

begin
  FSortedList.Clear;

  AddToList([
    FFirst,
    FTab,
    FOther,
    FLast,
    FDistibute,
    FRunning],
    FSortedList);

  Result := FSortedList;
end;

function TFieldOrganizer.GetFieldList : TFieldList;

  procedure AddToList(Src : array of TFieldList; Dest : TFieldList);
  var
    iList : Integer;
  begin
    for iList := Low(Src) to High(Src) do
      Dest.AddFrom(Src[iList]);
  end;

begin
  FSortedFieldList.Clear;

  AddToList([
    FFirst,
    FTab,
    FOther,
    FLast,
    FDistibute,
    FRunning],
    FSortedFieldList);

  Result := FSortedFieldList;
end;

{------------------------ TDataEditorDefinition -------------------------------}

constructor TDataEditorDefinition.Create;
begin
  inherited Create;
  FOpenCriteria := TCriteria.Create;
  FInternalCriteria := TCriteria.Create;

  FKeys := TFieldOrganizer.Create;// TFieldList;
{  FKeyList := TFieldList.Create;
  FKeyList.Duplicates := dupIgnore;}
  FHideFieldList := TFieldList.Create;
  FHideFieldList.Duplicates := dupIgnore;
  FHideKeyList := TFieldList.Create;
  FHideKeyList.Duplicates := dupIgnore;
  FOwnDisabledFieldList := TDefaultValueFieldList.Create;
  FDisabledFieldList := FOwnDisabledFieldList;
  FViewCommonKeys := False;
end;

destructor TDataEditorDefinition.Destroy;
begin
  FKeys.Free;
{  FKeyList.Free;
  FKeyList := nil;}
  FHideFieldList.Free;
  FHideFieldList := nil;
  FHideKeyList.Free;
  FHideKeyList := nil;
  FInternalCriteria.Free;
  FInternalCriteria := nil;
  FOpenCriteria.Free;
  FOpenCriteria := nil;
  FOwnDisabledFieldList.Free;
  FOwnDisabledFieldList := nil;
  FSaveToAuxtableFields.Free;
  FSaveToAuxtableFields := nil;
  inherited Destroy;
end;

procedure TDataEditorDefinition.SetOpenCriteria(ACriteria : TCondition);
begin
  FOpenCriteria.Free;
  if ACriteria = nil then
    FOpenCriteria := nil
  else
    FOpenCriteria := ACriteria.CreateCopy;
end;

procedure TDataEditorDefinition.SetInternalCriteria(ACriteria : TCriteria);
begin
  InternalCriteria.Free;
  FInternalCriteria := ACriteria.CreateCopyOfCriteria;
end;

{procedure TDataEditorDefinition.AddKey(aKey : TKeyField);
begin
  FKeyList.Add(aKey);
end;

procedure TDataEditorDefinition.InsertKey(idxKey : Integer; aKey : TKeyField);
begin
  FKeyList.Insert(idxKey, aKey);
end;
}
function TDataEditorDefinition.GetSaveToAuxTableFields : TDataFieldSet;
begin
  if FSaveToAuxtableFields = nil then
    FSaveToAuxtableFields := TDataFieldSet.Create;
  Result := FSaveToAuxtableFields;
end;

procedure TDataEditorDefinition.SetSaveToAuxTableFields(Fields : TDataFieldSet);
begin
  if FSaveToAuxtableFields = nil then
    FSaveToAuxtableFields := TDataFieldSet.Create
  else
    FSaveToAuxtableFields.Clear;

  FSaveToAuxtableFields.CopyFrom(Fields);
end;

end.

