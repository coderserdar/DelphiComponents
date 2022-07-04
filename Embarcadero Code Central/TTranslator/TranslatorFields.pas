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

{ $Id: TranslatorFields.pas,v 1.4 2002/10/31 07:47:16 laa Exp $ }

{-----------------------------------------------------------------------------
  Translator       The Polycon TTranslator component for translating form
                   properties and code strings. See more on
                   http://www.polycon.fi/translator

  What             TLangSortField
                   TUnitLookupField
                   TLangNameField

  Company          Polycon
  Authors          LGE
-----------------------------------------------------------------------------}

unit TranslatorFields;

interface

uses
  Classes, CalcField, Graphics, DataElements, DataType, DataTypes, ImageNames,
  SysUtils;

type
  TMarkRowCalcField = class(TCalcField)
  private
    FActLstPic : TPicture;
    FFormPic : TPicture;
    FFormDisablePic : TPicture;

    constructor CreateOld;
  public
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    destructor Destroy; override;
  end;

  TLangSortField = class(TCalcField)
  private
    FLangField : TDataField;
    constructor CreateOld(LangField : TDataField);
  public
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    destructor Destroy; override;
  end;

  TDoTranslateDisplayField = class(TCalcField)
  protected
    function GetReadOnly(ARow : TAbstractRow) : Boolean; override;
  public
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    function DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult; override;
  end;

  TLangDisplayField = class(TCalcField)
  private
    FLangField : TDataField;
    constructor CreateOld(LangField : TDataField);
  protected
    function GetReadOnly(ARow : TAbstractRow) : Boolean; override;
  public
    function CalcValue(ARow : TAbstractRow) : TValue; override;
    function DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult; override;

    destructor Destroy; override;
  end;

  TUnitLookupField = class(TCalcField)
  public
    function CalcValue(ARow : TAbstractRow) : TValue; override;
  end;

  TLangNameField = class(TDataField)
  private
    FDisplayField : TLangDisplayField;
  public
    procedure SetDescription(ADescr : String);
    property DisplayField : TLangDisplayField read FDisplayField;

    constructor CreateOld;
    destructor Destroy; override;
  end;

var
  FieldUnit : TUnitLookupField;
  FieldFormClass : TUnitLookupField;
  FieldGUID : TKeyField;
  FieldClass : TKeyField;
  FieldComponent : TKeyField;
  FieldComponentPointer : TDataField;
  FieldProperty : TKeyField;
  MarkRowField : TMarkRowCalcField;

  FieldSubClasses : TDataField;
  FieldDoTranslate : TDataField;
  FieldDisplayDoTranslate : TDoTranslateDisplayField;

  FieldDoTranslateText : TDataField;
  FieldDisplayDoTranslateText : TDataField;
  FieldSubClassesText : TDataField;

  AddedPropertiesTable : TDataTable;
  UnitListTable : TDataTable;

implementation

uses
  DerivedDataType, Translator, ActnList, CommonCalcFields;

const
  id_BMP_ACTLIST = 'BMP_ACTLIST';
  id_BMP_FORM = 'BMP_FORM';
  id_BMP_FORMDISABLE = 'BMP_FORMDISABLE';

type
  TTranslatedStringsLink = class(TTranslatedStrings);
  TTranslatorClientLink = class(TTranslatorClient);

{
function GetEditorWindow(Strings : IEditableTranslatedStrings) : TdlgStringsEditor;
begin
  Result := TdlgStringsEditor(Strings.EditorWindow);
end;
}

procedure GetServerAndClientAndComp(ARow : TAbstractRow; out Server : TTranslator; out Client : TTranslatorClient; out Component : TComponent);
begin
  Server := nil;
  Client := nil;
  Component := nil;

  if (ARow <> nil) and (ARow.Storage is TTranslatorRowStorage) then
  begin
    Server := TTranslatorRowStorage(ARow.Storage).Owner.Owner;

    Client := Server;
    TTranslatedStrings.FormHasComponent(Server.Owner, ARow.StringValue[FieldComponent], Component);

  end;
end;

function LockedByAction(ARow : TAbstractRow) : Boolean;
var
  Server : TTranslator;
  Client : TTranslatorClient;
  Component : TComponent;
  Dummy : TAction;
begin
  Result := False;

  GetServerAndClientAndComp(ARow, Server, Client, Component);
  if (Component <> nil) and
     TTranslatedStringsLink(Server.Strings).HasActionForProp(Component, ARow.StringValue[FieldProperty], Dummy) then
    Result := True;
end;

procedure CommonDistributeValue(ARow : TAbstractRow; DestField : TDataField; Value : TValue);
var
  Server : TTranslator;
  Client : TTranslatorClient;
  Component : TComponent;
begin
  if ARow <> nil then
  begin
    GetServerAndClientAndComp(ARow, Server, Client, Component);

    ARow[DestField] := Value;
    if DestField <> FieldDoTranslate then
      ARow[FieldDoTranslate] := TrueValue;

    if Server = nil then
      Exit;

    TTranslatorClientLink(Server).SetModified; // Mark this unit as modified!

    if (Client = nil) then
      Exit;

    if ARow.BooleanValue[FieldDoTranslate] then
    begin
      if (DestField = FieldDoTranslate) or
         (Client.Language = DestField.ShortDescription) then
        TTranslatorClientLink(Client).TranslateOneProperty(TDataRow(ARow));
    end;

    if Component is TAction then
      TTranslatedStringsLink(Server.Strings).UpdateActionItems(Component as TAction);
  end;
end;

(*
function DisplayFieldCalcValue(ARow : TAbstractRow; ASourceField : TDataField) : TValue;
var
  Server : TTranslator;
  Client : TTranslatorClient;
  Component : TComponent;
  Action : TAction;
  ActionRow : TDataRow;
  ActionClient : TTranslatorClient;
  i : Integer;
begin
  if ARow = nil then
    Result := ASourceField.DataType.DefaultValue
  else
  begin
    GetServerAndClientAndComp(ARow, Server, Client, Component);
    if (Component <> nil) and
       TTranslatedStringsLink(Server.Strings).HasActionForProp(Component, ARow.StringValue[FieldProperty], Action) then
    begin

      ActionClient := Client;


      if ActionClient <> nil then
        with TTranslatedStringsLink(Server.Strings) do
        begin
          ActionRow := Translations.LocateRow([ActionClient.UniqueIdentifier, Action.Name, ARow.StringValue[FieldProperty]]);
          if ActionRow <> nil then
          begin
            if ASourceField <> FieldDoTranslate then
              for i := 0 to LanguageCount - 1 do
                ARow[LanguageFields[i]] := ActionRow[LanguageFields[i]];

            ARow[FieldDoTranslate] := ActionRow[FieldDoTranslate];

            if ARow.BooleanValue[FieldDoTranslate] then
              TTranslatorClientLink(ActionClient).TranslateOneProperty(TDataRow(ARow));
          end;
        end;
    end;

    Result := ARow[ASourceField];
  end;
end;
*)

{ TMarkRowCalcField }

constructor TMarkRowCalcField.CreateOld;

  function CreatePic(AName : PChar) : TPicture;
  begin
    Result := TPicture.Create;
    LoadImageFromRes(Result, AName);
    Result.Bitmap.Transparent := True;
    Result.Bitmap.TransparentColor := Result.Bitmap.Canvas.Pixels[0, Result.Bitmap.Height - 1];
  end;

begin
  inherited CreateOld('', PictureType, True, True);
  Self.IsAggregable := True;

  FActLstPic := CreatePic(id_BMP_ACTLIST);
  FFormPic := CreatePic(id_BMP_FORM);
  FFormDisablePic := CreatePic(id_BMP_FORMDISABLE);
end;

function TMarkRowCalcField.CalcValue(ARow : TAbstractRow) : TValue;
var
  Server : TTranslator;
  Client : TTranslatorClient;
  Component : TComponent;
  Dummy : TAction;
begin
  GetServerAndClientAndComp(ARow, Server, Client, Component);

  if (Client = nil) or (Component = nil) then
    Result := ValueFromPicture(FFormDisablePic) // Inactive form
  else if not TTranslatedStringsLink(Server.Strings).HasProperty(Component, ARow.StringValue[FieldProperty]) then  // Fixa LGE denna måste ännu fiksas
    Result := {EmptyString} ValueFromPicture(FFormPic) // Inget som default
  else if TTranslatedStringsLink(Server.Strings).HasActionForProp(Component, ARow.StringValue[FieldProperty], Dummy) then
    Result := ValueFromPicture(FActLstPic) // TActionList
  else
    Result := ValueFromPicture(FFormPic); // Active form
end;

destructor TMarkRowCalcField.Destroy;
begin
  FActLstPic.Free;
  FFormPic.Free;
  FFormDisablePic.Free;

  inherited Destroy;
end;


{ TLangSortField }

constructor TLangSortField.CreateOld(LangField : TDataField);
begin
  inherited CreateOld('', LangField.DataType, True, True);
  FLangField := LangField;
end;

function TLangSortField.CalcValue(ARow : TAbstractRow) : TValue;
var
  sVal : String;
  p : Integer;
begin
  sVal := AnsiLowerCase(ARow.StringValue[FLangField]);
  while True do
  begin
    p := pos('&', sVal);
    if p < 1 then
      Break;

    sVal := Copy(sVal, 1, p-1) + Copy(sVal, p+1, Length(sVal));
  end;

  Result := ValueFromString(sVal);
end;

destructor TLangSortField.Destroy;
begin
  inherited Destroy;
end;

{ TDoTranslateDisplayField }

function TDoTranslateDisplayField.GetReadOnly(ARow : TAbstractRow) : Boolean;
begin
  Result := LockedByAction(ARow);
end;

function TDoTranslateDisplayField.CalcValue(ARow : TAbstractRow) : TValue;
begin
  // Result := DisplayFieldCalcValue(ARow, FieldDoTranslate);
  Result := ARow[FieldDoTranslate];
end;

function TDoTranslateDisplayField.DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult;
begin
  Result := srOk;
  CommonDistributeValue(ARow, FieldDoTranslate, Value);

{  if ARow <> nil then
  begin
    GetServerAndClientAndComp(ARow, Server, Client, Component);

    ARow[FieldDoTranslate] := Value;

    if Server = nil then
      Exit;

    TTranslatorClientLink(Server).SetModified; // Mark this unit as modified!

    if (Client = nil) then
      Exit;

    if ARow.BooleanValue[FieldDoTranslate] then
      TTranslatorClientLink(Client).TranslateOneProperty(TDataRow(ARow));

    if Component is TAction then
    begin
      TTranslatedStringsLink(Server.Strings).UpdateActionItems(Component as TAction);
      // GetEditorWindow(Server.Strings).InvalidateActiveEditor;
    end;
  end; }
end;

{ TLangDisplayField }

constructor TLangDisplayField.CreateOld(LangField : TDataField);
begin
  inherited CreateOld('', LangField.DataType, False, True);
  FLangField := LangField;
end;

destructor TLangDisplayField.Destroy;
begin
  inherited Destroy;
end;

function TLangDisplayField.CalcValue(ARow : TAbstractRow) : TValue;
begin
//  Result := DisplayFieldCalcValue(ARow, FLangField);
  Result := ARow[FLangField];
end;

function TLangDisplayField.DistributeValue(ARow : TAbstractRow; Value : TValue) : TSetResult;
begin
  Result := srOk;
  CommonDistributeValue(ARow, FLangField, Value);
end;

function TLangDisplayField.GetReadOnly(ARow : TAbstractRow) : Boolean;
begin
  Result := LockedByAction(ARow);
end;

{ TUnitLookupField }

function TUnitLookupField.CalcValue(ARow : TAbstractRow) : TValue;
var
  Server : TTranslator;
  Client : TTranslatorClient;
  Component : TComponent;
  UnitRow : TDataRow;
begin
  GetServerAndClientAndComp(ARow, Server, Client, Component);

  if Server <> nil then
  begin
    UnitRow := TTranslatedStringsLink(Server.Strings).UnitList.LocateRow([ARow.StringValue[FieldGUID]]);
    if UnitRow <> nil then
      Result := UnitRow[Self]
    else
      Result := EmptyString;
  end
  else
    Result := EmptyString;
end;

{ TLangNameField }

constructor TLangNameField.CreateOld;
begin
  inherited CreateOld('', StringType(255, False));
  Self.SortField := TLangSortField.CreateOld(Self);
  FDisplayField := TLangDisplayField.CreateOld(Self);
  FDisplayField.SortField := Self.SortField;
end;

destructor TLangNameField.Destroy;
begin
  FDisplayField.Free;
  Self.SortField.Free;
  inherited Destroy;
end;

procedure TLangNameField.SetDescription(ADescr : String);
begin
  Self.SetAllDescriptions([ADescr]);
  FDisplayField.SetAllDescriptions([ADescr]);
end;

initialization

  FieldClass := TKeyField.CreateNonAuxtabled('', StringType(64, False));
  FieldClass.SetAllDescriptions(['Class']);
  FieldGUID := TKeyField.CreateNonAuxtabled('', StringType(38, False));
  FieldGUID.SetAllDescriptions(['GUID']);
  FieldUnit := TUnitLookupField.CreateOld('', StringType(64, False), True, True);
  FieldUnit.SetAllDescriptions(['Unit']);
  FieldFormClass := TUnitLookupField.CreateOld('', StringType(64, False), True, True);
  FieldFormClass.SetAllDescriptions(['Form Class']);
  FieldComponent := TKeyField.CreateNonAuxtabled('', StringType(64, False));
  FieldComponent.SetAllDescriptions(['Component']);
  FieldProperty := TKeyField.CreateNonAuxtabled('', StringType(64, False));
  FieldProperty.SetAllDescriptions(['Property']);
  MarkRowField := TMarkRowCalcField.CreateOld;

  FieldDoTranslate := TDataField.CreateOld('', BooleanType);
  FieldDoTranslate.SetAllDescriptions(['Translate']);
  FieldDisplayDoTranslate := TDoTranslateDisplayField.CreateOld('', BooleanType, False, True);
  FieldDisplayDoTranslate.SetAllDescriptions(['Translate']);
  FieldDoTranslateText := TBooleanTextField.CreateOld('', FieldDoTranslate);
  FieldDisplayDoTranslateText := TBooleanTextField.CreateOld('', FieldDisplayDoTranslate);

  FieldSubClasses := TDataField.CreateOld('', BooleanType);
  FieldSubClasses.SetAllDescriptions(['Include subclasses']);
  FieldSubClassesText := TBooleanTextField.CreateOld('', FieldSubClasses);

  FieldComponentPointer := TDataField.CreateOld('', ObjectType);
  FieldComponentPointer.SetAllDescriptions(['Pointer to component']);

  AddedPropertiesTable := TDataTable.CreateOld('', nil,
                         [FieldClass, FieldProperty],
                         [FieldSubClasses, FieldDoTranslate],
                         nil);

  UnitListTable := TDataTable.CreateOld('', nil,
                         [FieldGUID],
                         [FieldUnit, FieldFormClass],
                         nil);

finalization

  FieldClass.Free;
  FieldComponent.Free;
  FieldGUID.Free;
  FieldUnit.Free;
  FieldFormClass.Free;
  FieldProperty.Free;
  MarkRowField.Free;
  FieldDoTranslate.Free;
  FieldDisplayDoTranslate.Free;
  FieldSubClasses.Free;
  FieldComponentPointer.Free;

  AddedPropertiesTable.Free;
  UnitListTable.Free;

  FieldDoTranslateText.Free;
  FieldDisplayDoTranslateText.Free;
  FieldSubClassesText.Free;

end.

