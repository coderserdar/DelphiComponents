{*********************************************************}
{*                   VPLOCALIZE.PAS 1.03                 *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit VpLocalize;

interface

uses
  Windows,
  Classes,
  Dialogs,
  SysUtils,
  Graphics,
  StdCtrls,
  VpBase,
  VpMisc,
  VpData,
  VpXParsr,
  VpPrtFmt, { For TVpAttributes }
  Forms;

type
  TVpLocalizeLanguage = class;

  TVpLocalizeLanguageItem = class (TVpCollectionItem)
    private
      FCollection    : TVpLocalizeLanguage;
      FLanguageID    : Integer;
      FSubLanguageID : Integer;
      FName          : string;
    protected

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

    published
      property Collection : TVpLocalizeLanguage read FCollection write FCollection;
      property LanguageID : Integer read FLanguageID write FLanguageID;
      property Name : string read FName write FName;
      property SubLanguageID : Integer read FSubLanguageID write FSubLanguageID;
  end;

  TVpLocalizeLanguage = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpLocalizeLanguageItem;
      function  GetOwner : TPersistent; override;
      procedure SetItem (Index : Integer; Value : TVpLocalizeLanguageItem);

    public
      constructor Create (AOwner : TPersistent);
      {$IFNDEF VERSION5}
      procedure Delete (Item : integer);
      {$ENDIF}
      function HasLanguage (ALanguage : Integer) : Integer;
      function HasSubLanguage (ALanguage : Integer;
                                ASubLanguage : Integer) : Integer;

      property Items[Index : Integer] : TVpLocalizeLanguageItem
               read GetItem write SetItem;
  end;

  TVpLocalizeStates = class;

  TVpLocalizeStatesItem = class (TVpCollectionItem)
    private
      FCollection  : TVpLocalizeStates;
      FName        : string;
      FAbbr        : string;

    protected

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

    published
      property Collection : TVpLocalizeStates read FCollection write FCollection;
      property Name : string read FName write FName;
      property Abbr : string read FAbbr write FAbbr;
  end;

  TVpLocalizeStates = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpLocalizeStatesItem;
      function  GetOwner : TPersistent; override;
      procedure SetItem (Index : Integer; Value : TVpLocalizeStatesItem);

    public
      constructor Create (AOwner : TPersistent);
      {$IFNDEF VERSION5}
      procedure Delete (Item : integer);
      {$ENDIF}
      property Items[Index : Integer] : TVpLocalizeStatesItem
               read GetItem write SetItem;

  end;

  TVpLocalizeCountry = class;

  TVpLocalizeCountryItem = class (TVpCollectionItem)
    private
      FCollection      : TVpLocalizeCountry;
      FStates          : TVpLocalizeStates;
      FLanguages       : TVpLocalizeLanguage;
      FName            : string;
      FAddress1Visible : Boolean;
      FAddress1Caption : string;
      FAddress2Visible : Boolean;
      FAddress2Caption : string;
      FAddress3Visible : Boolean;
      FAddress3Caption : string;
      FAddress4Visible : Boolean;
      FAddress4Caption : string;
      FCityVisible     : Boolean;
      FCityCaption     : string;
      FStatesVisible   : Boolean;
      FStateUseAbbr    : Boolean;
      FStateDupAbbr    : Boolean;
      FStateCaption    : string;
      FZipVisible      : Boolean;
      FZipCaption      : string;
    protected

    public
      constructor Create (Collection : TCollection); override;
      destructor  Destroy; override;

    published
      property Collection : TVpLocalizeCountry read FCollection write FCollection;
      property Languages : TVpLocalizeLanguage read FLanguages write FLanguages;
      property Name : string read FName write FName;
      property Address1Visible : Boolean read FAddress1Visible write FAddress1Visible;
      property Address1Caption : string read FAddress1Caption write FAddress1Caption;
      property Address2Visible : Boolean read FAddress2Visible write FAddress2Visible;
      property Address2Caption : string read FAddress2Caption write FAddress2Caption;
      property Address3Visible : Boolean read FAddress3Visible write FAddress3Visible;
      property Address3Caption : string read FAddress3Caption write FAddress3Caption;
      property Address4Visible : Boolean read FAddress4Visible write FAddress4Visible;
      property Address4Caption : string read FAddress4Caption write FAddress4Caption;
      property CityVisible : Boolean read FCityVisible write FCityVisible;
      property CityCaption : string read FCityCaption write FCityCaption;
      property StatesVisible : Boolean read FStatesVisible write FStatesVisible;
      property StateUseAbbr : Boolean read FStateUseAbbr write FStateUseAbbr;
      property StateDupAbbr : Boolean read FStateDupAbbr write FStateDupAbbr;
      property StateCaption : string read FStateCaption write FStateCaption;
      property ZipVisible : Boolean read FZipVisible write FZipVisible;
      property ZipCaption : string read FZipCaption write FZipCaption;
      property States : TVpLocalizeStates read FStates write FStates;
  end;

  TVpLocalizeCountry = class (TCollection)
    private
      FOwner : TPersistent;

    protected
      function  GetItem (Index : Integer) : TVpLocalizeCountryItem;
      function  GetOwner : TPersistent; override;
      procedure SetItem (Index : Integer; Value : TVpLocalizeCountryItem);

    public
      constructor Create (AOwner : TPersistent);
      {$IFNDEF VERSION5}
      procedure Delete (Item : integer);
      {$ENDIF}
      property Items[Index : Integer] : TVpLocalizeCountryItem
               read GetItem write SetItem;

  end;

  TVpLocalization = class (TObject)
    private
      FCountries    : TVpLocalizeCountry;
      FAttributes   : TVpAttributes;
      FLoadingIndex : Integer;
      FElementIndex : Integer;
      
    protected
      procedure xmlLocalizeAttribute (oOwner     : TObject;
                                      sName,
                                      sValue     : DOMString;
                                      bSpecified : Boolean);
      procedure xmlLocalizeEndElement (oOwner : TObject;
                                       sValue : DOMString);
      procedure xmlLocalizeStartElement (oOwner : TObject;
                                         sValue : DOMString);
    public
      constructor Create; 
      destructor Destroy; override;

      procedure CountriesByLanguage (ALanguage : Integer;
                                     AStrings  : TStrings);
      procedure CountriesBySubLanguage (ALanguage    : Integer;
                                        ASubLanguage : Integer;
                                        AStrings     : TStrings);
      function CountryNameToIndex (ACountry : string) : Integer;
      procedure CountriesToTStrings (AStrings : TStrings);
      function GetCurrentCountry : Integer;
      function GetCountryByLanguage (ALanguage : Integer) : Integer;
      function GetCountryBySubLanguage (ALanguage    : Integer;
                                        ASubLanguage : Integer) : Integer;
      procedure LoadFromFile (const FileName : string;
                              const Append   : Boolean);
      function StateNameToIndex (ACountry : Integer;
                                 AState   : string) : Integer;
      procedure StatesToTStrings (ACountry : Integer;
                                  AStrings : TStrings);
    published
      property Countries : TVpLocalizeCountry read FCountries write FCountries;
  end;




implementation

constructor TVpLocalizeLanguageItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TVpLocalizeLanguage.Create (TVpLocalizeLanguage (Collection).FOwner);

  FLanguageID    := -1;
  FSubLanguageID := -1;
  FName          := '';
end;

destructor TVpLocalizeLanguageItem.Destroy;
begin
  FCollection.Free;
  FCollection := nil;
  inherited Destroy;
end;

constructor TVpLocalizeLanguage.Create(AOwner : TPersistent);
begin
  inherited Create (TVpLocalizeLanguageItem);
  FOwner := AOwner;
end;
{=====}

{$IFNDEF VERSION5}
procedure TVpLocalizeLanguage.Delete(Item: integer);
begin
  GetItem(Item).Free;
end;
{=====}
{$ENDIF}

function TVpLocalizeLanguage.GetItem (Index : Integer) : TVpLocalizeLanguageItem;
begin
  Result := TVpLocalizeLanguageItem (inherited GetItem (Index));
end;
{=====}

function TVpLocalizeLanguage.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

function TVpLocalizeLanguage.HasLanguage (ALanguage : Integer) : Integer;
var
  i : Integer;

begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].LanguageID = ALanguage then begin
      Result := i;
      Break;
    end;
end;

function TVpLocalizeLanguage.HasSubLanguage (ALanguage : Integer;
                                             ASubLanguage : Integer) : Integer;
var
  i : Integer;

begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].LanguageID = ALanguage) and
       (Items[i].SubLanguageID = ASubLanguage) then begin
      Result := i;
      Break;
    end;
end;

procedure TVpLocalizeLanguage.SetItem (Index : Integer; Value : TVpLocalizeLanguageItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}

constructor TVpLocalizeStatesItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TVpLocalizeStates.Create (TVpLocalizeStates (Collection).FOwner);

  FName  := '';
  FAbbr  := '';
end;

destructor TVpLocalizeStatesItem.Destroy;
begin
  FCollection.Free;
  FCollection := nil;
  inherited Destroy;
end;

constructor TVpLocalizeStates.Create(AOwner : TPersistent);
begin
  inherited Create (TVpLocalizeStatesItem);
  FOwner := AOwner;
end;
{=====}

{$IFNDEF VERSION5}
procedure TVpLocalizeStates.Delete(Item: integer);
begin
  GetItem(Item).Free;
end;
{=====}
{$ENDIF}

function TVpLocalizeStates.GetItem (Index : Integer) : TVpLocalizeStatesItem;
begin
  Result := TVpLocalizeStatesItem (inherited GetItem (Index));
end;
{=====}

function TVpLocalizeStates.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpLocalizeStates.SetItem (Index : Integer; Value : TVpLocalizeStatesItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}

constructor TVpLocalizeCountryItem.Create (Collection : TCollection);
begin
  inherited Create (Collection);
  FCollection := TVpLocalizeCountry.Create (TVpLocalizeCountry (Collection).FOwner);

  FStates := TVpLocalizeStates.Create (Self);
  FLanguages := TVpLocalizeLanguage.Create (nil);

  FName            := '';
  FAddress1Visible := True;
  FAddress1Caption := 'Address';
  FAddress2Visible := True;
  FAddress2Caption := '';
  FAddress3Visible := True;
  FAddress3Caption := '';
  FAddress4Visible := False;
  FAddress4Caption := '';
  FCityVisible     := True;
  FCityCaption     := 'City';
  FStatesVisible   := True;
  FStateUseAbbr    := False;
  FStateDupAbbr    := False;
  FStateCaption    := 'Province';
  FZipVisible      := True;
  FZipCaption      := 'Postal Code';
end;

destructor TVpLocalizeCountryItem.Destroy;
begin
  FCollection.Free;
  FCollection := nil;

  FStates.Free;
  FStates := nil;

  FLanguages.Free;
  FLanguages := nil;
  
  inherited Destroy;
end;

constructor TVpLocalizeCountry.Create(AOwner : TPersistent);
begin
  inherited Create (TVpLocalizeCountryItem);
  FOwner := AOwner;
end;
{=====}

{$IFNDEF VERSION5}
procedure TVpLocalizeCountry.Delete(Item: integer);
begin
  GetItem(Item).Free;
end;
{=====}
{$ENDIF}

function TVpLocalizeCountry.GetItem (Index : Integer) : TVpLocalizeCountryItem;
begin
  Result := TVpLocalizeCountryItem (inherited GetItem (Index));
end;
{=====}

function TVpLocalizeCountry.GetOwner : TPersistent;
begin
  Result := FOwner;
end;
{=====}

procedure TVpLocalizeCountry.SetItem (Index : Integer; Value : TVpLocalizeCountryItem);
begin
  inherited SetItem (Index, Value);
end;
{=====}


constructor TVpLocalization.Create;
begin
  inherited Create;

  Countries   := TVpLocalizeCountry.Create (nil);
  FAttributes :=TVpAttributes.Create (nil);
end;

destructor TVpLocalization.Destroy;
begin
  Countries.Free;
  FAttributes.Free;

  inherited Destroy;
end;

procedure TVpLocalization.CountriesByLanguage (ALanguage : Integer;
                                               AStrings  : TStrings);
var
  i : Integer;

begin
  AStrings.Clear;
  
  for i := 0 to Countries.Count - 1 do
    if Countries.Items[i].Languages.HasLanguage (ALanguage) >= 0 then
      AStrings.Add (Countries.Items[i].Name);
end;

procedure TVpLocalization.CountriesBySubLanguage (ALanguage    : Integer;
                                                  ASubLanguage : Integer;
                                                  AStrings     : TStrings);
var
  i : Integer;

begin
  AStrings.Clear;
  
  for i := 0 to Countries.Count - 1 do
    if Countries.Items[i].Languages.HasSubLanguage (ALanguage, ASubLanguage) >= 0 then
      AStrings.Add (Countries.Items[i].Name);
end;

function TVpLocalization.CountryNameToIndex (ACountry : string) : Integer;
var
  i    : Integer;
  CLen : Integer;
begin
  Result := -1;

  if ACountry = '' then
    Exit;

  ACountry := LowerCase (ACountry);
  CLen := Length (ACountry);
  for i := 0 to FCountries.Count - 1 do
    if ACountry = Copy (LowerCase (FCountries.Items[i].Name), 1, CLen) then begin
      Result := i;
      Exit;
    end;
end;

procedure TVpLocalization.CountriesToTStrings (AStrings : TStrings);
var
  i : Integer;

begin
  AStrings.Clear;

  for i := 0 to FCountries.Count - 1 do
    AStrings.Add (FCountries.Items[i].Name);
end;

function TVpLocalization.GetCurrentCountry : Integer;

   function SubLangID (LanguageID : Word) : Word;
   begin
     Result := LanguageID shr 10;
   end;

   function PrimaryLangID (LanguageID : Word) : Word;
   begin
     Result := LanguageID and $3FF;
   end;

var
  LangId    : Word;
  Primary   : Word;
  Secondary : Word;

begin
  LangId    := GetUserDefaultLangID;
  Primary   := PrimaryLangID (LangID);
  Secondary := SubLangID (LangID);
  if Secondary > 0 then
    Result := Self.GetCountryBySubLanguage (Primary, Secondary)
  else
    Result := Self.GetCountryByLanguage (Primary);
end;

function TVpLocalization.GetCountryByLanguage (ALanguage : Integer) : Integer;
var
  i : Integer;

begin
  Result := -1;
  for i := 0 to Countries.Count - 1 do
    if Countries.Items[i].Languages.HasLanguage (ALanguage) >= 0 then begin
      Result := i;
      Break;
    end;
end;

function TVpLocalization.GetCountryBySubLanguage (ALanguage    : Integer;
                                                  ASubLanguage : Integer) : Integer;
var
  i : Integer;

begin
  Result := -1;
  for i := 0 to Countries.Count - 1 do
    if Countries.Items[i].Languages.HasSubLanguage (ALanguage,
                                                    ASubLanguage) >= 0 then begin
      Result := i;
      Break;
    end;
end;                                                  

procedure TVpLocalization.LoadFromFile (const FileName : string;
                                        const Append   : Boolean);
var
  Parser : TVpParser;

begin
  if not Append then
    FCountries.Clear;

  FLoadingIndex := -1;
  FElementIndex := -1;
  Parser := TVpParser.Create (nil);
  Parser.OnAttribute := xmlLocalizeAttribute;
  Parser.OnStartElement := xmlLocalizeStartElement;
  Parser.OnEndElement := xmlLocalizeEndElement;
  try
    Parser.ParseDataSource (FileName);
  finally
    Parser.Free;
  end;
  FLoadingIndex := -1;
  FElementIndex := -1;
end;

function TVpLocalization.StateNameToIndex (ACountry : Integer;
                                           AState   : string) : Integer;
var
  i : Integer;
begin
  Result := -1;

  if (ACountry < 0) or (ACountry >= FCountries.Count) then
    Exit;
  
  AState := LowerCase (AState);
  for i := 0 to FCountries.Items[ACountry].States.Count - 1 do
    if AState = LowerCase (FCountries.Items[ACountry].States.Items[i].Name) then begin
      Result := i;
      Exit;
    end;
end;

procedure TVpLocalization.StatesToTStrings (ACountry : Integer;
                                            AStrings : TStrings);
var
  i : Integer;

begin
  AStrings.Clear;
  
  if (ACountry < 0) or (ACountry >= FCountries.Count) then
    Exit;

  for i := 0 to FCountries.Items[ACountry].States.Count - 1 do
    AStrings.Add (FCountries.Items[ACountry].States.Items[i].Name);
end;

procedure TVpLocalization.xmlLocalizeAttribute (oOwner     : TObject;
                                                sName,
                                                sValue     : DOMString;
                                                bSpecified : Boolean);
var
  Item : TVpAttributeItem;
begin
  Item := TVpAttributeItem (FAttributes.Add);
  Item.Name := sName;
  Item.Value := sValue;
end;

procedure TVpLocalization.xmlLocalizeEndElement (oOwner : TObject;
                                                 sValue : DOMString);
begin
  if (sValue = 'Country') or (sValue = 'Countries') or
     (sValue = 'AddressDefinition') then begin
    FLoadingIndex := -1;
    FElementIndex := -1;
  end else if sValue = 'State' then
    FElementIndex := -1;
  FAttributes.Clear;
end;

procedure TVpLocalization.xmlLocalizeStartElement (oOwner : TObject;
                                                   sValue : DOMString);

  function GetBooleanValue (AString  : string;
                            ADefault : Boolean) : Boolean;
  begin
    Result := ADefault;            
    AString := LowerCase (AString);
    if (AString = 't') or (AString = 'true') or (AString = '1') or
       (AString = 'on') or (AString = 'yes') then
      Result := True
    else if (AString= 'f') or (AString = 'false') or (AString = '0') or
            (AString = 'off') or (AString = 'no') then
      Result := False;
  end;                                                                

  function GetIntegerValue (AString  : string;
                            ADefault : Integer) : Integer;
  begin
    try
      Result := StrToInt (AString);
    except on EConvertError do
      Result := ADefault;
    end;
  end;

var
  i           : Integer;
  NewItem     : TVpLocalizeCountryItem;
  NewElement  : TVpLocalizeStatesItem;
  NewLanguage : TVpLocalizeLanguageItem;

begin
  if sValue = 'Countries' then begin
    FLoadingIndex := -1;
    FElementIndex := -1;

  end else if sValue = 'Country' then begin
    NewItem := TVpLocalizeCountryItem (FCountries.Add);
    FLoadingIndex := NewItem.Index;
    for i := 0 to FAttributes.Count - 1 do begin
      if (FAttributes.Items[i].Name = 'Name') and
         (Fattributes.Items[i].Value <> '') then
        NewItem.Name := FAttributes.Items[i].Value;
    end

  end else if sValue = 'State' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].StateCaption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'DupAbbr' then
        FCountries.Items[FLoadingIndex].StateDupAbbr :=
            GetBooleanValue (FAttributes.Items[i].Value, False)
      else if FAttributes.Items[i].Name = 'UseAbbr' then
        FCountries.Items[FLoadingIndex].StateUseAbbr :=
            GetBooleanValue (FAttributes.Items[i].Value, False)
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].StatesVisible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'Address1' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].Address1Caption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].Address1Visible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'Address2' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].Address2Caption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].Address2Visible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'Address3' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].Address3Caption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].Address3Visible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'Address4' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].Address4Caption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].Address4Visible :=
            GetBooleanValue (FAttributes.Items[i].Value, False);
    end;

  end else if sValue = 'City' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].CityCaption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].CityVisible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'Zipcode' then begin
    if FLoadingIndex < 0 then
      Exit;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Caption' then
        FCountries.Items[FLoadingIndex].ZipCaption := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Visible' then
        FCountries.Items[FLoadingIndex].ZipVisible :=
            GetBooleanValue (FAttributes.Items[i].Value, True);
    end;

  end else if sValue = 'LegalValue' then begin
    if FLoadingIndex < 0 then
      Exit;
    NewElement := TVpLocalizeStatesItem (FCountries.Items[FLoadingIndex].States.Add);
    FElementIndex := NewElement.Index;
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Name' then
        NewElement.Name := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'Value' then
        NewElement.Abbr := FAttributes.Items[i].Value;
    end;
  end else if sValue = 'Language' then begin
    if FLoadingIndex < 0 then
      Exit;
    NewLanguage := TVpLocalizeLanguageItem (FCountries.Items[FLoadingIndex].Languages.Add);
    for i := 0 to FAttributes.Count - 1 do begin
      if FAttributes.Items[i].Name = 'Name' then
        NewLanguage.Name := FAttributes.Items[i].Value
      else if FAttributes.Items[i].Name = 'ID' then
        NewLanguage.LanguageID := GetIntegerValue (FAttributes.Items[i].Value, -1)
      else if FAttributes.Items[i].Name = 'SubID' then
        NewLanguage.SubLanguageID := GetIntegerValue (FAttributes.Items[i].Value, -1);
    end;
  end;


  FAttributes.Clear;
end;

end.

