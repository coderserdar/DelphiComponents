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

{ $Id: LangArray.pas,v 1.6 2002/12/30 07:21:30 laa Exp $ }

unit LangArray;

interface

uses
  Classes;

type
  TCommonLangArray = class; // forward

  TGetLanguageEvent = procedure(Sender : TCommonLangArray; var Language : Integer) of object;

  TCommonLangArray = class
  private
    FOnGetCurrentLanguage : TGetLanguageEvent;

    function GetCount : Integer;
    procedure SetCount(NewCount : Integer);
    function GetCurrenntIndex : Integer;
  protected
    FLanguages : TStringList;

    function GetDefaultTextValue : String; virtual;
    function GetDefaultObjValue : TObject; virtual;

    property DefaultTextValue : String read GetDefaultTextValue;
    property DefaultObjValue : TObject read GetDefaultObjValue;

    procedure AddDefaultValues(ToIndex : Integer);
    constructor Create;
  public
    destructor Destroy; override;

    property OnGetCurrentLanguage : TGetLanguageEvent read FOnGetCurrentLanguage write FOnGetCurrentLanguage;

    property Count : Integer read GetCount write SetCount;
    property CurrentIndex : Integer read GetCurrenntIndex;

    procedure Assign(ALangArray : TCommonLangArray); virtual;
  end;

  TLangArray = class(TCommonLangArray)
  private
    FDefaultText : String;

    procedure SetText(Index : Integer; Text : String);
    function GetText(Index : Integer) : String;
    function GetCurrentText : String;
  protected
    function GetDefaultTextValue : String; override;
  public
    property Texts[idx : Integer] : String read GetText write SetText;
    property Text : String read GetCurrentText;

    property DefaultTextValue : String read GetDefaultTextValue write FDefaultText;

    constructor Create(DefaultText : String);
    constructor CreateWithTexts(DefaultText : String; Texts : array of String);

    procedure SetAllTexts(Texts : array of String);
    procedure AddText(Text : String);

    procedure Assign(ALangArray : TCommonLangArray); override;
  end;

implementation

{ TCommonLangArray }

constructor TCommonLangArray.Create;
begin
  inherited Create;

  FLanguages := TStringList.Create;
end;

destructor TCommonLangArray.Destroy;
begin
  inherited Destroy;
  FLanguages.Free;
end;

procedure TCommonLangArray.Assign(ALangArray : TCommonLangArray);
begin
  if Self <> ALangArray then
    FLanguages.Assign(ALangArray.FLanguages);
end;

procedure TCommonLangArray.SetCount(NewCount : Integer);
begin
  if NewCount > Self.Count then
    AddDefaultValues(NewCount - 1)
  else
    while NewCount < Self.Count do
      FLanguages.Delete(Self.Count - 1);
end;

function TCommonLangArray.GetCount : Integer;
begin
  Result := FLanguages.Count;
end;

procedure TCommonLangArray.AddDefaultValues(ToIndex : Integer);
var
  i : Integer;
  DefaultText : String;
  DefaultObj : TObject;
begin
  if Count = 0 then
  begin
    DefaultText := Self.DefaultTextValue;
    DefaultObj := Self.DefaultObjValue;
  end
  else
  begin
    DefaultText := FLanguages.Strings[0];
    DefaultObj := FLanguages.Objects[0];
  end;

  for i := Count to ToIndex do
    FLanguages.AddObject(DefaultText, DefaultObj);
end;

function TCommonLangArray.GetCurrenntIndex : Integer;
begin
  Result := 0;
  if Assigned(OnGetCurrentLanguage) then
    OnGetCurrentLanguage(Self, Result);
end;

function TCommonLangArray.GetDefaultTextValue : String;
begin
  Result := '';
end;

function TCommonLangArray.GetDefaultObjValue : TObject;
begin
  Result := nil;
end;

{ TLangArray }

constructor TLangArray.Create(DefaultText : String);
begin
  inherited Create;
  FDefaultText := DefaultText;
end;

constructor TLangArray.CreateWithTexts(DefaultText : String; Texts : array of String);
begin
  Create(DefaultText);
  SetAllTexts(Texts);
end;

procedure TLangArray.SetText(Index : Integer; Text : String);
begin
  if Index >= Count then
    AddDefaultValues(Index);

  FLanguages.Strings[Index] := Text;
end;

function TLangArray.GetText(Index : Integer) : String;
begin
  if Index < Count then
    Result := FLanguages.Strings[Index]
  else if Count > 0 then
    Result := FLanguages.Strings[0]
  else
    Result := Self.DefaultTextValue;
end;

function TLangArray.GetCurrentText : String;
begin
  Result := Texts[CurrentIndex];
end;

procedure TLangArray.SetAllTexts(Texts : array of String);
var
  i : Integer;
begin
  FLanguages.Clear;
  for i := Low(Texts) to High(Texts) do
    AddText(Texts[i]);
end;

procedure TLangArray.AddText(Text : String);
begin
  FLanguages.Add(Text);
end;

procedure TLangArray.Assign(ALangArray : TCommonLangArray);
begin
  inherited Assign(ALangArray);

  if (Self <> ALangArray) and (ALangArray is TLangArray) then
  begin
    FDefaultText := TLangArray(ALangArray).FDefaultText;
  end;
end;

function TLangArray.GetDefaultTextValue : String;
begin
  Result := FDefaultText;
end;

end.

