{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2003 Polycon Ab

    This is a licensed version of TTranslator, it may be used as described
    in the TTranslator license agreement. If you have not acquired a 
    commercial TTranslator license, your are using this product illegaly.    
}

{ $Id: RegComboBox.pas,v 1.4 2002/11/06 07:24:19 laa Exp $}

{-------------------------------------------------------------------------
  RegComboBox      ???

  Company          Polycon
  Authors          MVJ
-------------------------------------------------------------------------}

unit RegComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DataType;

type

  TRootKey = (rkHKEY_CLASSES_ROOT,
              rkHKEY_CURRENT_USER,
              rkHKEY_LOCAL_MACHINE,
              rkHKEY_USERS,
              rkHKEY_PERFORMANCE_DATA,
              rkHKEY_CURRENT_CONFIG,
              rkHKEY_DYN_DATA);

  TRegComboBox = class(TComboBox)
  private
    FRegKeyPath: String;
    FMaxItemCount: Integer;
    FRegRootKey: TRootKey;
    procedure SetRegKeyPath(const Value: String);
    procedure SetMaxItemCount(const Value: Integer);
    procedure SetRegRootKey(const Value: TRootKey);
  protected
    procedure Loaded; override;

    procedure TryLoad;
    procedure TrySave;
    procedure LoadItems;
    procedure SaveItems;
    procedure LoadItemsFromReg(ARootKey : HKEY; ARegKey : String; AValues : TValueList);
    procedure SaveItemsToReg(ARootKey : HKEY; ARegKey : String; AValues : TValueList);
    procedure RemoveExcessItems(AValues: TStrings; AMaxCount : Integer);
    procedure ItemsToNameValue(AValues : TValueList);
    procedure ItemsFromNameValue(AValues : TValueList);
  public
    constructor Create(AOwner : TComponent); override;
    procedure SaveToRegistry;
    procedure AddToItems(Value : String);
  published
    property RegRootKey : TRootKey read FRegRootKey write SetRegRootKey default rkHKEY_CURRENT_USER;
    property RegKeyPath : String read FRegKeyPath write SetRegKeyPath;
    property MaxItemCount : Integer read FMaxItemCount write SetMaxItemCount default 8;
  end;

const
  RootKeyValues : Array[Low(TRootKey)..High(TRootKey)] of HKEY =
    (HKEY_CLASSES_ROOT,           //  rkHKEY_CLASSES_ROOT
     HKEY_CURRENT_USER,           //  rkHKEY_CURRENT_USER
     HKEY_LOCAL_MACHINE,          //  rkHKEY_LOCAL_MACHINE
     HKEY_USERS,                  //  rkHKEY_USERS
     HKEY_PERFORMANCE_DATA,       //  rkHKEY_PERFORMANCE_DATA
     HKEY_CURRENT_CONFIG,         //  rkHKEY_CURRENT_CONFIG
     HKEY_DYN_DATA                //  rkHKEY_DYN_DATA
     );



implementation

uses
  Registry, Math;



{ TRegComboBox }

procedure TRegComboBox.AddToItems(Value : String);
var
  i : Integer;
begin
  i := Items.IndexOf(Text);
  if i >= 0 then
    Items.Move(i, 0)
  else
  begin
    Items.Insert(0, Text);
    RemoveExcessItems(Items, MaxItemCount);
  end;
  ItemIndex := 0;
end;

constructor TRegComboBox.Create(AOwner: TComponent);
begin
  FRegRootKey := rkHKEY_CURRENT_USER;
  FMaxItemCount := 8;
  inherited;
end;

procedure TRegComboBox.ItemsFromNameValue(AValues: TValueList);
var
  i : Integer;
begin
  Items.Clear;
  Text := '';
  for i := 0 to AValues.Count -1 do
    Items.Add(AValues.Strings[i]);

  ItemIndex := 0;
end;

procedure TRegComboBox.ItemsToNameValue(AValues: TValueList);
var
  i : Integer;
begin
  AValues.Clear;

  for i := 0 to Items.Count -1 do
    AValues.AddValue(Items[i], ValueFromInteger(i), nil);
end;

procedure TRegComboBox.Loaded;
begin
  inherited;
  TryLoad;
end;

procedure TRegComboBox.LoadItems;
var
  TmpValues : TValueList;
begin
  TmpValues := TValueList.Create(IntegerType);
  TmpValues.Sorted := True;
  TmpValues.Duplicates := dupError;

  try
    LoadItemsFromReg(RootKeyValues[RegRootKey], RegKeyPath, TmpValues);
    ItemsFromNameValue(TmpValues);
    RemoveExcessItems(Items, MaxItemCount);
  except
  end;
  TmpValues.Free;
end;

procedure TRegComboBox.LoadItemsFromReg(ARootKey : HKEY; ARegKey: String;
  AValues: TValueList);
var
  AReg : TRegistry;
  i, idx : Integer;
  TmpStrings : TStringList;
begin
  AValues.Clear;
  AReg := TRegistry.Create;
  TmpStrings := TStringList.Create;
  try
    AReg.RootKey := ARootKey;
    if AReg.OpenKeyReadOnly(ARegKey) then
    begin
      AReg.GetValueNames(TmpStrings);
      for i := 0 to TmpStrings.Count -1 do
        try
          idx := StrToInt(TmpStrings[i]);
          AValues.AddValue(AReg.ReadString(TmpStrings[i]), ValueFromInteger(idx), nil);
        except
        end;
    end;
  except
  end;
  AReg.CloseKey;
  AReg.Free;
  TmpStrings.Free;
end;

procedure TRegComboBox.RemoveExcessItems(AValues: TStrings; AMaxCount : Integer);
begin
  while AValues.Count > AMaxCount do
    AValues.Delete(AValues.Count-1);
end;

procedure TRegComboBox.SaveItems;
var
  TmpValues : TValueList;
begin
  AddToItems(Text);
  TmpValues := TValueList.Create(IntegerType);
  TmpValues.Sorted := True;
  TmpValues.Duplicates := dupError;
  try
    RemoveExcessItems(Items, MaxItemCount);
    ItemsToNameValue(TmpValues);
    SaveItemsToReg(RootKeyValues[RegRootKey], RegKeyPath, TmpValues);
  except
  end;
  TmpValues.Free;
end;

procedure TRegComboBox.SaveItemsToReg(ARootKey : HKEY; ARegKey : String; AValues : TValueList);
var
  AReg : TRegistry;
  TmpStrings : TStringList;
  i : Integer;
begin
  AReg := TRegistry.Create;
  try
    AReg.RootKey := ARootKey;
    if AReg.OpenKey(ARegKey, True) then
    begin
      TmpStrings := TStringList.Create;
      try
        AReg.GetValueNames(TmpStrings);
        for i := 0 to TmpStrings.Count -1 do
          AReg.DeleteValue(TmpStrings[i]);
      except
      end;
      TmpStrings.Free;

      for i := 0 to AValues.Count -1 do
        AReg.WriteString(AsString(AValues[i]), AValues.Strings[i]);
    end;
  except
  end;
  AReg.CloseKey;
  AReg.Free;
end;

procedure TRegComboBox.SaveToRegistry;
begin
  TrySave;
end;

procedure TRegComboBox.SetMaxItemCount(const Value: Integer);
begin
  FMaxItemCount := Value;
  RemoveExcessItems(Items, MaxItemCount);
end;

procedure TRegComboBox.SetRegKeyPath(const Value: String);
begin
  FRegKeyPath := Value;
  TryLoad;
end;

procedure TRegComboBox.SetRegRootKey(const Value: TRootKey);
begin
  FRegRootKey := Value;
  TryLoad;
end;

procedure TRegComboBox.TryLoad;
begin
  if not (csReading in ComponentState) and
    (FRegKeyPath<>'') then
    LoadItems;
end;

procedure TRegComboBox.TrySave;
begin
  SaveItems;
end;

end.

