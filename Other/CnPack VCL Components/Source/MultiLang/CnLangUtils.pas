{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnLangUtils;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ����﹤���൥Ԫ
* ��Ԫ���ߣ�CnPack������ ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫ�����˶��﹤����
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2006.10.12 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, SysConst, Classes, Windows;

type
{ TCnLangStringExtractor }

  TLangTransFilter = (tfFont, tfCaption, tfCategory, tfHelpKeyword, tfHint,
    tfText, tfImeName, tfTitle, tfDefaultExt, tfFilter, tfInitialDir,
    tfSubItemsText, tfOthers);

  TLangTransFilterSet = set of TLangTransFilter;

  TCnLangStringExtractor = class
  private
    FFilterOptions: TLangTransFilterSet;
  protected
    procedure GetObjectStrings(AOwner: TComponent; AObject: TObject; Strings: TStrings;
      const BaseName: string; SkipEmptyStr: Boolean);
    procedure GetRecurComponentStrings(AOwner: TComponent; AComponent: TComponent;
      AList: TList; Strings: TStrings; const BaseName: string; SkipEmptyStr: Boolean); virtual;
    {* �ݹ���һ Component �Լ����� Component ���ִ� }
    procedure GetRecurObjectStrings(AOwner: TComponent; AObject: TObject; AList: TList;
      Strings: TStrings; const BaseName: string; SkipEmptyStr: Boolean); virtual;
    {* �ݹ���һ Object �����Լ��������Զ�����ִ� }
  public
    constructor Create;
    {* ȡ��һ�������������ַ���}
    procedure GetFormStrings(AForm: TComponent; Strings: TStrings; SkipEmptyStr: Boolean = False);
    {* ���һ Form �ϵ������ִ� }
    procedure GetComponentStrings(AComponent: TComponent; Strings: TStrings;
      const BaseName: string = ''; SkipEmptyStr: Boolean = False);
    {* ���һ Component �������ִ� }
    procedure SetFilterOptions(const AFilterOptions: TLangTransFilterSet);
    {* ���ù��� *}
  end;

  {* �� SysUtils �� TLanguages ��ֲ������������ DEP ����������б���}
  TCnLanguages = class(TObject)
  private
    FSysLangs: array of TLangRec;
    function LocalesCallback(LocaleID: PChar): Integer; stdcall;
    function GetExt(Index: Integer): string;
    function GetID(Index: Integer): string;
    function GetLCID(Index: Integer): LCID;
    function GetName(Index: Integer): string;
    function GetNameFromLocaleID(ID: LCID): string;
    function GetNameFromLCID(const ID: string): string;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(ID: LCID): Integer;
    property Count: Integer read GetCount;
    property Name[Index: Integer]: string read GetName;
    property NameFromLocaleID[ID: LCID]: string read GetNameFromLocaleID;
    property NameFromLCID[const ID: string]: string read GetNameFromLCID;
    property ID[Index: Integer]: string read GetID;
    property LocaleID[Index: Integer]: LCID read GetLCID;
    property Ext[Index: Integer]: string read GetExt;
  end;

function CnLanguages: TCnLanguages;
{* ����ȫ�ֵ� CnLanguages �б���}

implementation

uses
  {$IFDEF COMPILER6_UP}Variants, {$ENDIF}
  Forms, Dialogs, Graphics, Menus, Grids, ComCtrls, Controls, ExtCtrls,
  ToolWin, ActnList, ImgList, TypInfo, StdCtrls, CnCommon, CnIniStrUtils,
  Clipbrd, CnLangMgr, CnClasses, CnLangConsts, CnLangStorage;
  
const
  THUNK_SIZE = 4096; // x86 ҳ��С
  
var
  FLanguages: TCnLanguages;
  FTempLanguages: TCnLanguages = nil;

function EnumLocalesCallback(LocaleID: PChar): Integer; stdcall;
begin
  Result := FTempLanguages.LocalesCallback(LocaleID);
end;
  
{ TCnLanguages }

function GetLocaleDataW(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of WideChar;
begin
  Buffer[0] := #0;
  GetLocaleInfoW(ID, Flag, Buffer, SizeOf(Buffer) div 2);
  Result := Buffer;
end;

function GetLocaleDataA(ID: LCID; Flag: DWORD): string;
var
  Buffer: array[0..1023] of AnsiChar;
begin
  Buffer[0] := #0;
  SetString(Result, Buffer, GetLocaleInfoA(ID, Flag, Buffer, SizeOf(Buffer)) - 1);
end;

function TCnLanguages.LocalesCallback(LocaleID: PChar): Integer; stdcall;
var
  AID: LCID;
  ShortLangName: string;
  GetLocaleDataProc: function (ID: LCID; Flag: DWORD): string;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    GetLocaleDataProc := @GetLocaleDataW
  else
    GetLocaleDataProc := @GetLocaleDataA;
  AID := StrToInt('$' + Copy(LocaleID, 5, 4));
  ShortLangName := GetLocaleDataProc(AID, LOCALE_SABBREVLANGNAME);
  if ShortLangName <> '' then
  begin
    SetLength(FSysLangs, Length(FSysLangs) + 1);
    with FSysLangs[High(FSysLangs)] do
    begin
      FName := GetLocaleDataProc(AID, LOCALE_SLANGUAGE);
      FLCID := AID;
      FExt := ShortLangName;
    end;
  end;
  Result := 1;
end;

constructor TCnLanguages.Create;
begin
  inherited Create;
  FTempLanguages := Self; 
  EnumSystemLocales(@EnumLocalesCallback, LCID_SUPPORTED);
end;

destructor TCnLanguages.Destroy;
begin

  inherited;
end;

function TCnLanguages.GetCount: Integer;
begin
  Result := High(FSysLangs) + 1;
end;

function TCnLanguages.GetExt(Index: Integer): string;
begin
  Result := FSysLangs[Index].FExt;
end;

function TCnLanguages.GetID(Index: Integer): string;
begin
  Result := HexDisplayPrefix + IntToHex(FSysLangs[Index].FLCID, 8);
end;

function TCnLanguages.GetLCID(Index: Integer): LCID;
begin
  Result := FSysLangs[Index].FLCID;
end;

function TCnLanguages.GetName(Index: Integer): string;
begin
  Result := FSysLangs[Index].FName;
end;

function TCnLanguages.GetNameFromLocaleID(ID: LCID): string;
var
  Index: Integer;
begin
  Index := IndexOf(ID);
  if Index <> - 1 then Result := Name[Index];
  if Result = '' then Result := SUnknown;
end;

function TCnLanguages.GetNameFromLCID(const ID: string): string;
begin
  Result := NameFromLocaleID[StrToIntDef(ID, 0)];
end;

function TCnLanguages.IndexOf(ID: LCID): Integer;
begin
  for Result := Low(FSysLangs) to High(FSysLangs) do
    if FSysLangs[Result].FLCID = ID then Exit;
  Result := -1;
end;

function CnLanguages: TCnLanguages;
begin
  if FLanguages = nil then
    FLanguages := TCnLanguages.Create;
  Result := FLanguages;
end;

{ TCnLangStringExtractor }

constructor TCnLangStringExtractor.Create;
begin
  SetFilterOptions([]);
end;

procedure TCnLangStringExtractor.GetComponentStrings(AComponent: TComponent;
  Strings: TStrings; const BaseName: string; SkipEmptyStr: Boolean);
var
  AList: TList;
begin
  if (Strings <> nil) and (AComponent.ComponentCount > 0) then
  begin
    AList := TList.Create;
    try
      if AComponent.Owner = nil then
        GetRecurComponentStrings(AComponent, AComponent, AList, Strings, BaseName, SkipEmptyStr)
      else
        GetRecurComponentStrings(nil, AComponent, AList, Strings, BaseName, SkipEmptyStr)
    finally
      AList.Free;
    end;
  end
  else
    GetObjectStrings(nil, AComponent, Strings, BaseName, SkipEmptyStr);
end;

procedure TCnLangStringExtractor.GetFormStrings(AForm: TComponent;
  Strings: TStrings; SkipEmptyStr: Boolean);
begin
  GetComponentStrings(AForm, Strings, AForm.ClassName, SkipEmptyStr);
end;

procedure TCnLangStringExtractor.GetObjectStrings(AOwner: TComponent;
  AObject: TObject; Strings: TStrings; const BaseName: string; SkipEmptyStr: Boolean);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetRecurObjectStrings(AOwner, AObject, AList, Strings, BaseName, SkipEmptyStr);
  finally
    AList.Free;
  end;
end;

procedure TCnLangStringExtractor.GetRecurComponentStrings(AOwner: TComponent;
  AComponent: TComponent; AList: TList; Strings: TStrings; const BaseName: string;
  SkipEmptyStr: Boolean);
var
  I: Integer;
  T: TComponent;
begin
  if (AComponent <> nil) and (AList <> nil) and (AList.IndexOf(AComponent) = -1) then
  begin
    GetRecurObjectStrings(AOwner, AComponent, AList, Strings, BaseName, SkipEmptyStr);
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      T := AComponent.Components[I];
      if AComponent is TCustomForm then
        GetRecurComponentStrings(AOwner, T, AList, Strings, BaseName, SkipEmptyStr)
      else
        GetRecurComponentStrings(AOwner, T, AList, Strings, BaseName + DefDelimeter + AComponent.Name, SkipEmptyStr);
    end;
  end;
end;

procedure TCnLangStringExtractor.GetRecurObjectStrings(AOwner: TComponent;
  AObject: TObject; AList: TList; Strings: TStrings; const BaseName: string;
  SkipEmptyStr: Boolean);
var
  i: Integer;
  APropName, APropValue, AStr: string;
  APropType: TTypeKind;
  Data: PTypeData;
  ActionObj, SubObj: TObject;
  AItem: TCollectionItem;
  AListItem: TListItem;
  ATreeNode: TTreeNode;
  IsForm: Boolean;
  NeedIgnoreAction: Boolean;
  ActionCaption, ActionHint: string;
  Info: PPropInfo;
begin
  if (AObject <> nil) and (AList <> nil) and (AList.IndexOf(AObject) = -1) then
  begin
    AList.Add(AObject);

    // ���⴫��һЩҰ�˵� AObject ������ѭ�������� IDE �ڲ����ֹ�
    try
      if AObject.ClassType = AObject.ClassParent then
        Exit;
      
      if (AObject.ClassParent <> nil) and (AObject.ClassParent.ClassParent = AObject.ClassType) then
        Exit;
    except
      Exit;
    end;
  
    if (AObject is TCnCustomLangStorage) or (AObject is TCnCustomLangStorage)
      or ((AObject is TComponent) and ((AObject as TComponent).Name = '')) then
        Exit;

    if (AObject is TStrings) then  // Strings�Ķ���ֱ�Ӽ����� Text ���ԡ�
    begin
      if not (tfText in FFilterOptions) then
        Exit;

      AStr := 'Text';
      if BaseName <> '' then
        AStr := BaseName + DefDelimeter + AStr;

      if not SkipEmptyStr or ((AObject as TStrings).Text <> '') then
        Strings.Add(AStr + DefEqual + StringReplace((AObject as TStrings).Text,
          SCnCRLF, SCnBR, [rfReplaceAll, rfIgnoreCase]));
      Exit;
    end
    else if (AObject is TCollection) then // TCollection ��������� Item
    begin
      for i := 0 to (AObject as TCollection).Count - 1 do
      begin
        AItem := (AObject as TCollection).Items[i];
        if BaseName <> '' then
          GetRecurObjectStrings(AOwner, AItem, AList, Strings, BaseName + DefDelimeter
            + 'Item' + InttoStr(i), SkipEmptyStr)
        else
          GetRecurObjectStrings(AOwner, AItem, AList, Strings, 'Item' + InttoStr(i), SkipEmptyStr);
      end;
    end
    // ListView ����Ҫʱ������ Item
    else if CnLanguageManager.TranslateListItem and (AObject is TListView) then
    begin
      for i := 0 to (AObject as TListView).Items.Count - 1 do
      begin
        AListItem := (AObject as TListView).Items[i];
        if BaseName <> '' then
          GetRecurObjectStrings(AOwner, AListItem, AList, Strings, BaseName + DefDelimeter
            + TComponent(AObject).Name + DefDelimeter + 'ListItem' + InttoStr(i), SkipEmptyStr)
        else
          GetRecurObjectStrings(AOwner, AListItem, AList, Strings,
            TComponent(AObject).Name + DefDelimeter + 'ListItem' + InttoStr(i), SkipEmptyStr);
      end;
    end
    // �� ListItem ʱ������ Caption ���Ժ� SubItems ����
    else if CnLanguageManager.TranslateListItem and (AObject is TListItem) then
    begin
      if (tfCaption in FFilterOptions) then
        begin
          AStr := 'Caption';
          if BaseName <> '' then
            AStr := BaseName + DefDelimeter + AStr;

          if not SkipEmptyStr or ((AObject as TListItem).Caption <> '') then
            Strings.Add(AStr + DefEqual + (AObject as TListItem).Caption);
        end;

      if (tfSubItemsText in FFilterOptions) then
        begin
          AStr := 'SubItems.Text';
          if BaseName <> '' then
            AStr := BaseName + DefDelimeter + AStr;

          if not SkipEmptyStr or ((AObject as TListItem).SubItems.Text <> '') then
            Strings.Add(AStr + DefEqual + (AObject as TListItem).SubItems.Text);
        end;
      Exit;
    end
    // TreeView ����Ҫʱ������ Item
    else if CnLanguageManager.TranslateTreeNode and (AObject is TTreeView) then
    begin
      for i := 0 to (AObject as TTreeView).Items.Count - 1 do
      begin
        ATreeNode := (AObject as TTreeView).Items[i];
        if BaseName <> '' then
          GetRecurObjectStrings(AOwner, ATreeNode, AList, Strings, BaseName + DefDelimeter
            + TComponent(AObject).Name + DefDelimeter + 'TreeNode' + InttoStr(i), SkipEmptyStr)
        else
          GetRecurObjectStrings(AOwner, ATreeNode, AList, Strings,
            TComponent(AObject).Name + DefDelimeter + 'TreeNode' + InttoStr(i), SkipEmptyStr);
      end;
    end
    // �� TreeNode ʱ������ Text ����
    else if CnLanguageManager.TranslateTreeNode and (AObject is TTreeNode) then
    begin
      if not (tfText in FFilterOptions) then
        Exit;

      AStr := 'Text';
      if BaseName <> '' then
        AStr := BaseName + DefDelimeter + AStr;

      if not SkipEmptyStr or ((AObject as TTreeNode).Text <> '') then
        Strings.Add(AStr + DefEqual + (AObject as TTreeNode).Text);
      Exit;
    end;

    IsForm := (AObject is TCustomForm); // or (AObject is TCustomFrame);
    // if IsForm then IsForm := (AObject as TWinControl).Parent = nil;

    try
      Data := GetTypeData(AObject.Classinfo);
    except
      Exit; // TChartSeriesList ���ڴ˴��������ò�ץס����
    end;

    NeedIgnoreAction := False;
    if CnLanguageManager.IgnoreAction then
    begin
      // �����Ƿ��� Action ���ԣ����Ƿ� nil
      for I := 0 to Data^.PropCount - 1 do
      begin
        APropName := GetPropName(AObject, I);
        if (PropType(AObject, APropName) = tkClass) and (APropName = 'Action') then
        begin
          // ���� Action ���ԣ�ΪtkClass
          ActionObj := GetObjectProp(AObject, APropName);
          if (ActionObj <> nil) and (ActionObj is TCustomAction)then
          begin
            // �� Action ���Բ�Ϊ nil �ģ���Ҫ��¼ Caption �� Hint ���Ƚ�
            NeedIgnoreAction := True;
            ActionCaption := (ActionObj as TCustomAction).Caption;
            ActionHint := (ActionObj as TCustomAction).Hint;
            Break;
          end;
        end;
      end;
    end;

    for I := 0 to Data^.PropCount - 1 do
    begin
      APropName := GetPropName(AObject, I);

      // ������ TComponent �� Name ����
      if (AObject is TComponent) and (APropName = 'Name') then
        Continue;
      // ������ TCnComponent �� About ����
      if (AObject is TCnComponent) and (APropName = 'About') then
        Continue;

      APropType := PropType(AObject, APropName);
      if (APropType in [tkString, tkLString, tkWString //, tkWChar
        {$IFDEF UNICODE_STRING}, tkUString{$ENDIF}]) then
      begin
        try
          APropValue := VartoStr(GetPropValue(AObject, APropName));
        except
          // ���� OLE �������ȡ WideString ����ʱ�����Ӹ�����
          Continue;
        end;

        if NeedIgnoreAction then
        begin
          if (APropName = 'Caption') and (ActionCaption = APropValue) then
            Continue
          else if (APropName = 'Hint') and (ActionHint = APropValue) then
            Continue;
        end;

        Info := GetPropInfo(AObject, APropName);
        if (Info <> nil) and (Info^.SetProc = nil) then // ֻ������д�ģ��㿪
          Continue;

        // �����������
        if (APropName = 'Caption') then
        begin
          if not (tfCaption in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'Category') then
        begin
          if not (tfCategory in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'HelpKeyword') then
        begin
          if not (tfHelpKeyword in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'Hint') then
        begin
          if not (tfHint in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'ImeName') then
        begin
          if not (tfImeName in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'Title') then
        begin
          if not (tfTitle in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'DefaultExt') then
        begin
          if not (tfDefaultExt in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'Filter') then
        begin
          if not (tfFilter in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if (APropName = 'InitialDir') then
        begin
          if not (tfInitialDir in FFilterOptions) then
          begin
            Continue;
          end;
        end
        else if not (tfOthers in FFilterOptions) then
        begin
          Continue;
        end;

        if IsForm then
          AStr := AObject.ClassName + DefDelimeter + APropName
        else if AObject is TComponent then
          AStr := TComponent(AObject).Name + DefDelimeter + APropName
        else
          AStr := APropName;

        if (BaseName <> '') and not IsForm then
          AStr := BaseName + DefDelimeter + AStr;

        if not SkipEmptyStr or (APropValue <> '') then
          Strings.Add(AStr + DefEqual + APropValue);
      end
      else if APropType = tkClass then
      begin
        SubObj := GetObjectProp(AObject, APropName);
        if (SubObj is TComponent) and (AOwner <> nil) and
          ((SubObj as TComponent).Owner = AOwner) then
        begin
           // �Ӷ����Ǵ����ֱϵ���ʱ���������﷭��
        end
        else if AObject is TComponent then
        begin
          if AList.IndexOf(SubObj) = -1 then
          begin
            if (AObject is TControl) and (SubObj is TFont) and (APropName = 'Font') then
            begin
              if (tfFont in FFilterOptions) then
                if not IsParentFont(AObject as TControl) then // ��ʹ�� ParentFont ʱ������
                begin
                  if not IsForm then
                    AStr := TComponent(AObject).Name + DefDelimeter + SCnControlFont
                  else
                    AStr := SCnControlFont;

                  if BaseName <> ''  then
                    AStr := BaseName + DefDelimeter + AStr;

                  AList.Add(SubObj);
                  Strings.Add(AStr + DefEqual + FontToStringEx(SubObj as TFont,
                    GetParentFont(AObject as TComponent)));
                end;
            end // �������洦�� TControl������
            else if CnLanguageManager.TranslateOtherFont and (SubObj is TFont) then
            begin
              if (tfFont in FFilterOptions) then
                begin
                  if not IsForm then
                    AStr := TComponent(AObject).Name + DefDelimeter +
                      SystemNamePrefix + APropName
                  else
                    AStr := SystemNamePrefix + APropName;

                  if BaseName <> ''  then
                    AStr := BaseName + DefDelimeter + AStr;

                  AList.Add(SubObj);
                  Strings.Add(AStr + DefEqual + FontToStringEx(SubObj as TFont,
                    GetParentFont(AObject as TComponent)));
                end;                    
            end
            else if not (SubObj is TComponent) or ((SubObj as TComponent).Owner = nil) then
            begin
              if IsForm then
                GetRecurObjectStrings(AOwner, SubObj, AList, Strings,
                  TComponent(AObject).ClassName + DefDelimeter + APropName, SkipEmptyStr)
              else if (InheritsFromClassName(AObject, 'TNotebook') or InheritsFromClassName(AObject, 'TTabbedNotebook'))
                and (APropName = 'Pages') then
                // ����ȡ TNotebook/TTabbedNotebook �� Pages ���ԣ�������ַ����ҳ�����ݶ�ʧ��
              else
                GetRecurObjectStrings(AOwner, SubObj, AList, Strings, BaseName +
                  DefDelimeter + TComponent(AObject).Name + DefDelimeter + APropName, SkipEmptyStr);
            end;
          end;
        end
        else
        begin
          GetRecurObjectStrings(AOwner, SubObj, AList, Strings,
            BaseName + DefDelimeter + APropName, SkipEmptyStr);
        end;
      end;
    end;
  end;
end;

procedure TCnLangStringExtractor.SetFilterOptions(
  const AFilterOptions: TLangTransFilterSet);
begin
  if AFilterOptions = [] then
  begin
    Include(FFilterOptions, tfFont);
    Include(FFilterOptions, tfCaption);
    Include(FFilterOptions, tfCategory);
    Include(FFilterOptions, tfHelpKeyword);
    Include(FFilterOptions, tfHint);
    Include(FFilterOptions, tfText);
    Include(FFilterOptions, tfImeName);
    Include(FFilterOptions, tfTitle);
    Include(FFilterOptions, tfDefaultExt);
    Include(FFilterOptions, tfFilter);
    Include(FFilterOptions, tfInitialDir);
    Include(FFilterOptions, tfSubItemsText);
    Include(FFilterOptions, tfOthers);
  end
  else
    FFilterOptions := AFilterOptions;
end;

initialization

finalization
  if FLanguages <> nil then
    FreeAndNil(FLanguages);

end.
