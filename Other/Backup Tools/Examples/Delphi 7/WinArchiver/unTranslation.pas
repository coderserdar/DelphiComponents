unit unTranslation;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ArchiverRoot, TypInfo, unTransObj;

const
  WM_TRANSLATE = WM_USER + 1;

  sDefaultMsg = 1;
  sOk = 2;
  sOpenExistingArchive = 713;
  sCreateNewArchive = 714;
  sOpenSegment = 715;
  sFiles = 718;
  sFileAlreadyExists = 720;
  sResetArchive = 721;
  sDeleteArchive = 722;
  sByte = 723;
  sBytes = 724;
  sKb = 725;
  sMb = 726;
  sSelectedFiles = 727;
  sNotAvailable = 729;
  sRenameTo = 730;
  sCouldNotRename = 731;
  sSFXConfig = 732;
  sMakeSFX = 733;
  sMake = 734;
  sCouldNotMakeSFX = 735;
  sSetArchiveComment = 736;
  sArchiveComment = 737;
  sBusy = 738;
  sFileExecuted = 739;
  sOpenOrCreate = 740;
  sNoAssociation = 741;
  sName = 742;
  sDate = 743;
  sTime = 744;
  sSize = 745;
  sRatio = 746;
  sPacked = 747;
  sSeg = 748;
  sPath = 749;
  sFileAssoc = 799;
  sClosingArchive = 800;
  sRoot = 803;
  sClearingFileList = 811;
  sMakingFileList = 812;
  sSortingFileList = 813;
  sArchiveDoesNotExist = 814;

  function  GetStr( idx : Integer ) : String;
  procedure AddStr( id : Integer; const str : String );
  procedure TranslateTo( language : TLanguage );
  procedure InitXXForms;

implementation
{$I mmm.inc}

uses
  langEnglish,
  langFrench,
  langGerman,
  langChinese,
  //langChineseGB,
  langPortuguese,
  langItalian,
  langRussian,
  langSpanish,
  langDanish,
  langDutch,
  langCzech;

var
  gStrings : TStringList;
  gXXForms : TXXForms;

function GetStr( idx : Integer ) : String;

  // recursive algorithm
  function find( left, right : Integer ) : Integer;
  var
    middle, val : Integer;
  begin
    middle := (left + right) div 2;
    val := Integer(gStrings.Objects[middle]);
    if val = idx then
      Result := middle
    else if (left < right) and (idx < val) then
      Result := find( left, middle-1 )
    else if (left < right) and (idx > val) then
      Result := find( middle+1, right )
    else
      Result := -1;
  end;

  // iterativ algorithm
  function find2( left, right : Integer ) : Integer;
  var
    middle, val : Integer;
  begin
    Result := -1;
    while True do
      begin
        if left = right then
          begin
            if (left < gStrings.Count) and (Integer(gStrings.Objects[left]) = idx) then
              Result := left;
            Break;
          end
        else if left > right then
          Break;
        middle := (left + right) div 2;
        if middle >= gStrings.Count then
          Exit;
        val := Integer(gStrings.Objects[middle]);
        if (idx < val) then
          right := middle-1
        else if val = idx then
          begin
            Result := middle;
            Break;
          end
        else
          left := middle+1;
      end;
  end;

var
  i : Integer;
begin
  Result := '';
  if gStrings.Count > 0 then
    i := find2( 0, gStrings.Count - 1 )
  else
    i := -1;
  if i >= 0 then
    Result := gStrings.Strings[i];
{  for i := 0 to gStrings.Count - 1 do
    if Integer(gStrings.Objects[i]) = idx then
      begin
        Result := gStrings.Strings[i];
        Exit;
      end;}
end;

procedure AddStr( id : Integer; const str : String );
var
  i : Integer;
begin
  // if id is less than the last item, then find a place to insert this id
  if (gStrings.Count>0) and (Integer(gStrings.Objects[gStrings.Count-1]) > id) then
    begin
      for i := 0 to gStrings.Count - 1 do
        if Integer(gStrings.Objects[gStrings.Count-1]) > id then
          begin
            gStrings.InsertObject( i, str, TObject(id) );
            Break;
          end;
    end
  else // simply add this id at the end
    gStrings.AddObject( str, TObject(id) );
end;

procedure SetLanguage( language : TLanguage );
begin
  gStrings.Clear;
  case language of
  lgEnglish:    langEnglish.SetLanguage;
  lgFrench:     langFrench.SetLanguage;
  lgGerman:     langGerman.SetLanguage;
  lgChinese:    langChinese.SetLanguage;
  lgChineseGB:  langChinese.SetLanguage;
  lgPortuguese: langPortuguese.SetLanguage;
  lgItalian:    langItalian.SetLanguage;
  lgRussian:    langRussian.SetLanguage;
  lgSpanish:    langSpanish.SetLanguage;
  lgDanish:     langDanish.SetLanguage;
  lgDutch:      langDutch.SetLanguage;
  lgCzech:      langCzech.SetLanguage;
  end;
end;

function PropExists( obj : TObject; const PropName : String ) : Boolean;
var
  PropInfo : PPropInfo;
begin
  Result := False;
  if Assigned(obj) then
    begin
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      Result := Assigned(PropInfo);
    end;
end;

function GetPropString( obj : TObject; const PropName : String ) : String;
var
  PropInfo : PPropInfo;
begin
  Result := '';
  if Assigned(obj) then
    begin
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if Assigned(PropInfo) then
        Result := GetStrProp( obj, PropInfo );
    end;
end;

procedure SetPropString( obj : TObject; const PropName, Value : String );
var
  PropInfo : PPropInfo;
begin
  if Assigned(obj) then
    begin
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if Assigned(PropInfo) then
        SetStrProp( obj, PropInfo, Value );
    end;
end;

procedure TranslateComponent( comp : TComponent; C : TXXComponent );
var
  i : Integer;
  str : String;
begin
  if not Assigned(comp) or not Assigned(C) then
    Exit;
  for i := 0 to C.ItemCount - 1 do
    begin
      str := GetStr( C.Items[i].Id );
      if str <> '' then
        SetPropString( comp, C.Items[i].Name, str );
    end;
end;

procedure TranslateForm( form : TForm );
var
  F : TXXForm;
  C : TXXComponent;
  i : Integer;
begin
  if not Assigned(form) then
    Exit;
  F := gXXForms.Find( form.Name );
  if not Assigned(F) then
    Exit;
  TranslateComponent( form, F.This );
  for i := 0 to form.ComponentCount - 1 do
    begin
      C := F.Find( form.Components[i].Name );
      if Assigned( C ) then
        TranslateComponent( form.Components[i], C );
    end;
  SendMessage( form.Handle, WM_TRANSLATE, 0, 0 );
end;

procedure UpdateForms;
var
  i : Integer;
begin
  if not Assigned( gXXForms ) then
    Exit;
  for i := 0 to Screen.FormCount - 1 do
    TranslateForm( Screen.Forms[i] );
end;

procedure TranslateTo( language : TLanguage );
begin
  if language = lgAutomatic then
    language := GetUserLanguage;
  SetLanguage( language );
  UpdateForms;
end;

procedure InitXXForms;

  procedure AddProperty( obj : TObject; C : TXXComponent; const name, value : String );
  var
    p : TXXProperty;
    idx : Integer;
  begin
    if (Length(value)>3) and (value[1] = '#') then
      begin
        idx := Pos( ':', value );
        if (idx > 2) then
          begin
            p := TXXProperty.Create;
            p.Name := name;
            p.Id := StrToIntDef( Copy( value, 2, idx - 2 ), 0 );
            C.Add( p );
            SetPropString( obj, name, Copy( value, idx+1, Length(value) ) );
          end;
      end;
  end;

  procedure AddProps( obj : TObject; C : TXXComponent );
  var
    I, Count: Integer;
    PropInfo: PPropInfo;
    TempList: PPropList;
    str : String;
  begin
    Count := GetTypeData(obj.ClassInfo)^.PropCount;
    if Count > 0 then
    begin
      GetMem(TempList, Count * SizeOf(Pointer));
      try
        GetPropInfos(obj.ClassInfo, TempList);
        for I := 0 to Count - 1 do
        begin
          PropInfo := TempList^[I];
          {$IFDEF D3}
            with PropInfo.PropType^^, GetTypeData(PropInfo.PropType^)^ do
          {$ELSE}
            with PropInfo^.PropType^, GetTypeData(PropInfo.PropType)^ do
          {$ENDIF}
            begin
              case Kind of
                tkString,
                {$IFDEF D3}
                tkWString,
                {$ENDIF}
                tkLString:
                  begin
                    str := GetStrProp( obj, PropInfo );
                    AddProperty( obj, C, PropInfo^.Name, str );
                  end;
              end;
            end;
        end;
      finally
        FreeMem(TempList, Count * SizeOf(Pointer));
      end;
    end;
  end;

  function MakeComponent( comp : TComponent ) : TXXComponent;
  begin
    Result := TXXComponent.Create;
    Result.Name := comp.Name;
    AddProps( comp, Result );
  end;

var
  i, j : Integer;
  F : TXXForm;
  C : TXXComponent;
  comp : TComponent;
begin
  if Assigned(gXXForms) then
    gXXForms.Free;
  gXXForms := TXXForms.Create;
  for i := 0 to Screen.FormCount - 1 do
    with Screen.Forms[i] do
      begin
        F := TXXForm.Create;
        F.Name := Name;
        F.This := MakeComponent( Screen.Forms[i] );
        gXXForms.Add( F );
        for j := 0 to ComponentCount - 1 do
          begin
            comp := Components[j];
            C := MakeComponent( comp );
            F.Add( C );
          end;
      end;
end;

initialization
  gStrings := TStringList.Create;
finalization
  gStrings.Free;
  gXXForms.Free;
end.
