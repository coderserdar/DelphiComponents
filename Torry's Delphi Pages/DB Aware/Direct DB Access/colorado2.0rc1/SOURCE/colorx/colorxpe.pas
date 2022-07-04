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

unit colorxpe;

{$I ..\COLORADO\CDEFINES.INC}

interface

uses Windows, SysUtils, Classes, Dialogs, Graphics,
     Registry,
{$IFDEF VCL60}
     DesignIntf,
     DesignEditors,
{$ELSE}
     DsgnIntf,
{$ENDIF}
     Controls, Forms, TypInfo;

procedure Register;

implementation
uses colorx;


type

{$IFDEF VCL50}

{:-- TComponentRequiredProperty category}

  TRequiredComponentProperty = class(TComponentProperty)
  public
{    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;}
  end;

{:-- TStringRequiredProperty category}

  TRequiredStringProperty = class(TStringProperty)
  public
{    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;}
  end;

{$ELSE}
  TRequiredStringProperty = class(TStringProperty);
  TRequiredComponentProperty = class(TStringProperty);
{$ENDIF}


{:-- TCatalogOrUsersProperty}

  TCatalogOrUsersProperty = class(TRequiredComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{:-- TCatalogOrGroupssProperty}

  TCatalogOrGroupsProperty = class(TRequiredComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{$IFDEF VCL50}

procedure DrawRequiredPropName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean; AName: string);
var PrevFontStyle: TFontStyles;
begin
  PrevFontStyle := ACanvas.Font.Style;
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
  ACanvas.TextRect(ARect, ARect.Left + 1, ARect.Top + 1, AName);
  ACanvas.Font.Style := PrevFontStyle;
end;

{:-- TComponentRequiredProperty}

{procedure TRequiredComponentProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DrawRequiredPropName(ACanvas, ARect, ASelected, GetName);
end;

{:-- TStringRequiredProperty}

{procedure TRequiredStringProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if UpperCase(GetName) = 'INDEXNAME'
     then inherited PropDrawName(ACanvas, ARect, ASelected)
     else DrawRequiredPropName(ACanvas, ARect, ASelected, GetName);
end;
 }
{$ENDIF}

{:-- TCatalogOrUsersProperty }

procedure TCatalogOrUsersProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetComponentNames(GetTypeData(TypeInfo(TDBCatalog)), Proc);
  Designer.GetComponentNames(GetTypeData(TypeInfo(TDBUsers)), Proc);
end;

{:-- TCatalogOrGroupsProperty }

procedure TCatalogOrGroupsProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetComponentNames(GetTypeData(TypeInfo(TDBCatalog)), Proc);
  Designer.GetComponentNames(GetTypeData(TypeInfo(TDBGroups)), Proc);
end;

procedure Register;
begin
  RegisterComponents('colorADO', [TDBCatalog,
                                    TDBTables,
                                    TDBIndexes,
                                    TDBKeys,
                                    TDBColumns,
                                    TDBViews,
                                    TDBProcedures,
                                    TDBParameters,
                                    TDBGroups,
                                    TDBUsers,
                                    TDBPermissions,
                                    TDBProperties]);
  {$IFDEF VCL50}
  RegisterPropertyEditor(TypeInfo(TComponent), TDBCatalog, 'Connection', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBTables, 'Catalog', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBViews, 'Catalog', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBProcedures, 'Catalog', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBIndexes, 'Tables', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBKeys, 'Tables', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBColumns, 'Parent', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBParameters, 'Parent', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBPermissions, 'UsersOrGroups', TRequiredComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBProperties, 'Parent', TRequiredComponentProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TComponent), TDBGroups, 'CatalogOrUsers', TCatalogOrUsersProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TDBUsers, 'CatalogOrGroups', TCatalogOrGroupsProperty);
end;

end.

