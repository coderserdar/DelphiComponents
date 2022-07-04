unit u_regframeworkcomponents;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


interface

uses
  Classes,
{$IFDEF FPC}
  lresources,
{$ENDIF}
  SysUtils;


procedure Register;

implementation

uses unite_messages, U_DBListView, U_ExtDBNavigator,
     u_framework_dbcomponents, u_framework_components,
     U_OnFormInfoIni,U_ExtNumEdits,U_ExtColorCombos,
{$IFDEF FPC}
     ComponentEditors, dbpropedits, PropEdits,
{$ELSE}
     DBReg, Designintf,
{$ENDIF}
     U_ExtDBImage;

procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS_DB, [TDBListView,TExtDBNavigator,
                                                TExtDBImage,TExtDBNumEdit,
                                                TExtDBColorCombo,
                                                TFWDBEdit,TFWDBLookupCombo,TFWDBGrid,TFWDBMemo,TFWDBDateEdit{$IFNDEF FPC},TFWDBDateTimePicker{$ENDIF}]);
  RegisterComponents(CST_PALETTE_COMPOSANTS   , [TOnFormInfoIni,
                                                TExtNumEdit,
                                                TExtColorCombo,
                                                TFWLabel, TFWEdit,TFWGrid,TFWMemo,{$IFDEF FPC}TFWDateEdit{$ELSE}TFWDateTimePicker{$ENDIF}]);
{$IFNDEF FPC}
  RegisterPropertyEditor ( TypeInfo(string), TExtDBNavigator, 'SortField', TDataFieldProperty);
{$ENDIF}
  RegisterPropertyEditor ( TypeInfo({$IFDEF FPC}ShortString {$ELSE}string{$ENDIF}), TDBListView, 'DataKeyUnit'   , {$IFDEF FPC}TFieldProperty{$ELSE}TDataFieldProperty{$ENDIF});
  RegisterPropertyEditor ( TypeInfo({$IFDEF FPC}ShortString {$ELSE}string{$ENDIF}), TDBListView, 'DataSort'      , {$IFDEF FPC}TFieldProperty{$ELSE}TDataFieldProperty{$ENDIF});
  RegisterPropertyEditor ( TypeInfo(string), TDBListView, 'DataFieldsDisplay'   , {$IFDEF FPC}TFieldProperty{$ELSE}TDataFieldProperty{$ENDIF});
end;

initialization
{$IFDEF FPC}
  {$i u_framework_components.lrs}
  {$i u_framework_dbcomponents.lrs}
  {$I U_RegImageComponents.lrs}
  {$i U_ExtDBNavigator.lrs}
  {$i U_DBListView.lrs}
  {$i U_OnFormInfoIni.lrs}
  {$i U_ExtColorCombos.lrs}
  {$i U_ExtNumEdits.lrs}
{$ENDIF}
end.

