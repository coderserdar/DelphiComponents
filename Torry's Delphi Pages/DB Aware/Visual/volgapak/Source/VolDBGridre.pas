//---------------------------------------------------------------------------
//  Registering unit for TVolgaDBGrid
//---------------------------------------------------------------------------
//  Copyright © 2000, Olga Vlasova, Russia
//  http://volgatable.chat.ru
//  E-mail: volgatable@chat.ru
//---------------------------------------------------------------------------
unit VolDBGridRe;

interface

{$I Volga.INC}

procedure Register;

implementation

uses Classes, TypInfo,
  {$IFDEF VER140} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  VolDBGrid, VolDBGridEd;

procedure Register;
begin
  RegisterComponents('Volga', [TVolgaDBGrid]);
  RegisterComponentEditor(TVolgaDBGrid, TVolgaDBGridEditor);
  RegisterPropertyEditor(TypeInfo(TCollection), TVolgaDBGrid, 'Columns',
    TVolgaDBGridColumnsProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'FieldName', TVolgaColumnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupKeyField', TVolgaColumnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupLinkField', TVolgaColumnLookupKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TVolgaColumn, 'LookupDropDownFields', TVolgaColumnLookupKeyProperty);
end;

end.
