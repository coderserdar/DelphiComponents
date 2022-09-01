{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine                            }
{                                                       }
{       Copyright (c) 1996-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit OQBEregBDE;

interface

uses
  Classes, DB, DBTables, OQBEbde;

procedure Register;

implementation

uses
  DsgnIntf;

{ TOQBDatabaseNamePropertyBDE }

type
  TOQBDatabaseNamePropertyBDE = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TOQBDatabaseNamePropertyBDE.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TOQBDatabaseNamePropertyBDE.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TOQBDatabaseNamePropertyBDE.GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;

procedure Register;
begin
  RegisterComponents('OQBuilder', [TOQBEngineBDE]);
  RegisterPropertyEditor(TypeInfo(string), TOQBEngineBDE, 'DatabaseName', TOQBDatabaseNamePropertyBDE);
end;

end.
