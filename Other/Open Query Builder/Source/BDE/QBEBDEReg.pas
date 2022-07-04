{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for BDE Sources            }
{                                                       }
{       Copyright (c) 1996-2003 Sergey Orlik            }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Borland Moscow office                           }
{       Internet:  support@fast-report.com,             }
{                  sorlik@borland.com                   }
{                  http://www.fast-report.com           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEBDEReg;

interface

uses
  Classes, DB, DBTables, QBEBDE;

procedure Register;

implementation

uses
{$IFNDEF Delphi6}
  DsgnIntf
{$ELSE}
  DesignIntf, DesignEditors
{$ENDIF};

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
