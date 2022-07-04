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
{*******************************************************}
{File:      oqbeNCOCI8.PAS                              }
{Revision:  0.01.00 / 10.05.2000                        }
{Comment:   NC OCI8 VCL: OQB engine for NCOCI8          }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit oqbeRegNCOCI8;

interface

uses
  Classes, DB, NCOci, NCOciWrapper, NCOciDB, OQBENCOCI8;

procedure Register;

implementation

uses
{$IFDEF VER140}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;
  
{ TOQBDatabaseNamePropertyNCOCI8 }

type
  TOQBDatabaseNamePropertyNCOCI8 = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TOQBDatabaseNamePropertyNCOCI8.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TOQBDatabaseNamePropertyNCOCI8.GetValues(Proc: TGetStrProc);
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

procedure TOQBDatabaseNamePropertyNCOCI8.GetValueList(List: TStrings);
begin
  TOCIDatabase.GetObjectsList('', List, '', okService, False);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TOQBEngineNCOCI8, 'DatabaseName', TOQBDatabaseNamePropertyNCOCI8);
  RegisterComponents('OQBuilder', [TOQBEngineNCOCI8]);
end;

end.
