{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is xqreg.pas                                         }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

unit xqreg;

{$I XQ_FLAG.INC}
interface
{Thanks to : Steve Garland who created most of this unit
 Date: Jun 30, 2003}

{$R xqresour.dcr}

procedure Register;

implementation

uses
   Classes, xQuery, Db, SysUtils, SyntaxHi, xQBase
{$IFDEF LEVEL6}
   , DesignIntf, DesignEditors, Variants
{$ELSE}
   , DsgnIntf
{$ENDIF}
   ;

type
  TDataSetsPropEditor = class(TComponentProperty)
  private
    FProc : TGetStrProc;
    procedure getProc(const s : string);
  public
    procedure GetValues(Proc : TGetStrProc); override;
  end;

  TParamPropEditor = class(TComponentProperty)
  private
    FProc : TGetStrProc;
    procedure getProc(const s : string);
  public
    procedure GetValues(Proc : TGetStrProc); override;
  end;

procedure Register;
begin
   RegisterComponents('XQuery', [TxQuery, TSyntaxHighlighter]);
   RegisterPropertyEditor(TypeInfo(TComponent), TxDataSetItem,
      'DataSets', TDataSetsPropEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), TParam,
      'Params', TParamPropEditor);
end;

{ TDataSetsPropEditor - class implementation }
procedure TDataSetsPropEditor.getProc(const s : string);
var
  i    : integer;
  item : TxDataSetItem;
begin
  item := TxDataSetItem(GetComponent(0));
  with item, Collection do
    for i := 0 to Count - 1 do
      if Assigned(TxDataSetItem(Items[i]).DataSet) and
         (CompareText(s, TxDataSetItem(Items[i]).DataSet.Name) = 0) then
        exit;

  FProc(s);
end;

procedure TDataSetsPropEditor.GetValues(Proc : TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(getProc);
end;

{TParamPropEditor - class implementation}

procedure TParamPropEditor.getProc(const s : string);
var
  i    : integer;
  item : TParam;
begin
  item := TParam(GetComponent(0));
  with item, Collection do
    for i := 0 to Count - 1 do
      if CompareText(s, TParam(Items[i]).Name) = 0 then
        exit;

  FProc(s);
end;

procedure TParamPropEditor.GetValues(Proc : TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(getProc);
end;

end.
