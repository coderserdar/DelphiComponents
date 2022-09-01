
{*******************************************************}
{                                                       }
{           TDBLookupComboPlus Component                }
{                                                       }
{  Copyright (c) 1995,96,97 Out & About Production      }
{   Portions Copyright (c) 1995,96,97,98 Inprise Inc    }
{                                                       }
{                  Version 6.0                          }
{                                                       }
{                                                       }
{*******************************************************}

{$IFDEF VER100}
  {$DEFINE D3OR4}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE D3OR4}
{$ENDIF}

unit Dbluplus;

interface

uses Classes, DsgnIntf, DBLup2, Dialogs;

{$IFDEF Win32}
  {$R Dblupl32.DCR}
{$ELSE}
  {$R Dblupl16.DCR}
{$ENDIF}


procedure Register;

implementation

uses DBTables {$IFDEF D3OR4}, dbclient {$ENDIF};

{ TIndexNameProperty }
type
  TIndexNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TIndexNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TIndexNameProperty.GetValueList(List: TStrings);
begin
  if (GetComponent(0) as TDBLookupComboPlus).LookUpSource <> nil then
  begin
    If (GetComponent(0) as TDBLookupComboPlus).LookupSource.DataSet.InheritsFrom(TTable) then
      ((GetComponent(0) as TDBLookupComboPlus).LookUpSource.DataSet as TTable).GetIndexNames(List);
{$IFDEF D3OR4}
    If (GetComponent(0) as TDBLookupComboPlus).LookupSource.DataSet.InheritsFrom(TClientDataSet) then
      ((GetComponent(0) as TDBLookupComboPlus).LookUpSource.DataSet as TClientDataSet).GetIndexNames(List);
{$ENDIF}
  end;
end;

procedure TIndexNameProperty.GetValues(Proc: TGetStrProc);
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

{TDBLookupComponentEditor}
type
  TDBLookupComponentEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


procedure TDBLookupComponentEditor.ExecuteVerb(Index: Integer);
begin
  MessageDlg('TDBLookupComboPlus'
             +#10#13+ '(c) 1995, 1996, 1997 Out && About Productions'
             +#10#13+ '75664.1224@compuserve.com'
             +#10#13+ 'Fax 619.566.0210' ,mtInformation,[mbOK],0);
end;

function TDBLookupComponentEditor.GetVerb(Index: Integer): string;
begin
  result := 'Component Info';
end;

function TDBLookupComponentEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBLookupComboPlus]);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupComboPlus, 'LookupIndex', TIndexNameProperty);
  RegisterComponentEditor(TDBLookupComboPlus,TDBLookupComponentEditor);
end;

end.
