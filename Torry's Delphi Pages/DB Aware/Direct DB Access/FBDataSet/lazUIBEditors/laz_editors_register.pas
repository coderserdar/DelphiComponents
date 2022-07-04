unit laz_editors_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, componenteditors, typinfo, jvuib;

type

  { TUIBDatabaseEditor }

  TUIBDatabaseEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TUIBTransactionEditor }

  TUIBTransactionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  
procedure Register;
implementation
uses jvuibdatabaseedit, Forms, LCLType, Controls, jvuibtransactionedit;

procedure Register;
begin
  RegisterComponentEditor(TJvUIBDataBase, TUIBDatabaseEditor);
  RegisterComponentEditor(TJvUIBTransaction, TUIBTransactionEditor);
end;

{ TUIBDatabaseEditor }

procedure TUIBDatabaseEditor.ExecuteVerb(Index: Integer);
begin
  with TUIBDatabaseEditForm.Create(Application) do
  try
    Database := TJvUIBDataBase(Component);
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;

function TUIBDatabaseEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Database Editor ...';
end;

function TUIBDatabaseEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TUIBTransactionEditor }

procedure TUIBTransactionEditor.ExecuteVerb(Index: Integer);
begin
  with TUIBTransactionEditForm.Create(Application) do
  try
    Transaction := TJvUIBTransaction(Component);
    if ShowModal = mrOk then
      inherited Designer.Modified;
  finally
    Free;
  end;
end;

function TUIBTransactionEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Transaction Editor ...';
end;

function TUIBTransactionEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

end.

