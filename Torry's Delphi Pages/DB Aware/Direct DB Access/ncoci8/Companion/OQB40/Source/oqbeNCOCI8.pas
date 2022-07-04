{*******************************************************}
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

unit oqbeNCOCI8;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, QBuilder, NCOci, NCOciWrapper, NCOciDB;

type
  TOQBEngineNCOCI8 = class(TOQBEngine)
  private
    FDatabase: TOCICustomDatabase;
    FResultQuery: TOCIQuery;
    FShowViews: Boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabaseName(const Value: string); override;
    function  SelectDatabase: boolean; override;
    procedure ReadTableList; override;
    procedure ReadFieldList(ATableName: string); override;
    procedure ClearQuerySQL; override;
    procedure SetQuerySQL(Value: string); override;
    function  ResultQuery: TDataSet; override;
    procedure OpenResultQuery; override;
    procedure CloseResultQuery; override;
    procedure SaveResultQueryData; override;
  published
    property UserName;
    property Password;
    property ShowSystemTables default False;
    property SQLDialect default sdOracle;
    property ShowViews: Boolean read FShowViews write FShowViews default False;
  end;

implementation

uses QBDBFrmNCOCI8;

{ TOQBEngineNCOCI8 }

constructor TOQBEngineNCOCI8.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FResultQuery := TOCIQuery.Create(Self);
    ShowSystemTables := False;
    SQLDialect := sdOracle;
end;

destructor TOQBEngineNCOCI8.Destroy;
begin
    FResultQuery.Free;
    inherited Destroy;
end;

procedure TOQBEngineNCOCI8.SetDatabaseName(const Value: string);
begin
    if FDatabase <> nil then begin
        FDatabase.Connected := False;
        FDatabase := nil;
    end;
    inherited SetDatabaseName(Value);
    FResultQuery.DatabaseName := Value;
end;

procedure TOQBEngineNCOCI8.Loaded;
begin
    FDatabase := TOCIDatabase.DatabaseByName(DatabaseName, nil);
    inherited Loaded;
end;

function TOQBEngineNCOCI8.SelectDatabase: boolean;
begin
    Result := (FDatabase <> nil) and FDatabase.InteractiveOpen;
    if Result then begin
        with TOQBDBFormNCOCI8.Create(nil) do
        try
            CheckDB.Checked := ShowSystemTables;
            CheckView.Checked := ShowViews;
            if ShowModal = mrOK then begin
                ShowSystemTables := CheckDB.Checked;
                ShowViews := CheckView.Checked;
            end;
        finally
            Free;
        end;
    end;
end;

procedure TOQBEngineNCOCI8.ReadTableList;
begin
    try
        TOCIDatabase.GetObjectsList(DatabaseName, TableList, '',
            okSelectable, ShowSystemTables); // ??? ShowViews
    except
    end;
end;

procedure TOQBEngineNCOCI8.ReadFieldList(ATableName: string);
begin
    try
        TOCIDatabase.GetObjectsList(DatabaseName, FieldList, ATableName,
            okFields, ShowSystemTables);
        FieldList.Insert(0,'*');
    except
    end;
end;

procedure TOQBEngineNCOCI8.ClearQuerySQL;
begin
    FResultQuery.SQL.Clear;
end;

procedure TOQBEngineNCOCI8.SetQuerySQL(Value: string);
begin
    FResultQuery.SQL.Text := Value;
end;

function TOQBEngineNCOCI8.ResultQuery: TDataSet;
begin
    Result := FResultQuery;
end;

procedure TOQBEngineNCOCI8.OpenResultQuery;
begin
    FResultQuery.Active := True;
end;

procedure TOQBEngineNCOCI8.CloseResultQuery;
begin
    FResultQuery.Active := False;
end;

procedure TOQBEngineNCOCI8.SaveResultQueryData;
begin
    ShowMessage('Operation is not supported.');
end;

end.

