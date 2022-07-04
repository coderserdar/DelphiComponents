unit ASGSQLite3Dsg;

{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Albert Drent
Description:  SQLite Design time component class
              For a description of changes, bugfixes, enhancements etc.,
              look into the ASGSQLite3.pas file.  
Creation:     November 2003
Version:      2005.02.A
EMail:        a.drent@aducom.com (sqlite.aducom.com)
Support:      support@aducom.com (sqlite.aducom.com)
              for questions, remarks etc. please register on the forum on
              www.aducom.com/sqlite  
Legal issues: Copyright (C) 2003..2006 by Aducom Software

              Aducom Software
              Eckhartstr 61
              9746 BN  Groningen
              Netherlands

              Open Source licence (BSD: http://www.opensource.org/licenses/bsd-license.php)

              Copyright (c) 2006, Aducom Software
              All rights reserved.

              Redistribution and use in source and binary forms, with or without modification,
              are permitted provided that the following conditions are met:

              Redistributions of source code must retain the above copyright notice,
              this list of conditions and the following disclaimer.
              Redistributions in binary form must reproduce the above copyright notice,
              this list of conditions and the following disclaimer in the documentation
              and/or other materials provided with the distribution.
              Neither the name of Aducom Software nor the names of its contributors
              may be used to endorse or promote products derived from this software
              without specific prior written permission.
              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
              TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
              COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
              INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
              DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
              GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
              HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
              STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
              IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
              POSSIBILITY OF SUCH DAMAGE.

Acknowledgement
              These components were written for our own needs. Since SQLite is
              a freeware component we like to donate this one to the community
              too. Parts of the code is adapted from several sources, but mainly
              from the sample of Borland itself. And, of course, we did a lot
              and still are...
To Do
              A lot...
              We are very busy, but will develop on our needs. If anyone can
              contribute, please feel welcome. Alter the source with lots of comment
              and mail it to me. If it works right I will add it to the official
              source and add your credit here below. Before you start, please
              put a request on the forum. It would be a shame if you develop something
              which already is...
*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

interface

uses
{$I asqlite_def.inc}
{$IFDEF ASQLITE_D6PLUS}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}

 FileCtrl,Classes, Controls, AMDSqlite3;

type

  TASQLiteEditor = Class( TComponentEditor )
    Function GetVerbCount: integer; Override;
    Function GetVerb( Index: integer ): String; Override;
    Procedure ExecuteVerb( Index: integer ); Override;
  End;

  TDatabasePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteTable }
  TASQLite3DatabaseProperty = class(TDatabasePropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TTablePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteTableNames }
  TASQLite3TableNameProperty = class(TTablePropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TMasterDetailPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteMasterDetail fields }
  TASQLite3MasterDetailProperty = class(TMasterDetailPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TStoragePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteStorage fields }
  TASQLite3StorageProperty = class(TStoragePropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TSyncPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteStorage fields }
  TASQLite3SyncProperty = class(TSyncPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TDirPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteStorage fields }
  TASQLite3DirProperty = class(TDirPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TDLLDirPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for DLL directory}
  TASQLite3DLLDirProperty = class(TDLLDirPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TFileTypePropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteOutputtype fields }
  TASQLite3FileTypeProperty = class(TFileTypePropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TTransactionPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteOutputtype fields }
  TASQLite3TransactionProperty = class(TTransactionPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;

  TCharEncPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes      : TPropertyAttributes; override;
    function GetValue           : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TASQLiteOutputtype fields }
  TASQLite3CharEncProperty = class(TCharEncPropertyEditor)
//    procedure GetValueList(Values: TStringList);
  end;
procedure Register;

implementation

uses ASGSQLite3, SysUtils, Forms, Dialogs;

// *************************************************************************************
// ** GetVerbCount
// *************************************************************************************

Function TASQLiteEditor.GetVerbCount: integer;
Begin
     Result := 1;
End;

// *************************************************************************************
// ** GetVerb
// *************************************************************************************

Function TASQLiteEditor.GetVerb( Index: integer ): String;
Begin
     Case Index Of
          0: Result := 'Aducom Software';
     End;
End;

// *************************************************************************************
// ** ExecuteVerb
// *************************************************************************************

Procedure TASQLiteEditor.ExecuteVerb( Index: integer );
Begin
     Case Index Of
          0: ;//ShowAboutBox( 'Max''s WebUpdate Component' );
     End;
End;

function TDatabasePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

procedure TDatabasePropertyEditor.GetValues(Proc: TGetStrProc);
var
  sr: TSearchRec;
begin
  with GetComponent(0) as TASQLite3DB do begin
    if DefaultExt = '' then DefaultExt := '.sqb';
    if DefaultExt[1]<> '.' then DefaultExt := '.'+DefaultExt;
    if DefaultDir <> '' then
       if DefaultDir[Length(DefaultDir)]<>'\' then
          DefaultDir := DefaultDir + '\';
    if FindFirst(DefaultDir+'*'+DefaultExt,faAnyFile, sr) = 0 then begin
       repeat
         Proc(sr.Name);
       until FindNext(sr) <> 0;
       FindClose(sr);
    end;
  end;
  Proc(':memory:');
end;

function TDatabasePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TDatabasePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

//procedure TASQLiteTableNameProperty.GetValueList(Values: TStringList);
//begin
//end;

function TTablePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

procedure TTablePropertyEditor.GetValues(Proc: TGetStrProc);
var MyList : TStringList;
    i      : integer;
begin
 with GetComponent(0) as TASQLite3Table do begin
  if Connection = nil then begin                         // check to see if a valid database
    raise AsgError.Create('no database connection');     // object is linked
    exit;
  end;
  MyList := TStringList.Create;
  Connection.GetTableNames(MyList, true);
  if MyList.Count > 0 then                                                      // marc 20040222
     for i := 0 to MyList.Count - 1 do Proc(MyList[i]);
  MyList.Free;
 end;
end;

function TTablePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TTablePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TMasterDetailPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TMasterDetailPropertyEditor.GetValues(Proc: TGetStrProc);
var
    i,p    : integer;
    r      : string;
    oldr   : string;
begin
 with GetComponent(0) as TASQLite3BaseQuery do begin
   if not Assigned(MasterSource) then begin
      Proc('');
      exit;
   end;
   if not Assigned(MasterSource.Dataset) then begin
      Proc('');
      exit;
   end;

   FMD := TFMD.Create(Application);

   if not Active then open;
   if not Active then exit;

   for i := 0 to FieldDefs.Count - 1 do
       FMD.LBDetail.Items.Add(FieldDefs.Items[i].Name);

   if not Mastersource.Dataset.Active then Mastersource.DataSet.Open;
   if not Mastersource.Dataset.Active then exit;

   for i := 0 to MasterSource.DataSet.FieldDefs.Count - 1 do
       FMD.LBMaster.Items.Add(MasterSource.DataSet.FieldDefs.Items[i].Name);

   r := MasterFields;
   Oldr := r;
   while r <> '' do begin
    p := pos(';', r);
    if p = 0 then begin
       if Trim(r) <> '' then FMD.LBLinked.Items.Add(Trim(r));
       r := '';
    end else begin
       FMD.LBLinked.Items.Add(Trim(Copy(r, 1, p-1)));
       System.Delete(r, 1, p);
    end;
   end;
   if FMD.ShowModal = mrOk then begin
      for i := 0 to FMD.LBLinked.Items.Count - 1 do begin
          r := r + FMD.LBLinked.Items[i]+';';
      end;
   end else r := oldr;
   FMD.Free;
 end;
 Proc(r);
end;

function TMasterDetailPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TMasterDetailPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TStoragePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TStoragePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('DEFAULT');
  Proc('MEMORY');
  Proc('FILE');
end;

function TStoragePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TStoragePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TSyncPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TSyncPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('FULL');
  Proc('NORMAL');
  Proc('OFF');
end;

function TSyncPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TSyncPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TDirPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TDirPropertyEditor.GetValues(Proc: TGetStrProc);
var Dir : string;
begin
  Dir := 'C:\';
  SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],0);
  Proc(Dir);
end;

function TDirPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TDirPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TDLLDirPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TDLLDirPropertyEditor.GetValues(Proc: TGetStrProc);
var Dir : string;
//    Drv : string;
begin
  Dir := 'C:\';
  SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],0);
//  ExtractFileDrive(Dir);
//  if Drv=''
  if Dir[Length(Dir)]<>'\' then Dir := Dir +'\';
  Proc(Dir+'sqlite3.dll');
end;

function TDLLDirPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TDLLDirPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TFileTypePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TFileTypePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('htmlfile');
  Proc('xmlfile');
  Proc('textfile');
end;

function TFileTypePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TFileTypePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TTransactionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TTransactionPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('DEFAULT');
  Proc('DEFERRED');
  Proc('IMMEDIATE');
  Proc('EXCLUSIVE');
end;

function TTransactionPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TTransactionPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

function TCharEncPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TCharEncPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('STANDARD');
  Proc('UTF8');
//  Proc('OFF');
end;

function TCharEncPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TCharEncPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ This procedure is used to register this component on the component palette }
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TASQLite3DB, 'Database',
                         TASQLite3DatabaseProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3DB, 'DefaultDir',
                         TASQLite3DirProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3DB, 'DriverDLL',
                         TASQLite3DLLDirProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Table, 'TableName',
                         TASQLite3TableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3BaseQuery, 'MasterFields',
                         TASQLite3MasterDetailProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Pragma, 'TempStore',
                         TASQLite3StorageProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Pragma, 'DefaultTempStore',
                         TASQLite3StorageProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Pragma, 'Synchronous',
                         TASQLite3SyncProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Pragma, 'DefaultSynchronous',
                         TASQLite3SyncProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3Output, 'OutputType',
                         TASQLite3FileTypeProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3BaseQuery, 'TransactionType',
                         TASQLite3TransactionProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3DB, 'TransactionType',
                         TASQLite3TransactionProperty);
  RegisterPropertyEditor(TypeInfo(string), TASQLite3DB, 'CharacterEncoding',
                         TASQLite3CharEncProperty);

  RegisterComponents('Aducom SQLite3', [TASQLite3DB]);
  RegisterComponents('Aducom SQLite3', [TASQLite3Query]);
  RegisterComponents('Aducom SQLite3', [TASQLite3Table]);
  RegisterComponents('Aducom SQLite3', [TASQLite3UpdateSQL]);
  RegisterComponents('Aducom SQLite3', [TASQLite3Pragma]);
  RegisterComponents('Aducom SQLite3', [TASQLite3Log]);
  RegisterComponents('Aducom SQLite3', [TASQLite3InlineSQL]);
  RegisterComponents('Aducom SQLite3', [TASQLite3Output]);

  RegisterComponentEditor(TASQLite3DB, TASQLiteEditor);
end;

end.
