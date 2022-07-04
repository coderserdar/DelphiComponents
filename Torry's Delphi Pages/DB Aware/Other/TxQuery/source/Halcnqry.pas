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
{   The Original Code is HalcnQry.pas                                      }
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

Unit HalcnQry;

{-------------------------------------------------------------------------------}
{   (C) 2003 Alfonso moreno                                                     }
{   THalcyonxQuery class implementation                                         }
{   You need Halcyon 6 in order to use this unit                                }
{-------------------------------------------------------------------------------}

{$I XQ_FLAG.INC}
Interface
//{$R HalcnQry.dcr}

Uses
  SysUtils, Windows, Messages, classes, Graphics, Controls, forms, Dialogs,
  StdCtrls, QBAseExpr, xqmiscel, xquery, halcn6DB, gs6_shel, Db, IniFiles,
  xqbase;

Type
  {-------------------------------------------------------------------------------}
  {                  forward declarations                                         }
  {-------------------------------------------------------------------------------}
  TDataList = Class;
  THalcyonxQuery = Class;

  {-------------------------------------------------------------------------------}
  {                  Defines TDataItem                                            }
  {-------------------------------------------------------------------------------}

  TDataItem = Class
  Private
    FDataList: TDataList; { belongs to                                          }
    FDataSet: TDataSet; { the THalcyonDataset                                 }
    FFileName: String; { the original filename c:\mydb\file1.dbf             }
    FAlias: String; { the alias assigned (to be passed to THalcyonxQuery) }
    FIndexFiles: TStringList; { The list of index files to use in FDataSet          }
  Public
    Constructor Create( DataList: TDataList );
    Destructor Destroy; Override;
    Procedure Open;

    Property FileName: String Read FFileName Write FFileName;
    Property Alias: String Read FAlias Write FAlias;
    Property DataSet: TDataSet Read FDataSet Write FDataSet;
    Property IndexFiles: TStringList Read FIndexFiles Write FIndexFiles;
  End;

  {-------------------------------------------------------------------------------}
  {                  Defines TDataList                                            }
  {-------------------------------------------------------------------------------}

  TDataList = Class
  Private
    FItems: TList;
    FUseDeleted: Boolean;
    FConfigFileName: String;
    FInMemResultSet: Boolean;
    FMapFileSize: Longint;
    FDateFormat: String;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TDataItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add( Const pFileName, pAlias: String; pIndexFiles: TStringList ): TDataItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Function IndexOf( Const S: String ): Integer;
    Procedure LoadFromFile( Const ConfigFileName: String );
    Procedure SaveToFile( Const ConfigFileName: String );
    Procedure OpenDataSets;
    Procedure CloseDataSets;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TDataItem Read GetItem; Default;
    Property UseDeleted: Boolean Read FUseDeleted Write FUseDeleted;
    Property ConfigFileName: String Read FConfigFileName Write FConfigFileName;
    Property InMemResultSet: Boolean Read FInMemResultSet Write FInMemResultSet;
    Property MapFileSize: Longint Read FMapFileSize Write FMapFileSize;
    Property DateFormat: String Read FDateFormat Write FDateFormat;
  End;

  {-------------------------------------------------------------------------------}
  {                  Defines THalcyonxQuery                                       }
  {-------------------------------------------------------------------------------}

  THalcyonxQuery = Class( TCustomxQuery )
  Private
    FType: gsDBFTypes;
    FAutoOver: boolean;
    FUseDeleted: Boolean;
    FSaveUseDeleted: TBits;
    { this is only a reference to a global object and must not be created }
    FDataList: TDataList;
    // for temporary saving events
    FSaveIndexNeededFor: TIndexNeededForEvent;
    FSaveSetRange: TSetRangeEvent;
    FSaveCancelRange: TCancelRangeEvent;
    FSaveSetFilter: TSetFilterEvent;
    FSaveCancelFilter: TCancelFilterEvent;
    Procedure IndexNeededfor( Sender: TObject;
      DataSet: TDataset;
      Const FieldNames: String;
      ActivateIndex: Boolean;
      IsJoining: Boolean; Var Accept: Boolean );
    Procedure SetRange( Sender: TObject;
      RelOperator: TRelationalOperator;
      DataSet: TDataset;
      Const FieldNames, StartValues, EndValues: String;
      IsJoining: Boolean );
    Procedure CancelRange( Sender: TObject;
      DataSet: TDataset;
      IsJoining: Boolean );
  Protected
    Procedure FixDummiesForFilter( Var Filter: String ); Override;

    Procedure CreateTable( Sender: TObject; CreateTable: TCreateTableItem );
    Procedure CreateIndex( Sender: TObject; Unique, Descending: Boolean;
      Const TableName, IndexName: String; ColumnExprList: TStringList );
    Procedure DropTable( Sender: TObject; Const TableName: String );
    Procedure DropIndex( Sender: TObject; Const TableName, IndexName: String );
    Procedure BeforeQuery( Sender: TObject );
    Procedure AfterQuery( Sender: TObject );
    Procedure SetDataList( Value: TDataList );
    Procedure SetFilter( Sender: TObject; DataSet: TDataset; Const Filter: String;
      IsJoining: Boolean; Var Handled: Boolean );
    Procedure CancelFilter( Sender: TObject; DataSet: TDataset; IsJoining: Boolean );

  Public
    Constructor Create( AOwner: TComponent ); Override;
    Destructor Destroy; Override;

    Procedure SaveToDBF( Const FileName: String );
    Procedure Loaded; Override;

    Property DataList: TDataList Read FDataList Write SetDataList;
  Published
    { properties }
    Property DBFType: gsDBFTypes Read FType Write FType;
    Property AutoOverwrite: Boolean Read FAutoOver Write FAutoOver;

    Property UseDeleted: Boolean Read FUseDeleted Write FUseDeleted;

    { inherited properties }
    Property DataSets;
  End;

Procedure Register;

Implementation

Uses
  xqyacc;

Procedure Register;
Begin
  RegisterComponents( 'Halcyon6', [THalcyonxQuery] );
End;

Resourcestring
  hqErrOverwriteTable = 'Table exists.  Do you want to overwrite?';
  xqTablenameNotFound = 'Table name does not exists.';

Const
  IDXExtns: Array[0..3] Of String[4] = ( '.NTX', '.NDX', '.MDX', '.CDX' );

  {-------------------------------------------------------------------------------}
  {                  Implementes TDataItem                                        }
  {-------------------------------------------------------------------------------}

Constructor TDataItem.Create( DataList: TDataList );
Begin
  Inherited Create;
  FDataList := DataList;
  FIndexFiles := TStringList.Create;
  { the dataset belong to the DataList }
  FDataSet := THalcyonDataSet.Create( Nil );
End;

Destructor TDataItem.Destroy;
Begin
  FDataSet.Free;
  FIndexFiles.Free;
  Inherited Destroy;
End;

Procedure TDataItem.Open;
Begin
  FDataSet.Close;
  With ( FDataSet As THalcyonDataSet ) Do
  Begin
    DatabaseName := ExtractFilePath( FFileName );
    Tablename := ExtractFileName( FFileName );
    IndexFiles.Assign( Self.FIndexFiles );
    UseDeleted := FDataList.UseDeleted;
    Open;
  End;
End;

{-------------------------------------------------------------------------------}
{                  Implement TDataList                                          }
{-------------------------------------------------------------------------------}

Constructor TDataList.Create;
Begin
  Inherited Create;
  FItems := TList.Create;
End;

Destructor TDataList.Destroy;
Begin
  Clear;
  FItems.Free;
  Inherited Destroy;
End;

Function TDataList.IndexOf( Const S: String ): Integer;
Var
  I: Integer;
Begin
  result := -1;
  For I := 0 To FItems.Count - 1 Do
  Begin
    If AnsiCompareText( Items[I].FileName, S ) = 0 Then
    Begin
      Result := I;
      Exit;
    End;
  End;
End;

Function TDataList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TDataList.GetItem( Index: Integer ): TDataItem;
Begin
  Result := FItems[Index];
End;

Function TDataList.Add( Const pFileName, pAlias: String; pIndexFiles: TStringList ): TDataItem;
Var
  I: Integer;
Begin
  Result := TDataItem.Create( Self );
  Try
    With TDataItem( Result ) Do
    Begin
      DataSet.Close;
      With THalcyonDataSet( DataSet ) Do
      Begin
        DatabaseName := ExtractFilePath( pFileName );
        TableName := ExtractFileName( pFileName );
        IndexFiles.Clear;
        For I := 0 To pIndexFiles.Count - 1 Do
          IndexFiles.Add( ExtractFileName( pIndexFiles[I] ) );
      End;
      IndexFiles.Assign( pIndexFiles );
      If Length( pAlias ) > 0 Then
        Alias := pAlias
      Else
        Alias := ChangeFileExt( ExtractFileName( pFileName ), '' );
      FileName := pFileName;
    End;
  Except
    Result.Free;
    Raise;
  End;
  FItems.Add( Result );
End;

Procedure TDataList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TDataItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TDataList.Delete( Index: Integer );
Begin
  TDataItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

Procedure TDataList.LoadFromFile( Const ConfigFileName: String );
Var
  IniFile: TIniFile;
  NumFiles: Integer;
  NumIndexes: Integer;
  I: Integer;
  J: Integer;
  IndexFiles: TStringList;
  FileName: String;
  Alias: String;
Begin
  Clear;
  IniFile := TIniFile.Create( ConfigFileName );
  IndexFiles := TStringList.Create;
  Try
    { this is the configuration for the file :
    [General]
    NumFiles=3
    File1=C:\MyDatabase\File1.Dbf
    File2=C:\MyDatabase\File2.Dbf
    File3=C:\MyDatabase\File3.Dbf
    Alias1=Customer
    Alias2=Orders
    Alias3=Items
    ...
    UseDeleted=1 or 0
    FInMemResultSet : Boolean;
    FMapFileSize    : Longint;
    FDateFormat     : String;
    FUseDeleted     : Boolean;

    [File1]
    NumIndexes=1
    Index1=File1.Cdx
    }
    NumFiles := IniFile.ReadInteger( 'General', 'NumFiles', 0 );
    FUseDeleted := IniFile.ReadBool( 'General', 'UseDeleted', False );
    FInMemResultSet := IniFile.ReadBool( 'General', 'InMemResultSet', True );
    FMapFileSize := IniFile.ReadInteger( 'General', 'MapFileSize', 2000000 );
    FDateFormat := IniFile.ReadString( 'General', 'DateFormat', 'm/d/yyyy' );

    For I := 1 To NumFiles Do
    Begin
      IndexFiles.Clear;
      NumIndexes := IniFile.ReadInteger( 'File' + IntToStr( I ), 'NumIndexes', 0 );
      For J := 1 To NumIndexes Do
      Begin
        FileName := IniFile.ReadString( 'File' + IntToStr( I ), 'Index' + IntToStr( J ), '' );
        If Length( FileName ) > 0 Then
          IndexFiles.Add( FileName );
      End;
      FileName := IniFile.ReadString( 'General', 'File' + IntToStr( I ), '' );
      If Not FileExists( FileName ) Then
        Continue;
      Alias := IniFile.ReadString( 'General', 'Alias' + IntToStr( I ), '' );
      Add( FileName, Alias, IndexFiles );
    End;
  Finally
    IniFile.Free;
    IndexFiles.Free;
  End;
  FConfigFileName := ConfigFileName;
End;

Procedure TDataList.SaveToFile( Const ConfigFileName: String );
Var
  IniFile: TIniFile;
  NumFiles, NumIndexes, I, J: Integer;
  IndexFiles: TStringList;
  FileName, Alias: String;
  Item: TDataItem;
Begin
  IniFile := TIniFile.Create( ConfigFileName );
  IndexFiles := TStringList.Create;
  Try
    NumFiles := FItems.Count;
    IniFile.WriteInteger( 'General', 'NumFiles', NumFiles );
    IniFile.WriteBool( 'General', 'UseDeleted', FUseDeleted );
    IniFile.WriteBool( 'General', 'InMemResultSet', FInMemResultSet );
    IniFile.WriteInteger( 'General', 'MapFileSize', FMapFileSize );
    IniFile.WriteString( 'General', 'DateFormat', FDateFormat );
    For I := 0 To NumFiles - 1 Do
    Begin
      Item := Items[I];
      IndexFiles.Assign( THalcyonDataSet( Item.DataSet ).IndexFiles );
      NumIndexes := IndexFiles.Count;
      IniFile.WriteInteger( 'File' + IntToStr( I + 1 ), 'NumIndexes', NumIndexes );
      For J := 0 To IndexFiles.Count - 1 Do
      Begin
        FileName := IndexFiles[J];
        IniFile.writeString( 'File' + IntToStr( I + 1 ), 'Index' + IntToStr( J + 1 ), FileName );
      End;
      FileName := Item.FileName;
      IniFile.writeString( 'General', 'File' + IntToStr( I + 1 ), FileName );
      Alias := Item.Alias;
      IniFile.writeString( 'General', 'Alias' + IntToStr( I + 1 ), Alias );
    End;
  Finally
    IniFile.Free;
    IndexFiles.Free;
  End;
End;

Procedure TDataList.OpenDataSets;
Var
  I: Integer;
Begin
  Screen.Cursor := crHourglass;
  Try
    For I := 0 To FItems.Count - 1 Do
      TDataItem( FItems[I] ).Open;
  Finally
    Screen.Cursor := crDefault;
  End;
End;

Procedure TDataList.CloseDataSets;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TDataItem( FItems[I] ).DataSet.Close;
End;

{-------------------------------------------------------------------------------}
{                  Implementes THalcyonxQuery                                   }
{-------------------------------------------------------------------------------}

Constructor THalcyonxQuery.Create( AOwner: TComponent );
Begin
  Inherited Create( AOwner );
  //DataSets.DataSetClass   := THalcyonDataSet;
  AllSequenced := False;
  FSaveUseDeleted := TBits.Create;
  OnIndexNeededFor := IndexNeededFor;
  OnSetRange := SetRange;
  OnCancelRange := CancelRange;
End;

Destructor THalcyonxQuery.Destroy;
Begin
  FSaveUseDeleted.Free;
  Inherited Destroy;
End;

Procedure THalcyonxQuery.Loaded;
Begin
  Inherited Loaded;
  OnIndexNeededfor := IndexNeededfor;
  OnSetRange := SetRange;
  OnCancelRange := CancelRange;
  OnCreateTable := CreateTable;
  OnCreateIndex := CreateIndex;
  OnDropTable := DropTable;
  OnDropIndex := DropIndex;
  OnBeforeQuery := BeforeQuery;
  OnAfterQuery := AfterQuery;
  OnSetFilter := SetFilter;
  OnCancelFilter := CancelFilter;
End;

Procedure THalcyonxQuery.IndexNeededfor( Sender: TObject;
  DataSet: TDataset;
  Const FieldNames: String;
  ActivateIndex: Boolean;
  IsJoining: Boolean; Var Accept: Boolean );
Var
  i: integer;
  fNames: String;
Begin
  If IsJoining Then
    Exit;
  Accept := False;
  If WhereOptimizeMethod <> omSetFilter Then
    exit;

  fNames := UpperCase( FieldNames );
  { warning: only simple index expressions accepted:
    FIRSTNAME+LASTNAME}
  ReplaceString( fNames, ';', '+' );
  With THalcyonDataset( DataSet ) Do
  Begin
    For i := 1 To IndexCount Do
      If AnsiCompareText( fNames, UpperCase( IndexExpression( i ) ) ) = 0 Then
      Begin
        Accept := True;
        If ActivateIndex Then
          SetTagTo( IndexTagName( i ) );
      End;
  End;

End;

Procedure THalcyonxQuery.SetRange( Sender: TObject;
  RelOperator: TRelationalOperator;
  DataSet: TDataset;
  Const FieldNames, StartValues, EndValues: String;
  IsJoining: Boolean );

// DecStr - Decrements a string value (ex. 'Hello' -> 'Helln')-------------//
  Function DecStr( sStr: String ): String;
  Var
    iLen: Integer;
  Begin
    iLen := Length( sStr );
    If iLen > 0 Then
    Begin
      If sStr[iLen] > #0 Then
        sStr := Copy( sStr, 1, iLen - 1 ) + Char( Ord( sStr[iLen] ) - 1 ) // Minus one
      Else
        sStr := Copy( sStr, 1, iLen - 1 ); // Chop off last Char
    End;
    Result := sStr;
  End; //--------------------------------------------------------------------//

  // IncStr - Increments a string value (ex. 'Hello' -> 'Hellp')-------------//
  Function IncStr( sStr: String ): String;
  Var
    iLen: Integer;
  Begin
    iLen := Length( sStr );
    If iLen > 0 Then
    Begin
      If sStr[iLen] < #255 Then
        sStr := Copy( sStr, 1, iLen - 1 ) + Char( Ord( sStr[iLen] ) + 1 ) // Add one
      Else
        sStr := sStr + #1; // Add one character
    End;
    Result := sStr;
  End; //--------------------------------------------------------------------//

Begin
  With THalcyonDataset( DataSet ) Do
  Begin
    Case RelOperator Of
      ropBETWEEN: SetRange( StartValues, endValues );
      ropGT: SetRange( IncStr( StartValues ), #255 );
      ropGE: SetRange( StartValues, #255 );
      ropLT: SetRange( #0, DecStr( endValues ) );
      ropLE: SetRange( #0, endValues );
      ropNEQ: ; // how?
    End;
  End;
End;

Procedure THalcyonxQuery.CancelRange( Sender: TObject; DataSet: TDataSet;
  IsJoining: Boolean );
Begin
  ( DataSet As THalcyonDataSet ).SetRange( '', '' );
  ( DataSet As THalcyonDataSet ).Filtered := False;
End;

Procedure THalcyonxQuery.CreateTable( Sender: TObject; CreateTable: TCreateTableItem );
Var
  FieldList: TStringList;
  s,
    FileName,
    FieldName,
    IndexFileName: String;
  FieldType: Char;
  I,
    FieldSize,
    FieldDec: Integer;
  Halc: THalcyonDataSet;
  IndexFiles: TStringList;
Begin
  { if not datalist is assigned then I cannot save to a global configuration file }
  If Not Assigned( FDataList ) Then
    Exit;

  FileName := CreateTable.TableName;
  If FileExists( FileName ) And Not FAutoOver Then
  Begin
    If Application.MessageBox( PChar( hqErrOverwriteTable ), 'Warning', MB_OKCANCEL ) = IDCANCEL Then
      Exit;
  End;
  FieldList := TStringList.Create;
  IndexFiles := TStringList.Create;
  Try
    For I := 0 To CreateTable.FieldCount - 1 Do
    Begin
      FieldName := CreateTable.Fields[I].FieldName;
      Case CreateTable.Fields[I].FieldType Of
        // list of possible types accepted in TxQuery parser
        RW_CHAR:
          Begin
            FieldType := 'C';
            FieldSize := CreateTable.Fields[I].Size;
            FieldDec := 0;
          End;
        RW_INTEGER, RW_AUTOINC:
          Begin
            FieldType := 'N';
            FieldSize := 11;
            FieldDec := 0;
          End;
        RW_SMALLINT:
          Begin
            FieldType := 'N';
            FieldSize := 6;
            FieldDec := 0;
          End;
        RW_BOOLEAN:
          Begin
            FieldType := 'L';
            FieldSize := 1;
            FieldDec := 0;
          End;
        RW_DATE, RW_TIME, RW_DATETIME:
          Begin
            FieldType := 'D';
            FieldSize := 10;
            FieldDec := 0;
          End;
        RW_MONEY, RW_FLOAT:
          Begin
            FieldType := 'N';
            If CreateTable.Fields[I].Scale = 0 Then
            Begin
              FieldSize := 20;
              FieldDec := 4;
            End
            Else
            Begin
              FieldSize := CreateTable.Fields[I].Scale;
              FieldDec := CreateTable.Fields[I].Precision;
            End;
          End;
        RW_BLOB:
          Begin
            // use BlobType property here
            Case CreateTable.Fields[I].BlobType Of
              1, 3: // Memo, formatted Memo
                FieldType := 'M';
              2, 4: // Binary, OLE
                FieldType := 'B';
              5: // Graphic/Binary
                FieldType := 'G';
            End;
            FieldSize := 8;
            FieldDec := 0;
          End;
      End;
      FieldList.Add( format( '%s;%s;%d;%d', [FieldName, FieldType, FieldSize, FieldDec] ) );
    End;
    gs6_shel.CreateDBF( FileName, '', FType, FieldList );
    Halc := THalcyonDataSet.Create( Nil );
    Halc.DatabaseName := AddSlash( ExtractFilePath( FileName ) );
    Halc.TableName := ExtractFileName( FileName );
    Try
      Halc.Open;
    Except
      Halc.Free;
      Raise;
    End;
    { add the new created table to the list of datasets
    Self.AddDataSet(Halc, ChangeFileExt(Halc.TableName,''));}
    If CreateTable.PrimaryKey.Count > 0 Then
    Begin
      S := CreateTable.PrimaryKey[0];
      For I := 1 To CreateTable.PrimaryKey.Count - 1 Do
        S := S + '+' + CreateTable.PrimaryKey[I];
      IndexFileName := ChangeFileExt( FileName, IDXExtns[Ord( FType )] );
      Halc.IndexOn( IndexFileName, 'PRIMARY', S, '.NOT.DELETED()', // optional
        Halcn6DB.Unique, Halcn6DB.Ascending );
      IndexFiles.Add( IndexFileName );
    End;
    { add to the list }
    FDataList.Add( FileName, ChangeFileExt( Halc.TableName, '' ), IndexFiles );
    FDataList.SaveToFile( FDataList.ConfigFileName );
  Finally
    FieldList.Free;
    IndexFiles.Free;
  End;
End;

Procedure THalcyonxQuery.CreateIndex( Sender: TObject; Unique, Descending: Boolean;
  Const TableName, IndexName: String; ColumnExprList: TStringList );
Var
  Temps: String;
  j, Index: integer;
  IndexUnique: TgsIndexUnique;
  SortStatus: TgsSortStatus;
  Halc: THalcyonDataSet;
Begin
  If Not Assigned( FDataList ) Then
    Exit;

  Index := FDataList.IndexOf( TableName );
  If Index < 0 Then
    Exit;
  Temps := ColumnExprList[0];
  For j := 1 To ColumnExprList.Count - 1 Do
    Temps := Temps + '+' + ColumnExprList[j];
  If Unique Then
    IndexUnique := Halcn6DB.Unique
  Else
    IndexUnique := Halcn6DB.Duplicates;
  If Descending Then
    SortStatus := Halcn6DB.Descending
  Else
    SortStatus := Halcn6DB.Ascending;
  Halc := FDataList[Index].DataSet As THalcyonDataSet;
  { supposed to add to a primary index .cdx, .mdx }
  Halc.IndexOn( ChangeFileExt( Halc.TableName, IDXExtns[Ord( FType )] ),
    IndexName, Temps, '.NOT.DELETED()', // optional
    IndexUnique, SortStatus );
  FDataList.SaveToFile( FDataList.ConfigFileName );
End;

Procedure THalcyonxQuery.DropTable( Sender: TObject; Const TableName: String );
Var
  Index: integer;
Begin
  If Not Assigned( FDataList ) Then
    Exit;

  Index := FDataList.IndexOf( TableName );
  If Index < 0 Then
    Exit;
  SysUtils.DeleteFile( TableName );
  FDataList.Delete( Index );
End;

Procedure THalcyonxQuery.DropIndex( Sender: TObject; Const TableName, IndexName: String );
Var
  Halc: THalcyonDataSet;
  Index: integer;
Begin
  If Not Assigned( FDataList ) Then
    Exit;

  Index := FDataList.IndexOf( TableName );
  If Index < 0 Then
    Exit;
  Halc := FDataList[Index].DataSet As THalcyonDataSet;
  If Halc.Active Then
    Halc.IndexTagRemove( ChangeFileExt( Halc.TableName, IDXExtns[Ord( FType )] ), IndexName );
End;

Procedure THalcyonxQuery.SaveToDBF( Const FileName: String );
Var
  I: Integer;
  Field, SrcField, DestField: TField;
  FieldList, NewFieldNamesList: TStringList;
  FieldName: String;
  FieldType: Char;
  FieldSize, FieldDec: Integer;
  Halc: THalcyonDataSet;
  bm: TBookmark;

  Function CheckDuplicate( Const fname: String ): String;
  Var
    NumTry: Integer;
    Found: Boolean;
  Begin
    Result := fname;
    NumTry := 0;
    Repeat
      Found := NewFieldNamesList.IndexOf( Result ) >= 0;
      If Found Then
      Begin
        Inc( NumTry );
        Result := Copy( fname, 1, 8 ) + '_' + IntToStr( NumTry );
      End;
    Until Not Found;
  End;

Begin
  If FileExists( FileName ) And Not FAutoOver Then
  Begin
    If Application.MessageBox( PChar( hqErrOverwriteTable ), 'Warning', MB_OKCANCEL ) = IDCANCEL Then
      exit;
  End;
  FieldList := TStringList.Create;
  NewFieldNamesList := TStringList.Create;
  Halc := THalcyonDataSet.Create( Nil );
  Halc.DatabaseName := AddSlash( ExtractFilePath( FileName ) );
  Halc.TableName := ExtractFileName( FileName );
  DisableControls;
  bm := GetBookmark;
  Try
    For I := 0 To Self.FieldCount - 1 Do
    Begin
      Field := Self.Fields[I];
      // warning: SQL statement must have a valid DBF file name in as AS clause
      // this is not valid: SELECT CustNo As VeryLongFieldName FROM Customer
      // due to that DBF only accepts field names of 10 chars max length
      FieldName := Field.FieldName;
      ReplaceString( FieldName, ' ', '' );
      FieldName := CheckDuplicate( Copy( FieldName, 1, 10 ) );
      NewFieldNamesList.Add( FieldName );
      Case Field.DataType Of
        ftMemo, ftFmtMemo:
          Begin
            FieldType := 'M';
            FieldSize := 8;
            FieldDec := 0;
          End;
        ftGraphic:
          Begin
            FieldType := 'G';
            FieldSize := 8;
            FieldDec := 0;
          End;
        ftBytes, ftvarBytes, ftBlob,
          ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor
{$IFDEF LEVEL4}, ftADT, ftArray, ftReference, ftDataSet{$ENDIF}:
          Begin
            FieldType := 'B';
            FieldSize := 8;
            FieldDec := 0;
          End;
        ftString{$IFDEF LEVEL4}, ftFixedChar, ftWideString{$ENDIF}{$IFDEF LEVEL5}, ftGUID{$ENDIF}:
          Begin
            FieldType := 'C';
            FieldSize := ResultSet.Fields[I].ColWidth;
            FieldDec := 0;
          End;
        ftFloat, ftCurrency, ftBCD:
          Begin
            FieldType := 'N';
            FieldSize := 20; // you can change this
            FieldDec := 4;
          End;
        ftDate, ftTime, ftDateTime:
          Begin
            FieldType := 'D';
            FieldSize := 10;
            FieldDec := 0;
          End;
        ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}:
          Begin
            FieldType := 'N';
            FieldSize := 11; // configure this also to your needs
            FieldDec := 0;
          End;
        ftBoolean:
          Begin
            FieldType := 'L';
            FieldSize := 1;
            FieldDec := 0;
          End;
      End;
      FieldList.Add( format( '%s;%s;%d;%d', [FieldName, FieldType, FieldSize, FieldDec] ) );
    End;
    gs6_shel.CreateDBF( FileName, '', FoxPro2, FieldList ); // change FoxPro2 to your choice
    Halc.Open;
    // after creating dbf, insert records from source DBF to dest DBF
    Self.First;
    While Not Self.EOF Do
    Begin
      Halc.Insert;
      For I := 0 To Self.FieldCount - 1 Do
      Begin
        SrcField := Self.Fields[I];
        DestField := Halc.Fields[I];
        DestField.Assign( SrcField );
      End;
      Halc.Post;

      Self.Next;
    End;
  Finally
    If Bookmarkvalid( bm ) Then
      GotoBookmark( bm );
    FreeBookmark( bm );
    EnableControls;
    FieldList.Free;
    NewFieldNamesList.Free;
    Halc.Free;
  End;
End;

Procedure THalcyonxQuery.BeforeQuery( Sender: TObject );
Var
  I: Integer;
Begin
  //if WhereOptimizeMethod<>omSetFilter then
  //begin
  FSaveIndexNeededFor := OnIndexNeededfor;
  FSaveSetRange := OnSetRange;
  FSaveCancelRange := OnCancelRange;
  FSaveSetFilter := OnSetFilter;
  FSaveCancelFilter := OnCancelFilter;

  OnIndexNeededfor := IndexNeededFor;
  OnSetRange := SetRange;
  OnCancelRange := CancelRange;
  OnSetFilter := SetFilter;
  OnCancelFilter := CancelFilter;
  //end;
  For I := 0 To DataSets.Count - 1 Do
  Begin
    With ( DataSets[I].DataSet As THalcyonDataSet ) Do
    Begin
      FSaveUseDeleted[I] := UseDeleted;
      UseDeleted := Self.FUseDeleted;
    End;
  End;
End;

Procedure THalcyonxQuery.AfterQuery( Sender: TObject );
Var
  I: Integer;
Begin
  //if WhereOptimizeMethod<>omSetFilter then
  //begin
  OnIndexNeededfor := FSaveIndexNeededFor;
  OnSetRange := FSaveSetRange;
  OnCancelRange := FSaveCancelRange;
  OnSetFilter := FSaveSetFilter;
  OnCancelFilter := FSaveCancelFilter;
  //end;
  For I := 0 To DataSets.Count - 1 Do
  Begin
    With ( DataSets[I].DataSet As THalcyonDataSet ) Do
      { restore previous states of the dataset }
      UseDeleted := FSaveUseDeleted[I];
  End;
End;

Procedure THalcyonxQuery.SetDataList( Value: TDataList );
Var
  I: Integer;
Begin
  DataSets.Clear;
  If Not Assigned( Value ) Then
    Exit;
  { feed the datasets from the data list }
  For I := 0 To Value.Count - 1 Do
    AddDataSet( Value[I].DataSet, Value[I].Alias );
  UseDeleted := Value.UseDeleted;

  FDataList := Value;
End;

Procedure THalcyonxQuery.FixDummiesForFilter( Var Filter: String );
Var
  Ps: Integer;
  I: Integer;
  Dt: Double;
Begin
  { this method called in the WHERE clause is a filter in
    order to fix some flags:
    - working flag now is the date in the format: 'DummyDate(32445.6566)'
    - another is the handling of True and False in the expression parser }
  ReplaceString( Filter, 'DummyBoolean(True)', 'True' );
  ReplaceString( Filter, 'DummyBoolean(False)', 'False' );
  Ps := AnsiPos( 'DummyDate(', Filter );
  While Ps > 0 Do
  Begin
    If Ps > 0 Then
    Begin
      { by default, the date is left as it is but in descendant classes
        the date can be changed to meet the dataset filter implementation}
      For I := Ps + 1 To Length( Filter ) Do
        If Filter[I] = ')' Then
        Begin
          Dt := StrToFloat( Copy( Filter, Ps + 10, I - ( Ps + 10 ) ) );
          ReplaceString( Filter, Copy( Filter, Ps, ( I - Ps ) + 1 ), '{' + DateToStr( Dt ) + '}' );
          Break;
        End;
    End;
    Ps := AnsiPos( 'DummyDate(', Filter );
  End;
End;

Procedure THalcyonxQuery.SetFilter( Sender: TObject; DataSet: TDataset; Const Filter: String;
  IsJoining: Boolean; Var Handled: Boolean );
Var
  Halcn: THalcyonDataset;
Begin
  { this is only called for the WHERE expression }
  Try
    Halcn := Dataset As THalcyonDataset;
    Halcn.Filtered := False;
    Halcn.Filter := Filter;
    Halcn.Filtered := True;
    Handled := True;
  Except
    Handled := False;
    ( DataSet As THalcyonDataSet ).Filtered := False;
  End;
End;

Procedure THalcyonxQuery.CancelFilter( Sender: TObject; DataSet: TDataSet;
  IsJoining: Boolean );
Begin
  With ( DataSet As THalcyonDataset ) Do
  Begin
    Filtered := False;
    Filter := '';
  End;
End;

End.
