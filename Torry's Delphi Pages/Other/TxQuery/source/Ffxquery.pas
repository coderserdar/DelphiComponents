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
{   The Original Code is FFxQuery.pas                                      }
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

Unit FFxQuery;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, IniFiles, xquery, xqmiscel, xqbase;

//{$R FFxQUERY.DCR}

Type
  {-------------------------------------------------------------------------------}
  {                  forward declarations                                         }
  {-------------------------------------------------------------------------------}
  TDataList = Class;

  {-------------------------------------------------------------------------------}
  {                  Defines TDataItem                                            }
  {-------------------------------------------------------------------------------}

  TDataItem = Class
  Private
    FDataList: TDataList; { belongs to                                          }
    FDataSet: TDataSet; { the TFFxTable                                       }
    FDatabaseName: String;
    FTableName: String;
    FAlias: String; { the alias assigned (to be passed to TFFxQuery)      }
    Function GetFileName: String;
  Public
    Constructor Create( DataList: TDataList );
    Destructor Destroy; Override;
    Procedure Open;

    Property DatabaseName: String Read FDatabaseName Write FDatabaseName;
    Property TableName: String Read FTableName Write FTableName;
    Property Alias: String Read FAlias Write FAlias;
    Property DataSet: TDataSet Read FDataSet Write FDataSet;
    Property FileName: String Read GetFileName;
  End;

  {-------------------------------------------------------------------------------}
  {                  Defines TDataList                                            }
  {-------------------------------------------------------------------------------}

  TDataList = Class
  Private
    FItems: TList;
    FConfigFileName: String;
    FInMemResultSet: Boolean;
    FMapFileSize: Longint;
    FDateFormat: String;
    FUseDisplayLabel: Boolean;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TDataItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add( Const pDatabaseName, pTableName, pAlias: String ): TDataItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure LoadFromFile( Const ConfigFileName: String );
    Procedure SaveToFile( Const ConfigFileName: String );
    Procedure OpenDataSets;
    Procedure CloseDataSets;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TDataItem Read GetItem; Default;
    Property ConfigFileName: String Read FConfigFileName Write FConfigFileName;
    Property InMemResultSet: Boolean Read FInMemResultSet Write FInMemResultSet;
    Property MapFileSize: Longint Read FMapFileSize Write FMapFileSize;
    Property DateFormat: String Read FDateFormat Write FDateFormat;
    Property UseDisplayLabel: Boolean Read FUseDisplayLabel Write FUseDisplayLabel;
  End;

  TFFxQuery = Class( TCustomxQuery )
  Private
    fIndexList: Array[1..2] Of TStringList;
    fDatabaseName: String;
    { this is only a reference to a global object and must not be created }
    fDataList: TDataList;
    // for temporary saving events
    fSaveIndexNeededFor: TIndexNeededForEvent;
    fSaveSetRange: TSetRangeEvent;
    fSaveCancelRange: TCancelRangeEvent;
    fSaveSetFilter: TSetFilterEvent;
    fSaveCancelFilter: TCancelFilterEvent;
    FOldBeforeQuery: TNotifyEvent;
    FOldAfterQuery: TNotifyEvent;

    Procedure SetDataList( Value: TDataList );
    { overriden methods}
    Procedure IndexNeededFor( Sender: TObject;
      DataSet: TDataSet;
      Const FieldNames: String;
      ActivateIndex: Boolean;
      IsJoining: Boolean;
      Var Accept: Boolean );
    Procedure SetRange( Sender: TObject;
      RelOperator: TRelationalOperator;
      DataSet: TDataSet;
      Const FieldNames, StartValues, EndValues: String;
      IsJoining: Boolean );
    Procedure CancelRange( Sender: TObject;
      DataSet: TDataSet;
      IsJoining: Boolean );
    Procedure SetUserRange( Sender: TObject; Dataset: TDataset;
      Const UsingIndex: String;
      ForFields, StartValues, EndValues: TStrings );
    Procedure CancelUserRange( Sender: TObject; Dataset: TDataset );
  Protected

    { not overrides }
    Procedure CreateTable( Sender: TObject; CreateTable: TCreateTableItem );
    Procedure CreateIndex( Sender: TObject;
      Unique, Descending: Boolean;
      Const TableName, IndexName: String;
      ColumnExprList: TStringList );
    Procedure DropTable( Sender: TObject; Const TableName: String );
    Procedure DropIndex( Sender: TObject; Const TableName, IndexName: String );
  Public
    Constructor Create( AOwner: TComponent ); Override;
    Destructor Destroy; Override;
    Procedure Loaded; Override;
    Procedure BeforeQuery( Sender: TObject );
    Procedure AfterQuery( Sender: TObject );

    Property DataList: TDataList Read FDataList Write SetDataList;
  Published
    { inherited properties }
    Property OnBlobNeeded;
    Property DataSets;
    Property DatabaseName: String Read fDatabaseName Write fDatabaseName;
  End;

Procedure Register;

Implementation

Uses
  FFDB;

Procedure Register;
Begin
  RegisterComponents( 'FlashFiler', [TFFxQuery] );
End;

{-------------------------------------------------------------------------------}
{                  Implementes TDataItem                                        }
{-------------------------------------------------------------------------------}

Constructor TDataItem.Create( DataList: TDataList );
Begin
  Inherited Create;
  FDataList := DataList;
  { the dataset belong to the DataList }
  FDataSet := TffTable.Create( Nil );
End;

Destructor TDataItem.Destroy;
Begin
  FDataSet.Free;
  Inherited Destroy;
End;

Function TDataItem.GetFileName: String;
Begin
  Result := AddSlash( FDatabaseName ) + FTableName;
End;

Procedure TDataItem.Open;
Begin
  FDataSet.Close;
  With ( FDataSet As TffTable ) Do
  Begin
    DatabaseName := Self.FDatabaseName;
    Tablename := Self.FTableName;
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

Function TDataList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TDataList.GetItem( Index: Integer ): TDataItem;
Begin
  Result := FItems[Index];
End;

Function TDataList.Add( Const pDatabaseName, pTableName, pAlias: String ): TDataItem;
Begin
  Result := TDataItem.Create( Self );
  Try
    With TDataItem( Result ) Do
    Begin
      DataSet.Close;
      TffTable( DataSet ).DatabaseName := pDatabaseName;
      TffTable( DataSet ).TableName := pTableName;
      If Length( pAlias ) > 0 Then
        Alias := pAlias
      Else
        Alias := ChangeFileExt( ExtractFileName( pTableName ), '' );
      DatabaseName := pDatabaseName;
      TableName := pTableName;
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
  I: Integer;
  DatabaseName: String;
  Tablename: String;
  Alias: String;
Begin
  Clear;
  IniFile := TIniFile.Create( ConfigFileName );
  Try
    { this is the configuration for the file :
    [General]
    NumFiles=3
    DatabaseName1=Examples
    Tablename1=ExCust
    DatabaseName2=Examples
    Tablename2=ExOrders
    Alias1=Customer
    Alias2=Orders
    ...
    FInMemResultSet : Boolean;
    FMapFileSize    : Longint;
    FDateFormat     : String;
    }
    NumFiles := IniFile.ReadInteger( 'General', 'NumFiles', 0 );
    FInMemResultSet := IniFile.ReadBool( 'General', 'InMemResultSet', True );
    FMapFileSize := IniFile.ReadInteger( 'General', 'MapFileSize', 2000000 );
    FDateFormat := IniFile.ReadString( 'General', 'DateFormat', 'm/d/yyyy' );

    For I := 1 To NumFiles Do
    Begin
      DatabaseName := IniFile.ReadString( 'General', 'DatabaseName' + IntToStr( I ), '' );
      TableName := IniFile.ReadString( 'General', 'TableName' + IntToStr( I ), '' );
      Alias := IniFile.ReadString( 'General', 'Alias' + IntToStr( I ), '' );
      Add( DatabaseName, TableName, Alias );
    End;
  Finally
    IniFile.Free;
  End;
  FConfigFileName := ConfigFileName;
End;

Procedure TDataList.SaveToFile( Const ConfigFileName: String );
Var
  IniFile: TIniFile;
  NumFiles, I: Integer;
  DatabaseName, TableName, Alias: String;
  Item: TDataItem;
Begin
  IniFile := TIniFile.Create( ConfigFileName );
  Try
    NumFiles := FItems.Count;
    IniFile.WriteInteger( 'General', 'NumFiles', NumFiles );
    IniFile.WriteBool( 'General', 'InMemResultSet', FInMemResultSet );
    IniFile.WriteInteger( 'General', 'MapFileSize', FMapFileSize );
    IniFile.WriteString( 'General', 'DateFormat', FDateFormat );
    For I := 0 To NumFiles - 1 Do
    Begin
      Item := Items[I];
      DatabaseName := Item.DatabaseName;
      TableName := Item.TableName;
      IniFile.writeString( 'General', 'DatabaseName' + IntToStr( I + 1 ), DatabaseName );
      IniFile.writeString( 'General', 'TableName' + IntToStr( I + 1 ), TableName );
      Alias := Item.Alias;
      IniFile.writeString( 'General', 'Alias' + IntToStr( I + 1 ), Alias );
    End;
  Finally
    IniFile.Free;
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

{ Startt of TFFxQuery implementation }

Function filterL( line, from_ch, to_ch: String ): String;
Var
  posn,
    locn: integer;
Begin
  posn := 1;
  While posn <= length( line ) Do
  Begin
    locn := pos( line[posn], from_ch );
    If locn = 0 Then
      inc( posn )
    Else If locn > length( to_ch ) Then
      delete( line, posn, 1 )
    Else
    Begin
      line[posn] := to_ch[locn];
      inc( posn );
    End;
  End;
  result := line;
End;

Function extractL( line: String; sep: char; locn: integer ): String;
Var
  cntr,
    spos,
    posn: integer;
Begin
  spos := 1;
  For cntr := 1 To locn - 1 Do
  Begin
    posn := pos( sep, copy( line, spos, $FFFF ) );
    If posn = 0 Then
      spos := $FFFF
    Else
      inc( spos, posn );
  End;
  posn := pos( sep, copy( line, spos, $FFFF ) );
  If posn = 0 Then
    posn := $FFFF;
  result := copy( line, spos, posn - 1 );
End;

Constructor TFFxQuery.Create( AOwner: TComponent );
Begin
  Inherited Create( AOwner );
  fIndexList[1] := TStringList.Create;
  fIndexList[2] := TStringList.Create;
  DataSets.DataSetClass := TffTable; // don't accept other datasets
End;

Destructor TFFxQuery.Destroy;
Begin
  fIndexList[1].Free;
  fIndexList[2].Free;
  Inherited Destroy;
End;

Procedure TFFxQuery.Loaded;
Begin
  Inherited Loaded;
  OnIndexNeededFor := IndexNeededFor;
  OnSetRange := SetRange;
  OnCancelRange := CancelRange;
  OnSetUserRange := SetUserRange;
  OnCancelUserRange := CancelUserRange;
  OnCreateTable := CreateTable;
  OnCreateIndex := CreateIndex;
  OnDropTable := DropTable;
  OnDropIndex := DropIndex;
  FOldBeforeQuery := OnBeforeQuery;
  FOldAfterQuery := OnAfterQuery;
  OnBeforeQuery := BeforeQuery;
  OnAfterQuery := AfterQuery;
End;

Procedure TFFxQuery.IndexNeededFor( Sender: TObject;
  DataSet: TDataSet; Const FieldNames: String; ActivateIndex: Boolean;
  IsJoining: Boolean; Var Accept: Boolean );
Var
  fcntr,
    xcntr,
    cntr: integer;
  fNames,
    xNames: String;
Begin
  If IsJoining Then
    Exit;
  Accept := False;
  If Not ( WhereOptimizeMethod = omSetRange ) Then
    exit;

  fNames := AnsiUpperCase( FieldNames );
  fcntr := 1;
  For cntr := 1 To length( fNames ) Do
    If fNames[cntr] = ';' Then
      inc( fcntr );
  With TffTable( DataSet ) Do
  Begin
    If IndexDefs.Count = 0 Then
      IndexDefs.Update;
    For xcntr := fcntr Downto 1 Do
    Begin
      For cntr := 0 To IndexDefs.Count - 1 Do
      Begin
        xNames := AnsiUpperCase( TIndexDef( IndexDefs[cntr] ).Fields );
        If fNames + ';' = copy( xNames + ';', 1, length( fNames ) + 1 ) Then
        Begin
          Accept := True;
          If fIndexList[1].IndexOf( DataSet.Name + '|' + FieldNames ) = -1 Then
          Begin
            fIndexList[1].Add( DataSet.Name + '|' + FieldNames );
            fIndexList[2].Add( filterL( IntToStr( xcntr ) + ';' + fNames, ';', #13 ) );
          End;
          IndexFieldNames := fNames;
          break;
        End;
      End;
      If Accept Then
        break;
      If fcntr <> 1 Then
      Begin
        For cntr := length( fNames ) Downto 1 Do
        Begin
          If fNames[cntr] = ';' Then
          Begin
            fNames := copy( fNames, 1, cntr - 1 );
            break;
          End;
        End;
      End;
    End;
  End;
End;

Procedure TFFxQuery.SetRange( Sender: TObject; RelOperator: TRelationalOperator;
  DataSet: TDataSet; Const FieldNames, StartValues, EndValues: String; IsJoining: Boolean );
Var
  ix: integer;
  fc: integer;
  fNames: TStringList;

  Procedure LoadRangeValues( Const rvals: String );
  Var
    cntr: integer;
    valstr: String;
  Begin
    For cntr := 1 To fc Do
    Begin
      valstr := extractL( rvals, ';', cntr );
      Try
        TffTable( DataSet ).FieldByName( fNames[cntr] ).Value := valstr;
      Except
        TffTable( DataSet ).FieldByName( fNames[cntr] ).Value := StrToFloat( valstr );
      End;
    End;
  End;

Begin
  ix := fIndexList[1].IndexOf( DataSet.Name + '|' + FieldNames );
  If ix <> -1 Then
  Begin
    fNames := TStringList.Create;
    fNames.Text := fIndexList[2][ix];
    With TffTable( DataSet ) Do
    Begin
      fc := StrToInt( fNames[0] );
      SetRangeStart;
      KeyFieldCount := fc;
      If RelOperator In [ropBETWEEN, ropGT, ropGE] Then
        LoadRangeValues( StartValues );
      SetRangeEnd;
      KeyFieldCount := fc;
      If RelOperator In [ropBETWEEN, ropLT, ropLE] Then
        LoadRangeValues( EndValues );
      ApplyRange;
    End;
    fNames.Free;
  End;
End;

Procedure TFFxQuery.CancelRange( Sender: TObject; DataSet: TDataSet; IsJoining: Boolean );
Begin
  TffTable( DataSet ).CancelRange;
End;

{ end-user defined ranges}

Procedure TFFxQuery.SetUserRange( Sender: TObject; Dataset: TDataset;
  Const UsingIndex: String;
  ForFields, StartValues, EndValues: TStrings );
Var
  I: Integer;
Begin
  With TffTable( Dataset ) Do
  Begin
    IndexName := UsingIndex;

    SetRangeStart;

    KeyFieldCount := StartValues.Count;
    For I := 0 To ForFields.Count - 1 Do
      FieldByName( ForFields[I] ).AsString := StartValues[I];

    SetRangeEnd;

    KeyFieldCount := EndValues.Count;
    For I := 0 To ForFields.Count - 1 Do
      FieldByName( ForFields[I] ).AsString := EndValues[I];

    ApplyRange;
  End;
End;

Procedure TFFxQuery.CancelUserRange( Sender: TObject; Dataset: TDataset );
Begin
  TffTable( DataSet ).CancelRange;
End;

Procedure TFFxQuery.CreateTable( Sender: TObject; CreateTable: TCreateTableItem );
Begin
End;

Procedure TFFxQuery.CreateIndex( Sender: TObject; Unique, Descending: Boolean;
  Const TableName, IndexName: String; ColumnExprList: TStringList );
Begin
End;

Procedure TFFxQuery.DropTable( Sender: TObject; Const TableName: String );
Begin
End;

Procedure TFFxQuery.DropIndex( Sender: TObject; Const TableName, IndexName: String );
Begin
End;

Procedure TFFxQuery.SetDataList( Value: TDataList );
Var
  I: Integer;
Begin
  DataSets.Clear;
  If Not Assigned( Value ) Then
    Exit;
  { feed the datasets from the data list }
  For I := 0 To Value.Count - 1 Do
    AddDataSet( Value[I].DataSet, Value[I].Alias );

  FDataList := Value;
End;

Procedure TFFxQuery.BeforeQuery( Sender: TObject );
Begin
  If Not ( WhereOptimizeMethod = omSetRange ) Then
  Begin
    FSaveIndexNeededFor := OnIndexNeededfor;
    FSaveSetRange := OnSetRange;
    FSaveCancelRange := OnCancelRange;
    FSaveSetFilter := OnSetFilter;
    FSaveCancelFilter := OnCancelFilter;

    OnIndexNeededfor := Nil;
    OnSetRange := Nil;
    OnCancelRange := Nil;
    OnSetFilter := Nil;
    OnCancelFilter := Nil;
  End;
  If Assigned( FOldBeforeQuery ) Then
    FOldBeforeQuery( Self );
End;

Procedure TFFxQuery.AfterQuery( Sender: TObject );
Begin
  If Not ( WhereOptimizeMethod = omSetRange ) Then
  Begin
    OnIndexNeededfor := FSaveIndexNeededFor;
    OnSetRange := FSaveSetRange;
    OnCancelRange := FSaveCancelRange;
    OnSetFilter := FSaveSetFilter;
    OnCancelFilter := FSaveCancelFilter;
  End;
  If Assigned( FOldAfterQuery ) Then
    FOldAfterQuery( Self );
End;

End.
