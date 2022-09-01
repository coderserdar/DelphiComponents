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
{   The Original Code is xqJoins.pas                                       }
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

unit xqJoins;

{$I XQ_FLAG.INC}
interface

Uses
  SysUtils, Classes, Db, XQMiscel, xqbase, qexpryacc, qbaseexpr ;

Type

  TJoinAction = ( jkLeftInnerJoin,
                  jkRightInnerJoin,
                  jkLeftOuterJoin,
                  jkRightOuterJoin,
                  jkFullOuterJoin );

  TJOINOnList = Class;

  TJOINOnItem = Class
  Private
    FJOINOnList: TJOINOnList;   { belongs to }
    FJOINAction: TJOINAction;   { left inner JOIN, left outer JOIN, etc. }
    FJOINExpression: String;
    FLeftRefTest: string;
    FRightRefTest: string;
    FSortList: TxqSortList;
    { the expression resolver for the full JOIN expression }
    FResolver: TExprParser;
    { field used to obtain the value on the right Dataset in order to sort
      the FSortList }
    FLeftField: TField;
    FRightField: TField;
    // gis product
    FGraphicJoin : Boolean;
  Public
    Constructor Create( JOINOnList: TJOINOnList );
    Destructor Destroy; Override;
    Procedure Assign( Source: TJOINOnItem );

    Property JOINAction: TJOINAction Read FJOINAction Write FJOINAction;
    Property JOINExpression: String Read FJOINExpression Write FJOINExpression;
    Property Resolver: TExprParser Read FResolver Write FResolver;
    Property SortList: TxqSortList read FSortList;
    Property LeftRefTest: string read FLeftRefTest write FLeftRefTest;
    Property RightRefTest: string read FRightRefTest write FRightRefTest;
    // gis product
    Property GraphicJoin: Boolean read FGraphicJoin write FGraphicJoin;
  End;

  TJOINOnList = Class
  Private
    FAnalizer: TObject;
    FItems: TList;
    FLeftTableIndexList: TxqIntegerList;
    FRightTableIndexList: TxqIntegerList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TJOINOnItem;
  Public
    Constructor Create( Analizer: TObject );  // TObject in order to avoid redundancy with xquery.pas unit
    Destructor Destroy; Override;
    Function Add: TJOINOnItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Procedure PrepareJOIN;
    procedure DoJOINOn;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TJOINOnItem Read GetItem; Default;
  End;

implementation

uses
  xquery, xqconsts;

{-------------------------------------------------------------------------------}
{                  Implement TJOINOnItem                                        }
{-------------------------------------------------------------------------------}

Constructor TJOINOnItem.Create( JOINOnList: TJOINOnList );
Begin
  Inherited Create;
  fJOINOnList := JOINOnList;
  FSortList := TMemSortList.Create( True );
End;

Destructor TJOINOnItem.Destroy;
Begin
  If Assigned( FResolver ) Then FResolver.Free;
  FSortList.Free;
  Inherited Destroy;
End;

Procedure TJOINOnItem.Assign( Source: TJOINOnItem );
Begin
  FJOINAction     := Source.FJOINAction;
  FJOINExpression := Source.FJOINExpression;
  FLeftRefTest   := Source.FLeftRefTest;
  FRightRefTest  := Source.FRightRefTest;
End;

{-------------------------------------------------------------------------------}
{                  Implement TJOINOnList                                        }
{-------------------------------------------------------------------------------}

Constructor TJOINOnList.Create( Analizer: TObject );
Begin
  Inherited Create;
  FAnalizer:= Analizer;
  FItems := TList.Create;
  FLeftTableIndexList:= TxqIntegerList.Create;
  FRightTableIndexList:= TxqIntegerList.Create;
End;

Destructor TJOINOnList.Destroy;
Begin
  Clear;
  FItems.Free;
  FLeftTableIndexList.Free;
  FRightTableIndexList.Free;
  Inherited Destroy;
End;

Function TJOINOnList.GetCount;
Begin
  Result := FItems.Count;
End;

Function TJOINOnList.GetItem( Index: Integer ): TJOINOnItem;
Begin
  Result := FItems[Index];
End;

Function TJOINOnList.Add: TJOINOnItem;
Begin
  Result := TJOINOnItem.Create( Self );
  FItems.Add( Result );
End;

Procedure TJOINOnList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To FItems.Count - 1 Do
    TJOINOnItem( FItems[I] ).Free;
  FItems.Clear;
End;

Procedure TJOINOnList.Delete( Index: Integer );
Begin
  TJOINOnItem( FItems[Index] ).Free;
  FItems.Delete( Index );
End;

procedure TJOINOnList.DoJOINOn;
{$IFDEF FALSE}
  Procedure SaveBookmarks(var bml: TxqIntegerList);
  var
    I: Integer;
  begin
    bml:= TxqIntegerList.Create;
    With TSqlAnalizer(FAnalizer) Do
    begin
      For I:= 0 to TableList.Count-1 do
      begin
        bml.Add(0);
        If TableList[I].DataSet <> Nil then
          bml[I]:= Longint(TableList[I].DataSet.GetBookmark);
      end;
    end;
  end;

  Procedure RestoreBookmarks(var bml: TxqIntegerList);
  var
    I: Integer;
  begin
    With TSqlAnalizer(FAnalizer) Do
    begin
      For I:= 0 to TableList.Count-1 do
        If TableList[I].DataSet <> Nil then
        begin
          TableList[I].DataSet.GotoBookmark(TBookmark(bml[I]));
          TableList[I].DataSet.FreeBookmark(TBookmark(bml[I]));
        end;
      FreeAndNil(bml);
    end;
  end;
{$ENDIF}

  Procedure MoveNextForNonLinked(n: Integer);
  Var
    J: Integer;
    JOI, JOIK: TJOINOnItem;
    Found: Boolean;
  begin
    If n < 1 then Exit;
    JOI := FItems[n];
    Found:= False;
    for J:= 0 to n - 1 do
    begin
      JOIK:= FItems[J];
      If (JOIK.FLeftField.DataSet = JOI.FLeftField.DataSet) Or
         (JOIK.FRightField.DataSet = JOI.FLeftField.DataSet) Then
      begin
        Found:= True;
        Break;
      end;
    end;
    If Not Found And Not JOI.FLeftField.DataSet.Eof then
      JOI.FLeftField.DataSet.Next;
  end;

  Procedure RecursiveJOIN( Start: Integer; Var TotalRecsAdded: Integer );
  Var
    I, RecsAdded: Integer;
    JOI: TJOINOnItem;
    RightDataset: TDataset;
    HasMoreJOINs: Boolean;
    MustRemove: Boolean;
    ThisCount, RightIndex: Integer;
    D: TDataSet;
    //bml: TxqIntegerList;
  Begin
    RecsAdded := 0;

    RightIndex:= FRightTableIndexList[Start];

    With TSqlAnalizer( FAnalizer ) Do
    Begin
      JOI := JOINList[Start];

      HasMoreJOINs:= Start < JoinList.Count - 1 ;

      If JOI.JOINAction = jkLeftInnerJOIN Then
      Begin
        If Not JOI.FLeftField.IsNull Then
        Begin
          { filter the right Dataset with the value of the related left field }
          JOI.FSortList.Filter( JOI.FLeftField.Value );

          RightDataset := TableList[ RightIndex ].Dataset;

          // search for records meeting the criteria
          JOI.FSortList.First;
          While Not JOI.FSortList.Eof do
          Begin
            {$if RTLVersion <= 18.5}
            RightDataset.GotoBookmark( TBookmark( JOI.FSortList.SourceRecno ) );
            {$else}
            RightDataset.GotoBookmark( JOI.FSortList.SourceBookmark ); { patched by ccy }
            {$ifend}

            If JOI.Resolver.Expression.AsBoolean Then
            Begin
              If HasMoreJOINs Then
              Begin
                { call recursively }
                {SaveBookmarks(bml);
                try}
                RecursiveJOIN( Start + 1, RecsAdded );
                {finally
                  RestoreBookmarks(bml);
                end;}
              End
              Else
              Begin
                AddThisRecord( DefDataset );
                Inc( RecsAdded );
              End;
            End;

            JOI.FSortList.Next;
          End;

        End;
      End Else if JOI.JOINAction = jkLeftOuterJOIN Then
      Begin
        { con esto se cubre el caso en que el campo es nulo }
        ThisCount:= 0;
        RightDataset := TableList[ RightIndex ].Dataset;
        If JOI.FLeftField.IsNull Then
        Begin
          { disable all right Datasets }
          if HasMoreJoins then
          begin
            MustRemove:= false;
            If xQuery.DisabledDatasets.IndexOf( RightDataset ) < 0 Then
            begin
              xQuery.DisabledDatasets.Add( RightDataset );
              MustRemove:= true;
            end;
            {SaveBookmarks(bml);
            try}
            RecursiveJOIN( Start + 1, ThisCount );
            {finally
              RestoreBookmarks(bml);
            end;}
            If MustRemove then
              xQuery.DisabledDatasets.Remove( RightDataset );
          end;
        End Else
        Begin
          JOI.FSortList.Filter( JOI.FLeftField.Value );
          JOI.FSortList.First;
          While Not JOI.FSortList.Eof do
          Begin
            {$if RTLVersion <= 18.5}
            RightDataset.GotoBookmark( TBookmark( JOI.FSortList.SourceRecno ) );
            {$else}
            RightDataset.GotoBookmark( JOI.FSortList.SourceBookmark ); { patched by ccy }
            {$ifend}
            If JOI.Resolver.Expression.AsBoolean Then
            Begin
              If HasMoreJOINs Then
              Begin
                { call recursively }
                {SaveBookmarks(bml);
                try}
                RecursiveJOIN( Start + 1, ThisCount );
                {finally
                  RestoreBookmarks(bml);
                end;}
              End
              Else
              Begin
                AddThisRecord( DefDataset );
                Inc( ThisCount );
              End;
            End;
            JOI.FSortList.Next;
          End;

          MustRemove:= False;
          If ThisCount = 0 Then
          Begin
            { disable this Dataset because it has no meeting records }
            If xQuery.DisabledDatasets.IndexOf( RightDataset ) < 0 Then
            begin
              xQuery.DisabledDatasets.Add( RightDataset );
              MustRemove:= True;
            end;
          End;

          If ( ThisCount = 0 ) And HasMoreJOINs Then
          Begin
            {SaveBookmarks(bml);
            try}
            RecursiveJOIN( Start + 1, ThisCount );
            {finally
              RestoreBookmarks(bml);
            end;}
          End;

          If MustRemove Then
          Begin
            xQuery.DisabledDatasets.Remove( RightDataset );
          End;

        End;

        Inc( RecsAdded, ThisCount );

        If RecsAdded = 0 Then
        Begin
          for I:= Start to FRightTableIndexList.Count - 1 do
          begin
            D := TableList[FRightTableIndexList[I]].Dataset;
            If xQuery.DisabledDatasets.IndexOf( D ) < 0 Then
              xQuery.DisabledDatasets.Add( D );
          end;
          AddThisRecord( DefDataset );
          Inc( RecsAdded );
        End;

      End;

      MoveNextForNonLinked(Start);

      Inc( TotalRecsAdded, RecsAdded );

    End;

  End;

Var
  I, nRecs: Integer;
Begin
  { recursively JOINing }

  // Initialize this record join
  With TSqlAnalizer(FAnalizer) Do
  begin
    xQuery.DisabledDatasets.Clear;
    {For I:= 1 to TableList.Count-1 do
    begin
      If TableList[I].DataSet <> Nil then
        TableList[I].DataSet.First;
    end;}
  end;

  nRecs := 0;

  RecursiveJOIN( 0, nRecs );

  with TSqlAnalizer(FAnalizer) do
  begin

    If {Not WhereContainsOnlyBasicFields Or} (MainWhereResolver=Nil) Or
      (MainWhereResolver.Expression=Nil) Or ( nRecs = 0 ) Or
      ( SubQueryList.Count > 0 ) Or ( Length( WhereStr ) = 0 ) Then
    begin
      Exit;
    end;

    { delete the records not meeting WHERE expression }
    For I := ResultSet.RecordCount Downto ( ResultSet.RecordCount - nRecs + 1 ) Do
    Begin
      ResultSet.Recno := I;
      If Not MainWhereResolver.Expression.AsBoolean Then
      Begin
        ResultSet.Delete;
      End ;
    End;
  end;

end;

procedure TJOINOnList.PrepareJOIN;
Var
  I, J, Index, lrtIndex, rrtIndex, temp: Integer ;
  ThisRef, LeftRef, fname, tname, aname, lrt, rrt: string ;
  DSet : TDataset ;
  FieldExprType : TExprType ;
begin
  FRightTableIndexList.Clear;
  with TSqlAnalizer( FAnalizer ) do
  begin
    For I := 0 To FItems.Count - 1 Do
    Begin
      With TJOINOnItem( FItems[I] ) Do
      Begin
        lrt := UpperCase( FLeftRefTest );
        rrt := UpperCase( FRightRefTest );
        Index:= AnsiPos( '.', lrt );
        if Index = 0 then
          // if table not defined, then use the default
          lrt := AddSquareBrackets( TableList[0].TableName ) + '.' + lrt;
        Index:= AnsiPos( '.', rrt);
        if Index = 0 then
          // if table not defined, then use the default
          rrt := AddSquareBrackets( TableList[0].TableName ) + '.' + rrt;

        { In this version, only the AND operator is allowed in a JOIN, example
          SELECT * FROM customer INNER JOIN orderItems ON
            ( Customer.ItemID = OrderItems.ItemID ) AND
            ( Customer.CusID = OrderItems.CustomerID )

          The following JOIN using OR, is not allowed and will give unexpected results
          SELECT * FROM customer INNER JOIN orderItems ON
            ( Customer.ItemID = OrderItems.ItemID ) OR
            ( Customer.CusID = OrderItems.CustomerID ) }

        // encuentra los indices de la tabla que esta adelante y atras
        lrtIndex:= -1;
        rrtIndex:= -1;
        for J:= 0 to TableList.Count - 1 do
        begin
          // Detect which one of the two is: the table name or the alias
          tname := UpperCase( TableList[ J ].TableName );
          aname := UpperCase( TableList[ J ].Alias );

          If ( lrtIndex < 0 ) And (
             ( AnsiPos( TrimSquareBrackets( tname + '.' ), TrimSquareBrackets( lrt ) ) = 1 ) Or
             ( AnsiPos( TrimSquareBrackets( aname + '.' ), TrimSquareBrackets( lrt ) ) = 1 ) ) Then
          Begin
            lrtIndex:= J;
          End;

          If ( rrtIndex < 0 ) And (
             ( AnsiPos( TrimSquareBrackets( tname + '.' ), TrimSquareBrackets( rrt ) ) = 1 ) Or
             ( AnsiPos( TrimSquareBrackets( aname + '.' ), TrimSquareBrackets( rrt ) ) = 1 ) ) Then
          Begin
            rrtIndex:= J ;
          End ;

          if (lrtIndex >= 0) and (rrtIndex >= 0) then Break;

        end;

        if (lrtIndex < 0) or (rrtIndex < 0) then
          Raise ExQueryError.Create( SJOINInvalidFieldName );

        if lrtIndex < rrtIndex then
        begin
          ThisRef := rrt ;
          LeftRef := lrt ;
        end else
        begin
          ThisRef := lrt ;
          LeftRef := rrt ;

          temp:= rrtIndex;
          rrtIndex:= lrtIndex;
          lrtIndex:= temp;
        end;

        FRightTableIndexList.Add(rrtIndex);

        DSet := TableList[ rrtIndex ].Dataset;

        Resolver := TExprParser.Create( FAnalizer, DefDataset );
        Try
          FResolver.ParseExpression( JOINExpression );
          { now define the sort list for the first field on the referenced
            fields of the right table }
          Index:= AnsiPos( '.', ThisRef);
          fname := TrimSquareBrackets( Copy( ThisRef, Index + 1, Length( ThisRef ) ) );
          { ahora ordena por esta tabla. Nota: necesito guardar los bookmarks
            de la tabla para rapidamente localizar el registro correspondiente
            ya que se efectuara un ordenamiento por esa tabla
            The sort list will save the bookmark instead of the recno of the Dataset
            }
          { detect the field type in the sort list }
          FRightField:= DSet.FindField( fname );
          if FRightField = Nil then
            Raise ExQueryError.Create( SJOINInvalidFieldName );
          SortList.UsingBookmark:= true;
          SortList.BookmarkedDataset := DSet;
          FieldExprType:= XQMiscel.Field2Exprtype( FRightField.Datatype );
          SortList.AddField( FieldExprType, FRightField.Size, False );
          { now add all the records and sort }
          DSet.First;
          While Not DSet.Eof do
          Begin
            SortList.Insert;
            {$if RTLVersion <= 18.5}
            SortList.SourceRecno := Longint( DSet.GetBookmark );
            {$else}
            SortList.SourceBookmark := DSet.GetBookmark;  { patched by ccy }
            {$ifend}
            Case FieldExprType Of
              ttString : SortList.Fields[0].AsString  := FRightField.AsString;
              ttFloat  : SortList.Fields[0].AsFloat   := FRightField.AsFloat;
              ttInteger: SortList.Fields[0].AsInteger := FRightField.AsInteger;
              ttBoolean: SortList.Fields[0].AsBoolean := FRightField.AsBoolean;
            End;
            DSet.Next;
          End;
          // sort the records
          SortList.Sort();
        Except
          FreeObject( FResolver );
          Raise;
        End;
        // ahora busca la tabla izquierda que se relaciona con este JOIN
        FLeftField := Nil;
        Index:= AnsiPos( '.', LeftRef);
        fname := TrimSquareBrackets( Copy( LeftRef, Index + 1, Length( LeftRef ) ) );

        FLeftField := TableList[ lrtIndex ].Dataset.FindField( fname );
        if FLeftField = Nil then
          Raise ExQueryError.Create( SJOINInvalidFieldName );
      End;
    End;
  End;
end;

end.
