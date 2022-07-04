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
{   The Original Code is HTMLqry.pas                                       }
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

Unit HTMLqry;

{$I XQ_FLAG.INC}
Interface

Uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, Db;

{ this unit is for exporting a dataset to HTML }

Type
  THTMLExport = Class( TComponent )
  Private
    FFooter: TStrings;
    FHeader: TStrings;
    FTitle: TStrings;
    FDataSet: TDataSet;
    FBodyColor: TColor;
    FTableHeaderColor: TColor;
    FTableBodyColor: TColor;
    FTableOddRowColor: TColor;
    Procedure SetDataSet( Value: TDataSet );
  Protected
    Procedure Notification( AComponent: TComponent; Operation: toperation ); Override;
  Public
    Constructor Create( AOwner: TComponent ); Override;
    Destructor Destroy; Override;
    Procedure SaveToFile( Const FileName: String );
  Published
    Property Footer: TStrings Read FFooter;
    Property Header: TStrings Read FHeader;
    Property Title: TStrings Read FTitle;
    Property DataSet: TDataSet Read FDataSet Write SetDataSet;
    Property BodyColor: TColor Read FBodyColor Write FBodyColor;
    Property TableHeaderColor: TColor Read FTableHeaderColor Write FTableHeaderColor;
    Property TableBodyColor: TColor Read FTableBodyColor Write FTableBodyColor;
    Property TableOddRowColor: TColor Read FTableOddRowColor Write FTableOddRowColor;
  End;

Implementation

Uses xquery;

Constructor THTMLExport.Create( AOwner: TComponent );
Begin
  Inherited Create( AOwner );
  FFooter := TStringList.Create;
  FHeader := TStringList.Create;
  FTitle := TStringList.Create;
  FBodyColor := 16777194;
  FTableHeaderColor := 3394815;
  FTableBodyColor := 16777194;
  FTableOddRowColor := 3394764;
End;

Destructor THTMLExport.Destroy;
Begin
  FFooter.Free;
  FHeader.Free;
  FTitle.Free;
  Inherited Destroy;
End;

Procedure THTMLExport.SetDataSet( Value: TDataSet );
Begin
  If Value <> FDataSet Then
  Begin
    FDataSet := Value;
    Value.FreeNotification( Self );
  End;
End;

Procedure THTMLExport.Notification( AComponent: TComponent; Operation: toperation );
Begin
  Inherited Notification( AComponent, Operation );
  If ( Operation = opRemove ) And ( Acomponent = FDataSet ) Then
    FDataSet := Nil;
End;

Procedure THTMLExport.SaveToFile( Const FileName: String );
Var
  f: TextFile;
  i, Count: Integer;
  s, Align: String;
Begin
  If Not Assigned( FDataSet ) Or Not FDataSet.Active Then
    Exit;
  AssignFile( f, FileName );
  Rewrite( f );
  Try
    WriteLn( f, '<HTML>' );
    If ( Length( FHeader.Text ) > 0 ) Or ( Length( FTitle.Text ) > 0 ) Then
    Begin
      WriteLn( f, '<HEAD>' );
      If Length( FTitle.Text ) > 0 Then
      Begin
        Write( f, '<TITLE>' );
        For i := 0 To FTitle.Count - 1 Do
          WriteLn( f, FTitle[i] );
        WriteLn( f, '</TITLE>' );
      End;
      WriteLn( f, '<H3>' );
      For i := 0 To FHeader.Count - 1 Do
        WriteLn( f, FHeader[i], '<BR>' );
      WriteLn( f, '</H3>' );
      WriteLn( f, '</HEAD>' );
      WriteLn( f, '<HR>' );
    End;
    WriteLn( f, Format( '<BODY BGCOLOR="#%s">', [IntToHex( FBodyColor, 6 )] ) );
    WriteLn( f, Format( '<TABLE BGCOLOR="#%s" BORDER>', [IntToHex( FTableBodyColor, 6 )] ) );
    { The title }
    WriteLn( f, Format( '<TR BGCOLOR="#%s" NOWRAP>', [IntToHex( FTableHeaderColor, 6 )] ) );
    For i := 0 To FDataSet.FieldCount - 1 Do
      WriteLn( f, Format( '  <TH NOWRAP>%s</TH>', [FDataSet.Fields[i].FieldName] ) );
    WriteLn( f, '</TR>' );
    { now write all the rows }
    FDataSet.First;
    Count := 0;
    While Not FDataSet.EOF Do
    Begin
      Inc( Count );
      { write all this row }
      If ( Count Mod 2 ) = 0 Then
      Begin
        WriteLn( f, Format( '<TR BGCOLOR ="#%s">', [IntToHex( FTableOddRowColor, 6 )] ) );
      End
      Else
        WriteLn( f, '<TR>' );
      ;
      For i := 0 To FDataSet.FieldCount - 1 Do
      Begin
        If FDataSet.Fields[i].DataType In ftNonTextTypes Then
        Begin
          s := '(Blob/Memo)';
        End
        Else
        Begin
          Align := '';
          Case FDataSet.Fields[i].DataType Of
            ftFloat, ftCurrency, ftBCD,
              ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}
            : Align := ' ALIGN=RIGHT';
          End;
          s := FDataSet.Fields[i].AsString;
        End;
        Write( f, Format( '  <TD NOWRAP%s>%s', [Align, s] ) );
        If Length( s ) = 0 Then
          WriteLn( f, '<BR></TD>' )
        Else
          WriteLn( f, '</TD>' )
      End;
      WriteLn( f, '</TR>' );
      FDataSet.Next;
    End;
    WriteLn( f, '</TABLE>' );
    If Length( FFooter.Text ) > 0 Then
    Begin
      WriteLn( f, '<HR>' );
      Write( f, '<P>' );
      For i := 0 To FFooter.Count - 1 Do
        WriteLn( f, FFooter[i], '<BR>' );
      WriteLn( f, '</P>' );
    End;
    WriteLn( f, '</BODY>' );
    WriteLn( f, '</HTML>' );
  Finally
    CloseFile( f );
  End;
End;

End.
