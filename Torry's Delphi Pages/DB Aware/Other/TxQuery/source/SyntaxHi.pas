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
{   The Original Code is SyntaxHi.pas                                      }
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

Unit SyntaxHi;

Interface

{$I XQ_FLAG.INC}
Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  xqlex, xqyacc, StdCtrls, Inifiles, ComCtrls, Buttons, RichEdit, xquery;

Type

  TPosChangeEvent = Procedure( Sender: TObject; Row, Col: Integer ) Of Object;
  TUpdateMode = ( umCharacter, umLine );

  TColorConfig = Class; // forward declaration

  TSyntaxHighlighter = Class( TComponent )
  Private
    FEditor: TRichEdit;
    FColorConfig: TColorConfig;
    FFontFamily: String;
    FUpdateMode: TUpdateMode;
    FInternalModified, FChanging: Boolean;
    FLastLine: Integer;
    FxQuery: TCustomXQuery; // used only to hilite table names and fields

    FSaveOnChange: TNotifyEvent;
    FSaveOnSelectionChange: TNotifyEvent;
    FSaveOnExit: TNotifyEvent;
    FOnPosChange: TPosChangeEvent;
    Procedure SetEditor( Value: TRichEdit );
    Procedure SetXQuery( Value: TCustomXQuery );
    Procedure MyOnChange( Sender: TObject );
    Procedure MyOnSelectionChange( Sender: TObject );
    Procedure MyOnExit( Sender: TObject );
  Protected
    Procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
  Public
    Constructor Create( AOwner: TComponent ); Override;
    Destructor Destroy; Override;
    Procedure Execute;
{$IFNDEF BCB}
    Procedure EditColorSet;
{$ENDIF}
    Procedure FontChanged;

    Property ColorConfig: TColorConfig Read FColorConfig;
  Published
    Property UpdateMode: TUpdateMode Read FUpdateMode Write FUpdateMode;
    Property Editor: TRichEdit Read FEditor Write SetEditor;
    Property XQuery: TCustomXQuery Read FxQuery Write SetxQuery;

    Property OnPosChange: TPosChangeEvent Read FOnPosChange Write FOnPosChange;
  End;

  TElementGroup = ( idWhiteSpace,
    idComment,
    idReservedWord,
    idIdentifier,
    idTable,
    idField,
    idString,
    idNumber,
    idComma,
    idParenthesis,
    idOperator,
    idSemicolon,
    idPeriod );

  PColorElement = ^TColorElement;
  TColorElement = Record
    Elements: TList;
    Group: TElementGroup;
    ForeColor: TColor;
    BackColor: TColor;
    FontStyle: TFontStyles;
  End;

  TColorConfig = Class( TObject )
  Private
    FColorSettings: TList;
    FColorTable: TList;
    Function Get( Index: Integer ): TColorElement;
    Procedure Put( Index: Integer; Const Value: TColorElement );
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear;
    Procedure Assign( Value: TColorConfig );
    Procedure LoadFromFile( Const FileName: String );
    Procedure SaveToFile( Const FileName: String );
    Procedure SetColorElement( Group: TElementGroup; ForeColor, BackColor: TColor;
      FontStyle: TFontStyles );
    Function Count: Integer;
{$IFNDEF BCB}
    Procedure EditColorSettings;
{$ENDIF}
    Function FindConfig( Element: Integer; Var ForeColor, BackColor: TColor;
      Var FontStyle: TFontStyles ): Boolean;
    Procedure CreateColorTable;
    Function IndexOfColor( Color: TColor ): Integer;
    Function IndexOfGroup( Group: TElementGroup ): Integer;

    Property Elements[Index: Integer]: TColorElement Read Get Write Put;
    Property ColorTable: TList Read FColorTable;
    Property ColorSettings: TList Read FColorSettings;
  End;

Implementation

Uses
  DB, xqmiscel, xqconsts
{$IFNDEF BCB}
  , ColorSet
{$ENDIF}
  ;

Const
  _TABLE = 1000;
  _FIELD = 1010;

Type
  TCustomXQueryClass = Class( TCustomXQuery );

Constructor TSyntaxHighlighter.Create( AOwner: TComponent );
Begin
  Inherited Create( AOwner );
  FColorConfig := TColorConfig.Create;
  FColorConfig.LoadFromFile( ExtractFilePath( Application.Exename ) + 'colortbl.cfg' );
  FColorConfig.CreateColorTable;

  FLastLine := -1;
End;

Destructor TSyntaxHighlighter.Destroy;
Begin
  FColorConfig.free;
  Inherited Destroy;
End;

Procedure TSyntaxHighlighter.SetEditor( Value: TRichEdit );
Begin
  If Assigned( FEditor ) Then
  Begin
    //Restore previous
    FEditor.OnChange := FSaveOnChange;
    FEditor.OnSelectionChange := FSaveOnSelectionChange;
    FEditor.OnExit := FSaveOnExit;
  End;
  FEditor := Value;
  If Assigned( FEditor ) Then
  Begin
    FSaveOnChange := FEditor.OnChange;
    FSaveOnSelectionChange := FEditor.OnSelectionChange;
    FSaveOnExit := FEditor.OnExit;

    FEditor.OnChange := MyOnChange;
    FEditor.OnSelectionChange := MyOnSelectionChange;
    FEditor.OnExit := MyOnExit;
    FontChanged; // calculate new font family

    Value.FreeNotification( Self );
    If Not ( csDesigning In ComponentState ) Then
      Execute;
  End;
End;

Procedure TSyntaxHighlighter.SetXQuery( Value: TCustomXQuery );
Begin
  FXQuery := Value;
  If Assigned( Value ) Then
    Value.FreeNotification( Self );
End;

Procedure TSyntaxHighlighter.Notification( AComponent: TComponent; Operation: TOperation );
Begin
  Inherited Notification( AComponent, Operation );
  If ( Operation = opRemove ) Then
  Begin
    If AComponent = FEditor Then
      SetEditor( Nil )
    Else If AComponent = FXQuery Then
      FXQuery := Nil;
  End;
End;

{$IFNDEF BCB}

Procedure TSyntaxHighlighter.EditColorSet;
Var
  FileName: String;
Begin
  FileName := ExtractFilePath( Application.Exename ) + 'colortbl.cfg';
  With TfrmColorSettings.Create( Application ) Do
  Begin
    Try
      If Enter( FColorConfig ) = mrOk Then
      Begin
        FColorConfig.SaveToFile( FileName );
        FColorConfig.CreateColorTable;
        Self.Execute;
      End;
    Finally
      Free;
    End;
  End;
End;
{$ENDIF}

Procedure TSyntaxHighlighter.Execute;
Var
  inputStream: TMemoryStream;
  outputStream: TMemoryStream;
  errorStream: TMemoryStream;
  s: String;
  lexer: TXQLexer;
  yychar: Integer; (* current lookahead character *)
  forecolor, backcolor: TColor;
  fontstyle: TFontStyles;
  I, token: Integer;
  atext, Reslt, RtfHeader: String;
  DataSet: TDataSet;
  Field: TField;
  ayytext : string;

  Procedure strToRichEdit( Const S: String );
  Var
    aMem: TMemoryStream;
    SelStart: Integer;
  Begin
    aMem := TMemoryStream.Create;
    FChanging := True;
    SelStart := 0; //Basri
    Try
      aMem.Write( Pointer( S )^, Length( S ) );
      aMem.Position := 0;
      If FEditor.Focused Then
        SelStart := FEditor.SelStart;
      //LockWindowUpdate( FEditor.Handle );
      FEditor.Lines.BeginUpdate;
      FEditor.Lines.LoadFromStream( aMem );    
      FEditor.Lines.EndUpdate;
      If FEditor.Focused Then
        FEditor.SelStart := SelStart;
      //LockWindowUpdate( 0 );
    Finally
      aMem.Free;
      FChanging := False;
    End;
  End;

  { converts a Delphi TColor into a RTF-color table string }
  Function ColorToRtf( aColor: TColor ): String;
  Begin
    aColor := ColorToRGB( aColor );
    Result := '\red' + IntToStr( GetRValue( aColor ) ) +
      '\green' + IntToStr( GetGValue( aColor ) ) +
      '\blue' + IntToStr( GetBValue( aColor ) ) + ';';
  End;

Begin
{$IFDEF XQDEMO}
  If Not IsDelphiRunning Then
  Begin
    ShowAbout;
    Raise Exception.Create( SDelphiIsNotRunning );
  End;
{$ENDIF}
  If Not Assigned( FEditor ) Or ( csDestroying In ComponentState ) Then
    Exit;
  s := FEditor.Text + ' ';
  inputStream := TMemoryStream.create;
  inputStream.WriteBuffer( Pointer( s )^, Length( s ) );
  inputStream.Seek( 0, 0 );
  outputStream := TMemoryStream.create;
  errorStream := TMemoryStream.create;
  lexer := TXQLexer.Create;
  lexer.yyinput := inputStream;
  lexer.yyoutput := outputStream;
  lexer.yyerrorfile := errorStream;
  If Assigned( FXQuery ) Then
    ( lexer As TXQlexer ).DateFormat := FXQuery.DateFormat
  Else
    ( lexer As TXQlexer ).DateFormat := ShortDateFormat;//SDefaultDateFormat;

  RtfHeader :=
    '{\rtf1\ansi\deff0\deftab720' +
    '{\fonttbl' +
    //format('{\f0\\fcharset0\fprq2\f%s %s;}}',[FFontFamily,FEditor.Font.Name])+
  '{\f0\fswiss Arial;}' +
    format( '{\f1\f%s %s;}', [FFontFamily, FEditor.Font.Name] ) +
    //format('{\f0\\fcharset0\fprq2\fcharset186 %s;}}',[FEditor.Font.Name])+
  '}{\colortbl;';
  // the default color
  RtfHeader := RtfHeader + ColorToRtf( FEditor.Font.Color ); // foreground
  RtfHeader := RtfHeader + ColorToRtf( FEditor.Color ); // background
  // Create a table of colors specified
  For I := 0 To FColorConfig.ColorTable.Count - 1 Do
    RtfHeader := RtfHeader + ColorToRtf( TColor( FColorConfig.ColorTable[I] ) );
  RtfHeader := RtfHeader + '}' +
    format( '\deflang1031\pard\plain\f1\fs%d', [FEditor.Font.Size * 2] );

  Reslt := ''; // resulting rtf string

  yychar:= 0;
  Try
    Repeat
      Try
        Lexer.IgnoreBadDates := True;
        yychar := Lexer.yylex;
      Except
        { ignore syntax errors }
      End;
      If yychar < 0 Then
        yychar := 0;
      If yychar = 0 Then
        break; // normal termination
      lexer.GetyyText (atext);
      If yychar = _ILLEGAL Then
      Begin
        // illegal token on input
        Reslt := Reslt + '{\cb2\cf1\b0\i0\ul0';
        lexer.GetyyText (atext);
        ReplaceString( atext, #10, '\line ' );
        ReplaceString( atext, #13, '' );
        Reslt := Reslt + #32 + atext + '}';
      End
      Else
      Begin
        // it is a table name or database field coming from FXQuery property?
        If Assigned( FXQuery ) And ( yychar = _IDENTIFIER ) And
          ( Not Lexer.IsKeyword( atext, token ) ) Then
        Begin
          For I := 0 To TCustomXQueryClass( FxQuery ).DataSets.Count - 1 Do
          Begin
            If AnsiCompareText( TCustomXQueryClass( FxQuery ).DataSets[I].Alias, atext ) = 0 Then
            Begin
              yychar := _TABLE;
              Break;
            End;
            DataSet := TCustomXQueryClass( FxQuery ).DataSets[I].DataSet;
            If Assigned( DataSet ) And DataSet.Active Then
            Begin
              lexer.getyytext(ayytext);
              Field := DataSet.FindField( ayytext );
              If Assigned( Field ) Then
              Begin
                yychar := _FIELD;
                Break;
              End;
            End;
          End;
        End;
        If FColorConfig.findconfig( yychar, forecolor, backcolor, fontstyle ) Then
        Else
        Begin
          forecolor := clBlack;
          backcolor := clWhite;
          fontstyle := [];
        End;
        Reslt := Reslt + Format( '{\cb%d\cf%d', [FColorConfig.IndexOfColor( backcolor ) + 3,
          FColorConfig.IndexOfColor( forecolor ) + 3] );
        // the font style
        If fsBold In fontstyle Then
          Reslt := Reslt + '\b'
        Else
          Reslt := Reslt + '\b0';
        If fsItalic In fontstyle Then
          Reslt := Reslt + '\i'
        Else
          Reslt := Reslt + '\i0';
        If fsUnderline In fontstyle Then
          Reslt := Reslt + '\ul'
        Else
          Reslt := Reslt + '\ul0';
        Case yychar Of
          _NEWLINE:
            Reslt := Reslt + #32 + '\line}';
          _TAB:
            Reslt := Reslt + #32 + '\tab}';
          _BLANK:
            Reslt := Reslt + #32 + ' }';
        Else
          Begin

            ReplaceString( atext, #10, '\line ' );
            ReplaceString( atext, #13, '' );
            Reslt := Reslt + #32 + atext + '}';
          End;
        End;
      End;
    Until false;
    Reslt := RtfHeader + Reslt + '}'; //+ '\cb2\cf1\b0\i0\ul0}';
    strToRichEdit( Reslt );
    {FEditor.DefAttributes.Assign(FEditor.Font);}
    //FEditor.SelAttributes.Assign(FEditor.Font);
    FInternalModified := False;
  Finally
    lexer.free;
    inputStream.free;
    outputStream.free;
    errorStream.free;
  End;
End;

Procedure TSyntaxHighlighter.MyOnChange( Sender: TObject );
Begin
  If FChanging Or ( csDesigning In ComponentState ) Or Not Assigned( FEditor ) Then
    Exit;
  FInternalModified := True;
  If Not FEditor.Focused Or ( FUpdateMode = umCharacter ) Then
    Execute;
  If Assigned( FSaveOnChange ) Then
    FSaveOnChange( FEditor );
End;

Procedure TSyntaxHighlighter.MyOnSelectionChange( Sender: TObject );
Var
  CharPos: TPoint;
Begin
  If FChanging Or Not Assigned( FEditor ) Then
    Exit;
  CharPos.Y := SendMessage( FEditor.Handle, EM_EXLINEFROMCHAR, 0,
    FEditor.SelStart );
  CharPos.X := ( FEditor.SelStart -
    SendMessage( FEditor.Handle, EM_LINEINDEX, CharPos.Y, 0 ) );
  Inc( CharPos.Y );
  Inc( CharPos.X );
  If ( FUpdateMode = umLine ) And FInternalModified And ( CharPos.Y <> FLastLine ) Then
  Begin
    FLastLine := CharPos.Y;
    FChanging := True;
    Execute;
    FChanging := False;
  End;
  If Assigned( FOnPosChange ) Then
    FOnPosChange( Self, CharPos.Y, CharPos.X );
  If Assigned( FSaveOnSelectionChange ) Then
    FSaveOnSelectionChange( FEditor );
End;

Procedure TSyntaxHighlighter.FontChanged;
Var
  ControlCanvas: TControlCanvas;
  FontInfo: TTextMetric; // holds the font metric information
Begin
  If FChanging Or Not Assigned( FEditor ) Then
    Exit;
  // calculate font family
  ControlCanvas := TControlCanvas.Create;
  Try
    ControlCanvas.Control := FEditor;
    ControlCanvas.Font.Assign( FEditor.Font );
    GetTextMetrics( ControlCanvas.Handle, FontInfo );
  Finally
    ControlCanvas.Free;
  End;

  {Get the font family}
  FFontFamily := 'swiss';
  Case ( FontInfo.tmPitchAndFamily And $F0 ) Of
    FF_DECORATIVE: FFontFamily := 'decor';
    FF_DONTCARE: FFontFamily := 'swiss'; // actually 'nil'
    FF_MODERN: FFontFamily := 'modern';
    FF_ROMAN: FFontFamily := 'roman';

    FF_SCRIPT: FFontFamily := 'script';
    FF_SWISS: FFontFamily := 'swiss';
  End;

  Execute;

End;

Procedure TSyntaxHighlighter.MyOnExit( Sender: TObject );
Begin
  If Not Assigned( FEditor ) Then
    Exit;
  If ( FUpdateMode = umLine ) And FInternalModified Then
  Begin
    Execute;
  End;
  If Assigned( FSaveOnExit ) Then
    FSaveOnExit( FEditor );
End;

// TColorConfig - class implementation

Constructor TColorConfig.Create;
Var
  ColorElement: PColorElement;
  i: Integer;
Begin
  Inherited Create;
  FColorSettings := TList.Create;
  // default values

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _IDENTIFIER ) );
    Group := idIdentifier;
    ForeColor := clBlue;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _TABLE ) );
    Group := idTable;
    ForeColor := clFuchsia;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _FIELD ) );
    Group := idField;
    ForeColor := clTeal;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    //Elements.Add( Pointer( _SINTEGER ) );
    Elements.Add( Pointer( _UINTEGER ) );
    Elements.Add( Pointer( _NUMERIC ) );
    Group := idNumber;
    ForeColor := clFuchsia;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _STRING ) );
    Group := idString;
    ForeColor := clPurple;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _COMA ) );
    Group := idComma;
    ForeColor := clRed;
    BackColor := clWhite;
    FontStyle := [fsBold];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _LPAREN ) );
    Elements.Add( Pointer( _RPAREN ) );
    Group := idParenthesis;
    ForeColor := clRed;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    With Elements Do
    Begin
      Add( Pointer( _GT ) );
      Add( Pointer( _LT ) );
      Add( Pointer( _EQ ) );
      Add( Pointer( _MULT ) );
      Add( Pointer( _PLUS ) );
      Add( Pointer( _SUB ) );
      Add( Pointer( _DIV ) );
      Add( Pointer( _NEQ ) );
      Add( Pointer( _GE ) );
      Add( Pointer( _LE ) );
    End;
    Group := idOperator;
    ForeColor := clRed;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _PERIOD ) );
    Group := idPeriod;
    ForeColor := clRed;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _SEMICOLON ) );
    Elements.Add( Pointer( _COLON ) );
    Group := idSemicolon;
    ForeColor := clFuchsia;
    BackColor := clWhite;
    FontStyle := [fsBold];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _COMMENT ) );
    Group := idComment;
    ForeColor := clGray;
    BackColor := clWhite;
    FontStyle := [fsItalic];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    Elements.Add( Pointer( _BLANK ) );
    Elements.Add( Pointer( _TAB ) );
    Elements.Add( Pointer( _NEWLINE ) );
    Group := idWhiteSpace;
    ForeColor := clWhite;
    BackColor := clWhite;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  New( ColorElement );
  With ColorElement^ Do
  Begin
    Elements := TList.Create;
    For i := Low( rwords ) To High( rwords ) Do
      Elements.Add( Pointer( rwords[i].token ) );
    Group := idReservedWord;
    ForeColor := clGreen;
    BackColor := clBlack;
    FontStyle := [];
  End;
  FColorSettings.Add( ColorElement );

  FColorTable := TList.Create;

End;

Destructor TColorConfig.Destroy;
Begin
  Clear;
  FColorSettings.Free;
  FColorTable.Free;
  Inherited Destroy;
End;

// color table used in creating an Rtf file

Procedure TColorConfig.CreateColorTable;
Var
  I, Index: Integer;
Begin
  FColorTable.Clear;
  For I := 0 To FColorSettings.Count - 1 Do
  Begin
    With PColorElement( FColorSettings[I] )^ Do
    Begin
      Index := FColorTable.IndexOf( Pointer( ForeColor ) );
      If Index = -1 Then
        FColorTable.Add( Pointer( ForeColor ) );
      Index := FColorTable.IndexOf( Pointer( BackColor ) );
      If Index = -1 Then
        FColorTable.Add( Pointer( BackColor ) );
    End;
  End;
End;

Function TColorConfig.IndexOfColor( Color: TColor ): Integer;
Begin
  Result := FColorTable.IndexOf( Pointer( Color ) );
End;

Procedure TColorConfig.Clear;
Var
  I: Integer;
  ColorElement: PColorElement;
Begin
  For I := 0 To FColorSettings.Count - 1 Do
  Begin
    ColorElement := PColorElement( FColorSettings[I] );
    ColorElement^.Elements.Free;
    Dispose( ColorElement );
  End;
  FColorSettings.Clear;
End;

Procedure TColorConfig.SaveToFile( Const FileName: String );
Var
  Stream: TStream;
  i, j, n, ne, e: Integer;
  ColorElement: PColorElement;
Begin
  Stream := TFileStream.Create( FileName, fmCreate );
  Try
    n := FColorSettings.Count;
    Stream.Write( n, sizeof( n ) );
    For I := 0 To n - 1 Do
    Begin
      ColorElement := PColorElement( FColorSettings[I] );
      Stream.Write( ColorElement^, SizeOf( TColorElement ) );
      ne := ColorElement^.Elements.Count;
      Stream.Write( ne, sizeof( ne ) );
      For j := 0 To ne - 1 Do
      Begin
        e := LongInt( ColorElement^.Elements[j] );
        Stream.Write( e, SizeOf( e ) );
      End;
    End;
  Finally
    Stream.Free;
  End;
End;

Procedure TColorConfig.LoadFromFile( Const FileName: String );
Var
  Stream: TStream;
  i, j, n, ne, e: Integer;
  ColorElement: PColorElement;
Begin
  If Not FileExists( FileName ) Then
    Exit;
  Clear;
  Stream := TFileStream.Create( FileName, fmOpenRead Or fmShareDenyNone );
  Try
    Stream.Read( n, sizeof( n ) );
    For I := 0 To n - 1 Do
    Begin
      New( ColorElement );
      Stream.Read( ColorElement^, SizeOf( TColorElement ) );
      ColorElement^.Elements := TList.Create;
      Stream.Read( ne, sizeof( ne ) );
      For j := 0 To ne - 1 Do
      Begin
        Stream.Read( e, SizeOf( e ) );
        ColorElement^.Elements.Add( Pointer( e ) );
      End;
      FColorSettings.Add( ColorElement );
    End;
  Finally
    Stream.Free;
  End;
End;

Procedure TColorConfig.SetColorElement( Group: TElementGroup; ForeColor, BackColor: TColor;
  FontStyle: TFontStyles );
Var
  i: Integer;
  ColorElement: PColorElement;
Begin
  For i := 0 To FColorSettings.Count - 1 Do
  Begin
    ColorElement := PColorElement( FColorSettings[i] );
    If ColorElement^.Group = Group Then
    Begin
      ColorElement^.ForeColor := ForeColor;
      ColorElement^.BackColor := BackColor;
      ColorElement^.FontStyle := FontStyle;
      Break;
    End;
  End;
End;

Function TColorConfig.FindConfig( Element: Integer; Var ForeColor, BackColor: TColor;
  Var FontStyle: TFontStyles ): Boolean;
Var
  i, j: Integer;
  ColorElement: PColorElement;
Begin
  Result := False;
  For i := 0 To FColorSettings.Count - 1 Do
  Begin
    ColorElement := PColorElement( FColorSettings[i] );
    For j := 0 To ColorElement^.Elements.Count - 1 Do
      If Longint( ColorElement^.Elements[j] ) = Element Then
      Begin
        ForeColor := ColorElement^.ForeColor;
        BackColor := ColorElement^.BackColor;
        FontStyle := ColorElement^.FontStyle;
        Result := True;
        Exit;
      End;
  End;
End;

{$IFNDEF BCB}

Procedure TColorConfig.EditColorSettings;
Begin
  With TfrmColorSettings.Create( Application ) Do
  Begin
    Try
      Enter( Self );
    Finally
      Free;
    End;
  End;
End;
{$ENDIF}

Function TColorConfig.Get( Index: Integer ): TColorElement;
Begin
  If ( Index < 0 ) Or ( Index > FColorSettings.Count - 1 ) Then
    Exit;
  Result := PColorElement( FColorSettings[Index] )^;
End;

Procedure TColorConfig.Put( Index: Integer; Const Value: TColorElement );
Begin
  If ( Index < 0 ) Or ( Index > FColorSettings.Count - 1 ) Then
    Exit;
  PColorElement( FColorSettings[Index] )^ := Value;
End;

Function TColorConfig.Count: Integer;
Begin
  Result := FColorSettings.Count;
End;

Procedure TColorConfig.Assign( Value: TColorConfig );
Var
  ColorElement: PColorElement;
  i, j: Integer;
  TmpList: TList;
Begin
  Clear;
  For i := 0 To Value.FColorSettings.Count - 1 Do
  Begin
    New( ColorElement );
    ColorElement^ := PColorElement( Value.FColorSettings[I] )^;
    TmpList := ColorElement^.Elements;
    ColorElement^.Elements := TList.Create;
    For j := 0 To TmpList.Count - 1 Do
      ColorElement^.Elements.Add( TmpList[j] );
    FColorSettings.Add( ColorElement );
  End;
End;

Function TColorConfig.IndexOfGroup( Group: TElementGroup ): Integer;
Var
  I: Integer;
Begin
  Result := -1;
  For I := 0 To FColorSettings.Count - 1 Do
    If PColorElement( FColorSettings[I] )^.Group = Group Then
    Begin
      Result := I;
      Exit;
    End;
End;

End.
