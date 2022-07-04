{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit DBUCell;

interface

uses
  Classes,
{$ifndef LINUX}
  Windows, Grids, Controls,
{$else LINUX}
  Types, Qt, QGraphics, QGrids, QControls,
{$endif LINUX}
  DataType, DBUGrid, DBUTypes, DBUCellTypes, DBUInterfaces, DBUFormatter;

type
  TRejectParams = record
    ShowDialog : Boolean;
    YesNoChoise : Boolean;
    AllowRestore : Boolean;
    Msg : String;
  end;

  TDBUCell = class;
  TDBUEditorCell = class;

  TOnGetValueEvent = procedure ( Sender : IInfoCell; var AValue : TValue; var ADataType : TDataType ) of object;
  TOnSetValueEvent = function ( Sender : TDBUEditorCell; AValue : TValue ) : Boolean of object;
  TOnGetFormatterEvent = procedure ( Sender : IInfoCell; Formatter : TDBUFormatter; DrawState: TGridDrawState ) of object;
  TOnGetCellTypeEvent = procedure ( Sender : IInfoCell; var CellType : TDBUCellType ) of object;
  TOnCreateEditCustomizerEvent = function ( Sender : TDBUEditorCell; CellType : TDBUCellType ) : TDBUCustomizer of object;
  TOnCreateDrawCustomizerEvent = function ( Sender : TDBUCell; CellType : TDBUCellType ) : TDBUCustomizer of object;
  TOnGetReadOnlyEvent = function ( Sender : IInfoCell ) : Boolean of object;

  TCustomizerList = class
  private
    FIdentifiers : TList;
    FCustomizers : TList;

    function GetCustomizer(Identifier : Pointer): TDBUCustomizer;
    procedure SetCustomizer(Identifier : Pointer;
      const AValue: TDBUCustomizer);
  public
    constructor Create;
    destructor Destroy; override;

    property Customizer[Identifier : Pointer] : TDBUCustomizer read GetCustomizer write SetCustomizer;
  end;

  TDBUCellClass = class of TDBUCell;

  TDBUCell = class(TDBUCustomGridCell, IInfoCell, IGridCell, IGridIterator)
  private
    FCustomizerList : TCustomizerList;

    // Reset when changing cells
    FCustomizer : TDBUCustomizer;
    FCustomizerUpToDate : Boolean;
    FValueUpToDate : Boolean;
    FValue : TValue;
    FReadOnlyUpToDate : Boolean;
    FFormatterUpToDate : Boolean;

    FUniqueIterators : TList;
    FIterator : TDBUCell;
    FIsIterator : Boolean;

    FOnGetValue : TOnGetValueEvent;
    FOnGetFormatter : TOnGetFormatterEvent;
    FOnGetCellType : TOnGetCellTypeEvent;
    FOnGetReadOnly: TOnGetReadOnlyEvent;
    FOnCreateDrawCustomizer: TOnCreateDrawCustomizerEvent;

    function GetGrid : IInfoGrid;
    procedure SetOnGetValue(const AValue: TOnGetValueEvent);
    procedure SetOnGetFormatter(const AValue: TOnGetFormatterEvent);
    procedure SetOnGetCellType(const AValue: TOnGetCellTypeEvent);
    procedure SetOnGetReadOnly(const AValue: TOnGetReadOnlyEvent);

    function GetValue : TValue;
    procedure SetValue(const AValue: TValue);
    procedure SetOnCreateDrawCustomizer(
      const Value: TOnCreateDrawCustomizerEvent);

    function GetGridValue : TValue;
    procedure ChangeCell( ACol, ARow : Integer );
    function SelfObject : TObject;
//    function DataType: TDataType;
  protected
    FCellType : TDBUCellType;
    FDataType : TDataType;

    function DoGetValue : TValue; virtual;
    function GetDrawValue : TValue; virtual;

    procedure DrawCell( var Params : TDrawParams ); override;
    procedure GetCell(ACol, ARow: Integer); override;
    procedure DefineCellType; virtual;
    procedure ResetCaches; override;
    procedure ResetValues; override;

    function GetFormatter(DrawState: TGridDrawState): TDBUFormatter; override;
    function GetCustomizerIdentifier : Pointer; virtual;
    function GetCustomizer : TDBUCustomizer; virtual;

    procedure KeyDown(var NewKeyState : TKeyState); override;
    procedure KeyUp(var NewKeyState : TKeyState); override;

    procedure MouseDown(NewMouseState : TMouseState); override;
    procedure MouseMove(NewMouseState : TMouseMoveState); override;
    procedure MouseUp(NewMouseState : TMouseState); override;

    function ShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    procedure InitIterator( Iterator : TDBUCell ); virtual;

    function GetReadOnly: Boolean; override;
    procedure SetReadOnly( const AValue: Boolean ); override;

    function GetDataType: TDataType; virtual;
    procedure SetDataType(const Value: TDataType); virtual;

    property CustomizerIdentifier : Pointer read GetCustomizerIdentifier;
  published
    property OnGetValue : TOnGetValueEvent read FOnGetValue write SetOnGetValue;
    property OnGetFormatter : TOnGetFormatterEvent read FOnGetFormatter write SetOnGetFormatter;
    property OnGetCellType : TOnGetCellTypeEvent read FOnGetCellType write SetOnGetCellType;
    property OnGetReadOnly : TOnGetReadOnlyEvent read FOnGetReadOnly write SetOnGetReadOnly;
    property OnCreateDrawCustomizer : TOnCreateDrawCustomizerEvent read FOnCreateDrawCustomizer write SetOnCreateDrawCustomizer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    function CreateIterator(Unique : Boolean = False) : IGridIterator;

    property CellType : TDBUCellType read FCellType;
    property Value : TValue read GetValue write SetValue;
    property DataType : TDataType read GetDataType write SetDataType;
  end;

  TDBUEditorCellClass = class of TDBUEditorCell;

  TDBUEditorCell = class(TDBUCustomEditorCell, IInfoCell, IEditorCell, IEditorIterator)
  private
    FCustomizerList : TCustomizerList;

    // Reset when changing cells
    FCustomizer : TDBUCustomizer;
    FCustomizerUpToDate : Boolean;
    FValue : TValue;
    FValueUpToDate : Boolean;
    FEditorValue : TValue;
    FEditorValueUpToDate : Boolean;
    FReadOnly : Boolean;
    FReadOnlyUpToDate : Boolean;
    FFormatterUpToDate : Boolean;
    FHandleError : Boolean;

    FUniqueIterators : TList;
    FIterator : TDBUEditorCell;
    FIsIterator : Boolean;

    FOnGetValue: TOnGetValueEvent;
    FOnSetValue: TOnSetValueEvent;
    FOnGetFormatter: TOnGetFormatterEvent;
    FOnCreateEditCustomizer : TOnCreateEditCustomizerEvent;
    FOnGetCellType : TOnGetCellTypeEvent;
    FOnGetReadOnly: TOnGetReadOnlyEvent;
    FRestoreForIllegalValues : Boolean;
    FAbortOnIllegalValues: Boolean;

    procedure SetCellType(const AValue: TDBUCellType);
    procedure SetOnGetValue(const AValue: TOnGetValueEvent);
    procedure SetOnSetValue(const AValue: TOnSetValueEvent);

    function GetEditor : IInfoEditor;
    function GetGrid : IInfoGrid;
    procedure SetOnGetFormatter(const AValue: TOnGetFormatterEvent);
    procedure SetOnCreateEditCustomizer(
      const AValue: TOnCreateEditCustomizerEvent);
    procedure SetOnGetCellType(const AValue: TOnGetCellTypeEvent);
    procedure SetOnGetReadOnly(const AValue: TOnGetReadOnlyEvent);
    function GetEditorMode: Boolean;
    procedure SetEditorMode(AValue: Boolean);

    function GetValue : TValue;
    procedure SetValue(const AValue : TValue);
    function SetValueForCell(const AValue: TValue; var RejectParams : TRejectParams): Boolean;
    function GetInplaceEditText : String;
    procedure SetInplaceEditText( const Value : String );
    function GetGridValue : TValue;
    procedure SetGridValue(const AValue : TValue);
    procedure ChangeCell( ACol, ARow : Integer );
    function SelfObject : TObject;
//    function DataType: TDataType;
  protected
    FCellType : TDBUCellType;
    FDataType : TDataType;

    function DoGetValue : TValue; virtual;
    function GetDrawValue : TValue; virtual;
    function DoSetValue(const AValue : TValue; var RejectParams : TRejectParams) : Boolean; virtual;
    function ValueChanged( aValue : TValue ) : Boolean; virtual;

    function GetFormatter(DrawState: TGridDrawState): TDBUFormatter; override;
    function GetCustomizerIdentifier : Pointer; virtual;
    function GetCustomizer : TDBUCustomizer; virtual;

    // Perverted code in order to cover windows messages
    procedure DrawEditor( var Params : TDrawParams ); override;
    procedure HandleMessage( var Params : THandleWMParams ); override;
    procedure HandleCancelMode( var Params : THandleCMParams ); override;
    procedure GetCursor( APoint : TPoint; var ACursor : TCursor ); override;
    procedure ResetValues; override;
    procedure DoUndoChanges; override;

    procedure KeyDown(var NewKeyState : TKeyState); override;
    procedure KeyUp(var NewKeyState : TKeyState); override;

    procedure MouseDown(NewMouseState : TMouseState); override;
    procedure MouseMove(NewMouseState : TMouseMoveState); override;
    procedure MouseUp(NewMouseState : TMouseState); override;

    procedure GetCell(ACol, ARow: Integer); override;
    procedure DefineCellType; virtual;

    function ParseValue( SrcValue : TValue; var ParsedValue : TValue; var RejectParams : TRejectParams ) : Boolean; virtual;
    function SetCell : Boolean; override;

    function DoGetEditText : String; override;
    procedure SetEditText(const AValue: string); override;

    // Get the value to show in the InplaceEdit
    function GetEditorValue : TValue;
    // Set the value to show in the InplaceEdit to AValue
    procedure SetEditorValue( const AValue : TValue ); virtual;
    // Get the key value for this cell
    function GetEditorKeyValue : TValue; virtual;

    function GetDataType: TDataType; virtual;
    procedure SetDataType(const Value: TDataType); virtual;

    function GetReadOnly: Boolean;  override;
    procedure SetReadOnly( const AValue: Boolean ); override;

    procedure HandleSetCellError(RejectParams : TRejectParams);
    procedure InitIterator( Iterator : TDBUEditorCell ); virtual;

    property CellType : TDBUCellType read FCellType write SetCellType;
    property EditorValue : TValue read GetEditorValue write SetEditorValue;
    property CustomizerIdentifier : Pointer read GetCustomizerIdentifier;
  published
    property OnGetValue : TOnGetValueEvent read FOnGetValue write SetOnGetValue;
    property OnSetValue : TOnSetValueEvent read FOnSetValue write SetOnSetValue;
    property OnGetFormatter : TOnGetFormatterEvent read FOnGetFormatter write SetOnGetFormatter;
    property OnGetCellType : TOnGetCellTypeEvent read FOnGetCellType write SetOnGetCellType;
    property OnCreateEditCustomizer : TOnCreateEditCustomizerEvent read FOnCreateEditCustomizer write SetOnCreateEditCustomizer;
    property OnGetReadOnly : TOnGetReadOnlyEvent read FOnGetReadOnly write SetOnGetReadOnly;

    property RestoreForIllegalValues : Boolean read FRestoreForIllegalValues write FRestoreForIllegalValues;
    property AbortOnIllegalValues : Boolean read FAbortOnIllegalValues write FAbortOnIllegalValues;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    function CreateIterator(Unique : Boolean = False) : IEditorIterator;
    property Value : TValue read GetValue write SetValue;
    property DataType : TDataType read GetDataType write SetDataType;
  end;

implementation

uses
  CommonLib, DataTranslations,
{$ifndef LINUX}
  Dialogs,
{$else LINUX}
  QDialogs,
{$endif LINUX}
  SysUtils;

{ TDBUCell }

constructor TDBUCell.Create;
begin
  inherited Create;

  FCustomizerList := TCustomizerList.Create;
  FValueUpToDate := False;
  FIsIterator := False;
  FUniqueIterators := TList.Create;
  FIterator := nil;
end;

destructor TDBUCell.Destroy;
begin
  if FIsIterator then
    FFormatter := nil;
    
  FIterator.Free;
  FreeListWithObjects(FUniqueIterators);
  FCustomizerList.Free;
  inherited;
end;

function TDBUCell.SelfObject : TObject;
begin
  Result := Self;
end;

procedure TDBUCell.DrawCell( var Params : TDrawParams );
begin
  Reset;
  GetCell( Params.Col, Params.Row );

  if Assigned( CellType ) then
    CellType.DrawCell( Self, Params );
end;

function TDBUCell.DoGetValue: TValue;
begin
  if Assigned( OnGetValue ) then
    OnGetValue( Self, Result, FDataType )
  else
    Result := EmptyString;
end;

function TDBUCell.GetValue : TValue;
begin
  if not FValueUpToDate then
  begin
    FValue := DoGetValue;
    FValueUpToDate := True;
  end;

  Result := FValue;
end;

procedure TDBUCell.GetCell(ACol, ARow: Integer);
begin
  if (ACol = Col) and
     (ARow = Row) then
    Exit;

  Reset;
  inherited;

  DefineCellType;
end;

function TDBUCell.GetGrid: IInfoGrid;
begin
  Result := Grid;
end;

procedure TDBUCell.KeyDown(var NewKeyState: TKeyState);
begin
  GetCell( NewKeyState.Col, NewKeyState.Row );

  if Assigned( CellType ) then
    CellType.KeyDown( Self, NewKeyState );

  inherited;
end;

procedure TDBUCell.KeyUp(var NewKeyState: TKeyState);
begin
  GetCell( NewKeyState.Col, NewKeyState.Row );

  if Assigned( CellType ) then
    CellType.KeyUp( Self, NewKeyState );

  inherited;
end;

procedure TDBUCell.MouseDown(NewMouseState: TMouseState);
begin
  GetCell( NewMouseState.Col, NewMouseState.Row );

  if Assigned( CellType ) then
    CellType.MouseDown( Self, NewMouseState );

  inherited;
end;

procedure TDBUCell.MouseMove(NewMouseState: TMouseMoveState);
begin
  if GetMouseBtnStates <> [] then
    GetCell( MouseStateDown.Col, MouseStateDown.Row )
  else
    GetCell( NewMouseState.Col, NewMouseState.Row );

  if Assigned( CellType ) then
    CellType.MouseMove( Self, NewMouseState );

  inherited;
end;

procedure TDBUCell.MouseUp(NewMouseState: TMouseState);
begin
//  if GetMouseBtnStates <> [] then
    GetCell( MouseStateDown.Col, MouseStateDown.Row );
//  else
//    GetCell( NewMouseState.Col, NewMouseState.Row );

  if Assigned( CellType ) then
    CellType.MouseUp( Self, NewMouseState );

  inherited;
end;

procedure TDBUCell.SetOnGetValue(const AValue: TOnGetValueEvent);
begin
  FOnGetValue := AValue;
end;

function TDBUCell.GetFormatter(DrawState: TGridDrawState): TDBUFormatter;
begin
  if not FFormatterUpToDate and
     Assigned( OnGetFormatter ) then
  begin
    FFormatterUpToDate := True;
    OnGetFormatter( Self, Formatter, DrawState );
  end;
  Result := Formatter;
end;

procedure TDBUCell.SetOnGetFormatter(const AValue: TOnGetFormatterEvent);
begin
  FOnGetFormatter := AValue;
end;

procedure TDBUCell.SetOnGetCellType(const AValue: TOnGetCellTypeEvent);
begin
  FOnGetCellType := AValue;
end;

procedure TDBUCell.Reset;
begin
  inherited;

//  FValue := EmptyString;
  FValueUpToDate := False;
  FReadOnlyUpToDate := False;
  FDataType := nil;
end;

procedure TDBUCell.ResetCaches;
begin
  inherited;

  FCustomizerUpToDate := False;
  FCustomizer := nil;
  FFormatterUpToDate := False;
end;

procedure TDBUCell.SetOnGetReadOnly(const AValue: TOnGetReadOnlyEvent);
begin
  FOnGetReadOnly := AValue;
end;

function TDBUCell.ShowEditor(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
var
  HitInfo : TGridCoord;
begin
  HitInfo := Grid.MouseCoord(X, Y);
  GetCell(HitInfo.X, HitInfo.Y);
  if Assigned(CellType) then
    Result := CellType.ShowEditor(Self, Button, Shift, X, Y)
  else
    Result := False;
end;

function TDBUCell.GetReadOnly: Boolean;
begin
  if not FReadOnlyUpToDate then
  begin
    FReadOnlyUpToDate := True;
    if not Assigned( CellType ) or
       CellType.ReadOnly then
      ReadOnly := True
    else if Assigned( OnGetReadOnly ) then
      ReadOnly := OnGetReadOnly( Self )
    else
      ReadOnly := False;
  end;

  Result := inherited GetReadOnly
end;

procedure TDBUCell.SetValue(const AValue: TValue);
begin
  FValue := AValue;
  FValueUpToDate := True;
end;

procedure TDBUCell.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FReadOnlyUpToDate := True;
end;

procedure TDBUCell.DefineCellType;
begin
  if Assigned( Grid ) and
     ( (Col < Grid.FixedCols) or
       (Row < Grid.FixedRows) ) then
    FCellType := HeaderCellType
  else
    FCellType := NormalCellType;

  if Assigned( OnGetCellType ) then
    OnGetCellType( Self, FCellType )
end;

function TDBUCell.GetCustomizerIdentifier : Pointer;
begin
  Result := CellType;
end;

function TDBUCell.GetCustomizer: TDBUCustomizer;
var
  Identifier : Pointer;
begin
  if not FCustomizerUpToDate then
  begin
    Identifier := CustomizerIdentifier;
    FCustomizer := FCustomizerList.Customizer[Identifier];
    if FCustomizer = nil then
    begin
      if Assigned( OnCreateDrawCustomizer ) then
        FCustomizer := OnCreateDrawCustomizer( Self, CellType )
      else
        FCustomizer := CellType.CreateDrawCustomizer( Self );

      FCustomizerList.Customizer[Identifier] := FCustomizer;
    end;

    FCustomizerUpToDate := True;
  end;

  Result := FCustomizer;
end;

procedure TDBUCell.SetOnCreateDrawCustomizer(
  const Value: TOnCreateDrawCustomizerEvent);
begin
  FOnCreateDrawCustomizer := Value;
end;

function TDBUCell.CreateIterator(Unique : Boolean = False) : IGridIterator;
var
  ACell : TDBUCell;
begin
  if Unique or not Assigned(FIterator) then
  begin
    ACell := TDBUCellClass( Self.ClassType ).Create;
    ACell.FIsIterator := True;
    ACell.SetFormatter( Grid.CreateFormatter );
    InitIterator(ACell);
    if Unique then
      FUniqueIterators.Add(ACell)
    else
      FIterator := ACell;
  end
  else
    ACell := FIterator;
    
  Result := ACell;
end;

procedure TDBUCell.InitIterator(Iterator: TDBUCell);
var
  c : TDBUCell;
begin
  c := Iterator;
  c.FOnGetValue := FOnGetValue;
  c.FOnGetFormatter := FOnGetFormatter;
  c.FOnGetCellType := FOnGetCellType;
  c.FOnGetReadOnly := FOnGetReadOnly;
end;

procedure TDBUCell.ChangeCell(ACol, ARow: Integer);
begin
  GetCell( ACol, ARow );
end;

function TDBUCell.GetGridValue: TValue;
begin
  Result := GetDrawValue;
end;

function TDBUCell.GetDrawValue: TValue;
begin
  Result := Formatter.FormatValue( Value, DataType );
end;

(*function TDBUCell.DataType: TDataType;
begin
  Result := Value.DataType;
end;
*)
procedure TDBUCell.ResetValues;
begin
  inherited;

  FValueUpToDate := False;
  FDataType := nil;
end;

function TDBUCell.GetDataType: TDataType;
begin
  // The FDataType field is initialized in GetValue
  Value;
  if not Assigned(FDataType) then
    FDataType := Value.DataType;

  Result := FDataType;
end;

procedure TDBUCell.SetDataType(const Value: TDataType);
begin
  FDataType := Value;
end;

{ TDBUEditorCell }

constructor TDBUEditorCell.Create;
begin
  FRestoreForIllegalValues := False;
  FAbortOnIllegalValues := True;
  FHandleError := False;
  inherited;

  FCustomizerList := TCustomizerList.Create;
  FCustomizerUpToDate := False;
  FValueUpToDate := False;
  FEditorValueUpToDate := False;
  FIsIterator := False;
  FUniqueIterators := TList.Create;
  FIterator := nil;
end;

destructor TDBUEditorCell.Destroy;
begin
  if FIsIterator then
    FFormatter := nil;
  FIterator.Free;
  FreeListWithObjects(FUniqueIterators);

  inherited;

  FCustomizerList.Free;
end;                                                 

function TDBUEditorCell.SelfObject : TObject;
begin
  Result := Self;
end;

procedure TDBUEditorCell.DrawEditor( var Params : TDrawParams );
begin
  GetCell( Params.Col, Params.Row );
  CellType.DrawEditor( Self, Params );
end;

procedure TDBUEditorCell.GetCell(ACol, ARow: Integer);
begin
  if (ACol = Col) and
     (ARow = Row) then
    Exit;

  Reset;
  inherited;

  DefineCellType;
end;

procedure TDBUEditorCell.GetCursor(APoint: TPoint; var ACursor: TCursor);
begin
  CellType.GetCursor( Self, APoint, ACursor );
end;

function TDBUEditorCell.GetCustomizerIdentifier: Pointer;
begin
  Result := CellType;
end;

function TDBUEditorCell.GetCustomizer: TDBUCustomizer;
var
  Identifier : Pointer;
begin
  if not FCustomizerUpToDate then
  begin
    Identifier := CustomizerIdentifier;
    FCustomizer := FCustomizerList.Customizer[Identifier];
    if FCustomizer = nil then
    begin
      if Assigned( OnCreateEditCustomizer ) then
        FCustomizer := OnCreateEditCustomizer( Self, FCellType )
      else
        FCustomizer := CellType.CreateEditCustomizer( Self );

      FCustomizerList.Customizer[Identifier] := FCustomizer;
    end;

    FCustomizerUpToDate := True;
  end;

  Result := FCustomizer;
end;

function TDBUEditorCell.GetEditor: IInfoEditor;
begin
  Result := InplaceEdit;
end;

function TDBUEditorCell.DoGetEditText: String;
begin
  if CellType <> nil then
    Result := CellType.GetEditText( Self );
end;

procedure TDBUEditorCell.SetEditText(const AValue: string);
begin
  CellType.SetEditText( Self, AValue );
end;

function TDBUEditorCell.GetFormatter(DrawState: TGridDrawState): TDBUFormatter;
begin
  if not FFormatterUpToDate and
     Assigned( OnGetFormatter ) then
  begin
    FFormatterUpToDate := True;
    OnGetFormatter( Self, Formatter, DrawState );
  end;
  Result := Formatter;
end;

function TDBUEditorCell.DoGetValue: TValue;
begin
  if Assigned( OnGetValue ) then
    OnGetValue( Self, Result, FDataType )
  else
    Result := EmptyString;
end;

function TDBUEditorCell.GetValue : TValue;
begin
  if not FValueUpToDate then
  begin
    FValue := DoGetValue;
//    FOldValue := FValue;
    FValueUpToDate := True;
    FEditorValueUpToDate := False;
  end;

  Result := FValue;
end;

procedure TDBUEditorCell.HandleCancelMode(var Params: THandleCMParams);
begin
  if CellType = nil then
    GetCell( Params.Col, Params.Row );
  CellType.HandleCancelMode( Self, Params );
end;

procedure TDBUEditorCell.HandleMessage(var Params: THandleWMParams);
begin
  if CellType = nil then
    GetCell( Params.Col, Params.Row );
  CellType.HandleMessage( Self, Params );
end;

procedure TDBUEditorCell.MouseDown(NewMouseState: TMouseState);
begin
  if Assigned( CellType ) then
    CellType.EditorMouseDown( Self, NewMouseState );

  inherited;
end;

procedure TDBUEditorCell.MouseMove(NewMouseState: TMouseMoveState);
begin
  if Assigned( CellType ) then
    CellType.EditorMouseMove( Self, NewMouseState );

  inherited;
end;

procedure TDBUEditorCell.MouseUp(NewMouseState: TMouseState);
begin
  if Assigned( CellType ) then
    CellType.EditorMouseUp( Self, NewMouseState );

  inherited;
end;

function TDBUEditorCell.DoSetValue(const AValue: TValue; var RejectParams : TRejectParams) : Boolean;
begin
  if Assigned( OnSetValue ) then
    Result := OnSetValue( Self, AValue )
  else
    Result := True;
end;

function TDBUEditorCell.SetValueForCell(const AValue: TValue; var RejectParams : TRejectParams) : Boolean;
begin
  try
    Result := DoSetValue(AValue, RejectParams);
  except
    Result := False;
  end;
end;

function TDBUEditorCell.SetCell: Boolean;
var
  ParsedValue : TValue;
  RejectParams : TRejectParams;
begin
  if FHandleError then
  begin
    Result := False;
    Exit;
  end;

  try
    if not ReadOnly and
       Modified then
    begin
      RejectParams.ShowDialog := True;
      RejectParams.AllowRestore := RestoreForIllegalValues or Killing;
      RejectParams.Msg := TranslateMessage(E_CouldntSetValue, [AsString(EditorValue)]);
      RejectParams.YesNoChoise := RestoreForIllegalValues and not Killing;

      Result := ParseValue( EditorValue, ParsedValue, Rejectparams );

//      if Result and
//         ValueChanged( ParsedValue ) then

      if Result and
         not MemoTypeCS.Equals( ParsedValue, Value ) then
        Result := SetValueForCell(ParsedValue, Rejectparams);

      if not Result then
      begin
        if Killing and not AbortOnIllegalValues then
        begin
          RejectParams.YesNoChoise := False;
          RejectParams.AllowRestore := True;
        end;

        if RejectParams.AllowRestore and not RejectParams.YesNoChoise then
          RejectParams.Msg := RejectParams.Msg + #13#10 + TranslateMessage(E_RestoringOldValue)
        else if RestoreForIllegalValues then
          RejectParams.Msg := RejectParams.Msg + #13#10 + TranslateMessage(E_RestoreOldValue);

        HandleSetCellError(RejectParams);
//        ShowSetCellErrorAndRestore( 'Could not set the value to ''' + AsString(EditorValue) + '''!', not Killing )
      end
      else
      begin
        FValue := ParsedValue;
        Modified := False;
      end;
    end
    else
      Result := True;
  except
    UndoChanges;
    Result := False;
  end;

  if Result then
    ResetValues
  else if AbortOnIllegalValues then
    Abort;
end;

function TDBUEditorCell.ParseValue( SrcValue : TValue; var ParsedValue : TValue;
    var RejectParams : TRejectParams ) : Boolean;
var
  AFormatter : TDBUFormatter;
  ADataType : TDataType;
begin
  AFormatter := GetFormatter( [gdFocused] );
  ADataType := DataType;
  Result := AFormatter.ParseValue( SrcValue, ParsedValue, ADataType );
  if not Result then
    RejectParams.Msg := ADataType.GenerateError(AsString(SrcValue));
end;

procedure TDBUEditorCell.SetCellType(const AValue: TDBUCellType);
begin
  FCellType := AValue;
end;

procedure TDBUEditorCell.SetOnGetValue(const AValue: TOnGetValueEvent);
begin
  FOnGetValue := AValue;
end;

procedure TDBUEditorCell.SetOnSetValue(const AValue: TOnSetValueEvent);
begin
  FOnSetValue := AValue;
end;

procedure TDBUEditorCell.SetValue(const AValue: TValue);
begin
{  if not FValueUpToDate then
    FOldValue := FValue;
}
  FValueUpToDate := True;
  FEditorValueUpToDate := False;
  FValue := AValue;
end;

procedure TDBUEditorCell.SetOnGetFormatter(
  const AValue: TOnGetFormatterEvent);
begin
  FOnGetFormatter := AValue;
end;

procedure TDBUEditorCell.SetOnCreateEditCustomizer(
  const AValue: TOnCreateEditCustomizerEvent);
begin
  FOnCreateEditCustomizer := AValue;
end;

procedure TDBUEditorCell.SetOnGetCellType(
  const AValue: TOnGetCellTypeEvent);
begin
  FOnGetCellType := AValue;
end;

procedure TDBUEditorCell.Reset;
begin
  if Modified then
    raise Exception.Create( Self.ClassName + '.Reset: Cell is modified!' );
  inherited;

  FFormatterUpToDate := False;
//  DefaultFormatter.Reset;
  if Assigned( FCustomizer ) then
  begin
    FCustomizer.Reset;
    FCustomizer := nil;
  end;
  FCustomizerUpToDate := False;
  FValueUpToDate := False;
  FEditorValueUpToDate := False;
  FReadOnlyUpToDate := False;
  FDataType := nil;
end;

procedure TDBUEditorCell.SetOnGetReadOnly(
  const AValue: TOnGetReadOnlyEvent);
begin
  FOnGetReadOnly := AValue;
end;

function TDBUEditorCell.GetReadOnly: Boolean;
begin
  if not FReadOnlyUpToDate then
  begin
    FReadOnlyUpToDate := True;
    if not Assigned( CellType ) or
       CellType.ReadOnly then
      FReadOnly := True
    else if Assigned( OnGetReadOnly ) then
      FReadOnly := OnGetReadOnly( Self )
    else
      FReadOnly := False;
  end;

  Result := FReadOnly
end;

procedure TDBUEditorCell.SetReadOnly(const AValue: Boolean);
begin
  inherited;
  FReadOnlyUpToDate := True;
end;

function TDBUEditorCell.GetEditorMode: Boolean;
begin
  if Assigned( InplaceEdit) then
    Result := InplaceEdit.EditorMode
  else
    Result := True;
end;

procedure TDBUEditorCell.SetEditorMode(AValue: Boolean);
begin
  if Assigned( InplaceEdit) then
    InplaceEdit.EditorMode := AValue;
end;

procedure TDBUEditorCell.KeyDown(var NewKeyState: TKeyState);
begin
  inherited;

  if Assigned( CellType ) then
    CellType.EditorKeyDown( Self, NewKeyState );
end;

procedure TDBUEditorCell.KeyUp(var NewKeyState: TKeyState);
begin
  inherited;

  if Assigned( CellType ) then
    CellType.EditorKeyUp( Self, NewKeyState );
end;

function TDBUEditorCell.GetEditorValue: TValue;
var
  AFormatter : TDBUFormatter;
begin
  if not FEditorValueUpToDate then
  begin
    AFormatter := GetFormatter( [gdFocused] );
    FEditorValue := AFormatter.FormatValue( Value, DataType );
    FEditorValueUpToDate := True;
  end;

  Result := FEditorValue;
end;

procedure TDBUEditorCell.SetEditorValue(const AValue: TValue);
begin
  if FValueUpToDate then
  begin
    FEditorValueUpToDate := True;
    Modified := ValueChanged( AValue );
    FEditorValue := AValue;
  end
  else
    FEditorValueUpToDate := False;
end;
(*
function TDBUEditorCell.DataType: TDataType;
begin
  Result := Value.DataType;
end;
*)
procedure TDBUEditorCell.DefineCellType;
begin
  if Assigned( InplaceEdit) and
     ( (Col < InplaceEdit.DBUGrid.FixedCols) or
       (Row < InplaceEdit.DBUGrid.FixedRows) ) then
    FCellType := HeaderCellType
  else
    FCellType := NormalCellType;

  if Assigned( OnGetCellType ) then
    OnGetCellType( Self, FCellType )
end;

procedure TDBUEditorCell.HandleSetCellError(RejectParams : TRejectParams);
var
  Restore : Boolean;
begin
  with RejectParams do
  begin
    if FIsIterator then
      DoUndoChanges
    else
    begin
      FHandleError := True;
      try
        Restore := AllowRestore;
        if ShowDialog then
        begin
          if YesNoChoise then
            Restore := TranslateMessageDlg(Msg, mtError, [mbYes, mbNo], 0) = mrYes
          else
            TranslateMessageDlg(Msg, mtError, [mbOk], 0);
        end;

        if Restore then
          UndoChanges;

        Grid.ShowEditor;
      finally
        FHandleError := False;
      end;
    end;
  end;
end;

procedure TDBUEditorCell.DoUndoChanges;
begin
//  FValue := FOldValue;
  FEditorValueUpToDate := False;
  inherited;
end;

function TDBUEditorCell.GetInplaceEditText: String;
begin
  Result := InplaceText;
end;

function TDBUEditorCell.GetGrid: IInfoGrid;
begin
  Result := Grid;
end;

procedure TDBUEditorCell.SetInplaceEditText(const Value: String);
begin
  if Assigned( InplaceEdit) then
  begin
    InplaceEdit.Text := Value;
    InplaceText := Value;
  end;
end;

procedure TDBUEditorCell.ChangeCell(ACol, ARow: Integer);
begin
  GetCell( ACol, ARow );
end;

function TDBUEditorCell.CreateIterator(Unique : Boolean = False) : IEditorIterator;
var
  ACell : TDBUEditorCell;
begin
  if Unique or not Assigned(FIterator) then
  begin
    ACell := TDBUEditorCellClass( Self.ClassType ).Create;
    ACell.FIsIterator := True;
    ACell.SetFormatter( Grid.CreateEditorFormatter );
    InitIterator( ACell );
    if Unique then
      FUniqueIterators.Add(ACell)
    else
      FIterator := ACell;
  end
  else
    ACell := FIterator;

  Result := ACell;
end;

procedure TDBUEditorCell.InitIterator(Iterator: TDBUEditorCell);
var
  c : TDBUEditorCell;
begin
  c := Iterator;
  c.FOnGetValue := FOnGetValue;
  c.FOnSetValue := FOnSetValue;
  c.FOnGetFormatter := FOnGetFormatter;
  c.FOnGetCellType := FOnGetCellType;
  c.FOnGetReadOnly := FOnGetReadOnly;
end;

function TDBUEditorCell.GetGridValue: TValue;
begin
  Result := Value;
end;

procedure TDBUEditorCell.SetGridValue(const AValue: TValue);
begin
  if ValueChanged( AValue ) then
  begin
    SetEditorValue( AValue );
    SetCell;
  end;
  
  Modified := False;
end;

function TDBUEditorCell.ValueChanged( aValue : TValue ) : Boolean;
var
  OldFormattedValue : TValue;
begin
  try
    OldFormattedValue := GetDrawValue;//Formatter.FormatValue(Value, DataType);
    Result := not MemoTypeCS.Equals( OldFormattedValue, aValue );
  except
    Result := True;
  end;
end;

function TDBUEditorCell.GetDrawValue: TValue;
begin
  Result := Formatter.FormatValue( Value, DataType );
end;

function TDBUEditorCell.GetEditorKeyValue: TValue;
begin
  Result := EditorValue;
end;

procedure TDBUEditorCell.ResetValues;
begin
  inherited;

  FValueUpToDate := False;
  FEditorValueUpToDate := False;
  FDataType := nil;
end;

function TDBUEditorCell.GetDataType: TDataType;
begin
  // The FDataType field is initialized in GetValue
  Value;
  if not Assigned(FDataType) then
    FDataType := Value.DataType;
  
  Result := FDataType;
end;

procedure TDBUEditorCell.SetDataType(const Value: TDataType);
begin
  FDataType := Value;
end;

{ TCustomizerList }

constructor TCustomizerList.Create;
begin
  FCustomizers := TList.Create;
  FIdentifiers := TList.Create;
end;

destructor TCustomizerList.Destroy;
begin
  inherited;

  FreeListWithObjects( FCustomizers );
  FIdentifiers.Free;
end;

function TCustomizerList.GetCustomizer(
  Identifier: Pointer): TDBUCustomizer;
var
  idx : Integer;
begin
  idx := FIdentifiers.IndexOf(Identifier);
  if idx >= 0 then
    Result := TDBUCustomizer(FCustomizers[idx])
  else
    Result := nil;
end;

procedure TCustomizerList.SetCustomizer(Identifier : Pointer;
  const AValue: TDBUCustomizer);
var
  idx : Integer;
begin
  idx := FIdentifiers.IndexOf(Identifier);

  if idx >= 0 then
  begin
    TDBUCustomizer( FCustomizers[idx] ).Free;
    FCustomizers[idx] := AValue;
  end
  else
  begin
    FIdentifiers.Add(Identifier);
    FCustomizers.Add( AValue );
  end;
end;

end.

