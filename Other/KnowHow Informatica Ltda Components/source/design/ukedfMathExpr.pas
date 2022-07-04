{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukedfMathExpr;

interface

{$I s:\v100\include\iKLIB100.inc}

{$R-}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, DsgnIntf,
  TypInfo, Grids, ExtCtrls, StdCtrls, Buttons, ukeClasses, Menus,
  ukrClasses;

type

  EKPropExprExprEditor = class( EKPropertyExpression );

  TKPropExprEditorKind = ( peekIdent, peekMathSolver, peekPropExpr );

  TfrmPropExprEd = class(TForm)
    pnMain: TPanel;
    pnBtns: TPanel;
    bnAdd: TBitBtn;
    bnDel: TBitBtn;
    bnClear: TBitBtn;
    bnEval: TBitBtn;
    bnHelp: TBitBtn;
    bnClose: TBitBtn;
    pnCompList: TPanel;
    pnStrGrid: TPanel;
    sgExprs: TStringGrid;
    pmGrid: TPopupMenu;
    miAdd: TMenuItem;
    miDel: TMenuItem;
    miClear: TMenuItem;
    miEval: TMenuItem;
    miN2: TMenuItem;
    miHelp: TMenuItem;
    miClose: TMenuItem;
    miN1: TMenuItem;
    lbPropList: TLabel;
    cbPropList: TComboBox;
    lbCompList: TLabel;
    cbCompList: TComboBox;
    bnCopy: TBitBtn;
    miCopy: TMenuItem;
    bnEdit: TBitBtn;
    miEdit: TMenuItem;
    bnEvalFmt: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure bnAddEditClick(Sender: TObject);
    procedure bnDelClick(Sender: TObject);
    procedure bnClearClick(Sender: TObject);
    procedure bnEvalClick(Sender: TObject);
    procedure bnHelpClick(Sender: TObject);
    procedure bnCloseClick(Sender: TObject);
    procedure bnCopyClick(Sender: TObject);
    procedure sgExprsSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure sgExprsDblClick(Sender: TObject);
    procedure cbPropListChange(Sender: TObject);
    procedure cbCompListChange(Sender: TObject);
    procedure bnEvalFmtClick(Sender: TObject);
  private
    FEvalFmt: string;
    FExpressions: TStrings;
    FLastPropIndex: Integer;
    FLastCompIndex: Integer;
    FOwnerComp: TComponent;
    FEvalSolver: TKMathSolver;
    FMathSolver: TKCustomMathSolver;
    FFillFuncParams: Boolean;
    FPropExprEditorKind: TKPropExprEditorKind;

    procedure FillGridInfo;
    procedure FillGridHeader;
    procedure PrepareForm;
    procedure FillRegPropInfos;
    procedure FillExpressions;
    procedure SetExpressions( Value: TStrings );
    procedure SwitchBtns( IsEnabled: Boolean );
    procedure SwitchAddBtn;
    procedure AdjustColWidth( Col, Row: Integer );

    function EnumMathPropertyEdt( Index: Integer; EditorClass: TPropertyEditorClass;
      PropInfo: PTypeInfo; PropClass: TClass; const PropName: string; Data: Pointer ): Boolean;
      
    procedure MathSolverSolve( Sender: TKCustomMathSolver; Expr: TKMathExpression; ExprKind: TKMathExprKind;
      const StrExpr: String; const ReturnValue: Extended );
    procedure MathSolverSolveFunc( Sender: TKCustomMathSolver;
      Expr: TKMathExpression; const FuncName: String; FuncParams: TKExtendedStack;
      var FuncReturn: Extended );
    procedure MathSolverSolveIdent( Sender: TKCustomMathSolver;
      Expr: TKMathExpression; const IdentName: String;
      var IdentValue: Extended );

  public
    property OwnerComp: TComponent
             read FOwnerComp write FOwnerComp;
    property MathSolver: TKCustomMathSolver
             read FMathSolver write FMathSolver;
    property Expressions: TStrings
             read FExpressions write SetExpressions;
    property FillFuncParams: Boolean
             read FFillFuncParams write FFillFuncParams;
    property PropExprEditorKind: TKPropExprEditorKind
             read FPropExprEditorKind;
    property EvalFmt: string
             read FEvalFmt;         

  end;

procedure EditPropExprs( AOwner: TComponent; MathSolver: TKCustomMathSolver;
  IsExprs, FillFuncParams, ClearAllowed, EvalAllowed: Boolean; Exprs: TStrings );

implementation

{$R *.DFM}

uses
  ClipBrd, uksyUtils, uksyConsts, ukeResStr, ukedConsts, ukedClasses,
  ukefAddMathExpr;

const

  COL_EXPRID_IDX    = 0;
  COL_PROPNAME_IDX  = 1;
  COL_PROPVALUE_IDX = 2;
  COL_EXPRSOLVE_IDX = 3;

  STRGRID_COLCOUNT: array[TKPropExprEditorKind] of Integer = ( 4, 3, 4 );

  HEADER_CELLS: array[COL_EXPRID_IDX..COL_EXPRSOLVE_IDX] of string =
    ( 'ID', 'Name', 'Value', 'Solved' );

  CAPTION_MATH_TERM: array[Boolean] of string = ( 'Identifiers', 'Expressions' );

{ Utility Functions }

procedure EditPropExprs( AOwner: TComponent; MathSolver: TKCustomMathSolver;
  IsExprs, FillFuncParams, ClearAllowed, EvalAllowed: Boolean; Exprs: TStrings );
var
  frmPropExprEd: TfrmPropExprEd;
begin
  ForceObjects( [AOwner, MathSolver] );
  frmPropExprEd := TfrmPropExprEd.Create( nil );
  try
    if ( not IsExprs ) then
      frmPropExprEd.FPropExprEditorKind := peekIdent
    else if CheckObjectClass( MathSolver, TKMathSolver ) then
      frmPropExprEd.FPropExprEditorKind := peekMathSolver
    else if CheckObjectClass( MathSolver, TKPropertyExpression ) then
    begin
      frmPropExprEd.FPropExprEditorKind := peekPropExpr;
      frmPropExprEd.ActiveControl := frmPropExprEd.cbPropList;
    end
    else
      RaiseExceptionFmt( EKPropExprExprEditor, sErrInvMathExprExprEdtCls, [MathSolver.ClassName] );
    frmPropExprEd.OwnerComp := AOwner;
    frmPropExprEd.MathSolver := MathSolver;
    frmPropExprEd.Expressions := Exprs;
    frmPropExprEd.bnEval.Visible := EvalAllowed;
    frmPropExprEd.bnClear.Visible := ClearAllowed;
    frmPropExprEd.miEval.Visible := EvalAllowed;
    frmPropExprEd.miClear.Visible := ClearAllowed;
    frmPropExprEd.FillFuncParams := FillFuncParams;
    frmPropExprEd.Caption := Format( frmPropExprEd.Caption, [CAPTION_MATH_TERM[IsExprs]] );
    frmPropExprEd.ShowModal;
    Exprs.Assign( frmPropExprEd.Expressions );
  finally
    frmPropExprEd.Free;
  end;
end;

{ Form Methods }

type
  TKCustomMathSolverHack = class( TKCustomMathSolver );

procedure TfrmPropExprEd.SetExpressions( Value: TStrings );
begin
  FExpressions.Assign( Value );
end;

function TfrmPropExprEd.EnumMathPropertyEdt( Index: Integer;
  EditorClass: TPropertyEditorClass; PropInfo: PTypeInfo; PropClass: TClass;
  const PropName: string; Data: Pointer ): Boolean;
begin
  if ( CheckTrimStr( PropName ) and ( TStrings( Data ).IndexOf( PropName ) = -1 ) ) then
    TStrings( Data ).AddObject( PropName, TObject( PropInfo ) );
  Result := True;
end;

procedure TfrmPropExprEd.PrepareForm;
begin
  cbPropList.Items.Clear;
  cbCompList.Items.Clear;
  pnCompList.Enabled := ( PropExprEditorKind = peekPropExpr );
  if pnCompList.Enabled then
  begin
    FillRegPropInfos;
    pnCompList.Height := ( cbCompList.Top + cbCompList.Height + 5 );
  end
  else
    pnCompList.Height := 0;
  FillGridHeader;
  FillGridInfo;
  FEvalSolver.Identifiers := TKCustomMathSolverHack( MathSolver ).Identifiers;
end;

procedure TfrmPropExprEd.FillGridHeader;
var
  i: Integer;
begin
  sgExprs.ColCount := STRGRID_COLCOUNT[PropExprEditorKind];
  for i := 0 to sgExprs.ColCount - 1 do
    sgExprs.Cells[i, 0] := HEADER_CELLS[i];
  for i := 0 to sgExprs.ColCount - 1 do
    sgExprs.ColWidths[i] := sgExprs.Canvas.TextWidth( sgExprs.Cells[i, 0] ) + 10;
end;

procedure TfrmPropExprEd.FillRegPropInfos;
begin
  cbPropList.Items.BeginUpdate;
  try
    cbCompList.Items.Clear;
    cbPropList.Items.Clear;
    {$IFNDEF DESIGN_TIME_TEST}
      EnumRegMathPropEditors( cbPropList.Items, EnumMathPropertyEdt );
    {$ELSE}
      cbPropList.Items.AddObject( 'Left', TypeInfo( Integer ) );
      cbPropList.Items.AddObject( 'Top', TypeInfo( Integer ) );
      cbPropList.Items.AddObject( 'Height', TypeInfo( Integer ) );
      cbPropList.Items.AddObject( 'Width', TypeInfo( Integer ) );
    {$ENDIF}
    if CheckStrings( cbPropList.Items ) then
    begin
      cbPropList.ItemIndex := 0;
      cbPropListChange( cbPropList );
    end;
  finally
    cbPropList.Items.EndUpdate;
  end;
end;

procedure TfrmPropExprEd.FillGridInfo;
var
  i: Integer;
begin
  with sgExprs do
  begin
    RowCount := ( Expressions.Count + 2 );
    for i := FixedRows to RowCount - 2 do
    begin
      Cells[COL_EXPRID_IDX, i] := Format( '%-d', [i] );
      case PropExprEditorKind of
        peekMathSolver:
        begin
          Cells[COL_PROPNAME_IDX, i] := Expressions[( i - 1 )];
          Cells[COL_PROPVALUE_IDX, i] := sMathValueNotEval;
        end;
        peekIdent,
        peekPropExpr:
        begin
          Cells[COL_PROPNAME_IDX, i] := Expressions.Names[( i - 1 )];
          Cells[COL_PROPVALUE_IDX, i] := Expressions.Values[FExpressions.Names[( i - 1 )]];
        end;
      end;
      AdjustColWidth( COL_EXPRID_IDX, i );
      AdjustColWidth( COL_PROPNAME_IDX, i ); 
      AdjustColWidth( COL_PROPVALUE_IDX, i );
      if ( PropExprEditorKind in [peekIdent, peekPropExpr] ) then
      begin
        Cells[COL_EXPRSOLVE_IDX, i] := sMathValueNotEval;
        AdjustColWidth( COL_EXPRSOLVE_IDX, i );
        if ( PropExprEditorKind = peekPropExpr ) then
          SwitchAddBtn;
      end;
    end;
    SwitchBtns( ( RowCount > ( FixedRows + 1 ) ) );
  end;
end;

procedure TfrmPropExprEd.FillExpressions;
var
  i: Integer;
begin
  FExpressions.Clear;
  with sgExprs do
    for i := FixedRows to RowCount - 2 do
      case PropExprEditorKind of
        peekMathSolver:
          FExpressions.Add( Cells[COL_PROPNAME_IDX, i] );
        peekIdent,
        peekPropExpr  :
          FExpressions.Add( Cells[COL_PROPNAME_IDX, i] + CH_EQUAL_TOKEN + Cells[COL_PROPVALUE_IDX, i] );
      end;
end;

procedure TfrmPropExprEd.SwitchBtns( IsEnabled: Boolean );
begin
  bnDel.Enabled := IsEnabled;
  miDel.Enabled := IsEnabled;
  bnEval.Enabled := IsEnabled;
  miEval.Enabled := IsEnabled;
  bnClear.Enabled := IsEnabled;
  miClear.Enabled := IsEnabled;
  bnCopy.Enabled := IsEnabled;
  miCopy.Enabled := IsEnabled;
  bnEdit.Enabled := ( sgExprs.RowCount > ( sgExprs.FixedRows + 1 ) );
  miEdit.Enabled := bnEdit.Enabled;
end;

procedure TfrmPropExprEd.SwitchAddBtn;
begin
  bnAdd.Enabled := ( ( cbPropList.ItemIndex <> -1 ) and ( cbCompList.ItemIndex <> -1 ) );
  miAdd.Enabled := bnAdd.Enabled;
end;

procedure TfrmPropExprEd.AdjustColWidth( Col, Row: Integer );
begin
  if ( ValueBetween( Row, sgExprs.FixedRows, sgExprs.RowCount - 1, True ) and
       ValueBetween( Col, sgExprs.FixedCols, sgExprs.ColCount - 1, True ) ) then
      sgExprs.ColWidths[Col] := Max( sgExprs.Canvas.TextWidth( sgExprs.Cells[Col,Row] ) + 10,
        sgExprs.ColWidths[Col] );
end;

{ Form Events }

{ Main Events }

procedure TfrmPropExprEd.FormCreate(Sender: TObject);
begin
  FExpressions := TStringList.Create;
  FEvalSolver := TKMathSolver.Create( nil );
  FEvalSolver.SolverExprKind := msekNormal;
  FEvalSolver.OnSolve := MathSolverSolve;
  FEvalSolver.OnSolveFunc := MathSolverSolveFunc;
  FEvalSolver.OnSolveIdent := MathSolverSolveIdent;
  FLastPropIndex := -1;
  FLastCompIndex := -1;
  FEvalFmt := GetFirstString( [ReadFromRegistry( rkCurrentUser, False,
    PROPEXPRED_BASE_REGKEY, PROPEXPRED_EVAL_FMT ), '0'] ); { Default evaluate format }
end;

procedure TfrmPropExprEd.FormDestroy(Sender: TObject);
begin
  FExpressions.Free;
  FEvalSolver.Free;
end;

procedure TfrmPropExprEd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  cbPropList.Items.Clear;
  cbCompList.Items.Clear;
  WriteToRegistry( rkCurrentUser, True, PROPEXPRED_BASE_REGKEY, PROPEXPRED_EVAL_FMT,
    FEvalFmt );
  FillExpressions;
end;

procedure TfrmPropExprEd.FormShow(Sender: TObject);
begin
  ForceObjects( [FOwnerComp, FMathSolver] );
  PrepareForm;
end;

{ Main Button Events }

procedure TfrmPropExprEd.bnAddEditClick(Sender: TObject);

  function IdentNotExists( Col: Integer; const s: string ): Boolean;
  begin
    Result := ( ValueBetween( Col, sgExprs.FixedCols, sgExprs.ColCount - 1, True ) and
      ( sgExprs.Cols[Col].IndexOf( s ) = -1 ) );
  end;

const
  ADD_TAG = -1;
  EDT_TAG = -2;
var
  iRow: Integer;
  p: TPoint;
  sName,
  sValue: string;
begin
  iRow := ( Sender as TComponent ).Tag;
  if ( ( iRow <> ADD_TAG ) and ( iRow <> EDT_TAG ) ) then
    Exit;
  with sgExprs do
  begin
    sName := '';
    sValue := '';
    p.x := -1; p.y := -1;
    case PropExprEditorKind of
      peekIdent     :
      begin
        if ( iRow = EDT_TAG ) then
        begin
          sName := Cells[COL_PROPNAME_IDX, Selection.Top];
          sValue := Cells[COL_PROPVALUE_IDX, Selection.Top];
        end;
        if ( not ( InputDialog( CAPTION_MATH_TERM[False], Format( sIdentName,
          [MathSolver.Name] ), 'L' + StringOfChar( 'a', MAXLEN_PASCALID - 1 ) + ';0; ',
          sName ) and CheckTrimStr( sName ) and IdentNotExists( COL_PROPNAME_IDX, sName ) ) ) then
          RaiseExceptionFmt( EKPropExprExprEditor, sErrMathIdentError, [sName] );
      end;
      peekMathSolver:
      begin
        sName := MathSolver.Name + CH_DOTMARK + CAPTION_MATH_TERM[True];
        if ( iRow = EDT_TAG ) then
          sValue := Cells[COL_PROPNAME_IDX, Selection.Top];
      end;
      peekPropExpr  :
      begin
        sName := cbCompList.Text + CH_DOTMARK + cbPropList.Text;
        ForceTrimStr( sName );
        if ( iRow = EDT_TAG ) then
          sValue := Cells[COL_PROPVALUE_IDX, Selection.Top];
      end;
    end;
    if ( EditExpression( sName, OwnerComp, cbPropList.Items, // ( PropExprEditorKind in [peekIdent, peekPropExpr] ),
      FillFuncParams, sValue, @p ) and CheckTrimStr( sValue ) ) then
    begin
      if ( iRow = ADD_TAG ) then
      begin
        RowCount := ( RowCount + 1 );
        iRow := ( RowCount - 2 );
        Cells[COL_EXPRID_IDX, iRow] := Format( '%-d', [iRow] );
        AdjustColWidth( COL_EXPRID_IDX, iRow );
      end
      else
        iRow := Selection.Top;
      case PropExprEditorKind of
        peekMathSolver:
        begin
          Cells[COL_PROPNAME_IDX, iRow] := sValue;
          Cells[COL_PROPVALUE_IDX, iRow] := sMathValueNotEval;
        end;
        peekIdent,
        peekPropExpr:
        begin
          Cells[COL_PROPNAME_IDX, iRow] := sName;
          Cells[COL_PROPVALUE_IDX, iRow] := sValue;
          Cells[COL_EXPRSOLVE_IDX, iRow] := sMathValueNotEval;
          AdjustColWidth( COL_EXPRSOLVE_IDX, iRow );
        end;
      end;
      AdjustColWidth( COL_PROPNAME_IDX, iRow );
      AdjustColWidth( COL_PROPVALUE_IDX, iRow );
    end;
  end;
end;

procedure TfrmPropExprEd.bnDelClick(Sender: TObject);
var
  i, j: Integer;
  gRect: TGridRect;
begin
  with sgExprs do
    if ( RowCount > ( FixedRows + 1 ) ) then
    begin
      for i := Selection.Top to RowCount - 2 do
        for j := Selection.Left to ColCount - 1 do
          Cells[j, i] := Cells[j, ( i + 1 )];
      if ( RowCount > ( FixedRows + 1 ) ) then
        RowCount := ( RowCount - 1 );
      SwitchBtns( ( RowCount > ( FixedRows + 1 ) ) );
      gRect := Selection;
      gRect.Top := Max( TopRow, gRect.Top - 1 );
      gRect.Bottom := gRect.Top;
      Selection := gRect;
    end;
end;

procedure TfrmPropExprEd.bnClearClick(Sender: TObject);
var
  gRect: TGridRect;
begin
  with sgExprs do
    if ( RowCount > ( FixedRows + 1 ) ) then
    begin
      RowCount := ( FixedRows + 1 );
      Rows[RowCount - 1].Clear;
      Cells[COL_EXPRID_IDX, RowCount - 1] := Format( '%-d', [RowCount - 1] ); 
      SwitchBtns( ( RowCount > ( FixedRows + 1 ) ) );
      gRect := Selection;
      gRect.Top := TopRow;
      gRect.Bottom := gRect.Top;
      Selection := gRect;
    end;
end;

procedure TfrmPropExprEd.bnEvalClick(Sender: TObject);
const
  EXPR_TYPE: array[TKPropExprEditorKind] of TKStringType =
    ( stValues, stStrings, stValues );
begin
  with sgExprs do
    if ( RowCount > ( FixedRows + 1 ) ) then
    begin
      FillExpressions;
      FEvalSolver.ExpressionType := EXPR_TYPE[PropExprEditorKind];
      FEvalSolver.Expressions := Expressions;
      FEvalSolver.SolveAll;
    end;
end;

procedure TfrmPropExprEd.bnEvalFmtClick(Sender: TObject);
var
  s: string;
begin
  s := FEvalFmt;
  if ( InputDialog( sPropExprEdEvalFmtTitle, sPropExprEdEvalFmtPrompt, '', s ) and
    CheckTrimStr( s ) and CheckStrCharSet( s, PROPEXPRED_DEF_FLTFMTCHARSET ) ) then
    FEvalFmt := GetFirstString( [s, FEvalFmt] );
  InformFmt( sPropExprEdEvalFmt, [FEvalFmt] );  
end;

procedure TfrmPropExprEd.bnCopyClick(Sender: TObject);
begin
  with sgExprs do
    if ( RowCount > ( FixedRows + 1 ) ) then
    begin
      case PropExprEditorKind of
        peekIdent,
        peekMathSolver: ClipBoard.AsText := Cells[COL_PROPNAME_IDX, Selection.Top];
        peekPropExpr: ClipBoard.AsText := Cells[COL_PROPVALUE_IDX, Selection.Top];
      end;
    end;
end;

procedure TfrmPropExprEd.bnHelpClick(Sender: TObject);
begin
  NotYetImplemented;
end;

procedure TfrmPropExprEd.bnCloseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

{ String Grid Events }

procedure TfrmPropExprEd.sgExprsSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  CanSelect := ( Row < ( sgExprs.RowCount - 1 ) );
  SwitchBtns( CanSelect );
end;

procedure TfrmPropExprEd.sgExprsDblClick(Sender: TObject);
begin
  if bnEdit.Enabled then
    bnEdit.Click
  else if bnAdd.Enabled then
    bnAdd.Click;
end;

{ Combos Events }

procedure TfrmPropExprEd.cbPropListChange(Sender: TObject);
var
  s: string;
  i: Integer;
  pi: PPropInfo;
begin
  FLastCompIndex := -1;
  FLastPropIndex := cbPropList.ItemIndex;
  if ( FLastPropIndex <> -1 ) then
  begin
    cbCompList.Items.BeginUpdate;
    try
      s := cbCompList.Text;
      cbCompList.Items.Clear;
      for i := 0 to FOwnerComp.ComponentCount - 1 do
      begin
        pi := GetPropInfo( FOwnerComp.Components[i].ClassInfo, cbPropList.Items[FLastPropIndex] );
        if ( CheckPointers( [pi, PTypeInfo( cbPropList.Items.Objects[FLastPropIndex] )] ) and
          ( pi^.PropType^^.Kind = PTypeInfo( cbPropList.Items.Objects[FLastPropIndex] )^.Kind ) ) then
          cbCompList.Items.AddObject( FOwnerComp.Components[i].Name, FOwnerComp.Components[i] );
      end;
      if CheckStrings( cbCompList.Items ) then
      begin
        FLastCompIndex := cbCompList.Items.IndexOf( s );
        if ( FLastCompIndex = -1 ) then
           FLastCompIndex := 0;
        cbCompList.ItemIndex := FLastCompIndex;
      end;
    finally
      cbCompList.Items.EndUpdate;
    end;
  end;
  SwitchAddBtn;
end;

procedure TfrmPropExprEd.cbCompListChange(Sender: TObject);
begin
  FLastCompIndex := cbCompList.ItemIndex;
  SwitchAddBtn;
end;

{ Math Solver Events }

procedure TfrmPropExprEd.MathSolverSolve(Sender: TKCustomMathSolver;
  Expr: TKMathExpression; ExprKind: TKMathExprKind; const StrExpr: String;
  const ReturnValue: Extended);
begin
  if ( ExprKind = mekNormal ) then
  begin
    sgExprs.Cells[( STRGRID_COLCOUNT[PropExprEditorKind] - 1 ), ( Expr.Index + 1 )] :=
      FormatFloat( FEvalFmt, ReturnValue );
    AdjustColWidth( ( STRGRID_COLCOUNT[PropExprEditorKind] - 1 ), ( Expr.Index + 1 ) );
  end;
end;

type
  TKPropertyExpressionHack = class( TKPropertyExpression );

procedure TfrmPropExprEd.MathSolverSolveFunc(Sender: TKCustomMathSolver; Expr: TKMathExpression;
  const FuncName: String; FuncParams: TKExtendedStack; var FuncReturn: Extended);
begin
  if CheckObjectClass( FMathSolver, TKPropertyExpression ) then
    FuncReturn := TKPropertyExpressionHack( FMathSolver ).DoSolveFunc( Expr.Index, FuncName, FuncParams )
  else
  	RaiseExceptionFmt( EKPropExprExprEditor, sErrInvMathFunc, [FuncName, ( Expr.Index + 1 )] );
end;

procedure TfrmPropExprEd.MathSolverSolveIdent(Sender: TKCustomMathSolver;
  Expr: TKMathExpression; const IdentName: String;
  var IdentValue: Extended);
begin
  if CheckObjectClass( FMathSolver, TKPropertyExpression ) then
    IdentValue := TKPropertyExpressionHack( FMathSolver ).DoSolveIdent( Expr.Index, IdentName )
  else if CheckStrContains( CH_DOTMARK, IdentName ) then
  { assume ord as default }
    IdentValue := GetCompPathOrdValue( OwnerComp, IdentName )
  else
    RaiseExceptionFmt( EKPropExprExprEditor, sErrInvMathIdent, [IdentName, ( Expr.Index + 1 )] );
end;

end.
