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

unit ukefAddMathExpr;

interface

{$I s:\v100\include\iKLIB100.inc}

{$R-}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, ukeClasses;

type

  TKMathType = ( mtOp, mtFunc, mtIdent, mtComp );

  TfrmAddExpr = class(TForm)
    pnBtns: TPanel;
    bnOk: TBitBtn;
    bnCancel: TBitBtn;
    sbHelp: TSpeedButton;
    ilGeneral: TImageList;
    sbStatus: TStatusBar;
    pnMain: TPanel;
    pnExpr: TPanel;
    moExpr: TMemo;
    pnMathOp: TPanel;
    sbPlus: TSpeedButton;
    sbMinus: TSpeedButton;
    sbMul: TSpeedButton;
    sbDiv: TSpeedButton;
    sbMod: TSpeedButton;
    sbDivInt: TSpeedButton;
    sbShl: TSpeedButton;
    sbShr: TSpeedButton;
    sbOr: TSpeedButton;
    sbAnd: TSpeedButton;
    sbXor: TSpeedButton;
    sbBracesOpen: TSpeedButton;
    sbBracesClose: TSpeedButton;
    pnRegItems: TPanel;
    pnRIMain: TPanel;
    trvRegItems: TTreeView;
    pnRIGroups: TPanel;
    lbGroups: TListBox;
    pnRIGItems: TPanel;
    lbItems: TListBox;
    sbPower: TSpeedButton;
    cbFillFuncParams: TCheckBox;
    sbClear: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure trvRegItemsChange(Sender: TObject; Node: TTreeNode);
    procedure lbGroupsClick(Sender: TObject);
    procedure lbItemsDblClick(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure sbAllMathOpClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sbClearClick(Sender: TObject);
    procedure moExprDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure moExprDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FRegGroups: TList;
    FRegFuncs: TList;
    FRegIdents: TList;
    FComps: TStrings;
    FCompProps: TStrings;
    FLastNode: TTreeNode;
    FOwnerComp: TComponent;
    FFillComponents: Boolean;
    FInternalGroups: TKMathExprTermStrings;
    FLastGroupIndex: Integer;

    function GetCount( GetIdx: Integer ): Integer;
    function GetGroups( Index: Integer ): TKMathExprRegGroup;
    function GetInternalGroups( Index: Integer ): TKMathExprRegGroup;
    function GetFunctions( Index: Integer ): TKMathExprRegFunc;
    function GetIdentifiers( Index: Integer ): TKMathExprRegIdent;
    function GetCompExprNames( Index: Integer ): string;
    function GetCompExprValues( Index: Integer ): string;
    function GetCompProps( Index: Integer ): string;
    function GetExpression: string;

    procedure FillRegInfos;
    procedure FillMathOps( GroupID: ShortInt );
    procedure FillGroups( InfoType: TKMathType );
    procedure FillItemStrings( Group: TKMathExprRegGroup; MathType: TKMathType );
    procedure BuildTreeView;
    procedure BuildInternalGroups;
    function BuildFunctionText( FuncInfo: TKMathExprRegFunc ): string;
    function BuildHelp( Control: TWinControl ): string;

    function EnumRegInfos( Index: Integer; RegInfoType: TKMathExprRegType;
      RegItem: TKMathExprRegTerm; Data: Pointer ): Boolean;

  public
    property FillComponents: Boolean
             read FFillComponents write FFillComponents;
    property OwnerComp: TComponent
             read FOwnerComp write FOwnerComp;
    property Expression: string
             read GetExpression;
    property GroupCount: Integer
             index 0 read GetCount;
    property Groups[Index: Integer]: TKMathExprRegGroup
             read GetGroups;
    property InternalGroupCount: Integer
             index 1 read GetCount;
    property InternalGroups[Index: Integer]: TKMathExprRegGroup
             read GetInternalGroups;
    property FuncCount: Integer
             index 2 read GetCount;
    property Functions[Index: Integer]: TKMathExprRegFunc
             read GetFunctions;
    property IdentCount: Integer
             index 3 read GetCount;
    property Identifiers[Index: Integer]: TKMathExprRegIdent
             read GetIdentifiers;
    property CompCount: Integer
             index 4 read GetCount;
    property CompExprNames[Index: Integer]: string
             read GetCompExprNames;
    property CompExprValues[Index: Integer]: string
             read GetCompExprValues;
    property PropCount: Integer
             index 5 read GetCount;
    property CompProps[Index: Integer]: string
             read GetCompProps;

  end;

{
  AOwner          -> MUST not be nil
  CompProps       -> If nil or empty, there isn't component properties as identifiers
                     Otherwise, MUST be Strings[i]=PropName; Objects[i]=PPropInfo
  FillFuncParams  -> Will call the editor recursively to fill function parameters
  Expression      -> If empty, this is a new expression. If not, the editor loads it to edit
                     the expression will only be adjusted if the user confirm the execution (Ok)
  Corner          -> If nil, the form is screen centered
                     Otherwise, the form is positioned at this point (Left;Top).
                     If not nil but with -1;-1, it's screen centered and fill the Corner.
                     This will be used when the editor was caling itself recursively.
}

function EditExpression( const Caption: string; AOwner: TComponent; CompProps: TStrings;
  FillFuncParams: Boolean; var Expression: string; Corner: PPoint ): Boolean;

implementation

{$R *.DFM}

uses
  TypInfo, uksyConsts, uksyUtils, ukeConsts, ukeTypes, ukeResStr;

const

  FUNCTION_IMAGE_IDX         = 0;
  IDENTIFIERS_IMAGE_IDX      = 1;
  OPERATORS_IMAGE_IDX        = 2;
  REGISTERED_FUNC_IMAGE_IDX  = 3;
  REGISTERED_IDENT_IMAGE_IDX = 4;
  COMPONENT_IDENT_IMAGE_IDX  = 5;

  COMPONENTS_GROUP_ID       = DEFAULT_GROUP_ID - 1;
  ARITIMETIC_OP_GROUP_ID    = DEFAULT_GROUP_ID - 2;
  LOGIC_OP_GROUP_ID         = DEFAULT_GROUP_ID - 3;

  DEFAULT_X_PIXEL_OFFSET = 20;
  DEFAULT_Y_PIXEL_OFFSET = 20;

  ADD_GROUPS: array[0..3] of TKRegGroupInfo =
    (
      ( Name   : '<All>';
        Comment: 'All group items';
        GroupID: DEFAULT_GROUP_ID ),

      ( Name   : 'Aritimetic';
        Comment: 'All aritimetic operators (+, -, *, /, ^, etc)';
        GroupID: ARITIMETIC_OP_GROUP_ID ),

      ( Name   : 'Logic';
        Comment: 'All logic operators (and, or, xor). Note that "not" is a function';
        GroupID: LOGIC_OP_GROUP_ID ),

      ( Name   : 'Components';
        Comment: 'Component properties registered for expressions';
        GroupID: COMPONENTS_GROUP_ID )

    );

  GROUP_STATUS_BAR_TEXT: array[TKMathType] of string =
    ( 'Operator Groups', 'Function Groups', 'Identifier Groups', 'Component Groups' );

  TREENODE_CHILD_HELP: array[FUNCTION_IMAGE_IDX..COMPONENT_IDENT_IMAGE_IDX] of string =
    ( 'Available Functions', 'Available Identifiers', 'Math Operators',
      'Registered Functions', 'Registered Identifiers', 'Available Component Properties' );

  MATH_OP_CHARS = ['+', '-', '*', '/', '^', '(', ')'];
  MATH_OP_TEXT: array[TKMathOp] of string =
    ( '', '+', '-', '*', '/', 'mod', 'div', '^', 'shl', 'shr', 'or', 'and', 'xor' );

{ Utility Functions }

function EditExpression( const Caption: string; AOwner: TComponent; CompProps: TStrings;
  FillFuncParams: Boolean; var Expression: string; Corner: PPoint ): Boolean;
var
  frmAddExpr: TfrmAddExpr;
begin
  ForceObject( AOwner );
  frmAddExpr := TfrmAddExpr.Create( nil );
  try
    frmAddExpr.OwnerComp := AOwner;
    frmAddExpr.Caption := Format( frmAddExpr.Caption, [Caption] );
    frmAddExpr.cbFillFuncParams.Checked := FillFuncParams;
    if CheckObject( CompProps ) then
      frmAddExpr.FCompProps.Assign( CompProps );
    frmAddExpr.moExpr.Lines.Text := Expression;
    if CheckTrimStr( Expression ) then
      frmAddExpr.ActiveControl := frmAddExpr.moExpr;
    if ( CheckPointer( Corner ) and ( Corner^.x <> -1 ) and ( Corner^.y <> -1 ) ) then
    begin
      frmAddExpr.Top := Corner^.y;
      frmAddExpr.Left := Corner^.x;
    end
    else
      frmAddExpr.Position := poScreenCenter;
    Result := ( frmAddExpr.ShowModal = mrOk );
    if Result then
    begin
      Expression := frmAddExpr.Expression;
      if CheckPointer( Corner ) then
      begin
        Corner^.y := frmAddExpr.Top;
        Corner^.x := frmAddExpr.Left;
      end;
    end;  
  finally
    frmAddExpr.Free;
  end;
end;

{ Form Methods }

function TfrmAddExpr.GetCount( GetIdx: Integer ): Integer;
begin
  case GetIdx of
    0: Result := FRegGroups.Count;
    1: Result := FInternalGroups.Count;
    2: Result := FRegFuncs.Count;
    3: Result := FRegIdents.Count;
    4: Result := FComps.Count;
    5: Result := FCompProps.Count;
  else
    Result := 0;
  end;
end;

function TfrmAddExpr.GetGroups( Index: Integer ): TKMathExprRegGroup;
begin
  Result := TKMathExprRegGroup( FRegGroups[Index] );
end;

function TfrmAddExpr.GetInternalGroups( Index: Integer ): TKMathExprRegGroup;
begin
  Result := TKMathExprRegGroup( FInternalGroups.Objects[Index] );
end;

function TfrmAddExpr.GetFunctions( Index: Integer ): TKMathExprRegFunc;
begin
  Result := TKMathExprRegFunc( FRegFuncs[Index] );
end;

function TfrmAddExpr.GetIdentifiers( Index: Integer ): TKMathExprRegIdent;
begin
  Result := TKMathExprRegIdent( FRegIdents[Index] );
end;

function TfrmAddExpr.GetCompExprNames( Index: Integer ): string;
begin
  Result := FComps.Names[Index];
end;

function TfrmAddExpr.GetCompExprValues( Index: Integer ): string;
begin
  Result := FComps.Values[FComps.Names[Index]];
end;

function TfrmAddExpr.GetCompProps( Index: Integer ): string;
begin
  Result := FCompProps[Index];
end;

function TfrmAddExpr.GetExpression: string;
begin
  Result := Trim( moExpr.Text );
end;

procedure TfrmAddExpr.FillGroups( InfoType: TKMathType );
var
  i: Integer;
begin
  lbGroups.Items.BeginUpdate;
  try
    lbItems.Items.BeginUpdate;
    try
      lbItems.Items.Clear;
    finally
      lbItems.Items.EndUpdate;
    end;
    lbGroups.Items.Clear;
    lbGroups.Items.AddObject( InternalGroups[0].GroupName, InternalGroups[0] );
    case InfoType of
      mtOp   :
      begin
        lbGroups.Items.AddObject( InternalGroups[1].GroupName, InternalGroups[1] );
        lbGroups.Items.AddObject( InternalGroups[2].GroupName, InternalGroups[2] );
      end;
      mtFunc,
      mtIdent:
        for i := 0 to GroupCount - 1 do
          lbGroups.Items.AddObject( Groups[i].GroupName, Groups[i] );
      mtComp :
        for i := 0 to FCompProps.Count - 1 do
          lbGroups.Items.AddObject( FCompProps[i], InternalGroups[3] );
    end;
  finally
    lbGroups.Items.EndUpdate;
  end;
end;

procedure TfrmAddExpr.FillMathOps( GroupID: ShortInt );
var
  i: TKMathOp;
begin
  case GroupID of
    DEFAULT_GROUP_ID:
      for i := Succ( Low( TKMathOp ) ) to High( TKMathOp ) do
        lbItems.Items.Add( MATH_OP_TEXT[i] );
    ARITIMETIC_OP_GROUP_ID:
    begin
      for i := moPlus to moDiv do
        lbItems.Items.Add( MATH_OP_TEXT[i] );
      for i := moPower to moShr do
        lbItems.Items.Add( MATH_OP_TEXT[i] );
    end;
    LOGIC_OP_GROUP_ID     :
      for i := moOr to moXor do
        lbItems.Items.Add( MATH_OP_TEXT[i] );
  end;
  lbItems.Items.Add( CH_PARENTHESIS_OPEN );
  lbItems.Items.Add( CH_PARENTHESIS_CLOSE );
end;

procedure TfrmAddExpr.FillItemStrings( Group: TKMathExprRegGroup; MathType: TKMathType );
var
  i: Integer;
begin
  lbItems.Items.BeginUpdate;
  try
    lbItems.Items.Clear;
    sbStatus.SimpleText := GROUP_STATUS_BAR_TEXT[MathType];
    case MathType of
      mtOp   : FillMathOps( Group.GroupID );
      mtFunc :
        for i := 0 to FuncCount - 1 do
          if ( ( Group.GroupID = DEFAULT_GROUP_ID ) or ( Functions[i].GroupID = Group.GroupID ) ) then
            lbItems.Items.AddObject( Functions[i].FuncName, Functions[i] );
      mtIdent:
        for i := 0 to IdentCount - 1 do
          if ( ( Group.GroupID = DEFAULT_GROUP_ID ) or ( Identifiers[i].GroupID = Group.GroupID ) ) then
            lbItems.Items.AddObject( Identifiers[i].IdentName, Identifiers[i] );
      mtComp :
        if ( FLastGroupIndex <> -1 ) then
          for i := 0 to CompCount - 1 do
            if ( ( Group.GroupID = DEFAULT_GROUP_ID ) or CheckStrEqual( CompProps[FLastGroupIndex - 1], CompExprValues[i] ) ) then
              lbItems.Items.AddObject( CompExprNames[i], TObject( COMPONENTS_GROUP_ID ) );
    end;
  finally
    lbItems.Items.EndUpdate;
  end;
end;

procedure TfrmAddExpr.FillRegInfos;
var
  i, j: Integer;
  pi: PPropInfo;
begin
  TKCustomMathSolver.EnumRegInfos( mertGroup, FRegGroups, EnumRegInfos );
  TKCustomMathSolver.EnumRegInfos( mertFunc, FRegFuncs, EnumRegInfos );
  TKCustomMathSolver.EnumRegInfos( mertIdent, FRegIdents, EnumRegInfos );
(*
  FCompProps.Clear;
  {.$IFNDEF DESIGN_TIME_TEST}
    EnumRegMathPropEditors( FCompProps, EnumMathPropertyEdt );
  {.$ELSE}
    FCompProps.AddObject( 'Left', TypeInfo( Integer ) );
    FCompProps.AddObject( 'Top', TypeInfo( Integer ) );
    FCompProps.AddObject( 'Height', TypeInfo( Integer ) );
    FCompProps.AddObject( 'Width', TypeInfo( Integer ) );
  {.$ENDIF}
*)
  if CheckStrings( FCompProps ) then
    for i := 0 to FOwnerComp.ComponentCount - 1 do
      for j := 0 to FCompProps.Count - 1 do
      begin
        pi := GetPropInfo( FOwnerComp.Components[i].ClassInfo, FCompProps[j] );
        if ( CheckPointers( [pi, PTypeInfo( FCompProps.Objects[j] )] ) and
          ( pi^.PropType^^.Kind = PTypeInfo( FCompProps.Objects[j] )^.Kind ) ) then
          FComps.Add( FOwnerComp.Components[i].Name + CH_DOTMARK + FCompProps[j] +
            CH_EQUAL_TOKEN + FCompProps[j] );
      end;
end;

function TfrmAddExpr.EnumRegInfos( Index: Integer; RegInfoType: TKMathExprRegType;
  RegItem: TKMathExprRegTerm; Data: Pointer ): Boolean;
begin
  TList( Data ).Add( RegItem );
  Result := True;
end;

procedure TfrmAddExpr.BuildTreeView;
var
  n, nF, nI, nO: TTreeNode;
begin
  trvRegItems.Items.BeginUpdate;
  try
    trvRegItems.Items.Clear;
    nF := trvRegItems.Items.Add( nil, sFunctions );
    FLastNode := nF;
    nF.ImageIndex := FUNCTION_IMAGE_IDX;
    nF.SelectedIndex := FUNCTION_IMAGE_IDX;
    nI := trvRegItems.Items.Add( nil, sIdentifiers );
    nI.ImageIndex := IDENTIFIERS_IMAGE_IDX;
    nI.SelectedIndex := IDENTIFIERS_IMAGE_IDX;
    nO := trvRegItems.Items.Add( nil, sMathOperators );
    nO.ImageIndex := OPERATORS_IMAGE_IDX;
    nO.SelectedIndex := OPERATORS_IMAGE_IDX;
    n := trvRegItems.Items.AddChildObject( nF, sRegistered, FRegFuncs );
    n.ImageIndex := REGISTERED_FUNC_IMAGE_IDX;
    n.SelectedIndex := REGISTERED_FUNC_IMAGE_IDX;
    n := trvRegItems.Items.AddChildObject( nI, sRegistered, FRegIdents );
    n.ImageIndex := REGISTERED_IDENT_IMAGE_IDX;
    n.SelectedIndex := REGISTERED_IDENT_IMAGE_IDX;
    if CheckStrings( FCompProps ) then
    begin
      n := trvRegItems.Items.AddChildObject( nI, sComponents, FComps );
      n.ImageIndex := COMPONENT_IDENT_IMAGE_IDX;
      n.SelectedIndex := COMPONENT_IDENT_IMAGE_IDX;
    end;  
  finally
    trvRegItems.Items.EndUpdate;
  end;
end;

type
  TKMathExprRegGroupHack = class( TKMathExprRegGroup );

procedure TfrmAddExpr.BuildInternalGroups;
var
  i: Byte;
begin
  for i := 0 to 3 do
    FInternalGroups.AddObject( ADD_GROUPS[i].Name, TKMathExprRegGroupHack.Create(
      ADD_GROUPS[i].GroupID, ADD_GROUPS[i].Name, ADD_GROUPS[i].Comment ) );
end;

function TfrmAddExpr.BuildFunctionText( FuncInfo: TKMathExprRegFunc ): string;
var
  i: Integer;
  s: string;
  p: TPoint;
begin
  Result := FuncInfo.FuncName;
  if ( FuncInfo.ParamCount > 0 ) then
  begin
    Result := Result + CH_PARENTHESIS_OPEN + CH_SPACE;
    if cbFillFuncParams.Checked then
    begin
      p.x := Left;
      p.y := Top;
      for i := 0 to FuncInfo.ParamCount - 1 do
      begin
        Inc( p.x, DEFAULT_X_PIXEL_OFFSET );
        Inc( p.y, DEFAULT_Y_PIXEL_OFFSET );
        if ( EditExpression( Format( sFunParamEval, [FuncInfo.FuncName, ( i + 1 ),
          FuncInfo.ParamNames[i]] ), OwnerComp, FCompProps,
          cbFillFuncParams.Checked, s, @p ) and CheckTrimStr( s ) ) then
          Result := Result + s + CH_COMMA + CH_SPACE
        else
          Result := Result + CH_LOWERTHAN + CH_LOWERTHAN + CH_SPACE +
            FuncInfo.ParamNames[i] + CH_SPACE + CH_GREATERTHAN + CH_GREATERTHAN +
            CH_COMMA + CH_SPACE
      end;
    end
    else
      for i := 0 to FuncInfo.ParamCount - 1 do
        Result := Result + CH_LOWERTHAN + CH_LOWERTHAN + CH_SPACE +
          FuncInfo.ParamNames[i] + CH_SPACE + CH_GREATERTHAN + CH_GREATERTHAN +
          CH_COMMA + CH_SPACE;
    if CheckStr( Result ) then
      Delete( Result, Length( Result ) - 1, 1 ); { remove last comma }
    Result := Result + CH_PARENTHESIS_CLOSE;
  end
end;

function TfrmAddExpr.BuildHelp( Control: TWinControl ): string;
const
  LISTBOX_GROUP_TAG = -5;
  LISTBOX_ITEMS_TAG = -6;
begin
  Result := '';
  if ( CheckObjectClass( Control, TTreeView ) and CheckObject( FLastNode ) ) then
    Result := TREENODE_CHILD_HELP[FLastNode.ImageIndex]
  else if CheckObjectClass( Control, TListBox ) then
    with ( Control as TListBox ) do
      case Tag of
        LISTBOX_ITEMS_TAG: Result := TKMathExprRegTerm( Items.Objects[ItemIndex] ).Comment;
        LISTBOX_GROUP_TAG: Result := TKMathExprRegGroup( Items.Objects[ItemIndex] ).Comment;
      end
  else if CheckObjectClass( Control, TWinControl ) then
    Result := Control.Hint;
end;

{ Form Events }

{ Main Events }

procedure TfrmAddExpr.FormCreate(Sender: TObject);
begin
  FRegGroups := TList.Create;
  FRegFuncs := TList.Create;
  FRegIdents := TList.Create;
  FComps := TStringList.Create;
  FCompProps := TStringList.Create;
  FInternalGroups := TKMathExprTermStrings.Create;
  moExpr.Clear;
  FLastGroupIndex := -1;
  BuildInternalGroups;
end;

procedure TfrmAddExpr.FormDestroy(Sender: TObject);
begin
  FRegGroups.Clear;
  FRegFuncs.Clear;
  FRegIdents.Clear;
  FComps.Clear;
  FCompProps.Clear;
  FInternalGroups.Clear;
  FRegGroups.Free;
  FRegFuncs.Free;
  FRegIdents.Free;
  FComps.Free;
  FCompProps.Free;
  FInternalGroups.Free;
end;

procedure TfrmAddExpr.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ( ( ModalResult = mrOk ) or ( ( ModalResult = mrCancel ) and (
    ( not moExpr.Modified ) or ( moExpr.Modified and Confirm( sCancelExpr ) ) ) ) );
end;

procedure TfrmAddExpr.FormShow(Sender: TObject);
begin
  ForceObject( FOwnerComp );
  BuildTreeView;
  FillRegInfos;
  if CheckObject( FLastNode ) then
    FLastNode.Selected := True;
  if CheckTrimStr( moExpr.Text ) then
    moExpr.SelectAll;
end;

{ TreeView }

procedure TfrmAddExpr.trvRegItemsChange(Sender: TObject; Node: TTreeNode);
begin
  FLastNode := Node;
  lbGroups.Items.Clear;
  lbItems.Items.Clear;
  FLastGroupIndex := -1;
  sbStatus.SimpleText := TREENODE_CHILD_HELP[FLastNode.ImageIndex];
  case FLastNode.ImageIndex of
    FUNCTION_IMAGE_IDX,
    IDENTIFIERS_IMAGE_IDX: { do nothing };
    OPERATORS_IMAGE_IDX,
    REGISTERED_FUNC_IMAGE_IDX,
    REGISTERED_IDENT_IMAGE_IDX,
    COMPONENT_IDENT_IMAGE_IDX: FillGroups( TKMathType( FLastNode.ImageIndex - OPERATORS_IMAGE_IDX ) );
  end;
end;

{ ListBox Events }

procedure TfrmAddExpr.lbGroupsClick(Sender: TObject);
begin
  sbStatus.SimpleText := '';
  FLastGroupIndex := lbGroups.ItemIndex;
  if ( ( FLastGroupIndex <> - 1 ) and CheckObject( FLastNode ) ) then
    case FLastNode.ImageIndex of
      FUNCTION_IMAGE_IDX,
      IDENTIFIERS_IMAGE_IDX:
        lbItems.Items.Clear; { should not happen! }
      OPERATORS_IMAGE_IDX,
      REGISTERED_FUNC_IMAGE_IDX,
      REGISTERED_IDENT_IMAGE_IDX,
      COMPONENT_IDENT_IMAGE_IDX:
      { TListBoxStrings when issueing a GetObject, it calls GetItemData and after
        that check Result for -1 (DEFAULT_GROUP_ID - the value I put there) and
        raise an exception. Se we need to circunvent this problem this way bellow!
        We now use group references, there is no more problems at all }
        
        FillItemStrings( TKMathExprRegGroup( lbGroups.Items.Objects[FLastGroupIndex] ),
          TKMathType( FLastNode.ImageIndex - OPERATORS_IMAGE_IDX ) );
    end;
end;

procedure TfrmAddExpr.lbItemsClick(Sender: TObject);
begin
  sbStatus.SimpleText := '';
  if ( ( lbItems.ItemIndex <> - 1 ) and CheckObject( FLastNode ) ) then
    case FLastNode.ImageIndex of
      REGISTERED_FUNC_IMAGE_IDX:
        sbStatus.SimpleText := TKMathExprRegFunc( lbItems.Items.Objects[lbItems.ItemIndex] ).Formula;
      REGISTERED_IDENT_IMAGE_IDX:
        sbStatus.SimpleText := TKMathExprRegIdent( lbItems.Items.Objects[lbItems.ItemIndex] ).Comment;
      COMPONENT_IDENT_IMAGE_IDX:
        if ( LongInt( lbItems.Items.Objects[lbItems.ItemIndex] ) = COMPONENTS_GROUP_ID ) then
          sbStatus.SimpleText := InternalGroups[3].Comment;
    end;
end;

procedure TfrmAddExpr.lbItemsDblClick(Sender: TObject);
begin
  if ( ( lbItems.ItemIndex <> -1 ) and CheckObject( FLastNode ) ) then
  begin
    if ( FLastNode.ImageIndex = REGISTERED_FUNC_IMAGE_IDX ) then
      moExpr.Text := Trim( moExpr.Text + CH_SPACE + BuildFunctionText(
        TKMathExprRegFunc( lbItems.Items.Objects[lbItems.ItemIndex] ) ) )
    else
      moExpr.Text := Trim( moExpr.Text + CH_SPACE + lbItems.Items[lbItems.ItemIndex] );
    moExpr.SelStart := Length( moExpr.Text );
    if moExpr.CanFocus then
      moExpr.SetFocus;
  end;
end;

procedure TfrmAddExpr.lbItemsKeyPress(Sender: TObject; var Key: Char);
begin
  if ( Key in MATH_OP_CHARS ) then
  begin
    moExpr.Text := Trim( moExpr.Text + CH_SPACE + Key );
    moExpr.SelStart := Length( moExpr.Text );
  end;  
end;

{ Main Button Events }

procedure TfrmAddExpr.sbHelpClick(Sender: TObject);
begin
  Inform( GetFirstString( [BuildHelp( ActiveControl ), sNoHelp] ) );
end;

{ Math Op Button Events }

procedure TfrmAddExpr.sbAllMathOpClick(Sender: TObject);
const
  PARENTHESIS_OPEN_TAG  = -2;
  PARENTHESIS_CLOSE_TAG = -1;
  PARENTHESIS: array[PARENTHESIS_OPEN_TAG..PARENTHESIS_CLOSE_TAG] of Char =
    ( CH_PARENTHESIS_OPEN, CH_PARENTHESIS_CLOSE );
begin
  with ( Sender as TSpeedButton ) do
    case Tag of
      PARENTHESIS_OPEN_TAG,
      PARENTHESIS_CLOSE_TAG:
        moExpr.Text := Trim( moExpr.Text + CH_SPACE + PARENTHESIS[Tag] );
    else
      if ValueBetween( Tag, Integer( Succ( Low( TKMathOp ) ) ), Integer( High( TKMathOp ) ), True ) then
        moExpr.Text := Trim( moExpr.Text + CH_SPACE + MATH_OP_TEXT[TKMathOp( Tag )] );
    end;
  moExpr.SelStart := Length( moExpr.Text );
  if moExpr.CanFocus then
    moExpr.SetFocus;
end;

procedure TfrmAddExpr.sbClearClick(Sender: TObject);
begin
  moExpr.Lines.Clear;
  if moExpr.CanFocus then
    moExpr.SetFocus;
end;

{ Drag'n Drop Events }

procedure TfrmAddExpr.moExprDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := CheckObjectClass( Source, TListBox );
end;

procedure TfrmAddExpr.moExprDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  lbItemsDblClick( Source );
end;

end.


