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

unit ukrdfStrArray;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Menus, StdCtrls, Buttons, Grids, ExtCtrls, ukrClasses;

type

	TOnHeaderEvent = procedure( Sender: TObject; Header: TStrings ) of object;

	TOnGridOptionsEvent = procedure ( Sender: TObject; var GridOptions: TGridOptions )
		of object;

	TEditorStateEvent = procedure ( Sender: TObject; MultiString: TKCustomStringsArray;
		State: TEditorState; var Handled: Boolean ) of object;

	TfrmStringsArrayEditor = class( TForm )
		PopupMenu: TPopupMenu;
		NewRuleItem: TMenuItem;
		EditRuleItem: TMenuItem;
		DeleteRuleItem: TMenuItem;
		N1: TMenuItem;
		LoadItem: TMenuItem;
		SaveItem: TMenuItem;
		PanelGrid: TPanel;
		NewRule: TBitBtn;
		DeleteRule: TBitBtn;
		EditRule: TBitBtn;
		Cancel: TBitBtn;
		Ok: TBitBtn;
		Help: TBitBtn;
		Save: TBitBtn;
		Load: TBitBtn;
		Clear: TBitBtn;
		RuleGrid: TStringGrid;
		procedure LoadClick( Sender: TObject );
		procedure SaveClick( Sender: TObject );
		procedure ClearClick( Sender: TObject );
		procedure HelpClick( Sender: TObject );
		procedure NewRuleClick( Sender: TObject );
		procedure DeleteRuleClick( Sender: TObject );
		procedure RuleGridClick( Sender: TObject );
		procedure RuleGridKeyDown( Sender: TObject; var Key: Word;
			Shift: TShiftState );
	private
		FOnHeader: TOnHeaderEvent;
		FOnGridOptions: TOnGridOptionsEvent;
		FOnEditorState: TEditorStateEvent;
		FGetColDataAsRowData: Boolean;
		{OBS: Caso o nº de Linhas/Colunas não esteja de acordo com o conteúdo da
					StringsArray, o mesmo será truncado para as especificações.}
		FStringsArray: TKCustomStringsArray;
		FRowToPlaceHeader: Integer;
		FState: TEditorState;
		FMulDiv: Byte;
		procedure GeTKCustomStringsArray( StrArray: TKCustomStringsArray );
		procedure SeTKCustomStringsArray( StrArray: TKCustomStringsArray );
		procedure PrepareGrid( ARows, ACols: Integer;
			StrArray: TKCustomStringsArray );
		procedure ClearRuleGridRows;

	protected
		function DoOnGridOptions: TGridOptions; dynamic;
		function DoOnHeader( Strings: TStrings ): Boolean; dynamic;

	public
		{Viabiliza a execução da CallBack em pontos estratégicos de acordo com as
		 características atuais do StrArray. No caso, ele avisa a todas as CallBacks.}
		function DoEditorStateChange( StrArray: TKCustomStringsArray;
			AState: TEditorState ): Boolean; virtual;

		procedure AddColumn;
		procedure DelLastColumn;

		{Calcula o DefaultWidth ( Result ) que deve ser utilizado em função de uma linha
		 ( TStrings ) passada como base de informações e, sendo RowToPlaceHeader, a linha que
		 deverá ser preenchida. Se for -1, apenas calcula.}
		function CalcMakeHeaderWidth( Header: TStrings; const RowToPlaceHeader: Integer ): Integer; virtual;

		property StringsArray: TKCustomStringsArray
						 read FStringsArray;
		property MStrEditorState: TEditorState
						 read FState;
		property MultDivTextWidthFactor: Byte
						 read FMulDiv write FMulDiv;

		property ColDataAsRowData: Boolean
						 read FGetColDataAsRowData write FGetColDataAsRowData;
		property OnHeader: TOnHeaderEvent
						 read FOnHeader write FOnHeader;
		property OnGridOptions: TOnGridOptionsEvent
						 read FOnGridOptions write FOnGridOptions;
		property OnEditorState: TEditorStateEvent
						 read FOnEditorState write FOnEditorState;

	end;

{##NI##}

function EdiTKCustomStringsArray( Source: TKCustomStringsArray; const Rows, Cols: Integer;
	ColsAsRows: Boolean; HeaderEvent: TOnHeaderEvent; GridEvent: TOnGridOptionsEvent;
	EditorState: TEditorStateEvent ): Boolean;

implementation

{$R *.DFM}

uses
  uksyResStr, uksyUtils, ukrResStr, ukrUtils, ukrdConsts;

const
	sDefMulDiv = 4;

function EdiTKCustomStringsArray( Source: TKCustomStringsArray;
	const Rows, Cols: Integer; ColsAsRows: Boolean; HeaderEvent: TOnHeaderEvent;
	GridEvent: TOnGridOptionsEvent; EditorState: TEditorStateEvent ): Boolean;
var
	Form: TFrmStringsArrayEditor;
	iRows, iCols: Integer;
begin
	ForceObject( Source );
	Form := TFrmStringsArrayEditor.Create( nil );
	try
		Form.ColDataAsRowData := ColsAsRows;
		Form.OnHeader := HeaderEvent;
		Form.OnGridOptions := GridEvent;
		Form.OnEditorState := EditorState;
		if ( not ColsAsRows ) then
		begin
			iRows := Rows;
			iCols := Cols;
		end
		else
		begin
			iRows := Cols;
			iCols := Rows;
		end;
		Form.PrepareGrid( iRows, iCols, Source );
		Form.SeTKCustomStringsArray( Source );
		Result := ( Form.ShowModal = mrOk );
		if Result then
			Form.GeTKCustomStringsArray( Source );
	finally
		Form.DoEditorStateChange( Source, esDestroying );
		Form.FStringsArray := nil;
		Form.Free;
	end;
end;

{ TFrmMultiStrListEditor }

function TfrmStringsArrayEditor.CalcMakeHeaderWidth( Header: TStrings;
	const RowToPlaceHeader: Integer ): Integer;
var
	i,
	j: Integer;
begin
	FRowToPlaceHeader := RowToPlaceHeader;
	Result := RuleGrid.DefaultColWidth;
	i := 0;
	while ( i <= ( RuleGrid.ColCount - 1 ) ) and ( i <= Header.Count - 1 ) do
	begin
		if CheckStr( Header.Strings[i] ) then
		begin
			{ Monta o cabeçalho ou apenas calcula, tudo vai depender de RowToPlaceHeader }
			if ( RowToPlaceHeader >= 0 ) then
				RuleGrid.Cells[i, RowToPlaceHeader] := Header.Strings[i];
			j := Canvas.TextWidth( Header.Strings[i] );
			j := j + ( j div FMulDiv ) + ( Length( Header.Strings[i] ) ); // +/-28% a mais
			if ( Result < j ) then
				Result := j;
		end;
		Inc( i );
	end;
end;

function TfrmStringsArrayEditor.DoEditorStateChange( StrArray: TKCustomStringsArray;
	AState: TEditorState ): Boolean;
begin
	{Percorre cada CallBack Registrada e avisa do evento ( Self.State )!}
	Result := True;
	if Assigned( FOnEditorState ) then
		FOnEditorState( Self, StrArray, AState, Result );
end;

function TfrmStringsArrayEditor.DoOnHeader( Strings: TStrings ): Boolean;
begin
	if Assigned( FOnHeader ) then
		FOnHeader( Self, Strings );
	Result := CheckStrings( Strings );
end;

function TfrmStringsArrayEditor.DoOnGridOptions: TGridOptions;
begin
	Result := RuleGrid.Options;
	if Assigned( FOnGridOptions ) then
		FOnGridOptions( Self, Result );
end;

procedure TfrmStringsArrayEditor.PrepareGrid( ARows, ACols: Integer;
	StrArray: TKCustomStringsArray );
var
	Header: TStrings;
	HasHeader: Boolean;
	{iAux: Integer;}
begin
	{Antes de mais nada faz os testes de validação}
	if ( not StrArray.FreeObjects ) then
		RaiseException( EKStringsArray, sErrMultiStrListNotOwned );
	FMulDiv := sDefMulDiv;
	FStringsArray := StrArray;
	FMulDiv := sDefMulDiv;
	{if ColDataAsRowData then
	begin
		iAux  := ARows;
		ARows := ACols;
		ACols := iAux;
	end;}
	if ( not DoEditorStateChange( StrArray, esPreparing ) ) then
	  Exit;
	Header := TStringList.Create;
	try
		HasHeader := DoOnHeader( Header );
		with RuleGrid do
		begin
			if ( ARows <= FixedRows ) then
				ARows := ARows + FixedRows;
			RowCount := ARows + FixedRows{+RowToPlaceHeader; //- AbrirDepois};
			if ( ACols <= FixedCols ) then
				ACols := ACols + FixedCols;
			ColCount := ACols + FixedCols;
			Options := DoOnGridOptions;
			{Se possuir cabeçalho, calcula o valor de largura Defaul das colunas do
			 cabeçalho segundo as strings do Header.}
			if HasHeader then
				DefaultColWidth := CalcMakeHeaderWidth( Header, 0 );
		end;
	finally
		Header.Free;
	end;
end;

procedure TfrmStringsArrayEditor.GeTKCustomStringsArray( StrArray: TKCustomStringsArray );

	function IsRowClear( const Index: Integer ): Boolean;
	begin
		// Calcula se a Rows[Index] é Clear. Se for, terá #13#10*ColCount;
		Result := ( Length( RuleGrid.Rows[Index].Text ) = ( RuleGrid.ColCount * 2 ) );
	end;

	function IsColClear( const Index: Integer ): Boolean;
	begin
		Result := ( Length( RuleGrid.Cols[Index].Text ) = ( RuleGrid.RowCount * 2 ) );
	end;

	function IsCellByRowClear( const ARow, Index: Integer ): Boolean;
	begin
		Result := ( not CheckStr( RuleGrid.Rows[ARow].Strings[Index] ) );
	end;

	function IsCellByColClear( const ACol, Index: Integer ): Boolean;
	begin
		Result := ( not CheckStr( RuleGrid.Cols[ACol].Strings[Index] ) );
	end;

var
	i,
	j,
	k: Integer;
begin
	StrArray.Clear;
	if ( not ColDataAsRowData ) then
		for i:= RuleGrid.FixedRows to RuleGrid.RowCount - 1 do // Tira a primeira linha.
		begin
			if ( not IsRowClear( i ) ) then
			begin
				k := StrArray.Add( nil, '' );
				try
					for j := RuleGrid.FixedCols to RuleGrid.ColCount - 1 do
						if ( not IsCellByRowClear( i, j ) ) then
							StrArray.Rows[k].Add( RuleGrid.Rows[i].Strings[j] );
				except
					StrArray.Delete( k );
					raise;
				end;
			end;
		end
	else
		for i := RuleGrid.FixedCols to RuleGrid.ColCount - 1 do
		begin
			if ( not IsColClear( i ) ) then
			begin
				k := StrArray.Add( nil, '' );
				try
					for j := RuleGrid.FixedRows to RuleGrid.RowCount - 1 do
						if ( not IsCellByColClear( i, j ) ) then
							StrArray.Rows[k].Add( RuleGrid.Cols[i].Strings[j] );
				except
					StrArray.Delete( k );
					raise;
				end;
			end;
		end;
	{Aqui ocorre após todo o StrArray estar setado.}
	{if not} DoEditorStateChange( StrArray, esGetting ) {then
	  Exit;}	
end;

procedure TfrmStringsArrayEditor.SeTKCustomStringsArray( StrArray: TKCustomStringsArray );
var
	i,
	j: Integer;
begin
	{Aqui caso StrArray tenha mais linhas e/ou mais colunas, as mesmas serão
	 truncadas. Da mesma maneira, se tiverem menos, o grid aparecerá vazio.}
	if ( not DoEditorStateChange( StrArray, esSetting ) ) then
		Exit;

	{É assumido que o teste de RuleGrid.RowCount > 0 e RowCount <= I já foram feitos}
	for i:= 0 to StrArray.RowCount - 1 do
		for j:= 0 to StrArray.Rows[i].Count - 1 do
			if ( not ColDataAsRowData ) then
				RuleGrid.Cells[j, i + RuleGrid.FixedRows] := StrArray.Rows[i].Strings[j]
			else
				RuleGrid.Cells[i, j + RuleGrid.FixedRows] := StrArray.Rows[i].Strings[j];
end;

procedure TfrmStringsArrayEditor.AddColumn;
begin
	if DoEditorStateChange( StringsArray, esAddCol ) then
		RuleGrid.ColCount := RuleGrid.ColCount + 1;
end;

procedure TfrmStringsArrayEditor.DelLastColumn;
begin
	if DoEditorStateChange( StringsArray, esDelCol ) then
	begin
		RuleGrid.Cols[RuleGrid.ColCount - 1].Clear;
		RuleGrid.ColCount := ( RuleGrid.ColCount - 1 );
	end;
end;

procedure TfrmStringsArrayEditor.ClearRuleGridRows;
var
	i : Integer;
begin
	for i := 0 to RuleGrid.RowCount - 1 do
		if ( i <> FRowToPlaceHeader ) then
			RuleGrid.Rows[i].Clear;
end;

{------------------------------------ Eventos ---------------------------------}

procedure TfrmStringsArrayEditor.LoadClick( Sender: TObject );
var
	s: string;
	StrArray: TKCustomStringsArray;
begin
	s := SelectLoadFile( sMultiStrListFilter, sMultiStrListTitle, CurrentProjectPath,
		sMultiStrListDefExt );
	if CheckFile( s ) then
	begin
		if ( RuleGrid.RowCount > 2 ) and ( ShowDialog( sConfirmation, sMultiStrListConfirmClear,
			nil, dsYesNo, boQuestion01 ) = mrNo ) then
			Exit;
		Screen.Cursor := crHourGlass;
		try
			StrArray:= TKStringsArray.Create( True );
			try
				StrArray.LoadFromFile( s );
				ClearRuleGridRows;
				SeTKCustomStringsArray( StrArray );
			finally
				StrArray.Free;
			end;
		finally
			Screen.Cursor := crDefault;
		end;
	end;
	RuleGrid.SetFocus;
end;

procedure TfrmStringsArrayEditor.SaveClick( Sender: TObject );
var
	s: string;
	StrArray: TKCustomStringsArray;
begin
	s := SelectSaveFile( sMultiStrListFilter, sMultiStrListTitle, CurrentProjectPath,
		sMultiStrListDefExt );
	if CheckStr( s ) then
	begin
		if CheckFile( s ) and ( ShowDialogFmt( sConfirmation, sOverrideFile, nil,
			dsYesNo, boQuestion01, [s] ) = mrNo ) then
			Exit;
		Screen.Cursor := crHourGlass;
		try
			StrArray:= TKStringsArray.Create( True );
			try
				GeTKCustomStringsArray( StrArray );
				StrArray.SaveToFile( s );
			finally
				StrArray.Free;
			end;
		finally
			Screen.Cursor := crDefault;
		end;
	end;
	RuleGrid.SetFocus;
end;

procedure TfrmStringsArrayEditor.ClearClick( Sender: TObject );
begin
	if ( ShowDialog( sConfirmation, sMultiStrListConfirmClear,
		nil, dsYesNo, boQuestion01 ) = mrNo ) then
		Exit;
	if ( not DoEditorStateChange( StringsArray, esCleaning ) ) then
	  Exit;
	ClearRuleGridRows;
	if ( FRowToPlaceHeader <= 1 ) then
		RuleGrid.RowCount := 2
	else
		RuleGrid.RowCount := ( FRowToPlaceHeader + 1 );
	StringsArray.Clear;
	RuleGrid.SetFocus;
	if CheckDesigner( Designer ) then
	  Designer.Modified;
end;

procedure TfrmStringsArrayEditor.HelpClick( Sender: TObject );
begin
	{Caso ninguém mapei ele mostra o help padrão.}
	if DoEditorStateChange( StringsArray, esHelp ) then
    NotYetImplemented;
	RuleGrid.SetFocus;
end;

procedure TfrmStringsArrayEditor.NewRuleClick( Sender: TObject );
begin
	{Se alguém já fez o trabalho ( Result ser False ), ele não faz nada, se
	 ninguém o fez, ele adiciona a linha.}
	if DoEditorStateChange( StringsArray, esAddRow ) then
		RuleGrid.RowCount  := ( RuleGrid.RowCount + 1 );
	RuleGrid.SetFocus;
end;

procedure TfrmStringsArrayEditor.DeleteRuleClick( Sender: TObject );
var
	NumDelete,
	i,
	j: Integer;
begin
	if ( ShowDialog( sConfirmation, sMultiStrListConfirmLineDeletion,
		nil, dsYesNo, boQuestion01 ) = mrNo ) then
		Exit;
	if DoEditorStateChange( StringsArray, esDelRow ) then
	begin
		with RuleGrid do
		begin
			NumDelete := ( Selection.Bottom - Selection.Top + 1 );
			for i := Selection.Top to RowCount - 1 do
				for j := 0 to ColCount - 1 do
					Cells[j, i] := Cells[j, i + NumDelete];
			{ Não deletar a última linha.}
			//if ( RowCount - NumDelete ) = 1 then Dec( NumDelete );
			RowCount := ( RowCount - NumDelete );
			SetFocus;
		end;
	end;
end;

procedure TfrmStringsArrayEditor.RuleGridClick( Sender: TObject );
begin
{
	with RuleGrid do
	begin
		EditRule.Enabled  := ( Selection.Top < RowCount-1 ) and
												 ( Selection.Top = Selection.Bottom );
		DeleteRule.Enabled := ( Selection.Top < RowCount );
	end;
}
end;

procedure TfrmStringsArrayEditor.RuleGridKeyDown( Sender: TObject;
	var Key: Word; Shift: TShiftState );
begin
	case Key of
		VK_INSERT, VK_DOWN:
			if ( RuleGrid.Selection.Bottom = RuleGrid.RowCount - 1 ) then
				NewRuleClick( Sender );
		VK_DELETE         :
			if ( ssShift in Shift ) then
			  DeleteRuleClick( Sender );
		VK_RIGHT          :
			if ( ssShift in Shift ) then
				AddColumn;
		VK_LEFT           :
			if ( ssCtrl in Shift ) then
				DelLastColumn;
	end;
end;

end.

