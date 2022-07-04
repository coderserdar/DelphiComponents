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

unit ukrdfCharSet;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons,
	ExtCtrls, uksyTypes, ukrClasses;

type

	TfrmCharSet = class( TForm )
		DstList: TListBox;
		OKBtn: TBitBtn;
		CancelBtn: TBitBtn;
		HelpBtn: TBitBtn;
		ComboType: TComboBox;
		PanelListBoxes: TPanel;
		SrcEspecialList: TListBox;
		SrcListUpperAlpha: TListBox;
		SrcListAlpha: TListBox;
    SrcListNum: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
		LSourceType: TLabel;
    PanelButtons: TPanel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExcAllBtn: TSpeedButton;
    AddAllBtn: TSpeedButton;
		procedure IncludeBtnClick( Sender: TObject );
		procedure ExcludeBtnClick( Sender: TObject );
		procedure IncAllBtnClick( Sender: TObject );
		procedure ExcAllBtnClick( Sender: TObject );
		procedure FormCreate( Sender: TObject );
		procedure ComboTypeChange( Sender: TObject );
		procedure FormDestroy( Sender: TObject );
    procedure AddAllBtnClick( Sender: TObject );
	private
		FRegs: TStrings;
		FOwner: TKPersistentCharSet;
		procedure PrepareList;
		procedure MoveSelected( List: TCustomListBox; Items: TStrings );
		procedure MoveSelectedEx( List: TCustomListBox );
		procedure SetItem( List: TListBox; Index: Integer );
		function GetFirstSelection( List: TCustomListBox ): Integer;
		procedure SetButtons;
		procedure GetCharSet( Value: TKPersistentCharSet );
		procedure SetCharSet( Value: TKPersistentCharSet );
		function GetListBoxByID( const ID: Integer ): TListBox;
		function GetListBoxByString( const Str: string ): TListBox;
		function GetMaxItemCount: Integer;
		procedure SetListBoxes( const ID: Integer );
		procedure SetButtonTags( const ID: Integer );
	public
		property CharSet: TKPersistentCharSet read FOwner;
		property RegCharSets: TStrings read FRegs;
		property MaxItemCount: Integer read GetMaxItemCount;
	end;

{##NI##}

function EditValidCharSet( const OwnerName: string;
	APersistentCharSet: TKPersistentCharSet ): Boolean;
function EditValidCharSetEx( const OwnerName: string;
	var ACharSet: TKCharSet ): Boolean;

procedure RegisterEspecialValidCharSet( const ACharSet: string );
procedure UnRegisterEspecialValidCharSet( const ACharSet: string );

implementation

uses
	uksyUtils, ukrUtils, ukrdConsts;

const
	cNum    = 0;
	cAlpha  = 1;
	cUAlpha = 2;
	cEsp    = 3;

{$R *.DFM}

function EditValidCharSetEx( const OwnerName: string; var ACharSet: TKCharSet ): Boolean;
var
	pcs: TKPersistentCharSet;
begin
	pcs := TKPersistentCharSet.Create( nil );
	try
		pcs.CharSet := ACharSet;
		Result := EditValidCharSet( OwnerName, pcs );
		if Result then
			ACharSet := pcs.CharSet;
	finally
		pcs.Free;
	end;
end;

function EditValidCharSet( const OwnerName: string;
	APersistentCharSet: TKPersistentCharSet ): Boolean;
var
	Form: TfrmCharSet;
begin
	ForceObject( APersistentCharSet );
	Form := TfrmCharSet.Create( nil );
	try
		Form.FOwner := APersistentCharSet;
		Form.Caption := Format( sCSFormCaption, [OwnerName] );
		Form.PrepareList;
		Form.SetCharSet( APersistentCharSet );
		Result := ( Form.ShowModal = mrOk );
		if Result then
			Form.GetCharSet( APersistentCharSet );
	finally
		Form.FOwner := nil;
		Form.Free;
	end;
end;

var
	CharSetList : TStringList = nil;

procedure TestCharSet( ss: TStrings );
var
	i: Integer;
begin
	for i:= 0 to ss.Count - 1 do
		if ( ( Length( ss[i] ) > 1 ) and ( ss[i][1] <> '#' ) ) then
			RaiseExceptionFmt( EKCharSet, sErrCSInvEspecialCharSet, [ss[i]] );
end;

procedure IncludeCharSetToCharSetList( ss: TStrings; const ACharSet: string );
var
	i: Integer;
	sl: TStrings;
begin
	sl := TStringList.Create;
	try
		ExtractStrings( ACharSet, ListSeparator, sl );
    TestCharSet( sl );
		for i := 0 to sl.Count - 1 do
			if ( ss.IndexOf( sl[i] ) = -1 ) then
				ss.Add( sl[i] );
	finally
		sl.Free;
	end;
end;

procedure ExcludeCharSetFromCharSetList( ss: TStrings; const ACharSet: string );
var
	sl: TStrings;
begin
	sl := TStringList.Create;
	try
		ExtractStrings( ACharSet, ListSeparator, sl );
		TestCharSet( sl );
		StringsDifference( sl, ss );
	finally
		sl.Free;
	end;
end;

procedure RegisterEspecialValidCharSet( const ACharSet: string );
begin
  ForceTrimStr( ACharSet );
	if ( not CheckObject( CharSetList ) ) then
	begin
		CharSetList := TStringList.Create;
		CharSetList.Duplicates := dupError;
	end;
	IncludeCharSetToCharSetList( CharSetList, ACharSet );
end;

procedure UnRegisterEspecialValidCharSet( const ACharSet: string );
begin
	if ( not CheckObject( CharSetList ) ) then
		Exit;
	ForceTrimStr( ACharSet );
	ExcludeCharSetFromCharSetList( CharSetList, ACharSet );
	if ( not CheckStrings( CharSetList ) ) then
		FreeClean( CharSetList );
end;

procedure TfrmCharSet.PrepareList;
begin
	SrcEspecialList.Clear;
	FRegs.Clear;
	if Assigned( CharSetList ) then
	begin
		FRegs.Assign( CharSetList );
		SrcEspecialList.Assign( CharSetList );
	end;	
end;

procedure TfrmCharSet.GetCharSet( Value: TKPersistentCharSet );
var
	i: Byte;
	j: Integer;
	iSet: TKCharSet;
begin
	iSet := [];
	for i:= 0 to High( Byte ) do
	begin
		j := SensitiveIndexOf( DstList.Items, Chr( i ) );
		if ( j <> -1 ) then
			{We don't want to exclude the already setted char set.
			 and ( CompareStr( Chr( I ), DstList.Items[J] ) = 0 ) then }
			Include( iSet, Chr( i ) );
	end;
	if ( iSet <> Value.CharSet ) then
		Value.CharSet := iSet;
end;

procedure TfrmCharSet.SetCharSet( Value: TKPersistentCharSet );
var
	i: Byte;
	j: Integer;
	lb: TListBox;
begin
  lb := nil;
	for i := 0 to High( Byte ) do
		if ( Chr( i ) in Value.CharSet ) then
		begin
			j := -2;
			lb := nil;
			case Chr( i ) of
				#0..#31,
			#127..#159: j := SensitiveIndexOf( FRegs, '#'+IntToStr( i ) );
				'0'..'9': lb := SrcListNum;
				'a'..'z': lb := SrcListAlpha;
				'A'..'Z': lb := SrcListUpperAlpha;
			else
				j := SensitiveIndexOf( FRegs, Chr( i ) ); { Is this correct ? Must test. }
			end;

{ if any List box was found, we are looking for a special char set... }
			if ( ( not CheckObject( lb ) ) and ( j <> -2 ) ) then
			begin
				if ( j <> -1 ) then
					lb := SrcEspecialList
				else
					RaiseExceptionFmt( EKCharSet, sErrCSInvEspecialCharSet, ['#' + IntToStr( i )] );
				j := SensitiveIndexOf( lb.Items, '#' + IntToStr( i ) );
			end
			else
				j := SensitiveIndexOf( lb.Items, Chr( i ) );

			lb.Selected[j] := true;
			SetButtonTags( lb.Tag );
			IncludeBtnClick( nil );
			
		end;
	i := 0;
	if CheckObject( lb ) then
	begin
		if CheckStrings( lb.Items ) then
			case lb.Tag of
				cNum   : i := cAlpha;
				cAlpha : i := cUAlpha;
				cUAlpha: i := cEsp;
				cEsp   : i := cNum;
			end
		else
			i := lb.Tag;
	end;		
	ComboType.ItemIndex := i;
	ComboTypeChange( nil );
end;

{---------------------------------- Eventos -----------------------------------}

function TfrmCharSet.GetMaxItemCount: Integer;
begin
	Result := ( Ord( 'z' ) - Ord( 'a' ) + 1 ) + ( Ord( 'Z' ) - Ord( 'A' ) + 1 ) +
						( Ord( '9' ) - Ord( '0' ) + 1 ) + FRegs.Count;
end;

function TfrmCharSet.GetListBoxByID( const ID: Integer ): TListBox;
begin
  Result := nil;
  case ID of
		cNum   : Result := SrcListNum;
		cAlpha : Result := SrcListAlpha;
		cUAlpha: Result := SrcListUpperAlpha;
		cEsp   : Result := SrcEspecialList;
		else
			RaiseExceptionFmt( EKCharSet, sErrCSInvListBoxID, [ID] );
	end;
	ForceObject( Result );
end;           

function TfrmCharSet.GetListBoxByString( const Str: string ): TListBox;
begin
	ForceTrimStr( Str );
	Result := nil;
	case Str[1] of
		'0'..'9': Result := SrcListNum;
		'a'..'z': Result := SrcListAlpha;
		'A'..'Z': Result := SrcListUpperAlpha;
		'#'     :
			if ( SensitiveIndexOf( FRegs, Str ) <> -1 ) then
				Result := SrcEspecialList
			else
				RaiseExceptionFmt( EKCharSet, sErrCSInvEspecialCharSet, [Str] );
	else
		RaiseExceptionFmt( EKCharSet, sErrCSInvEspecialCharSet, [Str] );
	end;
end;

procedure TfrmCharSet.IncludeBtnClick( Sender: TObject );
var
	i: Integer;
	lb: TListBox;
begin
	lb := GetListBoxByID( IncludeBtn.Tag );
	i := GetFirstSelection( lb );
	MoveSelected( lb, DstList.Items );
	if CheckObject( Sender ) then
		SetItem( lb, i );
end;

procedure TfrmCharSet.ExcludeBtnClick( Sender: TObject );
var
	i: Integer;
begin
	i := GetFirstSelection( DstList );
	MoveSelectedEx( DstList );
	if CheckObject( Sender ) then
		SetItem( DstList, i );
end;

procedure TfrmCharSet.IncAllBtnClick( Sender: TObject );
var
	i: Integer;
	lb: TListBox;
begin
	lb := GetListBoxByID( IncAllBtn.Tag );
	lb.Items.BeginUpdate;
	try
		for i := 0 to lb.Items.Count - 1 do
			lb.Selected[i] := true;
		MoveSelected( lb, DstList.Items );
		lb.Items.Clear;
	finally
	  lb.Items.EndUpdate;
	end;
	i := -1;
	case IncAllBtn.Tag of
		cNum   : i := cAlpha;
		cAlpha : i := cUAlpha;
		cUAlpha:
			if CheckStrings( SrcEspecialList.Items ) then
				i := cEsp
			else
				i := cNum;
		cEsp   : i := cNum;
	end;
	if ( i <> -1 ) then
	begin
		lb := GetListBoxByID( i );
		ComboType.ItemIndex := i;
		ComboTypeChange( nil );
	end;
	if CheckObject( Sender ) then
		SetItem( lb, 0 );
end;

procedure TfrmCharSet.ExcAllBtnClick( Sender: TObject );
var
	i: Integer;
begin
	for i := 0 to DstList.Items.Count - 1 do
		DstList.Selected[i] := True;
	MoveSelectedEx( DstList );
	DstList.Items.Clear;
  if CheckObject( Sender ) then
		SetItem( DstList, 0 );
end;

procedure TfrmCharSet.AddAllBtnClick( Sender: TObject );
var
	i: Integer;
	j: Byte;
	lb: TListBox;
begin
	for j := cNum to cEsp do
	begin
		SetButtonTags( j );
		lb := GetListBoxByID( j );
		lb.Items.BeginUpdate;
		try
			for i := 0 to lb.Items.Count - 1 do
				lb.Selected[i] := True;
			MoveSelected( lb, DstList.Items );
			lb.Items.Clear;
		finally
			lb.Items.EndUpdate;
		end;
	end;
	lb := GetListBoxByID( cNum );
	ComboType.ItemIndex := cNum;
	ComboTypeChange( nil );
	if CheckObject( Sender ) then
		SetItem( lb, 0 );
end;

procedure TfrmCharSet.MoveSelected( List: TCustomListBox; Items: TStrings );
var
  i: Integer;
begin
  ForceObjects( [List, Items] );
	for i := List.Items.Count - 1 downto 0 do
		if List.Selected[i] then
		begin
			Items.AddObject( List.Items[i], List.Items.Objects[i] );
			List.Items.Delete( i );
		end;
	ComboType.Enabled := ( Items.Count <> MaxItemCount );
end;

procedure TfrmCharSet.MoveSelectedEx( List: TCustomListBox );
var
	i: Integer;
	lb: TListBox;
begin
  ForceObject( List );
	lb := nil;
	for i:= List.Items.Count -1 downto 0 do
		if List.Selected[i] then
		begin
			lb := GetListBoxByString( List.Items[i] );
			if ( not CheckObject( lb ) ) then
				Exit;
			lb.Items.AddObject( List.Items[i], List.Items.Objects[i] );
			List.Items.Delete( i );
		end;
	ComboType.Enabled := ( List.Items.Count <> MaxItemCount );
	if ( ComboType.Enabled and CheckObject( lb ) ) then
	begin
		ComboType.ItemIndex := lb.Tag;
		ComboTypeChange( nil );
	end;
end;

procedure TfrmCharSet.SetButtons;
var
	SrcEmpty,
	DstEmpty: Boolean;
	lb: TListBox;
begin
	lb := GetListBoxByID( IncAllBtn.Tag );
	SrcEmpty := lb.Items.Count = 0;
	DstEmpty := DstList.Items.Count = 0;
	IncludeBtn.Enabled  := not SrcEmpty;
	IncAllBtn.Enabled   := not SrcEmpty;
  AddAllBtn.Enabled   := ComboType.Enabled;//(  DstList.Items.Count <= MaxItemCount  );
	ExcludeBtn.Enabled  := not DstEmpty;
	ExcAllBtn.Enabled   := not DstEmpty;
end;

function TfrmCharSet.GetFirstSelection( List: TCustomListBox ): Integer;
begin
  ForceObject( List );
	for Result := 0 to List.Items.Count - 1 do
		if List.Selected[Result] then
		  Exit;
	Result := LB_ERR;
end;

procedure TfrmCharSet.SetItem( List: TListBox; Index: Integer );
var
	MaxIndex: Integer;
begin
  ForceObject( List );
	with List do
	begin
		SetFocus;
		MaxIndex := ( List.Items.Count - 1 );
		if ( Index = LB_ERR ) then
		  Index := 0
		else if ( Index > MaxIndex ) then
		  Index := MaxIndex;
		Selected[Index] := True;
	end;
	SetButtons;
end;

procedure TfrmCharSet.SetListBoxes( const ID: Integer );

	function SwitchParentsVisible( Control : TWinControl; AVisible : Boolean ): Integer;
	var
		i: Integer;
	begin
	  ForceObject( Control );
		Control.Visible := AVisible;
		Result := 0;
		for i := 0 to Control.ControlCount - 1 do
			Control.Controls[i].Visible := AVisible;
	end;
	
begin
	SwitchParentsVisible( PanelListBoxes, False );
	PanelListBoxes.Visible := True;
	case ID of
		cNum   :
		begin
			SrcListNum.Visible := True;
			SrcLabel.FocusControl := SrcListNum;
		end;
		cAlpha :
		begin
			SrcListAlpha.Visible := True;
			SrcLabel.FocusControl := SrcListAlpha;
		end;
		cUAlpha:
		begin
			SrcListUpperAlpha.Visible := True;
			SrcLabel.FocusControl := SrcListUpperAlpha;
		end;
		cEsp   :
		begin
			SrcEspecialList.Visible := True;
			SrcLabel.FocusControl := SrcEspecialList;
		end;
	else
		RaiseExceptionFmt( EKCharSet, sErrCSInvListBoxID, [ID] );
	end;
end;

procedure TfrmCharSet.SetButtonTags( const ID: Integer );
var
  i: Integer;
begin
	if ( not ValueBetween( ID, cNum, cEsp, True ) ) then
		RaiseExceptionFmt( EKCharSet, sErrCSInvListBoxID, [ID] );
	for i:= 0 to PanelButtons.ControlCount - 1 do
		PanelButtons.Controls[i].Tag := ID;
end;

procedure TfrmCharSet.FormCreate( Sender: TObject );
begin
	ComboType.ItemIndex := cNum;
	FRegs := TStringList.Create;
	TStringList( FRegs ).Duplicates := dupError;
end;

procedure TfrmCharSet.ComboTypeChange( Sender: TObject );
begin
	if ( ComboType.ItemIndex <> -1 ) then
	begin
		if ( ( ComboType.ItemIndex = cEsp ) and ( not CheckStrings( FRegs ) ) and
			 ( not CheckStrings( SrcEspecialList.Items ) ) ) then
		begin
			ComboType.ItemIndex := cNum;
			SetListBoxes( cNum );
			SetButtonTags( cNum );
			SetButtons;
			RaiseExceptionFmt( EKCharSet, sErrCSInvEspecialCharSet, ['empty'] );
		end;
		SetListBoxes( ComboType.ItemIndex );
		SetButtonTags( ComboType.ItemIndex );
		SetButtons;
	end
	else
		ComboType.ItemIndex := cNum;
end;

procedure TfrmCharSet.FormDestroy( Sender: TObject );
begin
	FreeClean( FRegs );
end;

{
initialization
	RegisterEspecialValidCharSet( '-;_;=;+;*;/' );

finalization
	UnRegisterEspecialValidCharSet( '-;_;=;+;*;/' );
}

end.
