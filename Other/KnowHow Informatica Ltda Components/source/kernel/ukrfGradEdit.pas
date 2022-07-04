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

unit ukrfGradEdit;

{$I s:\v100\include\iKLIB100.inc}
{$R+}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs,
	StdCtrls, uksyClasses, ExtCtrls, Buttons, ExtDlgs;

type
	TfrmGradientProps = class(TForm)
		pnGradProps: TPanel;
		grpGradProps: TGroupBox;
		lbBeginColor: TLabel;
		lbBitmap: TLabel;
		lbEndColor: TLabel;
		lbSteps: TLabel;
		lbGradientStyle: TLabel;
		imBitmap: TImage;
    kcxBeginColor: TComboBox;
    kcxEndColor: TComboBox;
		kcxGradientStyle: TComboBox;
    ksiSteps: TEdit;
    pnButtons: TPanel;
    pnGradient: TPanel;
    bnCancel: TBitBtn;
    bnOk: TBitBtn;
    opd: TOpenPictureDialog;
		cd: TColorDialog;
    bvImage: TBevel;
    pmBitmap: TPopupMenu;
    miSelectBitmap: TMenuItem;
		miClearBitmap: TMenuItem;
    pbGrad: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure kcxColorClick(Sender: TObject);
    procedure ksiStepsChange(Sender: TObject);
    procedure imBitmapDblClick(Sender: TObject);
    procedure kcxColorDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
    procedure miClearBitmapClick(Sender: TObject);
		procedure kcxColorKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure pbGradPaint(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	private
		FGradient: TKGradient;
		FRefreshingProps: Boolean;

		procedure RefreshGradientProps;
		procedure GetColorsProc( const Color: string );
		function GetColorRegKey( ATag: Integer ): string;
		function CallColorDialog( Sender: TComboBox; var Color: TColor ): Boolean;

	public
		property Gradient: TKGradient
						 read FGradient;
						 
	end;

function EditGradient( Gradient: TKGradient ): Boolean;

implementation

{$R *.DFM}

uses
	Consts, Registry, uksyShortCuts, uksyConsts, uksyUtils;

{ Internal Constants }

const

	BEGIN_COLOR_TAG = 1;
	END_COLOR_TAG		= 2;
	GRAD_STYLE_TAG  = 3;
	GRAD_STEPS_TAG  = 4;
	GRAD_BITMAP_TAG = 5;

	DEFAULT_GRADIENT_STYLE = gsVertical;
	DEFAULT_GRADIENT_STEPS = 64;	

{ Utility Functions }

function EditGradient( Gradient: TKGradient ): Boolean;
var
	frmGradProps: TfrmGradientProps;
begin
	ForceObject( Gradient );
	frmGradProps := TfrmGradientProps.Create( nil );
	try
		if CheckObject( Gradient.Owner ) then
			frmGradProps.grpGradProps.Caption := Format( frmGradProps.grpGradProps.Caption,
				[Gradient.Owner.GetNamePath] )
		else
			frmGradProps.grpGradProps.Caption := Format( frmGradProps.grpGradProps.Caption,
				[Gradient.GetNamePath] );
		frmGradProps.Gradient.Assign( Gradient );
		frmGradProps.RefreshGradientProps;
		Result := ( frmGradProps.ShowModal = mrOk );
		if Result then
			Gradient.Assign( frmGradProps.Gradient );
	finally
		frmGradProps.Free;
	end;
end;

{ Form Methods }

procedure TfrmGradientProps.GetColorsProc( const Color: string );
begin
	kcxBeginColor.Items.Add( Color );
	kcxEndColor.Items.Add( Color );
end;

procedure TfrmGradientProps.RefreshGradientProps;
var
	s: string;
	i: Integer;
begin
	FRefreshingProps := True;
	try
		kcxBeginColor.Items.Clear;
		kcxEndColor.Items.Clear;
		with Gradient do
		begin
			GetColorValues( GetColorsProc );
			KGetColorValues( GetColorsProc );
			s := KColorToString( BeginColor );
			i := kcxBeginColor.Items.IndexOf( s );
			if ( i = -1 ) then
				i := kcxBeginColor.Items.Add( s );
			kcxBeginColor.ItemIndex := i;
			s := KColorToString( EndColor );
			i := kcxEndColor.Items.IndexOf( s );
			if ( i = -1 ) then
				i := kcxEndColor.Items.Add( s );
			kcxEndColor.ItemIndex := i;
			kcxGradientStyle.ItemIndex := Integer( GradientStyle );
			ksiSteps.Text := IntToStr( Integer( Steps ) );
			if ( not Bitmap.Empty ) then
				imBitmap.Picture.Assign( Bitmap );
		end;
	finally
		FRefreshingProps := False;
	end;
end;

function TfrmGradientProps.GetColorRegKey( ATag: Integer ): string;
const
	SEL_DIR: array[BEGIN_COLOR_TAG..END_COLOR_TAG] of string = ( '\BeginC', '\EndC' );
begin
	Result := ( KLIBBaseRegKey + KNOWHOW_PROPERTY_EDITOR_SECTION +
		TKGradient.ClassName + SEL_DIR[ATag] );
end;

function TfrmGradientProps.CallColorDialog( Sender: TComboBox; var Color: TColor ): Boolean;

var
	Reg: TRegIniFile;

	procedure GetCustomColors;
	begin
		Reg := TRegIniFile.Create( GetColorRegKey( Sender.Tag ) );
		try
			cd.CustomColors.Clear;
			Reg.ReadSectionValues( SCustomColors, cd.CustomColors );
		except
			{ Ignore errors reading values }
		end;
	end;

	procedure SaveCustomColors;
	var
		i: Integer;
	begin
		if CheckObject( Reg ) then
			with cd.CustomColors do
				for i := 0 to Count - 1 do
					Reg.WriteString( SCustomColors, Names[i], Values[Names[i]] );
	end;

begin
	Reg := nil;
	try
		GetCustomColors;
		cd.Color := Color;
		Result := cd.Execute;
		Color := cd.Color;
		if Result then
  		SaveCustomColors;
	finally
		Reg.Free;
	end;
end;

{ Form Events }

procedure TfrmGradientProps.FormCreate(Sender: TObject);
begin
	FRefreshingProps := False;
	FGradient := TKGradient.Create( pbGrad, nil );
	ksiSteps.Text := IntToStr( DEFAULT_GRADIENT_STEPS );
	kcxGradientStyle.ItemIndex := Integer( DEFAULT_GRADIENT_STYLE );
end;

procedure TfrmGradientProps.FormDestroy(Sender: TObject);
begin
  FGradient.Free;
end;

procedure TfrmGradientProps.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if ( ShortCut( Key, Shift ) = SC_CTRL_M ) then
	begin
		Key := 0;
		imBitmapDblClick( imBitmap );
	end
	else if ( ShortCut( Key, Shift ) = SC_CTRL_C ) then
	begin
		Key := 0;
		miClearBitmapClick( miClearBitmap );
	end;
end;

procedure TfrmGradientProps.kcxColorKeyDown(Sender: TObject;
	var Key: Word; Shift: TShiftState);
begin
	if ( ShortCut( Key, Shift ) = SC_RETURN ) then
	begin
		Key := 0;
		kcxColorClick( Sender );
	end
	else 	if ( ShortCut( Key, Shift ) = SC_CTRL_RETURN ) then
		kcxColorDblClick( Sender );
end;

procedure TfrmGradientProps.kcxColorClick(Sender: TObject);
var
	i: TKGradientStyle;
begin
	with ( Sender as TComboBox ) do
		case Tag of
			BEGIN_COLOR_TAG: Gradient.BeginColor := KStringToColor( Text );
			END_COLOR_TAG	 : Gradient.EndColor := KStringToColor( Text );
			GRAD_STYLE_TAG :
			begin
				i := TKGradientStyle( ItemIndex );
				{ if there isn't a gradient bitmap and the style is for bitmaps then
					select one first }
				if ( Gradient.Bitmap.Empty and ( i in [gsBitmapTiled, gsBitmapStretched] ) ) then
					imBitmapDblClick( imBitmap );
				{ if no gradient bitmap are selected then, raise }
				try
					Gradient.GradientStyle := i;
				except
					on EKGradient do
					begin
						ItemIndex := Integer( Gradient.GradientStyle );
						raise;
					end;
				end;
			end;
		end;
end;

procedure TfrmGradientProps.ksiStepsChange(Sender: TObject);
begin
	Gradient.Steps := TKSteps( StrToIntDef( ksiSteps.Text, Low( TKSteps ) ) );
end;

procedure TfrmGradientProps.imBitmapDblClick(Sender: TObject);
begin
	if opd.Execute then
	begin
		ForceStrEqual( CH_ASTERISK + ExtractFileExt( opd.FileName ), opd.DefaultExt );
		imBitmap.Picture.LoadFromFile( opd.FileName );
		if ( not imBitmap.Picture.Bitmap.Empty ) then
			Gradient.Bitmap.Assign( imBitmap.Picture.Bitmap );
	end;
end;

procedure TfrmGradientProps.kcxColorDblClick(Sender: TObject);
var
  i: Integer;
	cl: TColor;
	cb: TComboBox;
begin
	cb := ( Sender as TComboBox );
	cl := KStringToColor( cb.Text );
	if CallColorDialog( cb, cl ) then
	begin
		i := cb.Items.IndexOf( KColorToString( cl ) );
		if ( i = -1 ) then
			i := cb.Items.Add( KColorToString( cl ) );
		cb.ItemIndex := i;
		kcxColorClick( Sender );
	end;
end;

procedure TfrmGradientProps.miClearBitmapClick(Sender: TObject);
begin
	imBitmap.Picture.Assign( nil );
	Gradient.Bitmap.Assign( nil );
	kcxGradientStyle.ItemIndex := Integer( Gradient.GradientStyle );
end;

procedure TfrmGradientProps.pbGradPaint(Sender: TObject);
begin
	if ( Gradient.GradientStyle <> gsNone ) then
	begin
		Gradient.Invalidate;
		pbGrad.Canvas.Draw( 0, 0, Gradient.GradientBmp );
	end;
end;

end.
