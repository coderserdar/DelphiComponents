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

unit _mWizard;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	Dialogs, ukfCtrls, ukfClasses, StdCtrls, ExtCtrls;

type
  TWizardDirection = ( wdForward, wdBackward );
  
	TfmWizard = class( TKForm )
    pnNavigate: TPanel;
    pnBody: TPanel;
		bv01: TBevel;
		bnHelp: TButton;
		bnPrior: TButton;
		bnNext: TButton;
		bnExit: TButton;
		procedure FormCreate(Sender: TObject);
    procedure bnPriorClick(Sender: TObject);
    procedure bnNextClick(Sender: TObject);
    procedure bnExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

	private
		FLastPage: Integer;
		FPriorPage: Integer;
		FCurrentPage: Integer;

		FRestart: Boolean;
		FShowHelp: Boolean;
		FShowNext: Boolean;
		FShowPrior: Boolean;

		FActiveHelp: Boolean;
		FActiveNext: Boolean;
		FActivePrior: Boolean;

		FCloseQueryMessage: string;

    FDirection: TWizardDirection;

		procedure SetActiveHelp( Value: Boolean );
		procedure SetActiveNext( Value: Boolean );
		procedure SetActivePrior( Value: Boolean );

		procedure SetShowHelp( Value: Boolean );
		procedure SetShowNext( Value: Boolean );
		procedure SetShowPrior( Value: Boolean );

		procedure OpenPage( APage: Integer );
		procedure ClosePage( APage: Integer );
		function ProcessNext( APage: Integer; var NextPage: Integer ): Boolean; virtual;
		function ProcessPrior( APage: Integer; var NextPage: Integer ): Boolean; virtual;

	protected
		procedure DoOpenPage; virtual; abstract;
		procedure DoReopenPage; virtual; abstract;
		procedure DoClosePage( APage: Integer ); virtual; abstract;
		function DoProcessNext( APage: Integer; var NextPage: Integer ): Boolean; virtual; abstract;
		function DoProcessPrior( APage: Integer; var NextPage: Integer ): Boolean; virtual; abstract;

		function FinishWizard: Boolean; virtual;
    procedure ActivateControl( AControl: TWinControl );

	public
		procedure Cleanup;
    procedure CleanAfter( APage: Integer );
		procedure CancelWizard( DoAbort: Boolean ); virtual;
		procedure StartWizard( ALastPage: Integer ); virtual;

		property ActiveHelp: Boolean
						 read FActiveHelp write SetActiveHelp;
		property ActivePrior: Boolean
						 read FActivePrior write SetActivePrior;
		property ActiveNext: Boolean
						 read FActiveNext write SetActiveNext;
		property CloseQueryMessage: string
						 read FCloseQueryMessage write FCloseQueryMessage;
		property CurrentPage: Integer
						 read FCurrentPage;
    property Direction: TWizardDirection
             read FDirection;
		property LastPage: Integer
						 read FLastPage;
		property PriorPage: Integer
						 read FPriorPage;
		property Restart: Boolean
						 read FRestart write FRestart;
		property ShowHelp: Boolean
						 read FShowHelp write SetShowHelp;
		property ShowPrior: Boolean
						 read FShowPrior write SetShowPrior;
		property ShowNext: Boolean
						 read FShowNext write SetShowNext;

	end;

implementation

uses
	uksyUtils;

{$R *.DFM}

procedure TfmWizard.SetActiveHelp( Value: Boolean );
begin
	FActiveHelp := Value;
	bnHelp.Enabled := FActiveHelp;
end;

procedure TfmWizard.SetActiveNext( Value: Boolean );
begin
	FActiveNext := Value;
	bnNext.Enabled := FActiveNext;
end;

procedure TfmWizard.SetActivePrior( Value: Boolean );
begin
	FActivePrior := Value;
	bnPrior.Enabled := FActivePrior;
end;

procedure TfmWizard.SetShowHelp( Value: Boolean );
begin
	FShowHelp := Value;
	bnHelp.Visible := FShowHelp;
end;

procedure TfmWizard.SetShowNext( Value: Boolean );
begin
	FShowNext := Value;
	bnNext.Visible := FShowNext;
end;

procedure TfmWizard.SetShowPrior( Value: Boolean );
begin
	FShowPrior := Value;
	bnPrior.Visible := FShowPrior;
end;

procedure TfmWizard.ActivateControl( AControl: TWinControl );
begin
  Application.ProcessMessages;
	if ( AControl.Showing and AControl.Enabled ) then
		AControl.SetFocus;
end;

procedure TfmWizard.Cleanup;
var
	i: Integer;
begin
	for i := 0 to LastPage - 1 do
		ClosePage( i );
end;

procedure TfmWizard.CleanAfter( APage: Integer );
var
	i: Integer;
begin
	for i := APage + 1 to LastPage - 1 do
		ClosePage( i );
end;

function TfmWizard.FinishWizard: Boolean;
begin
	Result := true;
end;

function TfmWizard.ProcessNext( APage: Integer; var NextPage: Integer ): Boolean;
begin
  FDirection := wdForward;
	Result := DoProcessNext( APage, NextPage );
end;

function TfmWizard.ProcessPrior( APage: Integer; var NextPage: Integer ): Boolean;
begin
  FDirection := wdBackward;
	Result := DoProcessPrior( APage, NextPage );
end;

procedure TfmWizard.OpenPage( APage: Integer );
begin
  if ( Direction = wdBackward ) then
    ClosePage( FPriorPage );

	if ( APage = 0 ) then
	begin
		bnExit.Caption := 'Sai&r';

		ActivePrior := false;
		ActiveNext := true;

		bnExit.Default := false;
		bnNext.Default := true;
	end
	else if ( APage < LastPage - 1 ) then
	begin
		bnExit.Caption := 'Cancela&r';

		ActivePrior := true;
		ActiveNext := true;

		bnExit.Default := false;
		bnNext.Default := true;
	end
	else if ( APage = LastPage - 1 ) then
	begin
		if Restart then
			bnExit.Caption := 'Continua&r'
		else
			bnExit.Caption := 'Conclui&r';

		ActivePrior := false;
		ActiveNext := false;

		bnExit.Default := true;
		bnNext.Default := false;
	end;

  if ( Direction = wdBackward ) then
    DoReopenPage
  else
		DoOpenPage;
  Application.ProcessMessages;
end;

procedure TfmWizard.ClosePage( APage: Integer );
begin
	DoClosePage( APage );
end;

procedure TfmWizard.CancelWizard( DoAbort: Boolean );
begin
	if DoAbort then
		ModalResult := mrCancel;
end;

procedure TfmWizard.StartWizard( ALastPage: Integer );
begin
  FDirection := wdForward;
  Cleanup;
  FPriorPage := 0;
	FCurrentPage := 0;
	FLastPage := ALastPage;
	ShowHelp := true;
	ShowNext := true;
  ShowPrior := true;
	OpenPage( 0 );
end;

procedure TfmWizard.FormCreate(Sender: TObject);
begin
	FRestart := false;
  FActiveHelp := true;
  FActiveNext := true;
  FActivePrior := true;
  FShowHelp := true;
  FShowNext := true;
	FShowPrior := true;
	FCloseQueryMessage := 'Se este assistente for fechado agora, todos os dados não salvos serão perdidos. Deseja realmente continuar fechando este assistente?';
end;

procedure TfmWizard.bnPriorClick(Sender: TObject);
var
	iNextPage: Integer;
begin
	iNextPage := FCurrentPage - 1;
	if ProcessPrior( FCurrentPage, iNextPage ) then
	begin
    ClosePage( FCurrentPage );
    FPriorPage := FCurrentPage;
		FCurrentPage := iNextPage;
 		OpenPage( FCurrentPage );
	end;
end;

procedure TfmWizard.bnNextClick(Sender: TObject);
var
	iNextPage: Integer;
begin
	iNextPage := FCurrentPage + 1;
	if ProcessNext( FCurrentPage, iNextPage ) then
	begin
    ClosePage( FCurrentPage );
    FPriorPage := FCurrentPage;
		FCurrentPage := iNextPage;
		OpenPage( FCurrentPage );
	end;
end;

procedure TfmWizard.bnExitClick(Sender: TObject);
var
	iNextPage: Integer;
begin
	if ( FCurrentPage = 0 ) then
	begin
		if Confirm( 'Deseja realmente finalizar este assistente?' ) then
			Close;
	end
	else if ( FCurrentPage < FLastPage - 1 ) then
	begin
		if Confirm( 'Todos os dados não salvos serão perdidos. Deseja realmente cancelar o assistente?' ) then
		begin
			CancelWizard( false );
			StartWizard( FLastPage );
		end;
	end
	else if ( FCurrentPage = FLastPage - 1 ) then
	begin
		iNextPage := -1;
		if ProcessNext( FCurrentPage, iNextPage ) and FinishWizard then
		begin
			if Restart then
				StartWizard( FLastPage )
			else
        ModalResult := mrOK;
		end;
	end;
end;

procedure TfmWizard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := true;
	if ( FCurrentPage > 0 ) and ( FCurrentPage < FLastPage - 1 ) then
	begin
		CanClose := Confirm( CloseQueryMessage );
		if CanClose then
			CancelWizard( true );
	end;
end;

end.
