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

unit _mSyncWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  _mWizard, ExtCtrls, StdCtrls;

type
  TfmSyncWizard = class(TfmWizard)
    nbIndex: TNotebook;
    nbData: TNotebook;
    Label2: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label5: TLabel;
		procedure FormCreate(Sender: TObject);
		
	private

	protected
		procedure InitPage( APage: Integer ); override;

	public

	end;

implementation

{$R *.DFM}

procedure TfmSyncWizard.InitPage( APage: Integer );
begin
	nbData.PageIndex := APage;
	nbIndex.PageIndex := APage;
	inherited InitPage( APage );
end;

procedure TfmSyncWizard.FormCreate(Sender: TObject);
begin
  inherited;
	StartWizard( nbData.Pages.Count );
end;

end.
