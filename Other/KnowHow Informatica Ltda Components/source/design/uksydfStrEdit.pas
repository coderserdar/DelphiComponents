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

=nit uksydfStrEdit;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}
uses
	Windows, Messages, SysUtils, EditIntf, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Menus, ComCtrls, ExtCtrls, TypInfo, uksydUtils, uksydClasses;

type

	TfrmStrEditDlg = class( TForm )
		Bevel1: TBevel;
		LineCount: TLabel;
		CodeWndBtn: TButton;
		HelpButton: TButton;
		OKButton: TButton;
		CancelButton: TButton;
		Memo: TRichEdit;
		OpenDialog: TOpenDialog;
		SaveDialog: TSaveDialog;
		StringEditorMenu: TPopupMenu;
		LoadItem: TMenuItem;
		SaveItem: TMenuItem;
		CodeEditorItem: TMenuItem;

		procedure FileOpen( Sender: TObject );
		procedure HelpButtonClick( Sender: TObject );
		procedure FileSave( Sender: TObject );
		procedure UpdateStatus( Sender: TObject );
		procedure CodeWndBtnClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);

	private
		FMultiSelect: Boolean;
		FCodeWndBtnClicked: Boolean;
		FDesigner: TKFormDesigner;
		FVFSFileName: string;
		FModIntf: TIModuleInterface;
		FPropInfo: PPropInfo;
		FSource: TPersistent;
		FVFSClass: TKIVirtualFileSystemClass;

		procedure CopyMemo( Module: TIModuleInterface; Editor: TIEditorInterface;
			Data: Pointer );

	public
		property Designer: TKFormDesigner
						 read FDesigner write FDesigner;
		property VFSFileName: string
						 read FVFSFileName write FVFSFileName;
		property PropInfo: PPropInfo
						 read FPropInfo write FPropInfo;
		property Source: TPersistent
						 read FSource write FSource;
		property ModuleIntf: TIModuleInterface
						 read FModIntf;
		property VFSClass: TKIVirtualFileSystemClass
						 read FVFSClass write FVFSClass;
		property MultiSelect: Boolean
						 read FMultiSelect write FMultiSelect;
	end;

const
	mrCodeEditor = mrYes;
{##NI##}
implementation

uses
	ToolIntf, ExptIntf, uksyConsts, uksyUtils, uksydConsts;

type
	EKSYDFStrEdit = class( EKDSystem );

{$R *.DFM}

procedure TfrmStrEditDlg.CopyMemo( Module: TIModuleInterface; Editor: TIEditorInterface;
	Data: Pointer );
var
	s: string;
begin
	s := Memo.Lines.Text;
	if ( Length( s ) > 1 ) then
		Delete( s, Length( s ) - 1, 2 ); { Cut the last CRLF! }
	InsertCode( Editor, 0, s );
	SaveModuleInterface( Editor.FileName, True, True );
end;

procedure TfrmStrEditDlg.FileOpen( Sender: TObject );
begin
	if OpenDialog.Execute then
	begin
		Screen.Cursor := crHourglass;
		try
			Memo.Lines.LoadFromFile( OpenDialog.Filename );
		finally
			Screen.Cursor := crDefault;
		end;
	end;
end;

procedure TfrmStrEditDlg.FileSave( Sender: TObject );
begin
	if SaveDialog.Execute then
	begin
		Screen.Cursor := crHourglass;
		try
			Memo.Lines.SaveToFile( OpenDialog.Filename );
		finally
			Screen.Cursor := crDefault;
		end;
	end;
end;

procedure TfrmStrEditDlg.HelpButtonClick( Sender: TObject );
begin
	Application.HelpCommand( HELP_KEY, Integer( PChar( 'String list editor') ) );
end;

procedure TfrmStrEditDlg.UpdateStatus( Sender: TObject );
begin
	if ( Memo.Lines.Count = 1 ) then
		LineCount.Caption := '1 line'
	else
		LineCount.Caption := Format( '%d lines', [Memo.Lines.Count] );
end;

procedure TfrmStrEditDlg.CodeWndBtnClick( Sender: TObject );
begin
	if MultiSelect then
		RaiseException( EKSYDFStrEdit, sErrInvVFSMultiSelect );
	if ( not IsFileSystemRegistered( VFSClass ) ) then
		RegisterFileSystem( VFSClass, TKIStringStream );
	FModIntf := ToolServices.CreateModuleEx( VFSFileName, '', '',
		GetFileSystemName( VFSClass ), nil, nil, [cmNewFile] );
	if CheckObject( FModIntf ) then
	begin
		FCodeWndBtnClicked := True;
		if ( not RegisterModuleForVFS( VFSClass, ChangeFileExt( ToolServices.GetCurrentFile,
			DELPHI_UNIT_EXT ), VFSFileName, Source, PropInfo ) ) then
		begin
			FCodeWndBtnClicked := False;
			FModIntf.Close;
			RaiseExceptionFmt( EKSYDFStrEdit, sErrVFSRegModule, [VFSFileName, GetFileSystemName( VFSClass )] );
		end;	
	end;
end;  

procedure TfrmStrEditDlg.FormCreate(Sender: TObject);
begin
	FSource := nil;
	FPropInfo := nil;
	FVFSClass := nil;
	FCodeWndBtnClicked := False;
end;

procedure TfrmStrEditDlg.FormClose( Sender: TObject; var Action: TCloseAction );
begin
	if FCodeWndBtnClicked then
	begin
		ForceObject( FModIntf );
		if CheckStrings( Memo.Lines ) then
			ExecuteEditorInterfaceByMethod( VFSFileName, CopyMemo, False, nil );
	end;
	if ( Memo.Modified and CheckDesigner( Designer ) ) then
		Designer.Modified;
end;

procedure TfrmStrEditDlg.FormShow(Sender: TObject);
begin
	ForceObject( FSource );
	ForcePointer( FPropInfo );
	ForceClass( FVFSClass );
	CodeWndBtn.Enabled := ( not MultiSelect );
end;

procedure TfrmStrEditDlg.FormDestroy(Sender: TObject);
begin
	FSource := nil;
	FPropInfo := nil;
	FVFSClass := nil;
	if CheckObject( FModIntf ) then
		FModIntf.Free;
	FModIntf := nil;	
end;

procedure TfrmStrEditDlg.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if ( Key = VK_ESCAPE ) then
		ModalResult := mrCancel;
end;

end.
