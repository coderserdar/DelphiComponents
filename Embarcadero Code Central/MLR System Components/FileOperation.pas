unit FileOperation;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI;

type
	TOperation 	= (foCopy, foDelete, foMove, foRename);
  TFOFlag			= (fofAllowUndo, fofConfirmMouse, fofFilesOnly,
                  fofMultiDestFiles, fofNoConfirmation,
                  fofNoConfimMakeDir, fofRenameOnCollision,
                  fofSilent, fofSimpleProgress);
  TFOFlags		= set of TFOFlag;

  TFileOperation = class(TComponent)
  private
    { Private declarations }
		FileOp	:TSHFileOpStruct;
  protected
    { Protected declarations }
    FFileFrom			:string;
    FFileTo				:string;
    FOperation		:TOperation;
    FFOFlags			:TFOFlags;
    FTitle				:string;
    function	GetAborted :Boolean;
    procedure	SetAborted(Value :Boolean);
  public
    { Public declarations }
		constructor	Create(AOwner :TComponent); override;
		function		Execute		:Boolean;
		property		Aborted		:Boolean read GetAborted write SetAborted;
	published
		{ Published declarations }
		property	FileFrom	:string read FFileFrom write FFileFrom;
		property	FileTo		:string read FFileTo write FFileTo;
		property	Operation	:TOperation read FOperation write FOperation;
		property	Flags			:TFOFlags read FFOFlags write FFOFlags;
		property	Title			:string read FTitle write FTitle;
	end;

procedure Register;

implementation

procedure EnsureNull(var s :string);
begin
	if s = '' then exit;
	if s[Length(s)] <> #0 then
		s	:= s + #0;
end;

procedure Register;
begin
	RegisterComponents('MLR Sysco', [TFileOperation]);
end;

constructor TFileOperation.Create(AOwner :TComponent);
begin
	inherited Create(AOwner);
end;

function TFileOperation.Execute :Boolean;
begin
	EnsureNull(FFileFrom);
	EnsureNull(FFileTo);
	EnsureNull(FTitle);
	with FileOp do begin
		if Owner is TWinControl then
			Wnd			:= TWinControl(Owner).Handle
		else
    	Wnd			:= 0;
    case FOperation of
			foCopy:			wFunc	:= FO_COPY;
      foDelete:		wFunc	:= FO_DELETE;
      foMove:			wFunc	:= FO_MOVE;
      foRename:		wFunc := FO_RENAME;
    end;
    pFrom		:= PChar(FFileFrom);
    pTo			:= PChar(FFileTo);
    fFlags	:= 0;
    if	fofAllowUndo in FFOFlags then
    	fFlags	:= fFlags or FOF_ALLOWUNDO;
    if	fofConfirmMouse in FFOFlags then
    	fFlags	:= fFlags or FOF_CONFIRMMOUSE;
    if	fofFilesOnly in FFOFlags then
    	fFlags	:= fFlags or FOF_FILESONLY;
    if fofMultiDestfiles in FFOFlags then
    	fFlags	:= fFlags or FOF_MULTIDESTFILES;
    if fofNoConfirmation in FFOFlags then
    	fFlags	:= fFlags or FOF_NOCONFIRMATION;
    if fofNoConfimMakeDir in FFOFlags then
    	fFlags	:= fFlags or FOF_NOCONFIRMMKDIR;
    if fofRenameOnCollision in FFOFlags then
    	fFlags	:= fFlags or FOF_RENAMEONCOLLISION;
		if fofSilent in FFOFlags then
			fFlags	:= fFlags or FOF_SILENT;
		if fofSimpleProgress in FFOFlags then
			fFlags	:= fFlags or FOF_SIMPLEPROGRESS;
		if FTitle <> '' then
			lpszProgressTitle	:= @Title[1];
		hNameMappings	:= nil;
	end;
	Result	:= (SHFileOperation(FileOp) = 0);
end;

function TFileOperation.GetAborted :Boolean;
begin
	Result	:= FileOp.fAnyOperationsAborted;
end;

procedure TFileOperation.SetAborted(Value :Boolean);
begin
	FileOp.fAnyOperationsAborted	:= Value;
end;

end.
