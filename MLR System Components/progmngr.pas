unit Progmngr;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DdeMan;

type
	TShowCmd = (scNone, scRestore, scIconic, scMaximized, scActivate,
    	scMinimize, scIconicNotActive, scDisplay);


  TProgramManager = class(TComponent)
  private
    { Private declarations }
    FGroupName		:string;
    FShowCommand	:TShowCmd;
    FCommandLine	:string;
    FIconPath			:string;
    FIconIndex		:Integer;
    FXPos					:Integer;
    FYPos					:Integer;
    FSaveState		:Boolean;
  protected
    { Protected declarations }
    DDEClient	:TDDEClientConv;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure CreateGroup;
    procedure ShowGroup;
    procedure AddItem;
    procedure DeleteGroup;
    procedure Exit;
    procedure Send(Command :string);
  published
    { Published declarations }
    property GroupName 		:string read FGroupName write FGroupName;
    property ShowCommand 	:TShowCmd read FShowCommand write FShowCommand default scNone;
    property CommandLine	:string read FCommandLine write FCommandLine;
    property IconPath		:string read FIconPath write FIconPath;
    property IconIndex		:Integer read FIconIndex write FIconIndex default -1;
    property XPos			:Integer read FXPos write FXPos default -1;
    property YPos			:Integer read FYPos write FYpos default -1;
	property SaveState		:Boolean read FSaveState write FSaveState default True;
  end;


procedure Register;

implementation

constructor TProgramManager.Create(AOwner :TComponent);
begin
	inherited Create(AOwner);
	DDEClient := TDDEClientConv.Create(Self);
  DDEClient.ConnectMode := ddeManual;
  DDEClient.SetLink('PROGMAN', 'PROGMAN');
  FSaveState 		:= True;
  FXpos 				:= -1;
  FYPos 				:= -1;
  FIconIndex 		:= -1;
  FShowCommand	:= scNone;
end;

destructor TProgramManager.Destroy;
begin
	DDEClient.Free;
	inherited Destroy;
end;

procedure TProgramManager.CreateGroup;
begin
	if FGroupName = '' then
    raise Exception.Create('A group name must be set');
  Send('[CreateGroup("' + FGroupName + '")]');
end;

procedure TProgramManager.ShowGroup;
var
	s	:string;
begin
	if FGroupName = '' then
		raise Exception.Create('A group name must be supplied.');
  s := '[ShowGroup("' + FGroupName + '"';
  if FShowCommand <> scNone then
    s := s + ',' + IntToStr(Ord(FShowCommand));
  s := s + ')]';
  Send(s);
end;

procedure TProgramManager.AddItem;
var
	s	:string;
begin
	if FCommandLine = '' then
		raise Exception.Create('A command line must be specified.');
	if FGroupName = '' then
		raise Exception.Create('A group name must be specified.');
  s := '[AddItem("' + FCommandLine + '", "' + FGroupName + '"';
  if FIconPath <> '' then begin
  	s := s + ', ' + FIconPath;
	  if FIconIndex <> -1 then
    	s := s + ', ' + IntToStr(FIconIndex)
    else
    	s := s + ',';
  end else
		s := s + ',,';
    if FXPos <> -1 then
    	s := s + ', ' + IntToStr(FXPos)
    else
    	s := s + ',';
    if FYPos <> -1 then
    	s := s + ', ' + IntToStr(FYPos)
    else
    	s := s + ',';

  s := s + ')]';
  Send(s);
end;

procedure TProgramManager.DeleteGroup;
begin
	if GroupName = '' then
		raise Exception.Create('A group name must be specified.');
	Send('[DeleteGroup("' + GroupName + '")]');
end;

procedure TProgramManager.Exit;
begin
	if FSaveState then
		Send('[ExitProgMan(True)]')
	else
		Send('[ExitProgMan(False)]');
end;

procedure Register;
begin
	RegisterComponents('MLR Visuals', [TProgramManager]);
end;

procedure TProgramManager.Send(Command :string);
var
	Macro	:string;
  Cmd		:array[0..255] of char;
begin
	Macro := Command + #13#10;
  StrPCopy(Cmd, Macro);
  if not DdeClient.OpenLink then
  	raise Exception.Create('Could not open the link to the Program Manager.');
  if not DdeClient.ExecuteMacro(Cmd, False) then
  	raise Exception.Create('Unable to execute: ' + Command + '.');
  DdeClient.CloseLink;
end;


end.
