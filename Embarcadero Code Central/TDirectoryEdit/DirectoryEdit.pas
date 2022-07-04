unit DirectoryEdit;
(*
TDirectoryEdit,(c) 2000-2005 Brad Prendergast (bradp@bpsoftware.com),
               http://www.bpsoftware.com/products/delphi.htm

Version: 1.4.0.0, Aug 05, 2005

TDirectoryEdit is a descendant of the native TEdit control which allows
for the selection (through a SelectDirectory dialog) of a directory to 
populate the TDirectoryEdit field.


New:
  Properties -
    SetHint: Boolean - Specifies if the components Hint property should be populated with 
                       the contents of the field after a directory is selected.
 
  Events - 
    OnButtonClick: Specifies the event if the directory select button is clicked
                   (Note: If assigned this event will override the select directory event) 


There is no guarantee or warranty, expressed or implied, concerning the applicability of 
code and techniques included in this example.  This example code is supplied AS IS.  If
you wish to use this code or technique, it is your responsibility to test and certify 
the code in your project.
*)
interface

uses
{$IFDEF WIN32}Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl,ShlObj
{$ELSE}
  Windows, Messages, SysUtils, Classes, Controls,
  StdCtrls, FileCtrl, System.ComponentModel
{$ENDIF};


type
  TDirectoryEdit = class(TEdit)
  private
    { Private declarations }
    fButton: TButton;
    fSetHint: Boolean;
    fOnButtonClick: TNotifyEvent;
    procedure ChildButtonClick(Sender: TObject);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    { Protected declarations }
    function GetSetHint: Boolean;
    procedure SetSetHint(Value: Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property SetHint: Boolean read GetSetHint write SetSetHint;
  end;

procedure Register;

implementation

{$IFNDEF WIN32}
uses
  System.Reflection, System.Runtime.InteropServices;
{$ENDIF}

constructor TDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle + [csCaptureMouse];
  AutoSize:= False;
  fButton:= TButton.Create(Self);
  fButton.Width:= 17;
  fButton.Height:= 17;
  fButton.Visible:= True;
  fButton.Parent:= Self;
  fButton.SetBounds(0,0,fButton.Width,fButton.Height);
  fButton.Caption:= '...';
  FButton.OnClick:= ChildButtonClick;
  Height:= 21;
end;

destructor TDirectoryEdit.Destroy;
begin
  fButton:= nil;
  inherited Destroy;
end;

procedure TDirectoryEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  If FButton <> nil then
    begin
      if NewStyleControls and Ctl3D then
        FButton.SetBounds(Width - FButton.Width - 5,0,FButton.Width,Height - 5)
      else
        FButton.SetBounds (Width - FButton.Width,1,FButton.Width,Height - 3);
      SetEditRect;
    end;
end;

procedure TDirectoryEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  SetEditRect;
end;

procedure TDirectoryEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  If SetHint Then
    Hint:= Text;
end;

procedure TDirectoryEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  fButton.Enabled:= Enabled;
end;

procedure TDirectoryEdit.ChildButtonClick(Sender: TObject);
var
  TitleName: string;
  {$IFDEF WIN32}
  lpItemID: PItemIDList;
  BrowseInfo: TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  TempPath: array[0..MAX_PATH] of char;
  {$ELSE}
   sDirectory: string;
  {$ENDIF}
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self)
  else
  begin
    TitleName := 'Please specify a directory';
    {$IFDEF WIN32}
    FillChar(BrowseInfo,sizeof(TBrowseInfo),#0);
    BrowseInfo.hwndOwner:= Handle;
    BrowseInfo.pszDisplayName:= @DisplayName;
    BrowseInfo.lpszTitle:= PChar(TitleName);
    BrowseInfo.ulFlags:= BIF_RETURNONLYFSDIRS;
    lpItemID:= SHBrowseForFolder(BrowseInfo);
    if lpItemId <> nil then begin
      SHGetPathFromIDList(lpItemID,TempPath);
      Text:= (TempPath);
      GlobalFreePtr(lpItemID);
    end;
    {$ELSE}
      if SelectDirectory(TitleName,'',sDirectory,
        [sdShowShares and sdNewUI],nil) then
        Text:= sDirectory;
    {$ENDIF}
  end;
end;

procedure TDirectoryEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TDirectoryEdit.SetEditRect;
var
  Loc: TRect;
  {$IFNDEF WIN32}
  p: IntPtr;
  {$ENDIF}
begin

  {$IFNDEF WIN32}
  p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TRect)));
  Marshal.StructureToPtr(Loc, p, false);
  {$ENDIF}
  SendMessage(Handle,EM_GETRECT,0,{$IFDEF WIN32}LongInt(@Loc){$ELSE}Integer(p){$ENDIF});
  Loc.Bottom:= ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right:= ClientWidth - FButton.Width - 2;
  Loc.Top:= 0;
  Loc.Left:= 0;
  SendMessage(Handle,EM_SETRECTNP,0,{$IFDEF WIN32}LongInt(@Loc){$ELSE}Integer(p){$ENDIF});
  SendMessage(Handle,EM_GETRECT,0,{$IFDEF WIN32}LongInt(@Loc){$ELSE}Integer(p){$ENDIF});
end;

procedure TDirectoryEdit.SetSetHint(Value: Boolean);
begin
  fSetHint:= Value;
end;

function TDirectoryEdit.GetSetHint: Boolean;
begin
  Result:= fSetHint;
end;

procedure TDirectoryEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:= Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure Register;
begin
  RegisterComponents('BPComponents', [TDirectoryEdit]);
end;

end.
