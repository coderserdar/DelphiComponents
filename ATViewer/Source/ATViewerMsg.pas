unit ATViewerMsg;

interface

uses
  Windows;

function MsgBox(const Msg, Title: WideString; Flags: Integer; hWnd: THandle = 0): Integer;
procedure MsgInfo(const Msg: WideString; hWnd: THandle = 0);
procedure MsgError(const Msg: WideString; hWnd: THandle = 0);
procedure MsgWarning(const Msg: WideString; hWnd: THandle = 0);

var
  ATViewerMessagesEnabled: Boolean = True;

var
  MsgViewerCaption: AnsiString = 'Viewer';
  MsgViewerShowCfm: AnsiString = 'Format not known'#13'Click here to show binary dump';
  MsgViewerErrCannotFindFile: AnsiString = 'File not found: "%s"';
  MsgViewerErrCannotFindFolder: AnsiString = 'Folder not found: "%s"';
  MsgViewerErrCannotOpenFile: AnsiString = 'Cannot open file: "%s"';
  MsgViewerErrCannotLoadFile: AnsiString = 'Cannot load file: "%s"';
  MsgViewerErrCannotReadFile: AnsiString = 'Cannot read file: "%s"';
  MsgViewerErrCannotReadStream: AnsiString = 'Cannot read stream';
  MsgViewerErrCannotReadPos: AnsiString = 'Read error at offset %s';
  MsgViewerErrImage: AnsiString = 'Unknown image format';
  MsgViewerErrMedia: AnsiString = 'Unknown multimedia format';
  MsgViewerErrInitControl: AnsiString = 'Cannot initialize %s';
  MsgViewerErrCannotCopyData: AnsiString = 'Cannot copy data to Clipboard';
  MsgViewerWlxException: AnsiString = 'Exception in plugin "%s" in function "%s"';
  MsgViewerWlxParentNotSpecified: AnsiString = 'Cannot load plugins: parent form not specified';
  MsgViewerAniTitle: AnsiString = 'Title: ';
  MsgViewerAniCreator: AnsiString = 'Creator: ';
  MsgViewerPageHint: AnsiString = 'Previous/Next page'#13'Current page: %d of %d';

implementation

uses
  SysUtils, Forms;

function MsgBox(const Msg, Title: WideString; Flags: Integer; hWnd: THandle = 0): Integer;
begin
  if ATViewerMessagesEnabled then
    //MessageBoxW supported under Win9x
    Result := MessageBoxW(hWnd, PWideChar(Msg), PWideChar(Title),
      Flags or MB_SETFOREGROUND or MB_TASKMODAL)
  else
    Result := IDCANCEL;
end;

procedure MsgInfo(const Msg: WideString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONINFORMATION, hWnd);
end;

procedure MsgError(const Msg: WideString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONERROR, hWnd);
end;

procedure MsgWarning(const Msg: WideString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONEXCLAMATION, hWnd);
end;

end.
