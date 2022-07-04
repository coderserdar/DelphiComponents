unit ArchiverMisc;
{
  TArchiver by Morgan Martinet (C) 1998 - mmm@imaginet.fr or mmm@mcom.fr

  this unit contains several functions used by the other units
  implementing TArchiver.
}
{$I Definition.Inc}

interface
  uses Windows, Messages, Classes, SysUtils;

  // Dialog / Query functions
  const
    mrNone     = 0;
    mrOk       = idOk;
    mrCancel   = idCancel;
    mrAbort    = idAbort;
    mrRetry    = idRetry;
    mrIgnore   = idIgnore;
    mrYes      = idYes;
    mrNo       = idNo;
    mrAll      = mrNo + 1;
    //mrNoToAll  = mrAll + 1;
    //mrYesToAll = mrNoToAll + 1;
  type
    TMyMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
    TMyMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
      mbAll, {mbNoToAll, mbYesToAll,} mbHelp);
    TMyMsgDlgButtons = set of TMyMsgDlgBtn;
  var
    MyModalResults: array[TMyMsgDlgBtn] of Integer = (
      mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, {mrNoToAll,
      mrYesToAll,} 0);
  function MessageDlg( const Msg: string; DlgType: TMyMsgDlgType;
                       Buttons: TMyMsgDlgButtons; HelpCtx: Longint): Integer;
  function InputQuery(const ACaption, APrompt: string; var AValue: string): Boolean;
  function QueryPassword(const ACaption, APrompt: string; var AValue: string): Boolean;
  function QueryFileOverwrite( const oldFileName, newFileName : String;
                               oldFileSize, newFileSize : Integer;
                               oldFileDate, newFileDate : TDateTime ) : Integer;
  function QueryContinue( const ErrorMsg, FileName : String;
                          FileSize : Integer;
                          FileDate : TDateTime ) : Integer;

  // Stream functions
  function  ReadInteger( S : TStream ) : Integer;
  procedure WriteInteger( S : TStream; val : Integer );
  function  ReadWord( S : TStream ) : Word;
  procedure WriteWord( S : TStream; val : Word);
  function  ReadFloat( S : TStream ) : Extended;
  procedure WriteFloat( S : TStream; val : Extended );
  function  ReadBoolean( S : TStream ) : Boolean;
  procedure WriteBoolean( S : TStream; val : Boolean );
  function  ReadString( S : TStream ) : String;
  procedure WriteString( S : TStream; val : String );

  // Misc
  function  RemoveSlash( const sDir : String ) : String;
  function  AppendSlash( const sDir : String ) : String;
  function  AdjustPath( const path : String; maxSize : Integer ) : String;

  function  DiskInDrive(Drive: Char): Boolean;
  function  CRC32R( CRC :Longint; const Data; cbData :Longint ) :Longint;
  function  Min( a, b : Integer ) : Integer;
  function  Max( a, b : Integer ) : Integer;
  function  Abs( val : Integer ) : Integer;
  function  EncodeBlockSize( IsCompressed : Boolean; BlockSize : Integer ) : Integer;
  procedure DecodeBlockSize( size : Integer; var IsCompressed : Boolean;
                             var BlockSize : Integer );
  function  CalcRatio( size, compressedSize : Integer ) : Integer;
  function  DirectoryExists(const Name: string): Boolean;
  function  GetFileDate( const FileName : String ) : TDateTime;
  function  GetFileSize( const fileName : String ) : Integer;
  function  IsExeFile( const FileName : String ) : Boolean;
  function  GetTempDir : String;

  procedure GetVersionInfo( const FileName : String; Infos : TStrings );
  function MSecsAsTime( secs : Integer ) : TDateTime;
  function TimeAsMSecs( time : TDateTime ) : Integer;

  procedure GetDiskSizeAvail2( TheDrive : PChar;
                               var TotalBytes : double;
                               var TotalFree : double);
  procedure GetDiskSizeAvail( TheDrive : PChar; var TotalBytes : double; var TotalFree : double);
  function EnumerateDirectory( const dir, filter : String;
                               recurseDir : Boolean;
                               dest : TStrings ) : Integer;

  // by Mauro Favagrossa:
  function GetVolumeInfo( const drive: string; var VolumeName: string; var SerialNum: dword ): boolean;
  {if fail then return false and GetLastError() return error-code<>0
   In "drive" string is used the first char only (no case sensitive), for ex. 'A:\namedir' = 'A' = 'a'
   "drive"='' is the current drive.}

{$IFDEF VER90} // for Delphi 2.0    by Mauro F. (bug fixed since D2.01)
  function ExtractFileDrive(const FileName: string): string;
{$ENDIF}

var
  strOk : String;
  strCancel : String;
  strInformation : String;
  strWarning : String;
  strConfirmation : String;
  strError : String;
  strYes : String;
  strYesToAll : String;
  strNo : String;
  strReplaceFile : String;
  strWithFile : String;
  strConfirmFileOverwrite : String;
  strFile : String;
  strCanContinue : String;

implementation
{$R ArchiverMisc.res}

/////////////////////////////////////////
// Dialog / Query functions
/////////////////////////////////////////


function MessageDlg(const Msg: string; DlgType: TMyMsgDlgType;
  Buttons: TMyMsgDlgButtons; HelpCtx: Longint): Integer;
var
  uType : Cardinal;
  title : String;
begin
  uType := MB_TASKMODAL;
  case DlgType of
  mtWarning:
    begin
      uType := uType or MB_ICONWARNING;
      title := strWarning;
    end;
  mtError:
    begin
      uType := uType or MB_ICONERROR;
      title := strError;
    end;
  mtInformation:
    begin
      uType := uType or MB_ICONINFORMATION;
      title := strInformation;
    end;
  mtConfirmation:
    begin
      uType := uType or MB_ICONQUESTION;
      title := strConfirmation;
    end;
  else
    begin
      uType := uType or MB_ICONINFORMATION;
      title := strInformation;
    end;
  end;
  if ([mbAbort, mbRetry, mbIgnore] - Buttons) = [] then
    uType := uType or MB_ABORTRETRYIGNORE
  else if ([mbYes, mbNo, mbCancel] - Buttons) = [] then
    uType := uType or MB_YESNOCANCEL
  else if ([mbYes, mbNo] - Buttons) = [] then
    uType := uType or MB_YESNO
  else if ([mbRetry, mbCancel] - Buttons) = [] then
    uType := uType or MB_RETRYCANCEL
  else if ([mbOk, mbCancel] - Buttons) = [] then
    uType := uType or MB_OKCANCEL
  else
    uType := uType or MB_OK;
  Result := MessageBox(
    GetActiveWindow, // handle of owner window
    PChar(msg),      // address of text in message box
    PChar(title),    // address of title of message box
    uType            // style of message box
   );
end;

procedure CenterWindow( win : HWND );
var
  x, y, cx, cy : Integer;
  topRect, dlgRect : TRect;
begin
  GetWindowRect( GetDesktopWindow, topRect );
  GetWindowRect( win, dlgRect );
  cx := dlgRect.Right - dlgRect.Left;
  cy := dlgRect.Bottom - dlgRect.Top;
  x := ((topRect.Right - topRect.Left) - cx) div 2;
  y := ((topRect.Bottom - topRect.Top) - cy) div 2;
  SetWindowPos(
    win,       // handle of window
    0,         // placement-order handle
    x,         // horizontal position
    y,         // vertical position
    cx,        // width
    cy,        // height
    0          // window-positioning flags
  );

end;

type
  TQueryParams = record
    Caption : String;
    Prompt : String;
    Value : String;
  end;
  PQueryParam = ^TQueryParams;

function QueryProc(
    hwndDlg : HWND;	// handle of dialog box
    uMsg : Cardinal;	// message
    wp : WPARAM;	// first message parameter
    lp : lParam 	// second message parameter
   ) : BOOL; far; stdcall;
var
  pparams : PQueryParam;
begin
  Result := False;
  case uMsg of
  WM_INITDIALOG:
    begin
      pparams := PQueryParam(lp);
      SetDlgItemText( hwndDlg, 1, PChar(strOk) );
      SetDlgItemText( hwndDlg, 2, PChar(strCancel) );
      SetWindowLong( hwndDlg, DWL_USER, lp );
      CenterWindow( hwndDlg );
      with pparams^ do
        begin
          SetWindowText( hwndDlg, PChar(Caption) );
          SetDlgItemText( hwndDlg, 100, PChar(Prompt) );
          SetDlgItemText( hwndDlg, 101, PChar(Value) );
        end;
      SetFocus( GetDlgItem( hwndDlg, 101 ) );
    end;
  WM_CLOSE, WM_QUIT:
    begin
      EndDialog( hwndDlg, 0 );
      Result := True;
    end;
  WM_COMMAND:
    begin
      case wp of
      1:
        begin
          pparams := PQueryParam( GetWindowLong( hwndDlg, DWL_USER ) );
          with pparams^ do
            begin
              SetLength( Value, 512 );
              SetLength( Value, GetDlgItemText( hwndDlg, 101, PChar(Value), Length(Value) ) );
            end;
          EndDialog( hwndDlg, 1 );
          Result := True;
        end;
      2:
        begin
          EndDialog( hwndDlg, 0 );
          Result := True;
        end;
      end;
    end;
  end;
end;

function InputQuery(const ACaption, APrompt: string; var AValue: string): Boolean;
var
  id : Integer;
  params : TQueryParams;
begin
  with params do
    begin
      Caption := ACaption;
      Prompt  := APrompt;
      Value   := AValue;
    end;
  id := DialogBoxParam(
     hInstance,                // handle of application instance
     MakeIntResource(101),     // identifies dialog box template
     GetActiveWindow,          // handle of owner window
     @QueryProc,               // address of dialog box procedure
     Integer(@params)          // initialization value
    );
 Result := id > 0;
 if Result then
   AValue := params.Value;
end;

function QueryPassword(const ACaption, APrompt: string; var AValue: string): Boolean;
var
  id : Integer;
  params : TQueryParams;
begin
  with params do
    begin
      Caption := ACaption;
      Prompt  := APrompt;
      Value   := AValue;
    end;
  id := DialogBoxParam(
     hInstance,                // handle of application instance
     MakeIntResource(102),     // identifies dialog box template
     GetActiveWindow,          // handle of owner window
     @QueryProc,               // address of dialog box procedure
     Integer(@params)          // initialization value
    );
 Result := id > 0;
 if Result then
   AValue := params.Value;
end;


type
  TQueryOverwriteParams = record
    oldFileName, newFileName : String;
    oldFileSize, newFileSize : Integer;
    oldFileDate, newFileDate : TDateTime;
  end;
  PQueryOverwriteParam = ^TQueryOverwriteParams;

function QueryOverwriteProc(
    hwndDlg : HWND;	// handle of dialog box
    uMsg : Cardinal;	// message
    wp : WPARAM;	// first message parameter
    lp : lParam 	// second message parameter
   ) : BOOL; far; stdcall;
var
  pparams : PQueryOverwriteParam;
begin
  Result := False;
  case uMsg of
  WM_INITDIALOG:
    begin
      pparams := PQueryOverwriteParam(lp);
      SetDlgItemText( hwndDlg,   6, PChar(strYes) );
      SetDlgItemText( hwndDlg,   2, PChar(strCancel) );
      SetDlgItemText( hwndDlg, 102, PChar(strYesToAll) );
      SetDlgItemText( hwndDlg, 101, PChar(strNo) );
      SetDlgItemText( hwndDlg, 103, PChar(strReplaceFile) );
      SetDlgItemText( hwndDlg, 106, PChar(strWithFile) );
      SetWindowText( hwndDlg, PChar(strConfirmFileOverwrite) );
      SetWindowLong( hwndDlg, DWL_USER, lp );
      CenterWindow( hwndDlg );
      with pparams^ do
        begin
          SetDlgItemText( hwndDlg, 104, PChar(oldFileName) );
          SetDlgItemText( hwndDlg, 105, PChar(Format('%.0n bytes  %s',[oldFileSize*1.0, DateTimeToStr(oldFileDate)])) );
          SetDlgItemText( hwndDlg, 107, PChar(newFileName) );
          SetDlgItemText( hwndDlg, 108, PChar(Format('%.0n bytes  %s',[newFileSize*1.0, DateTimeToStr(newFileDate)])) );
        end;
    end;
  WM_CLOSE, WM_QUIT:
    begin
      EndDialog( hwndDlg, 0 );
      Result := True;
    end;
  WM_COMMAND:
    begin
      EndDialog( hwndDlg, wp );
      Result := True;
    end;
  end;
end;

function QueryFileOverwrite( const oldFileName, newFileName : String;
                             oldFileSize, newFileSize : Integer;
                             oldFileDate, newFileDate : TDateTime ) : Integer;
var
  params : TQueryOverwriteParams;
begin
  params.oldFileName := oldFileName;
  params.newFileName := newFileName;
  params.oldFileSize := oldFileSize;
  params.newFileSize := newFileSize;
  params.oldFileDate := oldFileDate;
  params.newFileDate := newFileDate;
  Result := DialogBoxParam(
     hInstance,                // handle of application instance
     MakeIntResource(103),     // identifies dialog box template
     GetActiveWindow,          // handle of owner window
     @QueryOverwriteProc,      // address of dialog box procedure
     Integer(@params)          // initialization value
    );
end;


type
  TQueryContinueParams = record
    ErrorMsg : String;
    FileName : String;
    FileSize : Integer;
    FileDate : TDateTime;
  end;
  PQueryContinueParam = ^TQueryContinueParams;

function QueryContinueProc(
    hwndDlg : HWND;	// handle of dialog box
    uMsg : Cardinal;	// message
    wp : WPARAM;	// first message parameter
    lp : lParam 	// second message parameter
   ) : BOOL; far; stdcall;
var
  pparams : PQueryContinueParam;
begin
  Result := False;
  case uMsg of
  WM_INITDIALOG:
    begin
      pparams := PQueryContinueParam(lp);
      SetDlgItemText( hwndDlg,   6, PChar(strYes) );
      SetDlgItemText( hwndDlg,   2, PChar(strNo) );
      SetDlgItemText( hwndDlg, 102, PChar(strYesToAll) );
      SetDlgItemText( hwndDlg, 103, PChar(strError+' :') );
      SetDlgItemText( hwndDlg, 106, PChar(strFile+' :') );
      SetDlgItemText( hwndDlg, 108, PChar(strCanContinue) );
      SetWindowText( hwndDlg, PChar(strConfirmation) );
      SetWindowLong( hwndDlg, DWL_USER, lp );
      CenterWindow( hwndDlg );
      with pparams^ do
        begin
          SetDlgItemText( hwndDlg, 104, PChar(ErrorMsg) );
          SetDlgItemText( hwndDlg, 105, PChar(FileName) );
          SetDlgItemText( hwndDlg, 107, PChar(Format('%.0n bytes  %s',[FileSize*1.0, DateTimeToStr(FileDate)])) );
        end;
    end;
  WM_CLOSE, WM_QUIT:
    begin
      EndDialog( hwndDlg, 0 );
      Result := True;
    end;
  WM_COMMAND:
    begin
      EndDialog( hwndDlg, wp );
      Result := True;
    end;
  end;
end;

function QueryContinue( const ErrorMsg, FileName : String;
                        FileSize : Integer;
                        FileDate : TDateTime ) : Integer;
var
  params : TQueryContinueParams;
begin
  params.ErrorMsg := ErrorMsg;
  params.FileName := FileName;
  params.FileSize := FileSize;
  params.FileDate := FileDate;
  Result := DialogBoxParam(
     hInstance,                // handle of application instance
     MakeIntResource(104),     // identifies dialog box template
     GetActiveWindow,          // handle of owner window
     @QueryContinueProc,       // address of dialog box procedure
     Integer(@params)          // initialization value
    );
end;

/////////////////////////////////////////
// Misc functions
/////////////////////////////////////////

function ReadInteger( S : TStream ) : Integer;
begin
  S.ReadBuffer( Result, Sizeof(Result) );
end;

procedure WriteInteger( S : TStream; val : Integer );
begin
  S.WriteBuffer( val, Sizeof(val) );
end;

function ReadWord( S : TStream ) : Word;
begin
  S.ReadBuffer( Result, Sizeof(Result) );
end;

procedure WriteWord( S : TStream; val : Word );
begin
  S.WriteBuffer( val, Sizeof(val) );
end;

function ReadFloat( S : TStream ) : Extended;
begin
  S.ReadBuffer( Result, Sizeof(Result) );
end;

procedure WriteFloat( S : TStream; val : Extended );
begin
  S.WriteBuffer( val, Sizeof(val) );
end;

function ReadBoolean( S : TStream ) : Boolean;
begin
  S.ReadBuffer( Result, Sizeof(Result) );
end;

procedure WriteBoolean( S : TStream; val : Boolean );
begin
  S.WriteBuffer( val, Sizeof(val) );
end;


function ReadString( S : TStream ) : String;
var
  len : Integer;
begin
  len := ReadInteger(S);
  SetString(Result, PChar(nil), len);
  S.ReadBuffer(Pointer(Result)^, len);
end;

procedure WriteString( S : TStream; val : String );
begin
  WriteInteger( S, Length(val) );
  S.WriteBuffer(Pointer(val)^, Length(val));
end;

function  RemoveSlash( const sDir : String ) : String;
begin
  Result := sDir;
  if (Length(sDir)>0) and (sDir[Length(sDir)]='\') then
     Delete( Result, length(sDir), 1 );
end;

function  AppendSlash( const sDir : String ) : String;
begin
  Result := sDir;
  if (Length(sDir)>0) and (sDir[Length(sDir)]<>'\') then
     Result := Result+'\';
end;

function AdjustPath( const path : String; maxSize : Integer ) : String;
var
  drive, dir, fileName, firstDir, lastDir : String;
  i, idx : Integer;
begin
  if Length(path) < maxSize then
    begin
      Result := path;
      Exit;
    end;
  drive := AppendSlash( ExtractFileDrive(path) );
  dir := ExtractFilePath(path);
  if Length(drive)>0 then
    Delete( dir, 1, Length(drive) );
  fileName := ExtractFileName(path);
  if Length(fileName) > maxSize then
    begin
      Result := Copy(fileName, 1, maxSize-3)+'...';
      Exit;
    end;
  if (Length(dir) > 0) and (dir[1] = '\') then
    Delete( dir, 1, 1 );
  dir := RemoveSlash(dir);
  idx := Pos( '\', dir );
  if idx > 0 then
    begin
      firstDir := Copy( dir, 1, idx-1 );
      lastDir := '';
      for i := Length(dir) downto 1 do
        if dir[i] = '\' then
          begin
            lastDir := Copy( dir, i+1, Length(dir) );
            Break;
          end;
    end
  else
    begin
      firstDir := '';
      lastDir := '';
    end;
  if firstDir <> '' then
    begin
      firstDir := firstDir + '\';
      lastDir := lastDir + '\';
      if Length(drive)+Length(firstDir)+4+Length(lastDir)+Length(fileName) < maxSize then
        Result := drive + firstDir + '...\' + lastDir + fileName
      else if Length(drive)+4+Length(lastDir)+Length(fileName) < maxSize then
        Result := drive + '...\' + lastDir + fileName
      else if Length(drive)+Length(firstDir)+4+Length(fileName) < maxSize then
        Result := drive + firstDir + '...\' + fileName
      else
        Result := drive + '...\' + fileName;
    end
  else
    begin
      Result := drive + '...\' + fileName;
    end;
end;

function DiskInDrive(Drive: Char): Boolean;
var
  ErrorMode: word;
begin
  // make it upper case

  if Drive in ['a'..'z'] then Dec(Drive, $20);
  // make sure it's a letter
  if not (Drive in ['A'..'Z']) then
    raise Exception.Create('Not a valid drive ID');
  // turn off critical errors
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try
    // drive 1 = a, 2 = b, 3 = c, etc.
    if DiskSize(Ord(Drive) - $40) = -1 then
      Result := False
    else
      Result := True;
  finally
    // restore old error mode
    SetErrorMode(ErrorMode);
  end;
end;
{
    CRC32 by Glen Why (C) 1996
    e-mail: eugene@actcom.co.il
}
function CRC32R( CRC :Longint; const Data; cbData :Longint ) :Longint;
assembler;
asm
      or     edx, edx
      je     @@exi
      jecxz  @@exi
      push   ebx
@@upd:
      movzx  ebx, al
      xor    bl, [ edx ]
      shr    eax, 8
      and    eax, 00FFFFFFh
{$IFDEF DELPHI6_OR_HIGHER}
      xor    eax, cs:[ ebx + @@C32TT ]
{$ELSE}
      xor    eax, cs:[ ebx + OFFSET @@C32TT ]
{$ENDIF}
      inc    edx
      loop   @@upd
      pop    ebx
@@exi:
      ret

@@C32TT:

DD 000000000h, 077073096h, 0ee0e612ch, 0990951bah
DD 0076dc419h, 0706af48fh, 0e963a535h, 09e6495a3h
DD 00edb8832h, 079dcb8a4h, 0e0d5e91eh, 097d2d988h
DD 009b64c2bh, 07eb17cbdh, 0e7b82d07h, 090bf1d91h
DD 01db71064h, 06ab020f2h, 0f3b97148h, 084be41deh
DD 01adad47dh, 06ddde4ebh, 0f4d4b551h, 083d385c7h
DD 0136c9856h, 0646ba8c0h, 0fd62f97ah, 08a65c9ech
DD 014015c4fh, 063066cd9h, 0fa0f3d63h, 08d080df5h
DD 03b6e20c8h, 04c69105eh, 0d56041e4h, 0a2677172h
DD 03c03e4d1h, 04b04d447h, 0d20d85fdh, 0a50ab56bh
DD 035b5a8fah, 042b2986ch, 0dbbbc9d6h, 0acbcf940h
DD 032d86ce3h, 045df5c75h, 0dcd60dcfh, 0abd13d59h
DD 026d930ach, 051de003ah, 0c8d75180h, 0bfd06116h
DD 021b4f4b5h, 056b3c423h, 0cfba9599h, 0b8bda50fh
DD 02802b89eh, 05f058808h, 0c60cd9b2h, 0b10be924h
DD 02f6f7c87h, 058684c11h, 0c1611dabh, 0b6662d3dh
DD 076dc4190h, 001db7106h, 098d220bch, 0efd5102ah
DD 071b18589h, 006b6b51fh, 09fbfe4a5h, 0e8b8d433h
DD 07807c9a2h, 00f00f934h, 09609a88eh, 0e10e9818h
DD 07f6a0dbbh, 0086d3d2dh, 091646c97h, 0e6635c01h
DD 06b6b51f4h, 01c6c6162h, 0856530d8h, 0f262004eh
DD 06c0695edh, 01b01a57bh, 08208f4c1h, 0f50fc457h
DD 065b0d9c6h, 012b7e950h, 08bbeb8eah, 0fcb9887ch
DD 062dd1ddfh, 015da2d49h, 08cd37cf3h, 0fbd44c65h
DD 04db26158h, 03ab551ceh, 0a3bc0074h, 0d4bb30e2h
DD 04adfa541h, 03dd895d7h, 0a4d1c46dh, 0d3d6f4fbh
DD 04369e96ah, 0346ed9fch, 0ad678846h, 0da60b8d0h
DD 044042d73h, 033031de5h, 0aa0a4c5fh, 0dd0d7cc9h
DD 05005713ch, 0270241aah, 0be0b1010h, 0c90c2086h
DD 05768b525h, 0206f85b3h, 0b966d409h, 0ce61e49fh
DD 05edef90eh, 029d9c998h, 0b0d09822h, 0c7d7a8b4h
DD 059b33d17h, 02eb40d81h, 0b7bd5c3bh, 0c0ba6cadh
DD 0edb88320h, 09abfb3b6h, 003b6e20ch, 074b1d29ah
DD 0ead54739h, 09dd277afh, 004db2615h, 073dc1683h
DD 0e3630b12h, 094643b84h, 00d6d6a3eh, 07a6a5aa8h
DD 0e40ecf0bh, 09309ff9dh, 00a00ae27h, 07d079eb1h
DD 0f00f9344h, 08708a3d2h, 01e01f268h, 06906c2feh
DD 0f762575dh, 0806567cbh, 0196c3671h, 06e6b06e7h
DD 0fed41b76h, 089d32be0h, 010da7a5ah, 067dd4acch
DD 0f9b9df6fh, 08ebeeff9h, 017b7be43h, 060b08ed5h
DD 0d6d6a3e8h, 0a1d1937eh, 038d8c2c4h, 04fdff252h
DD 0d1bb67f1h, 0a6bc5767h, 03fb506ddh, 048b2364bh
DD 0d80d2bdah, 0af0a1b4ch, 036034af6h, 041047a60h
DD 0df60efc3h, 0a867df55h, 0316e8eefh, 04669be79h
DD 0cb61b38ch, 0bc66831ah, 0256fd2a0h, 05268e236h
DD 0cc0c7795h, 0bb0b4703h, 0220216b9h, 05505262fh
DD 0c5ba3bbeh, 0b2bd0b28h, 02bb45a92h, 05cb36a04h
DD 0c2d7ffa7h, 0b5d0cf31h, 02cd99e8bh, 05bdeae1dh
DD 09b64c2b0h, 0ec63f226h, 0756aa39ch, 0026d930ah
DD 09c0906a9h, 0eb0e363fh, 072076785h, 005005713h
DD 095bf4a82h, 0e2b87a14h, 07bb12baeh, 00cb61b38h
DD 092d28e9bh, 0e5d5be0dh, 07cdcefb7h, 00bdbdf21h
DD 086d3d2d4h, 0f1d4e242h, 068ddb3f8h, 01fda836eh
DD 081be16cdh, 0f6b9265bh, 06fb077e1h, 018b74777h
DD 088085ae6h, 0ff0f6a70h, 066063bcah, 011010b5ch
DD 08f659effh, 0f862ae69h, 0616bffd3h, 0166ccf45h
DD 0a00ae278h, 0d70dd2eeh, 04e048354h, 03903b3c2h
DD 0a7672661h, 0d06016f7h, 04969474dh, 03e6e77dbh
DD 0aed16a4ah, 0d9d65adch, 040df0b66h, 037d83bf0h
DD 0a9bcae53h, 0debb9ec5h, 047b2cf7fh, 030b5ffe9h
DD 0bdbdf21ch, 0cabac28ah, 053b39330h, 024b4a3a6h
DD 0bad03605h, 0cdd70693h, 054de5729h, 023d967bfh
DD 0b3667a2eh, 0c4614ab8h, 05d681b02h, 02a6f2b94h
DD 0b40bbe37h, 0c30c8ea1h, 05a05df1bh, 02d02ef8dh
end;

function Min( a, b : Integer ) : Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max( a, b : Integer ) : Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Abs( val : Integer ) : Integer;
begin
  if val < 0 then
    Result := -val
  else
    Result := val;
end;

function EncodeBlockSize( IsCompressed : Boolean; BlockSize : Integer ) : Integer;
begin
  // Format:
  // Bit 31 = 1 ? Compressed : Uncompressed
  // Bits 30..0 = Block size
  Result := BlockSize;
  if not IsCompressed then
    Result := - Result;
end;

procedure DecodeBlockSize( size : Integer; var IsCompressed : Boolean;
                           var BlockSize : Integer);
begin
  // Format:
  // Bit 31 = 1 ? Compressed : Uncompressed
  // Bits 30..0 = Block size
  IsCompressed := size >= 0;
  size := Abs(size);
  BlockSize := size;
end;

function CalcRatio( size, compressedSize : Integer ) : Integer;
begin
  if size > 0 then
    begin
      Result := 100 - MulDiv( compressedSize, 100, size );
      if Result < 0 then
        Result := 0;
    end
  else
    Result := 0;
end;

// Copied from unit FileCtrl.pas
function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function GetFileDate( const FileName : String ) : TDateTime;
var
  f : Integer;
  date : Integer;
begin
  f := FileOpen(FileName, fmShareDenyNone);
  try
    date := FileGetDate(f);
    if date <> -1 then
      Result := FileDateToDateTime( date )
    else
      Result := 0;
  finally
    FileClose(f);
  end;
end;

function GetFileSize( const fileName : String ) : Integer;
begin
  Result := 0;
  if FileExists( fileName ) then
    begin
      try
        with TFileStream.Create( fileName, fmOpenRead or fmShareDenyNone) do
          try
            Result := Size;
          finally
            Free;
          end;
      except
      end;
    end;
end;

function IsExeFile( const FileName : String ) : Boolean;
begin
  Result := UpperCase(ExtractFileExt(FileName)) = '.EXE';
end;

function GetTempDir : String;
var
  TmpPathLen : Integer;
begin
  TmpPathLen := 512;
  SetLength( Result, TmpPathLen + 1 );
  TmpPathLen := GetTempPath( TmpPathLen, PChar(Result) );
  if TmpPathLen = 0 then
    Result := 'c:\'
  else
    SetLength( Result, TmpPathLen );
  if Result[length(Result)] <> '\' then
    Result := Result + '\';
end;

procedure GetVersionInfo( const FileName : String; Infos : TStrings );
Var
  dwResourceSize : DWORD;
  dwJunk: DWORD; //The getfileversioninfosize set this to zero, otherwise unused
 
  pVersionInfo: Pointer;
  pTranslationTable: Pointer;
 
  sRequest: String;
 
  pData: Pointer;
  i : Integer;
 
const
  sVersionInfo : array [ 1 .. 12 ] of String = (
    'CompanyName',  'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright',  'OriginalFileName',
    'ProductName',  'ProductVersion',  'Author',
    'Comment', 'SFXCodeSize', 'TagInfoSize' );
Begin
  Infos.Clear;
  //Get the size of tbe version resource structure
  dwResourceSize := GetFileVersionInfoSize(PChar(Filename),dwJunk);
 
  if dwResourceSize>0 then
    begin
      GetMem(pVersionInfo,dwResourceSize);
 
      //get the version information
      GetFileVersionInfo(pChar(Filename),0,dwResourceSize,pVersionInfo);
 
      //get pointer to translation table
      VerQueryValue(pVersionInfo,'\\VarFileInfo\\Translation',pTranslationTable,dwResourceSize);
 
      //init the version value request string
      sRequest :='\\StringFileInfo\\'+IntToHex(LoWord(LongInt(pTranslationTable^)),4)+IntToHex(HiWord(LongInt(pTranslationTable^)),4)+'\\';
 
      for i := Low(sVersionInfo) to High(sVersionInfo) do
        begin
          //get the varies information strings
          if VerQueryValue(pVersionInfo,pChar(sRequest+sVersionInfo[i]),pData,dwResourceSize) then
            Infos.Values[sVersionInfo[i]] := pChar(pData);
        end;
 
      FreeMem(pVersionInfo,dwResourceSize);
    end;
end;

function MSecsAsTime( secs : Integer ) : TDateTime;
begin
  Result := secs / MSecsPerDay;
end;

function TimeAsMSecs( time : TDateTime ) : Integer;
var
  ts : TTimeStamp;
begin
  ts := DateTimeToTimeStamp( time );
  Result := ts.Time;
end;

var
  GetDiskFreeSpaceEx : function (lpDirectoryName: PAnsiChar;
    var lpFreeBytesAvailableToCaller : TLargeInteger;
    var lpTotalNumberOfBytes: TLargeInteger;
    var lpTotalNumberOfFreeBytes: TLargeInteger) : bool;
    stdcall;
  //external kernel32
  //name 'GetDiskFreeSpaceExA';

procedure GetDiskSizeAvail2(TheDrive : PChar;
                           var TotalBytes : double;
                           var TotalFree : double);
var
  sectors, bytesPerSector, freeClusters, clusters : DWORD;
begin
  if GetDiskFreeSpace( TheDrive, sectors, bytesPerSector, freeClusters, clusters ) then
    begin
      TotalBytes := sectors;
      TotalBytes := TotalBytes * bytesPerSector;
      TotalBytes := TotalBytes * clusters;
      TotalFree  := sectors;
      TotalFree  := TotalFree * bytesPerSector;
      TotalFree  := TotalFree  * freeClusters;
    end
  else
    begin
      TotalBytes := -1;
      TotalFree := -1;
    end;
end;

// Function copied from Inprise TI1630
// We use 2 methods for getting free disk space, because
// the GetDiskFreeSpaceEx doesn't exists in the first release of Win95.

procedure GetDiskSizeAvail(TheDrive : PChar;
                           var TotalBytes : double;
                           var TotalFree : double);
var
  AvailToCall : TLargeInteger;
  TheSize : TLargeInteger;
  FreeAvail : TLargeInteger;
begin
  if not Assigned(GetDiskFreeSpaceEx) then
    begin
      GetDiskSizeAvail2(TheDrive, TotalBytes, TotalFree );
      Exit;
    end;

  GetDiskFreeSpaceEx(TheDrive,
                     AvailToCall,
                     TheSize,
                     FreeAvail);
{$IFDEF DELPHI4_OR_HIGHER}
  TotalBytes := TheSize;
  TotalFree := AvailToCall;
{$ELSE}
  TotalBytes := TheSize.QuadPart;    {Double:=Comp by Mauro Favagrossa}
  TotalFree := AvailToCall.QuadPart; {by Mauro Favagrossa}
{$ENDIF}
end;

{
  This function enumerates the files contained in the directory "dir",
  using the filter "filter" and enumerates the sub-directories
  if recurseDir is true.
  All the enumerated files are placed in the "dest" list of strings.
  Each file placed in the list gets its size in the object property.

  it returns the size of the directory.
}
function EnumerateDirectory( const dir, filter : String;
                             recurseDir : Boolean;
                             dest : TStrings ) : Integer;

  function DoEnum( const dir, path : String ) : Integer;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    Result := 0;
    source := dir + path;
    if not DirectoryExists(source) then
      Exit;
    try
      Result := 0;
      // First look at files
      Found := FindFirst( source+'\'+Filter, faAnyFile, SR );
      try
        while Found = 0  do
          begin
            if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                if (SR.Attr and faDirectory) = 0 then
                   begin
                    // Add file
                    dest.AddObject( source+'\'+SR.Name, TObject(SR.Size) );
                    Inc( Result, SR.Size );
                  end;
              end;
            Found := FindNext( SR );
          end;
      finally
        FindClose(SR);
      end;
      // Then look at folders
      if recurseDir then
        begin
          Found := FindFirst( source+'\*.*', faDirectory, SR );
          try
            while Found = 0  do
              begin
                if (SR.Name <> '.') and (SR.Name <> '..') then
                  begin
                    if (SR.Attr and faDirectory) <> 0 then
                       Result := result + DoEnum( dir, path+'\'+SR.Name );
                  end;
                Found := FindNext( SR );
              end;
          finally
            FindClose(SR);
          end;
        end;
    except
    end;
  end;

begin
  Result := DoEnum( RemoveSlash(dir), '' );
end;


// by Mauro F.
function GetVolumeInfo( const drive: string; var VolumeName: string; var SerialNum: dword ): boolean;
var
  dummy      : DWord;
  VolNameBuf : array [0..255] of Char;
  psDrive    : PChar;
Begin
  SetLastError(0); {reset error}
  if drive <> '' then
    psDrive := PChar(drive[1]+':\')
  else
    psDrive := nil; {-> current drive}
  Result := GetVolumeInformation( psDrive, VolNameBuf, SizeOf(VolNameBuf), @SerialNum, dummy, dummy, nil,0);
  if Result then
    VolumeName := StrPas(VolNameBuf)
  else
    begin
      VolumeName := '';
      SerialNum := 0
    end;
End;

{$IFDEF VER90} // Delphi 2.0
// Override Sysutils.ExtractFileDrive() (fix a bug) in Delphi 2.0
// By Mauro Favagrossa
{ ExtractFileDrive extracts the drive part of the given filename.
  Return two form:
  '<drive>:' es. 'C:'
  '\\<servername>\<sharename>' }
function ExtractFileDrive(const FileName: string): string;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2 {prima era 3 [Mauro]}) and (FileName[2] = ':') then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 3 {prima era 2 [Mauro]}) and (FileName[1] = '\') and
    (FileName[2] = '\') then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = '\' then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = '\' then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;
{$ENDIF VER90}

initialization
  strOk := 'OK';
  strCancel := '&Cancel';
  strInformation := 'Information';
  strWarning := 'Warning';
  strConfirmation := 'Confirmation';
  strError := 'Error';
  strYes := '&Yes';
  strYesToAll := 'Yes to &All';
  strNo := '&No';
  strReplaceFile := 'Replace File :';
  strWithFile := 'With File :';
  strConfirmFileOverwrite := 'Confirm File Overwrite';
  strFile := 'File';
  strCanContinue := 'Do you want to continue ?';
  GetDiskFreeSpaceEx := GetProcAddress( LoadLibrary( kernel32 ), 'GetDiskFreeSpaceExA' );
finalization
end.
