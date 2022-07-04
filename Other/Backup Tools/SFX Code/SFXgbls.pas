{
  TArchiverSFX (C) Copyright by Oliver Buschjost (autor_oliver@iname.com), 1998

  I didn't mark my modifications, because there were to many. But on request I
  can send you the original Archive with the original ZIP-SFX package.

- This SFX is based on Freewarecode that is copyrighted by:
}
(******************************************************************)
(* Copyright 1997, Microchip Systems / Carl Bunton                *)
(* Email: Twojags@cris.com                                        *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(*                                                                *)
(* This program was written in Delphi 2 because version 2         *)  //## this release in delphi3
(* compiles a much smaller executable using the windows api.  It  *)
(* should be fully compatible with Delphi 3, but will produce a   *)
(* noticable increase of size in the final compiled program.      *)
(*                                                                *)
(*MODIFIED by M. Stephany mirbir.st@t-online.de  12/28/97-01/04/98*)
(* for some special purposes; modified lines are marked (##)      *)
(******************************************************************)

UNIT SFXgbls;

{$A-}

INTERFACE

USES Messages, Windows, Dialogsel, Extractor, ArchiverRoot, CustExtractor, SysUtils, SfxMisc;

TYPE
  TLocal = RECORD
     SignAtr        : LONGINT;
     VerNum         : WORD;  (* VersionMadeBy *)
     BitFlag        : WORD;
     CompressType   : WORD;
     FileDate       : LONGINT;
     crc32          : LONGINT;
     PackedSize     : LONGINT;
     UnpackedSize   : LONGINT;
     FilenameLen    : WORD;
     ExtraFieldLen  : WORD;
  END;
TMyClass = class(TObject)
  procedure ExtractorEnumeration( Sender : TObject;  const FileEntry :TFileEntry );
  procedure ExtractorStartOperation(Sender: TObject);
  procedure ExtractorFileProgress(Sender: TObject; Percent1: Integer);
  procedure ExtractorFinishOperation(Sender: TObject);
  procedure ExtractorExtractFile(Sender: TObject; const FileEntry: TFileEntry; var DestPath: string; var Accept: Boolean);
  procedure ExtractorFileExtracted(Sender: TObject;  const FileEntry: TFileEntry; const DestPath: string);
  procedure ExtractorShowComment(Sender: TObject; const Comment: string);
//  procedure ExtractorShowTiming(Sender: TObject; ElapsedTime, RemainingTime: TDateTime);
  constructor Create;
  destructor Destroy; override;
end;

VAR
  InFile         : THandle;
  OutFile        : THandle;
  Extr      : TExtractor;
  hDlgProgress   : hWnd;
  MainWinhwndDlg : hWnd;
  EnumerateYet   : boolean;
  ArchiveComment : string;
  //## the getarchiveoffset has been removed since this value is calculated in dialogsel.pas

VAR
   FileNow : String;
  Cancel_Progress:boolean;

(*--------------------------------------------------------------------------*)
// These functions are copyrighted by Arjen Broeze (Arjen@nedap.nl)
  function ProgressDialogProc(hwndDlg : hWnd; uMsg : UInt; wp : WParam; lp : LParam): Bool; stdcall; export;
  procedure DrawProgress(hwndProgress : hWnd; hdcProgress : hDC; Rect : TRect);
  procedure Progress(const FileName : String; const FileBytesWritten, FileBytes,
                                TotalBytesWritten, TotalBytes: Integer; var Cancel : Boolean);
  function MinimizeName(const Filename: String): String;
  function TooBig(hdcCtrl : hDC; const Width : Integer; const S : String): Boolean;
  PROCEDURE ProcessArchive(DlgWin: hWnd; lb:integer; FillListBox: BOOLEAN);
(*--------------------------------------------------------------------------*)

IMPLEMENTATION

USES Dialog;


(*--------------------------------------------------------------------------*)
constructor TMyClass.Create;
begin
  Extr:=TExtractor.create(nil);
  Extr.SFXCodeSize:=StartOfFile;
  Extr.filename:=ParamStr( 0 ){'c:\sfx140a\MMM - SFX\sfx1.exe'};
  Extr.Language:=Extr.Language;

  Extr.OnEnumeration := ExtractorEnumeration;
  Extr.OnStartOperation := ExtractorStartOperation;
  Extr.OnFileProgress := ExtractorFileProgress;
  Extr.OnFinishOperation := ExtractorFinishOperation;
  Extr.OnExtractFile:=ExtractorExtractFile;
  Extr.OnFileExtracted:=ExtractorFileExtracted;
  Extr.OnShowComment:=ExtractorShowComment;
//  Extr.OnShowTiming:=ExtractorShowTiming;
  end;

destructor TMyClass.destroy;
begin
  Extr.Free;
end;
(*--------------------------------------------------------------------------*)
procedure TMyClass.ExtractorEnumeration(Sender: TObject;
  const FileEntry: TFileEntry);
var
    hWndList   : hWnd;
begin
  hWndList := Extr.Tag;
 IF FileEntry.Name = '' THEN
     BEGIN
          MessageBox(0, STR_INVALIDNAME, STR_E, mb_OK);
         EXIT;
     END;
 (* Add each string to the listbox *)
SendMessage(hWndList, LB_ADDSTRING, 0, LONGINT(PCHAR(FileEntry.Name)))
end;

procedure TMyClass.ExtractorStartOperation(Sender: TObject);
Begin
hDlgProgress := CreateDialog(hInstance, 'SEProgressClass', 0, @ProgressDialogProc);
ShowWindow(hDlgProgress, SW_SHOW);
EnableWindow(MainWinhwndDlg, Bool(0));
End;

procedure TMyClass.ExtractorFileProgress(Sender: TObject; Percent1: Integer);
var cancel:boolean;
Begin
if (Extr.operation=opExtract) and (Extr.IsSolidArchive) and not (EnumerateYet) then FileNow := SfxMisc.Msg.PreparingExtraction else
if Extr.operation=opEnumerate then begin FileNow := SfxMisc.Msg.ReadingArchive; EnumerateYet:=True; end;

Progress(FileNow, 0, 0, Percent1, 100, cancel);
if Cancel_Progress then abort;
End;

procedure TMyClass.ExtractorFinishOperation(Sender: TObject);
Begin
EnableWindow(MainWinhwndDlg, Bool(1));
DestroyWindow(hDlgProgress);
Processresult:=True;
End;

procedure TMyClass.ExtractorFileExtracted(Sender: TObject;
  const FileEntry: TFileEntry; const DestPath: string);
var   hWndList    : hWnd;
      index       :integer;
begin
   hWndList:=Extr.Tag;
   Index := -1;
   index:=SendMessage(hWndList, LB_FINDSTRINGEXACT, Index, LONGINT(pchar(FileEntry.Name)));
   if index<>-1 then SendMessage(hWndList, LB_SETSEL, 0, index);
end;

procedure TMyClass.ExtractorExtractFile(Sender: TObject;
  const FileEntry: TFileEntry; var DestPath: string; var Accept: Boolean);
var   hWndList    : hWnd;
begin
   hWndList:=Extr.Tag;
   if IsSelected(hWndList, PCHAR(FileEntry.Name))
     then
       begin
            FileNow := DestPath + FileEntry.Name;
            Accept:=True;
       end
     else Accept:=false;
end;

procedure TMyClass.ExtractorShowComment(Sender: TObject;
  const Comment: string);
begin
ArchiveComment:=Comment;
if Dialogsel.TagInfo.comment in [Before,Both] then ShowComment(SplitLine(1,ArchiveComment));
end;

{procedure TMyClass.ExtractorShowTiming(Sender: TObject; ElapsedTime,
  RemainingTime: TDateTime);
begin
end;}
(*--------------------------------------------------------------------------*)
PROCEDURE ProcessArchive(DlgWin: hWnd;lb:integer; FillListBox: BOOLEAN);
VAR
   hWndList   : hWnd;
BEGIN
   processresult := false; //## added to know whether there have occured problems or not (yes, it could also be a bool-function)

   (* Get handle of ArchiveFileListbox *)
   hWndList := GetDlgItem(DlgWin, lb); //## changed
   Extr.Tag:=hWndList;
   (* Start search at beginning of listbox *)
   Index := -1;
   (* Clear the listbox *)
   IF FillListBox THEN BEGIN
   SendMessage(hWndList, LB_RESETCONTENT, 0, 0);
   try
//      Extr.enumeratefiles;
      Extr.options:=Extr.options+[oEnumerateAfterOpen];
      Extr.open;
   except
      on E: EAbort do
                     begin
                       halt;   
                       raise; // raise again the current trapped exception
                     end;
       on E: Exception do
        MessageBox ( 0,pchar(E.Message),STR_E, mb_iconerror);
   end;

   END

else begin

BEGIN
CASE OverWriteMode OF
0: Extr.RestoreAction:=raOverwrite;      (* Overwrite *)
1: Extr.RestoreAction:=raSkip;           (* Skip *)
2: Extr.RestoreAction:=raAsk;            (* Confirm *)
3: Extr.RestoreAction:=raUpdate;         (* Update *)
4: Extr.RestoreAction:=raExistingOnly;   (* ExistingOnly *)
5: Extr.RestoreAction:=raUpdateExisting; (* Update Existing *)
END;

GetDlgItemText(DlgWin, CM_EDIT1, ExtPath, fsMaxPath);
ForceDirectories(ExtPath);
Extr.ExtractPath := extpath;

try
Extr.Extractfiles;
except
 on E: EAbort do
                begin
                  halt;
                  raise; // raise again the current trapped exception
                end;
  on E: Exception do
   MessageBox ( 0,pchar(E.Message),STR_E, mb_iconerror);
end;

end;

  IF NOT FillListBox THEN
  BEGIN
     IF SendMessage(hWndList, LB_GETSELCOUNT, 0, 0) = 0 THEN
     begin
        MessageBox(0, STR_ALLEXT, STR_OK, mb_OK);
        //## removed the setting of the status-text cause we want to close the main-dialog now.
        processresult := true;
     end
     ELSE
     begin
        MessageBox(0, STR_NOTSELEXT, STR_E, mb_OK);
        processresult := false;
     end
  END
  ELSE
     (* if listbox empty, exit program *)
     IF FillListBox THEN
        IF SendMessage(hWndList, LB_GETCOUNT, 0, 0) = 0 THEN
            MessageBox(0, STR_EARCHIVE, STR_E, mb_OK);

 END;
 END (* ProcessArchive *);
(*--------------------------------------------------------------------------*)

function ProgressDialogProc(hwndDlg : hWnd; uMsg : UInt; wp : WParam; lp : LParam): Bool; stdcall; export;
var // This function is copyrighted by Arjen Broeze (Arjen@nedap.nl)
  Rect : TRect;
  iWidth, iHeight : Integer;
  cm1:integer;
begin
  Result := WordBool(0);
  case uMsg of
    WM_INITDIALOG:
     begin
       GetWindowRect(hwndDlg, Rect);
       iWidth := Rect.right - Rect.left;
       iHeight := Rect.bottom - Rect.top;
       Rect.left := (GetSystemMetrics(SM_CXSCREEN) - iWidth) div 2;
       Rect.top := (GetSystemMetrics(SM_CYSCREEN) - iHeight) div 2;
       MoveWindow(hwndDlg, Rect.left, Rect.top, iWidth, iHeight, False);
       {$IFDEF WINDOWS}
       SetClassWord(hwndDlg, GCW_HBRBACKGROUND, hbr);
       {$ENDIF}

       strpcopy(temp,SfxMisc.Msg.CANCEL);
       cm1 := getdlgitem(hwndDlg, 3); // the 'Cancel' Button
       sendmessage(cm1,wm_settext,0,integer(temp));

       setwindowtext(hwndDlg,caption); //The Caption

     end;
    {$IFDEF WINDOWS}
    WM_ERASEBKGND:
     begin
       GetClientRect(hwndDlg, Rect);
       FillRect(hDC(wp), Rect, hbr);
       Result := Bool(1);
     end;
    WM_CTLCOLOR:
     begin
       if (HIWORD(lp)=CTLCOLOR_BTN) or (HIWORD(lp)=CTLCOLOR_STATIC) then
        begin
          SetBkMode(HDC(wp), TRANSPARENT);
          SetBkColor(HDC(wp), GetSysColor(COLOR_BTNFACE));
          Result := Bool(hbr);
        end;
     end;
    {$ENDIF}
    WM_DRAWITEM:
     begin
       with PDrawItemStruct(lp)^ do
        DrawProgress(hwndItem, hdc, rcItem);
       Result := Bool(1);
     end;
    WM_COMMAND:
     if (LOWORD(wp)=3) then Cancel_Progress:=True;
  end;
end;

procedure DrawProgress(hwndProgress : hWnd; hdcProgress : hDC; Rect : TRect);
var // This function is copyrighted by Arjen Broeze (Arjen@nedap.nl)
  iPercent,
  iWidth   : Integer;
  nRect    : TRect;
  hbr      : HBrush;
  hrgn     : THandle;
  b        : Bool;
  pPercent : PChar;

begin
  with Rect do
   begin
     MoveToEx(hdcProgress, left, top, nil);
     LineTo(hdcProgress, left, bottom-1);
     LineTo(hdcProgress, right-1, bottom-1);
     LineTo(hdcProgress, right-1, top);
     LineTo(hdcProgress, left, top);
   end;
  InflateRect(Rect, -1, -1);
  iPercent := GetDlgItemInt(hdlgProgress, 2, b, Bool(1));
  pPercent := StrAlloc(5);
  try
    StrPCopy(pPercent, IntToStr(iPercent)+'%');

    SetBkMode(hdcProgress, TRANSPARENT);
    SelectClipRgn(hdcProgress, 0);

    iWidth := Trunc((rect.right-rect.left) * (iPercent/100));
    if iWidth>Rect.right-Rect.left then iWidth := Rect.right-Rect.left;

    CopyRect(nRect, Rect);
    inc(nRect.left, iWidth);

    hbr := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    try
      FillRect(hdcProgress, nRect, hbr);
    finally
      DeleteObject(hbr);
    end;

    with Rect do
     hRgn := CreateRectRgn(left+iWidth, top-1, right+1, bottom+1);
    try
      SelectClipRgn(hdcProgress, hRgn);
    finally
      DeleteObject(hRgn);
    end;

    SetTextColor(hdcProgress, GetSysColor(COLOR_HIGHLIGHT));
    DrawText(hdcProgress, pPercent, lstrlen(pPercent), Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    SelectClipRgn(hdcProgress, 0);

    CopyRect(nRect, Rect);
    nRect.right := nRect.left + iWidth;
    hbr := CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));
    try
      FillRect(hdcProgress, nRect, hbr);
    finally
      DeleteObject(hbr);
    end;

    with Rect do
     hRgn := CreateRectRgn(left-1, top-1, iWidth+1, bottom+1);
    try
      SelectClipRgn(hdcProgress, hRgn);
    finally
      DeleteObject(hRgn);
    end;
    SetTextColor(hdcProgress, GetSysColor(COLOR_HIGHLIGHTTEXT));
    DrawText(hdcProgress, pPercent, lstrlen(pPercent), Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  finally
    SelectClipRgn(hdcProgress, 0);
    StrDispose(pPercent);
  end;
end;

procedure Progress(const FileName : String; const FileBytesWritten, FileBytes,
                                TotalBytesWritten, TotalBytes: Integer; var Cancel : Boolean);
var // This function is copyrighted by Arjen Broeze (Arjen@nedap.nl)
  pFile    : PChar;
  sFile    : String;
  iPercent : Integer;
  b        : Bool;
begin
  pFile := StrAlloc(MAX_PATH);
  try
    sFile := MinimizeName(FileName);
    GetDlgItemText(hdlgProgress, 1, pFile, MAX_PATH);
    if lstrcmpi(pFile, StrPChar(sFile))<>0 then
     SetDlgItemText(hdlgProgress, 1, StrPChar(sFile));
  finally
    StrDispose(pFile);
  end;
  iPercent := Trunc((TotalBytesWritten/TotalBytes)*100);
  if iPercent<>Integer(GetDlgItemInt(hdlgProgress, 2, b, Bool(1))) then
   SetDlgItemText(hdlgProgress, 2, StrPChar(IntToStr(iPercent)));
   ProcessMessages;
end;

function MinimizeName(const Filename: String): String;
var // This function is copyrighted by Arjen Broeze (Arjen@nedap.nl)
  Drive: string[3];
  Name, Ext: String;
  Dir : TFileName;
  P: Integer;
  hwndCtrl : hWnd;
  hdcCtrl  : hDC;
  rect     : TRect;
begin

  hwndCtrl := GetDlgItem(hdlgProgress, 1);
  hdcCtrl := GetDC(hwndCtrl);
  GetClientRect(hwndCtrl, rect);

  Result := FileName;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);
  P := Pos('.', Name);
  if P > 0 then
   SetLength(Name, P-1);
  Ext := ExtractFileExt(Result);

 if length(Dir)>2 then begin
  if Dir[2] = ':' then
  begin
    Drive := Copy(Dir, 1, 2);
    Dir := Copy(Dir, 3, 255);
  end else Drive := '';
  end;
  while ((Dir <> '') or (Drive <> '')) and (TooBig(hdcCtrl, rect.right-rect.left, Result)) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end else if Dir = '' then Drive := ''
    else CutFirstDirectory(Dir);
    Result := Drive + Dir + Name + Ext;
  end;
  ReleaseDC(hwndCtrl, hdcCtrl);
end;

function TooBig(hdcCtrl : hDC; const Width : Integer; const S : String): Boolean;
var // This function is copyrighted by Arjen Broeze (Arjen@nedap.nl)
  Size     : TSize;
begin
  GetTextExtentPoint32(hdcCtrl, PChar(S), Length(S), Size);
  Result := Size.cx>Width;
end;

END.
