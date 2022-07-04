unit Main;

interface
uses
  Windows, Messages, CommDlg, CryptoAPI, HashTests;

const
  IDC_COMBOBOX = 101; //Combobox for selecting hashes
  IDC_EDIT1    = 102; //Input Edit
  IDC_EDIT2    = 107; //Output Edit
  IDC_BUTTON   = 105; //Get hash of a file button

  HashesAvail: array[0..20] of String = (
    'MD2', 'MD4', 'MD5', 'SHA1', 'SHA256', 'SHA384',
    'SHA512', 'HAVAL128', 'HAVAL160', 'HAVAL192',
    'HAVAl224', 'HAVAL256', 'GOST', 'TIGER128',
    'TIGER160', 'TIGER192', 'RIPE-MD128', 'RIPE-MD160',
    'CRC32', 'CRC32b', 'Adler32'
  );

  HashesAvailID: array[0..20] of LongWord = (
    HASH_MD2, HASH_MD4, HASH_MD5, HASH_SHA1, HASH_SHA256, HASH_SHA384,
    HASH_SHA512, HASH_HAVAL128, HASH_HAVAL160, HASH_HAVAL192,
    HASH_HAVAl224, HASH_HAVAL256, HASH_GOST, HASH_TIGER128,
    HASH_TIGER160, HASH_TIGER192, HASH_RIPEMD128, HASH_RIPEMD160,
    HASH_CRC32, HASH_CRC32B, HASH_ADLER32
  );

procedure StartApp;

var
  FMsg: TMsg;
  FWnd, FInputEdit, FOutputEdit, FComboBox: THandle;

  FOfn: TOpenFilename;           //'Open filename' dialog data
  FObuf: array[0..255] of Char;  //Buffer for the file name

implementation

procedure UpdateHash;
var
  buf: Pointer;
  buf_len: LongWord;
  Hash: String;
  HashToUse, HError: LongWord;
begin
  {If nothing is selected in the combobox then exit}
  if SendMessage(FCombobox, CB_GETCURSEL, 0, 0) < 0 then
    Exit;

  {Length of buffer to allocate}
  buf_len := SendMessage(FInputEdit, WM_GETTEXTLENGTH, 0, 0) + 1;

  {Allocate buffer}
  GetMem(buf, buf_len);

  {Retrieve text from FInputEdit into allocated buffer}
  SendMessage(FInputEdit, WM_GETTEXT, buf_len, LongWord(buf));

  {Choose hash type}
  if SendMessage(FCombobox, CB_GETCURSEL, 0, 0) in
    [Low(HashesAvailID)..High(HashesAvailID)] then
      HashToUse := HashesAvailID[SendMessage(FCombobox, CB_GETCURSEL, 0, 0)]
  else
    HashToUse := 0; //Unknown hash type

  {Hash allocated buffer}
  HError := HashBuf(HashToUse, buf, buf_len - 1, Hash);

  if HError <> HASH_NOERROR then
    Hash := 'Error: ' + HashErrorToStr(HError);

  SendMessage(FOutputEdit, WM_SETTEXT, 0, LongWord(Hash));

  {Free buffer}
  FreeMem(buf, buf_len);
end;

procedure UpdateFileHash(FName: String);
var
  Hash: String;
  HashToUse, HError: LongWord;
begin
  {If nothing is selected in the combobox then exit}
  if SendMessage(FCombobox, CB_GETCURSEL, 0, 0) < 0 then
    Exit;

  {Choose hash type}
  if SendMessage(FCombobox, CB_GETCURSEL, 0, 0) in
    [Low(HashesAvailID)..High(HashesAvailID)] then
      HashToUse := HashesAvailID[SendMessage(FCombobox, CB_GETCURSEL, 0, 0)]
  else
    HashToUse := 0; //Unknown hash type

  {Hash file}
  HError := HashFile(HashToUse, FName, Hash);

  if HError <> HASH_NOERROR then
    Hash := 'Error: ' + HashErrorToStr(HError);

  {Set output edit}
  SendMessage(FOutputEdit, WM_SETTEXT, 0, LongWord(Hash));
end;

function WndProc(hwnd, wmsg, wParam, lParam: lParam): LongBool; stdcall;
var
  rcDlg, rcOwner: TRect;
  n: LongWord;
begin
  Result := False;
  case wmsg Of
    WM_INITDIALOG:
    begin
      {Move window to the center of the screen}
      GetWindowRect(GetDesktopWindow, rcOwner);
      GetWindowRect(hwnd, rcDlg);
      SetWindowPos(hwnd,
        HWND_TOP,
        rcOwner.Right div 2 - (rcDlg.Right - rcDlg.Left) div 2,
        rcOwner.Bottom div 2 - (rcDlg.Bottom - rcDlg.Top) div 2,
        0, 0,
        SWP_NOSIZE);

      {Get handles of dialog items}
      FInputEdit := GetDlgItem(hwnd, IDC_EDIT1);
      FOutputEdit := GetDlgItem(hwnd, IDC_EDIT2);
      FComboBox := GetDlgItem(hwnd, IDC_COMBOBOX);
      
      {Add strings to combobox}
      for n := Low(HashesAvail) to High(HashesAvail) do
        SendMessage(FComboBox, CB_ADDSTRING, 0, Integer(PChar(HashesAvail[n])));
      SendMessage(FComboBox, CB_SETCURSEL, 0, 0); //Set combobox selected item to 0

      {Update selected hash} 
      UpdateHash;
    end;
    WM_COMMAND:
    begin
      case loWord(wParam) of
        {On input edit change}
        $66: if LongWord(lParam) = FInputEdit then UpdateHash;
        {'Get hash of a file' button pressed}
        IDC_BUTTON:
        begin
          {Fill the openfile template}
          FillChar(FOfn, SizeOf(FOfn), 0);
          FOfn.lStructSize := SizeOf(FOfn);
          FOfn.hWndOwner := hwnd;
          FOfn.nFilterIndex := 1;
          FOfn.lpstrFilter := 'Any file (*.*)' + #0 + '*.*'#0;
          FOfn.nMaxFile := SizeOf(FOBuf);
          FOfn.lpstrFile := @FObuf;
          FOfn.Flags := OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST;
          {Show open file dialog}
          if GetOpenFileName(FOfn) then
            UpdateFileHash(FObuf);
        end;
      end;
      case hiWord(wParam) of
        {Message is sent when user selects item in a combobox}
        CBN_SELCHANGE: UpdateHash;
      end;
    end;
    WM_CLOSE:
      SendMessage(hwnd, WM_DESTROY, 0, 0);
    WM_DESTROY:
    begin
      EndDialog(hwnd, 0);
      PostQuitMessage(0);
    end;
  end;
end;

procedure StartApp;
var
  ErrNo: LongWord;
begin
  {Test library before startup}
  ErrNo := HashTestLibrary;
  if ErrNo <> HASH_NOERROR then
  begin
    MessageBox(0, PChar('Could not verify library, error: ' + HashErrorToStr(ErrNo)), nil, 0);
    Halt;
  end;

  FWnd := CreateDialog(hInstance, '#1', 0, @WndProc);
  while GetMessage(FMsg, 0, 0, 0) do
  begin
    TranslateMessage(FMsg);
    DispatchMessage(FMsg);
  end;
end;

end.