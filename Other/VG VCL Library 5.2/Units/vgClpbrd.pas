{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Clipboard utility routines                    }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgClpbrd;

interface

procedure ClipboardCopy(const Text: String);
procedure ClipboardPaste(var Text: String);
{ Clipboard operations with Windows NT unicode support }
{ Originally by Alex Konshin }

implementation
uses Windows, SysUtils, Clipbrd;

procedure ClipboardCopy(const Text: String);
var
  Len, wLen: Integer;
  hClip: THandle;
  pwStr: PWideChar;
begin
  with Clipboard do
  begin
    Open;
    try
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        Len := Length(Text) + 1;
        wLen := Len shl 1;
        hClip := GlobalAlloc(GMEM_MOVEABLE, wLen);
        try
          pwStr := PWideChar(GlobalLock(hClip));
          MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(Text), Len, pwStr, wLen);
          GlobalUnlock(hClip);
          SetAsHandle(CF_UNICODETEXT, hClip);
        except
          GlobalFree(hClip);
          raise;
        end;
      end else
        SetTextBuf(PChar(Text));
    finally
      Close;
    end;
  end;
end;

procedure ClipboardPaste(var Text: String);
var
  Len, wLen: Integer;
  hClip: THandle;
  pwStr: PWideChar;
begin
  Text := '';
  with Clipboard do
  try
    Open;
    if HasFormat(CF_TEXT) or HasFormat(CF_UNICODETEXT) then
    begin
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      begin
        hClip := GetAsHandle(CF_UNICODETEXT);
        wlen := GlobalSize(hClip); // lstrlen and StrLen doesn't work
        pwStr := GlobalLock(hClip);
        try
          Len := (wLen div 2) - 1;
          SetLength(Text, Len);
          WideCharToMultiByte(CP_ACP, 0, pwStr, wlen, PChar(Text), Len, nil, nil);
        finally
          GlobalUnlock(hClip);
        end;
      end else begin // Win95
        hClip := GetAsHandle(CF_TEXT);
        Len := GlobalSize(hClip);
        SetLength(Text, Len);
        SetLength(Text, GetTextBuf(PChar(Text), Len));
      end;
    end;
  finally
    Close;
  end;
end;

end.
