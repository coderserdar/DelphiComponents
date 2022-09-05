unit uDoDrawHook;

interface

uses
  Windows, SysUtils, Classes, Graphics, ImgList, CommCtrl;

implementation

uses uImgTools;

type
  TJmpCommand = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  TImgListHack = class(TCustomImageList);

const
  // This was found in bds.exe imports section
  DoDrawFunctionName = '@Imglist@TCustomImageList@DoDraw$qqrip16Graphics@TCanvasiiuio';

var
  Backup: TJmpCommand;
  OriginalDoDraw: Pointer;

{ Hooking }

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TJmpCommand);
var
  n: DWORD;
  Code: TJmpCommand;
begin
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9; // JMP
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TJmpCommand);
var
  n: Cardinal;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

function FindVclBpl: string;
var
  SR: TSearchRec;
begin
  if SysUtils.FindFirst(GetEnvironmentVariable('WINDIR') + '\system32\vcl*.bpl', faAnyFile, SR) = 0 then
    Result := SR.Name
  else
    Result := '';
  SysUtils.FindClose(SR);
end;

procedure MyDraw(Self: TObject; Index: Integer; Canvas: TCanvas;
  X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  IL: TImgListHack;
  Gray, Mask: TBitmap;
begin
  IL := TImgListHack(Self);

  if not Assigned(IL) or not IL.HandleAllocated then
    Exit;

  if not Enabled then
  begin
    Gray := TBitmap.Create;
    Mask := TBitmap.Create;
    try
      Gray.SetSize(IL.Width, IL.Height);
      Mask.SetSize(IL.Width, IL.Height);
      IL.GetImages(Index, Gray, Mask);
      Grayscale(Gray);
      BitBlt(Canvas.Handle, X, Y, IL.Width, IL.Height, Mask.Canvas.Handle, 0, 0, SRCERASE);
      BitBlt(Canvas.Handle, X, Y, IL.Width, IL.Height, Gray.Canvas.Handle, 0, 0, SRCINVERT);
    finally
      FreeAndNil(Gray);
      FreeAndNil(Mask);
    end;
  end else
    ImageList_DrawEx(IL.Handle, Index, Canvas.Handle, X, Y, 0, 0,
      GetRGBColor(IL.BkColor), GetRGBColor(IL.BlendColor), Style);
end;

initialization
  // Replace FindVclBpl by 'vcl###.bpl' if you have several Delphi installations
  // (### is your Delphi vcl version, eg. vcl140.bpl for Delphi 2010)
  OriginalDoDraw := GetProcAddress(LoadPackage(FindVclBpl), DoDrawFunctionName);
  if OriginalDoDraw <> nil then
    HookProc(OriginalDoDraw, @MyDraw, Backup);

finalization
  if OriginalDoDraw <> nil then
    UnhookProc(OriginalDoDraw, Backup);

end.

