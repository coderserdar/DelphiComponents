unit nmzIrfanXnView;

// -----------------------------------------------------------------------------
// Class Name:      TIrfanXnView
// Module:          nmzIrfanXnView
// Description:     Implements support for IrfanView and XnView.
// Usage:           IrfanXnView := TIrfanXnView.Create('C:\Program Files\IrfanView\i_view32.exe');
//                  IrfanXnView.GetImage('c:\somepick.psd', Bitmap);
// Version:         1.1
// Date:            26-SEP-2006
// Target:          Win32, Delphi
// Author:          Nikolay M. Zhukov, http://www.nmzlabs.com, manager@nmzlabs.com
// Copyright:       © 2006 Nikolay M. Zhukov
// License:         Freeware
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------
// Minor changes:
// Sep 2007 by Alexey Torgashin (marked by '//AT')
// -----------------------------------------------------------------

interface

uses
  Windows,
  SysUtils,
  Messages,
  Controls,
  Graphics,
  Forms;

type
  TIrfanXnView = class(TObject)
  private
    FWindow     : HWND;
    FPath       : AnsiString;
    FForm       : TForm;
    FOldWndProc : TWndMethod;
    FHostLoaded : Boolean;
    FBitmapHandle : HBitmap; //AT
    FProcessInfo: TProcessInformation;
    procedure WndProc(var Message: TMessage);
    procedure LoadHost;
    procedure CloseHost;
    procedure SetHost(const Value: AnsiString);
  public
    // This class is not thread-safe!!! Do not call this function from different
    // threads! Create another instance of the TIrfanXnView and use it.
    constructor Create(const APath : AnsiString);
    destructor  Destroy; override;
    function    GetBitmap(const AFileName : AnsiString): HBitmap; //AT
    property    Host : AnsiString read FPath write SetHost;
  end;


implementation

const
  IrfanXnClassName = 'IrfanView';
  ParamHidden      = ' /hidden';
  
type
  TImgBufHeader = packed record
    copydatahandle : HWND;
    width          : integer;
    height         : integer;
    bits           : AnsiChar;
  end;
  PImgBufHeader = ^TImgBufHeader;


{ TIrfanXnView }

procedure TIrfanXnView.CloseHost;
begin
  if FProcessInfo.hProcess <> 0 then begin
    FHostLoaded := False;
    TerminateProcess(FProcessInfo.hProcess, 0);
  end;
end;

constructor TIrfanXnView.Create(const APath : AnsiString);
begin
  inherited Create;
  FPath            := APath;
  FForm            := TForm.Create(nil);
  FOldWndProc      := FForm.WindowProc;
  FForm.WindowProc := WndProc;
  ZeroMemory(@FProcessInfo, SizeOf(FProcessInfo));
end;

destructor TIrfanXnView.Destroy;
begin
  if FHostLoaded then
    CloseHost;

  FForm.WindowProc := FOldWndProc; //AT
  FOldWndProc := nil; //AT
  FForm.Release;      //AT, was FForm.Free
  FForm := nil;       //AT

  inherited;
end;

function TIrfanXnView.GetBitmap(const AFileName : AnsiString): HBitmap;
var
  CopyData  : TCopyDataStruct;
  ShortName : AnsiString;
  ImgBuf    : TImgBufHeader;
  Data, Temp: PAnsiChar;
  DataSize  : Integer;
begin
  Result := 0; //AT
  FBitmapHandle := 0;
  if not FHostLoaded then
    LoadHost;
  if not FHostLoaded then
    Exit;
  ZeroMemory(@CopyData, SizeOf(CopyData));
  CopyData.dwData       := 1;
  CopyData.cbData       := 512;
  ShortName             := ExtractShortPathName(AFileName); //AT
  ImgBuf.copydatahandle := FForm.Handle;
  ImgBuf.width          := 0;
  ImgBuf.height         := 0;
  DataSize              := SizeOf(ImgBuf) + Length(ShortName) + 1;
  Data                  := GetMemory(DataSize);
  Temp                  := Data;
  ZeroMemory(Data, DataSize);
  CopyMemory(Data, PAnsiChar(ShortName), Length(ShortName));
  Inc(Data, Length(ShortName) + 1);
  CopyMemory(Data, @ImgBuf, SizeOf(ImgBuf));
  CopyData.lpData := Temp;
  SendMessage(FWindow, WM_COPYDATA, FForm.Handle, Integer(@CopyData));
  Result := FBitmapHandle; //AT
end;

procedure TIrfanXnView.LoadHost;
var
  StartupInfo        : _StartupInfoA;
  Wnd : HWND;
  ThreadID : DWORD;
begin
  ZeroMemory(@FProcessInfo, SizeOf(TProcessInformation));
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  if CreateProcessA(PAnsiChar(FPath),
                   ParamHidden,
                   nil,
                   nil,
                   False,
                   0,
                   nil,
                   PAnsiChar(ExtractFileDir(FPath)),
                   StartupInfo,
                   FProcessInfo) then begin
    // We need wait until host application completely loaded.
    WaitForInputIdle(FProcessInfo.hProcess, INFINITE);
    // Now we need find window.
    Wnd := FindWindow(IrfanXnClassName, nil);
    while Wnd <> 0 do begin
      // We can use only window which we create by ourself!
      ThreadID := GetWindowThreadProcessId(Wnd, nil);
      if ThreadID = FProcessInfo.dwThreadId then begin
        FWindow := Wnd;
        FHostLoaded := True;
        Exit;
      end;
      Wnd := FindWindow(IrfanXnClassName, nil);
    end;
    // We can't find main host window, because somthing wrong... Close host process
    CloseHost;
  end;
end;

procedure TIrfanXnView.SetHost(const Value: AnsiString);
begin
  if Value <> FPath then begin
    FPath := Value;
    CloseHost;
  end;
end;

procedure TIrfanXnView.WndProc(var Message: TMessage);
var
  ImgBits      : PAnsiChar;
  TmpPtr       : PAnsiChar;
  CopyData     : PCopyDataStruct;
  BitmapInfo   : PBITMAPINFO;
  HeaderSize   : Integer;
  ImageSize    : Integer;
begin
  // Irfan/XnView send result bitmap to our window, using WM_COPYDATA 
  if Message.Msg = WM_COPYDATA then begin
   CopyData   := PCopyDataStruct(Message.LParam);
   BitmapInfo := PBITMAPINFO(CopyData^.lpdata);
   HeaderSize := BitmapInfo.bmiHeader.biSize + BitmapInfo.bmiHeader.biClrUsed * SizeOf(TRGBQUAD);
   // XnView returns image size in the BitmapInfo.bmiHeader.biSizeImage, but
   // IrfanView always return 0. So, we need to calculate image size by itself.
   ImageSize := ((((BitmapInfo.bmiheader.biWidth * BitmapInfo.bmiheader.biBitCount) + 31) and (not 31)) div 8) * BitmapInfo.bmiheader.biHeight;
   if (BitmapInfo.bmiHeader.biWidth > 0) and (BitmapInfo.bmiHeader.biHeight > 0) then begin
     FBitmapHandle := CreateDIBSection(0, BitmapInfo^, DIB_RGB_COLORS, Pointer(ImgBits), 0, 0);
     if FBitmapHandle <> 0 then begin
       GDIFlush;
       TmpPtr := CopyData^.lpdata;
       Inc(tmpptr, HeaderSize);
       move(TmpPtr[0], ImgBits[0], ImageSize);
     end;
   end;
  end
  else
  begin
    if Assigned(FOldWndProc) then //AT
      FOldWndProc(Message);
  end;
end;

end.
