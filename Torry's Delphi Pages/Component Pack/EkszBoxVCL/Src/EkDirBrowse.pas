unit EkDirBrowse;

//================
//  EkDirBrowse
//================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//==============================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==============================================================================

interface

uses
  Windows, Classes, Forms, SysUtils, ShlObj, ShellApi;

var
  gInitPath : array[0..MAX_PATH] of Char;

function  BrowseCallBack(handle : THandle; msg : UINT; lpParam, lpData : LPARAM) : Integer; stdcall;
procedure CenterWnd(handle : THandle);

type
  TEkDirBrowse = class(TComponent)
  private

  protected

  public
    InitialDirectory  : String;
    Directory         : String;
    Title             : String;
    Handle            : THandle;

    function Execute: Boolean;

  published
  end;

procedure Register;

implementation

//==============================================================================

function BrowseCallBack(handle : THandle; msg : UINT; lpParam, lpData : LPARAM): Integer; stdcall;
var
  path : array[0..MAX_PATH] of Char;
begin

  if msg = BFFM_SELCHANGED then
    begin
      SHGetPathFromIDList(PItemIDList(lpParam), @path);
      SendMessage(handle, BFFM_SETSTATUSTEXT, 0, LongInt(PChar(@path)));
    end
  else if msg = BFFM_INITIALIZED then
    begin
      SendMessage(handle, BFFM_SETSELECTION, 1, LongInt(@gInitPath));
      CenterWnd(handle);
    end;

  Result := 0;

end;

//==============================================================================

procedure CenterWnd(handle : THandle);
var
  r: TRect;
begin

GetWindowRect(handle, r);

SetWindowPos(handle, 0,
(GetSystemMetrics(SM_CXSCREEN) - r.Right + r.Left) div 2,
(GetSystemMetrics(SM_CYSCREEN) - r.Bottom + r.Top) div 3,
0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);

end;

//==============================================================================

function TEkDirBrowse.Execute() : Boolean;
const
  BIF_NEWDIALOGSTYLE = $0040; // Req. for older delphi versions
var
  RetBuffer,
  FName,
  ResultBuffer : array[0..MAX_PATH] of Char;
  BrowseInfo : TBrowseInfo;
  PIDL : PItemIDList;
begin

  // Init buffers
  FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
  Fillchar(RetBuffer, SizeOf(RetBuffer), #0);
  FillChar(ResultBuffer, SizeOf(ResultBuffer), #0);

  BrowseInfo.hwndOwner := Handle; 
  BrowseInfo.pszDisplayName := @Retbuffer;
  BrowseInfo.lpszTitle := @Title[1];

  if (Length (InitialDirectory) <> 0) then Move(InitialDirectory[1], gInitPath, MAX_PATH);

  // Options
  BrowseInfo.ulFlags := BIF_NEWDIALOGSTYLE or BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
  //or BIF_USENEWUI

  // Call-back function
  BrowseInfo.lpfn := @BrowseCallBack;
  BrowseInfo.lParam := Integer(@FName);

  // Show Browse For Folder Dlg
  PIDL := SHBrowseForFolder(BrowseInfo);

  // Return full path to file 
  if SHGetPathFromIDList(PIDL, ResultBuffer) then
  begin

    if (Length (InitialDirectory) <> 0) and (InitialDirectory <> ResultBuffer)
    then InitialDirectory := ResultBuffer;

    Move(ResultBuffer, gInitPath, MAX_PATH);

    Result := True;
    Directory := StrPas(ResultBuffer);
  end
  else
    Result := False;

  GlobalFreePtr(PIDL); // Cleanup

end;

//==============================================================================

procedure Register;
begin
  RegisterComponents('EkszBoxVCL', [TEkDirBrowse]);
end;

end.
