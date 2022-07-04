// unit GPPDrive
//
// This unit takes charge of driving Winamp general purpose plug-ins.
// This unit creates a seperate thread to drive Winamp general purpose plug-ins and the
//  created thread drives the plug-ins according to the directives of main thread.
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.2             20 Apr 2009
//   - Modified for Delphi 2009
//
// Ver 1.1              7 Oct 2008
//   Added procedures, functions to drive multiple Winamp GPPs simultaneously.
//   Changed procedure ShowMiniBrowser (added 2 parameters : Init_Width, Init_Height)
//
// (supplement at Ver 1.1.1)
//   - Added a function : GetGenDrawerInfo, GetLyricsText, AddMagneticWindow, RemoveMagneticWindow,
//                        GetSubClass_Proc
//   - Removed a function : GetGenHandles
//   - Changed function : InitGenDrawer (added a parameter "Requester")
//
// (supplement at Ver 1.1.2)
//   - Added a procedure : MBNavigate2
//   - Added procedures to support VisDrawer2.dll
//
// Ver 1.0                1 Oct 2008
//   Initial release


unit GPPDrive;

interface

uses Windows, Classes, Messages, SysUtils, ioplug, PluginCtrl, Forms;

  function  Start_GPP(GPP_Path : string; StatusMsg : HWND; ParentHWND : HWND) : integer;
  function  Start_AllGPPs(StatusMsg : HWND; ParentHWND : HWND) : integer;
  procedure Quit_GPP(GPP_RegNo : integer);
  procedure Quit_AllGPPs;

  function  Load_MBDrawerDLL(const dllfilename : string) : boolean;
  function  Load_VisDrawerDLL(const dllfilename : string) : boolean;
  procedure Unload_MBDrawerDLL;
  procedure Unload_VisDrawerDLL;
  function  Get_MBDrawerDLL_Handle : HWND;
  function  GetVisDrawerDLL2Handle : HWND;

var
  InitMiniBrowser: function(hDllInstance, MainHandle, MsgHandle, DataMsgId : HWND) : HWND; cdecl;
  ShowMiniBrowser: procedure(Loc_X, Loc_Y, Init_Width, Init_Height : integer; AllowDock : boolean); cdecl;
  MBNavigate:      procedure(URL : pAnsiChar); cdecl;
  MBNavigate2:     procedure(Artist, Title : pAnsiChar; UseCixURL : boolean);  cdecl;   // * Addd at Ver 1.1.2
  QuitMiniBrowser: procedure; cdecl;
  AddMagneticWindow: function(Handle: HWND; HWndParent: HWND; var SubClass_Proc : Pointer): Boolean; cdecl; // * Addd at Ver 1.1.1
  RemoveMagneticWindow: function(Handle: HWND): Boolean; cdecl;   // * Addd at Ver 1.1.1
  GetSubClass_Proc: function : TSubClass_Proc; cdecl;   // * Addd at Ver 1.1.1
 // Followings are the procedures, functions to support Winamp GPP, at Ver 1.1
  InitGenDrawer:  function(Requester : pchar; Main_Handle, Msg_Handle, Msg_Id : HWND; var FormNo : integer) : HWND; cdecl;
  ShowGenDrawer:  procedure(FormNo, Loc_X, Loc_Y, ClientWidth, ClientHeight : integer; AllowDock : boolean); cdecl;
  QuitGenDrawer:  procedure(FormNo : integer); cdecl;
 // GetGenHandles:  function : TGenHandles; cdecl;   // * Removed at Ver 1.1.1
  GetGenDrawerInfo: function(FormNo : integer) : TGenDrawerInfo; cdecl; // * Addd at Ver 1.1.1
  GetGenexBitmap: function : HBITMAP; cdecl;
  GetLyricsText: function(Artist, Title : pchar; CheckTitleMatch : boolean;
                          var pLyrics : pchar; var lLyrics : integer) : boolean; cdecl;   // * Addd at Ver 1.1.1
  RunVisDrawer: function(Main_Handle : HWND; InitWinPos : TInitWinPos;                    // * Addd at Ver 1.1.2
                         MagneticProc : TSubClass_Proc; DrawerDLLHandle : HWND; var VisParent : HWND) : HWND; cdecl;
  QuitVisDrawer: procedure; cdecl;                  // * Addd at Ver 1.1.2
  VisDrawerActive: function : boolean; cdecl;       // * Addd at Ver 1.1.2

implementation

var
   DriveThreadId : DWORD = 0;
   ThreadHandle : HWND;
   MainWinHandle : HWND;
   DataReadyMsg : HWND;

   WaitingFlag : boolean;
   GPP_Plugin : string;
   GPPRegNo  : integer;
   StartedGPPs : integer;

   hFakeWindow : HWND = 0;

   MBDrawerDLL_Handle : Thandle = 0;
   VisDrawerDLL_Handle : Thandle = 0;
   OrgWindowProc : Pointer;

procedure WinProcessMessages;
// Allow Windows to process other system messages
var
    ProcMsg  :  TMsg;
begin
    while PeekMessage(ProcMsg, 0, 0, 0, PM_REMOVE) do begin
      if (ProcMsg.Message = WM_QUIT) then Exit;
      TranslateMessage(ProcMsg);
      DispatchMessage(ProcMsg);
    end;
end;

// Check fake window which acts like the one of Winamp's main window
function CheckFakeWindow : HWND;
{var
   hInst : HWND; }
begin
   if hFakeWindow <> 0 then
   begin
      result := hFakeWindow;
      exit;
   end;

   hFakeWindow := GetFakeWinHandle;
   if hFakeWindow <> 0 then
      OrgWindowProc := pointer(GetWindowLong(hFakeWindow, GWL_WNDPROC));

   result := hFakeWindow;
end;


// We should relaese subclassed window procedure when GPP is freed.
procedure ReleaseFakeWindow;
begin
   if hFakeWindow = 0 then
      exit;

   if (OrgWindowProc <> pointer(GetWindowLong(hFakeWindow, GWL_WNDPROC))) then
      SetWindowLong(hFakeWindow, GWL_WNDPROC, LongInt(OrgWindowProc));

   hFakeWindow := 0;
end;

function StartAGPPModule(DLL_Path : string; ParentHandle : HWND) : integer;
var
   DLLRegNo : integer;
   GPPheader : PWinampGPP;
   GPPInfo : TGPPInfo;
   getGPPHeader : function : pointer; stdcall;
begin
   result := -1;

   if hFakeWindow = 0 then
      if CheckFakeWindow = 0 then
         exit;

   DLLRegNo := LoadGPPDLL(DLL_Path);
   if DLLRegNo = -1 then
      exit;

   if GetGPPInfo(DLLRegNo, true, GPPInfo) then
      @getGPPHeader := GPPInfo.GPPHeader
   else
      exit;

   GPPheader := getGPPHeader;
   if GPPheader = nil then
      exit;

   GPPheader^.hwndParent := hFakeWindow;
   GPPheader^.hDLLInstance := GPPInfo.Handle;
   if GPPheader^.init = 0 then
      result := DLLRegNo;
end;


function StartGPPModules(ParentHandle : HWND) : integer;   
var
   s1, s2{, s3} : string;
   SearchRec: TSearchRec;
   StartedGPP : integer;

begin
   result := 0;
   StartedGPP := 0;

   s1 := GetProgDir + 'Plugins\';
   if FindFirst(s1 + '*.dll', faAnyFile, SearchRec) = 0 then
   begin
     s2 := SearchRec.Name;
     s2 := lowerCase(copy(s2, 1, 4));
   //  s3 := copy(s2, 1, 3);
   //  if (s2 <> 'dsp_') and (s2 <> 'vis_') and (s3 <> 'in_') then
     if s2 = 'gen_' then
        if StartAGPPModule(s1 + SearchRec.Name, ParentHandle) <> -1 then
           inc(StartedGPP);

     if NumOfLoadedGPP = MaxGPPNo then
        exit;

     while (FindNext(SearchRec) = 0) do
     begin
        s2 := SearchRec.Name;
        s2 := lowerCase(copy(s2, 1, 4));
      //  s3 := copy(s2, 1, 3);
      //  if (s2 <> 'dsp_') and (s2 <> 'vis_') and (s3 <> 'in_') then
        if s2 = 'gen_' then
           if StartAGPPModule(s1 + SearchRec.Name, ParentHandle) <> -1 then
              inc(StartedGPP);

        if NumOfLoadedGPP = MaxGPPNo then
           break;
     end;

     FindClose(SearchRec);
   end;

   result := StartedGPP;
end;

procedure StopAGPPModule(DLLRegNo : integer);   
var
   GPPheader : PWinampGPP;
   GPPInfo : TGPPInfo;
   getGPPHeader : function : pointer; stdcall;
begin
   if (DLLRegNo < 1) or (DLLRegNo > MaxGPPNo) then
     exit;

   if GetGPPInfo(DLLRegNo, true, GPPInfo) then
      @getGPPHeader := GPPInfo.GPPHeader
   else
      exit;

   GPPheader := getGPPHeader;
   if GPPheader <> nil then
   begin
      GPPheader^.quit;
      UnloadGPPDLL(GPPInfo.FileName);
   end;

   if NumOfLoadedGPP = 0 then
      ReleaseFakeWindow;
end;

procedure StopGPPModules;
var
   i : integer;
begin
   for i := 1 to MaxGPPNo do
     StopAGPPModule(i);
end;

function GPPDriveThread(lpParam : pointer) : DWORD; stdcall;
var
   ExitLoop : boolean;
   MsgReturn : longbool;
   Msg : TMsg;
   Param : ^DWORD;
begin
   ExitLoop := false;

   Param := lpParam;
   if Param^ = 1 then
   begin
      GPPRegNo := StartAGPPModule(GPP_Plugin, MainWinHandle);
      WaitingFlag := false;
   end else
   if Param^ = 2 then
   begin
      StartedGPPs := StartGPPModules(MainWinHandle);
      WaitingFlag := false;
   end;

   repeat
      MsgReturn := GetMessage(Msg, 0, 0, 0);
      if ((Msg.message = WM_QUIT) or (Msg.message = WM_CLOSE)) then
      begin
         ExitLoop := true;
      end else if Msg.message = DataReadyMsg then
      begin
        case Msg.wParam of
           Run_GPP : begin
                       GPPRegNo := StartAGPPModule(GPP_Plugin, MainWinHandle);
                       WaitingFlag := false;
                    end;
           Stop_GPP : begin
                       StopAGPPModule(Msg.lParam);
                    end;
           Run_AllGPPs : begin
                       StartedGPPs := StartGPPModules(MainWinHandle);
                       WaitingFlag := false;
                    end;
           Stop_AllGPPs : begin
                        StopGPPModules;
                    end;
        end;

        Continue;
      end;

      TranslateMessage(Msg);
      DispatchMessage(Msg);
   until (integer(MsgReturn) <= 0) or ExitLoop;

   result := 0;
end;

function Start_GPP(GPP_Path : string; StatusMsg : HWND; ParentHWND : HWND) : integer;
var
   Arg1 : DWORD;
begin
   result := -1;

   if NumOfLoadedGPP = MaxGPPNo then
      exit;

   if trim(GPP_Path) = '' then
      exit;
   if not FileExists(GPP_Path) then
      exit;

   GPP_Plugin := GPP_Path;
   MainWinHandle := ParentHWND;
   DataReadyMsg := StatusMsg;

   if DriveThreadId <> 0 then
   begin
      PostThreadMessage(DriveThreadId, DataReadyMsg, Run_GPP, 0);
      WaitingFlag := true;
      repeat
          WinProcessMessages;
          Sleep(20);
      until WaitingFlag = false;

      result := GPPRegNo;
      exit;
   end;

   Arg1 := 1;
   ThreadHandle := CreateThread(nil, 0, @GPPDriveThread, @Arg1, 0, DriveThreadId);
   if ThreadHandle <> 0 then
      CloseHandle(ThreadHandle)  // if we do not use ThreadHandle elsewhere, we can close it.
   else
      MessageBox(MainWinHandle, 'Error : Unable to create thread !', 'Error!', MB_OK or MB_ICONERROR);

   if ThreadHandle <> 0 then
   begin
      WaitingFlag := true;
      repeat
          WinProcessMessages;
          Sleep(20);
      until WaitingFlag = false;

      result := GPPRegNo;
   end;

end;

function Start_AllGPPs(StatusMsg : HWND; ParentHWND : HWND) : integer;
var
   Arg1 : DWORD;
begin
   result := 0;

   MainWinHandle := ParentHWND;
   DataReadyMsg := StatusMsg;

   if DriveThreadId <> 0 then
   begin
      PostThreadMessage(DriveThreadId, DataReadyMsg, Run_AllGPPs, 0);
      WaitingFlag := true;
      repeat
          WinProcessMessages;
          Sleep(20);
      until WaitingFlag = false;

      result := StartedGPPs;
      exit;
   end;

   Arg1 := 2;
   ThreadHandle := CreateThread(nil, 0, @GPPDriveThread, @Arg1, 0, DriveThreadId);
   if ThreadHandle <> 0 then
      CloseHandle(ThreadHandle)  // if we do not use ThreadHandle elsewhere, we can close it.
   else
      MessageBox(MainWinHandle, 'Error : Unable to create thread !', 'Error!', MB_OK or MB_ICONERROR);

   if ThreadHandle <> 0 then
   begin
      WaitingFlag := true;
      repeat
          WinProcessMessages;
          Sleep(20);
      until WaitingFlag = false;

      result := StartedGPPs;
   end;
end;

procedure Quit_GPP(GPP_RegNo : integer);
begin
   if (DriveThreadId <> 0) then
      PostThreadMessage(DriveThreadId, DataReadyMsg, Stop_GPP, DWORD(GPP_RegNo));
end;

procedure Quit_AllGPPs;
begin
   if (DriveThreadId <> 0) then
      PostThreadMessage(DriveThreadId, DataReadyMsg, Stop_AllGPPs, 0);
end;

function Load_MBDrawerDLL(const dllfilename : string) : boolean;
var
   oldmode : integer;
begin
   if MBDrawerDLL_Handle <> 0 then // is it already there ?
      result := true
   else
   begin {go & load the dll}
     oldmode := SetErrorMode($8001);
     MBDrawerDLL_Handle := LoadLibrary(pchar(dllfilename));  // obtain the handle we want
     SetErrorMode(oldmode);
     if MBDrawerDLL_Handle <> 0 then
     begin {now we tie the functions to the VARs from above}
       @InitMiniBrowser:= GetProcAddress(MBDrawerDLL_Handle, 'InitMiniBrowser');
       @ShowMiniBrowser:= GetProcAddress(MBDrawerDLL_Handle, 'ShowMiniBrowser');
       @MBNavigate:= GetProcAddress(MBDrawerDLL_Handle, 'MBNavigate');
       @MBNavigate2:= GetProcAddress(MBDrawerDLL_Handle, 'MBNavigate2');
       @AddMagneticWindow := GetProcAddress(MBDrawerDLL_Handle, 'AddMagneticWindow');
       @RemoveMagneticWindow := GetProcAddress(MBDrawerDLL_Handle, 'RemoveMagneticWindow');
       @GetSubClass_Proc := GetProcAddress(MBDrawerDLL_Handle, 'GetSubClass_Proc');
       @QuitMiniBrowser:= GetProcAddress(MBDrawerDLL_Handle, 'QuitMiniBrowser');
       @InitGenDrawer := GetProcAddress(MBDrawerDLL_Handle, 'InitGenDrawer');
       @ShowGenDrawer := GetProcAddress(MBDrawerDLL_Handle, 'ShowGenDrawer');
       @QuitGenDrawer := GetProcAddress(MBDrawerDLL_Handle, 'QuitGenDrawer');
     //  @GetGenHandles := GetProcAddress(MBDrawerDLL_Handle, 'GetGenHandles');
       @GetGenDrawerInfo := GetProcAddress(MBDrawerDLL_Handle, 'GetGenDrawerInfo');
       @GetGenexBitmap := GetProcAddress(MBDrawerDLL_Handle, 'GetGenexBitmap');
       @GetLyricsText := GetProcAddress(MBDrawerDLL_Handle, 'GetLyricsText');
      { @RunVisDrawer := GetProcAddress(MBDrawerDLL_Handle, 'RunVisDrawer');
       @QuitVisDrawer := GetProcAddress(MBDrawerDLL_Handle, 'QuitVisDrawer');
       @VisDrawerActive := GetProcAddress(MBDrawerDLL_Handle, 'VisDrawerActive'); }
       if (@InitMiniBrowser = nil) or
          (@ShowMiniBrowser = nil) or
          (@MBNavigate = nil) or
          (@MBNavigate2 = nil) or
          (@AddMagneticWindow = nil) or
          (@RemoveMagneticWindow = nil) or
          (@GetSubClass_Proc = nil) or
          (@QuitMiniBrowser = nil) or
          (@InitGenDrawer = nil) or
          (@ShowGenDrawer = nil) or
          (@QuitGenDrawer = nil) or
        //  (@GetGenHandles = nil) or
          (@GetGenDrawerInfo = nil) or
          (@GetGenexBitmap = nil) or
          (@GetLyricsText = nil) { or
          (@RunVisDrawer = nil) or
          (@QuitVisDrawer = nil) or
          (@VisDrawerActive = nil)} then
       begin
          FreeLibrary(MBDrawerDLL_Handle);
          MBDrawerDLL_Handle := 0;
       end;
     end;

     result := (MBDrawerDLL_Handle <> 0);
   end;
end;

procedure Unload_MBDrawerDLL;
begin
   if MBDrawerDLL_Handle <> 0 then
      FreeLibrary(MBDrawerDLL_Handle);

   MBDrawerDLL_Handle := 0;
end;

function Load_VisDrawerDLL(const dllfilename : string) : boolean;
var
   oldmode : integer;
begin
   if VisDrawerDLL_Handle <> 0 then // is it already there ?
      result := true
   else
   begin // go & load the dll
     oldmode := SetErrorMode($8001);
     VisDrawerDLL_Handle := LoadLibrary(pchar(dllfilename));  // obtain the handle we want
     SetErrorMode(oldmode);
     if VisDrawerDLL_Handle <> 0 then
     begin
       @RunVisDrawer := GetProcAddress(VisDrawerDLL_Handle, 'RunVisDrawer');
       @QuitVisDrawer := GetProcAddress(VisDrawerDLL_Handle, 'QuitVisDrawer');
       @VisDrawerActive := GetProcAddress(VisDrawerDLL_Handle, 'VisDrawerActive');
       if (@RunVisDrawer = nil) or
          (@QuitVisDrawer = nil) or
          (@VisDrawerActive = nil) then
       begin
          FreeLibrary(VisDrawerDLL_Handle);
          VisDrawerDLL_Handle := 0;
       end;
     end;

     result := (VisDrawerDLL_Handle <> 0);
   end;
end;

procedure Unload_VisDrawerDLL;
begin
   if VisDrawerDLL_Handle <> 0 then
      FreeLibrary(VisDrawerDLL_Handle);

   VisDrawerDLL_Handle := 0;
end;

function Get_MBDrawerDLL_Handle : HWND;
begin
   result := MBDrawerDLL_Handle;
end;

function  GetVisDrawerDLL2Handle : HWND;
begin
   result := VisDrawerDLL_Handle;
 //  result := MBDrawerDLL_Handle;
end;

end.
