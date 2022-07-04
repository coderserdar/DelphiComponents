//  unit Util_LeoLyrics
//
// This unilt is written just to support the Gen_LeosLyrics.dll, a kind of Winamp
//  General Purpose Plug-in for lyrics display.
// I couldn't resolve the interface problem between TBASSPlayer and Gen_LeosLyrics.dll.
//  ( Gen_LeosLyrics.dll does not search/display lyrics via Winamp IPC messages with demo
//    program written by me. )
// There should be somethings, I do not know, to drive Gen_LeosLyrics.dll.
// So, I have made this unit to control Gen_LeosLyrics.dll at application level.
// The application program can force on Gen_LeosLyrics.dll to search/display lyrics using
//  the function/procedure of this unit.
// See the source program of demo program for the usage of this unit.
// Please let me know if you have any idea regarding driving Gen_LeosLyrics.dll.
// I hope this unit will be unnecessary soon, by someone's help for me to resolve the
//  interface problem.
//
//
//          written by Silhwan Hyun
//
// Ver 1.0                         Oct 7 2008
//   - Initial release
//


unit Util_LeoLyrics;

interface

uses
  Windows, Messages, SysUtils;


type

  TLeosHandleList = record
    hGenWindow  : HWND;
    hTabControl : HWND;
    hSheet      : HWND;
    hArtist     : HWND;
    hTitle      : HWND;
    hSearch     : HWND;
    hDisplay    : HWND;
  end;

 function GetLeosWinHandle(var LeosHandleList : TLeosHandleList) : integer;
 procedure SendTextToLeos(LeosHandleList : TLeosHandleList; Artist, Title : string);

implementation

const

// TCM_* - Tab Control Message constants
   TCM_FIRST = $1300;
   TCM_GETCURSEL = TCM_FIRST + 11;
   TCM_SETCURSEL = TCM_FIRST + 12;
   TCM_GETCURFOCUS = TCM_FIRST + 47;
   TCM_SETCURFOCUS = TCM_FIRST + 48;

var
   h_Edit1, h_Edit2 : HWND;
   IsFirstEdit : boolean;


function GetTabHandle(PHandle : HWND; var SheetHandle : HWND) : HWND;
var
   hChild, hChild2, hTabControl : HWND;
   pText : array[0..255] of char;

begin
 // The structure of Winamp GPP window activated by LeosLyrics plugin is as follows,
 //  * TGenDrawForm  "LeosLyrics.com"          ( Top level window )
 //     * #32770                               ( 1st level child window )
 //       * static                             ( a label component )
 //       * #32770   "LeosLyrics.com Plugin"   ( 2nd level child window )
 //         * #32770 "Search"                  ( 3rd level child window = TabSheet )
 //         |  * Edit                          ( Component on TabSheet )
 //         |  * Edit                                    "
 //         |  * Button                                  "
 //         |     .                                      '
 //         |     .                                      .
 //         |     .
 //         * SysTabControl32        ( TabControl component <= The window we want to find )

   result := 0;

   hChild := GetWindow(PHandle, GW_CHILD);   // Get 1st level child window
   if hChild = 0 then
      exit;

   GetClassName(hChild, pText, sizeof(pText));
   hChild2 := FindWindowEx(hChild,       // handle to parent window
                           0,            // handle to a child window
                           pText,        // pointer to class name (= same as parent)
                           nil);         // pointer to window name

   if hChild2 = 0 then                   // hChild2 : 2nd level child window
      exit;

   hTabControl := FindWindowEx(hChild2, 0, 'SysTabControl32', nil);
   if hTabControl <> 0 then
      result := hTabControl
   else
      exit;

 // SheetHandle : handle to 3rd level child window
   SheetHandle := FindWindowEx(hChild2, 0, pText, nil);  // class name = same as parent

end;


function EnumChildProc(h_Wnd : HWND; var Param : integer) : boolean; stdcall;
var
   n : integer;
   pText : array[0..255] of char;
begin
   result := true;

   n := GetClassName(h_Wnd, pText, sizeof(pText));
   if n > 0 then
      if string(pText) = 'Edit' then   // class name = 'Edit' ?
         if IsFirstEdit then
         begin
            h_Edit1 := h_Wnd;
            IsFirstEdit := false;
         end else begin
            h_Edit2 := h_Wnd;
            result := false;
         end;
end;

procedure Get_WinHandles(hSheet : HWND; var hEdit1, hEdit2, hButton1, hButton2 : HWND);
var
   Param : DWORD;
   r1, r2 : TRect;
begin
   hEdit1 := 0;
   hEdit2 := 0;

   hButton1 := FindWindowEx(hSheet, 0, 'Button', 'Search');    // handle to "Search" button
   hButton2 := FindWindowEx(hSheet, 0, 'Button', 'Display');   // handle to "Display" button

   IsFirstEdit := true;
   Param := 0;
   EnumChildWindows(hSheet, @EnumChildProc, Param);

   if (h_Edit1 <> 0) and (h_Edit2 <> 0) then
   begin
      GetWindowRect(h_Edit1, r1);
      GetWindowRect(h_Edit2, r2);
      if r1.Top < r2.Top then     // "Artist" Editbox is above "Title" Editbox
      begin
         hEdit1 := h_Edit1;       // handle to "Artist" Editbox
         hEdit2 := h_Edit2;       // handle to "Title" Editbox
      end else
      begin
         hEdit1 := h_Edit2;
         hEdit2 := h_Edit1;
      end
   end;
end;

procedure SetToSearchPage(hTabControl, hSheet : HWND);
var
   n : integer;

begin
   n := SendMessage(hTabControl, TCM_GETCURSEL, 0, 0);
   if n = 0 then
      exit;

   SendMessage(hTabControl, TCM_SETCURFOCUS, 0{=TabNo}, 0);  // TabNo = 0 : "Search" page
   SendMessage(hTabControl, TCM_SETCURSEL, 0{=TabNo}, 0);

end;

// The application calls this procedure to find out the handles to components of the
//  window activated by LeosLyrics plugin.
function GetLeosWinHandle(var LeosHandleList : TLeosHandleList) : integer;
var
   hTabControl, hSheet : HWND;
   hEdit1, hEdit2 : HWND;
   hButton1, hButton2 : HWND;
 //  IsSearchPage : boolean;
 //  pText : array[0..255] of char;
   n : integer;

begin
   result := -1;    // Any error

   hTabControl := GetTabHandle(LeosHandleList.hGenWindow, hSheet);
   if hTabControl = 0 then
      exit
   else begin
      LeosHandleList.hTabControl := hTabControl;
      LeosHandleList.hSheet := hSheet;
   end;

  // IsSearchPage := false;

   if hSheet <> 0 then     // Got the handle to the TabSheet ?
   begin
    {  n := GetWindowText(hSheet, pText, sizeof(pText));
      if n <> 0 then
         if string(pText) = 'Search' then
            IsSearchPage := true;  }

   // Get current selected page ( n => 0 : Search,   1 : Lyrics,   2 : About )
      n := SendMessage(hTabControl, TCM_GETCURSEL, 0, 0);

      if  n <> 0 { not  IsSearchPage } then   // current page is not "Search" page ?
      begin
         SetToSearchPage(hTabControl, hSheet);
         result := 1;  // error : current page is not "Search" page
         exit;         // The components on "Search" page may not be created at this time.
                       // So, exit then re-call this function later.
      end;

      Get_WinHandles(hSheet, hEdit1, hEdit2, hButton1, hButton2);
      LeosHandleList.hArtist  := hEdit1;
      LeosHandleList.hTitle   := hEdit2;
      LeosHandleList.hSearch  := hButton1;
      LeosHandleList.hDisplay := hButton2;
      result := 0;     // no error
   end;
end;

// The application calls this procedure to request to search lyrics for new Artist & Title.
procedure SendTextToLeos(LeosHandleList : TLeosHandleList; Artist, Title : string);
var
 //  n : integer;
   pText : array[0..255] of char;
 //  WndHandle : HWND;
begin
 // It is not necessary to change the current page to "Search" page at posting Artist & Title.
 {  n := SendMessage(LeosHandleList.hTabControl, TCM_GETCURFOCUS, 0, 0);
   if n <> 0 then
   begin
      SetToSearchPage(LeosHandleList.hTabControl, LeosHandleList.hSheet);
      exit;
   end; }

 // Set the text of Editbox with new Artist & Title.
   StrPLCopy(pText, Artist, 255);
   SendMessage(LeosHandleList.hArtist, WM_SETTEXT, 0, integer(@pText[0]));
   StrPLCopy(pText, Title, 255);
   SendMessage(LeosHandleList.hTitle, WM_SETTEXT, 0, integer(@pText[0]));

 //  WndHandle := GetForegroundWindow;
 // Emulate clicking "Search" button
   SendMessage(LeosHandleList.hSearch, WM_LBUTTONDOWN, MK_LBUTTON, 0);
   SendMessage(LeosHandleList.hSearch, WM_LBUTTONUP, MK_LBUTTON, 0);
 //  SetForegroundWindow(WndHandle);
end;


end.
