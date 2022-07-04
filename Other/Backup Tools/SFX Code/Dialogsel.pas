{
  TArchiverSFX (C) Copyright by Oliver Buschjost (autor_oliver@iname.com), 1998

  I didn't mark my modifications, because there were to many. But on request I
  can send you the original Archive with the original ZIP-SFX package.

- This SFX is based on Freewarecode that is copyrighted by:

}
(******************************************************************)
(* Copyright 1997,1998, Merkes' Pages (Markus Stephany)           *)
(* Email: mirbir.st@t-online.de/mirbir.st@saargate.de             *)
(* Web-page: http://home.t-online.de/home/mirbir.st/              *)
(*                                                                *)
(* This program was written in Borland Delphi 3 (and should be    *)
(* fully compatible with Delphi 2 (I hope it !).                  *)
(*                                                                *)
(******************************************************************)
{ this is a part of the modified version of ZipSfx.
  initially, zipsfx has been written by :
(******************************************************************)
(* ZipSFX                                                         *)
(* Copyright 1997, Carl Bunton                                    *)
(* Home of ZipTV compression components for Delphi                *)
(* Email: Twojags@cris.com                                        *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(******************************************************************)

the first modifications came from Eric W. Engler englere@swcp.com

he is the creator of the powerful freeware zip-vcl delzip for delphi 2? and 3.

now i am trying to make the code a bit more sfx-creator- and sfx-user- friendly,
and i must say, eric is a very hard beta-tester :).
}

UNIT Dialogsel;

INTERFACE

USES
    Messages, Windows , sfxmisc , shellapi, ArchiverRoot;

FUNCTION seldialogProc(dlgsel     : hWnd;   // the dialog-proc for directory-selection
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;

FUNCTION newdirProc(dlgnew     : hWnd;     // the dialog-proc for create-subdirectory
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;

procedure getSfxFileInfo; // reads the special header of type TTagInfo, and sets some variables according to this information
procedure executecmd(int:integer); // execute the command-line read from the special header
function getarg(index:integer):string; // splits the command line in application / parameters, if possible

var  seldir       : pchar;    // the selected directory for the select-dir-dialog (in / out)
     storedpath   : pchar;    // the default-directory stored in the special header (out)
     commandline  : pchar;    // the command line read from the special header
     caption      : pchar;    // the definable caption for the main dialog
     copyright    : string;    // the definable text for the copyright line
     TagInfo      : TTagInfo;
const
     {$I StartOfFile.inc}
     usecl        : Boolean = False;  // no cammand line in TTaginfo
     usesp        : Boolean = False;  // no stored path in TTaginfo
     usecap       : Boolean = False;  // no stored caption in TTaginfo
     allowsel     : Boolean = True;   // user can choose files to extract
     allowdcl     : Boolean = True;   // user can disable execution of the command line
     hideovm      : Boolean = False;  // user can change the overwrite-mode
     defovm       : Integer = cm_confirm; // default ovewritemode: confirm

(* The ID's of the Windowselements stored in the .RES File*)
     cm_browse  = 775;        // the item-id for the "browse dir"-button in the main dialog
     cm_runapp  = 1007;       // for the "run xxx after extraction"-checkbox
     cm_lbshow  = 302;        // for the single-selection listbox that the user sees if he is not allowed to select what files to extract
     cm_group   = 509;        // for the "existing file(s)" -groupbox (just to hide it if necessary)
     cm_about   = 103;        // id for about-button in main dialog
     cm_copyright = 601;      // id for copyright-label in main dialog
     ci_listbox = 2003;   // itemID of the directory-listbox in the select-dir dialog
     ci_label   = 2001;   // the directory-display - static text
     ci_net     = 2002;   // to connect a network-drive (not yet tested !!!)
     ci_new     = 2004;   // to show the create-dir dialog
     cn_path    = 3001;   // itemID of the current-path - display in the create-subdir dialog
     cn_edit    = 3002;   // of the enter-new-subdir edit

IMPLEMENTATION

uses sfxgbls;

(* the structure of a this SFX-file :
- zipsfx-executable code (byte 0 - byte n)
- special Header of Type TTagInfo (byte n+1 - byte m)

storedpath:       ++++added  march 01,98 if set to "<TD>", then use temp-dir

commandline:  the command line (to execute after successfull extraction) format :
              if the string "<AD>" (greater than+less than) is somewhere in the command line,
              it will be replaced with the path where the archive has been extracted to.
              (e.g. "<AD>readme\test.txt" after an extraction to the path "C:\Program files\unpacked" means :
              "c:\progra~1\unpacked\readme\test.txt") <- the short path will be created by zipsfx.
              if the pipe "|" is in the command-line, the part to the left will get the application to run
              and the part to the right will be it's argument;
              if the archive is extracted to e.g. "d:\unpack", then we will get the following :
              "<AD>setup\setup.exe|<AD>install.inf" will parse to :
              run "d:\unpack\setup\setup.exe" with parameters "d:\unpack\install.inf".
              "c:\windows\notepad.exe|<AD>readme.txt" will parse to :
              run "c:\windows\notepad.exe" with parameters "d:\unpack\readme.txt".
              "<AD>readme.txt" will parse to :
              open "d:\unpack\readme.txt" with its associated program, if there is any.
              "<AD>setup.exe" will run "d:\unpack\setup.exe" without special parameters.
              ...

*)

var iconheight : integer; // the height of a small shell-icon (to calc the item-height of the directory-listbox)
    shlv : integer;       // handle of the shell's small-image-list

function ImageList_Draw(ImageList: integer; Index: Integer; // to draw a imagelist-image on a hdc
  Dest: HDC; X, Y: Integer; Style: UINT): Bool; stdcall; external 'comctl32.dll' name 'ImageList_Draw';

procedure executecmd; // parses and shell-executes the stored command line after extraction
var sr1,sr2 : string;
begin
          if (int =1) and usecl then begin
             sr1 := getarg(1);
             sr2 := getarg(2);
             shellexecute(0,'open',pchar(sr1),pchar(sr2),'',sw_show);
          end;
end;

(*--------------------------------------------------------------------------*)
function getarg(index:integer):string; // gets an argument from the stored command line
//                1 : the part before the pipe (if there's no pipe, returns the whole command line)
//                2 : the part after the pipe (if no pipe, returns "")
//                all "<AD>" will be replaced by the ectraction path
//                all "<WD>" will be replaced by the windows directory
//                all "<SD>" will be replaced by the system directory
//                all "<TD>" will be replaced by the temp directory
//                all "<PF>" will be replaced by the Programfiles directory
var pip:integer;
begin
     appenddirtail(extpath);
     result := pchartostr(commandline,strlen(commandline));
     pip := pos('|',result);
     if pip = 0 then begin
                          if index = 2 then result := ''
                     end
                else begin
                          if index = 1 then result := copy(result,1,pip-1)
                                       else result := copy(result,pip+1,maxint);
                     end;
     repeat
           pip := pos('<AD>',result);   // '<AD>' is translated to the Aplication directory
           if pip = 0 then break;       // (where the files have been extracted to)
           result := copy(result,1,pip-1)+extpath+copy(result,pip+4,maxint);
     until false;

     repeat
           pip := pos('<WD>',result);   // '<WD>' is translated to the Windows directory
           if pip = 0 then break;
           result := copy(result,1,pip-1)+WindowsDir+'\'+copy(result,pip+4,maxint);
     until false;

     repeat
           pip := pos('<SD>',result);    // '<SD>' is translated to the System directory
           if pip = 0 then break;
           result := copy(result,1,pip-1)+SystemDir+'\'+copy(result,pip+4,maxint);
     until false;

     repeat
           pip := pos('<TD>',result);    // '<TD>' is translated to the Temppath
           if pip = 0 then break;
           result := copy(result,1,pip-1)+TempDir+copy(result,pip+4,maxint);
     until false;

     repeat
           pip := pos('<PF>',result);    // '<PF>' is translated to the ProgramFilesDirectory
           if pip = 0 then break;
           result := copy(result,1,pip-1)+GetProgramFilesPath(True)+copy(result,pip+4,maxint);
     until false;


     // get the short (8+3)-filename (it seems that shellexecute has some problems with lfn)
     getshortpathname(pchar(result),pchar(result),length(result));
end;
(*--------------------------------------------------------------------------*)

(* to check correct file size of the input file *)
procedure TestFileSize ( StoredSize : DWORD );
var asz : DWORD;
begin
     asz := GetFileSize ( InFile , nil );
     if asz <> StoredSize
     then begin
          MessageBox ( 0,STR_EINC_SIZE,STR_E,
                                     mb_iconerror);
          CloseHandle ( InFile );
          Halt;
     end;
end;


procedure getSfxFileInfo; // reads the values from the special header, if any
var bytesread : dword;
    pip:integer;
    tempstoredpath:string;
begin
     fseek(startoffile,file_begin); // let us look for a special-header signature directly after the sfx-code

     ReadFile(InFile, TagInfo, sizeof(TTagInfo), bytesread, NIL);
     startoffile := startoffile+sizeof(TTagInfo); //This is the beginning of the real archive

     usecl:=TagInfo.ExecuteFileAfterExtract;
     allowsel:=TagInfo.UserChooseFilesToExtract;
     hideovm:= not TagInfo.UserChooseOverwriteMode;
     allowdcl:=TagInfo.UserAllowedToDontRunTheFile;
     copyright:=TagInfo.CopyrightLine;       //Set the Copyrigth-String
     SfxMisc.Msg :=SetLanguage(TagInfo.Language);

//geändert und in Dialog.pas geschrieben
{     case TagInfo.DefaultOwerwriteMode of
          Skip:defovm := cm_skip;
f          overwrite:defovm := cm_overwrite;
          confirm:defovm := cm_confirm;
     end;}


     strpcopy(commandline,TagInfo.CommandLine);       //convert commandline-String to Win-Api PChar
     strpcopy(storedpath,TagInfo.DefaultExtractPath); //convert stored path-String to Win-Api PChar
     strpcopy(caption,TagInfo.Caption);               //convert caption-String to Win-Api PChar

     if commandline<>'' then usecap := true;          // is there a stored command line? - activate it!
     if caption<>'' then usecap := true;              // is there a stored caption? - activate it!
     if storedpath<>'' then usesp:= true;             // is there a stored path? - activate it!

//     if storedpath = '<TD>' then gettemppath(max_path,storedpath) else           // '<TD>' is translated to the Temppath
//     if storedpath = '<WD>' then strpcopy(storedpath,WindowsDir) else            // '<WD>' is translated to the Windows directory
//     if storedpath = '<SD>' then strpcopy(storedpath,SystemDir) else             // '<SD>' is translated to the System directory

     tempstoredpath:=pchartostr(storedpath,strlen(storedpath));

     repeat
           pip := pos('<TD>',tempstoredpath);    // '<TD>' is translated to the ProgramFiles directory
           if pip = 0 then break;
           tempstoredpath := copy(tempstoredpath,1,pip-1)+TempDir+copy(tempstoredpath,pip+4,maxint);
     until false;

     repeat
           pip := pos('<WD>',tempstoredpath);    // '<WD>' is translated to the Windows directory
           if pip = 0 then break;
           tempstoredpath := copy(tempstoredpath,1,pip-1)+WindowsDir+'\'+copy(tempstoredpath,pip+4,maxint);
     until false;

     repeat
           pip := pos('<SD>',tempstoredpath);    // '<SD>' is translated to the System directory
           if pip = 0 then break;
           tempstoredpath := copy(tempstoredpath,1,pip-1)+SystemDir+'\'+copy(tempstoredpath,pip+4,maxint);
     until false;

     repeat
           pip := pos('<PF>',tempstoredpath);    // '<PF>' is translated to the ProgramFiles directory
           if pip = 0 then break;
           tempstoredpath := copy(tempstoredpath,1,pip-1)+GetProgramFilesPath(True)+copy(tempstoredpath,pip+4,maxint);
     until false;

//     storedpath:=pchar(tempstoredpath);
     strpcopy(storedpath,tempstoredpath);

     SfxMisc.GetMemoryForTheStrings;
     strpcopy(STR_CANNOTOPEN  ,SFXMisc.Msg.STR_CANNOTOPEN+#0);
     strpcopy(STR_CANNOTCLOSE ,SFXMisc.Msg.STR_CANNOTCLOSE+#0);
     strpcopy(STR_E           ,SFXMisc.Msg.STR_E);
     strpcopy(STR_EXISTS      ,SFXMisc.Msg.STR_EXISTS+#0);
     strpcopy(STR_EARCHIVE    ,SFXMisc.Msg.STR_EARCHIVE+#0);
     strpcopy(STR_INVALIDNAME ,SFXMisc.Msg.STR_INVALIDNAME+#0);
     strpcopy(STR_EDIRECTORY  ,SFXMisc.Msg.STR_EDIRECTORY+#0);
     strpcopy(STR_PREXT       ,SFXMisc.Msg.STR_PREXT+#0);
     strpcopy(STR_ALLEXT      ,SFXMisc.Msg.STR_ALLEXT+#0);
     strpcopy(STR_OK          ,SFXMisc.Msg.STR_OK+#0);
     strpcopy(STR_NOTSELEXT   ,SFXMisc.Msg.STR_NOTSELEXT+#0);
     strpcopy(STR_MP          ,SFXMisc.Msg.STR_MP+#0);
     strpcopy(STR_RUN         ,SFXMisc.Msg.STR_RUN+#0);
     strpcopy(STR_EINC_SIZE   ,SFXMisc.Msg.STR_EINC_SIZE);
     TestFileSize ( TagInfo.SFXFileSize );
end;

procedure addseldir; // add a trailing backslash to the seldir, if there is none
begin
     appenddirtail(seldir);
end;

function forcedirs(path1,path2:pchar):boolean; // check whether all directories can be created
var sr : string;
begin
     result := false;
     sr := pchartostr(path2,strlen(path2));
     if pos(':',sr) > 0 then exit;
     while (sr <> '') and (sr[1] = BSL) do delete(sr,1,1);
     if sr = '' then exit;
     forcedirectories(pchartostr(path1,strlen(path1))+sr);
     if fileexists(pchar(sr+BSL)) then begin
        setcurrentdirectory(pchar(sr));
        result := true;
     end;
end;
(*--------------------------------------------------------------------------*)
(*                               getlongpath                                *)
(*    - returns the complete long file name for a given path/filename -     *)
(*--------------------------------------------------------------------------*)

procedure getlongpath;
var sr,sr1 : string;
    hand : integer;
    data : Twin32finddata;
    i : integer;
begin
     try
     // jedes einzelne teil suchen
     i := strlen(SelDir);
     if i < 4 then exit;
     AddSelDir;
     sr := SelDir;
     sr1 := sr;
     sr[3] := ':';
     SelDir [ 3 ] := #0;
     with data do repeat
           i := pos(BSL,sr);
           if i = 0 then break;
           sr[i]:=':';
           hand :=findfirstfile(pchar(copy(sr1,1,i-1)),data);
           if hand <> Integer(INVALID_HANDLE_VALUE) then begin
              windows.findclose(hand);
              Move ( cFileName , PChar ( Integer ( SelDir ) + StrLen ( SelDir ) )^ , StrLen ( cFileName ) +1 );
              AddSelDir;
           end else break ;
     until false;
     finally
     end;
end;

(*--------------------------------------------------------------------------*)
(*                             getshiconandname                             *)
(*  - get the shell's small icon and displayname for a given file/folder -  *)
(*--------------------------------------------------------------------------*)

function getshiconandname(sr:string;var i:integer):string; // get the shell's small icon and displayname for a given file/folder
var sfi : pshfileinfo;
begin
     getmem(sfi,sizeof(tshfileinfo));
     shlv:=shgetfileinfo(pchar(sr),0,sfi^,sizeof(tshfileinfo),shgfi_sysiconindex or shgfi_icon or shgfi_smallicon
           or shgfi_displayname);
     result := sfi.szdisplayname;
     i := sfi.iicon;
     freemem(sfi);
end;

(*--------------------------------------------------------------------------*)
(*      selDialogProc --- Handle messages for select dir dialog.            *)
(*--------------------------------------------------------------------------*)
FUNCTION selDialogProc(dlgsel     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;

  procedure setit; // fills the dialog with the directory-contents
  var cmd1 , cmd2 : integer;

  const    pp : array [0..4] of char = '[..]'#0;
  begin
     cmd2 := getdlgitem(dlgsel,ci_listbox);
     try
       LockWindowUpdate ( DlgSel ); // don't update the dialog until this action is finished

       SetCurrentDirectory ( SelDir ); // these two lines to expand the given directory to a fully qualified one
       getcurrentdirectory(fsmaxpath,seldir); // now get the path that has been set
       
       getlongpath; // convert it to a long file name (not needed on nt 4)
       DlgDirList(dlgsel,seldir,ci_listbox,ci_label,ddl_drives or ddl_directory or ddl_exclusive); // now fill the dialog
       getcurrentdirectory(fsmaxpath,seldir); // now get the long path name
       // now try to find the '..'-directory to set it to the first (0-) position in the listbox
       cmd1 := sendmessage(cmd2,lb_findstringexact,0,integer(@pp));
       if cmd1 <> lb_err then begin
          sendmessage(cmd2,lb_deletestring,cmd1,0);
          sendmessage(cmd2,lb_insertstring,0,integer(@pp));
       end;
       setfocus(cmd2);
       sendmessage(cmd2,lb_setcursel,0,0);
       finally
              LockWindowUpdate ( 0 ) ;
       end;
  end;

(*--------------------------------------------------------------------------*)
(*                               drawlistbox                                *)
(*         - draws a listbox-item in a win95-alike style (almost) -         *)
(*                  - with an icon and the longfilename -                   *)
(*--------------------------------------------------------------------------*)
  procedure drawlistbox;
  var sr : shortstring;
    kind : integer;
    icon : integer;

  begin
     if dlgwparam = ci_listbox then with pdrawitemstruct(dlglparam)^ do begin
        // get the item's text;
        setlength(sr,sendmessage(hwnditem,lb_gettext,itemid,integer(@sr[1])));

        // get the kind of item
        kind := 0;
        sr:=copy(sr,2,length(sr)-2);
        if length(sr) = 3 then begin
           if sr[1] = '-' then
              if sr [3] = '-' then  begin
                 sr := sr[2]+':\';
                 kind := 1; // for [-x-] - items (these are drives)
              end;
        end else if sr = '..' then kind := 2; // for the "up to parent"-directory ([..])
        if (itemState and ods_focus) <> 0 then begin
           // set the listbox' colors to selected state
           settextcolor(hdc,getsyscolor(color_highlighttext));
           setbkcolor(hdc,getsyscolor(color_highlight));
           fillrect(hdc,rcitem, color_highlight+1);
        end else begin
            // set the listbox-colors to unselected state
            settextcolor(hdc,getsyscolor(color_windowtext));
            setbkcolor(hdc,getsyscolor(color_window));
            fillrect(hdc,rcitem, color_window+1);
        end;

        sr := GetShIconAndName(sr,icon);
        if kind = 2 then sr := '..';


        // now set the indent of the item
        if kind = 0 then
           kind := iconheight div 2 //about 8 pixels to the right for subdirectories
        else kind := 0;
        imagelist_draw(shlv,icon,hDC,rcItem.left+kind+3,rcitem.top+1,1); // draws the icon
        TextOut(hDC,rcItem.left+iconheight+kind+8,   // draw either the shell's displayname or ".."
          rcitem.top+2,@sr[1],length(sr));

     end;
  end;

(*--------------------------------------------------------------------------*)
(*      selDialogProc --- Handle messages for select dir dialog.            *)
(*                         !Here it really starts!                          *)
(*--------------------------------------------------------------------------*)
var cm1:integer;
BEGIN (* selDialogProc *)

   RESULT := TRUE;
   CASE DlgMessage OF
      wm_drawitem   : drawlistbox; // draws a listbox entry
      WM_INITDIALOG : BEGIN
                     // Set the languageString
                     strpcopy(temp,SFXMisc.Msg.OK);
                     cm1 := getdlgitem(DlgSel, 1); // the 'OK' Button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SFXMisc.Msg.CANCEL);
                     cm1 := getdlgitem(DlgSel, 2); // the 'Cancel' Button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGSEL_2002);
                     cm1 := getdlgitem(DlgSel, 2002); // the '&Network...' Button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGSEL_2004);
                     cm1 := getdlgitem(DlgSel, 2004); // the 'New...' Button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGNEW_8001);
                     cm1 := getdlgitem(DlgSel, 2000); // the 'Current directory :' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGSEL_CAPTION);
                     setwindowtext(DlgSel,temp);
                     // Set the languageString

                      addseldir; // adds a '\' to seldir
                      if not fileexists(seldir) then
                         getcurrentdirectory(fsmaxpath,seldir); // to prevent from showing senseless pathes

                     // calculate the listbox' item-height (shell-small-icon-height+2)
                     iconheight := getsystemmetrics(sm_cysmicon);
                     sendmessage(getdlgitem(dlgsel,ci_listbox),lb_setitemheight,0,iconheight+2);
                     (* Center dialog on screen.       *)
                     CenterDialog(dlgsel);
                     // fill the dialog with the directory-contents
                     setit;
                  END;
                 (* Handle button presses, etc. *)

         WM_COMMAND    : CASE LOWORD(DlgWParam) OF
                            ci_listbox : case hiword(dlgwparam) of
                                        lbn_dblclk : begin  // a diff. path has been dblckicked on the listbox
                               DlgDirSelectEx(dlgsel,seldir,fsmaxpath,ci_listbox); // get the chosen path

                               addseldir; // add a '\'
                               setit;     // fill the dialog
                                                     end;
                                        // repaint the listbox if its focus-state has been changed
                                        lbn_killfocus,lbn_setfocus : invalidaterect(getdlgitem(dlgsel,ci_listbox),nil,true);
                            end;
                            // allow to connect a network-drive
                            ci_net    : if wnetconnectiondialog(dlgsel,resourcetype_disk) = no_error then setit;
                            // okay, the current directory has been choosen, end this dialog
                            CM_OK, CM_CANCEL     : BEGIN
                               addseldir;
                               EndDialog(dlgsel, LOWORD(DlgWParam));
                               EXIT;
                            END;
                            // open the create-subdir dialog
                           ci_new: begin
                                if DialogBox( hInstance, STR_NEWDLG , dlgsel, @newdirProc ) = idok then
                                   setit;
                               end;
                         ELSE      ;
                         END (* CASE *);
         ELSE          ;
     END (* CASE *);
     RESULT := FALSE;
END   (* selDialogProc *);
(*--------------------------------------------------------------------------*)


(*--------------------------------------------------------------------------*)
(*       newdirProc --- Handle messages for add subdir dialog.              *)
(*--------------------------------------------------------------------------*)
FUNCTION newdirProc(dlgnew     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;

var buffer : array [0..fsMAXPATH +1 ] of Char;
    cm1    : integer;
BEGIN (* selDialogProc *)

   CASE DlgMessage OF
      WM_INITDIALOG : BEGIN
                     // Set the languageString
                     strpcopy(temp,SfxMisc.Msg.OK);
                     cm1 := getdlgitem(DlgNew, 1); // the 'OK' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.CANCEL);
                     cm1 := getdlgitem(DlgNew, 2); // the 'Cancel' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGNEW_8001);
                     cm1 := getdlgitem(DlgNew, 8001); // the 'Current directory :' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGNEW_8002);
                     cm1 := getdlgitem(DlgNew, 8002); // the 'New subdirectory :' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DLGNEW_CAPTION);
                     setwindowtext(DlgNew,temp);
                     // Set the languageString

                      addseldir;
                     (* Center dialog on screen.       *)
                     CenterDialog(dlgnew);
                     getdlgitemtext( GetParent( dlgnew ) ,ci_label,buffer,max_path);  // get the possibly abbreviated path from the parent sel dialog
                     setdlgitemtext(dlgnew,cn_path,buffer); // and set it in this dialog
                     setfocus(getdlgitem(dlgnew,cn_edit));
                  END;
                 (* Handle button presses, etc. *)

         WM_COMMAND    : CASE LOWORD(DlgWParam) OF
                            CM_OK     : BEGIN

                               // make subdirs;
                               getdlgitemtext(dlgnew,cn_edit,buffer,max_path);
                               if strlen(buffer) <> 0 then
                                  if not forcedirs(seldir,buffer) then
                                     messagebox(dlgnew,STR_EDIRECTORY,STR_E,
                                     mb_iconerror) else begin
                                     if strlen(seldir)+strlen(buffer)+1 < fsmaxpath then
                                        move(buffer[0],seldir[strlen(seldir)],strlen(buffer)+1);

                                     EndDialog(dlgnew, LOWORD(DlgWParam));
                                     //EXIT;

                                 end;
                            END;
                           CM_CANCEL : BEGIN
                               EndDialog(dlgnew, LOWORD(DlgWParam));
                               //EXIT;
                            END;
                         ELSE      ;
                         END (* CASE *);
         ELSE          ;
     END (* CASE *);
     RESULT := FALSE;
END   (* newdirProc *);
(*--------------------------------------------------------------------------*)


initialization
              getmem(seldir,max_path+1);
              getmem(storedpath,500+1);
              getmem(commandline,max_path+1);
              getmem(caption,max_path+1);

finalization
            freemem(seldir);
            freemem(storedpath);
            freemem(commandline);
            freemem(caption);
END.
