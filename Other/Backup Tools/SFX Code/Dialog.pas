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
UNIT Dialog;

INTERFACE

USES
    Messages,sysutils,
    Windows;

FUNCTION MainDialogProc(DlgWin     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;

IMPLEMENTATION

USES  SFXgbls, SFXmisc,dialogsel;//## MST added dialogsel

(*--------------------------------------------------------------------------*)
(*     MainDialogProc --- Handle messages for main window dialog.           *)
(*--------------------------------------------------------------------------*)
FUNCTION MainDialogProc(DlgWin     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; STDCALL;
VAR
   EditLen: LONGINT;
   cm1 : integer;
   tempstr : AnsiString;
   tempoverwrite: AnsiString;
BEGIN (* MainDialogProc *)
   RESULT := TRUE;
   CASE DlgMessage OF
      WM_INITDIALOG : BEGIN
                     // Set the languageString
                     strpcopy(temp,SfxMisc.Msg.MAINDIALOG_100);
                     cm1 := getdlgitem(DlgWin, 100); // the 'extract to:' line
                     MainWinhwndDlg:=DlgWin;
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.MAINDIALOG_8008);
                     cm1 := getdlgitem(DlgWin, 8008); // the 'Files :' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.existingfiles);
                     cm1 := getdlgitem(DlgWin, 509); // the 'Existing file(s) :' line
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.SelectAll);
                     cm1 := getdlgitem(DlgWin, 104); // the 'SelectAll' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.DeSelectAll);
                     cm1 := getdlgitem(DlgWin, 105); // the 'DeSelectAll' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.MAINDIALOG_1);
                     cm1 := getdlgitem(DlgWin, 1); // the 'Start' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.MAINDIALOG_2);
                     cm1 := getdlgitem(DlgWin, 2); // the 'C&lose' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.about_button);
                     cm1 := getdlgitem(DlgWin, CM_ABOUT); // the '&About this SFX...' button
                     sendmessage(cm1,wm_settext,0,integer(temp));

                     strpcopy(temp,SfxMisc.Msg.confirm);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (confirm)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     strpcopy(temp,SfxMisc.Msg.overwrite);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (overwrite)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     strpcopy(temp,SfxMisc.Msg.skip);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (skip)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     strpcopy(temp,SfxMisc.Msg.update);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (update)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     strpcopy(temp,SfxMisc.Msg.existing);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (existing)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     strpcopy(temp,SfxMisc.Msg.updateexisting);
                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox (updateexisting)
                     SendMessage(cm1, CB_ADDSTRING, 0, integer(temp));

                     cm1 := getdlgitem(DlgWin, 106); // the ComboBox
                     case TagInfo.DefaultOwerwriteMode of
                          Skip:begin SendMessage(cm1, CB_SETCURSEL , SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.skip))), 0); OverWriteMode:=1; end;
                          overwrite:begin SendMessage(cm1, CB_SETCURSEL, SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.overwrite))), 0); OverWriteMode:=0; end;
                          confirm:begin SendMessage(cm1, CB_SETCURSEL, SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.confirm))), 0); OverWriteMode:=2; end;
                          update:begin SendMessage(cm1, CB_SETCURSEL, SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.update))), 0); OverWriteMode:=3; end;
                          existing:begin SendMessage(cm1, CB_SETCURSEL, SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.existing))), 0); OverWriteMode:=4; end;
                          updateexisting:begin SendMessage(cm1, CB_SETCURSEL, SendMessage(cm1, CB_FINDSTRINGEXACT, -1, integer(StrPChar(SfxMisc.Msg.updateexisting))), 0); OverWriteMode:=5; end;
                     end;

                     // Set the languageString

                     cm1 := getdlgitem(DlgWin, CM_COPYRIGHT); // the copyright text line
                     sendmessage(cm1,wm_settext,0,integer(pchar(copyright)));
                     cm1 := getdlgitem(DlgWin, CM_runapp); // the run... checkbox
                     //## this is to get either the current dir or the stored def-path
                     if not usesp then GetCurrentDirectory(fsMaxPath, ExtPath) else
                        move(storedpath[0],extpath[0],strlen(storedpath)+1);

                     if not usecl then  //## no cmd-line, so hide the run... checkbox
                        showwindow(cm1, sw_hide) else
                       begin
                        //## give the run... checkbox a title
                        sendmessage(cm1,wm_settext,0,integer(pchar(STR_RUN+' '+
                        extractfilename(getarg(1))+' '+extractfilename(getarg(2)))));

                        //## check it by default
                        sendmessage(cm1,bm_setcheck,1,0);
                        if not allowdcl then //## if not allowed to disable the cmd-line, hide the run... cb
                        showwindow(cm1, sw_hide);
                       end;
                     SendMessage(GetDlgItem(DlgWin, CM_EDIT1), WM_SETTEXT, 0, LONGINT(@ExtPath));
                     (* Hilite string in Edit1 control *)
                     SendMessage(GetDlgItem(DlgWin, CM_EDIT1), EM_SETSEL, 0, $7fff);

                     if hideovm then begin //## if we do not want to select another overwrite-mode, destroy the controls
                        destroywindow(GetDlgItem(DlgWin, 106)); //the combobox
                        destroywindow(GetDlgItem(DlgWin, 509)); //the label
                        destroywindow(GetDlgItem(DlgWin, cm_group));
                     end;
                     if usecap then setwindowtext(dlgwin,caption); //## if we have a stored caption for the dialog, use it
                     CenterDialog(DlgWin); //## now center the dialog
                     (* Fill the list box *)

                     //## added a parameter to the processarchive cause we have two listboxes (only 1 is visible)
                     ProcessArchive(DlgWin,cm_list, TRUE); //## first fill the multisel-listbox
                                                           // the name has changed from cm_filelist to cm_list

                     if allowsel then //## if the user can (de)select files
                        showwindow(GetDlgItem(DlgWin, cm_lbshow),sw_hide) //## hide the singlesel-listbox
                     else begin //## else
                        showwindow(GetDlgItem(DlgWin, cm_list),sw_hide); //## hide the multisel-lb
                        showwindow(GetDlgItem(DlgWin, 104),sw_hide); //## hide the multisel-lb
                        showwindow(GetDlgItem(DlgWin, 105),sw_hide); //## hide the multisel-lb
                        ProcessArchive(DlgWin,cm_lbshow, TRUE); //## and read the archive-contents to the singlesel-lb
                     end;
                     (* Select all items in the listbox *)
                     SendMessage(GetDlgItem(DlgWin, CM_LIST), LB_SETSEL, 1, -1);
                     (* Assign to a global *)
                     MainWin := DlgWin;
                  END;
                 (* Handle button presses, etc. *)

         WM_DRAWITEM:
         begin
              case DlgWParam of
                               106 : DrawOverwriteListItem(PDrawItemStruct(DlgLParam)^);
              end;
         end;

         WM_COMMAND    : CASE LOWORD(DlgWParam) OF

                            //## added the ability to select a extract directory

                            cm_browse:begin
                               GetDlgItemText(DlgWin, CM_EDIT1, ExtPath, fsMaxPath);

                               //## this is in dialogsel.pas
                               move(extpath[0],seldir[0],fsmaxpath);
                               if DialogBox( hInstance, STR_SELDLG, dlgwin, @seldialogProc ) = idok then begin
                                  move(seldir[0],extpath[0],fsmaxpath);
                                  setdlgitemtext(dlgwin,cm_edit1,extpath);
                               end;
                            end;

                            //## removed storedpath-checkbox-handler cause this control has been removed

                            CM_ABOUT  : begin
                                      SetLength ( tempstr , 1000 );
//                                      SetLength ( tempstr , LoadString ( HINSTANCE , 10000 , @tempstr[1] , 499 ) );
//                                      tempstr := tempstr+#0;
                                      tempstr:=FmtLoadStr(10000,[SfxMisc.Msg.author_of_this_sfx,SfxMisc.Msg.copyright,SfxMisc.Msg.further_info])+#0;
                                      MessageBox ( DlgWin , @tempstr[1] , pchar(SfxMisc.Msg.about_caption) , 0 );
                                        end;

                            104:    begin
                                          SendMessage(GetDlgItem(DlgWin, CM_LIST), LB_SETSEL, 1, -1);
                                    end;

                            105:    begin
                                          SendMessage(GetDlgItem(DlgWin, CM_LIST), LB_SETSEL, 0, -1);
                                    end;

                            106:    case HIWORD(DlgWParam) of CBN_SELCHANGE:
                                          begin
                                               SetLength ( tempoverwrite , SendMessage(DlgLParam, CB_GETLBTEXTLEN, SendMessage(DlgLParam, CB_GETCURSEL, 0, 0), 0)+1);
                                               SendMessage(DlgLParam, CB_GETLBTEXT, SendMessage(DlgLParam, CB_GETCURSEL, 0, 0), LongInt(tempoverwrite));
                                                 if tempoverwrite=SfxMisc.Msg.OverWrite+#0 then OverWriteMode:=0 else
                                                 if tempoverwrite=SfxMisc.Msg.skip+#0 then OverWriteMode:=1 else
                                                 if tempoverwrite=SfxMisc.Msg.Confirm+#0 then OverWriteMode:=2 else
                                                 if tempoverwrite=SfxMisc.Msg.Update+#0 then OverWriteMode:=3 else
                                                 if tempoverwrite=SfxMisc.Msg.Existing+#0 then OverWriteMode:=4 else
                                                 if tempoverwrite=SfxMisc.Msg.UpdateExisting+#0 then OverWriteMode:=5;
                                          end;
                                    end;

                            CM_OK     : BEGIN

                               //## if the user is not allowed to (de)selct files from the archive, then
                               //   select them all; hide the single-sel-lb and show the multisel-lb
                               //   to have the ability to show the user what files have not been extracted
                               if not allowsel then begin
                                  SendMessage(GetDlgItem(DlgWin, CM_LIST), LB_SETSEL, 1, -1);
                                  showwindow(GetDlgItem(DlgWin, CM_Lbshow),sw_hide);
                                  showwindow(GetDlgItem(DlgWin, CM_List),sw_show);
                               end;
                               EditLen := GetDlgItemText(DlgWin, CM_EDIT1, ExtPath, fsMaxPath);
                               ExtPath[ EditLen ] := #0;
                               removedirtail(extpath); //## cut a final \
                               IF ExtPath = '' THEN
                               BEGIN
                                  GetCurrentDirectory(fsMaxPath, ExtPath);
                                  SendMessage(GetDlgItem(DlgWin, CM_EDIT1), WM_SETTEXT, 0, LONGINT(@ExtPath));
                               END;
                               ProcessArchive(DlgWin,cm_list, FALSE);

                               //## processresult is set in processarchive, if true (all files have been extracted)
                               //   execute the command line (if any) and close the dialog
                               if processresult then begin
                                  executecmd(SendMessage(GetDlgItem(DlgWin, CM_runapp), BM_getCHECK, 0, 0));
                                  enddialog(dlgwin,idok);
                               end;
                               if Dialogsel.TagInfo.comment in [Both,After] then ShowComment(SplitLine(2,ArchiveComment));
                            END;
                            CM_CANCEL : BEGIN
                               EditLen := GetDlgItemText(DlgWin, CM_EDIT1, ExtPath, fsMaxPath);
                               Extpath[ EditLen ] := #0;
                               IF FileExists(ExtPath) THEN
                                  SetCurrentDirectory(ExtPath);
                               EndDialog(DlgWin, LOWORD(DlgWParam));
                               EXIT;
                            END;
                         ELSE      ;
                         END (* CASE *);
         ELSE          ;
     END (* CASE *);
     RESULT := FALSE;
END   (* MainDialogProc *);
(*--------------------------------------------------------------------------*)
END.
