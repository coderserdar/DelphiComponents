// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  17595: uFormBrowse.pas
//
//    Rev 1.16    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.15    11-08-2009 10:34:10  mcm
// Modified registry keys.
//
//    Rev 1.14    05-11-2006 18:56:32  mcm    Version: IMG 3.1
// Added support for showing all images in a multi-paged file.
//
//    Rev 1.13    17/11/2005 18:36:36  mcm    Version: IMG 2.10
// Enabled Directory monitor / TmcmThumbView.EnableDirMonitor. 
//
//   Rev 1.12    23-08-2005 22:22:42  mcm    Version: IMG 2.9
// Added example use of TmcmThumb>View.OnBeforeLoadThumb event.

//
//   Rev 1.11    04-08-2005 18:53:28  mcm    Version: IMG 2.9
// Added drag & drop support.
// Added multi-file selection.
// Added OnBeforeLoadThunb event.

//
//   Rev 1.10    30/07/2005 12:22:34  mcm
// Initial Drag & Drop implementation.

//
//   Rev 1.9    30-01-2004 20:44:44  mcm    Version: IMG 2.3
// Modified to use the new border style and thumb colors.

//
//   Rev 1.8    08-06-2003 12:35:08  mcm    Version: IMG 1.3.4
// Added pop-up menus for sorting in Ascending or Descending direction and check
// mark the selected sort method. Note sorting is not available in Delphi 3 & 4.

//
//   Rev 1.7    26-03-2003 09:17:52  mcm    Version: IMG 1.3.3
// Re-introduced the OnKeyUp to handle key (F5 & Esc) press for TmcmThumbView
// when this is not focused.

//
//   Rev 1.6    11-03-2003 00:01:04  mcm    Version: IMG 1.3.2

//
//   Rev 1.5    10-03-2003 13:41:32  mcm    Version: IMG 1.3.2
// Esc and F5 key is now handled by TmcmThumbView.

//
//   Rev 1.4    05-02-03 16:34:34  mcm
// Added the OnClick event for demo.

//
//   Rev 1.3    29-01-2003 15:51:20  mcm
// Beauty fix.

//
//   Rev 1.2    27-01-2003 14:07:12  mcm
// Minor modification for better selection and update of thumbs directory.

//
//   Rev 1.1    07-08-2002 13:55:26  mcm    Version: IMG 1.1
// Added time measurement.

//
//   Rev 1.0    27-05-2002 16:22:30  mcm

unit uFormBrowse;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      ComCtrls, Menus, ExtCtrls, StdCtrls, FileCtrl,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
      Vcl.ExtCtrls, Vcl.FileCtrl, 
     {$ENDIF}
     mcmThumbnail,
     mcmImageTypeDef;

type
  TFormBrowse = class(TForm)
    PanelPath        : TPanel;
    DirectoryListBox : TDirectoryListBox;
    PanelDir         : TPanel;
    DriveComboBox    : TDriveComboBox;
    SplitterWin      : TSplitter;
    mcmThumbView     : TmcmThumbView;
    StatusBar        : TStatusBar;
    PopupMenu        : TPopupMenu;
    DeleteItem       : TMenuItem;
    ToRecyclebinItem : TMenuItem;
    RenameItem       : TMenuItem;
    N1               : TMenuItem;
    SortMenu         : TMenuItem;
    ByNameItem       : TMenuItem;
    ByExtensionItem  : TMenuItem;
    ByDateTimeItem   : TMenuItem;
    BySizeItem       : TMenuItem;
    N2               : TMenuItem;
    AscendingItem    : TMenuItem;
    DescendingItem   : TMenuItem;
    ShowMultiPagesItem: TMenuItem;
    procedure FormCreate              (    Sender   : TObject);
    procedure FormDestroy             (    Sender   : TObject);
    procedure FormCloseQuery          (    Sender   : TObject;
                                       var CanClose : Boolean);
    procedure DirectoryListBoxChange  (    Sender   : TObject);
    procedure FormClose               (    Sender   : TObject;
                                       var Action   : TCloseAction);
    procedure FormKeyUp               (    Sender   : TObject;
                                       var Key      : Word;
                                           Shift    : TShiftState);
    procedure mcmThumbViewScanProgress(    Sender   : TObject;
                                           Position : word;
                                           Total    : word;
                                       var Break    : Boolean);
    procedure DeleteItemClick         (    Sender   : TObject);
    procedure RenameItemClick         (    Sender   : TObject);
    procedure mcmThumbViewClick       (    Sender   : TObject);
    procedure ToRecyclebinItemClick   (    Sender   : TObject);
    procedure OnSortClick             (    Sender   : TObject);
    procedure PopupMenuPopup          (    Sender   : TObject);
    procedure SortDirItemClick        (    Sender   : TObject);
    procedure ThumbViewBeforeLoadThumb(    Sender   : TObject;
                                           FileName : String;
                                       var Skip,
                                           Cancel   : Boolean);
    procedure ShowMultiPagesItemClick(Sender: TObject);
  private
    { Private declarations }
    FLoadCount     : integer;
    FStartTime     : {$IFDEF DCB3}TLargeInteger{$ELSE}int64{$ENDIF};
    FEndTime       : {$IFDEF DCB3}TLargeInteger{$ELSE}int64{$ENDIF};
    FOnProcessTime : TOnProcessTime;
  public
    { Public declarations }
    property OnProcessTime : TOnProcessTime
      read   FOnProcessTime
      write  FOnProcessTime;
  end;

var FormBrowse : TFormBrowse;

implementation

{$R *.DFM}

uses {$IFNDEF GE_DXE2}
      Registry;
     {$ELSE}
      System.UITypes, System.Win.Registry;
     {$ENDIF}
     
const RegKey = '\SOFTWARE\MCM DESIGN\mcmImaging\';

procedure TFormBrowse.FormCreate(Sender : TObject);
var Reg : TRegIniFile;
begin
  FOnProcessTime := Nil;
  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE;
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         DirectoryListBox.Directory := Reg.ReadString('', 'BrowsePath', DirectoryListBox.Directory);
         QueryPerformanceCounter(FStartTime);
         mcmThumbView.Dir := DirectoryListBox.Directory;
         FLoadCount := mcmThumbView.Count;
       except
         on E:Exception
         do mcmThumbView.Dir := DirectoryListBox.Directory;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
end; // TFormBrowse.FormCreate.


procedure TFormBrowse.FormDestroy(Sender : TObject);
begin
  FormBrowse := Nil;
end; // TFormBrowse.FormDestroy.


procedure TFormBrowse.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  mcmThumbView.Cancel := True;
  CanClose := True;
end; // TFormBrowse.FormCloseQuery.


procedure TFormBrowse.DirectoryListBoxChange(Sender : TObject);
var Reg : TRegIniFile;
begin
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Reg.WriteString('', 'BrowsePath', DirectoryListBox.Directory);
       except
         on E:Exception
         do mcmThumbView.Dir := DirectoryListBox.Directory;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;

  Caption := 'Browse, ' + DirectoryListBox.Directory;
  QueryPerformanceCounter(FStartTime);
  if (mcmThumbView.Dir <> DirectoryListBox.Directory)
  then mcmThumbView.Dir := DirectoryListBox.Directory
  else mcmThumbView.UpdateThumbsInDir;
  FLoadCount := mcmThumbView.Count;
  if (FLoadCount > 0)
  then StatusBar.Panels[0].Text := 'Loading 0 of ' + IntToStr(FLoadCount)
  else StatusBar.Panels[0].Text := '';
end; // TFormBrowse.DirectoryListBoxChange.


procedure TFormBrowse.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  Action := caFree;
end; // TFormBrowse.FormClose.


procedure TFormBrowse.mcmThumbViewScanProgress(    Sender   : TObject;
                                                   Position : word;
                                                   Total    : word;
                                               var Break    : Boolean);
begin
  if (Position <= FLoadCount)
  then StatusBar.Panels[0].Text := 'Loading ' + IntToStr(Position) +' of ' + IntToStr(Total)
  else begin
       QueryPerformanceCounter(FEndTime);
       if Assigned(FOnProcessTime)
       then FOnProcessTime(Self, FStartTime, FEndTime);
       StatusBar.Panels[0].Text := 'Number of image ' + IntToStr(mcmThumbView.Count);
       FLoadCount := Total;
  end;
end; // TFormBrowse.mcmThumbViewProgress.


procedure TFormBrowse.FormKeyUp(    Sender : TObject;
                                var Key    : Word;
                                    Shift  : TShiftState);
begin
  // Handles keyboard action when TmcmThumbView is not focused.
  case Key of
  VK_ESCAPE : begin
                mcmThumbView.Cancel := True;
                StatusBar.Panels[0].Text := '';
              end;
  VK_F5     : begin
                DirectoryListBoxChange(Self);
              end;
  end;
end; // TFormBrowse.FormKeyUp.


procedure TFormBrowse.PopupMenuPopup(Sender : TObject);
{$IFNDEF DCB3_4}
var i : integer;
{$ENDIF}
begin
  DeleteItem.Enabled       := (mcmThumbView.SelectedCount > 0);
  ToRecyclebinItem.Enabled := DeleteItem.Enabled;
  RenameItem.Enabled       := (mcmThumbView.SelectedCount = 1);
  ShowMultiPagesItem.Checked := mcmThumbView.ShowAllPages;

  {$IFDEF DCB3_4}
    SortMenu.Enabled := False;
  {$ELSE}
     for i := 0 to (SortMenu.Count - 1)
     do SortMenu.Items[i].Checked := False;

     case mcmThumbView.SortMethod of
     TSM_FileName      : ByNameItem.Checked := True;
     TSM_FileExt       : ByExtensionItem.Checked := True;
     TSM_FileTimeStamp : ByDateTimeItem.Checked := True;
     TSM_FileSize      : BySizeItem.Checked := True;
     else ;
     end;
     if mcmThumbView.SortDescending
     then DescendingItem.Checked := True
     else AscendingItem.Checked := True;
  {$ENDIF}
end; // TFormBrowse.PopupMenuPopup.


procedure TFormBrowse.DeleteItemClick(Sender : TObject);
begin
  if (MessageDlg('Permanently delete selected file(s)', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then mcmThumbView.DeleteSelected;
  mcmThumbViewClick(Sender);
end; // TFormBrowse.DeleteItemClick.


procedure TFormBrowse.ToRecyclebinItemClick(Sender : TObject);
begin
  mcmThumbView.RecycleSelected(True, False);
  mcmThumbViewClick(Sender);
end; // TFormBrowse.ToRecyclebinItemClick.


procedure TFormBrowse.RenameItemClick(Sender : TObject);
var NewName : string;
begin
  NewName := mcmThumbView.SelectedFilename;
  if InputQuery('Rename file', 'New file name:', NewName)
  then mcmThumbView.RenameSelected(NewName);
end; // TFormBrowse.RenameItemClick.


procedure TFormBrowse.mcmThumbViewClick(Sender : TObject);
var index : longint;
begin
  Index := mcmThumbView.Selected + 1;
  FLoadCount := mcmThumbView.Count;
  StatusBar.Panels[0].Text := 'Selected image ' + IntToStr(Index) + ' of ' + IntToStr(FLoadCount);
end; // TFormBrowse.mcmThumbViewClick.


procedure TFormBrowse.OnSortClick(Sender : TObject);
begin
  if (Sender is TMenuItem)
  then mcmThumbView.SortMethod := TThumbSortMethod(TMenuItem(Sender).Tag);
end; // TFormBrowse.OnSortClick.


procedure TFormBrowse.SortDirItemClick(Sender : TObject);
begin
 {$IFNDEF DCB3_4}
  if (Sender is TMenuItem)
  then mcmThumbView.SortDescending := (TMenuItem(Sender).Tag = 1);
 {$ENDIF}
end;


procedure TFormBrowse.ShowMultiPagesItemClick(Sender : TObject);
begin
  ShowMultiPagesItem.Checked := Not(ShowMultiPagesItem.Checked);
  mcmThumbView.ShowAllPages := ShowMultiPagesItem.Checked;
end; // TFormBrowse.ShowMultiPagesItemClick.


procedure TFormBrowse.ThumbViewBeforeLoadThumb(    Sender       : TObject;
                                                   FileName     : string;
                                               var Skip, Cancel : Boolean);
begin
  // Use this event to either stop loading thumbs by setting Cancal = True, or
  // Skip loading FileName and go to the next image.

  // For example, don't load PNG image
  {
  if (Pos('.png', Filename) <> 0)
  then Skip := True;
  }
end; // TFormBrowse.mcmThumbViewBeforeLoadThumb.

Initialization
  FormBrowse := Nil;

end.
