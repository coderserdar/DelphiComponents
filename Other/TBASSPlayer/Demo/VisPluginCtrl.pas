// unit VisPluginCtrl
//
// This unit takes charge of managing Winamp vis plug-ins for Demo program.
// You can select, start or stop Winamp vis plug-ins using this unit.
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.24                         1 Oct 2008
//   Added procedure ApplyLocalLanguage to support localizaton of program
//
// Ver 1.23                        19 Jul 2008
//   Changed the DLL's hwndParent to MainForm's window handle at loading vis plug-in.
//    ( to merge window procedure for vis plug-in with MainForm's window procedure )
//   Changed to use different colors for displaying plug-in's information according to
//    the status of the vis plug-in (in use or not in use).
//   Changed procedure btnConfigureClick to prohibit any user actions on vis plug-in while
//    a vis plug-in's Configuration windows is shown, to prevent problems by conflicts between
//    vis plug-ins.
//
// Ver 1.22                        8 Dec 2006
//   Added a check box which enables you to select Winamp-like Vis drawer.
//   Deleted 2 check boxes because of obsoleted properties of TBASSPlayer.
//     - The check box which enables you to select external vis plug-in driver("VisOut.exe").
//     - The check box which enables you to select synchronized action between windows.
//
// Ver 1.21                       24 Oct 2006
//   Some trivial changes such as caption, comments
//
// Ver 1.2                         29 Jan 2005
//  Modified some functions to adapt to updated TBASSPlayer.
//
// Ver 1.1                         12 May 2003
//  Added a checkbox to enable or to disable synchronized action between the
//   windows of main program and the driver of visualization plug-in.
//  Added a trackbar to adjust the output level of FFT
//
// Ver 1.0                         5 Apr 2003
//   - Initial release

unit VisPluginCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ComCtrls, ExtCtrls, PluginCtrl;

type
  TVisControlForm = class(TForm)
    PluginList: TListBox;
    btnConfigure: TButton;
    lbl_Description: TLabel;
    btnStart: TButton;
    Descript: TEdit;
    lbl_Modules: TLabel;
    btnStop: TButton;
    VisModules: TStringGrid;
    lbl_Note_2: TLabel;
    VisScaleBar: TTrackBar;
    lbl_VisScale: TLabel;
    btnClose: TButton;
    lbl_Note_1: TLabel;
    lbl_Detected: TLabel;
    cbUseWinampLook: TCheckBox;
    lbl_UseWinampLook: TLabel;

    procedure FormShow(Sender: TObject);
    procedure PluginListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbSyncVisClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure VisScaleBarChange(Sender: TObject);
    procedure cbUseWinampLookClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PluginListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure VisModulesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
    procedure GetPluginInfo;
  public
    { Public declarations }
    procedure ApplyLocalLanguage;        // * New at Ver 1.24
    procedure SetVisWindowAttr(VisWindowAttr : TVisPluginInfo); // * New at Ver 1.23
  end;

var
  VisControlForm: TVisControlForm;

implementation

{$R *.DFM}

uses
   ioPlug, BassTest;

const
   msgCount = 4;

var
   Vismod : TVismod;
   CurIndexNum,
   NumVismod : integer;
   CurVisWindowAttr : TVisPluginInfo;
 //  WaitingPrevVisStop : boolean = false;

   IsShownConfigWindow : boolean;

   msg : array[1..msgCount] of string
               = ('Close plug-in''s Configuration window first.',
                  'Unknown error in plug-in.',
                  'Mainform''s Visualization panel is invisible.',
                  'Visualization starts in a descrete window.');

procedure TVisControlForm.ApplyLocalLanguage;  // * New at Ver 1.24
var
  F: TextFile;
  SearchRec: TSearchRec;
  S: string;
  FormName : string;
  MyFormEntry : boolean;
  CommonEntry : boolean;

  i, j, SepPos : integer;
  ItemType : string[1];
  ItemName, ItemVal : string;
  Temp: TComponent;

  Lang_Str : array of string;
  Entries_Lang_Str : integer;
  Items_Lang_Str : integer;
 // tmpStr : string;
  S_org, S_local : string;
begin
  if FindFirst(ExtractFilePath(ParamStr(0)) + 'lang_*.txt', faAnyFile, SearchRec) <> 0 then
  begin
     FindClose(SearchRec);
     exit;
  end;
  
  MyFormEntry := false;
  CommonEntry := false;
  Items_Lang_Str := 0;
  SetLength(Lang_Str, 16);
  Entries_Lang_Str := 16;

  AssignFile(F, ExtractFilePath(ParamStr(0)) + SearchRec.Name);
  Reset(F);
  FindClose(SearchRec);

  while not Eof(F) do
  begin
     Readln(F, S);
     S := trim(S);

     if S = '' then
        continue;
     if copy(S, 1, 2) = '//' then
        continue;

     if (S[1] = '[') and (S[length(S)] = ']') then
     begin
        FormName := copy(S, 2, length(S) - 2);
        if FormName = Self.Name then
        begin
           MyFormEntry := true;
           CommonEntry := false;
           continue;
        end else if uppercase(FormName) = 'COMMON' then
        begin
           MyFormEntry := false;
           CommonEntry := true;
           continue;
        end else if MyFormEntry then
           break
        else begin
           MyFormEntry := false;
           CommonEntry := false;
           continue;
        end   
     end
     else if (not MyFormEntry) and (not CommonEntry) then
        continue;

     SepPos := pos('=', S);
     if SepPos = 0 then
        Continue;

     ItemVal := trim(copy(S, SepPos + 1, length(S) - SepPos));
     if ItemVal = '' then
        continue;
     if ItemVal[1] = '"' then
        ItemVal := copy(ItemVal, 2, length(ItemVal) - 1);
     if ItemVal[length(ItemVal)] = '"' then
        ItemVal := copy(ItemVal, 1, length(ItemVal) - 1);
     if ItemVal = '' then
        continue;

     if ((S[1] = '&') or (S[1] = '$') or (S[1] = '*')) then
     begin
        ItemType := S[1];
        ItemName := trim(copy(S, 2, SepPos - 2));

        if (ItemType = '&') and (ItemName = Self.Name) then  // Caption setting ?
        begin
           Self.Caption := ItemVal;
           continue;
        end;

        for i := ComponentCount - 1 downto 0 do
        begin
           Temp := Components[i];
           if not (Temp is TControl) then
              continue;
           if Temp.Name = ItemName then
           begin
              if ItemType = '&' then
              begin
                 if (Temp is TLabel) then
                    (Temp as TLabel).Caption := ItemVal
                 else if (Temp is TButton) then
                    (Temp as TButton).Caption := ItemVal
                 else if (Temp is TCheckBox) then
                    (Temp as TCheckBox).Caption := ItemVal
              end else if ItemType = '$' then
              begin
                 if (Temp is TEdit) then
                    (Temp as TEdit).Text := ItemVal;
              end else if ItemType = '*' then
                 (Temp as TControl).Hint := ItemVal;
           end;
        end;
     end else
     begin
     // Store message strings (format : Original=Local)
        Lang_Str[Items_Lang_Str] := trim(copy(S, 1, SepPos - 1)) + '=' + ItemVal;
        inc(Items_Lang_Str);
        if Items_Lang_Str = Entries_Lang_Str then
        begin
           Entries_Lang_Str := Entries_Lang_Str + 16;
           SetLength(Lang_Str, Entries_Lang_Str);
        end;
        continue;
     end;

  end;

  CloseFile(F);
  SetLength(Lang_Str, Items_Lang_Str);

  // Substitute message string with local text.
  for i := 1 to Items_Lang_Str do
  begin
     SepPos := pos('=', Lang_Str[i - 1]);
     S_org := copy(Lang_Str[i - 1], 1, SepPos - 1);
     S_local := copy(Lang_Str[i - 1], SepPos + 1, length(Lang_Str[i - 1]) - SepPos);
     for j := 1 to msgCount do
        if msg[j] = S_org then
        begin
           msg[j] := S_local;
           break;
        end;
  end;

  SetLength(Lang_Str, 0);
end;

procedure TVisControlForm.SetVisWindowAttr(VisWindowAttr : TVisPluginInfo);
begin
   CurVisWindowAttr := VisWindowAttr;

   PluginList.Repaint;
   VisModules.Repaint;

  // See comment in the procedure btnStartClick
  { if CurVisWindowAttr.VisHandle <> 0 then
      WaitingPrevVisStop := false
   else
      if WaitingPrevVisStop then
         btnStartClick(Self); }
end;

procedure TVisControlForm.GetPluginInfo;
var
   Vis_Plugin : string;
   Visheader : PWinampVisHeader;
   i : integer;
begin
   CurIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[CurIndexNum];
   LoadVisModule(Vis_Plugin, Visheader, Vismod, NumVismod, MainForm.Handle);

   if NumVismod = 0 then
      exit;

   Descript.Text := string(Visheader^.description);

   VisModules.RowCount := NumVismod;
   for i := 0 to (NumVismod - 1) do
      VisModules.Cells[0, i] := Vismod[i].description;

end;

procedure TVisControlForm.FormShow(Sender: TObject);
var
   SearchRec : TSearchRec;
begin
   CurIndexNum := -1;
   NumVismod := 0;
   PluginList.Clear;

   if FindFirst(GetProgDir + 'Plugins\vis_*.dll', 0, SearchRec) = 0 then
   begin
      PluginList.Items.Add(SearchRec.Name);
      while FindNext(SearchRec) = 0 do
         PluginList.Items.Add(SearchRec.Name);
   end;
   FindClose(SearchRec);

   if PluginList.Items.Count > 0 then
   begin
      PluginList.ItemIndex := 0;
      GetPluginInfo;
   end else
   begin
      Descript.Text := '';
      VisModules.RowCount := 0;
      PluginList.ItemIndex := -1;
   end;

  // cbSyncVis.Checked := BassPlayer1.SyncVisWindow;
   cbUseWinampLook.Checked := BassPlayer1.UseVisDrawer;
   VisScaleBar.Position := (BassPlayer1.VisScale - 256) div 24;

end;

procedure TVisControlForm.PluginListClick(Sender: TObject);
begin
   if PluginList.ItemIndex = CurIndexNum then
      exit;

   UnloadVisModule;
   GetPluginInfo;
end;

procedure TVisControlForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if IsShownConfigWindow then
   begin
      Application.MessageBox(pChar(msg[1]), 'Confirm', MB_OK + MB_ICONINFORMATION);
      Action := caNone;
      exit;
   end;

   UnloadVisModule;
end;

procedure TVisControlForm.btnConfigureClick(Sender: TObject);
var
   NewSelection : TGridRect;
   Vis_Plugin : string;
   Visheader : PWinampVisHeader;
   SelectedIndexNum : integer;
begin
   if IsShownConfigWindow then
      exit;

 // Reload vis plug-in if it is released.
   if NumVismod = 0 then
      if PluginList.Items.Count > 0 then
      begin
         SelectedIndexNum := PluginList.ItemIndex;
         Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
         LoadVisModule(Vis_Plugin, Visheader, Vismod, NumVismod, MainForm.Handle);
         if NumVismod = 0 then
            exit;
      end else
        exit;

   NewSelection := VisModules.Selection;
   if NewSelection.Top > (NumVismod - 1) then
      exit;

   IsShownConfigWindow := true;
   PluginList.Enabled := false;
   try
      Vismod[NewSelection.Top].Config(Vismod[NewSelection.Top]);
   except
      Application.MessageBox(pChar(msg[2]), 'Error', MB_OK + MB_ICONERROR);
   end;
   IsShownConfigWindow := false;
   PluginList.Enabled := true;
end;

procedure TVisControlForm.btnStartClick(Sender: TObject);
var
   Vis_Plugin : string;
   SelectedIndexNum : integer;
begin
   if IsShownConfigWindow then
      exit;

 // The vis plug-in should be loaded by the seperate thread which runs that.
 // Some vis plug-in gets main program's thread id (i.e., not the driver of vis plug-in's
 // thread id) if they are not unloaded before running vis plug-in even though they are
 // reloaded by the driver of vis plug-in.
 // In such case it is difficult to know whether vis window is created.

   if PluginList.Items.Count = 0 then    // if no vis plug-in
      exit;

 // The API call of CreateChildWindow(UserEMBED) results in failure
 //  if MainForm.PageControl1.ActivePage is not VisSheet.
  { if MainForm.PageControl1.ActivePage <> MainForm.VisSheet then
      if MainForm.cbMergeVisWindow.Checked then
         Application.MessageBox(pAnsiChar(msg[3] + chr(10) + msg[4]), 'Information',
                                   MB_OK + MB_ICONINFORMATION);   }

   SelectedIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
   if Vis_Plugin = string(CurVisWindowAttr.PluginPath) then  // Is currently running vis plug-in ?
      if VisModules.Selection.Top = CurVisWindowAttr.ModNo then
         exit;

   UnloadVisModule;
   NumVismod := 0;

  // TBASSPlayer is modified to enforce stability at starting a vis plug-in, while other
  // plug-in is running.  So, following code is not necessary and left only for reference.
  { if CurVisWindowAttr.VisHandle <> 0 then   // Visualization is active ?
   begin
    // call BassPlayer1.RunVisPlugin indirectly if vis plug-in is running, for stability.
    // procedure btnStartClick is re-called in procedure SetVisWindowAttr at detecting the
    // the signal of closing previous vis plug-in.
      WaitingPrevVisStop := true;
      BassPlayer1.QuitVisPlugin;
      exit;
   end; }

   BassPlayer1.RunVisPlugin(GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum],
                                VisModules.Selection.Top);
end;

procedure TVisControlForm.btnStopClick(Sender: TObject);
begin
   if CurVisWindowAttr.VisHandle = 0 then   // Visualization is not active ?
      exit;

   BassPlayer1.QuitVisPlugin;
end;

procedure TVisControlForm.cbSyncVisClick(Sender: TObject);
begin
 //  BassPlayer1.SyncVisWindow := cbSyncVis.Checked;
end;

procedure TVisControlForm.VisScaleBarChange(Sender: TObject);
begin
   BassPlayer1.VisScale := VisScaleBar.Position * 24 + 256;
end;

procedure TVisControlForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TVisControlForm.cbUseWinampLookClick(Sender: TObject);
begin
   if cbUseWinampLook.Checked then
      BassPlayer1.UseVisDrawer := true
   else
      BassPlayer1.UseVisDrawer := false;
end;

procedure TVisControlForm.FormCreate(Sender: TObject);
begin
   CurVisWindowAttr.VisHandle := 0;
   ApplyLocalLanguage;
end;

procedure TVisControlForm.PluginListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
   Vis_Plugin : string;
   IsRunningPlugin : boolean;
begin
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[Index];
   IsRunningPlugin := (Vis_Plugin = string(CurVisWindowAttr.PluginPath));

   with (Control as TListBox).Canvas do
   begin
      if odSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Control as TListBox).Color;

    	FillRect(Rect);       // clear the rectangle

      if IsRunningPlugin then
         if odSelected in State then
            Font.Color := clYellow
         else
            Font.Color := clRed
      else
         if odSelected in State then
            Font.Color := clWhite
         else
            Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 1, PluginList.Items[Index]);
      if odFocused in State then
         DrawFocusRect(Rect);
   end;
end;

procedure TVisControlForm.VisModulesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   Vis_Plugin, ModuleName : string;
   IsRunningModule : boolean;
   SelectedIndexNum : integer;
begin
   if PluginList.ItemIndex = -1 then
      exit;
      
   SelectedIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
   if Vis_Plugin = string(CurVisWindowAttr.PluginPath) then
      if ARow = CurVisWindowAttr.ModNo then
         IsRunningModule := true
      else
         IsRunningModule := false
   else
      IsRunningModule := false;

   ModuleName := VisModules.Cells[0, ARow];
   with (Sender as TDrawGrid).Canvas do
   begin
      if gdSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Sender as TDrawGrid).Color;

      FillRect(Rect);   // clear the rectangle

      if IsRunningModule then
         if gdSelected in State then
            Font.Color := clYellow
         else
            Font.Color := clRed
      else if gdSelected in State then
         Font.Color := clWhite
      else
         Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 1, ModuleName);

      if gdFocused in State then
         DrawFocusRect(Rect);
   end;
end;

end.
