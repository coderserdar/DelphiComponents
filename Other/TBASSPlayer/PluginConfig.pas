// Unit PluginConfig
//
//  This unit takes charge of configuring Winamp input plug-ins for TBASSPlayer
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.22                       1  Oct 2008
//   Added procedure ApplyLocalLanguage to support localizaton of program
//
// Ver 1.21.1                     9 Jun 2008
//   Some portions of code are simplified for readability
//
// Ver 1.21                       24 Oct 2006
//   Some trivial changes such as caption, comments
//
// Ver 1.2                         7 Feb 2005
//   Changed to set the enabled state of "Unload" button automatically, according
//      to the in-use-state of plug-ins.
//   Changed to use different colors for displaying plug-in's information according to
//    the status of the input plug-in (in use or not in use, loaded or not loaded).
//
// Ver 1.1                         15 Feb 2003
//   Changed the goRangeSelect property of PluginGrid from true to false
//       (bug fix : to prohibit multiple selection of plug-ins to unload)
//
// Ver 1.0                         30 Jan 2003
//   - Initial release
//

unit PluginConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, PluginCtrl;

type
  TPluginConfigForm = class(TForm)
    lbl_Detected: TLabel;
    lbl_Note_1: TLabel;
    PluginGrid: TStringGrid;
    lbl_Loaded: TLabel;
    btnLoad: TButton;
    btnUnload: TButton;
    btnAbout: TButton;
    btnConfigure: TButton;
    btnClose: TButton;
    lbl_Note_2: TLabel;
    cbPluginFirst: TCheckBox;
    lbl_PluginFirst: TLabel;
    PluginList: TStringGrid;
    procedure ClearPluginGrid;
    procedure FillPluginGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnUnloadClick(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure cbPluginFirstClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PluginGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PluginListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure PluginGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyLocalLanguage;        // * New at Ver 1.22
    procedure SetInUsePlugin(CurPluginName : string);
  end;

{var
  PluginConfigForm: TPluginConfigForm; }

  procedure SetMessageHandle(MessageHandle : hwnd);

implementation

{$R *.DFM}

const
   msgCount = 17;

var
   LoadCounter : integer;
   PluginCounter : integer;
   CurSelection : TGridRect;
   IndexNum : integer;
   PluginName : string;
   InUsePlugin : string = '';
   MainMessageHandle : hwnd;

   msg : array[1..msgCount] of string
               = ('No plug-in to load',
                  'Select plug-in to load first',
                  'Load denied : Output plug-in is not ready',
                  'Load denied : Already loaded before',
                  'Load denied : Max. number of plug-ins loaded',
                  'Load denied : Unknown Error',
                  'Load denied : Not a valid Winamp input plug-in',
                  'Confirm',                                 // 8
                  'No plug-ins are loaded',                  // 9
                  'Select plug-in to unload first',          // 10
                  'Cannot unload : Unknown Error',           // 11
                  'Cannot unload : Plug-in is not loaded',   // 12
                  'Cannot unload : Plug-in is in use',       // 13
                  'Select plug-in to query first',           // 14
                  'No plug-ins to query are loaded',         // 15
                  'Select plug-in to configure first',       // 16
                  'No plug-ins to configure are loaded');    // 17


procedure SetMessageHandle(MessageHandle : hwnd);
begin
   MainMessageHandle := MessageHandle;
end;

procedure TPluginConfigForm.ApplyLocalLanguage;  // * New at Ver 1.22
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
  tmpStr : string;
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

   for i := 1 to Items_Lang_Str do
   begin
      tmpStr := copy(Lang_Str[i-1], 1, 8);
      if (tmpStr = '^Cell_00') or (tmpStr = '^Cell_10') or (tmpStr = '^Cell_20') then
      begin
         SepPos := pos('=', Lang_Str[i-1]);
         ItemVal := copy(Lang_Str[i-1], SepPos + 1, length(Lang_Str[i-1]) - SepPos);
         if tmpStr = '^Cell_00' then
            PluginGrid.Cells[0, 0] := ItemVal
         else if tmpStr = '^Cell_10' then
            PluginGrid.Cells[1, 0] := ItemVal
         else if tmpStr = '^Cell_20' then
            PluginGrid.Cells[2, 0] := ItemVal;
      end else
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
      end
   end;

   SetLength(Lang_Str, 0);
end;

procedure TPluginConfigForm.ClearPluginGrid;
var
   i : integer;
begin
   with PluginGrid do
   begin
     Perform(WM_SETREDRAW, 0, 0);  // block visual updates
     try
       for i := FixedRows to RowCount - 1 do
         Rows[i].Clear;
     finally
       Perform(WM_SETREDRAW, 1, 0);
       Invalidate;
     end;
   end;
end;

procedure TPluginConfigForm.FillPluginGrid;
var
   i : integer;
   WinampPlugin : TPlugin;
   WinampPluginInfo : TPluginInfo;
begin
   LoadCounter := 0;
   for i := 0 to MaxPluginNum - 1 do
   begin
      WinampPlugin := GetPlugin(i);
      if WinampPlugin <> nil then     // APlugin = nil -> empty element
      begin
         WinampPluginInfo := GetPluginInfo(i);
         inc(LoadCounter);
         PluginGrid.Cells[0, LoadCounter] := WinampPluginInfo.Name;
         PluginGrid.Cells[1, LoadCounter] := WinampPluginInfo.Description;
         PluginGrid.Cells[2, LoadCounter] := WinampPluginInfo.FileExtensions;
      end;
   end;

   if LoadCounter = 0 then
      btnUnload.Enabled := false
   else begin
      CurSelection := PluginGrid.Selection;
      if CurSelection.Top > 0 then
         if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
            btnUnload.Enabled := false
         else
            btnUnload.Enabled := true
      else
         btnUnload.Enabled := false;
   end;
end;

procedure TPluginConfigForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;


procedure TPluginConfigForm.btnLoadClick(Sender: TObject);
var
   WinampPluginInfo : TPluginInfo;
begin
   IndexNum := PluginList.Selection.Top;
   PluginName := PluginList.Cells[0, IndexNum];
   if PluginName = '' then
   begin
      Application.MessageBox(pchar(msg[2]), pchar(msg[8]), MB_OK);
      exit;
   end;

   case LoadWinampPlugin(PluginName) of
      0 : begin      // 0 : No error
             inc(LoadCounter);
             IndexNum := GetPluginIndex(PluginName);
             WinampPluginInfo := GetPluginInfo(IndexNum);
             PluginGrid.Cells[0, LoadCounter] := WinampPluginInfo.Name;
             PluginGrid.Cells[1, LoadCounter] := WinampPluginInfo.Description;
             PluginGrid.Cells[2, LoadCounter] := WinampPluginInfo.FileExtensions;

            { if LoadCounter = 1 then
             begin } // put the 1st plug-in to selected state
                CurSelection.Top := LoadCounter;
                CurSelection.Left := 0;
                CurSelection.Right := 2;
                CurSelection.Bottom := LoadCounter;
                PluginGrid.Selection := CurSelection;
                if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
                   btnUnload.Enabled := false
                else
                   btnUnload.Enabled := true;
            { end; }
             PluginList.Invalidate;
         //    btnLoad.Enabled := false;
          end;
      ERROR_OMOD_NOTREADY : Application.MessageBox(pchar(msg[3]), pchar(msg[8]), MB_OK);
      ERROR_LOADED_BEFORE : Application.MessageBox(pchar(msg[4]), pchar(msg[8]), MB_OK);
      ERROR_LOADED_FULL   : Application.MessageBox(pchar(msg[5]), pchar(msg[8]), MB_OK);
      ERROR_CANNOT_LOAD   : Application.MessageBox(pchar(msg[6]), pchar(msg[8]), MB_OK);
      ERROR_INVALID_PLUGIN : Application.MessageBox(pchar(msg[7]), pchar(msg[8]), MB_OK);
   end;
end;


procedure TPluginConfigForm.btnUnloadClick(Sender: TObject);
begin
   if LoadCounter = 0 then
   begin
      Application.MessageBox(pchar(msg[9]), pchar(msg[8]), MB_OK);
      exit;
   end;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top = -1 then
   begin
      Application.MessageBox(pchar(msg[10]), pchar(msg[8]), MB_OK);
      exit;
   end;

   PluginName := PluginGrid.Cells[0, CurSelection.Top];
   if PluginName = '' then
   begin
      Application.MessageBox(pchar(msg[10]), pchar(msg[8]), MB_OK);
      exit;
   end;

   case UnloadWinampPlugin(PluginName) of
      0 : begin
            if CurSelection.Top = LoadCounter then   // the last item in PluginGrid ?
            begin
               PluginGrid.Cells[0, LoadCounter] := '';
               PluginGrid.Cells[1, LoadCounter] := '';
               PluginGrid.Cells[2, LoadCounter] := '';
               dec(LoadCounter);
               if LoadCounter > 0 then
               begin  // Move selected position
                  CurSelection.Top := LoadCounter;
                  CurSelection.Left := 0;
                  CurSelection.Right := 2;
                  CurSelection.Bottom := LoadCounter;
               end else
               begin  // No selected position
                  CurSelection.Top := -1;
                  CurSelection.Left := 0;
                  CurSelection.Right := 2;
                  CurSelection.Bottom := -1;
               end;
               PluginGrid.Selection := CurSelection;
               if LoadCounter = 0 then  // No plug-in is loaded
                  btnUnload.Enabled := false
               else if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
                  btnUnload.Enabled := false
               else
                  btnUnload.Enabled := true
            end else
            begin
               ClearPluginGrid;
               FillPluginGrid;
            end;
            PluginList.Invalidate;
         end;
      ERROR_CANNOT_UNLOAD : Application.MessageBox(pchar(msg[11]), pchar(msg[8]), MB_OK);
      ERROR_NOT_LOADED : Application.MessageBox(pchar(msg[12]), pchar(msg[8]), MB_OK);
      ERROR_IN_USE     : Application.MessageBox(pchar(msg[13]), pchar(msg[8]), MB_OK);
   end;
end;


procedure TPluginConfigForm.btnAboutClick(Sender: TObject);
var
   WinampPlugin : TPlugin;
   PluginNum : integer;
begin
   if LoadCounter = 0 then
   begin
      Application.MessageBox(pchar(msg[15]), pchar(msg[8]), MB_OK);
      exit;
   end;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top = -1 then
   begin
      Application.MessageBox(pchar(msg[14]), pchar(msg[8]), MB_OK);
      exit;
   end;
   PluginName := PluginGrid.Cells[0, CurSelection.Top];
   if PluginName = '' then
   begin
      Application.MessageBox(pchar(msg[14]), pchar(msg[8]), MB_OK);
      exit;
   end;

   PluginNum := GetPluginIndex(PluginName);
   WinampPlugin := GetPlugin(PluginNum);
   WinampPlugin.About(Self.Handle);
end;

procedure TPluginConfigForm.btnConfigureClick(Sender: TObject);
var
   WinampPlugin : TPlugin;
   PluginNum : integer;
begin
   if LoadCounter = 0 then
   begin
      Application.MessageBox(pchar(msg[17]), pchar(msg[8]), MB_OK);
      exit;
   end;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top = -1 then
   begin
      Application.MessageBox(pchar(msg[16]), pchar(msg[8]), MB_OK);
      exit;
   end;
   PluginName := PluginGrid.Cells[0, CurSelection.Top];
   if PluginName = '' then
   begin
      Application.MessageBox(pchar(msg[16]), pchar(msg[8]), MB_OK);
      exit;
   end;

   PluginNum := GetPluginIndex(PluginName);
   WinampPlugin := GetPlugin(PluginNum);
   WinampPlugin.config(Self.Handle);
end;

procedure TPluginConfigForm.cbPluginFirstClick(Sender: TObject);
var
   msgPluginFirst : longint;
begin
   if cbPluginFirst.Checked then
      msgPluginFirst := 1
   else
      msgPluginFirst := 0;

 // Inform to parent component.
   PostMessage(MainMessageHandle, WM_PluginFirst_Changed, 0, msgPluginFirst);
end;


procedure TPluginConfigForm.FormShow(Sender: TObject);
var
   GridRect : TGridRect;
   SearchRec : TSearchRec;
   i : integer;
begin
 // Clear PluginList's contents
   PluginList.RowCount := 1;
   PluginList.Cells[0, 0] := '';
   btnLoad.Enabled := false;

   i := 0;
   if FindFirst(GetProgDir + 'Plugins\in_*.dll', 0, SearchRec) = 0 then
   begin
  //    PluginConfigForm.PluginList.Items.Add(SearchRec.Name);
      PluginList.Cells[0, 0] := SearchRec.Name;
      i := 1;
      while FindNext(SearchRec) = 0 do
      begin
     //    PluginConfigForm.PluginList.Items.Add(SearchRec.Name);
         PluginList.RowCount := i + 1;
         PluginList.Cells[0, i] := SearchRec.Name;
         inc(i);
      end;
   end;
   FindClose(SearchRec);
   PluginCounter := i;

   ClearPluginGrid;
   FillPluginGrid;

   if PluginCounter = 0 then   // There are no plug-ins.
   begin
      GridRect.Top := -1;
      GridRect.Left := -1;
      GridRect.Right := -1;
      GridRect.Bottom := -1;
      PluginList.Selection := GridRect;
      PluginList.Enabled := false;
      PluginGrid.Enabled := false;
   end else
   begin
      GridRect.Top := 0;
      GridRect.Left := 0;
      GridRect.Right := 0;
      GridRect.Bottom := 0;
      PluginList.Selection := GridRect;
      PluginList.Enabled := true;
      PluginGrid.Enabled := true;
   end;

   if LoadCounter = 0 then   // There are no loaded plug-ins.
   begin
      GridRect.Top := -1;
      GridRect.Left := -1;
      GridRect.Right := -1;
      GridRect.Bottom := -1;
      PluginGrid.Selection := GridRect;
   end else
   begin
      GridRect.Top := 1;
      GridRect.Left := 0;
      GridRect.Bottom := 1;
      GridRect.Right := 2;
      PluginGrid.Selection := GridRect;
   end;

end;

procedure TPluginConfigForm.FormCreate(Sender: TObject);
begin
   with PluginGrid do
   begin
      Cells[0, 0] := 'Name';
      Cells[1, 0] := 'Decsription';
      Cells[2, 0] := 'Playable File Types';
   end;

   ApplyLocalLanguage;
end;

procedure TPluginConfigForm.PluginGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
   if PluginGrid.Cells[ACol, ARow] = '' then
      CanSelect := false
   else begin
      if PluginGrid.Cells[0, ARow] = InUsePlugin then
         btnUnload.Enabled := false
      else
         btnUnload.Enabled := true;
      CanSelect := true;
   end;
end;

procedure TPluginConfigForm.PluginListDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
 //  PluginName : string;
   PluginLoaded : boolean;
   i : integer;
begin
   PluginName := PluginList.Cells[0, ARow];

   PluginLoaded := false;
   for i := 1 to LoadCounter do
      if Uppercase(PluginGrid.Cells[0, i]) = Uppercase(PluginName) then
      begin
          PluginLoaded := true;
          break;
      end;

   with (Sender as TDrawGrid).Canvas do
   begin
      if gdSelected in State then
      begin
         Brush.Color := clHighlight;
         if PluginLoaded then
            btnLoad.Enabled := false
         else
            btnLoad.Enabled := true;
      end else
         Brush.Color := (Sender as TDrawGrid).Color;
      FillRect(Rect);

      if PluginLoaded then
         Font.Color := clLtGray
      else
         if gdSelected in State then
            Font.Color := clWhite
         else
            Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
      if gdFocused in State then
         DrawFocusRect(Rect);
   end;
end;

// Enables or disables "Unload" button according to the state of plug-in,
//  if selected plug-in is in use then "Unload" button is set to disabled state
//  else set to enabled state.
procedure TPluginConfigForm.SetInUsePlugin(CurPluginName : string);
begin
   InUsePlugin := CurPluginName;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top > 0 then
      PluginName := PluginGrid.Cells[0, CurSelection.Top]
   else
      PluginName := '';

   if InUsePlugin = '' then   // no plug-in is in use.
      btnUnload.Enabled := true
   else if InUsePlugin = PluginName then
      btnUnload.Enabled := false
   else
      btnUnload.Enabled := true;

   PluginGrid.Invalidate;   // Redraw PluginGrid surface
end;

// Paints with different colors according to the state of plug-ins,
// i.e., paints with Red color if the plug-in is in use.
procedure TPluginConfigForm.PluginGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   PluginName : string;
   Descr : string;
   ExtStr : string;
begin
   PluginName := PluginGrid.Cells[0, ARow];
   case ACol of
     1 : Descr := PluginGrid.Cells[1, ARow];
     2 : ExtStr := PluginGrid.Cells[2, ARow];
   end;

   with (Sender as TDrawGrid).Canvas do
   begin
      if ARow = 0 then
         Brush.Color := TColor($C0DCC0)  // = clMoneyGreen
      else if gdSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Sender as TDrawGrid).Color;
      FillRect(Rect);

      if PluginName = InUsePlugin then
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
      case ACol of
         0 : TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
         1 : TextOut(Rect.Left + 3, Rect.Top + 1, Descr);
         2 : TextOut(Rect.Left + 3, Rect.Top + 1, ExtStr);
      end;

      if gdFocused in State then
         DrawFocusRect(Rect);
   end;
end;

end.
