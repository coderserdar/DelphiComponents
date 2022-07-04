// Unit PluginConfig
//
//  This unit takes charge of configuring BASS add-ons for BassTest
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.1                          1 Oct 2008
//   Added procedure ApplyLocalLanguage to support localizaton of program
//
// Ver 1.0                         26 Oct 2006
//   - Initial release
//

unit AddonConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, BassPlayer, PluginCtrl;

type
  TAddonConfigForm = class(TForm)
    lbl_Detected: TLabel;
    lbl_Note_1: TLabel;
    PluginGrid: TStringGrid;
    lbl_Loaded: TLabel;
    btnLoad: TButton;
    btnUnload: TButton;
    btnClose: TButton;
    lbl_Note_3: TLabel;
    PluginList: TStringGrid;
    lbl_Note_2: TLabel;
    procedure ClearPluginGrid;
    procedure FillPluginGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnUnloadClick(Sender: TObject);
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
    procedure ApplyLocalLanguage;      // * Added at Ver 1.1
    procedure SetInUsePlugin(CurPluginName : string);
  end;

var
  AddonConfigForm: TAddonConfigForm;


implementation

uses BassTest;

{$R *.DFM}

const
   msgCount = 6;

var
   LoadCounter : integer;
   PluginCounter : integer;
   CurSelection : TGridRect;
   IndexNum : integer;
   PluginName : string;
   InUsePlugin : string = '';
 //  MainMessageHandle : hwnd;
   BASSAddonList : TBASSAddonList;

   msg : array[1..msgCount] of string =
                  ('No add-ons are loaded',
                   'Select an add-on to unload first',
                   'Cannot unload : add-on is in use',
                   'Cannot get the handle of add-on',
                   'Not a valid add-on',
                   'Confirm');

procedure TAddonConfigForm.ApplyLocalLanguage;  // * Added at Ver 1.1
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
              end else if ItemType = '$' then
              begin
                 if (Temp is TEdit) then
                    (Temp as TEdit).Text := ItemVal
                 else if (Temp is TComboBox) then
                    (Temp as TComboBox).Text := ItemVal
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

procedure TAddonConfigForm.ClearPluginGrid;
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

procedure TAddonConfigForm.FillPluginGrid;
var
   i, j : integer;
   s1, s2 : string;


begin
   LoadCounter := 0;
   BASSAddonList := BassPlayer1.GetBASSAddonList;
   for i := 1 to MaxLoadableAddons do
   begin
      if BASSAddonList[i].Handle <> 0 then     // Handle = 0 : empty element
      begin
         inc(LoadCounter);
         PluginGrid.Cells[0, LoadCounter] := BASSAddonList[i].Name;
         s1 := '';
         s2 := '';
         for j := 1 to BASSAddonList[i].NumFormat do
         begin
            if s1 <> '' then
               s1 := s1 + ', ' + BASSAddonList[i].FormatP[j-1].name
            else
               s1 := BASSAddonList[i].FormatP[j-1].name;
            if s2 <> '' then
               s2 := s2 + ';' + BASSAddonList[i].FormatP[j-1].exts
            else
               s2 := BASSAddonList[i].FormatP[j-1].exts;
         end;
         PluginGrid.Cells[1, LoadCounter] := s1 + ' V' +
                                          intToStr(Hi(HiWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Lo(HiWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Hi(LoWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Lo(LoWord(BASSAddonList[i].Version)));
         PluginGrid.Cells[2, LoadCounter] := s2;
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

procedure TAddonConfigForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TAddonConfigForm.btnLoadClick(Sender: TObject);
var
   BASSAddonInfo : TBASSAddonInfo;
   s1, s2 : string;
   j : integer;
begin
   IndexNum := PluginList.Selection.Top;
   PluginName := PluginList.Cells[0, IndexNum];
   if PluginName = '' then
   begin
      Application.MessageBox(pChar(msg[2]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   BASSAddonInfo := BassPlayer1.BASSAddonLoad(GetProgDir + 'Plugins\' + PluginName);
   if BASSAddonInfo.Handle <> 0 then
   begin
      inc(LoadCounter);
      PluginGrid.Cells[0, LoadCounter] := BASSAddonInfo.Name;
      s1 := '';
      s2 := '';
      for j := 1 to BASSAddonInfo.NumFormat do
      begin
         if s1 <> '' then
            s1 := s1 + ', ' + BASSAddonInfo.FormatP[j-1].name
         else
            s1 := BASSAddonInfo.FormatP[j-1].name;
         if s2 <> '' then
            s2 := s2 + ';' + BASSAddonInfo.FormatP[j-1].exts
         else
            s2 := BASSAddonInfo.FormatP[j-1].exts;
      end;
      PluginGrid.Cells[1, LoadCounter] := s1 + ' V' +
                                          intToStr(Hi(HiWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Lo(HiWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Hi(LoWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Lo(LoWord(BASSAddonInfo.Version)));
      PluginGrid.Cells[2, LoadCounter] := s2;

   { if LoadCounter = 1 then
      begin } // put the 1st add-on to selected state
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
   end else
      Application.MessageBox(pChar(msg[5]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
end;


procedure TAddonConfigForm.btnUnloadClick(Sender: TObject);
var
   AddonName : string;
   AddonHandle : DWORD;
   Foundmatched : boolean;
   i : integer;
begin
   if LoadCounter = 0 then
   begin
      Application.MessageBox(pChar(msg[1]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top = -1 then
   begin
      Application.MessageBox(pChar(msg[2]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   AddonName := PluginGrid.Cells[0, CurSelection.Top];
   if AddonName = '' then
   begin
      Application.MessageBox(pChar(msg[2]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   Foundmatched := false;
   BASSAddonList := BassPlayer1.GetBASSAddonList;
   for i := 1 to MaxLoadableAddons do
      if BASSAddonList[i].Name = AddonName then
      begin
         AddonHandle := BASSAddonList[i].Handle;
         Foundmatched := true;
      end;

   if not Foundmatched then
   begin
      Application.MessageBox(pChar(msg[4]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   if BassPlayer1.BASSAddonFree(AddonHandle) > 0 then
   begin
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
         if LoadCounter = 0 then  // No add-on is loaded.
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
   end else
      Application.MessageBox(pChar(msg[3]), pChar(msg[6]), MB_OK or MB_ICONEXCLAMATION);
end;


procedure TAddonConfigForm.FormShow(Sender: TObject);
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
   if FindFirst(GetProgDir + 'Plugins\bass_*.dll', 0, SearchRec) = 0 then
   begin
      PluginList.Cells[0, 0] := SearchRec.Name;
      i := 1;
      while FindNext(SearchRec) = 0 do
      begin
         PluginList.RowCount := i + 1;
         PluginList.Cells[0, i] := SearchRec.Name;
         inc(i);
      end;
   end;
   FindClose(SearchRec);
   PluginCounter := i;

   ClearPluginGrid;
   FillPluginGrid;

   if PluginCounter = 0 then   // There are no add-on in <Prog_Dir>\Plugins directory..
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

   if LoadCounter = 0 then   // There are no loaded add-on.
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

procedure TAddonConfigForm.FormCreate(Sender: TObject);
begin
   with PluginGrid do
   begin
      Cells[0, 0] := 'Name';
      Cells[1, 0] := 'Decsription';
      Cells[2, 0] := 'Playable File Types';
   end;

   ApplyLocalLanguage;
end;

procedure TAddonConfigForm.PluginGridSelectCell(Sender: TObject; ACol,
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

procedure TAddonConfigForm.PluginListDrawCell(Sender: TObject; ACol,
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

   with Sender as TDrawGrid do
   begin
      if gdSelected in State then
      begin
         Canvas.Brush.Color := clHighlight;
         if PluginLoaded then
            btnLoad.Enabled := false
         else
            btnLoad.Enabled := true;
      end else
         Canvas.Brush.Color := (Sender as TDrawGrid).Color;
      Canvas.FillRect(Rect);

      if PluginLoaded then
         Canvas.Font.Color := clLtGray
      else
         if gdSelected in State then
            Canvas.Font.Color := clWhite
         else
            Canvas.Font.Color := clBlack;

      Canvas.Font.Charset := ANSI_CHARSET;
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 9;
      Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
      if gdFocused in State then
         Canvas.DrawFocusRect(Rect);
   end;
end;

// Enables or disables "Unload" button according to the state of a selected add-on,
//  if selected add-on is in use then "Unload" button is set to disabled state
//  else set to enabled state.
procedure TAddonConfigForm.SetInUsePlugin(CurPluginName : string);
begin
   InUsePlugin := CurPluginName;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top > 0 then
      PluginName := PluginGrid.Cells[0, CurSelection.Top]
   else
      PluginName := '';

   if InUsePlugin = '' then   // no add-on is in use.
      btnUnload.Enabled := true
   else if InUsePlugin = PluginName then
      btnUnload.Enabled := false
   else
      btnUnload.Enabled := true;

   PluginGrid.Invalidate;   // Redraw PluginGrid surface
end;

// Paints with different colors according to the state of add-ons,
// i.e., paints with Red color if the add-on is in use.
procedure TAddonConfigForm.PluginGridDrawCell(Sender: TObject; ACol,
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

   with Sender as TDrawGrid do
   begin
      if ARow = 0 then
         Canvas.Brush.Color := TColor($C0DCC0)  // = clMoneyGreen
      else if gdSelected in State then
         Canvas.Brush.Color := clHighlight
      else
         Canvas.Brush.Color := (Sender as TDrawGrid).Color;
      Canvas.FillRect(Rect);

      if PluginName = InUsePlugin then
         if gdSelected in State then
            Canvas.Font.Color := clYellow
         else
            Canvas.Font.Color := clRed
      else if gdSelected in State then
         Canvas.Font.Color := clWhite
      else
         Canvas.Font.Color := clBlack;

      Canvas.Font.Charset := ANSI_CHARSET;
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 9;
      case ACol of
         0 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
         1 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, Descr);
         2 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, ExtStr);
      end;

      if gdFocused in State then
         Canvas.DrawFocusRect(Rect);
   end;
end;

end.
