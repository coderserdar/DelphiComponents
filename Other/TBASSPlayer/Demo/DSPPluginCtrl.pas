// unit DSPPluginCtrl
//
// This unit takes charge of managing Winamp DSP plug-ins for Demo program.
// You can select, start or stop Winamp DSP plug-ins using this unit.
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.1                         1 Oct 2008
//   Fixed the problem of quitting operation of DSP plug-in when user click on
//    PluginList
//   Added procedure ApplyLocalLanguage to support localizaton of program
//
// Ver 1.01                       24 Oct 2006
//   Some trivial changes such as caption, comments
//
// Ver 1.0                        15 Dec 2003
//   - Initial release

unit DSPPluginCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls;

type
  TDSPControlForm = class(TForm)
    lbl_Detected: TLabel;
    lbl_Descripion: TLabel;
    lbl_Modules: TLabel;
    lbl_Note_1: TLabel;
    lbl_Note_2: TLabel;
    PluginList: TListBox;
    btnConfigure: TButton;
    btnStart: TButton;
    Descript: TEdit;
    btnStop: TButton;
    DSPModules: TStringGrid;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure PluginListClick(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ApplyLocalLanguage;   // * Added at Ver 1.1
    procedure GetPluginInfo;
  public
    { Public declarations }
  end;

var
  DSPControlForm: TDSPControlForm;

implementation

{$R *.DFM}

uses
   ioPlug, PluginCtrl, BassTest;

const
   msgCount = 3;

var
   DSPmod : TDSPmod;
   IndexNum,
   NumDSPmod : integer;
   DLLHandle : THandle = 0;
   RunningDSP : string;

   msg : array[1..msgCount] of string
               = ('Invalid plug-in.',
                  'Unknown error in plug-in.',
                  'Error');

procedure TDSPControlForm.ApplyLocalLanguage;  // * Added at Ver 1.1
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

procedure TDSPControlForm.GetPluginInfo;
var
   DSP_Plugin : string;
   getDSPHeader2 : function : pointer; stdcall;
   DSPheader : PWinampDSPHeader;
   i : integer;
begin
   NumDSPmod := 0;
   if DSPModules.RowCount > 0 then
      DSPModules.Cells[0, 0] := '';
   DSPModules.RowCount := 0;
   Descript.Text := '';

   IndexNum := PluginList.ItemIndex;
   DSP_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[IndexNum];

   if DSP_Plugin = RunningDSP then
   begin
      DSPheader := getDSPHeader;
      if DSPHeader = nil then
         exit;
   end else
   begin
      DLLHandle := LoadLibrary(pchar(DSP_Plugin));
      if (DLLHandle = 0) then
         exit;

      getDSPHeader2 := GetProcAddress(DLLHandle, 'winampDSPGetHeader2');
      if @getDSPHeader2 = nil then
      begin
         FreeLibrary(DLLHandle);
         exit;
      end;

      DSPheader := getDSPHeader2;
      if DSPHeader = nil then
      begin
         FreeLibrary(DLLHandle);
         exit;
      end;
   end;

   for i := 0 to (maxDSPmodNum - 1) do
   begin
      DSPmod[i] := DSPheader.getModule(i);
      if DSPmod[i] <> nil then
      begin
         DSPModules.Cells[0, i] := DSPmod[i].description;
         inc(NumDSPmod);
      end else
         break;
   end;

   DSPModules.RowCount := NumDSPmod;
   if NumDSPmod > 0 then
      Descript.Text := string(DSPheader^.description)

end;


procedure TDSPControlForm.FormShow(Sender: TObject);
var
   SearchRec : TSearchRec;
begin
   IndexNum := -1;
   NumDSPmod := 0;
   PluginList.Clear;

   if FindFirst(GetProgDir + 'Plugins\DSP_*.dll', 0, SearchRec) = 0 then
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
      DSPModules.RowCount := 0;
   end;

end;

procedure TDSPControlForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TDSPControlForm.PluginListClick(Sender: TObject);
begin
   if PluginList.ItemIndex = IndexNum then
      exit;

   if DLLHandle <> 0 then
   begin
      FreeLibrary(DLLHandle);
      DLLHandle := 0;
   end;
   GetPluginInfo;
end;

procedure TDSPControlForm.btnConfigureClick(Sender: TObject);
var
   NewSelection : TGridRect;
begin
   if NumDSPmod = 0 then
   begin
      Application.MessageBox(pchar(msg[1]), pchar(msg[3]), MB_OK + MB_ICONERROR);
      exit;
   end;

   NewSelection := DSPModules.Selection;
   if NewSelection.Top > (NumDSPmod - 1) then
      exit;

   try
      DSPmod[NewSelection.Top].Config(DSPmod[NewSelection.Top]);
   except
      Application.MessageBox(pchar(msg[2]), pchar(msg[3]), MB_OK + MB_ICONERROR);
   end;
end;

procedure TDSPControlForm.btnStartClick(Sender: TObject);
var
   DSPToRun : string;
begin
   if NumDSPmod = 0 then
   begin
      Application.MessageBox(pchar(msg[1]), pchar(msg[3]), MB_OK + MB_ICONERROR);
      exit;
   end;

   DSPToRun := GetProgDir + 'Plugins\' + PluginList.Items[IndexNum];
   if RunningDSP <> '' then
      if DSPToRun = RunningDSP then
         exit
      else
         BassPlayer1.QuitDSPPlugin;

   if BassPlayer1.RunDSPPlugin(DSPToRun, DSPModules.Selection.Top) then
      RunningDSP := DSPToRun
   else
      RunningDSP := '';
end;

procedure TDSPControlForm.btnStopClick(Sender: TObject);
begin
   if RunningDSP <> '' then
   begin
      BassPlayer1.QuitDSPPlugin;
      RunningDSP := '';
   end;
end;

procedure TDSPControlForm.FormDestroy(Sender: TObject);
begin
   if DLLHandle <> 0 then
      FreeLibrary(DLLHandle);
end;
   
procedure TDSPControlForm.FormCreate(Sender: TObject);
begin
   ApplyLocalLanguage;

end;

end.
