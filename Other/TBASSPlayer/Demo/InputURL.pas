// unit InputURL
//
// This unit takes charge of providing user with the method to select or input an URL to play.
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.0                        19 Jan 2009
//   - Initial release

unit InputURL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, buttons, ExtCtrls;

type
  TURLInputForm = class(TForm)
    Panel2: TPanel;
    Label_Warn1: TLabel;
    ComboBox1: TComboBox;
    btnNetOpen: TButton;
    lbl_note1: TLabel;
    lbl_note2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNetOpenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RequestToOpen : boolean;

    procedure ApplyLocalLanguage;
  end;

var
  URLInputForm: TURLInputForm;

implementation

{$R *.DFM}

const
   URLHdr_1 = 'http://';
   URLHdr_2 = 'mms://';

   msgCount = 2;
   msgs : array[1..msgCount] of string
            = ('Confirm',
               'The allowed format of URL is "http://..." or "mms://..."');

function IsURLHeader(S : string) : boolean;
begin
    if (copy(S, 1, 7) = URLHdr_1) or (copy(S, 1, 6) = URLHdr_2) then
       result := true
    else
       result := false;
end;

procedure TURLInputForm.ApplyLocalLanguage;
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
                 else if (Temp is TSpeedButton) then
                    (Temp as TSpeedButton).Caption := ItemVal
                 else if (Temp is TCheckBox) then
                    (Temp as TCheckBox).Caption := ItemVal
                 else if (Temp is TTabSheet) then
                    (Temp as TTabSheet).Caption := ItemVal
                 else if (Temp is TGroupBox) then
                    (Temp as TGroupBox).Caption := ItemVal
              end else if ItemType = '$' then
              begin
                 if (Temp is TEdit) then
                    (Temp as TEdit).Text := ItemVal
                 else if (Temp is TComboBox) then
                    (Temp as TComboBox).Text := ItemVal
              end else if ItemType = '*' then
                 (Temp as TControl).Hint := ItemVal
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

  // Substitute message string with local text.
  for i := 1 to Items_Lang_Str do
  begin
     SepPos := pos('=', Lang_Str[i - 1]);
     S_org := copy(Lang_Str[i - 1], 1, SepPos - 1);
     S_local := copy(Lang_Str[i - 1], SepPos + 1, length(Lang_Str[i - 1]) - SepPos);
     for j := 1 to msgCount do
        if msgs[j] = S_org then
        begin
           msgs[j] := S_local;
           break;
        end;
  end;

  SetLength(Lang_Str, 0);
end;

procedure TURLInputForm.FormShow(Sender: TObject);
begin
   RequestToOpen := false;
  { ComboBox1.Text := 'Or, you can enter URL here.';
   ComboBox1.ItemIndex := -1; }
end;

procedure TURLInputForm.FormCreate(Sender: TObject);
begin
   ComboBox1.Items.Add('http://stream-5.ssatr.ch:80/rsp/aacp');  // AAC+  (needs bass_aac.dll)
   ComboBox1.Items.Add('http://scfire-chi-aa02.stream.aol.com:80/stream/1040'); // MP3
   ComboBox1.Items.Add('http://yle.fi/livestream/ylenklassinen.asx');  // WMA
   ComboBox1.Items.Add('http://www.gaduradio.pl/winamp.m3u');  // M3U play list
   ComboBox1.Items.Add('http://www.sky.fm/mp3/classical_low.pls');  // PLS play list
   ComboBox1.Items.Add('http://zam.inlive.co.kr:4580');    // MP3 (Korean site)
   ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/bette midler - the rose.ogg');
   ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/love story of a girl.mp3');
   ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/arirang.wma');
   ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/i like you.asf');
   ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/takelong.mid');
 //  ComboBox1.Items.Add('http://myfile.hanafos.com/~hyunsh/music/frank mills - the poet and i.aac');

   ApplyLocalLanguage;
end;

procedure TURLInputForm.btnNetOpenClick(Sender: TObject);
begin
   if not IsURLHeader(ComboBox1.Text) then
   begin
      if ComboBox1.Text[1] = '/' then
         if ComboBox1.Text[2] = '/' then
            ComboBox1.Text := 'http:' + ComboBox1.Text
         else
            ComboBox1.Text := 'http:/' + ComboBox1.Text
      else
         ComboBox1.Text := 'http://' + ComboBox1.Text;

    //  Application.MessageBox(pAnsiChar(msgs[2]), pAnsiChar(msgs[1]), MB_OK or MB_ICONEXCLAMATION);
    //  exit;
   end;

   RequestToOpen := true;
   Close;
end;

end.
