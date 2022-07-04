// unit MPEGInfoBox
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.10                      29 Aug 2008
//  Added support for AAC files.
//
// Ver 1.01                       6 Nov 2006
//  Changed property "Position" of TMPEGFileInfoForm to poDesktopCenter.
//
// Ver 1.0                        31 Mar 2005
//   - Initial release
//


unit MPEGInfoBox;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MPEGaudio, ID3v1, ID3v2, filectrl, AACfile;

type
  TMPEGFileInfoForm = class(TForm)
    GroupBox1: TGroupBox;
    cbID3v1Tag: TCheckBox;
    Label1: TLabel;
    Tag1_TrackNo: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Tag1_Title: TEdit;
    Tag1_Artist: TEdit;
    Tag1_Album: TEdit;
    GroupBox2: TGroupBox;
    Tag1_Year: TEdit;
    Tag1_Genre: TComboBox;
    Genre: TLabel;
    Tag1_Comment: TEdit;
    GroupBox3: TGroupBox;
    lbSize: TLabel;
    lbHeaderPos: TLabel;
    lbLength: TLabel;
    lbType: TLabel;
    lbBitFrame: TLabel;
    lbFreq_Channels: TLabel;
    lbCRC: TLabel;
    lbCopyrighted: TLabel;
    lbOriginal: TLabel;
    lbEmphasis: TLabel;
    btnUpdate: TButton;
    cbID3v2Tag: TCheckBox;
    Label7: TLabel;
    Tag2_TrackNo: TEdit;
    Label8: TLabel;
    Tag2_Title: TEdit;
    Label9: TLabel;
    Tag2_Artist: TEdit;
    Label10: TLabel;
    Tag2_Album: TEdit;
    Label11: TLabel;
    Tag2_Year: TEdit;
    Label12: TLabel;
    Tag2_Genre: TComboBox;
    Label13: TLabel;
    Tag2_Comment: TMemo;
    Label14: TLabel;
    Tag2_Composer: TEdit;
    Label15: TLabel;
    Tag2_OrigArtist: TEdit;
    Label16: TLabel;
    Tag2_Copyright: TEdit;
    Label17: TLabel;
    Tag2_URL: TEdit;
    Label18: TLabel;
    Tag2_Encodedby: TEdit;
    btnCancel: TButton;
    btnUndo: TButton;
    btnCopyTo: TButton;
    btnCopyFrom: TButton;
    Label19: TLabel;
    fFileName: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbID3v1TagClick(Sender: TObject);
    procedure cbID3v2TagClick(Sender: TObject);
    procedure btnCopyToClick(Sender: TObject);
    procedure btnCopyFromClick(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Tag1_GenreKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }

  public
    { Public declarations }
 //   procedure SetMPEG(MPEG : TMPEGaudio);
    procedure SetAttributes(StreamName : string;
                            const MPEG_Info : TMPEGaudio;
                            const AAC_Info : TAACfile;
                            WorkOnAACYag : boolean;
                            Duration, BitRate : DWORD;
                            UseGivenData : boolean);
    procedure ClearTag1Fields;
    procedure ClearTag2Fields;
    procedure EnableTag1Fields;
    procedure EnableTag2Fields;
    procedure DisableTag1Fields;
    procedure DisableTag2Fields;
    procedure FillTagFields(const ID3V1 : TID3v1; const ID3V2 : TID3v2);
    procedure FillByFileContents;
    procedure FillByFileContents_AAC;
  end;

{var
  MPEGFileInfoForm: TMPEGFileInfoForm; }

implementation

{$R *.DFM}

var
   MPEG_ : TMPEGaudio;
   AAC_  : TAACfile;
   IsAACTag : boolean;
   FileName : string;
   HasID3v1Tag, HasID3v2Tag : boolean;
   GivenDuration : DWORD;
   GivenBitRate : DWORD;
   Use_GivenData : boolean;

{ procedure TMPEGFileInfoForm.SetMPEG(MPEG : TMPEGaudio);
begin
   MPEG_ := MPEG;
end; }

procedure TMPEGFileInfoForm.SetAttributes(StreamName : string;
                                          const MPEG_Info : TMPEGaudio;
                                          const AAC_Info : TAACfile;
                                          WorkOnAACYag : boolean;
                                          Duration, BitRate : DWORD;
                                          UseGivenData : boolean);
begin
   FileName := StreamName;
   MPEG_ := MPEG_Info;
   AAC_ := AAC_Info;
   IsAACTag := WorkOnAACYag;
   GivenDuration := Duration;
   GivenBitRate := BitRate;
   Use_GivenData := UseGivenData;
end;

procedure TMPEGFileInfoForm.ClearTag1Fields;
begin
   Tag1_TrackNo.Text := '';
   Tag1_Title.Text := '';
   Tag1_Artist.Text := '';
   Tag1_Album.Text := '';
   Tag1_Year.Text := '';
   Tag1_Genre.ItemIndex := 0;
   Tag1_Comment.Text := '';
end;

procedure TMPEGFileInfoForm.ClearTag2Fields;
begin
   Tag2_TrackNo.Text := '';
   Tag2_Title.Text := '';
   Tag2_Artist.Text := '';
   Tag2_Album.Text := '';
   Tag2_Year.Text := '';
   Tag2_Genre.ItemIndex := -1;
   Tag2_Comment.Clear;
   Tag2_Composer.Text := '';
   Tag2_OrigArtist.Text := '';
   Tag2_Copyright.Text := '';
   Tag2_URL.Text := '';
   Tag2_Encodedby.Text := '';
end;

procedure TMPEGFileInfoForm.EnableTag1Fields;
begin
   Tag1_TrackNo.Enabled := true;
   Tag1_Title.Enabled := true;
   Tag1_Artist.Enabled := true;
   Tag1_Album.Enabled := true;
   Tag1_Year.Enabled := true;
   Tag1_Genre.Enabled := true;
   Tag1_Comment.Enabled := true;

   Tag1_TrackNo.Color := clWindow;
   Tag1_Title.Color := clWindow;
   Tag1_Artist.Color := clWindow;
   Tag1_Album.Color := clWindow;
   Tag1_Year.Color := clWindow;
   Tag1_Genre.Color := clWindow;
   Tag1_Comment.Color := clWindow;
end;

procedure TMPEGFileInfoForm.EnableTag2Fields;
begin
   Tag2_TrackNo.Enabled := true;
   Tag2_Title.Enabled := true;
   Tag2_Artist.Enabled := true;
   Tag2_Album.Enabled := true;
   Tag2_Year.Enabled := true;
   Tag2_Genre.Enabled := true;
   Tag2_Comment.Enabled := true;
   Tag2_Composer.Enabled := true;
   Tag2_OrigArtist.Enabled := true;
   Tag2_Copyright.Enabled := true;
   Tag2_URL.Enabled := true;
   Tag2_Encodedby.Enabled := true;

   Tag2_TrackNo.Color := clWindow;
   Tag2_Title.Color := clWindow;
   Tag2_Artist.Color := clWindow;
   Tag2_Album.Color := clWindow;
   Tag2_Year.Color := clWindow;
   Tag2_Genre.Color := clWindow;
   Tag2_Comment.Color := clWindow;
   Tag2_Composer.Color := clWindow;
   Tag2_OrigArtist.Color := clWindow;
   Tag2_Copyright.Color := clWindow;
   Tag2_URL.Color := clWindow;
   Tag2_Encodedby.Color := clWindow;
end;

procedure TMPEGFileInfoForm.DisableTag1Fields;
begin
   Tag1_TrackNo.Enabled := false;
   Tag1_Title.Enabled := false;
   Tag1_Artist.Enabled := false;
   Tag1_Album.Enabled := false;
   Tag1_Year.Enabled := false;
   Tag1_Genre.Enabled := false;
   Tag1_Comment.Enabled := false;

   Tag1_TrackNo.Color := clBtnFace;
   Tag1_Title.Color := clBtnFace;
   Tag1_Artist.Color := clBtnFace;
   Tag1_Album.Color := clBtnFace;
   Tag1_Year.Color := clBtnFace;
   Tag1_Genre.Color := clBtnFace;
   Tag1_Comment.Color := clBtnFace;
end;

procedure TMPEGFileInfoForm.DisableTag2Fields;
begin
   Tag2_TrackNo.Enabled := false;
   Tag2_Title.Enabled := false;
   Tag2_Artist.Enabled := false;
   Tag2_Album.Enabled := false;
   Tag2_Year.Enabled := false;
   Tag2_Genre.Enabled := false;
   Tag2_Comment.Enabled := false;
   Tag2_Composer.Enabled := false;
   Tag2_OrigArtist.Enabled := false;
   Tag2_Copyright.Enabled := false;
   Tag2_URL.Enabled := false;
   Tag2_Encodedby.Enabled := false;

   Tag2_TrackNo.Color := clBtnFace;
   Tag2_Title.Color := clBtnFace;
   Tag2_Artist.Color := clBtnFace;
   Tag2_Album.Color := clBtnFace;
   Tag2_Year.Color := clBtnFace;
   Tag2_Genre.Color := clBtnFace;
   Tag2_Comment.Color := clBtnFace;
   Tag2_Composer.Color := clBtnFace;
   Tag2_OrigArtist.Color := clBtnFace;
   Tag2_Copyright.Color := clBtnFace;
   Tag2_URL.Color := clBtnFace;
   Tag2_Encodedby.Color := clBtnFace;
end;

procedure TMPEGFileInfoForm.FillTagFields(const ID3V1 : TID3v1; const ID3V2 : TID3v2);
var
   i : integer;
begin
   ClearTag1Fields;
   ClearTag2Fields;
   HasID3v1Tag := false;
   HasID3v2Tag := false;

   if ID3v1.Exists then
   begin
      HasID3v1Tag := true;
      cbID3v1Tag.Checked := true;
      EnableTag1Fields;
      Tag1_TrackNo.Text := intToStr(ID3v1.Track);
      Tag1_Title.Text := ID3v1.Title;
      Tag1_Artist.Text := ID3v1.Artist;
      Tag1_Album.Text := ID3v1.Album;
      Tag1_Year.Text := ID3v1.Year;

      Tag1_Genre.ItemIndex := 0;
      if ID3v1.GenreID < MAX_MUSIC_GENRES then
      begin
         for i := 1 to MAX_MUSIC_GENRES do
         if Tag1_Genre.Items[i] = aTAG_MusicGenre[ID3v1.GenreID] then
         begin
            Tag1_Genre.ItemIndex := i;
            break;
         end;
      end;

      Tag1_Comment.Text := ID3v1.Comment;
      btnCopyFrom.Enabled := true;
   end else
   begin
      cbID3v1Tag.Checked := false;
      DisableTag1Fields;
      btnCopyFrom.Enabled := false;
   end;

   if ID3v2.Exists then
   begin
      HasID3v2Tag := true;
      cbID3v2Tag.Checked := true;
      EnableTag2Fields;
      Tag2_TrackNo.Text := intToStr(ID3v2.Track);
      Tag2_Title.Text := ID3v2.Title;
      Tag2_Artist.Text := ID3v2.Artist;
      Tag2_Album.Text := ID3v2.Album;
      Tag2_Year.Text := ID3v2.Year;

      Tag2_Genre.ItemIndex := -1;
      Tag2_Genre.Text := ID3v2.Genre;
      for i := 0 to (MAX_MUSIC_GENRES - 1) do
      begin
         if ID3v2.Genre = Tag2_Genre.Items[i] then
         begin
            Tag2_Genre.ItemIndex := i;
            break;
         end;
      end;

      Tag2_Comment.Lines.Add(ID3v2.Comment);
      Tag2_Composer.Text := ID3v2.Composer;
    //  Tag2_OrigArtist.Text := ID3v2.OrigArtist;
      Tag2_Copyright.Text := ID3v2.Copyright;
      Tag2_URL.Text := ID3v2.Link;
      Tag2_Encodedby.Text := ID3v2.Encoder;
      btnCopyTo.Enabled := true;
   end else
   begin
      cbID3v2Tag.Checked := false;
      DisableTag2Fields;
      btnCopyTo.Enabled := false;
   end;
end;

procedure TMPEGFileInfoForm.FillByFileContents;

begin
   fFileName.Caption := MinimizeName(FileName, fFileName.Canvas, fFileName.Width);
 {  MPEG_.ReadFromFile(FileName);
   if MPEG_.Valid then
   begin  }
      lbSize.Caption := 'Size: ' + intToStr(MPEG_.FileLength) + ' bytes';
      lbHeaderPos.Caption := 'Header found at: ' + intToStr(MPEG_.Frame.Position) + ' bytes';
      lbLength.Caption := 'Length: ' + intToStr(trunc(MPEG_.Duration)) + ' seconds';
      lbType.Caption := MPEG_VERSION[MPEG_.Frame.VersionID] + ' ' + MPEG_LAYER[MPEG_.Frame.LayerID];
      lbBitFrame.Caption := intToStr(MPEG_.BitRate) + 'kbit, ' + intToStr(MPEG_.Frames) + ' frames';
      lbFreq_Channels.Caption := intToStr(MPEG_.SampleRate) + 'Hz ' + MPEG_.ChannelMode;
      if MPEG_.Frame.ProtectionBit then
         lbCRC.Caption := 'CRCs: Yes'
      else
         lbCRC.Caption := 'CRCs: No';
      if MPEG_.Frame.CopyrightBit then
         lbCopyrighted.Caption := 'Copyrighted: Yes'
      else
         lbCopyrighted.Caption := 'Copyrighted: No';
      if MPEG_.Frame.OriginalBit then
         lbOriginal.Caption := 'Original: Yes'
      else
         lbOriginal.Caption := 'Original: No';
      lbEmphasis.Caption := 'Emphasis: ' + MPEG_EMPHASIS[MPEG_.Frame.EmphasisID];

      FillTagFields(MPEG_.ID3V1, MPEG_.ID3V2);
  // end;
end;

procedure TMPEGFileInfoForm.FillByFileContents_AAC;
{var
   i : integer; }
begin
   fFileName.Caption := MinimizeName(FileName, fFileName.Canvas, fFileName.Width);
 {  MPEG_.ReadFromFile(FileName);
   if MPEG_.Valid then
   begin  }
      lbSize.Caption := 'Size: ' + intToStr(AAC_.FileSize) + ' bytes';
      lbHeaderPos.Caption := 'Header Type: ' + AAC_.HeaderType;

      if Use_GivenData then
      begin
         lbLength.Caption := 'Length: ' + intToStr(GivenDuration div 1000) + ' seconds';
         lbBitFrame.Caption := intToStr(GivenBitRate) + 'kbit, '
                              + intToStr(AAC_.TotalFrames) + ' frames';
      end else
      begin
         lbLength.Caption := 'Length: ' + intToStr(trunc(AAC_.Duration)) + ' seconds';
         lbBitFrame.Caption := intToStr(AAC_.BitRate div 1000) + 'kbit, '
                              + intToStr(AAC_.TotalFrames) + ' frames';
      end;

      lbType.Caption := 'MPEG Version: ' + AAC_.MPEGVersion;
      lbFreq_Channels.Caption := intToStr(AAC_.SampleRate) + 'Hz, '
                                 + intToStr(AAC_.Channels) + ' channels(s)';
      lbCRC.Caption := 'AAC Profile: ' + AAC_.Profile;
      lbCopyrighted.Caption := 'BitRate Type: ' + AAC_.BitRateType;
      lbOriginal.Caption := 'Original: N.A.';
      lbEmphasis.Caption := 'Emphasis: N.A.';

      FillTagFields(AAC_.ID3V1, AAC_.ID3V2);
end;

procedure TMPEGFileInfoForm.FormShow(Sender: TObject);
begin
   if not IsAACtag then
      FillByFileContents
   else
      FillByFileContents_AAC;

end;

procedure TMPEGFileInfoForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
 //  MPEG_ := TMPEGaudio.Create;
   FileName := '';

   Tag1_Genre.Items.Add('');
   for i := 0 to (MAX_MUSIC_GENRES - 1) do
   begin
      Tag1_Genre.Items.Add(aTAG_MusicGenre[i]);
      Tag2_Genre.Items.Add(aTAG_MusicGenre[i]);
   end;
   Tag1_Genre.ItemIndex := 0;
   Tag1_Genre.ItemIndex := -1;
end;


procedure TMPEGFileInfoForm.FormDestroy(Sender: TObject);
begin
 //  MPEG_.Free;
end;

procedure TMPEGFileInfoForm.cbID3v1TagClick(Sender: TObject);
begin
   if cbID3v1Tag.Checked then
   begin
      EnableTag1Fields;
      btnCopyFrom.Enabled := true;
   end else
   begin
      DisableTag1Fields;
      btnCopyFrom.Enabled := false;
   end;
end;

procedure TMPEGFileInfoForm.cbID3v2TagClick(Sender: TObject);
begin
   if cbID3v2Tag.Checked then
   begin
      EnableTag2Fields;
      btnCopyTo.Enabled := true;
   end else
   begin
      DisableTag2Fields;
      btnCopyTo.Enabled := false;
   end;
end;

procedure TMPEGFileInfoForm.btnCopyToClick(Sender: TObject);
var
   i : integer;
begin
   cbID3v1Tag.Checked := true;
   EnableTag1Fields;
   Tag1_TrackNo.Text := Tag2_TrackNo.Text;
   Tag1_Title.Text := Tag2_Title.Text;
   Tag1_Artist.Text := Tag2_Artist.Text;
   Tag1_Album.Text := Tag2_Album.Text;
   Tag1_Year.Text := Tag2_Year.Text;

   Tag1_Genre.ItemIndex := 0;
   for i := 1 to MAX_MUSIC_GENRES do
   begin
      if Tag1_Genre.Items[i] = Tag2_Genre.Text then
      begin
         Tag1_Genre.ItemIndex := i;
         break;
      end;
   end;

   Tag1_Comment.Text := Tag2_Comment.Text;
   btnCopyFrom.Enabled := true;
end;

procedure TMPEGFileInfoForm.btnCopyFromClick(Sender: TObject);
begin
   cbID3v2Tag.Checked := true;
   EnableTag2Fields;
   Tag2_TrackNo.Text := Tag1_TrackNo.Text;
   Tag2_Title.Text := Tag1_Title.Text;
   Tag2_Artist.Text := Tag1_Artist.Text;
   Tag2_Album.Text := Tag1_Album.Text;
   Tag2_Year.Text := Tag1_Year.Text;
   Tag2_Genre.Text := '';
   Tag2_Genre.ItemIndex := Tag1_Genre.ItemIndex - 1;
   Tag2_Comment.Text := Tag1_Comment.Text;
   btnCopyTo.Enabled := true;
end;

procedure TMPEGFileInfoForm.btnUndoClick(Sender: TObject);
begin
   if not IsAACtag then
      FillByFileContents
   else
      FillByFileContents_AAC;
end;

procedure TMPEGFileInfoForm.btnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TMPEGFileInfoForm.btnUpdateClick(Sender: TObject);

 procedure SetID3v1Tag(ID3v1 : TID3v1);
 var
   i : integer;
 begin
      try
         ID3v1.Track := StrToInt(Tag1_TrackNo.Text)
      except
         ID3v1.Track := 0;
      end;

      ID3v1.Title := Tag1_Title.Text;
      ID3v1.Artist := Tag1_Artist.Text;
      ID3v1.Album := Tag1_Album.Text;
      ID3v1.Year := Tag1_Year.Text;

      ID3v1.GenreID := MAX_MUSIC_GENRES;
      if Tag1_Genre.ItemIndex > 0 then
      begin
        for i := 0 to (MAX_MUSIC_GENRES - 1) do
           if Tag1_Genre.Text = aTAG_MusicGenre[i] then
           begin
              ID3v1.GenreID := i;
              break;
           end;
      end;

      ID3v1.Comment := Tag1_Comment.Text;
      ID3v1.SaveToFile(FileName);
 end;

 procedure SetID3v2Tag(ID3v2 : TID3v2);
 begin
      try
         ID3v2.Track := StrToInt(Tag2_TrackNo.Text);
      except
         ID3v2.Track := 0;
      end;

      ID3v2.Title := Tag2_Title.Text;
      ID3v2.Artist := Tag2_Artist.Text;
      ID3v2.Album := Tag2_Album.Text;
      ID3v2.Year := Tag2_Year.Text;
      ID3v2.Genre := Tag2_Genre.Text;
      ID3v2.Comment := Tag2_Comment.Text;
      ID3v2.Composer := Tag2_Composer.Text;
    //  ID3v2.OrigArtist := Tag2_OrigArtist.Text;
      ID3v2.Copyright := Tag2_Copyright.Text;
      ID3v2.Link := Tag2_URL.Text;
      ID3v2.Encoder := Tag2_Encodedby.Text;
      if not ID3v2.SaveToFile(FileName) then
         if HasID3v2Tag then
            ShowMessage('Can''t reconstruct ID3v2 tag of an opened MPEG file.')
         else
            ShowMessage('Can''t add ID3v2 tag to an opened MPEG file.')
 end;

begin
   if cbID3v1Tag.Checked then
   begin
      if (not IsAACtag) then
         SetID3V1Tag(MPEG_.ID3v1)
      else
         SetID3v1Tag(AAC_.ID3v1);
   end else
      if HasID3v1Tag then
         if (not IsAACtag) then
            MPEG_.ID3v1.RemoveFromFile(FileName)
         else
            AAC_.ID3v1.RemoveFromFile(FileName);

   if cbID3v2Tag.Checked then
   begin
      if (not IsAACtag) then
         SetID3v2Tag(MPEG_.ID3v2)
      else
         SetID3v2Tag(AAC_.ID3v2);
   end else
      if HasID3v2Tag then
         if (not IsAACtag) then
         begin
            if not MPEG_.ID3v2.RemoveFromFile(FileName) then
               ShowMessage('Can''t remove ID3v2 tag of an opened MPEG file.');
         end else
            if not AAC_.ID3v2.RemoveFromFile(FileName) then
               ShowMessage('Can''t remove ID3v2 tag of an opened MPEG file.');
   Close;
end;

procedure TMPEGFileInfoForm.Tag1_GenreKeyPress(Sender: TObject;
  var Key: Char);
begin
   Key := #0;
end;

end.
