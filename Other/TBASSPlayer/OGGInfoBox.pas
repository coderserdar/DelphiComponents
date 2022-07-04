// unit OGGInfoBox
//
//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.0                        31 Mar 2005
//   - Initial release
//
// Ver 1.01                       6 Nov 2006
//  Changed property "Position" of TMPEGFileInfoForm to poDesktopCenter.
//

unit OGGInfoBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OGGVorbis, TAGEdit, filectrl;

type
  TOggVorbisInfoForm = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Genre: TLabel;
    Tag_TrackNo: TEdit;
    Tag_Title: TEdit;
    Tag_Artist: TEdit;
    Tag_Album: TEdit;
    Tag_Date: TEdit;
    Tag_Genre: TComboBox;
    Tag_Comment: TMemo;
    GroupBox3: TGroupBox;
    lbFileSize: TLabel;
    lbVendor2: TLabel;
    lbLength: TLabel;
    lbNominalBitrate: TLabel;
    lbAverageBitrate: TLabel;
    lbChannels: TLabel;
    lbSamplingRate: TLabel;
    lbSerialNo: TLabel;
    lbVersion: TLabel;
    lbVendor1: TLabel;
    lbVendor: TLabel;
    GroupBox2: TGroupBox;
    ExtraTagList: TListBox;
    Label9: TLabel;
    btnAddNew: TButton;
    btnDelete: TButton;
    btnDeleteAll: TButton;
    btnUpdate: TButton;
    btnCancel: TButton;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    fFileName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnAddNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDeleteAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetAttributes(StreamName : string; OGG_Info : TOGGVorbis);
    procedure ClearTagFields;
    procedure FillByFileContents;
  end;

{var
  OggVorbisInfoForm: TOggVorbisInfoForm; }

implementation

{$R *.DFM}

const
   PREDEFINED_NUM = 9;
   PREDEFINED_FIELD: array [1..PREDEFINED_NUM] of string =
    ('TITLE', 'ARTIST', 'ALBUM', 'TRACKNUMBER', 'DATE', 'GENRE', 'COMMENT',
     'PERFORMER', 'DESCRIPTION');
var
   TAGEditForm : TTAGEditForm;
   OGG_ : TOGGVorbis;
   FileName : string;

procedure TOggVorbisInfoForm.SetAttributes(StreamName : string; OGG_Info : TOGGVorbis);
begin
   FileName := StreamName;
   OGG_ := OGG_Info;
end;

procedure TOggVorbisInfoForm.FormCreate(Sender: TObject);
begin
   TAGEditForm := TTAGEditForm.Create(Self);
 //  OGG_ := TOggVorbis.Create;
end;

procedure TOggVorbisInfoForm.FormDestroy(Sender: TObject);
begin
   TAGEditForm.Free;
 //  OGG_.Free;
end;

procedure TOggVorbisInfoForm.ClearTagFields;
begin
   Tag_TrackNo.Text := '';
   Tag_Title.Text := '';
   Tag_Artist.Text := '';
   Tag_Album.Text := '';
   Tag_Date.Text := '';
   Tag_Genre.Text := '';
   Tag_Comment.Text := '';
   ExtraTagList.Clear;
end;

procedure TOggVorbisInfoForm.FillByFileContents;
var
 //  rSize : real;
   TimeStr : string[5];
   tmpStr, TagItem : string;
   SpacePos, SpacePos2, SeperatorPos : integer;
begin
   fFileName.Caption := MinimizeName(FileName, fFileName.Canvas, fFileName.Width);
 {  OGG_.ReadFromFile(FileName);
   if OGG_.Valid then
   begin }
      TimeStr := FormatDateTime('nn:ss', OGG_.Duration / (24 * 60 * 60));
      if TimeStr[1] = '0' then
         lbLength.Caption := 'Length: ' + copy(TimeStr, 2, 4)
      else
         lbLength.Caption := 'Length: ' + TimeStr;
      lbAverageBitrate.Caption := 'Average bitrate: ' + intToStr(OGG_.BitRate) + ' kbps';
   //   rSize := OGG_.FileSize;
   //   lbFileSize.Caption := 'File size: ' + TrimLeft(Format('%12.0n', [rSize])) + ' bytes';
      lbFileSize.Caption := 'File size: ' + FormatFloat('#,# bytes', OGG_.FileSize);
      lbNominalBitrate.Caption := 'Nominal bitrate: ' + intToStr(OGG_.BitRateNominal) + ' kbps';
      lbChannels.Caption := 'Channels: ' + intToStr(OGG_.ChannelModeID);
      lbSamplingRate.Caption := 'Sampling rate: ' + intToStr(OGG_.SampleRate) + ' Hz';
      lbSerialNo.Caption := 'Serial number: ' + intToStr(OGG_.SerialNumber);
      lbVersion.Caption := 'Version: ' + intToStr(OGG_.StreamVersion);
      if length(OGG_.Vendor) < 25 then
      begin
         lbVendor1.Caption := OGG_.Vendor;
         lbVendor1.Caption := '';
      end else
      begin
         SpacePos := pos(' ', OGG_.Vendor);
         if SpacePos < 25 then
         begin
            SpacePos2 := 0;
            repeat
               SpacePos := SpacePos + SpacePos2;
               tmpStr := copy(OGG_.Vendor, SpacePos+1, 50);
               SpacePos2 := pos(' ', tmpStr);
            until (SpacePos2 = 0) or (SpacePos + SpacePos2 > 25);
         end;

         lbVendor1.Caption := copy(OGG_.Vendor, 1, SpacePos-1);
         lbVendor2.Caption := copy(OGG_.Vendor, SpacePos+1, 25);
      end;

      ClearTagFields;
      Tag_TrackNo.Text := intToStr(OGG_.Track);
      Tag_Title.Text := OGG_.Title;
      Tag_Artist.Text := OGG_.Artist;
      Tag_Album.Text := OGG_.Album;
      Tag_Date.Text := OGG_.Date;
      Tag_Genre.Text := OGG_.Genre;
      Tag_Comment.Text := OGG_.Comment;
      if OGG_.ExtraTag <> '' then
      begin
         tmpStr := OGG_.ExtraTag;
         repeat
            SeperatorPos := pos(AnsiChar(0), tmpStr);
            if SeperatorPos > 0 then
            begin
               TagItem := copy(tmpStr, 1, SeperatorPos - 1);
               tmpStr := copy(tmpStr, SeperatorPos + 1, length(tmpStr) - SeperatorPos);
            end else
               TagItem := tmpStr;

            ExtraTagList.Items.Add(TagItem);
         until (SeperatorPos = 0);
      end;
  // end;
end;

procedure TOggVorbisInfoForm.FormShow(Sender: TObject);
begin
   FillByFileContents;
end;

procedure TOggVorbisInfoForm.btnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TOggVorbisInfoForm.btnUpdateClick(Sender: TObject);
var
   i : integer;
begin
   try
      OGG_.Track := StrToInt(TAG_TrackNo.Text);
   except
      OGG_.Track := 0;
   end;

   OGG_.Title := Tag_Title.Text;
   OGG_.Artist := Tag_Artist.Text;
   OGG_.Album := Tag_Album.Text;
   OGG_.Date := Tag_Date.Text;
   OGG_.Genre := Tag_Genre.Text;
   OGG_.Comment := Tag_Comment.Text;
   OGG_.ExtraTag := '';
   if ExtraTagList.Items.Count > 0 then
      for i := 0 to (ExtraTagList.Items.Count - 1) do
         if OGG_.ExtraTag = '' then
            OGG_.ExtraTag := ExtraTagList.Items[i]
         else
            OGG_.ExtraTag := OGG_.ExtraTag + AnsiChar(0) + ExtraTagList.Items[i];

   if not OGG_.SaveTag(FileName) then
      ShowMessage('Can''t update the tag of an opened OGG file.');
   Close;
end;

procedure TOggVorbisInfoForm.btnAddNewClick(Sender: TObject);
var
   i, EqualPos : integer;
   TAGID, tmpStr : string;
begin
   if TAGEditForm.ShowModal = mrOK then
   begin
      if (trim(TAGEditForm.TAGName.Text) = '')
         or (trim(TAGEditForm.TAGMemo.Text) = '') then
           exit;

      TAGID := UPPERCASE(TAGEditForm.TAGName.Text);
      for i := 1 to PREDEFINED_NUM do
         if TAGID = PREDEFINED_FIELD[i] then
         begin
            Application.MessageBox('Can''t add predefined TAG ID.', 'Error',
                                            MB_OK + MB_ICONERROR);
            exit;
         end;

      for i := 0 to (ExtraTagList.Items.Count - 1) do
      begin
         EqualPos := pos('=', ExtraTagList.Items[i]);
         tmpStr := copy(ExtraTagList.Items[i], 1, EqualPos - 1);
         if TAGID = tmpStr then
         begin
            Application.MessageBox('Duplicate TAG ID.', 'Error',
                                                        MB_OK + MB_ICONERROR);
            exit;
         end;
      end;

      ExtraTagList.Items.Add(TAGID + '=' + TAGEditForm.TAGMemo.Text);
   end;
end;

procedure TOggVorbisInfoForm.btnDeleteClick(Sender: TObject);
begin
   if ExtraTagList.ItemIndex < 0 then
      exit;

   ExtraTagList.Items.Delete(ExtraTagList.ItemIndex);
end;

procedure TOggVorbisInfoForm.btnDeleteAllClick(Sender: TObject);
begin
   ExtraTagList.Clear;
end;

end.
