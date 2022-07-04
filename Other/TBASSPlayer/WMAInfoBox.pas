
  {
  MetaData Example
    Loading & Editing Wma File Tags

    Available Fields:
    ================

    Encoder, Genre, Author, Title, Album, Composer,
    Url, Track, Year, CopyRight, Comments, Lyrics.

    Read Only Fields: Version, Duration, BitRate, FileSize, VBR.

    (C) 2004 Copyright Philip Hadar - Israel
        Philip@EverX.co.il
        WWW.EverX.co.il

   modifications by Chris Trösken
   Contact : cst-tech@foni.net

}



// This unit is renamed and modified just a little to adapt to TBASSPlayer by Silhwan Hyun

unit WMAInfoBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, filectrl, WMAReader, WMAfile;



type
  TWMAInfoForm = class(TForm)
    Label9: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    fArtist: TEdit;
    fTitle: TEdit;
    fAlbum: TEdit;
    fYear: TEdit;
    fTrack: TEdit;
    fComments: TMemo;
    fGenre: TEdit;
    btnUpdate: TButton;
    btnCancel: TButton;
    fLyrics: TMemo;
    fEncoder: TEdit;
    fComposer: TEdit;
    fUrl: TEdit;
    fCopyright: TEdit;
    Label18: TLabel;
    Label20: TLabel;
    GroupBox3: TGroupBox;
    lbSize: TLabel;
    lbLength: TLabel;
    lbBitrate: TLabel;
    lbChannels: TLabel;
    lbVBR: TLabel;
    lbSamplingrate: TLabel;
    lbVersion: TLabel;
    Label4: TLabel;
    fFileName: TLabel;
    procedure ClearAll;
    procedure btnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
  //  function DurationToStr(Duration: integer; ShowMs: boolean): string;

    { Private declarations }
  public
    { Public declarations }
    procedure SetAttributes(StreamName : string; WMA_Info : TWMAFile);
  end;

{var
  WMAInfoForm: TWMAInfoForm; }


implementation

{$R *.DFM}

var
   WMA_ : TWMAfile;
   FileName : string;

procedure TWMAInfoForm.SetAttributes(StreamName : string; WMA_Info : TWMAFile);
begin
   FileName := StreamName;
   WMA_ := WMA_Info;
end;

procedure TWMAInfoForm.ClearAll;
var
  I: Integer;
begin

  for I := 0 to ComponentCount - 1 do
    if Components[I] is TEdit then
      (Components[I] as TEdit).Text := ''
    else if Components[I] is TMemo then
      (Components[I] as TMemo).Text := '';

end;

procedure TWMAInfoForm.btnUpdateClick(Sender: TObject);
var
  TagsData: PTagsData;
begin

  New(TagsData);

  Try

  TagsData.Encoder := fEncoder.Text;
  TagsData.Genre := fGenre.Text;
  TagsData.Author := fArtist.Text;
  TagsData.Composer := fComposer.Text;
  TagsData.Title := fTitle.Text;
  TagsData.Album := fAlbum.Text;
  TagsData.Year := fYear.Text;
  TagsData.Track := fTrack.Text;
  TagsData.Url := fUrl.Text;
  TagsData.Comments := fComments.Text;
  TagsData.Lyrics := fLyrics.Text;
  TagsData.CopyRight := fCopyright.Text;

  SaveWMAFile(TagsData);

  Finally
  Dispose(TagsData);
  End;

  Close;
end;

{ function TWMAInfoForm.DurationToStr(Duration: integer; ShowMs: boolean): string;
begin
  if ShowMs then
    Result := Format('%d:%2.2d,%3.3d', [(Abs(Duration div 60)), Abs(Duration mod 60), Abs(Duration mod 1000)])
  else
    Result := Format('%d:%2.2d', [(Abs(Duration div 60)), Abs(Duration mod 60)]);
end; }


procedure TWMAInfoForm.FormShow(Sender: TObject);
var
  TagsData: PTagsData;
 // TimeStr : string;
begin
  ClearAll;
  fFileName.Caption := MinimizeName(FileName, fFileName.Canvas, fFileName.Width);

  lbSamplingrate.Caption := 'Samplingrate: ' + intToStr(WMA_.SampleRate) + ' Hz';
  lbChannels.Caption := 'Channels: ' + intToStr(WMA_.ChannelModeID);

  btnUpdate.Enabled := True;

  New(TagsData);
  try
    OpenWMAFile(FileName, TagsData);
    lbSize.Caption := 'File Size: ' + FormatFloat('#,# bytes', TagsData.FileSize);

   // TimeStr := FormatDateTime('nn:ss', TagsData.Duration / (24 * 60 * 60));
    lbLength.Caption := 'Length: ' + DurationToStr(TagsData.Duration, true{= show ms}) + ' sec';
   { if TimeStr[1] = '0' then
       lbLength.Caption := 'Length: ' + copy(TimeStr, 2, 4)
    else
       lbLength.Caption := 'Length: ' + TimeStr;
  //  lbLength.Caption := 'Length: ' + DurationToStr(TagsData.Duration, true) + ' Sec'; }

    lbBitrate.Caption := 'Bitrate: ' + IntToStr(TagsData.BitRate div 1000) + ' KBPS';
    if TagsData.VBR then
      lbVBR.Caption := 'VBR: Yes'
    else
      lbVbr.Caption := 'VBR: No';
    lbVersion.Caption := 'WMFSDK Version: ' + TagsData.Version;

    fEncoder.Text := TagsData.Encoder;
    fGenre.Text := TagsData.Genre;
    fArtist.Text := TagsData.Author;
    fComposer.Text := TagsData.Composer;
    fTitle.Text := TagsData.Title;
    fAlbum.Text := TagsData.Album;
    fYear.Text := TagsData.Year;
    fTrack.Text := TagsData.Track;
    fUrl.Text := TagsData.Url;
    fComments.Text := TagsData.Comments;
    fLyrics.Text := TagsData.Lyrics;
    fCopyright.Text := TagsData.CopyRight;

  finally
    Dispose(TagsData);
  end;

end;

procedure TWMAInfoForm.btnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TWMAInfoForm.FormCreate(Sender: TObject);
begin
 //  WMA_ := TWMAfile.Create;
end;

procedure TWMAInfoForm.FormDestroy(Sender: TObject);
begin
 //  WMA_.Free;
end;

end.

