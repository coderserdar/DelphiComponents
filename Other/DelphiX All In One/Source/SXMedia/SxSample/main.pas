{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
 }
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SXMovie, SXModPlayer, SXEngine, Menus, DXSounds, DXDraws, MMSystem, DIB;

type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    FileMnu: TMenuItem;
    SXShowMnu: TMenuItem;
    SXEngineMnu: TMenuItem;
    SXModPlayerMnu: TMenuItem;
    Exit1: TMenuItem;
    OpenFileItem: TMenuItem;
    PlayItem: TMenuItem;
    StopItm: TMenuItem;
    StartItem: TMenuItem;
    StopItem: TMenuItem;
    OpenFile1: TMenuItem;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    OpenDlg: TOpenDialog;
    SXModPlayer: TSXModPlayer;
    SXEngine: TSXEngine;
    SXMovie: TSXMovie;
    DXSound1: TDXSound;
    DXDraw1: TDXDraw;
    DXImageList1: TDXImageList;
    procedure Exit1Click(Sender: TObject);
    procedure DXDraw1Finalize(Sender: TObject);
    procedure OpenFileItemClick(Sender: TObject);
    procedure OpenFile1Click(Sender: TObject);
    procedure PlayItemClick(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure DXSound1Initialize(Sender: TObject);
    procedure SXEngineRender(Sender: TObject);
    procedure StartItemClick(Sender: TObject);
    procedure StopItemClick(Sender: TObject);
    procedure DXDraw1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.DFM}
procedure FreeObj(var Obj);
begin
  if TObject(Obj) <> nil then
  begin
     TObject(Obj).Free;
     TObject(Obj) := nil;
  end;
end;

procedure ClearSurfaces;
begin
  FrmMain.DXDraw1.Surface.Fill(clBlack);
  FrmMain.DXDraw1.Primary.Fill(clBlack);
end;

procedure TFrmMain.Exit1Click(Sender: TObject);
begin
   if SXMovie.Playing then SXMovie.Stop;
   Close;
end;

procedure TFrmMain.DXDraw1Finalize(Sender: TObject);
begin
  if SXEngine.Enabled then SXEngine.Enabled := False;
end;

procedure TFrmMain.OpenFileItemClick(Sender: TObject);
begin
  OpenDlg.Filter := 'All Media Files|*.avi;*.mpg;*.mov|' +
                     'AVI (*.avi)|*.avi|MPG (*.mpg)|*.mpg|MOV (*.mov)|*.mov';
  if OpenDlg.Execute then
  begin
     SXMovie.Filename := OpenDlg.FileName;
  end;
end;

procedure TFrmMain.OpenFile1Click(Sender: TObject);
begin
  OpenDlg.Filter := 'All Media Files|*.mod;*.it;*.s3m;*.xm|' +
                     'Mod (*.mod)|*.mod|Impulse Tracker (*.it)|*.it|Scream Tracker (*.s3m)|*.s3m|Fast Tracker (*.xm)|*.xm';
  if OpenDlg.Execute then
  begin
     SXModPlayer.Filename := OpenDlg.FileName;
     DXSound1Initialize(Sender);
     SXModPlayer.Initialize(nil);
  end;
end;

procedure TFrmMain.PlayItemClick(Sender: TObject);
begin
  if not SXMovie.Playing then
  begin
     if DXDraw1.CanDraw then
       ClearSurfaces;
     SXMovie.DisplayRect(80,60, 400, 300);
     SXMovie.Play
  end
  else
     SXMovie.Stop;
end;


procedure TFrmMain.Start1Click(Sender: TObject);
begin
   SXModPlayer.Play(True);
end;

procedure TFrmMain.Stop1Click(Sender: TObject);
begin
  SXModPlayer.Stop;
end;

procedure TFrmMain.DXSound1Initialize(Sender: TObject);
var fmt:TWaveFormatEx;
begin
  with Fmt do
  begin
     wFormatTag := WAVE_FORMAT_PCM;
     nSamplesPerSec := 44100;
     nChannels := 2;
     wBitsPerSample := 16;
     nBlockAlign := wBitsPerSample div 8 * nChannels;
     nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
  end;
  // must be in exclusive to set the format
  DXSound1.Options := DXSound1.Options + [soExclusive]; 
  DXSound1.Primary.SetFormat(Fmt);
end;

procedure TFrmMain.SXEngineRender(Sender: TObject);
var Angle: Integer;
begin
  Angle := 0;
  if DXDraw1.CanDraw then
  begin
    DXDraw1.Surface.Fill(clBlack);
    with DXImageList1.Items[0] do
      DrawWaveX(DXDraw1.Surface,160,50,Width,Height, 0, 5, 80, Angle*4);
    with DXDraw1.Surface.Canvas do
    begin
      try
        Brush.Style := bsClear;
        Font.Color := clWhite;
        Font.Size := 8;
        Textout(5, 5, 'Frames Per Sec  :   ' + IntToStr(SXEngine.FramesPerSecond));
      finally
        Release;
      end;
    end;
    DXDraw1.Flip;
    Inc(Angle);
  end;
end;

procedure TFrmMain.StartItemClick(Sender: TObject);
begin
   if DXDraw1.CanDraw then ClearSurfaces;
   SXEngine.Enabled := True;
end;

procedure TFrmMain.StopItemClick(Sender: TObject);
begin
  SXEngine.Enabled := False;
end;

procedure TFrmMain.DXDraw1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_ESCAPE then Close;
end;

end.
