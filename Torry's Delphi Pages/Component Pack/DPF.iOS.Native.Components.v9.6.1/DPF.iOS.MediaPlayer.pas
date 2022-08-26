// ------------------------------------------------------------------------------
// DPF.iOS.MediaPlayer Wrapped Classes & Interfaces
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.MediaPlayer;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
{$IFDEF IOS}
  iOSapi.UIKit,
  iOSapi.AVFoundation,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.QuartzCore,
  iOSapi.CoreLocation,
  iOSapi.MediaPlayer,
  iOSapi.CoreMedia,

  Macapi.ObjCRuntime, Macapi.CoreFoundation,
  iOSapi.CoreImage, iOSapi.CoreData,
{$ENDIF}
  DPF.iOS.CacheManager,
  DPF.iOS.Common,
  DPF.iOS.Dispatch,
  System.TypInfo;

const
  kCMTimeFlags_Valid = 1 shl 0;

  libMediaPlayer = '/System/Library/Frameworks/MediaPlayer.framework/MediaPlayer';

{$IFDEF IOS}


  // ---------------------------------------------------------------------------------------------

procedure SetNowPlayingInfo( Title: string; AlbumTitle: string; Artist: string; Artwork: string; PlaybackDuration: NSTimeInterval; PlaybackTime: Int64 );
function CMTIME_IS_VALID( time: CMTime ): boolean;

// ----------------------------------------------------------------------------
// General Media Item Property Keys
function MPMediaItemPropertyTitle: NSString;
function MPMediaItemPropertyAlbumTitle: NSString;
function MPMediaItemPropertyArtist: NSString;
function MPMediaItemPropertyAlbumArtist: NSString;
function MPMediaItemPropertyGenre: NSString;
function MPMediaItemPropertyComposer: NSString;
function MPMediaItemPropertyPlaybackDuration: NSString;
function MPNowPlayingInfoPropertyPlaybackRate: NSString;
function MPNowPlayingInfoPropertyElapsedPlaybackTime: NSString;
function MPMediaItemPropertyAlbumTrackNumber: NSString;
function MPMediaItemPropertyAlbumTrackCount: NSString;
function MPMediaItemPropertyDiscNumber: NSString;
function MPMediaItemPropertyDiscCount: NSString;
function MPMediaItemPropertyArtwork: NSString;
function MPMediaItemPropertyLyrics: NSString;
function MPMediaItemPropertyReleaseDate: NSString;
function MPMediaItemPropertyBeatsPerMinute: NSString;
function MPMediaItemPropertyComments: NSString;
function MPMediaItemPropertyAssetURL: NSString;

{$ENDIF}

// ------------------------------------------------------------------------------
implementation

{$IFDEF IOS}

var
  nowPlayingInfoQueue: dispatch_queue_t = 0;

  // ----------------------------------------------------------------------------
  // General Media Item Property Keys
function MPMediaItemPropertyTitle: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyTitle' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyAlbumTitle: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyAlbumTitle' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyArtist: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyArtist' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyAlbumArtist: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyAlbumArtist' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyGenre: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyGenre' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyComposer: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyComposer' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyPlaybackDuration: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyPlaybackDuration' );
end;

// ------------------------------------------------------------------------------
function MPNowPlayingInfoPropertyPlaybackRate: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPNowPlayingInfoPropertyPlaybackRate' );
end;

// ------------------------------------------------------------------------------
function MPNowPlayingInfoPropertyElapsedPlaybackTime: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPNowPlayingInfoPropertyElapsedPlaybackTime' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyAlbumTrackNumber: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyAlbumTrackNumber' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyAlbumTrackCount: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyAlbumTrackCount' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyDiscNumber: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyDiscNumber' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyDiscCount: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyDiscCount' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyArtwork: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyArtwork' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyLyrics: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyLyrics' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyReleaseDate: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyReleaseDate' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyBeatsPerMinute: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyBeatsPerMinute' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyComments: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyComments' );
end;

// ------------------------------------------------------------------------------
function MPMediaItemPropertyAssetURL: NSString;
begin
  result := CocoaNSStringConst( libMediaPlayer, 'MPMediaItemPropertyAssetURL' );
end;

// ------------------------------------------------------------------------------
function CMTIME_IS_VALID( time: CMTime ): boolean;
begin
  result := ( time.flags and kCMTimeFlags_Valid ) <> 0;
end;

// ------------------------------------------------------------------------------
procedure SetNowPlayingInfo( Title: string; AlbumTitle: string; Artist: string; Artwork: string; PlaybackDuration: NSTimeInterval; PlaybackTime: Int64 );
var
  info : NSMutableDictionary;
  url  : NSURL;
  NData: NSData;
  img  : UIImage;
  mArt : MPMediaItemArtwork;
  found: boolean;
  NSD  : NSData;
begin
  if nowPlayingInfoQueue = 0 then
    nowPlayingInfoQueue := dispatch_queue_create( 'NowPlayingInfo Queue', 0 );

  dispatch_sync( nowPlayingInfoQueue,
    procedure
    begin
      info := TNSMutableDictionary.Wrap( TNSMutableDictionary.Alloc.init );

      // Title
      if Title <> '' then
        info.setObject( ( NSStr( Title ) as ILocalObject ).GetObjectID, ( MPMediaItemPropertyTitle as ILocalObject ).GetObjectID );

      // AlbumTitle
      if AlbumTitle <> '' then
        info.setObject( ( NSStr( AlbumTitle ) as ILocalObject ).GetObjectID, ( MPMediaItemPropertyAlbumTitle as ILocalObject ).GetObjectID );

      // Artist
      if Artist <> '' then
        info.setObject( ( NSStr( Artist ) as ILocalObject ).GetObjectID, ( MPMediaItemPropertyArtist as ILocalObject ).GetObjectID );

      // Artwork
      if Artwork <> '' then
      begin
        NSD := GetCachedData( Artwork );
        if NSD <> nil then
          Img := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NSD ) )
        else
          Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( Artwork ) ) );
        found := Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 );
        if not found then
        begin
          url := GetNSURL( Artwork );
          NData := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( url ) );
          Img := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NData ) );
          found := Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 );
        end;

        if found then
        begin
          mArt := TMPMediaItemArtwork.Wrap( TMPMediaItemArtwork.Alloc.initWithImage( img ) );
          info.setObject( ( mArt as ILocalObject ).GetObjectID, ( MPMediaItemPropertyArtwork as ILocalObject ).GetObjectID );
          mArt.release;
        end;
      end;

      // PlaybackDuration
      info.setObject( TNSNumber.OCClass.numberWithDouble( PlaybackDuration ), ( MPMediaItemPropertyPlaybackDuration as ILocalObject ).GetObjectID );

      // PlaybackTime
      info.setObject( TNSNumber.OCClass.numberWithDouble( PlaybackTime ), ( MPNowPlayingInfoPropertyElapsedPlaybackTime as ILocalObject ).GetObjectID );

      // PlaybackRate
      info.setObject( TNSNumber.OCClass.numberWithInt( 1 ), ( MPNowPlayingInfoPropertyPlaybackRate as ILocalObject ).GetObjectID );

      TMPNowPlayingInfoCenter.Wrap( TMPNowPlayingInfoCenter.OCClass.defaultCenter ).setNowPlayingInfo( info );
      info.release;
    end );
end;

// ------------------------------------------------------------------------------
initialization

finalization

if nowPlayingInfoQueue > 0 then
  dispatch_release( nowPlayingInfoQueue );

{$ENDIF}

end.
