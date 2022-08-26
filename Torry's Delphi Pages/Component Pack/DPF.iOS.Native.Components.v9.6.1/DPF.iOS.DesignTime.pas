// ------------------------------------------------------------------------------
// DPF.iOS.DesignTime helper functions
//
// Developed By: Fenistil
//
// Email: fenistil@hu.hu
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

unit DPF.iOS.DesignTime;

interface

{$I DPF.iOS.Defs.inc}

uses System.Types, System.UITypes, System.Classes, System.Math,
       {$IFDEF DELPHIXE5} FMX.Graphics,{$ENDIF}FMX.Types, FMX.Controls,
     DPF.iOS.BaseControl;

{$R DPF.iOS.DesignTime.res}

{$IFNDEF IOS}
type
  TUniversalToolBarItem = record
                            ButtonItemKind:TDPFBarButtonKind;
                            ButtonItemStyle:TDPFBarButtonItemStyle;
                            ButtonSystemItem:TDPFBarButtonSystemItem;
                            Title:string;
                            Width:single;
                          end;

  TArrayOfUniversalToolBarItem = array of TUniversalToolBarItem;

  TSegmentedControl_Bitmaps_30px = record
                                     ButtonBG_Down,
                                     ButtonBG_Up,
                                     Left_Down,
                                     Left_Up,
                                     Right_Down,
                                     Right_Up,
                                     Separator_Down,
                                     Separator_Up:TBitmap;
                                   end;

  TSegmentedControl_Bitmaps_44px = record
                                     ButtonBG_Down,
                                     ButtonBG_Up,
                                     Left_Down,
                                     Left_Up,
                                     Right_Down,
                                     Right_Up,
                                     Separator_DownLeft,
                                     Separator_DownRight,
                                     Separator_Down,
                                     Separator_Up:TBitmap;
                                   end;
  TPickerView_Bitmaps = record
                          FrameLeft,
                          FrameMiddle,
                          FrameRight,
                          SelectorLeft,
                          SelectorBG,
                          SelectorRight,
                          SelectorSeparator,
                          Separator,
                          WheelBG:TBitmap;
                        end;

var iOS_GUI_Bitmaps:record
                      ActivityIndicator:record
                                          Gray,
                                          White,
                                          WhiteLarge:TBitmap;
                                        end;
                      Button:record
                               RoundedRect,
                               DetailDisclosure,
                               InfoLight,
                               InfoDark,
                               ContactAdd:TBitmap;
                             end;
                      DatePicker:record
                                   CountDown,
                                   DateAndTime,
                                   Date,
                                   Time:TBitmap;
                                 end;
                      MapView:record
                                TrackingDot,
                                TrackingDotHalo:TBitmap;
                              end;
                      NavigationController:record
                                             ButtonLeftBack,
                                             ButtonLeft,
                                             ButtonBG,
                                             ButtonRight,
                                             LargeBG,
                                             SmallBG:TBitmap;
                                           end;
                      PickerView:record
                                   Size162,
                                   Size180,
                                   Size216:TPickerView_Bitmaps;
                                 end;
                      ProgressView:record
                                     Left,
                                     BG,
                                     Right,
                                     Fill_Left,
                                     Fill_BG,
                                     Fill_Right:TBitmap;
                                   end;
                      SearchBar:record
                                  BG,
                                  Bookmarks,
                                  Field_BG,
                                  Field_Left,
                                  Field_Right,
                                  Results,
                                  Scope_BG_Down,
                                  Scope_BG,
                                  Scope_BG_Up,
                                  Scope_Left_Down,
                                  Scope_Left_Up,
                                  Scope_Right_Down,
                                  Scope_Right_Up,
                                  Scope_Separator_Down,
                                  Scope_Separator_Up,
                                  SearchIcon:TBitmap;
                                end;
                      SegmentedControl:record
                                         Bar, Bezeled:TSegmentedControl_Bitmaps_30px;
                                         Bordered, Plain:TSegmentedControl_Bitmaps_44px;
                                       end;
                      Slider:record
                               Tab,
                               BarOff,
                               BarOn:TBitmap;
                             end;
                      Stepper:TBitmap;
                      Switch:record
                               IsOn,
                               IsOff:TBitmap;
                             end;
                      TabBar:record
                               BG,
                               BadgeLeft,
                               BadgeBG,
                               BadgeRight,
                               Bookmark_Off,
                               Bookmark_On,
                               Contacts_Off,
                               Contacts_On,
                               Downloads_Off,
                               Downloads_On,
                               Favorites_Off,
                               Favorites_On,
                               Featured_Off,
                               Featured_On,
                               History_Off,
                               History_On,
                               More_Off,
                               More_On,
                               MostRecent_Off,
                               MostRecent_On,
                               MostViewed_Off,
                               MostViewed_On,
                               Search_Off,
                               Search_On,
                               SelectionLeft,
                               SelectionBG,
                               SelectionRight:TBitmap;
                             end;
                      TableView:record
                                  Grouped_ItemBorder,
                                  Item_Check,
                                  Item_Delete,
                                  Item_Grabber,
                                  Item_NextButton,
                                  Item_NextSelection,
                                  Plain_HeaderBG:TBitmap;
                                end;
                      TextField:record
                                  Bezel,
                                  Line,
                                  RoundedRect:TBitmap;
                                end;
                      ToolBar:record
                                BG,
                                DoneLeft,
                                DoneBG,
                                DoneRight,
                                ButtonLeft,
                                ButtonBG,
                                ButtonRight:TBitmap;
                                LargeIcons, SmallIcons:record
                                                         Action,
                                                         Add,
                                                         Bookmarks,
                                                         Cancel,
                                                         Camera,
                                                         Compose,
                                                         Done,
                                                         Edit,
                                                         FastForward,
                                                         Organize,
                                                         Pause,
                                                         Play,
                                                         Refresh,
                                                         Reply,
                                                         Rewind,
                                                         Save,
                                                         Search,
                                                         Stop,
                                                         Trash:TBitmap;
                                                       end;
                              end;
                      VolumeControl:record
                                      Knob:TBitmap;
                                    end;
                    end;
{$ENDIF}

procedure LoadResourceToBitmap (var ABitmap:TBitmap; ResourceName:string);
procedure BitmapAsBorder (Control:TDPFiOSBaseControl; ABitmap:TBitmap; BorderSize:integer; const FillColor:TAlphaColor=TAlphaColors.Null);
procedure BitmapAsBorderToRect (Control:TDPFiOSBaseControl; ABitmap:TBitmap; BorderSize:integer; ARect:TRectF; const FillColor:TAlphaColor=TAlphaColors.Null);
procedure BitmapAsBackground (Control:TDPFiOSBaseControl; ABitmap:TBitmap);
procedure BitmapToPosition (Control:TDPFiOSBaseControl; ABitmap:TBitmap; X, Y:single);
procedure BitmapToRect (Control:TDPFiOSBaseControl; ABitmap:TBitmap; R:TRectF);
procedure PaintCaption (Control:TDPFiOSBaseControl; Caption:string; CaptionRect:TRectF; LineBreak:TDPFLineBreak; NumberOfLines:integer; Alignment: TTextAlign);
function  GetWidthOfToolBarButton (Button:TUniversalToolBarItem; IncludeMargin:boolean):single;
procedure PaintToolBar (Control:TDPFiOSBaseControl; BarItems:TArrayOfUniversalToolBarItem);
function FMTextAlign(TextAlign:TDPFTextAlignment):TTextAlign;

implementation

procedure LoadResourceToBitmap (var ABitmap:TBitmap; ResourceName:string);
  var Stream:TResourceStream;

  begin
    if not Assigned (ABitmap) then ABitmap:=TBitmap.Create(0,0);
    Stream:=TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    ABitmap.LoadFromStream(Stream);
    Stream.Free;
  end;

procedure BitmapAsBorder (Control:TDPFiOSBaseControl; ABitmap:TBitmap; BorderSize:integer; const FillColor:TAlphaColor=TAlphaColors.Null);
begin
  BitmapAsBorderToRect(Control, ABitmap, BorderSize, Control.ClipRect, FillColor);
end;

procedure BitmapAsBorderToRect (Control:TDPFiOSBaseControl; ABitmap:TBitmap; BorderSize:integer; ARect:TRectF; const FillColor:TAlphaColor=TAlphaColors.Null);
var R:TRectF;

  begin
    //Top Left
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(0,0,BorderSize,BorderSize),
                              RectF(ARect.Left,ARect.Top,ARect.Left+BorderSize,ARect.Top+BorderSize),
                              1, true);
    //Top
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(BorderSize,0,ABitmap.Width-BorderSize,BorderSize),
                              RectF(ARect.Left+BorderSize,ARect.Top,ARect.Right-BorderSize,ARect.Top+BorderSize),
                              1, true);
    //Top Right
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(ABitmap.Width-BorderSize,0,ABitmap.Width,BorderSize),
                              RectF(ARect.Right-BorderSize,ARect.Top,ARect.Right,ARect.Top+BorderSize),
                              1, true);
    //Right
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(ABitmap.Width-BorderSize,BorderSize,ABitmap.Width,ABitmap.Height-BorderSize),
                              RectF(ARect.Right-BorderSize,ARect.Top+BorderSize,ARect.Right,ARect.Bottom-BorderSize),
                              1, true);
    //Bottom Right
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(ABitmap.Width-BorderSize,ABitmap.Height-BorderSize,ABitmap.Width,ABitmap.Height),
                              RectF(ARect.Right-BorderSize,ARect.Bottom-BorderSize,ARect.Right,ARect.Bottom),
                              1, true);
    //Bottom
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(BorderSize,ABitmap.Height-BorderSize,ABitmap.Width-BorderSize,ABitmap.Height),
                              RectF(ARect.Left+BorderSize,ARect.Bottom-BorderSize,ARect.Right-BorderSize,ARect.Bottom),
                              1, true);
    //Bottom Left
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(0,ABitmap.Height-BorderSize,BorderSize,ABitmap.Height),
                              RectF(ARect.Left,ARect.Bottom-BorderSize,ARect.Left+BorderSize,ARect.Bottom),
                              1, true);
    //Left
    Control.Canvas.DrawBitmap(ABitmap,
                              RectF(0,BorderSize,BorderSize,ABitmap.Height-BorderSize),
                              RectF(ARect.Left+0,ARect.Top+BorderSize,ARect.Left+BorderSize,ARect.Bottom-BorderSize),
                              1, true);
    if FillColor<>TAlphaColors.Null then
      begin
        R:=ARect;
        System.Types.InflateRect(R,-BorderSize,-BorderSize);
        Control.Canvas.Fill.Kind:=TBrushKind.Solid;
        Control.Canvas.Fill.Color:=FillColor;
        Control.Canvas.FillRect(R,0,0,AllCorners,1,TCornerType.Round);
      end;
  end;

procedure BitmapAsBackground (Control:TDPFiOSBaseControl; ABitmap:TBitmap);
  begin
    Control.Canvas.DrawBitmap(ABitmap, RectF(0,0,ABitmap.Width,ABitmap.Height), Control.ClipRect, 1,true);
  end;

procedure BitmapToPosition (Control:TDPFiOSBaseControl; ABitmap:TBitmap; X, Y:single);
  begin
    Control.Canvas.DrawBitmap(ABitmap, RectF(0,0,ABitmap.Width,ABitmap.Height), RectF(X,Y,X+ABitmap.Width,Y+ABitmap.Height), 1);
  end;

procedure BitmapToRect (Control:TDPFiOSBaseControl; ABitmap:TBitmap; R:TRectF);
  begin
    Control.Canvas.DrawBitmap(ABitmap, RectF(0,0,ABitmap.Width,ABitmap.Height), R, 1, true);
  end;

procedure PaintCaption (Control:TDPFiOSBaseControl; Caption:string; CaptionRect:TRectF; LineBreak:TDPFLineBreak; NumberOfLines:integer;Alignment: TTextAlign);
  var
    s, s2 : string;
    i     : integer;
    w,m   : single;
    trunc1: boolean;
//    R:TRectF;

  begin
    case LineBreak of
      lbWordWrap, lbCharacterWrap:
        begin
          // Note: CharacterWrap is not supported by FireMonkey
          Control.Canvas.FillText( CaptionRect, Caption, NumberOfLines > 1, 1, [], Alignment, TTextAlign.Center );
        end;
      lbClip:
        begin
          if NumberOfLines=0 then //No line limit
            Control.Canvas.FillText( CaptionRect, Caption, true, 1, [], Alignment, TTextAlign.Center );
          if NumberOfLines=1 then //1 line
            Control.Canvas.FillText( CaptionRect, Caption, false, 1, [], Alignment, TTextAlign.Center );
          if NumberOfLines>1 then //X lines
            begin
              w:=(Control.Canvas.TextHeight(Caption)*NumberOfLines)/2;
              m:=(CaptionRect.Top+CaptionRect.Bottom)/2;
              CaptionRect.Top:=m-w;
              CaptionRect.Bottom:=m+w;
              Control.Canvas.FillText( CaptionRect, Caption, true, 1, [], Alignment, TTextAlign.Center );
            end;
        end;
      lbHeadTruncation:
        begin
          s := Caption;
          if NumberOfLines=1 then
            begin
              w := Control.Canvas.TextWidth( s );
              if w >= CaptionRect.Width then
                begin
                  i := 1;
                  while Control.Canvas.TextWidth( '...' + copy( s, i, Length( s ) ) ) >= CaptionRect.Width do
                  begin
                    inc( i );
                    if i > Length( s ) then
                      Break;
                  end;
                  s := '...' + copy( s, i, Length( s ) );
                end;
            end;
          Control.Canvas.FillText( CaptionRect, s, NumberOfLines<>1, 1, [], Alignment, TTextAlign.Center );
        end;
      lbTailTruncation:
        begin
          s := Caption;
          if NumberOfLines=1 then
            begin
              w := Control.Canvas.TextWidth( s );
              if w >= CaptionRect.Width then
                begin
                  i := Length( s );
                  while Control.Canvas.TextWidth( copy( s, 1, i ) + '...' ) >= CaptionRect.Width do
                  begin
                    dec( i );
                    if i < 1 then
                      Break;
                  end;
                  s := copy( s, 1, i ) + '...';
                end;
            end;
          Control.Canvas.FillText( CaptionRect, s, NumberOfLines<>1, 1, [], Alignment, TTextAlign.Center );
        end;
      lbMiddleTruncation:
        begin
          s := Caption;
          if NumberOfLines=1 then
            begin
              w := Control.Canvas.TextWidth( s );
              if w >= CaptionRect.Width then
                begin
                  s      := copy( Caption, 1, Length( Caption ) div 2 );
                  s2     := copy( Caption, Length( s ) + 1, Length( Caption ) );
                  trunc1 := true;
                  while Control.Canvas.TextWidth( s + '...' + s2 ) >= CaptionRect.Width do
                  begin
                    if trunc1 then
                      s := copy( s, 1, Length( s ) - 1 )
                    else
                      s2   := copy( s2, 2, Length( s2 ) );
                    trunc1 := not trunc1;
                    if ( s = '' ) and ( s2 = '' ) then
                      Break;
                  end;
                  s := s + '...' + s2;
                end;
            end;
          Control.Canvas.FillText( CaptionRect, s, NumberOfLines<>1, 1, [], Alignment, TTextAlign.Center );
        end;
    end;
  end;

function GetWidthOfToolBarButton (Button:TUniversalToolBarItem; IncludeMargin:boolean):single;
  begin
    with Button do
    begin
      Result := 0;
      case ButtonItemKind of
        bkCustomView, bkImage, bkTitle:
          begin
            Result := Width;
            if IncludeMargin then
              Result := Result + 10;
          end;
        bkSystem:
          begin
            case ButtonItemStyle of
              bbisPlain:
                begin
                  case ButtonSystemItem of
                    bbsiDone:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Done.Width;
                    bbsiCancel:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Cancel.Width;
                    bbsiEdit:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Edit.Width;
                    bbsiSave:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Save.Width;
                    bbsiAdd:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Add.Width;
                    bbsiFixedSpace:
                      Result := Width;
                    bbsiCompose:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Compose.Width;
                    bbsiReply:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Reply.Width;
                    bbsiAction:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Action.Width;
                    bbsiOrganize:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Organize.Width;
                    bbsiBookmarks:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Bookmarks.Width;
                    bbsiSearch:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Search.Width;
                    bbsiRefresh:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Refresh.Width;
                    bbsiStop:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Stop.Width;
                    bbsiCamera:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Camera.Width;
                    bbsiTrash:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Trash.Width;
                    bbsiPlay:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Play.Width;
                    bbsiPause:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Pause.Width;
                    bbsiRewind:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Rewind.Width;
                    bbsiFastForward:
                      Result := iOS_GUI_Bitmaps.ToolBar.LargeIcons.FastForward.Width;
                  end;
                  if IncludeMargin and ( ButtonSystemItem <> bbsiFixedSpace ) and ( ButtonSystemItem <> bbsiFlexibleSpace ) then
                    Result := Result + 12;
                end;
              bbisBordered, bbisDone:
                begin
                  case ButtonSystemItem of
                    bbsiDone:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done.Width + 10;
                    bbsiCancel:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel.Width + 10;
                    bbsiEdit:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit.Width + 10;
                    bbsiSave:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save.Width + 10;
                    bbsiAdd:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add.Width + 10;
                    bbsiFixedSpace:
                      Result := Width;
                    bbsiCompose:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose.Width + 10;
                    bbsiReply:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply.Width + 10;
                    bbsiAction:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action.Width + 10;
                    bbsiOrganize:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize.Width + 10;
                    bbsiBookmarks:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks.Width + 10;
                    bbsiSearch:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search.Width + 10;
                    bbsiRefresh:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh.Width + 10;
                    bbsiStop:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop.Width + 10;
                    bbsiCamera:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera.Width + 10;
                    bbsiTrash:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash.Width + 10;
                    bbsiPlay:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play.Width + 10;
                    bbsiPause:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause.Width + 10;
                    bbsiRewind:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind.Width + 10;
                    bbsiFastForward:
                      Result := iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward.Width + 10;
                  end;
                  if IncludeMargin and ( ButtonSystemItem <> bbsiFixedSpace ) and ( ButtonSystemItem <> bbsiFlexibleSpace ) then
                    Result := Result + 10;
                end;
            end;
          end;
      end;
    end;
  end;

procedure PaintToolBar (Control:TDPFiOSBaseControl; BarItems:TArrayOfUniversalToolBarItem);
var
  i, NoS            : integer;
  x, {w, }SumWidth, fs: single;
  Bmp               : TBitmap;
  R                 : TRectF;
  CaptionRect       : TRectF;
  LineBreak         : TDPFLineBreak;

begin
  with Control do
    begin
    // Background
    BitmapAsBackground( Control, iOS_GUI_Bitmaps.ToolBar.BG );

    // Calculate Spaces
    NoS      := 0;
    SumWidth := 0;
    for i := 0 to High (BarItems) do
      if BarItems[i].ButtonSystemItem = bbsiFlexibleSpace then
        inc( NoS )
      else
        SumWidth := SumWidth + GetWidthOfToolBarButton( BarItems[i], true );
    if NoS > 0 then
      fs := Max( ( Control.Width - SumWidth ) / NoS, 0 )
    else
      fs := 0;
    // Drawing Buttons
    x     := 0;
    for i := 0 to High (BarItems) do
    begin
      // Space
      if ( BarItems[i].ButtonItemKind = bkSystem ) and ( BarItems[i].ButtonSystemItem in [bbsiFlexibleSpace, bbsiFixedSpace] ) then
      begin
        if BarItems[i].ButtonSystemItem = bbsiFlexibleSpace then
          x := x + fs;
        if BarItems[i].ButtonSystemItem = bbsiFixedSpace then
          x := x + BarItems[i].Width;
        Continue;
      end;

      // Background
      if BarItems[i].ButtonItemStyle in [bbisBordered, bbisDone] then
      begin
        x := x + 5; // Left margin
        // Left
        R := RectF( x, 8, x + 5, 38 );
        if BarItems[i].ButtonItemStyle = bbisBordered then
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.ButtonLeft, R )
        else
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.DoneLeft, R );
        // Center
        R.Left  := R.Right;
        R.Right := R.Left + GetWidthOfToolBarButton( BarItems[i], false ) - 10;
        if BarItems[i].ButtonItemStyle = bbisBordered then
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.ButtonBG, R )
        else
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.DoneBG, R );
        // Right
        R.Left  := R.Right;
        R.Right := R.Left + 5;
        if BarItems[i].ButtonItemStyle = bbisBordered then
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.ButtonRight, R )
        else
          BitmapToRect( Control, iOS_GUI_Bitmaps.ToolBar.DoneRight, R );
      end;

      // Custom / Image
      if BarItems[i].ButtonItemKind in [bkCustomView, bkImage] then
      begin
        if BarItems[i].ButtonItemStyle = bbisPlain then
        begin
          x                   := x + 5; // Left margin
          R                   := RectF( x, 8, x + BarItems[i].Width, 38 );
          Canvas.Stroke.Color := TAlphaColors.White;
          Canvas.Stroke.Kind  := TBrushKind.Solid;
          Canvas.Fill.Color   := TAlphaColors.Gray;
          Canvas.Fill.Kind    := TBrushKind.Solid;
          Canvas.FillRect( R, 0, 0, AllCorners, 1 );
        end;
        x := x + BarItems[i].Width;
      end;

      // System
      if BarItems[i].ButtonItemKind = bkSystem then
      begin
        if BarItems[i].ButtonItemStyle = bbisPlain then
        begin
          Bmp := nil;
          case BarItems[i].ButtonSystemItem of
            bbsiAdd:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Add;
            bbsiCancel:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Cancel;
            bbsiDone:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Done;
            bbsiEdit:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Edit;
            bbsiSave:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Save;
            bbsiCompose:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Compose;
            bbsiReply:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Reply;
            bbsiAction:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Action;
            bbsiOrganize:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Organize;
            bbsiBookmarks:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Bookmarks;
            bbsiSearch:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Search;
            bbsiRefresh:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Refresh;
            bbsiStop:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Stop;
            bbsiCamera:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Camera;
            bbsiTrash:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Trash;
            bbsiPlay:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Play;
            bbsiPause:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Pause;
            bbsiRewind:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.Rewind;
            bbsiFastForward:
              Bmp := iOS_GUI_Bitmaps.ToolBar.LargeIcons.FastForward;
          end;
          if Bmp <> nil then
          begin
            x := x + 6; // Left margin
            BitmapToPosition( Control, Bmp, x, 8 );
            x := x + Bmp.Width;
            x := x + 6; // Right margin
          end;
        end
        else
        begin // Bordered / Done
          Bmp := nil;
          case BarItems[i].ButtonSystemItem of
            bbsiAdd:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add;
            bbsiCancel:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel;
            bbsiDone:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done;
            bbsiEdit:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit;
            bbsiSave:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save;
            bbsiCompose:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose;
            bbsiReply:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply;
            bbsiAction:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action;
            bbsiOrganize:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize;
            bbsiBookmarks:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks;
            bbsiSearch:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search;
            bbsiRefresh:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh;
            bbsiStop:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop;
            bbsiCamera:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera;
            bbsiTrash:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash;
            bbsiPlay:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play;
            bbsiPause:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause;
            bbsiRewind:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind;
            bbsiFastForward:
              Bmp := iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward;
          end;
          if Bmp <> nil then
          begin
            x := x + 5; // Left padding
            BitmapToPosition( Control, Bmp, x, 8 );
            x := x + Bmp.Width;
            x := x + 5; // Right padding
          end;
          x := x + 5; // Right margin
        end;
      end;

      // Title
      if BarItems[i].ButtonItemKind = bkTitle then
      begin
        Canvas.Font.Style  := [TFontStyle.fsBold];
        Canvas.Font.Family := 'Helvetica';
        LineBreak          := lbMiddleTruncation;
        if BarItems[i].ButtonItemStyle = bbisPlain then
          Canvas.Font.Size   := 15
        else
          Canvas.Font.Size   := 12;
        // Shadow
        Canvas.Fill.Color := TAlphaColors.Gray;
        CaptionRect       := RectF( x, 7, x + GetWidthOfToolBarButton( BarItems[i], false ), 37 );
        PaintCaption( Control, BarItems[i].Title, CaptionRect, LineBreak, 1, TTextAlign.Center );
        // Text
        Canvas.Fill.Color := TAlphaColors.White;
        CaptionRect       := RectF( x, 8, x + GetWidthOfToolBarButton( BarItems[i], false ), 38 );
        PaintCaption( Control, BarItems[i].Title, CaptionRect, LineBreak, 1, TTextAlign.Center );
        x := x + CaptionRect.Width + 5;
      end;
    end;
  end;
end;

function FMTextAlign(TextAlign:TDPFTextAlignment):TTextAlign;
  begin
    Result:=TTextAlign.Center;
    case TextAlign of
      taLeft:   Result:=TTextAlign.Leading;
      taCenter: Result:=TTextAlign.Center;
      taRight:  Result:=TTextAlign.Trailing;
    end;
  end;
initialization
{$IFNDEF IOS}
  with iOS_GUI_Bitmaps do
    begin
      with ActivityIndicator do
        begin
          LoadResourceToBitmap(Gray,'ActivityIndicator_Gray_iOS6');
          LoadResourceToBitmap(White,'ActivityIndicator_White_iOS6');
          LoadResourceToBitmap(WhiteLarge,'ActivityIndicator_WhiteLarge_iOS6');
        end;
      with Button do
        begin
          LoadResourceToBitmap(RoundedRect,'Button_RoundedRect_iOS6');
          LoadResourceToBitmap(DetailDisclosure,'Button_DetailDisclosure_iOS6');
          LoadResourceToBitmap(InfoLight,'Button_InfoLight_iOS6');
          LoadResourceToBitmap(InfoDark,'Button_InfoDark_iOS6');
          LoadResourceToBitmap(ContactAdd,'Button_ContactAdd_iOS6');
        end;
      with DatePicker do
        begin
          LoadResourceToBitmap(CountDown,'DatePicker_CountDown_iOS6');
          LoadResourceToBitmap(DateAndTime,'DatePicker_DateAndTime_iOS6');
          LoadResourceToBitmap(Date,'DatePicker_Date_iOS6');
          LoadResourceToBitmap(Time,'DatePicker_Time_iOS6');
        end;
      with MapView do
        begin
          LoadResourceToBitmap(TrackingDot,'MapView_TrackingDot_iOS6');
          LoadResourceToBitmap(TrackingDotHalo,'MapView_TrackingDotHalo_iOS6');
        end;
      with NavigationController do
        begin
          LoadResourceToBitmap(ButtonLeftBack,'NavigationController_ButtonLeftBack_iOS6');
          LoadResourceToBitmap(ButtonLeft,'NavigationController_ButtonLeft_iOS6');
          LoadResourceToBitmap(ButtonBG,'NavigationController_ButtonBG_iOS6');
          LoadResourceToBitmap(ButtonRight,'NavigationController_ButtonRight_iOS6');
          LoadResourceToBitmap(LargeBG,'NavigationController_LargeBG_iOS6');
          LoadResourceToBitmap(SmallBG,'NavigationController_SmallBG_iOS6');
        end;
      with PickerView do
        begin
          with Size162 do
            begin
              LoadResourceToBitmap(FrameLeft,'PickerView_FrameLeft_162_iOS6');
              LoadResourceToBitmap(FrameMiddle,'PickerView_FrameMiddle_162_iOS6');
              LoadResourceToBitmap(FrameRight,'PickerView_FrameRight_162_iOS6');
              LoadResourceToBitmap(SelectorLeft,'PickerView_SelectorLeft_162_iOS6');
              LoadResourceToBitmap(SelectorBG,'PickerView_SelectorBG_162_iOS6');
              LoadResourceToBitmap(SelectorRight,'PickerView_SelectorRight_162_iOS6');
              LoadResourceToBitmap(SelectorSeparator,'PickerView_SelectorSeparator_162_iOS6');
              LoadResourceToBitmap(Separator,'PickerView_Separator_162_iOS6');
              LoadResourceToBitmap(WheelBG,'PickerView_WheelBG_162_iOS6');
            end;
          with Size180 do
            begin
              LoadResourceToBitmap(FrameLeft,'PickerView_FrameLeft_180_iOS6');
              LoadResourceToBitmap(FrameMiddle,'PickerView_FrameMiddle_180_iOS6');
              LoadResourceToBitmap(FrameRight,'PickerView_FrameRight_180_iOS6');
              LoadResourceToBitmap(SelectorLeft,'PickerView_SelectorLeft_180_iOS6');
              LoadResourceToBitmap(SelectorBG,'PickerView_SelectorBG_180_iOS6');
              LoadResourceToBitmap(SelectorRight,'PickerView_SelectorRight_180_iOS6');
              LoadResourceToBitmap(SelectorSeparator,'PickerView_SelectorSeparator_180_iOS6');
              LoadResourceToBitmap(Separator,'PickerView_Separator_180_iOS6');
              LoadResourceToBitmap(WheelBG,'PickerView_WheelBG_180_iOS6');
            end;
          with Size216 do
            begin
              LoadResourceToBitmap(FrameLeft,'PickerView_FrameLeft_216_iOS6');
              LoadResourceToBitmap(FrameMiddle,'PickerView_FrameMiddle_216_iOS6');
              LoadResourceToBitmap(FrameRight,'PickerView_FrameRight_216_iOS6');
              LoadResourceToBitmap(SelectorLeft,'PickerView_SelectorLeft_216_iOS6');
              LoadResourceToBitmap(SelectorBG,'PickerView_SelectorBG_216_iOS6');
              LoadResourceToBitmap(SelectorRight,'PickerView_SelectorRight_216_iOS6');
              LoadResourceToBitmap(SelectorSeparator,'PickerView_SelectorSeparator_216_iOS6');
              LoadResourceToBitmap(Separator,'PickerView_Separator_216_iOS6');
              LoadResourceToBitmap(WheelBG,'PickerView_WheelBG_216_iOS6');
            end;
        end;
      with ProgressView do
        begin
          LoadResourceToBitmap(Left,'ProgressView_Left_iOS6');
          LoadResourceToBitmap(BG,'ProgressView_BG_iOS6');
          LoadResourceToBitmap(Right,'ProgressView_Right_iOS6');
          LoadResourceToBitmap(Fill_Left,'ProgressView_Fill_Left_iOS6');
          LoadResourceToBitmap(Fill_BG,'ProgressView_Fill_BG_iOS6');
          LoadResourceToBitmap(Fill_Right,'ProgressView_Fill_Right_iOS6');
        end;
      with SearchBar do
        begin
          LoadResourceToBitmap(BG,'SearchBar_BG_iOS6');
          LoadResourceToBitmap(Bookmarks,'SearchBar_Bookmarks_iOS6');
          LoadResourceToBitmap(Field_BG,'SearchBar_Field_BG_iOS6');
          LoadResourceToBitmap(Field_Left,'SearchBar_Field_Left_iOS6');
          LoadResourceToBitmap(Field_Right,'SearchBar_Field_Right_iOS6');
          LoadResourceToBitmap(Results,'SearchBar_Results_iOS6');
          LoadResourceToBitmap(Scope_BG_Down,'SearchBar_Scope_BG_Down_iOS6');
          LoadResourceToBitmap(Scope_BG,'SearchBar_Scope_BG_iOS6');
          LoadResourceToBitmap(Scope_BG_Up,'SearchBar_Scope_BG_Up_iOS6');
          LoadResourceToBitmap(Scope_Left_Down,'SearchBar_Scope_Left_Down_iOS6');
          LoadResourceToBitmap(Scope_Left_Up,'SearchBar_Scope_Left_Up_iOS6');
          LoadResourceToBitmap(Scope_Right_Down,'SearchBar_Scope_Right_Down_iOS6');
          LoadResourceToBitmap(Scope_Right_Up,'SearchBar_Scope_Right_Up_iOS6');
          LoadResourceToBitmap(Scope_Separator_Down,'SearchBar_Scope_Separator_Down_iOS6');
          LoadResourceToBitmap(Scope_Separator_Up,'SearchBar_Scope_Separator_Up_iOS6');
          LoadResourceToBitmap(SearchIcon,'SearchBar_SearchIcon_iOS6');
        end;
      with SegmentedControl do
        begin
          with Bar do
            begin
              LoadResourceToBitmap(ButtonBG_Down,'SegmentedControl_Bar_ButtonBG_Down_iOS6');
              LoadResourceToBitmap(ButtonBG_Up,'SegmentedControl_Bar_ButtonBG_Up_iOS6');
              LoadResourceToBitmap(Left_Down,'SegmentedControl_Bar_Left_Down_iOS6');
              LoadResourceToBitmap(Left_Up,'SegmentedControl_Bar_Left_Up_iOS6');
              LoadResourceToBitmap(Right_Down,'SegmentedControl_Bar_Right_Down_iOS6');
              LoadResourceToBitmap(Right_Up,'SegmentedControl_Bar_Right_Up_iOS6');
              LoadResourceToBitmap(Separator_Down,'SegmentedControl_Bar_Separator_Down_iOS6');
              LoadResourceToBitmap(Separator_Up,'SegmentedControl_Bar_Separator_Up_iOS6');
            end;
          with Bezeled do
            begin
              LoadResourceToBitmap(ButtonBG_Down,'SegmentedControl_Bezeled_ButtonBG_Down_iOS6');
              LoadResourceToBitmap(ButtonBG_Up,'SegmentedControl_Bezeled_ButtonBG_Up_iOS6');
              LoadResourceToBitmap(Left_Down,'SegmentedControl_Bezeled_Left_Down_iOS6');
              LoadResourceToBitmap(Left_Up,'SegmentedControl_Bezeled_Left_Up_iOS6');
              LoadResourceToBitmap(Right_Down,'SegmentedControl_Bezeled_Right_Down_iOS6');
              LoadResourceToBitmap(Right_Up,'SegmentedControl_Bezeled_Right_Up_iOS6');
              LoadResourceToBitmap(Separator_Down,'SegmentedControl_Bezeled_Separator_Down_iOS6');
              LoadResourceToBitmap(Separator_Up,'SegmentedControl_Bezeled_Separator_Up_iOS6');
            end;
          with Bordered do
            begin
              LoadResourceToBitmap(ButtonBG_Down,'SegmentedControl_Bordered_ButtonBG_Down_iOS6');
              LoadResourceToBitmap(ButtonBG_Up,'SegmentedControl_Bordered_ButtonBG_Up_iOS6');
              LoadResourceToBitmap(Left_Down,'SegmentedControl_Bordered_Left_Down_iOS6');
              LoadResourceToBitmap(Left_Up,'SegmentedControl_Bordered_Left_Up_iOS6');
              LoadResourceToBitmap(Right_Down,'SegmentedControl_Bordered_Right_Down_iOS6');
              LoadResourceToBitmap(Right_Up,'SegmentedControl_Bordered_Right_Up_iOS6');
              LoadResourceToBitmap(Separator_DownLeft,'SegmentedControl_Bordered_Separator_DownLeft_iOS6');
              LoadResourceToBitmap(Separator_DownRight,'SegmentedControl_Bordered_Separator_DownRight_iOS6');
              LoadResourceToBitmap(Separator_Down,'SegmentedControl_Bordered_Separator_Down_iOS6');
              LoadResourceToBitmap(Separator_Up,'SegmentedControl_Bordered_Separator_Up_iOS6');
            end;
          with Plain do
            begin
              LoadResourceToBitmap(ButtonBG_Down,'SegmentedControl_Plain_ButtonBG_Down_iOS6');
              LoadResourceToBitmap(ButtonBG_Up,'SegmentedControl_Plain_ButtonBG_Up_iOS6');
              LoadResourceToBitmap(Left_Down,'SegmentedControl_Plain_Left_Down_iOS6');
              LoadResourceToBitmap(Left_Up,'SegmentedControl_Plain_Left_Up_iOS6');
              LoadResourceToBitmap(Right_Down,'SegmentedControl_Plain_Right_Down_iOS6');
              LoadResourceToBitmap(Right_Up,'SegmentedControl_Plain_Right_Up_iOS6');
              LoadResourceToBitmap(Separator_DownLeft,'SegmentedControl_Plain_Separator_DownLeft_iOS6');
              LoadResourceToBitmap(Separator_DownRight,'SegmentedControl_Plain_Separator_DownRight_iOS6');
              LoadResourceToBitmap(Separator_Down,'SegmentedControl_Plain_Separator_Down_iOS6');
              LoadResourceToBitmap(Separator_Up,'SegmentedControl_Plain_Separator_Up_iOS6');
            end;
        end;
      with Slider do
        begin
          LoadResourceToBitmap(Tab,'Slider_Tab_iOS6');
          LoadResourceToBitmap(BarOff,'Slider_Bar_Off_iOS6');
          LoadResourceToBitmap(BarOn,'Slider_Bar_On_iOS6');
        end;
      //Stepper
      LoadResourceToBitmap(Stepper,'Stepper_iOS6');
      with Switch do
        begin
          LoadResourceToBitmap(IsOn,'Switch_On_iOS6');
          LoadResourceToBitmap(IsOff,'Switch_Off_iOS6');
        end;
      with TabBar do
        begin
          LoadResourceToBitmap(BG,'TabBar_BG_iOS6');
          LoadResourceToBitmap(BadgeLeft,'TabBar_BadgeLeft_iOS6');
          LoadResourceToBitmap(BadgeBG,'TabBar_BadgeBG_iOS6');
          LoadResourceToBitmap(BadgeRight,'TabBar_BadgeRight_iOS6');
          LoadResourceToBitmap(Bookmark_Off,'TabBar_Bookmark_Off_iOS6');
          LoadResourceToBitmap(Bookmark_On,'TabBar_Bookmark_On_iOS6');
          LoadResourceToBitmap(Contacts_Off,'TabBar_Contacts_Off_iOS6');
          LoadResourceToBitmap(Contacts_On,'TabBar_Contacts_On_iOS6');
          LoadResourceToBitmap(Downloads_Off,'TabBar_Downloads_Off_iOS6');
          LoadResourceToBitmap(Downloads_On,'TabBar_Downloads_On_iOS6');
          LoadResourceToBitmap(Favorites_Off,'TabBar_Favorites_Off_iOS6');
          LoadResourceToBitmap(Favorites_On,'TabBar_Favorites_On_iOS6');
          LoadResourceToBitmap(Featured_Off,'TabBar_Featured_Off_iOS6');
          LoadResourceToBitmap(Featured_On,'TabBar_Featured_On_iOS6');
          LoadResourceToBitmap(History_Off,'TabBar_History_Off_iOS6');
          LoadResourceToBitmap(History_On,'TabBar_History_On_iOS6');
          LoadResourceToBitmap(More_Off,'TabBar_More_Off_iOS6');
          LoadResourceToBitmap(More_On,'TabBar_More_On_iOS6');
          LoadResourceToBitmap(MostRecent_Off,'TabBar_MostRecent_Off_iOS6');
          LoadResourceToBitmap(MostRecent_On,'TabBar_MostRecent_On_iOS6');
          LoadResourceToBitmap(MostViewed_Off,'TabBar_MostViewed_Off_iOS6');
          LoadResourceToBitmap(MostViewed_On,'TabBar_MostViewed_On_iOS6');
          LoadResourceToBitmap(Search_Off,'TabBar_Search_Off_iOS6');
          LoadResourceToBitmap(Search_On,'TabBar_Search_On_iOS6');
          LoadResourceToBitmap(SelectionLeft,'TabBar_SelectionLeft_iOS6');
          LoadResourceToBitmap(SelectionBG,'TabBar_SelectionBG_iOS6');
          LoadResourceToBitmap(SelectionRight,'TabBar_SelectionRight_iOS6');
        end;
      with TableView do
        begin
          LoadResourceToBitmap(Grouped_ItemBorder,'TableView_Grouped_ItemBorder_iOS6');
          LoadResourceToBitmap(Item_Check,'TableView_Item_Check_iOS6');
          LoadResourceToBitmap(Item_Delete,'TableView_Item_Delete_iOS6');
          LoadResourceToBitmap(Item_Grabber,'TableView_Item_Grabber_iOS6');
          LoadResourceToBitmap(Item_NextButton,'TableView_Item_NextButton_iOS6');
          LoadResourceToBitmap(Item_NextSelection,'TableView_Item_NextSelection_iOS6');
          LoadResourceToBitmap(Plain_HeaderBG,'TableView_Plain_HeaderBG_iOS6');
        end;
      with TextField do
        begin
          LoadResourceToBitmap(Bezel,'TextField_Bezel_iOS6');
          LoadResourceToBitmap(Line,'TextField_Line_iOS6');
          LoadResourceToBitmap(RoundedRect,'TextField_RoundedRect_iOS6');
        end;
      with ToolBar do
        begin
          LoadResourceToBitmap(BG,'ToolBar_BG_iOS6');
          LoadResourceToBitmap(DoneLeft,'ToolBar_DoneLeft_iOS6');
          LoadResourceToBitmap(DoneBG,'ToolBar_DoneBG_iOS6');
          LoadResourceToBitmap(DoneRight,'ToolBar_DoneRight_iOS6');
          LoadResourceToBitmap(ButtonLeft,'ToolBar_ButtonLeft_iOS6');
          LoadResourceToBitmap(ButtonBG,'ToolBar_ButtonBG_iOS6');
          LoadResourceToBitmap(ButtonRight,'ToolBar_ButtonRight_iOS6');
          with LargeIcons do
            begin
              LoadResourceToBitmap(Action,'ToolBar_ButtonIconLarge_Action_iOS6');
              LoadResourceToBitmap(Add,'ToolBar_ButtonIconLarge_Add_iOS6');
              LoadResourceToBitmap(Bookmarks,'ToolBar_ButtonIconLarge_Bookmarks_iOS6');
              LoadResourceToBitmap(Cancel,'ToolBar_ButtonIconLarge_Cancel_iOS6');
              LoadResourceToBitmap(Camera,'ToolBar_ButtonIconLarge_Camera_iOS6');
              LoadResourceToBitmap(Compose,'ToolBar_ButtonIconLarge_Compose_iOS6');
              LoadResourceToBitmap(Done,'ToolBar_ButtonIconLarge_Done_iOS6');
              LoadResourceToBitmap(Edit,'ToolBar_ButtonIconLarge_Edit_iOS6');
              LoadResourceToBitmap(FastForward,'ToolBar_ButtonIconLarge_FastForward_iOS6');
              LoadResourceToBitmap(Organize,'ToolBar_ButtonIconLarge_Organize_iOS6');
              LoadResourceToBitmap(Pause,'ToolBar_ButtonIconLarge_Pause_iOS6');
              LoadResourceToBitmap(Play,'ToolBar_ButtonIconLarge_Play_iOS6');
              LoadResourceToBitmap(Refresh,'ToolBar_ButtonIconLarge_Refresh_iOS6');
              LoadResourceToBitmap(Reply,'ToolBar_ButtonIconLarge_Reply_iOS6');
              LoadResourceToBitmap(Rewind,'ToolBar_ButtonIconLarge_Rewind_iOS6');
              LoadResourceToBitmap(Save,'ToolBar_ButtonIconLarge_Save_iOS6');
              LoadResourceToBitmap(Search,'ToolBar_ButtonIconLarge_Search_iOS6');
              LoadResourceToBitmap(Stop,'ToolBar_ButtonIconLarge_Stop_iOS6');
              LoadResourceToBitmap(Trash,'ToolBar_ButtonIconLarge_Trash_iOS6');
            end;
          with SmallIcons do
            begin
              LoadResourceToBitmap(Action,'ToolBar_ButtonIconSmall_Action_iOS6');
              LoadResourceToBitmap(Add,'ToolBar_ButtonIconSmall_Add_iOS6');
              LoadResourceToBitmap(Bookmarks,'ToolBar_ButtonIconSmall_Bookmarks_iOS6');
              LoadResourceToBitmap(Cancel,'ToolBar_ButtonIconSmall_Cancel_iOS6');
              LoadResourceToBitmap(Camera,'ToolBar_ButtonIconSmall_Camera_iOS6');
              LoadResourceToBitmap(Compose,'ToolBar_ButtonIconSmall_Compose_iOS6');
              LoadResourceToBitmap(Done,'ToolBar_ButtonIconSmall_Done_iOS6');
              LoadResourceToBitmap(Edit,'ToolBar_ButtonIconSmall_Edit_iOS6');
              LoadResourceToBitmap(FastForward,'ToolBar_ButtonIconSmall_FastForward_iOS6');
              LoadResourceToBitmap(Organize,'ToolBar_ButtonIconSmall_Organize_iOS6');
              LoadResourceToBitmap(Pause,'ToolBar_ButtonIconSmall_Pause_iOS6');
              LoadResourceToBitmap(Play,'ToolBar_ButtonIconSmall_Play_iOS6');
              LoadResourceToBitmap(Refresh,'ToolBar_ButtonIconSmall_Refresh_iOS6');
              LoadResourceToBitmap(Reply,'ToolBar_ButtonIconSmall_Reply_iOS6');
              LoadResourceToBitmap(Rewind,'ToolBar_ButtonIconSmall_Rewind_iOS6');
              LoadResourceToBitmap(Save,'ToolBar_ButtonIconSmall_Save_iOS6');
              LoadResourceToBitmap(Search,'ToolBar_ButtonIconSmall_Search_iOS6');
              LoadResourceToBitmap(Stop,'ToolBar_ButtonIconSmall_Stop_iOS6');
              LoadResourceToBitmap(Trash,'ToolBar_ButtonIconSmall_Trash_iOS6');
            end;
        end;
      with VolumeControl do
        begin
          LoadResourceToBitmap(Knob,'VolumeControl_Knob_iOS6');
        end;
    end;
{$ENDIF}

finalization
{$IFNDEF IOS}
  //ActivityIndicator
  iOS_GUI_Bitmaps.ActivityIndicator.Gray.Free;
  iOS_GUI_Bitmaps.ActivityIndicator.White.Free;
  iOS_GUI_Bitmaps.ActivityIndicator.WhiteLarge.Free;
  //Button
  iOS_GUI_Bitmaps.Button.RoundedRect.Free;
  iOS_GUI_Bitmaps.Button.DetailDisclosure.Free;
  iOS_GUI_Bitmaps.Button.InfoLight.Free;
  iOS_GUI_Bitmaps.Button.InfoDark.Free;
  iOS_GUI_Bitmaps.Button.ContactAdd.Free;
  //DatePicker
  iOS_GUI_Bitmaps.DatePicker.CountDown.Free;
  iOS_GUI_Bitmaps.DatePicker.DateAndTime.Free;
  iOS_GUI_Bitmaps.DatePicker.Date.Free;
  iOS_GUI_Bitmaps.DatePicker.Time.Free;
  //MapView
  iOS_GUI_Bitmaps.MapView.TrackingDot.Free;
  iOS_GUI_Bitmaps.MapView.TrackingDotHalo.Free;
  //NavigationController
  iOS_GUI_Bitmaps.NavigationController.ButtonLeftBack.Free;
  iOS_GUI_Bitmaps.NavigationController.ButtonLeft.Free;
  iOS_GUI_Bitmaps.NavigationController.ButtonBG.Free;
  iOS_GUI_Bitmaps.NavigationController.ButtonRight.Free;
  iOS_GUI_Bitmaps.NavigationController.LargeBG.Free;
  iOS_GUI_Bitmaps.NavigationController.SmallBG.Free;
  //PickerView
  iOS_GUI_Bitmaps.PickerView.Size162.FrameLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.FrameMiddle.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.FrameRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.SelectorLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.SelectorBG.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.SelectorRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.SelectorSeparator.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.Separator.Free;
  iOS_GUI_Bitmaps.PickerView.Size162.WheelBG.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.FrameLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.FrameMiddle.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.FrameRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.SelectorLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.SelectorBG.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.SelectorRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.SelectorSeparator.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.Separator.Free;
  iOS_GUI_Bitmaps.PickerView.Size180.WheelBG.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.FrameLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.FrameMiddle.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.FrameRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.SelectorLeft.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.SelectorBG.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.SelectorRight.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.SelectorSeparator.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.Separator.Free;
  iOS_GUI_Bitmaps.PickerView.Size216.WheelBG.Free;
  //ProgressBar
  iOS_GUI_Bitmaps.ProgressView.Left.Free;
  iOS_GUI_Bitmaps.ProgressView.BG.Free;
  iOS_GUI_Bitmaps.ProgressView.Right.Free;
  iOS_GUI_Bitmaps.ProgressView.Fill_Left.Free;
  iOS_GUI_Bitmaps.ProgressView.Fill_BG.Free;
  iOS_GUI_Bitmaps.ProgressView.Fill_Right.Free;
  //SearchBar
  iOS_GUI_Bitmaps.SearchBar.BG.Free;
  iOS_GUI_Bitmaps.SearchBar.Bookmarks.Free;
  iOS_GUI_Bitmaps.SearchBar.Field_BG.Free;
  iOS_GUI_Bitmaps.SearchBar.Field_Left.Free;
  iOS_GUI_Bitmaps.SearchBar.Field_Right.Free;
  iOS_GUI_Bitmaps.SearchBar.Results.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_BG_Down.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_BG.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_BG_Up.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Left_Down.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Left_Up.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Right_Down.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Right_Up.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Separator_Down.Free;
  iOS_GUI_Bitmaps.SearchBar.Scope_Separator_Up.Free;
  iOS_GUI_Bitmaps.SearchBar.SearchIcon.Free;
  //SegmentedControl
  iOS_GUI_Bitmaps.SegmentedControl.Bar.ButtonBG_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.ButtonBG_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Left_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Left_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Right_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Right_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Separator_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bar.Separator_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.ButtonBG_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.ButtonBG_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Left_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Left_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Right_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Right_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Separator_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bezeled.Separator_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.ButtonBG_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.ButtonBG_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Left_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Left_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Right_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Right_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Separator_DownLeft.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Separator_DownRight.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Separator_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Bordered.Separator_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.ButtonBG_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.ButtonBG_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Left_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Left_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Right_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Right_Up.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Separator_DownLeft.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Separator_DownRight.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Separator_Down.Free;
  iOS_GUI_Bitmaps.SegmentedControl.Plain.Separator_Up.Free;
  //Slider
  iOS_GUI_Bitmaps.Slider.Tab.Free;
  iOS_GUI_Bitmaps.Slider.BarOff.Free;
  iOS_GUI_Bitmaps.Slider.BarOn.Free;
  //Stepper
  iOS_GUI_Bitmaps.Stepper.Free;
  //Switch
  iOS_GUI_Bitmaps.Switch.IsOn.Free;
  iOS_GUI_Bitmaps.Switch.IsOff.Free;
  //TabBar
  iOS_GUI_Bitmaps.TabBar.BG.Free;
  iOS_GUI_Bitmaps.TabBar.BadgeLeft.Free;
  iOS_GUI_Bitmaps.TabBar.BadgeBG.Free;
  iOS_GUI_Bitmaps.TabBar.BadgeRight.Free;
  iOS_GUI_Bitmaps.TabBar.Bookmark_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Bookmark_On.Free;
  iOS_GUI_Bitmaps.TabBar.Contacts_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Contacts_On.Free;
  iOS_GUI_Bitmaps.TabBar.Downloads_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Downloads_On.Free;
  iOS_GUI_Bitmaps.TabBar.Favorites_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Favorites_On.Free;
  iOS_GUI_Bitmaps.TabBar.Featured_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Featured_On.Free;
  iOS_GUI_Bitmaps.TabBar.History_Off.Free;
  iOS_GUI_Bitmaps.TabBar.History_On.Free;
  iOS_GUI_Bitmaps.TabBar.More_Off.Free;
  iOS_GUI_Bitmaps.TabBar.More_On.Free;
  iOS_GUI_Bitmaps.TabBar.MostRecent_Off.Free;
  iOS_GUI_Bitmaps.TabBar.MostRecent_On.Free;
  iOS_GUI_Bitmaps.TabBar.MostViewed_Off.Free;
  iOS_GUI_Bitmaps.TabBar.MostViewed_On.Free;
  iOS_GUI_Bitmaps.TabBar.Search_Off.Free;
  iOS_GUI_Bitmaps.TabBar.Search_On.Free;
  iOS_GUI_Bitmaps.TabBar.SelectionLeft.Free;
  iOS_GUI_Bitmaps.TabBar.SelectionBG.Free;
  iOS_GUI_Bitmaps.TabBar.SelectionRight.Free;
  //TableView
  iOS_GUI_Bitmaps.TableView.Grouped_ItemBorder.Free;
  iOS_GUI_Bitmaps.TableView.Item_Check.Free;
  iOS_GUI_Bitmaps.TableView.Item_Delete.Free;
  iOS_GUI_Bitmaps.TableView.Item_Grabber.Free;
  iOS_GUI_Bitmaps.TableView.Item_NextButton.Free;
  iOS_GUI_Bitmaps.TableView.Item_NextSelection.Free;
  iOS_GUI_Bitmaps.TableView.Plain_HeaderBG.Free;
  //TextField
  iOS_GUI_Bitmaps.TextField.Bezel.Free;
  iOS_GUI_Bitmaps.TextField.Line.Free;
  iOS_GUI_Bitmaps.TextField.RoundedRect.Free;
  //ToolBar
  iOS_GUI_Bitmaps.ToolBar.BG.Free;
  iOS_GUI_Bitmaps.ToolBar.DoneLeft.Free;
  iOS_GUI_Bitmaps.ToolBar.DoneBG.Free;
  iOS_GUI_Bitmaps.ToolBar.DoneRight.Free;
  iOS_GUI_Bitmaps.ToolBar.ButtonLeft.Free;
  iOS_GUI_Bitmaps.ToolBar.ButtonBG.Free;
  iOS_GUI_Bitmaps.ToolBar.ButtonRight.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Action.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Add.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Bookmarks.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Cancel.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Camera.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Compose.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Done.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Edit.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.FastForward.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Organize.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Pause.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Play.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Refresh.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Reply.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Rewind.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Save.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Search.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Stop.Free;
  iOS_GUI_Bitmaps.ToolBar.LargeIcons.Trash.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Action.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Add.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Bookmarks.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Cancel.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Camera.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Compose.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Done.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Edit.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.FastForward.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Organize.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Pause.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Play.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Refresh.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Reply.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Rewind.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Save.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Search.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Stop.Free;
  iOS_GUI_Bitmaps.ToolBar.SmallIcons.Trash.Free;
  //VolumeControl
  iOS_GUI_Bitmaps.VolumeControl.Knob.Free;
{$ENDIF}

end.
