unit SXSkinRegionManager;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Windows, Classes, SXSkinLibrary, GR32, SysUtils, GR32_Resamplers,
     SXZipUtils, SXMathEval;

type

 TSXSkinPreloadedRegion=class
  public
   Resized:Boolean;
   Width:Integer;
   Height:Integer;
   FilePath:String;
   Color:TColor32;
   ColorMask:TColor32;
   Region:HRGN;
   SkinLibrary:TSXSkinLibrary;
   destructor Destroy; override;
 end;

 TSXSkinPreloadedRegionList=class
  protected
   FItem:TList;
   function Get(const Index:Integer):TSXSkinPreloadedRegion;
   procedure Put(const Index:Integer;const Item:TSXSkinPreloadedRegion);
   function GetCount:Integer;
  public
   procedure Add(Region:TSXSkinPreloadedRegion);
   function GetRegionIndex(Region:TSXSkinPreloadedRegion):Integer;
   procedure Delete(const Index:Integer);
   procedure Clear;
   constructor Create;
   destructor Destroy; override;
   property Item[const Index:Integer]:TSXSkinPreloadedRegion read Get write Put; default;
   property Count:Integer read GetCount;
 end;

function GetPreloadedRegion(SkinLibrary:TSXSkinLibrary;Tile:Boolean;
           const FilePath,ZipFilePath:String;Resized:Boolean;Width:Integer;
           Height:Integer;SaveResized:Boolean;Color,ColorMask:TColor32):HRGN;
procedure DeletePreloadedRegions(SkinLibrary:TSXSkinCustomLibrary);
function EvaluateRegion(const Region,SkinFilePath:String;ZipFilePath:String;
           Width,Height:Integer;SkinLibrary:TSXSkinLibrary;SaveResized:Boolean;
           AOnGetVariable:TSXOnGetVariable=nil):HRGN;

var PreloadedRegions:TSXSkinPreloadedRegionList;

implementation

uses Math, jpeg, SXPNGUtils, SXSkinUtils;

function CompareRegions(Region1,Region2:TSXSkinPreloadedRegion):Integer;
begin
 Result:=CompareValue(Integer(Region1.SkinLibrary),Integer(Region2.SkinLibrary));
 if Result=0 then
  Result:=CompareStr(Region1.FilePath,Region2.FilePath) else exit;
 if Result=0 then
  Result:=CompareValue(Region1.Color,Region2.Color) else exit;
 if Result=0 then
  Result:=CompareValue(Region1.ColorMask,Region2.ColorMask) else exit;
 if Result=0 then
  Result:=CompareValue(Ord(Region1.Resized),Ord(Region2.Resized)) else exit;
 if Result<>0 then exit;
 if Region1.Resized then
  begin
   Result:=CompareValue(Ord(Region1.Width),Ord(Region2.Width));
   if Result=0 then
    Result:=CompareValue(Ord(Region1.Height),Ord(Region2.Height)) else exit;
  end;
end;

function CreateRgnFromBitmap(B:TBitmap32;Color,ColorMask:TColor32):HRGN;
var A,C,StartX:Integer;
       RgnData:PRgnData;
    Size,Count:Integer;
            MS:TMemoryStream;
             P:PColor32;
             R:TRect;
begin
 Result:=0;
 if B.Empty then exit;
 MS:=TMemoryStream.Create;
 try
  P:=@B.Bits[0];
  StartX:=-1;
  for A:=0 to B.Height-1 do
   begin
    for C:=0 to B.Width-1 do
     begin
      if P^ and ColorMask<>Color then
       begin
        if StartX<0 then
         StartX:=C;
       end else
        begin
         if StartX>=0 then
          begin
           R:=Rect(StartX,A,C,A+1);
           MS.Write(R,sizeof(R));
           StartX:=-1;
          end;
        end;
      Inc(P);
     end;
    if StartX>=0 then
     begin
      R:=Rect(StartX,A,B.Width,A+1);
      MS.Write(R,sizeof(R));
      StartX:=-1;
     end;
   end;
  Count:=MS.Size div sizeof(TRect);
  Size:=sizeof(TRgnDataHeader)+MS.Size;
  GetMem(RgnData,Size);
  FillChar(RgnData^,Size,0);
  RgnData^.rdh.dwSize:=sizeof(TRgnDataHeader);
  RgnData^.rdh.iType:=RDH_RECTANGLES;
  RgnData^.rdh.nCount:=Count;
  RgnData^.rdh.nRgnSize:=0;
  RgnData^.rdh.rcBound:=Rect(0,0,B.Width,B.Height);
  Move(MS.Memory^,RgnData^.Buffer,MS.Size);
  Result:=ExtCreateRegion(nil,Size,RgnData^);
  FreeMem(RgnData);
 finally
  MS.Free;
 end;
end;

{ TSXSkinPreloadedRegion }

destructor TSXSkinPreloadedRegion.Destroy;
begin
 if Region<>0 then
  DeleteObject(Region);
 inherited;
end;

{ TSXSkinPreloadedRegionList }

function TSXSkinPreloadedRegionList.Get(const Index:Integer):TSXSkinPreloadedRegion;
begin
 Result:=TSXSkinPreloadedRegion(FItem[Index]);
end;

procedure TSXSkinPreloadedRegionList.Put(const Index:Integer;const Item:TSXSkinPreloadedRegion);
begin
 FItem[Index]:=Item;
end;

function TSXSkinPreloadedRegionList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

procedure TSXSkinPreloadedRegionList.Add(Region:TSXSkinPreloadedRegion);
var L,H,A,C:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   C:=CompareRegions(Region,Item[A]);
   if C<0 then L:=A+1 else
    begin
     H:=A-1;
     if C=0 then L:=A;
    end;
  end;
 FItem.Insert(L,Region);
end;

function TSXSkinPreloadedRegionList.GetRegionIndex(Region:TSXSkinPreloadedRegion):Integer;
var  A,C:Integer;
     L,H:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   C:=CompareRegions(Region,Item[A]);
   if C<0 then L:=A+1 else
    begin
     H:=A-1;
     if C=0 then L:=A;
    end;
  end;
 if (L>=0) and (L<Count) and (CompareRegions(Region,Item[L])=0) then
  Result:=L else Result:=-1;
end;

procedure TSXSkinPreloadedRegionList.Delete(const Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXSkinPreloadedRegionList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

constructor TSXSkinPreloadedRegionList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXSkinPreloadedRegionList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////

function TileRegion(Rgn:HRGN;Width,Height,NewWidth,NewHeight:Integer):HRGN;
var X,Y,XN,YN:Integer;
       TmpRgn:HRGN;
begin
 Result:=CreateRectRgn(0,0,0,0);
 if (Width=0) or (Height=0) then exit;
 XN:=NewWidth div Width;
 YN:=NewHeight div Height;
 if NewWidth mod Width=0 then Dec(XN);
 if NewHeight mod Height=0 then Dec(YN);
 for X:=0 to XN do
  begin
   if X<>0 then
    OffsetRgn(Rgn,Width,0);
   CombineRgn(Result,Result,Rgn,RGN_OR);
  end;
 if XN>0 then
  OffsetRgn(Rgn,-XN*Width,0);
 if YN>0 then
  begin
   TmpRgn:=CreateRectRgn(0,0,0,0);
   CombineRgn(TmpRgn,Result,0,RGN_COPY);
   for Y:=1 to YN do
    begin
     OffsetRgn(TmpRgn,0,Height);
     CombineRgn(Result,Result,TmpRgn,RGN_OR);
    end;
   DeleteObject(TmpRgn);
  end;
 TmpRgn:=CreateRectRgn(0,0,NewWidth,NewHeight);
 CombineRgn(Result,Result,TmpRgn,RGN_AND);
 DeleteObject(TmpRgn);
end;

function GetPreloadedRegion(SkinLibrary:TSXSkinLibrary;Tile:Boolean;
           const FilePath,ZipFilePath:String;Resized:Boolean;Width:Integer;
           Height:Integer;SaveResized:Boolean;Color,ColorMask:TColor32):HRGN;
type TRectArray=array of TRect;
var A,B,W,H:Integer;
          R:TSXSkinPreloadedRegion;
         B2:TBitmap32;
   BmpWidth:Integer;
  BmpHeight:Integer;        
        JPG:TJpegImage;
        PNG:TPNGObject;
         MS:TMemoryStream;
  ImageType:TSXSkinStyleImageType;
    ZipFile:TZipFile;
    RgnData:PRgnData;
      XForm:TXForm;
begin
 R:=TSXSkinPreloadedRegion.Create;
 R.SkinLibrary:=SkinLibrary;
 R.FilePath:=FilePath;
 R.Color:=Color;
 R.ColorMask:=ColorMask;
 R.Resized:=Resized;
 R.Width:=Width;
 R.Height:=Height;
 A:=PreloadedRegions.GetRegionIndex(R);
 if A>=0 then
  begin
   Result:=PreloadedRegions[A].Region;
   R.Free;
   exit;
  end;
 if Resized then
  begin
   R.Resized:=False;
   R.Width:=0; R.Height:=0;
   A:=PreloadedRegions.GetRegionIndex(R);
   if A>=0 then
    begin
     R.Resized:=True;
     R.Width:=Width; R.Height:=Height;
     W:=Width; H:=Height;
     if W=0 then W:=PreloadedRegions[A].Width;
     if H=0 then H:=PreloadedRegions[A].Height;
     if Tile then
      begin
       Result:=TileRegion(R.Region,PreloadedRegions[A].Width,PreloadedRegions[A].Height,W,H);
       R.Free;
       exit;
      end;
     B:=GetRegionData(PreloadedRegions[A].Region,0,nil);
     if B>=0 then
      begin
       GetMem(RgnData,B);
       GetRegionData(PreloadedRegions[A].Region,B,RgnData);
       FillChar(XForm,sizeof(XForm),0);
       if PreloadedRegions[A].Width>0 then
        XForm.eM11:=W/PreloadedRegions[A].Width;
       if PreloadedRegions[A].Height>0 then
        XForm.eM22:=H/PreloadedRegions[A].Height;
       R.Region:=ExtCreateRegion(@XForm,B,RgnData^);
       FreeMem(RgnData);
      end;
     Result:=R.Region;
     PreloadedRegions.Add(R);
     exit;
    end;
  end;
 R.Width:=Width;
 R.Height:=Height;
 //
 if SameText(ExtractFileExt(FilePath),'.png') then
  ImageType:=ssitPNG else ImageType:=ssitJPEG;
 B2:=nil; 
 case ImageType of
  ssitJPEG: begin
             JPG:=TJpegImage.Create;
             try
              try
               if ZipFilePath<>'' then
                begin
                 MS:=TMemoryStream.Create;
                 try
                  ZipFile:=GetPreloadedZipFile(SkinLibrary,ZipFilePath);
                  if ZipFile<>nil then
                   begin
                    ZipFile.WriteFileToStream(MS,FilePath);
                    MS.Seek(0,soFromBeginning);
                    if MS.Size>0 then
                     JPG.LoadFromStream(MS);
                   end;
                 finally
                  MS.Free;
                 end;
                end else
               if FileExists(FilePath) then
                JPG.LoadFromFile(FilePath);
              except
              end;
              B2:=TBitmap32.Create;
              B2.Assign(JPG);
             finally
              JPG.Free;
             end;
            end;
  ssitPNG:  begin
             PNG:=TPNGObject.Create;
             try
              try
               if ZipFilePath<>'' then
                begin
                 MS:=TMemoryStream.Create;
                 try
                  ZipFile:=GetPreloadedZipFile(SkinLibrary,ZipFilePath);
                  if ZipFile<>nil then
                   begin
                    ZipFile.WriteFileToStream(MS,FilePath);
                    MS.Seek(0,soFromBeginning);
                    if MS.Size>0 then
                     PNG.LoadFromStream(MS);
                   end;
                 finally
                  MS.Free;
                 end;
                end else
               if FileExists(FilePath) then
                PNG.LoadFromFile(FilePath);
              except
              end;
              B2:=TBitmap32.Create;
              PNGObjectToBitmap32(PNG,B2);
             finally
              PNG.Free;
             end;
            end;
 end;
 if B2=nil then
  begin
   Result:=0;
   exit;
  end;
 if (B2.Width=0) or (B2.Height=0) then
  begin
   Result:=CreateRectRgn(0,0,0,0);
   exit;
  end;
 BmpWidth:=B2.Width;
 BmpHeight:=B2.Height;
 R.Region:=CreateRgnFromBitmap(B2,Color,ColorMask);
 B2.Free;
 //
 if Resized then
  begin
   W:=Width; H:=Height;
   if W=0 then W:=BmpWidth;
   if H=0 then H:=BmpHeight;
   if Tile then
    begin
     Result:=TileRegion(R.Region,BmpWidth,BmpHeight,W,H);
     R.Free;
     exit;
    end;
   if (W<>BmpWidth) or (H<>BmpHeight) then
    begin
     B:=GetRegionData(R.Region,0,nil);
     if B>=0 then
      begin
       GetMem(RgnData,B);
       GetRegionData(R.Region,B,RgnData);
       FillChar(XForm,sizeof(XForm),0);
       if BmpWidth>0 then
        XForm.eM11:=W/BmpWidth;
       if BmpHeight>0 then
        XForm.eM22:=H/BmpHeight;
       DeleteObject(R.Region);
       R.Region:=ExtCreateRegion(@XForm,B,RgnData^);
       FreeMem(RgnData);
      end;
    end;
  end;
 Result:=R.Region;
 if Resized and not SaveResized then
  begin
   R.Region:=0;
   R.Free;
  end else PreloadedRegions.Add(R);
end;

procedure DeletePreloadedRegions(SkinLibrary:TSXSkinCustomLibrary);
var A:Integer;
begin
 for A:=PreloadedRegions.Count-1 downto 0 do
  if PreloadedRegions[A].SkinLibrary=SkinLibrary then
   PreloadedRegions.Delete(A);
end;

function EvaluateRegion(const Region,SkinFilePath:String;ZipFilePath:String;
           Width,Height:Integer;SkinLibrary:TSXSkinLibrary;SaveResized:Boolean;
           AOnGetVariable:TSXOnGetVariable=nil):HRGN;
var     A,B,D:Integer;
       TmpRgn:HRGN;
      TmpRgn2:HRGN;
  ParserState:(psNeedType,
               psNeedRectCoord,
               psNeedEllipseCoord,
               psNeedRoundRectCoord,
               psNeedPolygonCoord,
               psNeedImagePathOffset,
               psNeedImagePath,
               psNeedImageOffsetX,
               psNeedImageOffsetY,
               psNeedImageWidth,
               psNeedImageHeight,
               psNeedImageColor,
               psNeedImageColorMask,
               psNeedBoxTilePathOffset,
               psNeedBoxTilePath,
               psNeedBoxTileRSMOffset,
               psNeedBoxTileRSM,
               psNeedBoxTileOffsetX,
               psNeedBoxTileOffsetY,
               psNeedBoxTileWidth,
               psNeedBoxTileHeight,
               psNeedBoxTileColor,
               psNeedBoxTileColorMask,
               psNeedBoxTileLeft,
               psNeedBoxTileRight,
               psNeedBoxTileTop,
               psNeedBoxTileBottom);
    Operation:(opIntersect,
               opCombine,
               opDifference,
               opExclusiveOr);
      Inverse:Boolean;
   PolyCoords:array of TPoint;
       Coords:array of Integer;
   PolyCoordN:Integer;
     PolyGotX:Boolean;
    CurParamN:Integer;
  ImagePath,S:String;
   ImageWidth:Integer;
  ImageHeight:Integer;
 ImageOffsetX:Integer;
 ImageOffsetY:Integer;
        Color:TColor32;
    ColorMask:TColor32;
       BTLeft:Integer;
      BTRight:Integer;
        BTTop:Integer;
     BTBottom:Integer;
    BTCenterW:Integer;
    BTCenterH:Integer;    
    RSMIsTile:Boolean;

 procedure SetWhatGot;
 var S2:String;
      E:Boolean;
   CM,C:Integer;
     R2:HRGN;
 begin
  if B>A then
   begin
    S2:=Copy(Region,A,B-A);
    case ParserState of
     psNeedType:              begin
                               if S2='Full' then
                                begin
                                 DeleteObject(Result);
                                 Result:=CreateRectRgn(0,0,Width,Height);
                                 exit;
                                end else
                                 begin
                                  Operation:=opCombine;
                                  Inverse:=False;
                                  while S2<>'' do
                                   begin
                                    case S2[1] of
                                     '!',
                                     '~': Inverse:=not Inverse;
                                     '+',
                                     '&': Operation:=opCombine;
                                     '*': Operation:=opIntersect;
                                     '-': Operation:=opDifference;
                                     '^': Operation:=opExclusiveOr;
                                     else break;
                                    end;
                                    Delete(S2,1,1);
                                   end;
                                  if S2='Rect' then
                                   ParserState:=psNeedRectCoord else
                                  if S2='Ellipse' then
                                   ParserState:=psNeedEllipseCoord else
                                  if S2='RoundRect' then
                                   ParserState:=psNeedRoundRectCoord else
                                  if S2='Polygon' then
                                   ParserState:=psNeedPolygonCoord else
                                  if S2='Image' then
                                   begin
                                    ParserState:=psNeedImagePath;
                                    ImageOffsetX:=0;
                                    ImageOffsetY:=0;
                                   end else
                                  if S2='ImageO' then
                                   ParserState:=psNeedImagePathOffset else
                                  if S2='BoxTile' then
                                   begin
                                    ParserState:=psNeedBoxTilePath;
                                    ImageOffsetX:=0;
                                    ImageOffsetY:=0;
                                   end else
                                  if S2='BoxTileO' then
                                   ParserState:=psNeedBoxTilePathOffset;
                                  CurParamN:=1;
                                  PolyCoordN:=0;
                                  PolyGotX:=False;
                                 end;
                              end;
     psNeedRectCoord,
     psNeedEllipseCoord,
     psNeedRoundRectCoord,
     psNeedPolygonCoord:      begin
                               if @AOnGetVariable=nil then
                                D:=StrToIntDef(S2,0) else
                                 begin
                                  E:=False;
                                  D:=round(SXEvalMathString(S2,AOnGetVariable,E));
                                  if E then D:=0;
                                 end;
                               if ParserState=psNeedPolygonCoord then
                                begin
                                 if CurParamN=1 then
                                  begin
                                   PolyCoordN:=D;
                                   SetLength(PolyCoords,D);
                                  end else
                                   begin
                                    if PolyGotX then
                                     PolyCoords[(CurParamN-2) div 2].Y:=D else
                                      PolyCoords[(CurParamN-2) div 2].X:=D;
                                    PolyGotX:=not PolyGotX;
                                   end;
                                 if CurParamN=1+2*PolyCoordN then
                                  begin
                                   ParserState:=psNeedType;
                                   TmpRgn:=CreatePolygonRgn(PolyCoords[0],PolyCoordN,ALTERNATE);
//                                   TmpRgn:=CreateRectRgn(PolyCoords[0].X,PolyCoords[0].Y,PolyCoords[1].X,PolyCoords[1].Y);
                                  end;
                                end else
                               if ParserState=psNeedRectCoord then
                                begin
                                 if CurParamN=1 then
                                  SetLength(Coords,4);
                                 Coords[CurParamN-1]:=D;
                                 if CurParamN=4 then
                                  begin
                                   ParserState:=psNeedType;
                                   TmpRgn:=CreateRectRgn(Coords[0],Coords[1],Coords[2],Coords[3]);
                                  end;
                                end else
                               if ParserState=psNeedRoundRectCoord then
                                begin
                                 if CurParamN=1 then
                                  SetLength(Coords,5);
                                 Coords[CurParamN-1]:=D;
                                 if CurParamN=5 then
                                  begin
                                   ParserState:=psNeedType;
                                   TmpRgn:=CreateRoundRectRgn(Coords[0],Coords[1],Coords[2],Coords[3],Coords[4]*2,Coords[4]*2);
                                  end;
                                end else
                               if ParserState=psNeedEllipseCoord then
                                begin
                                 if CurParamN=1 then
                                  SetLength(Coords,4);
                                 Coords[CurParamN-1]:=D;
                                 if CurParamN=4 then
                                  begin
                                   ParserState:=psNeedType;
                                   TmpRgn:=CreateEllipticRgn(Coords[0],Coords[1],Coords[2],Coords[3]);
                                  end;
                                end;
                               Inc(CurParamN);
                              end;
     psNeedImagePath:         begin
                               ImagePath:=S2;
                               ParserState:=psNeedImageWidth;
                              end;
     psNeedImagePathOffset:   begin
                               ImagePath:=S2;
                               ParserState:=psNeedImageOffsetX;
                              end;
     psNeedImageOffsetX:      begin
                               ImageOffsetX:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageOffsetX:=0;
                               ParserState:=psNeedImageOffsetY;
                              end;
     psNeedImageOffsetY:      begin
                               ImageOffsetY:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageOffsetY:=0;
                               ParserState:=psNeedImageWidth;
                              end;
     psNeedImageWidth:        begin
                               ImageWidth:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageWidth:=0;
                               ParserState:=psNeedImageHeight;
                              end;
     psNeedImageHeight:       begin
                               ImageHeight:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageHeight:=0;
                               ParserState:=psNeedImageColor;
                              end;
     psNeedImageColor:        begin
                               Color:=GetColorFromString(S2);
                               ParserState:=psNeedImageColorMask;
                              end;
     psNeedImageColorMask:    begin
                               ColorMask:=GetColorFromString(S2);
                               ParserState:=psNeedType;
                               if not ExtractZipPath(ImagePath,S) then
                                ImagePath:=GetFullPath(ImagePath,SkinFilePath) else
                                 ZipFilePath:=S;
                               TmpRgn:=CreateRectRgn(0,0,0,0);
                               CombineRgn(TmpRgn,GetPreloadedRegion(SkinLibrary,False,ImagePath,ZipFilePath,
                                                        (ImageWidth<>0) or (ImageHeight<>0),ImageWidth,
                                                        ImageHeight,SaveResized,Color,ColorMask),
                                          0,RGN_COPY);
                               if (ImageOffsetX<>0) or (ImageOffsetY<>0) then
                                OffsetRgn(TmpRgn,ImageOffsetX,ImageOffsetY);
                              end;
     psNeedBoxTilePathOffset: begin
                               ImagePath:=S2;
                               ParserState:=psNeedBoxTileRSMOffset;
                              end;
     psNeedBoxTilePath:       begin
                               ImagePath:=S2;
                               ParserState:=psNeedBoxTileRSM;
                              end;
     psNeedBoxTileRSMOffset:  begin
                               RSMIsTile:=SameText(S2,'Tile');
                               ParserState:=psNeedBoxTileOffsetX;
                              end;
     psNeedBoxTileRSM:        begin
                               RSMIsTile:=SameText(S2,'Tile');
                               ParserState:=psNeedBoxTileWidth;
                              end;
     psNeedBoxTileOffsetX:    begin
                               ImageOffsetX:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageOffsetX:=0;
                               ParserState:=psNeedBoxTileOffsetY;
                              end;
     psNeedBoxTileOffsetY:    begin
                               ImageOffsetY:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageOffsetY:=0;
                               ParserState:=psNeedBoxTileWidth;
                              end;
     psNeedBoxTileWidth:      begin
                               ImageWidth:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageWidth:=0;
                               ParserState:=psNeedBoxTileHeight;
                              end;
     psNeedBoxTileHeight:     begin
                               ImageHeight:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then ImageHeight:=0;
                               ParserState:=psNeedBoxTileColor;
                              end;
     psNeedBoxTileColor:      begin
                               Color:=GetColorFromString(S2);
                               ParserState:=psNeedBoxTileColorMask;
                              end;
     psNeedBoxTileColorMask:  begin
                               ColorMask:=GetColorFromString(S2);
                               ParserState:=psNeedBoxTileLeft;
                              end;
     psNeedBoxTileLeft:       begin
                               BTLeft:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then BTLeft:=0;
                               ParserState:=psNeedBoxTileRight;
                              end;
     psNeedBoxTileRight:      begin
                               BTRight:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then BTRight:=0;
                               ParserState:=psNeedBoxTileTop;
                              end;
     psNeedBoxTileTop:        begin
                               BTTop:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then BTTop:=0;
                               ParserState:=psNeedBoxTileBottom;
                              end;
     psNeedBoxTileBottom:     begin
                               BTBottom:=round(SXEvalMathString(S2,AOnGetVariable,E));
                               if E then BTBottom:=0;
                               ParserState:=psNeedType;
                               if not ExtractZipPath(ImagePath,S) then
                                ImagePath:=GetFullPath(ImagePath,SkinFilePath) else
                                 ZipFilePath:=S;
                               BTCenterW:=ImageWidth-BTLeft-BTRight;
                               BTCenterH:=ImageHeight-BTTop-BTBottom;
                               if BTCenterW<0 then
                                begin
                                 Inc(BTLeft,BTCenterW div 2);
                                 Inc(BTRight,BTCenterW-(BTCenterW div 2));
                                 BTCenterW:=0;
                                 if BTRight<0 then
                                  begin
                                   Inc(BTLeft,BTRight);
                                   BTRight:=0;
                                  end;
                                end;
                               if BTCenterH<0 then
                                begin
                                 Inc(BTTop,BTCenterH div 2);
                                 Inc(BTBottom,BTCenterH-(BTCenterH div 2));
                                 BTCenterH:=0;
                                 if BTBottom<0 then
                                  begin
                                   Inc(BTTop,BTBottom);
                                   BTBottom:=0;
                                  end;
                                end;
                               TmpRgn:=CreateRectRgn(0,0,0,0);
                               for C:=1 to 9 do
                                begin
                                 TmpRgn2:=0;
                                 case C of
                                  1: begin
                                      //Top Left
                                      if (BTTop>0) and (BTLeft>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,False,
                                           StringReplace(ImagePath,'*','topleft',[]),
                                           ZipFilePath,True,BTLeft,BTTop,SaveResized,
                                           Color,ColorMask);
                                     end;
                                  2: begin
                                      //Top
                                      if (BTTop>0) and (BTCenterW>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,RSMIsTile,
                                           StringReplace(ImagePath,'*','top',[]),
                                           ZipFilePath,True,BTCenterW,BTTop,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft,0);
                                     end;
                                  3: begin
                                      //Top Right
                                      if (BTTop>0) and (BTRight>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,False,
                                           StringReplace(ImagePath,'*','topright',[]),
                                           ZipFilePath,True,BTRight,BTTop,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft+BTCenterW,0);
                                     end;
                                  4: begin
                                      //Left
                                      if (BTCenterH>0) and (BTLeft>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,RSMIsTile,
                                           StringReplace(ImagePath,'*','left',[]),
                                           ZipFilePath,True,BTLeft,BTCenterH,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,0,BTTop);
                                     end;
                                  5: begin
                                      //Center
                                      if (BTCenterH>0) and (BTCenterW>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,RSMIsTile,
                                           StringReplace(ImagePath,'*','center',[]),
                                           ZipFilePath,True,BTCenterW,BTCenterH,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft,BTTop);
                                     end;
                                  6: begin
                                      //Right
                                      if (BTCenterH>0) and (BTRight>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,RSMIsTile,
                                           StringReplace(ImagePath,'*','right',[]),
                                           ZipFilePath,True,BTRight,BTCenterH,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft+BTCenterW,BTTop);
                                     end;
                                  7: begin
                                      //Bottom Left
                                      if (BTBottom>0) and (BTLeft>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,False,
                                           StringReplace(ImagePath,'*','bottomleft',[]),
                                           ZipFilePath,True,BTLeft,BTBottom,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,0,BTTop+BTCenterH);
                                     end;
                                  8: begin
                                      //Bottom
                                      if (BTBottom>0) and (BTCenterW>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,RSMIsTile,
                                           StringReplace(ImagePath,'*','bottom',[]),
                                           ZipFilePath,True,BTCenterW,BTBottom,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft,BTTop+BTCenterH);
                                     end;
                                  9: begin
                                      //Bottom Right
                                      if (BTBottom>0) and (BTRight>0) then
                                       TmpRgn2:=GetPreloadedRegion(SkinLibrary,False,
                                           StringReplace(ImagePath,'*','bottomright',[]),
                                           ZipFilePath,True,BTRight,BTBottom,SaveResized,
                                           Color,ColorMask);
                                      OffsetRgn(TmpRgn2,BTLeft+BTCenterW,BTTop+BTCenterH);
                                     end;
                                 end;
                                 if TmpRgn2<>0 then
                                  begin
                                   CombineRgn(TmpRgn,TmpRgn,TmpRgn2,RGN_OR);
                                   DeleteObject(TmpRgn2);
                                  end;
                                end;
                               if (ImageOffsetX<>0) or (ImageOffsetY<>0) then
                                OffsetRgn(TmpRgn,ImageOffsetX,ImageOffsetY);
                              end;
    end;
    if ParserState=psNeedType then
     begin
      if Inverse then
       begin
        R2:=CreateRectRgn(0,0,Width,Height);
        CombineRgn(TmpRgn,R2,TmpRgn,RGN_DIFF);
        DeleteObject(R2);
       end;
      case Operation of
       opIntersect:   CM:=RGN_AND;
       opCombine:     CM:=RGN_OR;
       opDifference:  CM:=RGN_DIFF;
       opExclusiveOr: CM:=RGN_XOR;
       else           CM:=0;
      end;
      CombineRgn(Result,Result,TmpRgn,CM);
      DeleteObject(TmpRgn);
     end;
   end;
 end;

begin
 ParserState:=psNeedType;
 CurParamN:=0;
 Result:=CreateRectRgn(0,0,0,0);
 A:=1; B:=1;
 while B<=length(Region) do
  begin
   if Region[B]=',' then
    begin
     SetWhatGot;
     Inc(B);
     A:=B;
    end else Inc(B);
  end;
 SetWhatGot;
end;

initialization

 PreloadedRegions:=TSXSkinPreloadedRegionList.Create;

finalization

 PreloadedRegions.Free;

end.
