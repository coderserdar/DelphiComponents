unit SXSkinLibrary;

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

{$R WinXP.RES}
{$R InternalZip.RES}

uses Windows, Classes, Forms, SysUtils, IniFiles, GR32, Controls, GR32_Blend,
     SXMathEval, GR32_Polygons, Dialogs, Graphics, Types, SXBitmap32Utils,
     StdCtrls, GR32_Resamplers, SXZipUtils;

const SXSkinComponents_Version:Integer=10200;
      SXSkinComponents_VersionStr:String='1.2.0';
      SXS_Header='SXSkinComponents Binary Skin';
      SXSuppVersion:array[0..1]of Integer=(10100, 10200);

      VARSTD_W = 1;
      VARSTD_H = 2;

type

 {$IFNDEF COMPILER_9_UP}
 TVerticalAlignment=(taAlignTop,
                     taAlignBottom,
                     taVerticalCenter);
 {$ENDIF}

 TSXGlyphPosition=(gpLeftTop,      gpTop,
                   gpRightTop,     gpRight,
                   gpRightBottom,  gpBottom,
                   gpLeftBottom,   gpLeft);

 TSXGlyphChangeAction=(gcaHighlightIn,
                       gcaHighlightOut,
                       gcaDown,
                       gcaUp,
                       gcaCheck,
                       gcaUncheck,
                       gcaEnable,
                       gcaDisable,
                       gcaFocus,
                       gcaUnfocus);

 TSXTransformEffectType=(tetBlend,
                         tetNone,
                         tetClear,
                         tetFade,
                         tetOverDraw,
                         tetSlide);

 TSXTransformEffectDirection=(tedLeft,
                              tedRight,
                              tedTop,
                              tedBottom,
                              tedLeftRight,
                              tedTopBottom,
                              tedLeftTop,
                              tedLeftBottom,
                              tedRightTop,
                              tedRightBottom);

 TSXTransformEffectData=record
  SetOldType:Boolean;
  SetOldDirection:Boolean;
  SetOldDirOut:Boolean;
  SetOldInverted:Boolean;
  SetOldOnTop:Boolean;
  SetNewType:Boolean;
  SetNewDirection:Boolean;
  SetNewDirOut:Boolean;
  SetNewInverted:Boolean;
  SetStepsNum:Boolean;
  SetOffset:Boolean;
  SetDrawCaption:Boolean;
  //
  OldType:TSXTransformEffectType;
  OldDirection:TSXTransformEffectDirection;
  OldDirOut:Boolean;
  OldInverted:Boolean;
  OldOnTop:Boolean;
  NewType:TSXTransformEffectType;
  NewDirection:TSXTransformEffectDirection;
  NewDirOut:Boolean;
  NewInverted:Boolean;
  StepsNum:Integer;
  Offset:Integer;
  DrawCaption:Boolean;
 end;

 TSXSkinStyleElementType=(ssetUnknown,
                          ssetImage,
                          ssetBoxTile,
                          ssetFigure,
                          ssetText,
                          ssetStyle);

 TSXVariableComparer=class;

 ISXVariableComparerNotify=interface
  procedure VCNotification(VComparer:TSXVariableComparer);
 end;

 TSXVariableComparer=class
  protected
   CurValList:TList;
   function VarListOnGetVariable(const VarName:String;var Error:Boolean):Single; virtual; abstract;
  public
   OnGetVariable:TSXOnGetVariable;
   function GetVarListForRect(const S:String):TList; virtual;
   function GetVarListForPoly(const S:String):TList; virtual;
   function GetVarValsForVarList(VarList:TList):TList; virtual; abstract;
   function Changed(VarList:TList;OldVarVals:TList):Boolean; virtual; abstract;
   procedure Update(VarList:TList;VarVals:TList); virtual; abstract;
   procedure DestroyVarList(VarList:TList); virtual; abstract;
   procedure DestroyVarVals(VarList:TList;VarVals:TList); virtual; abstract;
 end;

 TSXStdVariableComparer=class(TSXVariableComparer)
  private
   Width:Integer;
   Height:Integer;
   function GetValue(VarID:Integer):Integer;
   function IntOnGetVariable(const VarName:String;var Error:Boolean):Single;
  protected
   function VarListOnGetVariable(const VarName:String;var Error:Boolean):Single; override;
  public
   procedure SetSize(AWidth,AHeight:Integer);
   procedure GetSize(var AWidth,AHeight:Integer);
   function GetVarValsForVarList(VarList:TList):TList; override;
   function Changed(VarList:TList;OldVarVals:TList):Boolean; override;
   procedure Update(VarList:TList;VarVals:TList); override;
   procedure DestroyVarList(VarList:TList); override;
   procedure DestroyVarVals(VarList:TList;VarVals:TList); override;
   constructor Create;
 end;

 TSXSkinStyleElement=class
  private
   FControl:TControl;
   FCElementID:Integer;
  public
   Name:String;
   procedure Assign(Element:TSXSkinStyleElement); virtual;
   function GetCopy:TSXSkinStyleElement; virtual;
   procedure SetProperty(const Name,Value,SkinFilePath:String); virtual;
   procedure SaveToStream(S:TStream;const RootPath:String); virtual;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); virtual;
 end;

 TSXSkinStyleElementList=class
  protected
   FItem:TList;
   function Get(Index:Integer):TSXSkinStyleElement;
   procedure Put(Index:Integer;Item:TSXSkinStyleElement);
   function GetCount:Integer;
  public
   procedure Assign(ElementList:TSXSkinStyleElementList);
   function GetIndexByName(const Name:String):Integer;
   procedure Add(SXSkinStyleElement:TSXSkinStyleElement);
   procedure Delete(Index:Integer);
   procedure SaveToStream(S:TStream;const RootPath:String);
   procedure LoadFromStream(S:TStream;Version:Integer;const RootPath,ZipFilePath:String;
              DoClear:Boolean=True);
   procedure Clear;
   constructor Create;
   destructor Destroy; override;
   property Item[Index:Integer]:TSXSkinStyleElement read Get write Put; default;
   property Count:Integer read GetCount;
 end;

 TSXSkinStyleImageType=(ssitAutoDetect,
                        ssitJPEG,
                        ssitPNG);

 TSXSkinLibrary=class;

 TSXImageFilterType=(iftNone,
                     iftLighten,
                     iftLightenHorizG,
                     iftLightenVertG,
                     iftDarken,
                     iftDarkenHorizG,
                     iftDarkenVertG,
                     iftAlpha,
                     iftMonochrome,
                     iftColorOverlay,
                     iftColorOverlayHorizG,
                     iftColorOverlayVertG);

 TSXFilterData=record
  SetAType:Boolean;
  SetValue:Boolean;
  SetColor:Boolean;
  SetColor2:Boolean;
  //
  AType:TSXImageFilterType;
  Value:Integer;
  Color:TColor32;
  Color2:TColor32;
 end;

 TSXImageResizeMode=(irmNone,
                     irmTile,
                     irmStretch);

 TSXImageStretchFilter=(isfNearest,
                        isfLinear,
                        isfSpline,
                        isfLanczos,
                        isfMitchell);

 TSXSkinStyleImageElement=class(TSXSkinStyleElement)
  public
   ImageType:TSXSkinStyleImageType;
   LoadedPath:String; //Is used for loading and saving only
   Path:String;
   ZipFilePath:String;
   Transparent:Boolean;
   Bitmap:TBitmap32;
   Filter:TSXFilterData;
   ResizeMode:TSXImageResizeMode;
   StretchFilter:TSXImageStretchFilter;
   OffsetX:Integer;
   OffsetY:Integer;
   ResizedWidth:Integer;
   ResizedHeight:Integer;
   Centered:Boolean;
   DrawRect:String;
   procedure Assign(Element:TSXSkinStyleElement); override;
   function GetCopy:TSXSkinStyleElement; override;
   procedure SetProperty(const Name,Value,SkinFilePath:String); override;
   procedure ValidateBitmap(SkinLibrary:TSXSkinLibrary);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); override;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinStyleStyleElement=class(TSXSkinStyleElement)
  public
   Style:String;
   Filter:TSXFilterData;
   DrawRect:String;
   procedure Assign(Element:TSXSkinStyleElement); override;
   function GetCopy:TSXSkinStyleElement; override;
   procedure SetProperty(const Name,Value,SkinFilePath:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); override;
 end;

 TSXSkinStyleBoxTileElement=class(TSXSkinStyleElement)
  public
   ZipFilePath:String;
   CenterPath:String;
   LeftPath:String;
   TopPath:String;
   RightPath:String;
   BottomPath:String;
   TopLeftPath:String;
   TopRightPath:String;
   BottomRightPath:String;
   BottomLeftPath:String;
   //
   CenterColor:TColor32;
   LeftColor:TColor32;
   TopColor:TColor32;
   RightColor:TColor32;
   BottomColor:TColor32;
   TopLeftColor:TColor32;
   TopRightColor:TColor32;
   BottomRightColor:TColor32;
   BottomLeftColor:TColor32;
   //
   SetCenterColor:Boolean;
   SetLeftColor:Boolean;
   SetTopColor:Boolean;
   SetRightColor:Boolean;
   SetBottomColor:Boolean;
   SetTopLeftColor:Boolean;
   SetTopRightColor:Boolean;
   SetBottomRightColor:Boolean;
   SetBottomLeftColor:Boolean;
   SetResizeMode:Boolean;
   //
   SetCenter:Boolean;
   SetLeft:Boolean;
   SetTop:Boolean;
   SetRight:Boolean;
   SetBottom:Boolean;
   SetTopLeft:Boolean;
   SetTopRight:Boolean;
   SetBottomRight:Boolean;
   SetBottomLeft:Boolean;
   //
   CenterBitmap:TBitmap32;
   LeftBitmap:TBitmap32;
   TopBitmap:TBitmap32;
   RightBitmap:TBitmap32;
   BottomBitmap:TBitmap32;
   TopLeftBitmap:TBitmap32;
   TopRightBitmap:TBitmap32;
   BottomRightBitmap:TBitmap32;
   BottomLeftBitmap:TBitmap32;
   Transparent:Boolean;
   Filter:TSXFilterData;
   ResizeMode:TSXImageResizeMode;
   //
   LoadedPathTemplate:String; //Is used for loading and saving only
   PathTemplate:String;
   DrawRect:String;
   procedure Assign(Element:TSXSkinStyleElement); override;
   function GetCopy:TSXSkinStyleElement; override;
   function GetPixelAt(PX,PY,Width,Height:Integer;AOnGetVariable:TSXOnGetVariable=nil):TColor32;
   procedure DrawToBitmap(B:TBitmap32;X,Y,Width,Height:Integer;DrawRgn:HRGN;
              AOnGetVariable:TSXOnGetVariable=nil);
   procedure SetProperty(const Name,Value,SkinFilePath:String); override;
   procedure ValidateBitmaps(SkinLibrary:TSXSkinLibrary);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); override;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinStyleFigureType=(ssftRectangle,
                         ssftEllipse,
                         ssftRoundRectangle,
                         ssftLine,
                         ssftSolidFill,
                         ssftFocusRectangle,
                         ssftPolygon,
                         ssftEraseRect);

 TSXFigureControlPreparedData=class
  private
   Control:TControl;
   CElementID:Integer;
   RectValueVarList:TList;
   RectValueVarVals:TList;
   PolyValueVarList:TList;
   PolyValueVarVals:TList;
   VComparer:TSXVariableComparer;
   Rect:TRect;
   Polygon1:TPolygon32;
   Polygon1Offset:TPoint;
   Polygon2:TPolygon32;
   Polygon2Offset:TPoint;
  public
   destructor Destroy; override;
 end;

 TSXFigureControlPreparedDataList=class
  protected
   FItem:TList;
   function Get(Index:Integer):TSXFigureControlPreparedData;
   procedure Put(Index:Integer;Item:TSXFigureControlPreparedData);
   function GetCount:Integer;
  public
   function GetIndexToInsert(CElementID:Integer):Integer;
   function GetIndexByControl(CElementID:Integer):Integer;
   procedure Add(SXFigureControlPreparedData:TSXFigureControlPreparedData);
   procedure Delete(Index:Integer);
   procedure Clear;
   constructor Create;
   destructor Destroy; override;
   property Item[Index:Integer]:TSXFigureControlPreparedData read Get write Put; default;
   property Count:Integer read GetCount;
 end;

 TSXSkinStyleFigureElement=class(TSXSkinStyleElement)
  public
   HasBorder:Boolean;
   HasFill:Boolean;
   Antialiased:Boolean;
   FigureType:TSXSkinStyleFigureType;
   BorderColor:TColor32;
   FillColor:TColor32;
   FillColor2:TColor32;
   GradientFill:Boolean;
   VertGradientFill:Boolean;
   Roundness:Integer;
   RectValue:String;
   PolyValue:String;
   CornersStyle:TSXCorners;
   BorderThickness:Single;
   ControlsData:TSXFigureControlPreparedDataList;
   procedure Assign(Element:TSXSkinStyleElement); override;
   function GetCopy:TSXSkinStyleElement; override;
   procedure DrawToBitmap(B:TBitmap32;X,Y:Integer);
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure EvalPrePaintParams(Control:TControl;CElementID:Integer;
              VComparer:TSXVariableComparer=nil;Changed:Boolean=False);
   procedure SetProperty(const Name,Value,SkinFilePath:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); override;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXFontData=record
  SetFontName:Boolean;
  SetFontSize:Boolean;
  SetFontStyle:Boolean;
  SetFontColor:Boolean;
  SetHasShadow:Boolean;
  SetShadowColor:Boolean;
  SetSmoothLevel:Boolean;
  SetDoPrepaint:Boolean;
  //
  FontName:String;
  FontSize:Integer;
  FontStyle:TFontStyles;
  FontColor:TColor32;
  HasShadow:Boolean;
  ShadowColor:TColor32;
  SmoothLevel:Integer;
  DoPrepaint:Boolean;
 end;

 TSXSkinStyleTextElement=class(TSXSkinStyleElement)
  public
   Alignment:TAlignment;
   VerticalAlignment:TVerticalAlignment;
   RectValue:String;
   Text:String;
   FontData:TSXFontData;
   TextRect:TRect;
   procedure Assign(Element:TSXSkinStyleElement); override;
   function GetCopy:TSXSkinStyleElement; override;
   procedure DrawToBitmap(B:TBitmap32;X,Y:Integer);
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure EvalPrePaintParams(Control:TControl;CElementID:Integer;
              VComparer:TSXVariableComparer=nil;Changed:Boolean=False);
   procedure SetProperty(const Name,Value,SkinFilePath:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String); override;
 end;

 TSXSkinStyleType=(sstUnknown,
                   sstGeneral,
                   sstLabel,
                   sstCheckBox,
                   sstRadioButton,
                   sstGroupBox,
                   sstMultiState,
                   sstMultiStateCheck,
                   sstButton,
                   sstEdit,
                   sstSelective,
                   sstForm,
                   sstUpDown,
                   sstSpinEdit);

 TSXSkinStyleList=class;

 TSXSkinStyle=class
  public
   Name:String;
   BasedOnList:TStringList;
   procedure AssignStyle(Style:TSXSkinStyle); virtual;
   procedure SetParameter(const Name,Value:String); virtual;
   procedure SaveToStream(S:TStream;const RootPath:String); virtual;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); virtual;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinGeneralStyle=class(TSXSkinStyle)
  public
   ZipFilePath:String;
   SkinFilePath:String;
   SetTransparent:Boolean;
   SetUseBuffering:Boolean;
   SetMouseCaptureByTransparency:Boolean;
   SetMouseCaptureTransparencyLimit:Boolean;
   SetMouseCaptureRegion:Boolean;
   //
   Transparent:Boolean;
   UseBuffering:Boolean;
   MouseCaptureByTransparency:Boolean;
   MouseCaptureTransparencyLimit:Integer;
   MouseCaptureRegion:String;
   Elements:TSXSkinStyleElementList;
   //
   TmpWidth:Integer;
   TmpHeight:Integer;
   TmpOnGetVariable:TSXOnGetVariable;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure DrawToBitmap(Control:TControl;CElementID:Integer;
              Bitmap:TBitmap32;X,Y,Width,Height:Integer;Rect:TRect;Rgn:HRGN;
              SkinLibrary:TSXSkinLibrary;VComparer:TSXVariableComparer=nil;
              ForceEvalParams:Boolean=False);
   function NestedOnGetVariable(const VarName:String;var Error:Boolean):Single;
   function NeedToPaintBackground:Boolean;
   function IsTransparent(Control:TControl;CElementID:Integer;
             X,Y,Width,Height:Integer;SkinLibrary:TSXSkinLibrary;Limit:Integer=10;
             VComparer:TSXVariableComparer=nil;ForceEvalParams:Boolean=False):Boolean;
   function CapturesMouseAt(Control:TControl;CElementID:Integer;
             X,Y,Width,Height:Integer;SkinLibrary:TSXSkinLibrary;
             VComparer:TSXVariableComparer=nil):Boolean;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinSelectiveStyle=class(TSXSkinStyle)
  public
   Conditions:TStringList;
   Styles:TStringList;
   DefaultStyle:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   function GetAppropriateStyle(AOnGetVariable:TSXOnGetVariable):String;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinLabelStateParam=record
  FD:TSXFontData;
 end;
 PSXSkinLabelStateParam=^TSXSkinLabelStateParam;

 TSXSkinLabelStyle=class(TSXSkinStyle)
  public
   NState:TSXSkinLabelStateParam;
   HState:TSXSkinLabelStateParam;
   RState:TSXSkinLabelStateParam;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinCheckBoxStateParam=record
  SetStyle:Boolean;
  SetOverStyle:Boolean;
  SetGlyphStyle:Boolean;
  SetGlyphWidth:Boolean;
  SetGlyphHeight:Boolean;
  //
  Style:String;
  OverStyle:String;
  GlyphStyle:String;
  GlyphWidth:Integer;
  GlyphHeight:Integer;
  FD:TSXFontData;
  //
  SetCaptionLeftOffset:Boolean;
  SetCaptionTopOffset:Boolean;
  SetCaptionRightOffset:Boolean;
  SetCaptionBottomOffset:Boolean;
  CaptionLeftOffset:Integer;
  CaptionTopOffset:Integer;
  CaptionRightOffset:Integer;
  CaptionBottomOffset:Integer;
  //
  SetTextLeftOffset:Boolean;
  SetTextTopOffset:Boolean;
  SetTextRightOffset:Boolean;
  SetTextBottomOffset:Boolean;
  TextLeftOffset:Integer;
  TextTopOffset:Integer;
  TextRightOffset:Integer;
  TextBottomOffset:Integer;
 end;
 PSXSkinCheckBoxStateParam=^TSXSkinCheckBoxStateParam;

 TSXSkinCheckBoxStyle=class(TSXSkinStyle)
  public
   NUUState:TSXSkinCheckBoxStateParam;
   NFUState:TSXSkinCheckBoxStateParam;
   HUUState:TSXSkinCheckBoxStateParam;
   HFUState:TSXSkinCheckBoxStateParam;
   DFUState:TSXSkinCheckBoxStateParam;
   RUUState:TSXSkinCheckBoxStateParam;
   NUCState:TSXSkinCheckBoxStateParam;
   NFCState:TSXSkinCheckBoxStateParam;
   HUCState:TSXSkinCheckBoxStateParam;
   HFCState:TSXSkinCheckBoxStateParam;
   DFCState:TSXSkinCheckBoxStateParam;
   RUCState:TSXSkinCheckBoxStateParam;
   NUGState:TSXSkinCheckBoxStateParam;
   NFGState:TSXSkinCheckBoxStateParam;
   HUGState:TSXSkinCheckBoxStateParam;
   HFGState:TSXSkinCheckBoxStateParam;
   DFGState:TSXSkinCheckBoxStateParam;
   RUGState:TSXSkinCheckBoxStateParam;
   //
   HInCheckBoxEffect:TSXTransformEffectData;
   HOutCheckBoxEffect:TSXTransformEffectData;
   CheckCheckBoxEffect:TSXTransformEffectData;
   UncheckCheckBoxEffect:TSXTransformEffectData;
   DownCheckBoxEffect:TSXTransformEffectData;
   UpCheckBoxEffect:TSXTransformEffectData;
   EnableCheckBoxEffect:TSXTransformEffectData;
   DisableCheckBoxEffect:TSXTransformEffectData;
   FocusCheckBoxEffect:TSXTransformEffectData;
   UnfocusCheckBoxEffect:TSXTransformEffectData;
   //
   HInGlyphEffect:TSXTransformEffectData;
   HOutGlyphEffect:TSXTransformEffectData;
   CheckGlyphEffect:TSXTransformEffectData;
   UncheckGlyphEffect:TSXTransformEffectData;
   DownGlyphEffect:TSXTransformEffectData;
   UpGlyphEffect:TSXTransformEffectData;
   EnableGlyphEffect:TSXTransformEffectData;
   DisableGlyphEffect:TSXTransformEffectData;
   FocusGlyphEffect:TSXTransformEffectData;
   UnfocusGlyphEffect:TSXTransformEffectData;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentCBState(var CBState:TSXSkinCheckBoxStateParam;
              State:TCheckBoxState;Down,MouseOver,Focused,Enabled:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinRadioButtonStateParam=record
  SetStyle:Boolean;
  SetOverStyle:Boolean;
  SetGlyphStyle:Boolean;
  SetGlyphWidth:Boolean;
  SetGlyphHeight:Boolean;
  //
  Style:String;
  OverStyle:String;
  GlyphStyle:String;
  GlyphWidth:Integer;
  GlyphHeight:Integer;
  FD:TSXFontData;
  //
  SetCaptionLeftOffset:Boolean;
  SetCaptionTopOffset:Boolean;
  SetCaptionRightOffset:Boolean;
  SetCaptionBottomOffset:Boolean;
  CaptionLeftOffset:Integer;
  CaptionTopOffset:Integer;
  CaptionRightOffset:Integer;
  CaptionBottomOffset:Integer;
  //
  SetTextLeftOffset:Boolean;
  SetTextTopOffset:Boolean;
  SetTextRightOffset:Boolean;
  SetTextBottomOffset:Boolean;
  TextLeftOffset:Integer;
  TextTopOffset:Integer;
  TextRightOffset:Integer;
  TextBottomOffset:Integer;
 end;
 PSXSkinRadioButtonStateParam=^TSXSkinRadioButtonStateParam;

 TSXSkinRadioButtonStyle=class(TSXSkinStyle)
  public
   NUUState:TSXSkinRadioButtonStateParam;
   NFUState:TSXSkinRadioButtonStateParam;
   HUUState:TSXSkinRadioButtonStateParam;
   HFUState:TSXSkinRadioButtonStateParam;
   DFUState:TSXSkinRadioButtonStateParam;
   RUUState:TSXSkinRadioButtonStateParam;
   NUCState:TSXSkinRadioButtonStateParam;
   NFCState:TSXSkinRadioButtonStateParam;
   HUCState:TSXSkinRadioButtonStateParam;
   HFCState:TSXSkinRadioButtonStateParam;
   DFCState:TSXSkinRadioButtonStateParam;
   RUCState:TSXSkinRadioButtonStateParam;
   //
   HInRadioButtonEffect:TSXTransformEffectData;
   HOutRadioButtonEffect:TSXTransformEffectData;
   CheckRadioButtonEffect:TSXTransformEffectData;
   UncheckRadioButtonEffect:TSXTransformEffectData;
   DownRadioButtonEffect:TSXTransformEffectData;
   UpRadioButtonEffect:TSXTransformEffectData;
   EnableRadioButtonEffect:TSXTransformEffectData;
   DisableRadioButtonEffect:TSXTransformEffectData;
   FocusRadioButtonEffect:TSXTransformEffectData;
   UnfocusRadioButtonEffect:TSXTransformEffectData;
   //
   HInGlyphEffect:TSXTransformEffectData;
   HOutGlyphEffect:TSXTransformEffectData;
   CheckGlyphEffect:TSXTransformEffectData;
   UncheckGlyphEffect:TSXTransformEffectData;
   DownGlyphEffect:TSXTransformEffectData;
   UpGlyphEffect:TSXTransformEffectData;
   EnableGlyphEffect:TSXTransformEffectData;
   DisableGlyphEffect:TSXTransformEffectData;
   FocusGlyphEffect:TSXTransformEffectData;
   UnfocusGlyphEffect:TSXTransformEffectData;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentRBState(var RBState:TSXSkinRadioButtonStateParam;
              Checked,Down,MouseOver,Focused,Enabled:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinGroupBoxStateParam=record
  SetStyle:Boolean;
  SetOverStyle:Boolean;
  SetCaptionPosition:Boolean;
  SetTransparentRect:Boolean;
  //
  Style:String;
  OverStyle:String;
  CaptionPosition:Integer;
  //
  SetCaptionLeftOffset:Boolean;
  SetCaptionTopOffset:Boolean;
  SetCaptionRightOffset:Boolean;
  SetCaptionBottomOffset:Boolean;
  CaptionLeftOffset:Integer;
  CaptionTopOffset:Integer;
  CaptionRightOffset:Integer;
  CaptionBottomOffset:Integer;
  //
  SetTextLeftOffset:Boolean;
  SetTextTopOffset:Boolean;
  SetTextRightOffset:Boolean;
  SetTextBottomOffset:Boolean;
  TextLeftOffset:Integer;
  TextTopOffset:Integer;
  TextRightOffset:Integer;
  TextBottomOffset:Integer;
  //
  TransparentRect:String;
 end;
 PSXSkinGroupBoxStateParam=^TSXSkinGroupBoxStateParam;

 TSXSkinGroupBoxStyle=class(TSXSkinStyle)
  public
   SetLabelStyle:Boolean;
   SetGlyphWidth:Boolean;
   SetGlyphHeight:Boolean;
   SetClientRect:Boolean;
   //
   LabelStyle:String;
   NUState:TSXSkinGroupBoxStateParam;
   NFState:TSXSkinGroupBoxStateParam;
   HUState:TSXSkinGroupBoxStateParam;
   HFState:TSXSkinGroupBoxStateParam;
   RUState:TSXSkinGroupBoxStateParam;
   GlyphWidth:Integer;
   GlyphHeight:Integer;
   ClientRect:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentGBState(var GBState:TSXSkinGroupBoxStateParam;
              MouseOver,Focused,Enabled:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinMultiStateStyle=class(TSXSkinStyle)
  public
   SetNStyle:Boolean;
   SetHStyle:Boolean;
   SetDStyle:Boolean;
   SetRStyle:Boolean;
   //
   NStyle:String;
   HStyle:String;
   DStyle:String;
   RStyle:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinMultiStateCheckStyle=class(TSXSkinStyle)
  public
   SetNUStyle:Boolean;
   SetHUStyle:Boolean;
   SetDUStyle:Boolean;
   SetRUStyle:Boolean;
   SetNCStyle:Boolean;
   SetHCStyle:Boolean;
   SetDCStyle:Boolean;
   SetRCStyle:Boolean;
   //
   NUStyle:String;
   HUStyle:String;
   DUStyle:String;
   RUStyle:String;
   NCStyle:String;
   HCStyle:String;
   DCStyle:String;
   RCStyle:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinButtonStateParam=record
  SetStyle:Boolean;
  SetOverStyle:Boolean;
  SetCaptionLeftOffset:Boolean;
  SetCaptionTopOffset:Boolean;
  SetCaptionRightOffset:Boolean;
  SetCaptionBottomOffset:Boolean;
  //
  Style:String;
  OverStyle:String;
  FD:TSXFontData;
  CaptionLeftOffset:Integer;
  CaptionTopOffset:Integer;
  CaptionRightOffset:Integer;
  CaptionBottomOffset:Integer;
  DefGlyphFilter:TSXFilterData;
 end;
 PSXSkinButtonStateParam=^TSXSkinButtonStateParam;

 TSXSkinButtonStyle=class(TSXSkinStyle)
  public
   NUUState:TSXSkinButtonStateParam;
   NFUState:TSXSkinButtonStateParam;
   HUUState:TSXSkinButtonStateParam;
   HFUState:TSXSkinButtonStateParam;
   DFUState:TSXSkinButtonStateParam;
   RUUState:TSXSkinButtonStateParam;
   NUCState:TSXSkinButtonStateParam;
   NFCState:TSXSkinButtonStateParam;
   HUCState:TSXSkinButtonStateParam;
   HFCState:TSXSkinButtonStateParam;
   DFCState:TSXSkinButtonStateParam;
   RUCState:TSXSkinButtonStateParam;
   //
   HInButtonEffect:TSXTransformEffectData;
   HOutButtonEffect:TSXTransformEffectData;
   CheckButtonEffect:TSXTransformEffectData;
   UncheckButtonEffect:TSXTransformEffectData;
   DownButtonEffect:TSXTransformEffectData;
   UpButtonEffect:TSXTransformEffectData;
   EnableButtonEffect:TSXTransformEffectData;
   DisableButtonEffect:TSXTransformEffectData;
   FocusButtonEffect:TSXTransformEffectData;
   UnfocusButtonEffect:TSXTransformEffectData;
   //
   HInGlyphEffect:TSXTransformEffectData;
   HOutGlyphEffect:TSXTransformEffectData;
   CheckGlyphEffect:TSXTransformEffectData;
   UncheckGlyphEffect:TSXTransformEffectData;
   DownGlyphEffect:TSXTransformEffectData;
   UpGlyphEffect:TSXTransformEffectData;
   EnableGlyphEffect:TSXTransformEffectData;
   DisableGlyphEffect:TSXTransformEffectData;
   FocusGlyphEffect:TSXTransformEffectData;
   UnfocusGlyphEffect:TSXTransformEffectData;
   //
   HInDDGlyphEffect:TSXTransformEffectData;
   HOutDDGlyphEffect:TSXTransformEffectData;
   CheckDDGlyphEffect:TSXTransformEffectData;
   UncheckDDGlyphEffect:TSXTransformEffectData;
   DownDDGlyphEffect:TSXTransformEffectData;
   UpDDGlyphEffect:TSXTransformEffectData;
   EnableDDGlyphEffect:TSXTransformEffectData;
   DisableDDGlyphEffect:TSXTransformEffectData;
   FocusDDGlyphEffect:TSXTransformEffectData;
   UnfocusDDGlyphEffect:TSXTransformEffectData;
   //
   SetDropDownGlyphWidth:Boolean;
   SetDropDownGlyphHeight:Boolean;
   SetDropDownGlyphOffset:Boolean;
   //
   DropDownGlyphWidth:Integer;
   DropDownGlyphHeight:Integer;
   DropDownGlyphOffset:Integer;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentBState(var BState:TSXSkinButtonStateParam;
              FChecked,Enabled,FMouseOver,FDown,FLastFocused:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinEditStateParam=record
  SetStyle:Boolean;
  Style:String;
  FD:TSXFontData;
 end;
 PSXSkinEditStateParam=^TSXSkinEditStateParam;

 TSXSkinEditStyle=class(TSXSkinStyle)
  public
   NUState:TSXSkinEditStateParam;
   NFState:TSXSkinEditStateParam;
   HUState:TSXSkinEditStateParam;
   HFState:TSXSkinEditStateParam;
   RUState:TSXSkinEditStateParam;
   //
   SetTextLeftOffset:Boolean;
   SetTextTopOffset:Boolean;
   SetTextRightOffset:Boolean;
   SetTextBottomOffset:Boolean;
   TextLeftOffset:Integer;
   TextTopOffset:Integer;
   TextRightOffset:Integer;
   TextBottomOffset:Integer;
   //
   HInEditEffect:TSXTransformEffectData;
   HOutEditEffect:TSXTransformEffectData;
   EnableEditEffect:TSXTransformEffectData;
   DisableEditEffect:TSXTransformEffectData;
   FocusEditEffect:TSXTransformEffectData;
   UnfocusEditEffect:TSXTransformEffectData;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinFormStateParam=record
  SetMaskLeftWidth:Boolean;
  SetMaskRightWidth:Boolean;
  SetMaskTopHeight:Boolean;
  SetMaskBottomHeight:Boolean;
  SetMaskTopLeft:Boolean;
  SetMaskTopRight:Boolean;
  SetMaskBottomLeft:Boolean;
  SetMaskBottomRight:Boolean;
  SetMaskLeft:Boolean;
  SetMaskRight:Boolean;
  SetMaskTop:Boolean;
  SetMaskBottom:Boolean;
  SetFullMask:Boolean;
  SetResizeTopLeft:Boolean;
  SetResizeTopRight:Boolean;
  SetResizeBottomLeft:Boolean;
  SetResizeBottomRight:Boolean;
  SetResizeLeft:Boolean;
  SetResizeRight:Boolean;
  SetResizeTop:Boolean;
  SetResizeBottom:Boolean;
  SetCaptionRegion:Boolean;
  SetCaptionStyle:Boolean;
  SetLeftFrameStyle:Boolean;
  SetRightFrameStyle:Boolean;
  SetBottomFrameStyle:Boolean;
  SetIconRect:Boolean;
  SetTextRect:Boolean;
  SetCloseRect:Boolean;
  SetMaximizeRect:Boolean;
  SetMinimizeRect:Boolean;
  SetHelpRect:Boolean;
  SetCloseButton:Boolean;
  SetMaximizeButton:Boolean;
  SetMinimizeButton:Boolean;
  SetRestoreButton:Boolean;
  SetHelpButton:Boolean;
  SetTextAlignment:Boolean;
  //
  MaskLeftWidth:Integer;
  MaskRightWidth:Integer;
  MaskTopHeight:Integer;
  MaskBottomHeight:Integer;
  MaskTopLeft:String;
  MaskTopRight:String;
  MaskBottomLeft:String;
  MaskBottomRight:String;
  MaskLeft:String;
  MaskRight:String;
  MaskTop:String;
  MaskBottom:String;
  FullMask:String;
  ResizeTopLeft:String;
  ResizeTopRight:String;
  ResizeBottomLeft:String;
  ResizeBottomRight:String;
  ResizeLeft:String;
  ResizeRight:String;
  ResizeTop:String;
  ResizeBottom:String;
  CaptionRegion:String;
  CaptionStyle:String;
  LeftFrameStyle:String;
  RightFrameStyle:String;
  BottomFrameStyle:String;
  IconRect:String;
  TextRect:String;
  CloseRect:String;
  MaximizeRect:String;
  MinimizeRect:String;
  HelpRect:String;
  CloseButton:String;
  MaximizeButton:String;
  MinimizeButton:String;
  RestoreButton:String;
  HelpButton:String;
  TextFont:TSXFontData;
  TextAlignment:TAlignment;
 end;
 PSXSkinFormStateParam=^TSXSkinFormStateParam;

 TSXSkinFormStyle=class(TSXSkinStyle)
  public
   NUState:TSXSkinFormStateParam;
   NFState:TSXSkinFormStateParam;
   MUState:TSXSkinFormStateParam;
   MFState:TSXSkinFormStateParam;
   XUState:TSXSkinFormStateParam;
   XFState:TSXSkinFormStateParam;
   //
   SetCaptionHeight:Boolean;
   SetLeftFrameWidth:Boolean;
   SetRightFrameWidth:Boolean;
   SetBottomFrameHeight:Boolean;
   //
   CaptionHeight:String;
   LeftFrameWidth:Integer;
   RightFrameWidth:Integer;
   BottomFrameHeight:Integer;
   //
   ZipFilePath:String;
   SkinFilePath:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentFState(var FState:TSXSkinFormStateParam;
              Maximized,Minimized,Focused:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinUpDownStateParam=record
  SetStyle:Boolean;
  SetOverStyle:Boolean;
  //
  Style:String;
  OverStyle:String;
 end;
 PSXSkinUpDownStateParam=^TSXSkinUpDownStateParam;

 TSXSkinUpDownStyle=class(TSXSkinStyle)
  public
   NUState:TSXSkinUpDownStateParam;
   NFState:TSXSkinUpDownStateParam;
   HUState:TSXSkinUpDownStateParam;
   HFState:TSXSkinUpDownStateParam;
   DFState:TSXSkinUpDownStateParam;
   RUState:TSXSkinUpDownStateParam;
   //
   HInUpDownEffect:TSXTransformEffectData;
   HOutUpDownEffect:TSXTransformEffectData;
   DownUpDownEffect:TSXTransformEffectData;
   UpUpDownEffect:TSXTransformEffectData;
   EnableUpDownEffect:TSXTransformEffectData;
   DisableUpDownEffect:TSXTransformEffectData;
   FocusUpDownEffect:TSXTransformEffectData;
   UnfocusUpDownEffect:TSXTransformEffectData;
   //
   SetUpButton:Boolean;
   SetDownButton:Boolean;
   SetUpButtonRect:Boolean;
   SetDownButtonRect:Boolean;
   //
   UpButton:String;
   DownButton:String;
   UpButtonRect:String;
   DownButtonRect:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure GetCurrentUDState(var UDState:TSXSkinUpDownStateParam;
              Enabled,MouseOver,Down,Focused:Boolean);
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinSpinEditStyle=class(TSXSkinStyle)
  public
   SetEdit:Boolean;
   SetUpDown:Boolean;
   SetEditRect:Boolean;
   SetUpDownRect:Boolean;
   //
   Edit:String;
   UpDown:String;
   EditRect:String;
   UpDownRect:String;
   procedure AssignStyle(Style:TSXSkinStyle); override;
   procedure SetParameter(const Name,Value:String); override;
   procedure SaveToStream(S:TStream;const RootPath:String); override;
   procedure LoadFromStream(S:TStream;Version:Integer;
              const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList); override;
 end;

 TSXSkinStyleList=class
  protected
   FItem:TList;
   TmpWidth:Integer;
   TmpHeight:Integer;
   function Get(Index:Integer):TSXSkinStyle;
   procedure Put(Index:Integer;Item:TSXSkinStyle);
   function GetCount:Integer;
  public
   function GetIndexToInsert(const Name:String):Integer;
   function GetIndexByName(const Name:String):Integer;
   function GetGStyleIndexOnGetVar(const VarName:String;var Error:Boolean):Single;
   function GetGStyleIndex(const Name:String;Width,Height:Integer):Integer;
   procedure Add(SXSkinStyle:TSXSkinStyle);
   procedure AddUnique(SXSkinStyle:TSXSkinStyle);
   procedure Delete(Index:Integer);
   procedure Clear;
   procedure ClearLinks;
   procedure SaveToStream(S:TStream;const RootPath:String);
   procedure LoadFromStream(S:TStream;Version:Integer;const RootPath,ZipFilePath:String;
              DestList1,DestList2:TSXSkinStyleList);
   constructor Create;
   destructor Destroy; override;
   property Item[Index:Integer]:TSXSkinStyle read Get write Put; default;
   property Count:Integer read GetCount;
 end;

 TSXStoredSkin=class(TComponent)
  private
   FFileName:String;
   FStream:TMemoryStream;
   procedure SetFileName(const Value:String);
  protected
   procedure ReadData(Reader:TStream);
   procedure WriteData(Writer:TStream);
   procedure DefineProperties(Filer:TFiler); override;
  public
   property Stream:TMemoryStream read FStream;
   procedure LoadStrings(SL:TStringList);
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   procedure LoadFromFile(const AFileName:String);
   procedure SaveToFile(const AFileName:String);
  published
   property FileName:String read FFileName write SetFileName;
 end;

 TSXSkinCustomLibrary=class(TComponent)
  private
   FActive:Boolean;
   FSkinDir:String;
   FSkinFile:String;
   FSkinFile2:String;
   FIsLoaded:Boolean;
   FStoredSkin:TSXStoredSkin;
   FStoredSkin2:TSXStoredSkin;
   SkinComponents:TList;
   Strings:TStringList;
   BasedOnList:TStringList;
   function GetCanBeUsed:Boolean;
   function CanSetActive:Boolean;
   procedure SetActive(Value:Boolean);
   procedure SetSkinDir(const Value:String);
   procedure SetSkinFile(const Value:String);
   procedure SetSkinFile2(const Value:String);
   procedure SetStoredSkin(const Value:TSXStoredSkin);
   procedure SetStoredSkin2(const Value:TSXStoredSkin);
   function VarValue(const Value:String):String;
   function GetFullSkinFile:String;
   function GetFullSkinFile2:String;
   procedure LoadFromINIFile(const FilePath:String);
   procedure LoadFromZIPFile(const FilePath:String);
   procedure LoadFromSXSFile(const FilePath:String);
   procedure LoadFromZIPStream(Stream:TStream;const FilePath:String);
   procedure LoadFromSXSStream(Stream:TStream;const FilePath,ZipFilePath:String);
   procedure ReloadSkinStyles(IfNotActive:Boolean=False);
  protected
   procedure Notification(AComponent:TComponent;Operation:TOperation); override;
   procedure Loaded; override;
   //procedure PaletteCreated; override;
  public
   Styles:TSXSkinStyleList;
   procedure AddSkinComponent(Component:TComponent);
   procedure RemoveSkinComponent(Component:TComponent);
   procedure RemoveSkinComponentData(Component:TComponent);
   procedure InvalidateSkinComponents;
   procedure RuntimeCreatedSetLoaded;
   function GetStringsValue(const Name:String):String;
   procedure LoadFromFile(const FilePath:String);
   procedure SaveToSXSFile(const FilePath:String);
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   property Active:Boolean read FActive write SetActive;
   property IsLoaded:Boolean read FIsLoaded;
   property CanBeUsed:Boolean read GetCanBeUsed;
   property SkinDir:String read FSkinDir write SetSkinDir;
   property SkinFile:String read FSkinFile write SetSkinFile;
   property SkinFile2:String read FSkinFile2 write SetSkinFile2;
   property StoredSkin:TSXStoredSkin read FStoredSkin write SetStoredSkin;
   property StoredSkin2:TSXStoredSkin read FStoredSkin2 write SetStoredSkin2;
 end;

 TSXSkinLibrary=class(TSXSkinCustomLibrary)
  published
   property Active stored False;
   property SkinDir;
   property SkinFile;
   property SkinFile2;
   property StoredSkin;
   property StoredSkin2;
 end;

function VersionSupported(Version:Integer):Boolean;

procedure ClearFontData(var FD:TSXFontData);
procedure AddFontData(var FD:TSXFontData;FDToAdd:TSXFontData);
function SameFontData(const FD1,FD2:TSXFontData):Boolean;
procedure SetDefaultFontData(var FD:TSXFontData;Font:TFont);
function SameFilterData(const Filter1,Filter2:TSXFilterData):Boolean;
function CreateSkinStyleElementByType(const TypeName:String):TSXSkinStyleElement;
function GetFilterTypeByName(const Name:String):TSXImageFilterType;
function HasTransformEffect(const Effect:TSXTransformEffectData):Boolean;
function SameTransformEffectData(const Effect1,Effect2:TSXTransformEffectData):Boolean;
procedure ApplyFilterToBitmap(var Bitmap:TBitmap32;const Filter:TSXFilterData);
function GetTransformEffectTypeByName(const Name:String):TSXTransformEffectType;
function GetTransformEffectDirectionByName(const Name:String):TSXTransformEffectDirection;
procedure GetTransformEffectByName(const Name:String;var Effect:TSXTransformEffectData);
procedure ApplyTransformEffectToBitmaps(B1,B2:TBitmap32;const Effect:TSXTransformEffectData;
           CurrentStep:Integer;var B:TBitmap32);
function GetImageResizeModeByName(const Name:String):TSXImageResizeMode;
function GetImageStretchFilterByName(const Name:String):TSXImageStretchFilter;

procedure GetColorFromString(const S:String;var A,R,G,B:Byte); overload;
function GetColorFromString(const S:String):TColor32; overload;

procedure GetRectFromString(const S:String;var R:TRect;OnGetVar:TSXOnGetVariable=nil);
procedure GetPolygonFromString(const S:String;P:TPolygon32;OnGetVar:TSXOnGetVariable=nil);

function CreateSkinStyleFromStream(S:TStream;Version:Integer;
          const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList):TSXSkinStyle;
function CreateSkinStyleElementFromStream(S:TStream;Version:Integer;
          const RootPath,ZipFilePath:String):TSXSkinStyleElement;

procedure SaveFontData(S:TStream;const FD:TSXFontData);
procedure LoadFontData(S:TStream;var FD:TSXFontData);
procedure SaveFilterData(S:TStream;const Filter:TSXFilterData);
procedure LoadFilterData(S:TStream;var Filter:TSXFilterData);
procedure SaveTransformEffectData(S:TStream;const Effect:TSXTransformEffectData);
procedure LoadTransformEffectData(S:TStream;var Effect:TSXTransformEffectData);
procedure LoadStringsFromZIPSkinStream(Stream:TStream;SL:TStringList);
procedure LoadStringsFromSkinFile(const FilePath:String;SL:TStringList);

function StoredSkinCount:Integer;
function GetStoredSkinByIndex(Index:Integer):TSXStoredSkin;
function GetStoredSkinByZIPName(const FileName:String):TSXStoredSkin;

function GetNewCElementID:Integer;

var StdVariableComparer:TSXStdVariableComparer;

implementation

uses SXSkinUtils, jpeg, SXPNGUtils, SXSkinControl, Math, SXSkinBitmapManager,
     SXSkinRegionManager, SXSkinForm;

var SXStoredSkins:TList;
     InternalSkin:TSXStoredSkin;
     LastCElementID:Integer=0;

function StoredSkinCount:Integer;
begin
 Result:=SXStoredSkins.Count;
end;

function GetStoredSkinByIndex(Index:Integer):TSXStoredSkin;
begin
 if (Index>=0) and (Index<SXStoredSkins.Count) then
  Result:=SXStoredSkins[Index] else Result:=nil;
end;

function GetStoredSkinByZIPName(const FileName:String):TSXStoredSkin;
var A:Integer;
begin
 for A:=0 to SXStoredSkins.Count-1 do
  if SameText(FileName,TSXStoredSkin(SXStoredSkins[A]).FileName) then
   begin
    Result:=TSXStoredSkin(SXStoredSkins[A]);
    exit;
   end;
 Result:=nil;
end;

procedure CreateResourceSkins;
var RS:TResourceStream;
begin
 InternalSkin:=TSXStoredSkin.Create(nil);
 InternalSkin.FileName:='_internal.zip';
 RS:=TResourceStream.Create(HInstance,'INTERNAL_ZIP',RT_RCDATA);
 try
  InternalSkin.FStream.LoadFromStream(RS);
  InternalSkin.FStream.Seek(0,soFromBeginning);
 finally
  RS.Free;
 end;
 //
 RS:=TResourceStream.Create(HInstance,'GENERAL_ZIP',RT_RCDATA);
 try
  PreloadZipFile(RS,'_general.zip',nil);
 finally
  RS.Free;
 end;
end;

procedure GetRectFromString(const S:String;var R:TRect;OnGetVar:TSXOnGetVariable=nil);
var A,B,C,D:Integer;

 procedure SetWhatGot;
 var S2:String;
      E:Boolean;
 begin
  if B>A then
   begin
    S2:=Copy(S,A,B-A);
    if @OnGetVar=nil then
     D:=StrToIntDef(S2,0) else
      begin
       E:=False;
       D:=round(SXEvalMathString(S2,OnGetVar,E));
       if E then D:=0;
      end;
    case C of
     0: R.Left:=D;
     1: R.Top:=D;
     2: R.Right:=D;
     3: R.Bottom:=D;
    end;
   end;
 end;

begin
 R:=Rect(0,0,0,0);
 A:=1; B:=1; C:=0;
 while (B<=length(S)) and (C<4) do
  begin
   if S[B]=',' then
    begin
     SetWhatGot;
     Inc(C); Inc(B);
     A:=B;
    end else Inc(B);
  end;
 SetWhatGot; 
end;

procedure GetPolygonFromString(const S:String;P:TPolygon32;OnGetVar:TSXOnGetVariable=nil);
var A,B,C:Integer;
  LastX,D:Single;

 procedure SetWhatGot;
 var S2:String;
      E:Boolean;
 begin
  if B>A then
   begin
    S2:=Copy(S,A,B-A);
    if @OnGetVar=nil then
     D:=StrToIntDef(S2,0) else
      begin
       E:=False;
       D:=SXEvalMathString(S2,OnGetVar,E);
       if E then D:=0;
      end;
    if C and 1=0 then LastX:=D else
     P.Add(FixedPoint(LastX,D));
    Inc(C);
   end;
 end;

begin
 A:=1; B:=1; C:=0;
 while B<=length(S) do
  begin
   if S[B]=',' then
    begin
     SetWhatGot;
     Inc(B);
     A:=B;
    end else Inc(B);
  end;
 SetWhatGot;
end;

function HexDigit(C:Char):Byte;
begin
 if C in ['A'..'F'] then
  Result:=10+Ord(C)-Ord('A') else
 if C in ['a'..'f'] then
  Result:=10+Ord(C)-Ord('a') else
 if C in ['0'..'9'] then
  Result:=Ord(C)-Ord('0') else
   Result:=0;
end;

procedure GetColorFromString(const S:String;var A,R,G,B:Byte); 
var AA,BB,C,D:Integer;

 procedure SetWhatGot;
 var S2:String;
 begin
  if BB>AA then
   begin
    S2:=Copy(S,AA,BB-AA);
    D:=StrToIntDef(S2,0);
    case C of
     0: A:=D;
     1: R:=D;
     2: G:=D;
     3: B:=D;
    end;
   end;
 end;

 procedure SetAsWinColor(Color:TColor);
 var CL:TColor32;
 begin
  CL:=Color32(Color);
  R:=RedComponent(CL);
  G:=GreenComponent(CL);
  B:=BlueComponent(CL);
 end;

begin
 A:=255; R:=0; G:=0; B:=0;
 if S='' then exit;
 if S='ScrollBar' then
  begin
   SetAsWinColor(clScrollBar); exit;
  end;
 if S='Background' then
  begin
   SetAsWinColor(clBackground); exit;
  end;
 if S='ActiveCaption' then
  begin
   SetAsWinColor(clActiveCaption); exit;
  end;
 if S='InactiveCaption' then
  begin
   SetAsWinColor(clInactiveCaption); exit;
  end;
 if S='Menu' then
  begin
   SetAsWinColor(clMenu); exit;
  end;
 if S='Window' then
  begin
   SetAsWinColor(clWindow); exit;
  end;
 if S='WindowFrame' then
  begin
   SetAsWinColor(clWindowFrame); exit;
  end;
 if S='MenuText' then
  begin
   SetAsWinColor(clMenuText); exit;
  end;
 if S='WindowText' then
  begin
   SetAsWinColor(clWindowText); exit;
  end;
 if S='CaptionText' then
  begin
   SetAsWinColor(clCaptionText); exit;
  end;
 if S='ActiveBorder' then
  begin
   SetAsWinColor(clActiveBorder); exit;
  end;
 if S='InactiveBorder' then
  begin
   SetAsWinColor(clInactiveBorder); exit;
  end;
 if S='AppWorkSpace' then
  begin
   SetAsWinColor(clAppWorkSpace); exit;
  end;
 if S='Highlight' then
  begin
   SetAsWinColor(clHighlight); exit;
  end;
 if S='HighlightText' then
  begin
   SetAsWinColor(clHighlightText); exit;
  end;
 if S='BtnFace' then
  begin
   SetAsWinColor(clBtnFace); exit;
  end;
 if S='BtnShadow' then
  begin
   SetAsWinColor(clBtnShadow); exit;
  end;
 if S='GrayText' then
  begin
   SetAsWinColor(clGrayText); exit;
  end;
 if S='BtnText' then
  begin
   SetAsWinColor(clBtnText); exit;
  end;
 if S='InactiveCaptionText' then
  begin
   SetAsWinColor(clInactiveCaptionText); exit;
  end;
 if S='BtnHighlight' then
  begin
   SetAsWinColor(clBtnHighlight); exit;
  end;
 if S='3DDkShadow' then
  begin
   SetAsWinColor(cl3DDkShadow); exit;
  end;
 if S='3DLight' then
  begin
   SetAsWinColor(cl3DLight); exit;
  end;
 if S='InfoText' then
  begin
   SetAsWinColor(clInfoText); exit;
  end;
 if S='InfoBk' then
  begin
   SetAsWinColor(clInfoBk); exit;
  end;
 {$IFDEF COMPILER_9_UP}
 if S='HotLight' then
  begin
   SetAsWinColor(clHotLight); exit;
  end;
 {$ENDIF}
 if S='GradientActiveCaption' then
  begin
   SetAsWinColor(clGradientActiveCaption); exit;
  end;
 if S='GradientInactiveCaption' then
  begin
   SetAsWinColor(clGradientInactiveCaption); exit;
  end;
 {$IFDEF COMPILER_9_UP}
 if S='MenuHighlight' then
  begin
   SetAsWinColor(clMenuHighlight); exit;
  end;
 if S='MenuBar' then
  begin
   SetAsWinColor(clMenuBar); exit;
  end;
 {$ENDIF}
 if S[1] in ['#','$'] then
  begin
   if (length(S)<>7) and (length(S)<>9) then exit;
   if length(S)=7 then C:=2 else
    begin
     C:=4;
     A:=(HexDigit(S[2]) shl 4) or HexDigit(S[3]);
    end;
   R:=(HexDigit(S[C  ]) shl 4) or HexDigit(S[C+1]);
   G:=(HexDigit(S[C+2]) shl 4) or HexDigit(S[C+3]);
   B:=(HexDigit(S[C+4]) shl 4) or HexDigit(S[C+5]);
   exit;
  end;
 AA:=1; BB:=1; C:=0;
 while (BB<=length(S)) and (C<4) do
  begin
   if S[BB]=',' then
    begin
     SetWhatGot;
     Inc(C); Inc(BB);
     AA:=BB;
    end else Inc(BB);
  end;
 SetWhatGot;
end;

function GetColorFromString(const S:String):TColor32;
var A,R,G,B:Byte;
begin
 GetColorFromString(S,A,R,G,B);
 Result:=Color32(R,G,B,A);
end;

function GetAlignmentFromString(const S:String):TAlignment;
begin
 if S='Right' then Result:=taRightJustify else
  if S='Center' then Result:=taCenter else
   Result:=taLeftJustify;
end;

procedure ClearFontData(var FD:TSXFontData);
begin
 Finalize(FD);
 FillChar(FD,sizeof(FD),0);
end;

function SameFontData(const FD1,FD2:TSXFontData):Boolean;
begin
 Result:=(FD1.FontName=FD2.FontName) and (FD1.FontSize=FD2.FontSize) and
         (FD1.FontStyle=FD2.FontStyle) and (FD1.FontColor=FD2.FontColor) and
         (FD1.HasShadow=FD2.HasShadow) and (not FD1.HasShadow or
         (FD1.ShadowColor=FD2.ShadowColor)) and (FD1.DoPrepaint=FD2.DoPrepaint) and
         (FD1.SmoothLevel=FD2.SmoothLevel);
end;

procedure AddFontData(var FD:TSXFontData;FDToAdd:TSXFontData);
begin
 if not FD.SetFontName then
  begin
   FD.SetFontName:=FDToAdd.SetFontName;
   FD.FontName:=FDToAdd.FontName;
  end;
 if not FD.SetFontSize then
  begin
   FD.SetFontSize:=FDToAdd.SetFontSize;
   FD.FontSize:=FDToAdd.FontSize;
  end;
 if not FD.SetFontStyle then
  begin
   FD.SetFontStyle:=FDToAdd.SetFontStyle;
   FD.FontStyle:=FDToAdd.FontStyle;
  end;
 if not FD.SetFontColor then
  begin
   FD.SetFontColor:=FDToAdd.SetFontColor;
   FD.FontColor:=FDToAdd.FontColor;
  end;
 if not FD.SetHasShadow then
  begin
   FD.SetHasShadow:=FDToAdd.SetHasShadow;
   FD.HasShadow:=FDToAdd.HasShadow;
  end;
 if not FD.SetShadowColor then
  begin
   FD.SetShadowColor:=FDToAdd.SetShadowColor;
   FD.ShadowColor:=FDToAdd.ShadowColor;
  end;
 if not FD.SetSmoothLevel then
  begin
   FD.SetSmoothLevel:=FDToAdd.SetSmoothLevel;
   FD.SmoothLevel:=FDToAdd.SmoothLevel;
  end;
 if not FD.SetDoPrepaint then
  begin
   FD.SetDoPrepaint:=FDToAdd.SetDoPrepaint;
   FD.DoPrepaint:=FDToAdd.DoPrepaint;
  end;
end;

procedure SetDefaultFontData(var FD:TSXFontData;Font:TFont);
begin
 if not FD.SetFontName then
  begin
   FD.FontName:=Font.Name;
   FD.SetFontName:=True;
  end;
 if not FD.SetFontSize then
  begin
   FD.FontSize:=Font.Size;
   FD.SetFontSize:=True;
  end;
 if not FD.SetFontStyle then
  begin
   FD.FontStyle:=Font.Style;
   FD.SetFontStyle:=True;
  end;
 if not FD.SetFontColor then
  begin
   FD.FontColor:=Color32(Font.Color);
   FD.SetFontColor:=True;
  end; 
end;

function TryToSetFontDataParameter(const Name,Value:String;var FD:TSXFontData):Boolean;
var C:Integer;
begin
 Result:=False;
 with FD do
  begin
   if Name='FontName' then
    begin
     FontName:=Trim(Value);
     SetFontName:=True;
     Result:=True;
    end else
   if Name='FontSize' then
    begin
     FontSize:=StrToIntDef(Value,0);
     SetFontSize:=True;
     Result:=True;
    end else
   if Name='FontColor' then
    begin
     FontColor:=GetColorFromString(Value);
     SetFontColor:=True;
     Result:=True;
    end else
   if Name='HasTextShadow' then
    begin
     HasShadow:=Value='1';
     SetHasShadow:=True;
     Result:=True;
    end else
   if Name='ShadowColor' then
    begin
     ShadowColor:=GetColorFromString(Value);
     SetShadowColor:=True;
     Result:=True;
    end else
   if Name='SmoothLevel' then
    begin
     SmoothLevel:=StrToIntDef(Value,0);
     SetSmoothLevel:=True;
     Result:=True;
    end else
   if Name='DoPrepaint' then
    begin
     DoPrepaint:=Value='1';
     SetDoPrepaint:=True;
     Result:=True;
    end else
   if Name='FontStyle' then
    begin
     for C:=1 to length(Value) do
      case Value[C] of
       'B','b': FontStyle:=FontStyle+[fsBold];
       'I','i': FontStyle:=FontStyle+[fsItalic];
       'U','u': FontStyle:=FontStyle+[fsUnderline];
       'S','s': FontStyle:=FontStyle+[fsStrikeOut];
      end;
     SetFontStyle:=True;
     Result:=True;
    end;
  end;
end;

function SameFilterData(const Filter1,Filter2:TSXFilterData):Boolean;
begin
 Result:=((Filter1.AType=iftNone) and (Filter2.AType=iftNone)) or
         ((Filter1.AType=Filter2.AType) and (Filter1.Color=Filter2.Color) and
          (Filter1.Color2=Filter2.Color2) and (Filter1.Value=Filter2.Value));
end;

function TryToSetFilterDataParameter(const Name,Value:String;var Filter:TSXFilterData;
           const Prefix:String=''):Boolean;
var AName:String;
begin
 Result:=False;
 if (Prefix<>'') and (Copy(Name,1,length(Prefix))<>Prefix) then exit;
 if Prefix='' then AName:=Name else
  AName:=Copy(Name,length(Prefix)+1,MaxInt);
 if AName='Filter' then
  begin
   Filter.AType:=GetFilterTypeByName(Value);
   Filter.SetAType:=True;
   Result:=True;
  end else
 if (AName='FilterColor') or (AName='FilterColor1') then
  begin
   Filter.Color:=GetColorFromString(Value);
   Filter.SetColor:=True;
   if Filter.AType in [iftLighten,iftLightenHorizG,iftLightenVertG,iftDarken,
    iftDarkenHorizG,iftDarkenVertG] then Filter.Color:=Filter.Color and $00FFFFFF;
   Result:=True;
  end else
 if AName='FilterColor2' then
  begin
   Filter.Color2:=GetColorFromString(Value);
   Filter.SetColor2:=True;
   if Filter.AType in [iftLighten,iftLightenHorizG,iftLightenVertG,iftDarken,
    iftDarkenHorizG,iftDarkenVertG] then Filter.Color2:=Filter.Color2 and $00FFFFFF;
   Result:=True;
  end else
 if AName='FilterValue' then
  begin
   Filter.Value:=StrToIntDef(Value,0);
   Filter.SetValue:=True;
   Result:=True;
  end;
end;

function SameTransformEffectData(const Effect1,Effect2:TSXTransformEffectData):Boolean;
begin
 Result:=CompareMem(@Effect1,@Effect2,sizeof(TSXTransformEffectData));
end;

function TryToSetTransformEffectDataParameter(const Name,Value:String;
           var Effect:TSXTransformEffectData;const Prefix:String=''):Boolean;
var AName:String;
begin
 Result:=False;
 if (Prefix<>'') and (Copy(Name,1,length(Prefix))<>Prefix) then exit;
 if Prefix='' then AName:=Name else
  AName:=Copy(Name,length(Prefix)+1,MaxInt);
 with Effect do
  begin
   if AName='Transform' then
    begin
     GetTransformEffectByName(Value,Effect);
     Result:=True;
    end else
   if AName='TransformO' then
    begin
     SetOldType:=True;
     OldType:=GetTransformEffectTypeByName(Value);
     Result:=True;
    end else
   if AName='TransformDirO' then
    begin
     SetOldDirection:=True;
     OldDirection:=GetTransformEffectDirectionByName(Value);
     Result:=True;
    end else
   if AName='TransformOutO' then
    begin
     SetOldDirOut:=True;
     OldDirOut:=Value='1';
     Result:=True;
    end else
   if AName='TransformInvertO' then
    begin
     SetOldInverted:=True;
     OldInverted:=Value='1';
     Result:=True;
    end else
   if AName='TransformN' then
    begin
     SetNewType:=True;
     NewType:=GetTransformEffectTypeByName(Value);
     Result:=True;
    end else
   if AName='TransformDirN' then
    begin
     SetNewDirection:=True;
     NewDirection:=GetTransformEffectDirectionByName(Value);
     Result:=True;
    end else
   if AName='TransformOutN' then
    begin
     SetNewDirOut:=True;
     NewDirOut:=Value='1';
     Result:=True;
    end else
   if AName='TransformInvertN' then
    begin
     SetNewInverted:=True;
     NewInverted:=Value='1';
     Result:=True;
    end else
   if AName='TransformOffset' then
    begin
     SetOffset:=True;
     Offset:=StrToIntDef(Value,0);
     Result:=True;
    end else
   if AName='TransformOldOnTop' then
    begin
     SetOldOnTop:=True;
     OldOnTop:=Value='1';
     Result:=True;
    end else
   if AName='TransformDrawCaption' then
    begin
     SetDrawCaption:=True;
     DrawCaption:=Value='1';
     Result:=True;
    end else
   if AName='StepsNum' then
    begin
     SetStepsNum:=True;
     StepsNum:=StrToIntDef(Value,0);
     Result:=True;
    end;
  end;
end;

{ TSXVariableComparer }

function TSXVariableComparer.GetVarListForRect(const S:String):TList;
var R:TRect;
begin
 CurValList:=nil;
 GetRectFromString(S,R,VarListOnGetVariable);
 Result:=CurValList;
end;

function TSXVariableComparer.GetVarListForPoly(const S:String):TList;
var P:TPolygon32;
begin
 CurValList:=nil;
 P:=TPolygon32.Create;
 try
  GetPolygonFromString(S,P,VarListOnGetVariable);
 finally
  P.Free;
 end;
 Result:=CurValList;
end;

{ TSXStdVariableComparer }

function TSXStdVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARSTD_W else
 if VarName='H' then
  CurVarVal:=VARSTD_H;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXStdVariableComparer.IntOnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if VarName='W' then
  begin
   Result:=Width; exit;
  end;
 if VarName='H' then
  begin
   Result:=Height; exit;
  end;
 Error:=True;
end;

function TSXStdVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 case VarID of
  VARSTD_W: Result:=Width;
  VARSTD_H: Result:=Height;
 end;
end;

procedure TSXStdVariableComparer.SetSize(AWidth,AHeight:Integer);
begin
 Width:=AWidth;
 Height:=AHeight;
end;

procedure TSXStdVariableComparer.GetSize(var AWidth,AHeight:Integer);
begin
 AWidth:=Width;
 AHeight:=Height;
end;

function TSXStdVariableComparer.GetVarValsForVarList(VarList:TList):TList;
var A:Integer;
begin
 if VarList=nil then
  begin
   Result:=nil;
   exit;
  end;
 Result:=TList.Create;
 for A:=0 to VarList.Count-1 do
  Result.Add(Pointer(GetValue(Integer(VarList[A]))));
end;

function TSXStdVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
var A:Integer;
begin
 Result:=False;
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  if Integer(OldVarVals[A])<>GetValue(Integer(VarList[A])) then
   begin
    Result:=True;
    exit;
   end;
end;

procedure TSXStdVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXStdVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXStdVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

constructor TSXStdVariableComparer.Create;
begin
 inherited Create;
 OnGetVariable:=IntOnGetVariable;
end;

{ TSXSkinStyleElement }

procedure TSXSkinStyleElement.Assign(Element:TSXSkinStyleElement);
begin
 Name:=Element.Name;
end;

function TSXSkinStyleElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleElement.Create;
 Result.Assign(Self);
end;

procedure TSXSkinStyleElement.SetProperty(const Name,Value,SkinFilePath:String);
begin
end;

procedure TSXSkinStyleElement.SaveToStream(S:TStream;const RootPath:String);
var AType:TSXSkinStyleElementType;
begin
 if Self is TSXSkinStyleImageElement then AType:=ssetImage else
  if Self is TSXSkinStyleBoxTileElement then AType:=ssetBoxTile else
  if Self is TSXSkinStyleFigureElement then AType:=ssetFigure else
  if Self is TSXSkinStyleTextElement then AType:=ssetText else
  if Self is TSXSkinStyleStyleElement then AType:=ssetStyle else
   AType:=ssetUnknown;
 S.Write(AType,sizeof(AType));  
 SaveString(S,Name);
end;

procedure TSXSkinStyleElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
begin
 LoadString(S,Name);
end;

{ TSXSkinStyleImageElement }

procedure TSXSkinStyleImageElement.Assign(Element:TSXSkinStyleElement);
begin
 inherited;
 if Element is TSXSkinStyleImageElement then
  begin
   ImageType:=TSXSkinStyleImageElement(Element).ImageType;
   LoadedPath:=TSXSkinStyleImageElement(Element).LoadedPath;
   Path:=TSXSkinStyleImageElement(Element).Path;
   ZipFilePath:=TSXSkinStyleImageElement(Element).ZipFilePath;
   Transparent:=TSXSkinStyleImageElement(Element).Transparent;
   Filter:=TSXSkinStyleImageElement(Element).Filter;
   ResizeMode:=TSXSkinStyleImageElement(Element).ResizeMode;
   StretchFilter:=TSXSkinStyleImageElement(Element).StretchFilter;
   OffsetX:=TSXSkinStyleImageElement(Element).OffsetX;
   OffsetY:=TSXSkinStyleImageElement(Element).OffsetY;
   ResizedWidth:=TSXSkinStyleImageElement(Element).ResizedWidth;
   ResizedHeight:=TSXSkinStyleImageElement(Element).ResizedHeight;
   Centered:=TSXSkinStyleImageElement(Element).Centered;
   DrawRect:=TSXSkinStyleImageElement(Element).DrawRect;
   Bitmap:=nil;
  end;
end;

function TSXSkinStyleImageElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleImageElement.Create;
 Result.Assign(Self);
end;

procedure TSXSkinStyleImageElement.ValidateBitmap(SkinLibrary:TSXSkinLibrary);
begin
 if Bitmap=nil then
  begin
   Bitmap:=GetPreloadedBitmap(SkinLibrary,Path,ZipFilePath,
            (ResizedWidth<>0) or (ResizedHeight<>0),ResizedWidth,ResizedHeight,
            StretchFilter,Filter);
   if ResizeMode=irmStretch then
    case StretchFilter of
     isfLinear:   TLinearResampler.Create(Bitmap);
     else         begin
                   TKernelResampler.Create(Bitmap);
                   case StretchFilter of
                    isfSpline:   TKernelResampler(Bitmap.Resampler).
                                  Kernel:=TSplineKernel.Create;
                    isfLanczos:  TKernelResampler(Bitmap.Resampler).
                                  Kernel:=TLanczosKernel.Create;
                    isfMitchell: TKernelResampler(Bitmap.Resampler).
                                  Kernel:=TMitchellKernel.Create;
                   end;
                  end;
    end;
  end;
end;

procedure TSXSkinStyleImageElement.SetProperty(const Name,Value,SkinFilePath:String);
begin
 if Name='Transparent' then Transparent:=Value='1' else
  if Name='Path' then
   begin
    LoadedPath:=Value;
    Path:=Value;
    if not ExtractZipPath(Path,ZipFilePath) then
     Path:=GetFullPath(Value,SkinFilePath);
   end else
  if Name='ResizeMode' then ResizeMode:=GetImageResizeModeByName(Value) else
  if Name='Stretch' then StretchFilter:=GetImageStretchFilterByName(Value) else
  if Name='OffsetX' then OffsetX:=StrToIntDef(Value,0) else
  if Name='OffsetY' then OffsetY:=StrToIntDef(Value,0) else
  if Name='Width' then ResizedWidth:=StrToIntDef(Value,0) else
  if Name='Height' then ResizedHeight:=StrToIntDef(Value,0) else
  if Name='Rect' then DrawRect:=WithoutAllSpaces(Value) else
  if Name='Centered' then Centered:=Value='1' else
   TryToSetFilterDataParameter(Name,Value,Filter);
end;

procedure TSXSkinStyleImageElement.SaveToStream(S:TStream;const RootPath:String);
var Resized,HasOffset:Boolean;
  IsTiled,IsStretched:Boolean;
     LinearF,NearestF:Boolean;
          HasDrawRect:Boolean;
begin
 inherited;
 S.Write(ImageType,sizeof(ImageType));
 SaveString(S,LoadedPath);
 Resized:=(ResizedWidth<>0) or (ResizedHeight<>0);
 HasOffset:=(OffsetX<>0) or (OffsetY<>0);
 IsTiled:=(ResizeMode=irmTile);
 IsStretched:=(ResizeMode=irmStretch);
 LinearF:=(StretchFilter=isfLinear);
 NearestF:=(StretchFilter=isfNearest);
 HasDrawRect:=(DrawRect<>'');
 Save8Flags(S,Transparent,Resized,HasOffset,IsTiled,IsStretched,LinearF,NearestF,HasDrawRect);
 if not (LinearF or NearestF) then
  S.Write(StretchFilter,sizeof(StretchFilter));
 if Resized then
  begin
   SavePackedInteger(S,ResizedWidth);
   SavePackedInteger(S,ResizedHeight);
  end;
 if HasOffset then
  begin
   SavePackedInteger(S,OffsetX);
   SavePackedInteger(S,OffsetY);
  end;
 if HasDrawRect then
  SaveString(S,DrawRect) else
   S.Write(Centered,sizeof(Centered));
 SaveFilterData(S,Filter);
end;

procedure TSXSkinStyleImageElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
var Resized,HasOffset:Boolean;
  IsTiled,IsStretched:Boolean;
     LinearF,NearestF:Boolean;
          HasDrawRect:Boolean;
begin
 inherited;
 S.Read(ImageType,sizeof(ImageType));
 LoadString(S,LoadedPath);
 Path:=LoadedPath;
 if not ExtractZipPath(Path,Self.ZipFilePath) then
  begin
   Path:=GetFullPath(Path,RootPath);
   Self.ZipFilePath:=ZipFilePath;
  end; 
 Load8Flags(S,Transparent,Resized,HasOffset,IsTiled,IsStretched,LinearF,NearestF,HasDrawRect);
 if IsTiled then ResizeMode:=irmTile else
  if IsStretched then ResizeMode:=irmStretch else
   ResizeMode:=irmNone;
 if LinearF then StretchFilter:=isfLinear else
  if NearestF then StretchFilter:=isfNearest else
   S.Read(StretchFilter,sizeof(StretchFilter));
 if Resized then
  begin
   LoadPackedInteger(S,ResizedWidth);
   LoadPackedInteger(S,ResizedHeight);
  end else
   begin
    ResizedWidth:=0;
    ResizedHeight:=0;
   end;
 if HasOffset then
  begin
   LoadPackedInteger(S,OffsetX);
   LoadPackedInteger(S,OffsetY);
  end else
   begin
    OffsetX:=0;
    OffsetY:=0;
   end;
 if HasDrawRect then
  LoadString(S,DrawRect) else
   begin
    DrawRect:='';
    S.Read(Centered,sizeof(Centered));
   end;
 LoadFilterData(S,Filter);
end;

constructor TSXSkinStyleImageElement.Create;
begin
 inherited Create;
 Transparent:=True;
end;

destructor TSXSkinStyleImageElement.Destroy;
begin
 inherited Destroy;
end;

{ TSXSkinStyleStyleElement }

procedure TSXSkinStyleStyleElement.Assign(Element:TSXSkinStyleElement);
begin
 inherited;
 if Element is TSXSkinStyleStyleElement then
  begin
   Style:=TSXSkinStyleStyleElement(Element).Style;
   DrawRect:=TSXSkinStyleStyleElement(Element).DrawRect;
   Filter:=TSXSkinStyleStyleElement(Element).Filter;
  end;
end;

function TSXSkinStyleStyleElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleStyleElement.Create;
 Result.Assign(Self);
end;

procedure TSXSkinStyleStyleElement.SetProperty(const Name,Value,SkinFilePath:String);
begin
 if Name='Style' then Style:=Value else
  if Name='Rect' then DrawRect:=Value else
   TryToSetFilterDataParameter(Name,Value,Filter);
end;

procedure TSXSkinStyleStyleElement.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 SaveString(S,Style);
 SaveString(S,DrawRect);
 SaveFilterData(S,Filter);
end;

procedure TSXSkinStyleStyleElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
begin
 inherited;
 LoadString(S,Style);
 LoadString(S,DrawRect);
 LoadFilterData(S,Filter);
end;

{ TSXSkinStyleBoxTileElement }

procedure TSXSkinStyleBoxTileElement.Assign(Element:TSXSkinStyleElement);
begin
 inherited;
 if Element is TSXSkinStyleBoxTileElement then
  begin
   ZipFilePath:=TSXSkinStyleBoxTileElement(Element).ZipFilePath;
   CenterPath:=TSXSkinStyleBoxTileElement(Element).CenterPath;
   LeftPath:=TSXSkinStyleBoxTileElement(Element).LeftPath;
   TopPath:=TSXSkinStyleBoxTileElement(Element).TopPath;
   RightPath:=TSXSkinStyleBoxTileElement(Element).RightPath;
   BottomPath:=TSXSkinStyleBoxTileElement(Element).BottomPath;
   TopLeftPath:=TSXSkinStyleBoxTileElement(Element).TopLeftPath;
   TopRightPath:=TSXSkinStyleBoxTileElement(Element).TopRightPath;
   BottomRightPath:=TSXSkinStyleBoxTileElement(Element).BottomRightPath;
   BottomLeftPath:=TSXSkinStyleBoxTileElement(Element).BottomLeftPath;
   //
   CenterColor:=TSXSkinStyleBoxTileElement(Element).CenterColor;
   LeftColor:=TSXSkinStyleBoxTileElement(Element).LeftColor;
   TopColor:=TSXSkinStyleBoxTileElement(Element).TopColor;
   RightColor:=TSXSkinStyleBoxTileElement(Element).RightColor;
   BottomColor:=TSXSkinStyleBoxTileElement(Element).BottomColor;
   TopLeftColor:=TSXSkinStyleBoxTileElement(Element).TopLeftColor;
   TopRightColor:=TSXSkinStyleBoxTileElement(Element).TopRightColor;
   BottomRightColor:=TSXSkinStyleBoxTileElement(Element).BottomRightColor;
   BottomLeftColor:=TSXSkinStyleBoxTileElement(Element).BottomLeftColor;
   //
   SetCenterColor:=TSXSkinStyleBoxTileElement(Element).SetCenterColor;
   SetLeftColor:=TSXSkinStyleBoxTileElement(Element).SetLeftColor;
   SetTopColor:=TSXSkinStyleBoxTileElement(Element).SetTopColor;
   SetRightColor:=TSXSkinStyleBoxTileElement(Element).SetRightColor;
   SetBottomColor:=TSXSkinStyleBoxTileElement(Element).SetBottomColor;
   SetTopLeftColor:=TSXSkinStyleBoxTileElement(Element).SetTopLeftColor;
   SetTopRightColor:=TSXSkinStyleBoxTileElement(Element).SetTopRightColor;
   SetBottomRightColor:=TSXSkinStyleBoxTileElement(Element).SetBottomRightColor;
   SetBottomLeftColor:=TSXSkinStyleBoxTileElement(Element).SetBottomLeftColor;
   SetResizeMode:=TSXSkinStyleBoxTileElement(Element).SetResizeMode;
   //
   SetCenter:=TSXSkinStyleBoxTileElement(Element).SetCenter;
   SetLeft:=TSXSkinStyleBoxTileElement(Element).SetLeft;
   SetTop:=TSXSkinStyleBoxTileElement(Element).SetTop;
   SetRight:=TSXSkinStyleBoxTileElement(Element).SetRight;
   SetBottom:=TSXSkinStyleBoxTileElement(Element).SetBottom;
   SetTopLeft:=TSXSkinStyleBoxTileElement(Element).SetTopLeft;
   SetTopRight:=TSXSkinStyleBoxTileElement(Element).SetTopRight;
   SetBottomRight:=TSXSkinStyleBoxTileElement(Element).SetBottomRight;
   SetBottomLeft:=TSXSkinStyleBoxTileElement(Element).SetBottomLeft;
   //
   CenterBitmap.Free;
   LeftBitmap.Free;
   TopBitmap.Free;
   RightBitmap.Free;
   BottomBitmap.Free;
   TopLeftBitmap.Free;
   TopRightBitmap.Free;
   BottomRightBitmap.Free;
   BottomLeftBitmap.Free;
   //
   Transparent:=TSXSkinStyleBoxTileElement(Element).Transparent;
   Filter:=TSXSkinStyleBoxTileElement(Element).Filter;
   ResizeMode:=TSXSkinStyleBoxTileElement(Element).ResizeMode;
   //
   LoadedPathTemplate:=TSXSkinStyleBoxTileElement(Element).LoadedPathTemplate;
   PathTemplate:=TSXSkinStyleBoxTileElement(Element).PathTemplate;
   DrawRect:=TSXSkinStyleBoxTileElement(Element).DrawRect;
  end;
end;

function TSXSkinStyleBoxTileElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleBoxTileElement.Create;
 Result.Assign(Self);
end;

function TSXSkinStyleBoxTileElement.GetPixelAt(PX,PY,Width,Height:Integer;AOnGetVariable:TSXOnGetVariable=nil):TColor32;
var LeftW,CenterW,RightW:Integer;
    TopH,CenterH,BottomH:Integer;
                       R:TRect;
                     X,Y:Integer;

 procedure SetSizeParam(Bitmap:TBitmap32;UseColor:Boolean;var AWidth,AHeight:Integer);
 begin
  if UseColor then
   begin
    if AWidth<1 then AWidth:=1;
    if AHeight<1 then AHeight:=1;
   end else
  if Bitmap<>nil then
   begin
    if Bitmap.Width>AWidth then
     AWidth:=Bitmap.Width;
    if Bitmap.Height>AHeight then
     AHeight:=Bitmap.Height;
   end;
 end;

 function PixelInRect(Bitmap:TBitmap32;UseColor:Boolean;Color:TColor32;R:TRect):TColor32;
 var TX,TY:Integer;
 begin
  if UseColor then Result:=Color else
   if (Bitmap<>nil) and (Bitmap.Width>0) and (Bitmap.Height>0) then
    begin
     TX:=(PX-R.Left-X) mod Bitmap.Width;
     TY:=(PY-R.Top-Y) mod Bitmap.Height;
     Result:=Bitmap.Pixel[TX,TY];
    end else Result:=0;
 end;

begin
 Result:=0;
 X:=0; Y:=0;
 if DrawRect<>'' then
  begin
   GetRectFromString(DrawRect,R,AOnGetVariable);
   if not IsRectEmpty(R) then
    begin
     Inc(X,R.Left);
     Inc(Y,R.Top);
     Width:=R.Right-R.Left;
     Height:=R.Bottom-R.Top;
    end;
  end;
 if (PX<X) or (PX>=X+Width) or (PY<Y) or (PY>=Y+Height) then exit;
 LeftW:=0; RightW:=0;
 TopH:=0;  BottomH:=0;
 SetSizeParam(TopLeftBitmap,SetTopLeftColor,LeftW,TopH);
 SetSizeParam(TopRightBitmap,SetTopRightColor,RightW,TopH);
 SetSizeParam(BottomLeftBitmap,SetBottomLeftColor,LeftW,BottomH);
 SetSizeParam(BottomRightBitmap,SetBottomRightColor,RightW,BottomH);
 CenterW:=Width-LeftW-RightW;
 CenterH:=Height-TopH-BottomH;
 if CenterW<0 then
  begin
   Inc(RightW,CenterW);
   if RightW<0 then
    begin
     Inc(LeftW,RightW);
     RightW:=0;
    end;
  end;
 if CenterH<0 then
  begin
   Inc(BottomH,CenterH);
   CenterH:=0;
   if BottomH<0 then
    begin
     Inc(TopH,BottomH);
     BottomH:=0;
    end;
  end;
 if (TopH>0) and (PY<Y+TopH) then
  begin
   if PX<X+LeftW then
    Result:=PixelInRect(TopLeftBitmap,SetTopLeftColor,TopLeftColor,Rect(0,0,LeftW,TopH)) else
   if PX>=X+Width-RightW then
    Result:=PixelInRect(TopRightBitmap,SetTopRightColor,TopRightColor,Rect(Width-RightW,0,Width,TopH)) else
     Result:=PixelInRect(TopBitmap,SetTopColor,TopColor,Rect(LeftW,0,Width-RightW,TopH));
  end else
 if (BottomH>0) and (PY>=Y+Height-BottomH) then
  begin
   if PX<X+LeftW then
    Result:=PixelInRect(BottomLeftBitmap,SetBottomLeftColor,BottomLeftColor,Rect(0,Height-BottomH,LeftW,Height)) else
   if PX>=X+Width-RightW then
    Result:=PixelInRect(BottomRightBitmap,SetBottomRightColor,BottomRightColor,Rect(Width-RightW,Height-BottomH,Width,Height)) else
     Result:=PixelInRect(BottomBitmap,SetBottomColor,BottomColor,Rect(LeftW,Height-BottomH,Width-RightW,Height));
  end else
 if CenterH>0 then
  begin
   if PX<X+LeftW then
    Result:=PixelInRect(LeftBitmap,SetLeftColor,LeftColor,Rect(0,TopH,LeftW,Height-BottomH)) else
   if PX>=X+Width-RightW then
    Result:=PixelInRect(RightBitmap,SetRightColor,RightColor,Rect(Width-RightW,TopH,Width,Height-BottomH)) else
     Result:=PixelInRect(CenterBitmap,SetCenterColor,CenterColor,Rect(LeftW,TopH,Width-RightW,Height-BottomH));
  end;
end;

procedure TSXSkinStyleBoxTileElement.DrawToBitmap(B:TBitmap32;X,Y,Width,Height:Integer;
           DrawRgn:HRGN;AOnGetVariable:TSXOnGetVariable=nil);
var LeftW,CenterW,RightW:Integer;
    TopH,CenterH,BottomH:Integer;
                       R:TRect;
              BB,TBitmap:TBitmap32;
                 TX,TY,A:Integer;
         XOffset,YOffset:Integer; 

 procedure SetSizeParam(Bitmap:TBitmap32;UseColor:Boolean;var AWidth,AHeight:Integer);
 begin
  if UseColor then
   begin
    if AWidth<1 then AWidth:=1;
    if AHeight<1 then AHeight:=1;
   end else
  if Bitmap<>nil then
   begin
    if Bitmap.Width>AWidth then
     AWidth:=Bitmap.Width;
    if Bitmap.Height>AHeight then
     AHeight:=Bitmap.Height;
   end;
 end;

 procedure DrawBitmapToRect(Bitmap:TBitmap32;UseColor:Boolean;Color:TColor32;R:TRect);
 var  TX2,TY2:Integer;
  DRect,SRect:TRect;
          C,D:Integer;
 begin
  if RectInRegion(DrawRgn,R) then
   begin
    if UseColor then
     begin
      OffsetRect(R,X,Y);
      B.FillRectTS(R,Color);
     end else
    if (Bitmap<>nil) and (Bitmap.Width>0) and (Bitmap.Height>0) then
     begin
      if (Bitmap.Width=R.Right-R.Left) and (Bitmap.Height=R.Bottom-R.Top) then
       B.Draw(X+R.Left,Y+R.Top,Bitmap) else
      if (Bitmap.Height=1) and (Bitmap.Width=R.Right-R.Left) then
       begin
        OffsetRect(R,X,Y);
        Bitmap.DrawTo(B,R);
       end else
      if (Bitmap.Width=1) and (Bitmap.Height=R.Bottom-R.Top) then
       begin
        OffsetRect(R,X,Y);
        Bitmap.DrawTo(B,R);
       end else
      if (Bitmap.Width=1) and (Bitmap.Height=1) then
       begin
        OffsetRect(R,X,Y);
        B.FillRectTS(R,Bitmap.Pixel[0,0]);
       end else
      if ResizeMode=irmStretch then
       begin
        OffsetRect(R,X,Y);
        Bitmap.DrawTo(B,R);
       end else
        begin
         TX2:=(R.Right-R.Left) div Bitmap.Width;
         if (R.Right-R.Left) mod Bitmap.Width<>0 then Inc(TX2);
         TY2:=(R.Bottom-R.Top) div Bitmap.Height;
         if (R.Bottom-R.Top) mod Bitmap.Height<>0 then Inc(TY2);
         for C:=0 to TY2 do
          for D:=0 to TX2 do
           begin
            SRect:=Bitmap.BoundsRect;
            DRect:=SRect;
            OffsetRect(DRect,D*Bitmap.Width+R.Left,C*Bitmap.Height+R.Top);
            if R.Left>DRect.Left then
             begin
              Inc(SRect.Left,R.Left-DRect.Left);
              DRect.Left:=R.Left;
             end;
            if R.Top>DRect.Top then
             begin
              Inc(SRect.Top,R.Top-DRect.Top);
              DRect.Top:=R.Top;
             end;
            if R.Right<DRect.Right then
             begin
              Dec(SRect.Right,DRect.Right-R.Right);
              DRect.Right:=R.Right;
             end;
            if R.Bottom<DRect.Bottom then
             begin
              Dec(SRect.Bottom,DRect.Bottom-R.Bottom);
              DRect.Bottom:=R.Bottom;
             end;
            OffsetRect(DRect,X,Y);
            Bitmap.DrawTo(B,DRect,SRect);
           end;
        end;
     end;
   end;
 end;

begin
 XOffset:=0; YOffset:=0;
 if DrawRect<>'' then
  begin
   GetRectFromString(DrawRect,R,AOnGetVariable);
   if not IsRectEmpty(R) then
    begin
     Inc(X,R.Left);
     Inc(Y,R.Top);
     Width:=R.Right-R.Left;
     Height:=R.Bottom-R.Top;
     XOffset:=R.Left;
     YOffset:=R.Top;
     OffsetRgn(DrawRgn,-XOffset,-YOffset);
    end;
  end;
 if Filter.AType<>iftNone then
  begin
   GetRgnBox(DrawRgn,R);
   IntersectRect(R,R,Rect(0,0,Width,Height));
   BB:=TBitmap32.Create;
   BB.SetSize(R.Right-R.Left,R.Bottom-R.Top);
   BB.Clear(0);
   BB.DrawMode:=dmBlend;
   BB.CombineMode:=cmMerge;
   TBitmap:=B;
   B:=BB;
   TX:=X; TY:=Y;
   X:=-R.Left; Y:=-R.Top;
  end else
   begin
    BB:=nil; TBitmap:=nil;
    TX:=0; TY:=0;
   end;
 LeftW:=0; RightW:=0;
 TopH:=0;  BottomH:=0;
 SetSizeParam(TopLeftBitmap,SetTopLeftColor,LeftW,TopH);
 SetSizeParam(TopRightBitmap,SetTopRightColor,RightW,TopH);
 SetSizeParam(BottomLeftBitmap,SetBottomLeftColor,LeftW,BottomH);
 SetSizeParam(BottomRightBitmap,SetBottomRightColor,RightW,BottomH);
 SetSizeParam(LeftBitmap,SetLeftColor,LeftW,A);
 SetSizeParam(RightBitmap,SetRightColor,RightW,A);
 SetSizeParam(TopBitmap,SetTopColor,A,TopH);
 SetSizeParam(BottomBitmap,SetBottomColor,A,BottomH);
 CenterW:=Width-LeftW-RightW;
 CenterH:=Height-TopH-BottomH;
 if CenterW<0 then
  begin
   Inc(LeftW,CenterW div 2);
   Inc(RightW,CenterW-(CenterW div 2));
   CenterW:=0;
   if RightW<0 then
    begin
     Inc(LeftW,RightW);
     RightW:=0;
    end;
  end;
 if CenterH<0 then
  begin
   Inc(TopH,CenterH div 2);
   Inc(BottomH,CenterH-(CenterH div 2));
   CenterH:=0;
   if BottomH<0 then
    begin
     Inc(TopH,BottomH);
     BottomH:=0;
    end;
  end;
 if TopH>0 then
  begin
   if LeftW>0 then
    DrawBitmapToRect(TopLeftBitmap,SetTopLeftColor,TopLeftColor,Rect(0,0,LeftW,TopH));
   if CenterW>0 then
    DrawBitmapToRect(TopBitmap,SetTopColor,TopColor,Rect(LeftW,0,Width-RightW,TopH));
   if RightW>0 then
    DrawBitmapToRect(TopRightBitmap,SetTopRightColor,TopRightColor,Rect(Width-RightW,0,Width,TopH));
  end;
 if CenterH>0 then
  begin
   if LeftW>0 then
    DrawBitmapToRect(LeftBitmap,SetLeftColor,LeftColor,Rect(0,TopH,LeftW,Height-BottomH));
   if CenterW>0 then
    DrawBitmapToRect(CenterBitmap,SetCenterColor,CenterColor,Rect(LeftW,TopH,Width-RightW,Height-BottomH));
   if RightW>0 then
    DrawBitmapToRect(RightBitmap,SetRightColor,RightColor,Rect(Width-RightW,TopH,Width,Height-BottomH));
  end;
 if BottomH>0 then
  begin
   if LeftW>0 then
    DrawBitmapToRect(BottomLeftBitmap,SetBottomLeftColor,BottomLeftColor,Rect(0,Height-BottomH,LeftW,Height));
   if CenterW>0 then
    DrawBitmapToRect(BottomBitmap,SetBottomColor,BottomColor,Rect(LeftW,Height-BottomH,Width-RightW,Height));
   if RightW>0 then
    DrawBitmapToRect(BottomRightBitmap,SetBottomRightColor,BottomRightColor,Rect(Width-RightW,Height-BottomH,Width,Height));
  end;
 if Filter.AType<>iftNone then
  begin
   ApplyFilterToBitmap(BB,Filter);
   TBitmap.Draw(TX-X,TY-Y,BB);
   BB.Free;
  end;
 if (XOffset<>0) or (YOffset<>0) then
  OffsetRgn(DrawRgn,XOffset,YOffset);
end;

procedure TSXSkinStyleBoxTileElement.ValidateBitmaps(SkinLibrary:TSXSkinLibrary);

 procedure LoadSXBitmap(const Path:String;var Bitmap:TBitmap32);
 var FD:TSXFilterData;
 begin
  if (Bitmap=nil) and (Path<>'') then
   begin
    FD.AType:=iftNone;
    FD.Value:=0;
    FD.Color:=0;
    FD.Color2:=0;
    Bitmap:=GetPreloadedBitmap(SkinLibrary,Path,ZipFilePath,False,0,0,isfNearest,FD);
   end;
 end;

begin
 if not SetCenterColor then
  LoadSXBitmap(CenterPath,CenterBitmap);
 if not SetLeftColor then
  LoadSXBitmap(LeftPath,LeftBitmap);
 if not SetTopColor then
  LoadSXBitmap(TopPath,TopBitmap);
 if not SetRightColor then
  LoadSXBitmap(RightPath,RightBitmap);
 if not SetBottomColor then
  LoadSXBitmap(BottomPath,BottomBitmap);
 if not SetTopLeftColor then
 LoadSXBitmap(TopLeftPath,TopLeftBitmap);
 if not SetTopRightColor then
  LoadSXBitmap(TopRightPath,TopRightBitmap);
 if not SetBottomRightColor then
  LoadSXBitmap(BottomRightPath,BottomRightBitmap);
 if not SetBottomLeftColor then
  LoadSXBitmap(BottomLeftPath,BottomLeftBitmap);
end;

procedure TSXSkinStyleBoxTileElement.SetProperty(const Name,Value,SkinFilePath:String);
var T:Integer;

 procedure ProcessPathOrColor(var Path:String;var Color:TColor32;var UseColor:Boolean);
 begin
  if (Value<>'') and (Value[1]='#') then
   begin
    Color:=GetColorFromString(Value);
    UseColor:=True;
   end else
    begin
     Path:=GetFullPath(Value,SkinFilePath);
     UseColor:=False;
    end;
 end;

 procedure ProcessMaskPath(const PathPart:String;var Path:String;var UseColor:Boolean);
 var S:String;
 begin
  S:=Copy(Value,1,T-1)+PathPart+Copy(Value,T+1,MaxInt);
  S:=GetFullPath(S,SkinFilePath);
  if FileExists(S) then
   begin
    Path:=S;
    UseColor:=False;
   end; 
 end;

begin
 if Name='Transparent' then Transparent:=Value='1' else
  if Name='Paths' then
   begin
    T:=Pos('*',Value);
    if T>0 then
     begin
      LoadedPathTemplate:=Value;
      PathTemplate:=Value;
      if not ExtractZipPath(PathTemplate,ZipFilePath) then
       PathTemplate:=GetFullPath(Value,SkinFilePath);
      ProcessMaskPath('center',CenterPath,SetCenterColor);
      ProcessMaskPath('left',LeftPath,SetLeftColor);
      ProcessMaskPath('top',TopPath,SetTopColor);
      ProcessMaskPath('right',RightPath,SetRightColor);
      ProcessMaskPath('bottom',BottomPath,SetBottomColor);
      ProcessMaskPath('topleft',TopLeftPath,SetTopLeftColor);
      ProcessMaskPath('topright',TopRightPath,SetTopRightColor);
      ProcessMaskPath('bottomright',BottomRightPath,SetBottomRightColor);
      ProcessMaskPath('bottomleft',BottomLeftPath,SetBottomLeftColor);
     end;
   end else
  if Name='Center' then
   begin
    SetCenter:=True;
    ProcessPathOrColor(CenterPath,CenterColor,SetCenterColor);
   end else
  if Name='Left' then
   begin
    SetLeft:=True;
    ProcessPathOrColor(LeftPath,LeftColor,SetLeftColor);
   end else
  if Name='Top' then
   begin
    SetTop:=True;
    ProcessPathOrColor(TopPath,TopColor,SetTopColor);
   end else
  if Name='Right' then
   begin
    SetRight:=True;
    ProcessPathOrColor(RightPath,RightColor,SetRightColor);
   end else
  if Name='Bottom' then
   begin
    SetBottom:=True;
    ProcessPathOrColor(BottomPath,BottomColor,SetBottomColor);
   end else
  if Name='TopLeft' then
   begin
    SetTopLeft:=True;
    ProcessPathOrColor(TopLeftPath,TopLeftColor,SetTopLeftColor);
   end else
  if Name='TopRight' then
   begin
    SetTopRight:=True;
    ProcessPathOrColor(TopRightPath,TopRightColor,SetTopRightColor);
   end else
  if Name='BottomRight' then
   begin
    SetBottomRight:=True;
    ProcessPathOrColor(BottomRightPath,BottomRightColor,SetBottomRightColor);
   end else
  if Name='BottomLeft' then
   begin
    SetBottomLeft:=True;
    ProcessPathOrColor(BottomLeftPath,BottomLeftColor,SetBottomLeftColor);
   end else
  if Name='Rect' then DrawRect:=WithoutAllSpaces(Value) else
  if Name='ResizeMode' then
   begin
    SetResizeMode:=True;
    ResizeMode:=GetImageResizeModeByName(Value);
   end else TryToSetFilterDataParameter(Name,Value,Filter);
end;

procedure TSXSkinStyleBoxTileElement.SaveToStream(S:TStream;const RootPath:String);
var HasDrawRect:Boolean;
begin
 inherited;
 Save8Flags(S,SetCenter,SetLeft,SetTop,SetRight,SetBottom,SetTopLeft,SetTopRight,
              SetBottomRight);
 Save8Flags(S,SetCenterColor,SetLeftColor,SetTopColor,SetRightColor,
              SetBottomColor,SetTopLeftColor,SetTopRightColor,SetBottomRightColor);
 HasDrawRect:=(DrawRect<>'');
 Save8Flags(S,SetBottomLeftColor,Transparent,HasDrawRect,SetResizeMode,
              SetBottomLeft,False,False,False);
 SaveFilterData(S,Filter);
 SaveString(S,LoadedPathTemplate);
 if SetCenterColor then
  S.Write(CenterColor,sizeof(CenterColor)) else
   if SetCenter then SaveString(S,GetRelativePath(RootPath,CenterPath));
 if SetLeftColor then
  S.Write(LeftColor,sizeof(LeftColor)) else
   if SetLeft then SaveString(S,GetRelativePath(RootPath,LeftPath));
 if SetTopColor then
  S.Write(TopColor,sizeof(TopColor)) else
   if SetTop then SaveString(S,GetRelativePath(RootPath,TopPath));
 if SetRightColor then
  S.Write(RightColor,sizeof(RightColor)) else
   if SetRight then SaveString(S,GetRelativePath(RootPath,RightPath));
 if SetBottomColor then
  S.Write(BottomColor,sizeof(BottomColor)) else
   if SetBottom then SaveString(S,GetRelativePath(RootPath,BottomPath));
 if SetTopLeftColor then
  S.Write(TopLeftColor,sizeof(TopLeftColor)) else
   if SetTopLeft then SaveString(S,GetRelativePath(RootPath,TopLeftPath));
 if SetTopRightColor then
  S.Write(TopRightColor,sizeof(TopRightColor)) else
   if SetTopRight then SaveString(S,GetRelativePath(RootPath,TopRightPath));
 if SetBottomRightColor then
  S.Write(BottomRightColor,sizeof(BottomRightColor)) else
   if SetBottomRight then SaveString(S,GetRelativePath(RootPath,BottomRightPath));
 if SetBottomLeftColor then
  S.Write(BottomLeftColor,sizeof(BottomLeftColor)) else
   if SetBottomLeft then SaveString(S,GetRelativePath(RootPath,BottomLeftPath));
 if HasDrawRect then
  SaveString(S,DrawRect);
 if SetResizeMode then
  S.Write(ResizeMode,sizeof(ResizeMode));
end;

procedure TSXSkinStyleBoxTileElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
var        A:Integer;
        B,BB:Boolean;
       S1,S2:String;
 HasDrawRect:Boolean;
begin
 inherited;
 Load8Flags(S,SetCenter,SetLeft,SetTop,SetRight,SetBottom,SetTopLeft,SetTopRight,
              SetBottomRight);
 Load8Flags(S,SetCenterColor,SetLeftColor,SetTopColor,SetRightColor,
              SetBottomColor,SetTopLeftColor,SetTopRightColor,SetBottomRightColor);
 Load8Flags(S,SetBottomLeftColor,Transparent,HasDrawRect,SetResizeMode,SetBottomLeft,B,B,B);
 LoadFilterData(S,Filter);
 LoadString(S,LoadedPathTemplate);
 BB:=False;
 PathTemplate:=LoadedPathTemplate;
 if not ExtractZipPath(PathTemplate,Self.ZipFilePath) then
  begin
   Self.ZipFilePath:=ZipFilePath;
   BB:=True;
  end; 
 if PathTemplate<>'' then
  begin
   if BB then
    PathTemplate:=GetFullPath(PathTemplate,RootPath);
   A:=Pos('*',PathTemplate);
   if A>0 then
    begin
     S1:=Copy(PathTemplate,1,A-1);
     S2:=Copy(PathTemplate,A+1,MaxInt);
     CenterPath:=S1+'center'+S2;
     LeftPath:=S1+'left'+S2;
     TopPath:=S1+'top'+S2;
     RightPath:=S1+'right'+S2;
     BottomPath:=S1+'bottom'+S2;
     TopLeftPath:=S1+'topleft'+S2;
     TopRightPath:=S1+'topright'+S2;
     BottomRightPath:=S1+'bottomright'+S2;
     BottomLeftPath:=S1+'bottomleft'+S2;
    end;
  end;
 if SetCenterColor then
  S.Read(CenterColor,sizeof(CenterColor)) else
   if SetCenter then LoadString(S,CenterPath);
 if SetLeftColor then
  S.Read(LeftColor,sizeof(LeftColor)) else
   if SetLeft then LoadString(S,LeftPath);
 if SetTopColor then
  S.Read(TopColor,sizeof(TopColor)) else
   if SetTop then LoadString(S,TopPath);
 if SetRightColor then
  S.Read(RightColor,sizeof(RightColor)) else
   if SetRight then LoadString(S,RightPath);
 if SetBottomColor then
  S.Read(BottomColor,sizeof(BottomColor)) else
   if SetBottom then LoadString(S,BottomPath);
 if SetTopLeftColor then
  S.Read(TopLeftColor,sizeof(TopLeftColor)) else
   if SetTopLeft then LoadString(S,TopLeftPath);
 if SetTopRightColor then
  S.Read(TopRightColor,sizeof(TopRightColor)) else
   if SetTopRight then LoadString(S,TopRightPath);
 if SetBottomRightColor then
  S.Read(BottomRightColor,sizeof(BottomRightColor)) else
   if SetBottomRight then LoadString(S,BottomRightPath);
 if SetBottomLeftColor then
  S.Read(BottomLeftColor,sizeof(BottomLeftColor)) else
   if SetBottomLeft then LoadString(S,BottomLeftPath);
 if HasDrawRect then
  LoadString(S,DrawRect) else DrawRect:='';
 if SetResizeMode then
  S.Read(ResizeMode,sizeof(ResizeMode));
end;

constructor TSXSkinStyleBoxTileElement.Create;
begin
 inherited Create;
 Transparent:=True;
 ResizeMode:=irmTile;
end;

destructor TSXSkinStyleBoxTileElement.Destroy;
begin
 inherited Destroy;
end;

{ TSXFigureControlPreparedData }

destructor TSXFigureControlPreparedData.Destroy;
begin
 Polygon1.Free;
 Polygon2.Free;
 if VComparer<>nil then
  begin
   VComparer.DestroyVarVals(RectValueVarList,RectValueVarVals);
   VComparer.DestroyVarList(RectValueVarList);
   VComparer.DestroyVarVals(PolyValueVarList,PolyValueVarVals);
   VComparer.DestroyVarList(PolyValueVarList);
  end;
 inherited Destroy;
end;

{ TSXFigureControlPreparedDataList }

function TSXFigureControlPreparedDataList.Get(Index:Integer):TSXFigureControlPreparedData;
begin
 Result:=TSXFigureControlPreparedData(FItem[Index]);
end;

procedure TSXFigureControlPreparedDataList.Put(Index:Integer;Item:TSXFigureControlPreparedData);
begin
 FItem[Index]:=Item;
end;

function TSXFigureControlPreparedDataList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

function TSXFigureControlPreparedDataList.GetIndexByControl(CElementID:Integer):Integer;
var A,L,H:Integer;
        B:Boolean;
begin
 B:=False;
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   if Item[A].CElementID<CElementID then L:=A+1 else
    begin
     H:=A-1;
     if Item[A].CElementID=CElementID then
      begin
       L:=A;
       B:=True;
      end;
    end;
  end;
 if B then Result:=L else Result:=-1;
end;

function TSXFigureControlPreparedDataList.GetIndexToInsert(CElementID:Integer):Integer;
var A,L,H:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   if Item[A].CElementID<CElementID then L:=A+1 else
    begin
     H:=A-1;
     if Item[A].CElementID=CElementID then L:=A;
    end;
  end;
 Result:=L;
end;

procedure TSXFigureControlPreparedDataList.Add(SXFigureControlPreparedData:TSXFigureControlPreparedData);
var A:Integer;
begin
 A:=GetIndexToInsert(SXFigureControlPreparedData.CElementID);
 FItem.Insert(A,SXFigureControlPreparedData);
end;

procedure TSXFigureControlPreparedDataList.Delete(Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXFigureControlPreparedDataList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

constructor TSXFigureControlPreparedDataList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXFigureControlPreparedDataList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

{ TSXSkinStyleFigureElement }

procedure TSXSkinStyleFigureElement.Assign(Element:TSXSkinStyleElement);
begin
 inherited;
 if Element is TSXSkinStyleFigureElement then
  begin
   HasBorder:=TSXSkinStyleFigureElement(Element).HasBorder;
   HasFill:=TSXSkinStyleFigureElement(Element).HasFill;
   Antialiased:=TSXSkinStyleFigureElement(Element).Antialiased;
   FigureType:=TSXSkinStyleFigureElement(Element).FigureType;
   BorderColor:=TSXSkinStyleFigureElement(Element).BorderColor;
   FillColor:=TSXSkinStyleFigureElement(Element).FillColor;
   FillColor2:=TSXSkinStyleFigureElement(Element).FillColor2;
   GradientFill:=TSXSkinStyleFigureElement(Element).GradientFill;
   VertGradientFill:=TSXSkinStyleFigureElement(Element).VertGradientFill;
   Roundness:=TSXSkinStyleFigureElement(Element).Roundness;
   RectValue:=TSXSkinStyleFigureElement(Element).RectValue;
   PolyValue:=TSXSkinStyleFigureElement(Element).PolyValue;
   CornersStyle:=TSXSkinStyleFigureElement(Element).CornersStyle;
   BorderThickness:=TSXSkinStyleFigureElement(Element).BorderThickness;
   ControlsData.Clear;
  end; 
end;

function TSXSkinStyleFigureElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleFigureElement.Create;
 Result.Assign(Self);
end;

procedure TSXSkinStyleFigureElement.DrawToBitmap(B:TBitmap32;X,Y:Integer);
var T:TSXFigureControlPreparedData;
    A:Integer;
begin
 A:=ControlsData.GetIndexByControl(FCElementID);
 if A<0 then exit;
 T:=ControlsData[A];
 case FigureType of
  ssftRectangle:      begin
                       if (T.Polygon1<>nil) and ((T.Polygon1Offset.X<>X) or (T.Polygon1Offset.Y<>Y)) then
                        begin
                         T.Polygon1.Offset(Fixed(X-T.Polygon1Offset.X),Fixed(Y-T.Polygon1Offset.Y));
                         T.Polygon1Offset.X:=X;
                         T.Polygon1Offset.Y:=Y;
                        end;
                       if (T.Polygon2<>nil) and ((T.Polygon2Offset.X<>X) or (T.Polygon2Offset.Y<>Y)) then
                        begin
                         T.Polygon2.Offset(Fixed(X-T.Polygon2Offset.X),Fixed(Y-T.Polygon2Offset.Y));
                         T.Polygon2Offset.X:=X;
                         T.Polygon2Offset.Y:=Y;
                        end;
                       if HasFill then
                        begin
                         if GradientFill then
                          begin
                           if VertGradientFill then
                            RectVFadeT(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top) else
                             RectHFadeT(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top);
                          end else
                           B.FillRectTS(X+T.Rect.Left,Y+T.Rect.Top,X+T.Rect.Right,Y+T.Rect.Bottom,FillColor);
                        end;
                       if HasBorder then
                        begin
                         if BorderThickness=0 then
                          T.Polygon1.DrawEdge(B,BorderColor) else
                           begin
                            T.Polygon2.DrawFill(B,BorderColor);
                           end;
                        end;
                      end;
  ssftEllipse:        begin
                       if (T.Polygon1<>nil) and ((T.Polygon1Offset.X<>X) or (T.Polygon1Offset.Y<>Y)) then
                        begin
                         T.Polygon1.Offset(Fixed(X-T.Polygon1Offset.X),Fixed(Y-T.Polygon1Offset.Y));
                         T.Polygon1Offset.X:=X;
                         T.Polygon1Offset.Y:=Y;
                        end;
                       if (T.Polygon2<>nil) and ((T.Polygon2Offset.X<>X) or (T.Polygon2Offset.Y<>Y)) then
                        begin
                         T.Polygon2.Offset(Fixed(X-T.Polygon2Offset.X),Fixed(Y-T.Polygon2Offset.Y));
                         T.Polygon2Offset.X:=X;
                         T.Polygon2Offset.Y:=Y;
                        end;
                       if HasFill then
                        begin
                         if GradientFill then
                          begin
                           if VertGradientFill then
                            EllipseVFade(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top) else
                             EllipseHFade(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top);
                          end else EllipseFill(B,FillColor,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top);
                        end;
                       if HasBorder then
                        begin
                         if BorderThickness=0 then
                          T.Polygon1.DrawEdge(B,BorderColor) else
                           begin
                            T.Polygon2.DrawFill(B,BorderColor);
                           end;
                        end;
                      end;
  ssftRoundRectangle: begin
                       if (T.Polygon1<>nil) and ((T.Polygon1Offset.X<>X) or (T.Polygon1Offset.Y<>Y)) then
                        begin
                         T.Polygon1.Offset(Fixed(X-T.Polygon1Offset.X),Fixed(Y-T.Polygon1Offset.Y));
                         T.Polygon1Offset.X:=X;
                         T.Polygon1Offset.Y:=Y;
                        end;
                       if (T.Polygon2<>nil) and ((T.Polygon2Offset.X<>X) or (T.Polygon2Offset.Y<>Y)) then
                        begin
                         T.Polygon2.Offset(Fixed(X-T.Polygon2Offset.X),Fixed(Y-T.Polygon2Offset.Y));
                         T.Polygon2Offset.X:=X;
                         T.Polygon2Offset.Y:=Y;
                        end;
                       if HasFill then
                        begin
                         if GradientFill then
                          begin
                           if VertGradientFill then
                            RoundRectVFadeT(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top,Roundness,CornersStyle) else
                             RoundRectHFadeT(B,FillColor,FillColor2,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top,Roundness,CornersStyle);
                          end else
                           RoundRectFill(B,FillColor,X+T.Rect.Left,Y+T.Rect.Top,T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top,Roundness);
                        end;
                       if HasBorder then
                        begin
                         if BorderThickness=0 then
                          begin
                            T.Polygon1.DrawEdge(B,BorderColor);
                          end else
                           begin
                            T.Polygon2.DrawFill(B,BorderColor);
                           end;
                        end;
                      end;
  ssftLine,
  ssftPolygon:        begin
                       if (T.Polygon1<>nil) and ((T.Polygon1Offset.X<>X) or (T.Polygon1Offset.Y<>Y)) then
                        begin
                         T.Polygon1.Offset(Fixed(X-T.Polygon1Offset.X),Fixed(Y-T.Polygon1Offset.Y));
                         T.Polygon1Offset.X:=X;
                         T.Polygon1Offset.Y:=Y;
                        end;
                       if (T.Polygon2<>nil) and ((T.Polygon2Offset.X<>X) or (T.Polygon2Offset.Y<>Y)) then
                        begin
                         T.Polygon2.Offset(Fixed(X-T.Polygon2Offset.X),Fixed(Y-T.Polygon2Offset.Y));
                         T.Polygon2Offset.X:=X;
                         T.Polygon2Offset.Y:=Y;
                        end;
                       if HasFill then
                        begin
                         T.Polygon1.DrawFill(B,FillColor);
                        end;
                       if HasBorder then
                        begin
                         if BorderThickness=0 then
                          begin
                           T.Polygon1.DrawEdge(B,BorderColor);
                          end else
                           begin
                            T.Polygon2.DrawFill(B,BorderColor);
                           end;
                        end;
                      end;
  ssftFocusRectangle: begin
                       OffsetRect(T.Rect,X,Y);
                       DrawFocusRect(B.Handle,T.Rect);
                       OffsetRect(T.Rect,-X,-Y);
                      end;
  ssftEraseRect:      begin
                       OffsetRect(T.Rect,X,Y);
                       B.FillRectS(T.Rect.Left,T.Rect.Top,T.Rect.Right,T.Rect.Bottom,0);
                       OffsetRect(T.Rect,-X,-Y);
                      end;
 end;
end;

function TSXSkinStyleFigureElement.OnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if FControl<>nil then
  begin
   if VarName='W' then
    begin
     Result:=FControl.Width;
     exit;
    end;
   if VarName='H' then
    begin
     Result:=FControl.Height;
     exit;
    end;
  end;
 Error:=True;
end;

procedure TSXSkinStyleFigureElement.EvalPrePaintParams(Control:TControl;
           CElementID:Integer;VComparer:TSXVariableComparer=nil;Changed:Boolean=False);
var A:Integer;
    T:TSXFigureControlPreparedData;
   TP:TPolygon32;
begin
 if @VComparer=nil then
  begin
   VComparer:=StdVariableComparer;
   if Control<>nil then
    StdVariableComparer.SetSize(Control.Width,Control.Height);
  end;
 FControl:=Control;
 FCElementID:=CElementID;
 A:=ControlsData.GetIndexByControl(CElementID);
 if A<0 then
  begin
   T:=TSXFigureControlPreparedData.Create;
   T.Control:=Control;
   T.CElementID:=CElementID;
   if RectValue<>'' then
    begin
     T.RectValueVarList:=VComparer.GetVarListForRect(RectValue);
     T.RectValueVarVals:=VComparer.GetVarValsForVarList(T.RectValueVarList);
     T.VComparer:=VComparer;
    end;
   if PolyValue<>'' then
    begin
     T.PolyValueVarList:=VComparer.GetVarListForPoly(PolyValue);
     T.PolyValueVarVals:=VComparer.GetVarValsForVarList(T.PolyValueVarList);
     T.VComparer:=VComparer;
    end;
  end else T:=ControlsData[A];
 if (A<0) or VComparer.Changed(T.RectValueVarList,T.RectValueVarVals) or
    VComparer.Changed(T.PolyValueVarList,T.PolyValueVarVals) then
  begin
   if RectValue<>'' then
    begin
     GetRectFromString(RectValue,T.Rect,VComparer.OnGetVariable);
     VComparer.Update(T.RectValueVarList,T.RectValueVarVals);
    end;

   T.Polygon1.Free;
   T.Polygon2.Free;
   T.Polygon1:=nil;
   T.Polygon2:=nil;
   if HasFill or HasBorder then
    begin
     T.Polygon1:=TPolygon32.Create;
     case FigureType of
      ssftRectangle:      SetRectangle(T.Polygon1,T.Rect.Left,T.Rect.Top,
                            T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top);
      ssftEllipse:        SetEllipse(T.Polygon1,(T.Rect.Left+T.Rect.Right-1)/2,
                            (T.Rect.Top+T.Rect.Bottom-1)/2,
                            (T.Rect.Right-T.Rect.Left-1)/2,
                            (T.Rect.Bottom-T.Rect.Top-1)/2);
      ssftRoundRectangle: SetRoundRectangle(T.Polygon1,T.Rect.Left,T.Rect.Top,
                            T.Rect.Right-T.Rect.Left,T.Rect.Bottom-T.Rect.Top,
                            Roundness,CornersStyle);
      ssftLine,
      ssftPolygon:        begin
                           GetPolygonFromString(PolyValue,T.Polygon1,VComparer.OnGetVariable);
                           VComparer.Update(T.PolyValueVarList,T.PolyValueVarVals);
                           T.Polygon1.Closed:=(FigureType=ssftPolygon);
                          end;
     end;
     T.Polygon1.Antialiased:=Antialiased;
     T.Polygon1.AntialiasMode:=am4times;
     T.Polygon1Offset.X:=0;
     T.Polygon1Offset.Y:=0;
    end;
   if HasBorder and (BorderThickness<>0) then
    begin
     TP:=T.Polygon1.Outline;
     T.Polygon2:=TP.Grow(Fixed(BorderThickness/2),0.5);
     T.Polygon2.Offset(Fixed(0.5),Fixed(0.5));
     T.Polygon2.FillMode:=pfWinding;
     TP.Free;
     T.Polygon2Offset.X:=0;
     T.Polygon2Offset.Y:=0;
    end;
  end;
 if A<0 then ControlsData.Add(T);
end;

procedure TSXSkinStyleFigureElement.SetProperty(const Name,Value,SkinFilePath:String);
begin
 if Name='HasBorder' then HasBorder:=Value='1' else
  if Name='HasFill' then HasFill:=Value='1' else
  if Name='Antialiased' then Antialiased:=Value='1' else
  if Name='Rect' then RectValue:=WithoutAllSpaces(Value) else
  if Name='Poly' then PolyValue:=WithoutAllSpaces(Value) else
  if Name='Fill' then
   begin
    VertGradientFill:=Value='GRADIENT_V';
    GradientFill:=VertGradientFill or (Value='GRADIENT_H');
   end else
  if Name='BorderColor' then
   BorderColor:=GetColorFromString(Value) else
  if (Name='FillColor') or (Name='FillColor1') then
   FillColor:=GetColorFromString(Value) else
  if Name='FillColor2' then
   FillColor2:=GetColorFromString(Value) else
  if Name='Roundness' then Roundness:=StrToIntDef(Value,0) else
  if Name='CornersStyle' then Byte(CornersStyle):=StrToIntDef(Value,0) else
  if Name='BorderThickness' then BorderThickness:=SXStrToFloatDef(Value,0);
end;

procedure TSXSkinStyleFigureElement.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 Save8Flags(S,HasBorder,HasFill,GradientFill,VertGradientFill,Antialiased,
              False,False,False);
 S.Write(FigureType,sizeof(FigureType));
 if HasBorder then
  begin
   S.Write(BorderColor,sizeof(BorderColor));
   S.Write(BorderThickness,sizeof(BorderThickness));
  end; 
 if HasFill or (FigureType=ssftSolidFill) then
  begin
   S.Write(FillColor,sizeof(FillColor));
   if GradientFill or VertGradientFill then
    S.Write(FillColor2,sizeof(FillColor2));
  end;
 if FigureType=ssftRoundRectangle then
  begin
   S.Write(Roundness,sizeof(Roundness));
   S.Write(CornersStyle,sizeof(CornersStyle));
  end;
 if FigureType in [ssftLine,ssftPolygon] then
  SaveString(S,PolyValue) else
   SaveString(S,RectValue);
end;

procedure TSXSkinStyleFigureElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
var B:Boolean;
begin
 inherited;
 Load8Flags(S,HasBorder,HasFill,GradientFill,VertGradientFill,Antialiased,B,B,B);
 S.Read(FigureType,sizeof(FigureType));
 if HasBorder then
  begin
   S.Read(BorderColor,sizeof(BorderColor));
   S.Read(BorderThickness,sizeof(BorderThickness));
  end;
 if HasFill or (FigureType=ssftSolidFill) then
  begin
   S.Read(FillColor,sizeof(FillColor));
   if GradientFill or VertGradientFill then
    S.Read(FillColor2,sizeof(FillColor2));
  end;
 if FigureType=ssftRoundRectangle then
  begin
   S.Read(Roundness,sizeof(Roundness));
   S.Read(CornersStyle,sizeof(CornersStyle));
  end;
 if FigureType in [ssftLine,ssftPolygon] then
  LoadString(S,PolyValue) else
   LoadString(S,RectValue);
end;

constructor TSXSkinStyleFigureElement.Create;
begin
 inherited Create;
 ControlsData:=TSXFigureControlPreparedDataList.Create;
 CornersStyle:=[crLeftTop,crRightTop,crRightBottom,crLeftBottom];
 Antialiased:=True;
end;

destructor TSXSkinStyleFigureElement.Destroy;
begin
 ControlsData.Free;
 inherited Destroy;
end;

{ TSXSkinStyleTextElement }

procedure TSXSkinStyleTextElement.Assign(Element:TSXSkinStyleElement);
begin
 inherited;
 if Element is TSXSkinStyleTextElement then
  begin
   Alignment:=TSXSkinStyleTextElement(Element).Alignment;
   VerticalAlignment:=TSXSkinStyleTextElement(Element).VerticalAlignment;
   RectValue:=TSXSkinStyleTextElement(Element).RectValue;
   Text:=TSXSkinStyleTextElement(Element).Text;
   FontData:=TSXSkinStyleTextElement(Element).FontData;
  end;
end;

function TSXSkinStyleTextElement.GetCopy:TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleTextElement.Create;
 Result.Assign(Self);
end;

procedure TSXSkinStyleTextElement.DrawToBitmap(B:TBitmap32;X,Y:Integer);
var     FD:TSXFontData;
     Flags:Cardinal;
 FTextRect:TRect;
   OldRect:TRect;
         A:Integer;
begin
 FD:=FontData;
 if (FControl<>nil) and (FControl is TSXSkinCustomControl) then
  SetDefaultFontData(FD,TSXSkinCustomControl(FControl).Font);
 B.Font.Name:=FD.FontName;
 B.Font.Size:=FD.FontSize;
 B.Font.Style:=FD.FontStyle;
 B.Canvas.Font:=B.Font;
 Flags:=DT_CALCRECT or DT_NOPREFIX or DT_NOCLIP or DT_TOP or DT_WORDBREAK;
 case Alignment of
  taLeftJustify:  Flags:=Flags or DT_LEFT;
  taRightJustify: Flags:=Flags or DT_RIGHT;
  taCenter:       Flags:=Flags or DT_CENTER;
 end;
 //
 FTextRect:=TextRect;
 OldRect:=FTextRect;
 if FD.SmoothLevel=0 then
  DrawText(B.Canvas.Handle,PChar(Text),-1,FTextRect,Flags) else
   DrawSmoothText(B.Canvas,Text,FTextRect,Flags,FD.SmoothLevel);
 if (Alignment=taRightJustify) and (FTextRect.Right<OldRect.Right) then
  begin
   FTextRect.Left:=OldRect.Right-FTextRect.Right+FTextRect.Left;
   FTextRect.Right:=OldRect.Right;
  end;
 if (Alignment=taCenter) and (FTextRect.Right<OldRect.Right) then
  begin
   A:=(OldRect.Right-FTextRect.Right) div 2;
   Inc(FTextRect.Left,A);
   Inc(FTextRect.Right,A);
  end;
 if (VerticalAlignment=taVerticalCenter) and (FTextRect.Bottom<OldRect.Bottom) then
  begin
   A:=(OldRect.Bottom-FTextRect.Bottom) div 2;
   Inc(FTextRect.Top,A);
   Inc(FTextRect.Bottom,A);
  end else
 if (VerticalAlignment=taAlignBottom) and (FTextRect.Bottom<OldRect.Bottom) then
  begin
   FTextRect.Top:=OldRect.Bottom-FTextRect.Bottom+FTextRect.Top;
   FTextRect.Bottom:=OldRect.Bottom;
  end;
 //
 Flags:=Flags and not DT_CALCRECT; 
 OffsetRect(FTextRect,X,Y);
 if FD.HasShadow then
  begin
   OffsetRect(FTextRect,1,1);
   if FD.SmoothLevel=0 then
    DrawAlphaText(B,Text,FTextRect,Flags,FD.ShadowColor) else
     DrawSmoothText(B,Text,FTextRect,Flags,FD.SmoothLevel,FD.ShadowColor);
   OffsetRect(FTextRect,-1,-1);
  end;
 if FD.SmoothLevel=0 then
  DrawAlphaText(B,Text,FTextRect,Flags,FD.FontColor) else
   DrawSmoothText(B,Text,FTextRect,Flags,FD.SmoothLevel,FD.FontColor);
end;

function TSXSkinStyleTextElement.OnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if FControl<>nil then
  begin
   if VarName='W' then
    begin
     Result:=FControl.Width;
     exit;
    end;
   if VarName='H' then
    begin
     Result:=FControl.Height;
     exit;
    end;
  end;
 Error:=True;
end;

procedure TSXSkinStyleTextElement.EvalPrePaintParams(Control:TControl;
           CElementID:Integer;VComparer:TSXVariableComparer=nil;Changed:Boolean=False);
begin
 if @VComparer=nil then
  VComparer:=StdVariableComparer;
 FControl:=Control;
 FCElementID:=CElementID;
 GetRectFromString(RectValue,TextRect,VComparer.OnGetVariable);
end;

procedure TSXSkinStyleTextElement.SetProperty(const Name,Value,SkinFilePath:String);
begin
 if Name='Text' then Text:=Value else
  if Name='Align' then
   begin
    if Value='Center' then Alignment:=taCenter else
     if Value='Right' then Alignment:=taRightJustify else
      Alignment:=taLeftJustify;
   end else
  if Name='VAlign' then
   begin
    if Value='Center' then VerticalAlignment:=taVerticalCenter else
     if Value='Bottom' then VerticalAlignment:=taAlignBottom else
      VerticalAlignment:=taAlignTop;    
   end else
  if Name='Rect' then RectValue:=WithoutAllSpaces(Value) else 
    TryToSetFontDataParameter(Name,Value,FontData);
end;

procedure TSXSkinStyleTextElement.SaveToStream(S:TStream;const RootPath:String);
var B:Byte;
begin
 inherited;
 B:=Byte(Alignment) or (Byte(VerticalAlignment) shl 4);
 S.Write(B,sizeof(B));
 SaveString(S,RectValue);
 SaveString(S,Text);
 SaveFontData(S,FontData);
end;

procedure TSXSkinStyleTextElement.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String);
var B:Byte;
begin
 inherited;
 S.Read(B,sizeof(B));
 Alignment:=TAlignment(B and $F);
 VerticalAlignment:=TVerticalAlignment(B shr 4);
 LoadString(S,RectValue);
 LoadString(S,Text);
 LoadFontData(S,FontData);
end;

{ TSXSkinStyle }

procedure TSXSkinStyle.AssignStyle(Style:TSXSkinStyle);
begin
end;

procedure TSXSkinStyle.SetParameter(const Name,Value:String);
begin
end;

procedure TSXSkinStyle.SaveToStream(S:TStream;const RootPath:String);
var AType:TSXSkinStyleType;
begin
 if Self is TSXSkinGeneralStyle then AType:=sstGeneral else
  if Self is TSXSkinSelectiveStyle then AType:=sstSelective else
  if Self is TSXSkinLabelStyle then AType:=sstLabel else
  if Self is TSXSkinCheckBoxStyle then AType:=sstCheckBox else
  if Self is TSXSkinRadioButtonStyle then AType:=sstRadioButton else
  if Self is TSXSkinGroupBoxStyle then AType:=sstGroupBox else
  if Self is TSXSkinMultiStateStyle then AType:=sstMultiState else
  if Self is TSXSkinMultiStateCheckStyle then AType:=sstMultiStateCheck else
  if Self is TSXSkinButtonStyle then AType:=sstButton else
  if Self is TSXSkinEditStyle then AType:=sstEdit else
  if Self is TSXSkinFormStyle then AType:=sstForm else
  if Self is TSXSkinUpDownStyle then AType:=sstUpDown else
  if Self is TSXSkinSpinEditStyle then AType:=sstSpinEdit else
   AType:=sstUnknown;
 S.Write(AType,sizeof(AType));
 SaveString(S,Name);
 SaveListToStream(S,BasedOnList);
end;

procedure TSXSkinStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var A,B:Integer;
begin
 LoadString(S,Name);
 LoadListFromStream(S,BasedOnList);
 for A:=0 to BasedOnList.Count-1 do
  begin
   B:=DestList1.GetIndexByName(BasedOnList[A]);
   if B>=0 then AssignStyle(DestList1[B]) else
    begin
     B:=DestList2.GetIndexByName(BasedOnList[A]);
     if B>=0 then AssignStyle(DestList2[B]);
    end;
  end;
end;

constructor TSXSkinStyle.Create;
begin
 inherited;
 BasedOnList:=TStringList.Create;
end;

destructor TSXSkinStyle.Destroy;
begin
 BasedOnList.Free;
 inherited;
end;

{ TSXSkinGeneralStyle }

procedure TSXSkinGeneralStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinGeneralStyle then
  begin
   ZipFilePath:=TSXSkinGeneralStyle(Style).ZipFilePath;
   SkinFilePath:=TSXSkinGeneralStyle(Style).SkinFilePath;
   Elements.Assign(TSXSkinGeneralStyle(Style).Elements);
   SetTransparent:=TSXSkinGeneralStyle(Style).SetTransparent;
   SetUseBuffering:=TSXSkinGeneralStyle(Style).SetUseBuffering;
   SetMouseCaptureByTransparency:=TSXSkinGeneralStyle(Style).SetMouseCaptureByTransparency;
   SetMouseCaptureTransparencyLimit:=TSXSkinGeneralStyle(Style).SetMouseCaptureTransparencyLimit;
   SetMouseCaptureRegion:=TSXSkinGeneralStyle(Style).SetMouseCaptureRegion;
   //
   Transparent:=TSXSkinGeneralStyle(Style).Transparent;
   UseBuffering:=TSXSkinGeneralStyle(Style).UseBuffering;
   MouseCaptureByTransparency:=TSXSkinGeneralStyle(Style).MouseCaptureByTransparency;
   MouseCaptureTransparencyLimit:=TSXSkinGeneralStyle(Style).MouseCaptureTransparencyLimit;
   MouseCaptureRegion:=TSXSkinGeneralStyle(Style).MouseCaptureRegion;
  end;
end;

procedure TSXSkinGeneralStyle.SetParameter(const Name,Value:String);
begin
 if Name='Transparent' then
  begin
   Transparent:=Value='1';
   SetTransparent:=True;
  end else
 if Name='UseBuffering' then
  begin
   UseBuffering:=Value='1';
   SetUseBuffering:=True;
  end else
 if Name='MouseCapture' then
  begin
   if Value='ByTransparency' then
    begin
     MouseCaptureByTransparency:=True;
     SetMouseCaptureByTransparency:=True;
    end else
   if Value='Full' then
    begin
     MouseCaptureByTransparency:=False;
     SetMouseCaptureByTransparency:=False;
     MouseCaptureRegion:='';
     SetMouseCaptureRegion:=False;
    end else
     begin
      MouseCaptureRegion:=Value;
      SetMouseCaptureRegion:=True;
     end;
  end else
 if Name='MouseCaptureTransparencyLimit' then
  begin
   MouseCaptureTransparencyLimit:=StrToIntDef(Value,250);
   SetMouseCaptureTransparencyLimit:=True;
  end;
end;

function TSXSkinGeneralStyle.NestedOnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 if VarName='W' then
  begin
   Result:=TmpWidth;
   exit;
  end;
 if VarName='H' then
  begin
   Result:=TmpHeight;
   exit;
  end;
 Result:=TmpOnGetVariable(VarName,Error);
end;

procedure TSXSkinGeneralStyle.DrawToBitmap(Control:TControl;CElementID:Integer;
           Bitmap:TBitmap32;X,Y,Width,Height:Integer;Rect:TRect;Rgn:HRGN;
           SkinLibrary:TSXSkinLibrary;VComparer:TSXVariableComparer=nil;
           ForceEvalParams:Boolean=False);
var    A,C,D:Integer;
       Image:TSXSkinStyleImageElement;
       Style:TSXSkinStyleStyleElement;
      GStyle:TSXSkinGeneralStyle;
      Figure:TSXSkinStyleFigureElement;
        Text:TSXSkinStyleTextElement;
     BoxTile:TSXSkinStyleBoxTileElement;
 DRect,SRect:TRect;
   R,TmpRect:TRect;
       TX,TY:Integer;
     TX2,TY2:Integer;
  TBitmap,BB:TBitmap32;
   TmpX,TmpY:Integer;
    TmpWidth:Integer;
   TmpHeight:Integer;
     RestCmp:Boolean;
begin
 RestCmp:=False;
 if VComparer=nil then
  begin
   StdVariableComparer.GetSize(TmpWidth,TmpHeight);
   StdVariableComparer.SetSize(Width,Height);
   VComparer:=StdVariableComparer;
   RestCmp:=True;
  end;
 if UseBuffering then
  begin
   TBitmap:=Bitmap;
   Bitmap:=TBitmap32.Create;
   Bitmap.DrawMode:=dmBlend;
   Bitmap.CombineMode:=cmMerge;
   Bitmap.SetSize(Rect.Right-Rect.Left,Rect.Bottom-Rect.Top);
   Bitmap.Clear(0);
   TmpX:=X; TmpY:=Y;
   X:=0; Y:=0;
  end;
 for A:=0 to Elements.Count-1 do
  begin
   if Elements[A] is TSXSkinStyleImageElement then
    begin
     Image:=TSXSkinStyleImageElement(Elements[A]);
     Image.ValidateBitmap(SkinLibrary);
     if (Image.Bitmap<>nil) and (Image.Bitmap.Width>0) and (Image.Bitmap.Height>0) then
      begin
       if Image.DrawRect<>'' then
        begin
         GetRectFromString(Image.DrawRect,R,VComparer.OnGetVariable);
         case Image.ResizeMode of
          irmTile: begin
                    IntersectRect(TmpRect,R,Rect);
                    if not IsRectEmpty(TmpRect) then
                     begin
                      OffsetRect(TmpRect,-Image.OffsetX,-Image.OffsetY);
                      TX:=Floor(TmpRect.Left/Image.Bitmap.Width);
                      TY:=Floor(TmpRect.Top/Image.Bitmap.Height);
                      TX2:=Ceil(TmpRect.Right/Image.Bitmap.Width);
                      TY2:=Ceil(TmpRect.Bottom/Image.Bitmap.Height);
                      for C:=TY to TY2 do
                       for D:=TX to TX2 do
                        begin
                         SRect:=Image.Bitmap.BoundsRect;
                         DRect:=SRect;
                         OffsetRect(DRect,D*Image.Bitmap.Width,C*Image.Bitmap.Height);
                         if TmpRect.Left>DRect.Left then
                          begin
                           Inc(SRect.Left,TmpRect.Left-DRect.Left);
                           DRect.Left:=TmpRect.Left;
                          end;
                         if TmpRect.Top>DRect.Top then
                          begin
                           Inc(SRect.Top,TmpRect.Top-DRect.Top);
                           DRect.Top:=TmpRect.Top;
                          end;
                         if TmpRect.Right<DRect.Right then
                          begin
                           Dec(SRect.Right,DRect.Right-TmpRect.Right);
                           DRect.Right:=TmpRect.Right;
                          end;
                         if TmpRect.Bottom<DRect.Bottom then
                          begin
                           Dec(SRect.Bottom,DRect.Bottom-TmpRect.Bottom);
                           DRect.Bottom:=TmpRect.Bottom;
                          end;
                         OffsetRect(DRect,X-Rect.Left,Y-Rect.Top);
                         Image.Bitmap.DrawTo(Bitmap,DRect,SRect);
                        end;
                     end;
                   end;
          else     begin
                    IntersectRect(TmpRect,R,Rect);
                    if not IsRectEmpty(TmpRect) then
                     Image.Bitmap.DrawTo(Bitmap,Types.Rect(X-Rect.Left+R.Left,Y-Rect.Top+R.Top,X-Rect.Left+R.Right,Y-Rect.Top+R.Bottom));
                   end;
         end;
        end else
       if Image.Centered then
        begin
         TX:=(Width-Image.Bitmap.Width) div 2;
         TY:=(Height-Image.Bitmap.Height) div 2;
         Image.Bitmap.DrawTo(Bitmap,X-Rect.Left+TX,Y-Rect.Top+TY);
        end else
         begin
          case Image.ResizeMode of
           irmNone:    Image.Bitmap.DrawTo(Bitmap,X-Rect.Left+Image.OffsetX,Y-Rect.Top+Image.OffsetY);
           irmTile:    begin
                        OffsetRect(Rect,-Image.OffsetX,-Image.OffsetY);
                        TX:=Floor(Rect.Left/Image.Bitmap.Width);
                        TY:=Floor(Rect.Top/Image.Bitmap.Height);
                        TX2:=Ceil(Rect.Right/Image.Bitmap.Width);
                        TY2:=Ceil(Rect.Bottom/Image.Bitmap.Height);
                        for C:=TY to TY2 do
                         for D:=TX to TX2 do
                          begin
                           SRect:=Image.Bitmap.BoundsRect;
                           DRect:=SRect;
                           OffsetRect(DRect,D*Image.Bitmap.Width,C*Image.Bitmap.Height);
                           if Rect.Left>DRect.Left then
                            begin
                             Inc(SRect.Left,Rect.Left-DRect.Left);
                             DRect.Left:=Rect.Left;
                            end;
                           if Rect.Top>DRect.Top then
                            begin
                             Inc(SRect.Top,Rect.Top-DRect.Top);
                             DRect.Top:=Rect.Top;
                            end;
                           if Rect.Right<DRect.Right then
                            begin
                             Dec(SRect.Right,DRect.Right-Rect.Right);
                             DRect.Right:=Rect.Right;
                            end;
                           if Rect.Bottom<DRect.Bottom then
                            begin
                             Dec(SRect.Bottom,DRect.Bottom-Rect.Bottom);
                             DRect.Bottom:=Rect.Bottom;
                            end;
                           OffsetRect(DRect,X-Rect.Left,Y-Rect.Top);
                           Image.Bitmap.DrawTo(Bitmap,DRect,SRect);
                          end;
                        OffsetRect(Rect,Image.OffsetX,Image.OffsetY);
                       end;
           irmStretch: begin
                        case Image.StretchFilter of
                         isfLinear:   TLinearResampler.Create(Image.Bitmap);
                         else         begin
                                       TKernelResampler.Create(Image.Bitmap);
                                       case Image.StretchFilter of
                                        isfSpline:   TKernelResampler(Image.Bitmap.Resampler).
                                                      Kernel:=TSplineKernel.Create;
                                        isfLanczos:  TKernelResampler(Image.Bitmap.Resampler).
                                                      Kernel:=TLanczosKernel.Create;
                                        isfMitchell: TKernelResampler(Image.Bitmap.Resampler).
                                                      Kernel:=TMitchellKernel.Create;
                                       end;
                                      end;
                        end;
                        Image.Bitmap.DrawTo(Bitmap,Types.Rect(X-Rect.Left,Y-Rect.Top,X-Rect.Left+Width,Y-Rect.Top+Height));
                       end;
          end;
         end;
      end;
    end else
   if Elements[A] is TSXSkinStyleStyleElement then
    begin
     Style:=TSXSkinStyleStyleElement(Elements[A]);
     if Style.DrawRect='' then
      R:=Types.Rect(0,0,Width,Height) else
       GetRectFromString(Style.DrawRect,R,VComparer.OnGetVariable);
     StdVariableComparer.GetSize(TmpWidth,TmpHeight);
     StdVariableComparer.SetSize(R.Right-R.Left,R.Bottom-R.Top);
     if not IsRectEmpty(R) then
      begin
       C:=SkinLibrary.Styles.GetGStyleIndex(Style.Style,TmpWidth,TmpHeight);
       if C>=0 then
        begin
         GStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[C]);
         IntersectRect(DRect,Rect,R);
         if not IsRectEmpty(DRect) then
          begin
           TmpRect:=DRect;
           OffsetRect(TmpRect,-R.Left,-R.Top);
           OffsetRgn(Rgn,-R.Left,-R.Top);
           if Style.Filter.AType<>iftNone then
            begin
             BB:=TBitmap32.Create;
             try
              BB.SetSize(DRect.Right-DRect.Left,DRect.Bottom-DRect.Top);
              GStyle.DrawToBitmap(Control,CElementID,BB,0,0,R.Right-R.Left,R.Bottom-R.Top,
                   TmpRect,Rgn,SkinLibrary,StdVariableComparer,ForceEvalParams);
              ApplyFilterToBitmap(BB,Style.Filter);
              Bitmap.Draw(X-Rect.Left+DRect.Left,Y-Rect.Top+DRect.Top,BB);
             finally
              BB.Free;
             end;
            end else
             begin
              GStyle.DrawToBitmap(Control,CElementID,Bitmap,X-Rect.Left+DRect.Left,
                   Y-Rect.Top+DRect.Top,R.Right-R.Left,R.Bottom-R.Top,TmpRect,
                   Rgn,SkinLibrary,StdVariableComparer,ForceEvalParams);
             end;
           OffsetRgn(Rgn,R.Left,R.Top);
          end;
        end;
      end;
     StdVariableComparer.SetSize(TmpWidth,TmpHeight);
    end else
   if Elements[A] is TSXSkinStyleFigureElement then
    begin
     Figure:=TSXSkinStyleFigureElement(Elements[A]);
     if Figure.FigureType=ssftSolidFill then
      begin
       DRect:=Types.Rect(X,Y,X+Rect.Right-Rect.Left,Y+Rect.Bottom-Rect.Top);
       Bitmap.FillRectTS(DRect,Figure.FillColor);
      end else
       begin
        Figure.EvalPrePaintParams(Control,CElementID,VComparer,ForceEvalParams);
        Figure.DrawToBitmap(Bitmap,X-Rect.Left,Y-Rect.Top);
       end;
    end else
   if Elements[A] is TSXSkinStyleTextElement then
    begin
     Text:=TSXSkinStyleTextElement(Elements[A]);
     Text.EvalPrePaintParams(Control,CElementID,VComparer,ForceEvalParams);
     Text.DrawToBitmap(Bitmap,X-Rect.Left,Y-Rect.Top);
    end else
   if Elements[A] is TSXSkinStyleBoxTileElement then
    begin
     BoxTile:=TSXSkinStyleBoxTileElement(Elements[A]);
     BoxTile.ValidateBitmaps(SkinLibrary);
     BoxTile.DrawToBitmap(Bitmap,X-Rect.Left,Y-Rect.Top,Width,Height,Rgn,VComparer.OnGetVariable);
    end;
  end;
 if UseBuffering then
  begin
   TBitmap.Draw(TmpX,TmpY,Bitmap);
   Bitmap.Free;
  end;
 if RestCmp then
  StdVariableComparer.SetSize(TmpWidth,TmpHeight);
end;

function TSXSkinGeneralStyle.NeedToPaintBackground:Boolean;
{var  B,C:Integer;
   Image:TSXSkinStyleImageElement;
   Style:TSXSkinStyleStyleElement;
  GStyle:TSXSkinGeneralStyle;
  Figure:TSXSkinStyleFigureElement;
 BoxTile:TSXSkinStyleBoxTileElement;}
begin
 Result:=Transparent;
{ if not Result then exit;
 for B:=0 to Elements.Count-1 do
  begin
   if Elements[B] is TSXSkinStyleImageElement then
    begin
     Image:=TSXSkinStyleImageElement(Elements[B]);
     Result:=Image.Transparent;
     if not Result then exit;
    end else
   if Elements[B] is TSXSkinStyleBoxTileElement then
    begin
     BoxTile:=TSXSkinStyleBoxTileElement(Elements[B]);
     Result:=BoxTile.Transparent;
     if not Result then exit;
    end else
   if Elements[B] is TSXSkinStyleFigureElement then
    begin
     Figure:=TSXSkinStyleFigureElement(Elements[B]);
     Result:=Figure.FigureType<>ssftSolidFill;
     if not Result then exit;
    end;
  end;}
end;

function TSXSkinGeneralStyle.IsTransparent(Control:TControl;CElementID:Integer;
           X,Y,Width,Height:Integer;SkinLibrary:TSXSkinLibrary;Limit:Integer=10;
           VComparer:TSXVariableComparer=nil;ForceEvalParams:Boolean=False):Boolean;
var    B,C:Integer;
     Image:TSXSkinStyleImageElement;
     Style:TSXSkinStyleStyleElement;
    GStyle:TSXSkinGeneralStyle;
   BoxTile:TSXSkinStyleBoxTileElement;
    Figure:TSXSkinStyleFigureElement;
     XX,YY:Integer;
         R:TRect;
   RestCmp:Boolean;
  TmpWidth:Integer;
 TmpHeight:Integer;
begin
 RestCmp:=False;
 if VComparer=nil then
  begin
   VComparer:=StdVariableComparer;
   StdVariableComparer.GetSize(TmpWidth,TmpHeight);
   StdVariableComparer.SetSize(Width,Height);
   RestCmp:=True;
  end;
 Result:=True;
 for B:=0 to Elements.Count-1 do
  begin
   if Elements[B] is TSXSkinStyleImageElement then
    begin
     Image:=TSXSkinStyleImageElement(Elements[B]);
     if (Image.Bitmap<>nil) and (Image.Bitmap.Width>0) and (Image.Bitmap.Height>0) then
      begin
       if Image.DrawRect<>'' then
        begin
         GetRectFromString(Image.DrawRect,R,VComparer.OnGetVariable);
         if PtInRect(R,Point(X,Y)) then
          begin
           case Image.ResizeMode of
            irmTile: begin
                      XX:=(X-Image.OffsetX-R.Left) mod Image.Bitmap.Width;
                      if XX<0 then XX:=XX+Image.Bitmap.Width;
                      YY:=(Y-Image.OffsetY-R.Top) mod Image.Bitmap.Height;
                      if YY<0 then YY:=YY+Image.Bitmap.Height;
                      if AlphaComponent(Image.Bitmap.PixelS[XX,YY])>=Limit then
                       Result:=False;
                     end;
            else     begin
                      if (R.Right>R.Left) and (R.Bottom>R.Top) then
                       begin
                        XX:=round((X-R.Left)/(R.Right-R.Left)*Image.Bitmap.Width);
                        YY:=round((Y-R.Top)/(R.Bottom-R.Top)*Image.Bitmap.Height);
                        if AlphaComponent(Image.Bitmap.PixelS[XX,YY])>=Limit then
                         Result:=False;
                       end;
                     end;
           end;
          end;
        end else
         begin
          case Image.ResizeMode of
           irmNone:    begin
                        if PtInRect(Image.Bitmap.BoundsRect,Point(X-Image.OffsetX,Y-Image.OffsetY)) and
                           (AlphaComponent(Image.Bitmap.PixelS[X-Image.OffsetX,Y-Image.OffsetY])>=Limit) then
                         Result:=False;
                       end;
           irmTile:    begin
                        XX:=(X-Image.OffsetX) mod Image.Bitmap.Width;
                        if XX<0 then XX:=XX+Image.Bitmap.Width;
                        YY:=(Y-Image.OffsetY) mod Image.Bitmap.Height;
                        if YY<0 then YY:=YY+Image.Bitmap.Height;
                        if AlphaComponent(Image.Bitmap.PixelS[XX,YY])>=Limit then
                         Result:=False;
                       end;
           irmStretch: begin
                        if (Width>0) and (Height>0) then
                         begin
                          XX:=round(X/Width*Image.Bitmap.Width);
                          YY:=round(Y/Height*Image.Bitmap.Height);
                          if AlphaComponent(Image.Bitmap.PixelS[XX,YY])>=Limit then
                           Result:=False;
                         end;
                       end;
          end;
         end;
      end;
    end else
   if Elements[B] is TSXSkinStyleStyleElement then
    begin
     Style:=TSXSkinStyleStyleElement(Elements[B]);
     C:=SkinLibrary.Styles.GetGStyleIndex(Style.Style,Width,Height);
     if C>=0 then
      begin
       GStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[C]);
       if Style.DrawRect='' then
        R:=Types.Rect(0,0,Width,Height) else
         GetRectFromString(Style.DrawRect,R,VComparer.OnGetVariable);
       Result:=GStyle.IsTransparent(Control,CElementID,X-R.Left,Y-R.Top,R.Right-R.Left,
            R.Bottom-R.Top,SkinLibrary,Limit);
      end;
    end else
   if Elements[B] is TSXSkinStyleBoxTileElement then
    begin
     BoxTile:=TSXSkinStyleBoxTileElement(Elements[B]);
     if AlphaComponent(BoxTile.GetPixelAt(X,Y,Width,Height,VComparer.OnGetVariable))>=Limit then
      Result:=False;
    end else
   if Elements[B] is TSXSkinStyleFigureElement then
    begin
     Figure:=TSXSkinStyleFigureElement(Elements[B]);
     Figure.EvalPrePaintParams(Control,CElementID,VComparer,ForceEvalParams);
     C:=Figure.ControlsData.GetIndexByControl(CElementID);
     if C>=0 then
      begin
       case Figure.FigureType of
        ssftEllipse:        begin
                             Result:=not PtInEllipse(X,Y,Figure.ControlsData[C].Rect.Left,
                               Figure.ControlsData[C].Rect.Top,Figure.ControlsData[C].Rect.Right,
                               Figure.ControlsData[C].Rect.Bottom);
                            end;
        ssftRectangle:      begin
                             Result:=not PtInRect(Figure.ControlsData[C].Rect,Point(X,Y));
                            end;
        ssftRoundRectangle: begin
                             Result:=not PtInRoundRect(X,Y,Figure.ControlsData[C].Rect.Left,
                               Figure.ControlsData[C].Rect.Top,Figure.ControlsData[C].Rect.Right,
                               Figure.ControlsData[C].Rect.Bottom,Figure.Roundness);
                            end;
       end;
      end;
    end;
   if not Result then break; 
  end;
 if RestCmp then
  StdVariableComparer.SetSize(TmpWidth,TmpHeight);
end;

function TSXSkinGeneralStyle.CapturesMouseAt(Control:TControl;CElementID:Integer;
           X,Y,Width,Height:Integer;SkinLibrary:TSXSkinLibrary;
           VComparer:TSXVariableComparer=nil):Boolean;
var Rgn:HRGN;
begin
 Result:=(X>=0) and (X<Width) and (Y>=0) and (Y<Height);
 if Result then
  begin
   if MouseCaptureByTransparency then
    Result:=not IsTransparent(Control,CElementID,X,Y,Width,Height,SkinLibrary,
                              MouseCaptureTransparencyLimit,VComparer) else
   if MouseCaptureRegion<>'' then
    begin
     Rgn:=EvaluateRegion(MouseCaptureRegion,SkinFilePath,ZipFilePath,Width,Height,
                         SkinLibrary,False,VComparer.OnGetVariable);
     Result:=PtInRegion(Rgn,X,Y);
     DeleteObject(Rgn);
    end;
  end;
end;

procedure TSXSkinGeneralStyle.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 Save8Flags(S,SetTransparent,SetMouseCaptureByTransparency,SetMouseCaptureTransparencyLimit,
              SetMouseCaptureRegion,SetUseBuffering,False,False,False);
 Save8Flags(S,Transparent,MouseCaptureByTransparency,UseBuffering,False,
              False,False,False,False);
 if SetMouseCaptureTransparencyLimit then
  SavePackedInteger(S,MouseCaptureTransparencyLimit);
 if SetMouseCaptureRegion then
  SaveString(S,MouseCaptureRegion);
 Elements.SaveToStream(S,RootPath);
end;

procedure TSXSkinGeneralStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;
begin
 inherited;
 Self.ZipFilePath:=ZipFilePath;
 SkinFilePath:=RootPath;
 Load8Flags(S,SetTransparent,SetMouseCaptureByTransparency,SetMouseCaptureTransparencyLimit,
              SetMouseCaptureRegion,SetUseBuffering,B,B,B);
 Load8MaskedFlags(S,Transparent,MouseCaptureByTransparency,UseBuffering,B,
                    B,B,B,B,
                    SetTransparent,SetMouseCaptureByTransparency,SetUseBuffering,False,
                    False,False,False,False);
 if SetMouseCaptureTransparencyLimit then
  LoadPackedInteger(S,MouseCaptureTransparencyLimit);
 if SetMouseCaptureRegion then
  LoadString(S,MouseCaptureRegion);
 Elements.LoadFromStream(S,Version,RootPath,ZipFilePath,False);
end;

constructor TSXSkinGeneralStyle.Create;
begin
 inherited Create;
 Elements:=TSXSkinStyleElementList.Create;
 Transparent:=True;
 MouseCaptureTransparencyLimit:=250;
end;

destructor TSXSkinGeneralStyle.Destroy;
begin
 Elements.Free;
 inherited Destroy;
end;

{ TSXSkinSelectiveStyle }

procedure TSXSkinSelectiveStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinSelectiveStyle then
  begin
   Conditions.Assign(TSXSkinSelectiveStyle(Style).Conditions);
   Styles.Assign(TSXSkinSelectiveStyle(Style).Styles);
   DefaultStyle:=TSXSkinSelectiveStyle(Style).DefaultStyle;
  end; 
end;

procedure TSXSkinSelectiveStyle.SetParameter(const Name,Value:String);
var A:Integer;
begin
 if Name='Default' then
  DefaultStyle:=Value else
 if Copy(Name,1,5)='Style' then
  begin
   A:=Pos(',',Value);
   if A>0 then
    begin
     Conditions.Add(Copy(Value,1,A-1));
     Styles.Add(Copy(Value,A+1,MaxInt));
    end;
  end;
end;

function TSXSkinSelectiveStyle.GetAppropriateStyle(AOnGetVariable:TSXOnGetVariable):String;
var Error:Boolean;
      Res:Single;
        A:Integer;
begin
 for A:=0 to Conditions.Count-1 do
  begin
   Error:=False;
   Res:=SXEvalMathString(Conditions[A],AOnGetVariable,Error);
   if not Error and (Res<>0) then
    begin
     Result:=Styles[A];
     exit;
    end;
  end;
 Result:=DefaultStyle; 
end;

procedure TSXSkinSelectiveStyle.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 SaveListToStream(S,Conditions);
 SaveListToStream(S,Styles);
 SaveString(S,DefaultStyle);
end;

procedure TSXSkinSelectiveStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
begin
 inherited;
 LoadListFromStream(S,Conditions);
 LoadListFromStream(S,Styles);
 LoadString(S,DefaultStyle);
end;

constructor TSXSkinSelectiveStyle.Create;
begin
 inherited Create;
 Conditions:=TStringList.Create;
 Styles:=TStringList.Create;
end;

destructor TSXSkinSelectiveStyle.Destroy;
begin
 Conditions.Free;
 Styles.Free;
 inherited Destroy;
end;

{ TSXSkinStyleList }

function TSXSkinStyleList.Get(Index:Integer):TSXSkinStyle;
begin
 Result:=TSXSkinStyle(FItem[Index]);
end;

procedure TSXSkinStyleList.Put(Index:Integer;Item:TSXSkinStyle);
begin
 FItem[Index]:=Item;
end;

function TSXSkinStyleList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

function TSXSkinStyleList.GetIndexByName(const Name:String):Integer;
label l1,l2,l3;
var A,L,H:Integer;
        B:Boolean;
begin
 B:=False;
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   SXLStrCmp(Item[A].Name,Name);
   asm
    jz l2;
    jnb l1;
   end;
   L:=A+1;
   goto l3;
l2: L:=A; B:=True;
l1: H:=A-1; l3:
  end;
 if B then Result:=L else Result:=-1;
end;

function TSXSkinStyleList.GetIndexToInsert(const Name:String):Integer;
label l1,l2,l3;
var A,L,H:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   SXLStrCmp(Item[A].Name,Name);
   asm
    jz l2;
    jnb l1;
   end;
   L:=A+1;
   goto l3;
l2: L:=A;
l1: H:=A-1; l3:
  end;
 Result:=L;
end;

function TSXSkinStyleList.GetGStyleIndexOnGetVar(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if VarName='W' then
  begin
   Result:=TmpWidth;
   exit;
  end;
 if VarName='H' then
  begin
   Result:=TmpHeight;
   exit;
  end;
 Error:=True;
end;

function TSXSkinStyleList.GetGStyleIndex(const Name:String;Width,Height:Integer):Integer;
var S:String;
begin
 Result:=GetIndexByName(Name);
 if Result<0 then exit;
 if Item[Result] is TSXSkinGeneralStyle then exit;
 if Item[Result] is TSXSkinSelectiveStyle then
  begin
   if (Width=0) and (Height=0) then
    S:=TSXSkinSelectiveStyle(Item[Result]).DefaultStyle else
     begin
      TmpWidth:=Width;
      TmpHeight:=Height;
      S:=TSXSkinSelectiveStyle(Item[Result]).GetAppropriateStyle(GetGStyleIndexOnGetVar);
     end; 
   Result:=GetIndexByName(S);
   if (Result<0) or (Item[Result] is TSXSkinGeneralStyle) then exit;
  end;
 Result:=-1;
end;

procedure TSXSkinStyleList.Add(SXSkinStyle:TSXSkinStyle);
var A:Integer;
begin
 A:=GetIndexToInsert(SXSkinStyle.Name);
 FItem.Insert(A,SXSkinStyle);
end;

procedure TSXSkinStyleList.AddUnique(SXSkinStyle:TSXSkinStyle);
var A:Integer;
begin
 A:=GetIndexByName(SXSkinStyle.Name);
 if A<0 then
  begin
   A:=GetIndexToInsert(SXSkinStyle.Name);
   FItem.Insert(A,SXSkinStyle);
  end else
   begin
    Item[A].Free;
    FItem[A]:=SXSkinStyle;
   end;
end;

procedure TSXSkinStyleList.Delete(Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXSkinStyleList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

procedure TSXSkinStyleList.ClearLinks;
begin
 FItem.Clear;
end;

procedure TSXSkinStyleList.SaveToStream(S:TStream;const RootPath:String);
var A:Integer;
begin
 SavePackedInteger(S,Count);
 for A:=0 to Count-1 do
  Item[A].SaveToStream(S,RootPath);
end;

procedure TSXSkinStyleList.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var A,C:Integer;
      T:TSXSkinStyle;
begin
 Clear;
 LoadPackedInteger(S,C);
 for A:=0 to C-1 do
  begin
   T:=CreateSkinStyleFromStream(S,Version,RootPath,ZipFilePath,DestList1,DestList2);
   Add(T);
  end;
end;

constructor TSXSkinStyleList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXSkinStyleList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

{ TSXSkinStyleElementList }

function TSXSkinStyleElementList.Get(Index:Integer):TSXSkinStyleElement;
begin
 Result:=TSXSkinStyleElement(FItem[Index]);
end;

procedure TSXSkinStyleElementList.Put(Index:Integer;Item:TSXSkinStyleElement);
begin
 FItem[Index]:=Item;
end;

function TSXSkinStyleElementList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

function TSXSkinStyleElementList.GetIndexByName(const Name:String):Integer;
begin
 Result:=Count-1;
 while (Result>0) and (Item[Result].Name<>Name) do Dec(Result);
end;

procedure TSXSkinStyleElementList.Assign(ElementList:TSXSkinStyleElementList);
var A:Integer;
begin
 Clear;
 for A:=0 to ElementList.Count-1 do
  Add(ElementList[A].GetCopy);
end;

procedure TSXSkinStyleElementList.Add(SXSkinStyleElement:TSXSkinStyleElement);
begin
 FItem.Add(SXSkinStyleElement);
end;

procedure TSXSkinStyleElementList.Delete(Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXSkinStyleElementList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

procedure TSXSkinStyleElementList.SaveToStream(S:TStream;const RootPath:String);
var A:Integer;
begin
 SavePackedInteger(S,Count);
 for A:=0 to Count-1 do
  Item[A].SaveToStream(S,RootPath);
end;

procedure TSXSkinStyleElementList.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DoClear:Boolean=True);
var A,C:Integer;
      T:TSXSkinStyleElement;
begin
 if DoClear then Clear;
 LoadPackedInteger(S,C);
 for A:=0 to C-1 do
  begin
   T:=CreateSkinStyleElementFromStream(S,Version,RootPath,ZipFilePath);
   Add(T);
  end;
end;

constructor TSXSkinStyleElementList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXSkinStyleElementList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

{ TSXSkinCheckBoxStyle }

procedure TSXSkinCheckBoxStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinCheckBoxStyle then
  begin
   NUUState:=TSXSkinCheckBoxStyle(Style).NUUState;
   NFUState:=TSXSkinCheckBoxStyle(Style).NFUState;
   HUUState:=TSXSkinCheckBoxStyle(Style).HUUState;
   HFUState:=TSXSkinCheckBoxStyle(Style).HFUState;
   DFUState:=TSXSkinCheckBoxStyle(Style).DFUState;
   RUUState:=TSXSkinCheckBoxStyle(Style).RUUState;
   NUCState:=TSXSkinCheckBoxStyle(Style).NUCState;
   NFCState:=TSXSkinCheckBoxStyle(Style).NFCState;
   HUCState:=TSXSkinCheckBoxStyle(Style).HUCState;
   HFCState:=TSXSkinCheckBoxStyle(Style).HFCState;
   DFCState:=TSXSkinCheckBoxStyle(Style).DFCState;
   RUCState:=TSXSkinCheckBoxStyle(Style).RUCState;
   NUGState:=TSXSkinCheckBoxStyle(Style).NUGState;
   NFGState:=TSXSkinCheckBoxStyle(Style).NFGState;
   HUGState:=TSXSkinCheckBoxStyle(Style).HUGState;
   HFGState:=TSXSkinCheckBoxStyle(Style).HFGState;
   DFGState:=TSXSkinCheckBoxStyle(Style).DFGState;
   RUGState:=TSXSkinCheckBoxStyle(Style).RUGState;
   //
   HInCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).HInCheckBoxEffect;
   HOutCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).HOutCheckBoxEffect;
   CheckCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).CheckCheckBoxEffect;
   UncheckCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).UncheckCheckBoxEffect;
   DownCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).DownCheckBoxEffect;
   UpCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).UpCheckBoxEffect;
   EnableCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).EnableCheckBoxEffect;
   DisableCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).DisableCheckBoxEffect;
   FocusCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).FocusCheckBoxEffect;
   UnfocusCheckBoxEffect:=TSXSkinCheckBoxStyle(Style).UnfocusCheckBoxEffect;
   //
   HInGlyphEffect:=TSXSkinButtonStyle(Style).HInGlyphEffect;
   HOutGlyphEffect:=TSXSkinButtonStyle(Style).HOutGlyphEffect;
   CheckGlyphEffect:=TSXSkinButtonStyle(Style).CheckGlyphEffect;
   UncheckGlyphEffect:=TSXSkinButtonStyle(Style).UncheckGlyphEffect;
   DownGlyphEffect:=TSXSkinButtonStyle(Style).DownGlyphEffect;
   UpGlyphEffect:=TSXSkinButtonStyle(Style).UpGlyphEffect;
   EnableGlyphEffect:=TSXSkinButtonStyle(Style).EnableGlyphEffect;
   DisableGlyphEffect:=TSXSkinButtonStyle(Style).DisableGlyphEffect;
   FocusGlyphEffect:=TSXSkinButtonStyle(Style).FocusGlyphEffect;
   UnfocusGlyphEffect:=TSXSkinButtonStyle(Style).UnfocusGlyphEffect;
  end;
end;

procedure TSXSkinCheckBoxStyle.SetParameter(const Name,Value:String);
var CBParam:PSXSkinCheckBoxStateParam;
          S:String;
begin
 CBParam:=nil;
 S:=Copy(Name,1,4);
 if S='NUU_' then CBParam:=@NUUState else
  if S='NFU_' then CBParam:=@NFUState else
  if S='HUU_' then CBParam:=@HUUState else
  if S='HFU_' then CBParam:=@HFUState else
  if S='DFU_' then CBParam:=@DFUState else
  if S='RUU_' then CBParam:=@RUUState else
  if S='NUC_' then CBParam:=@NUCState else
  if S='NFC_' then CBParam:=@NFCState else
  if S='HUC_' then CBParam:=@HUCState else
  if S='HFC_' then CBParam:=@HFCState else
  if S='DFC_' then CBParam:=@DFCState else
  if S='RUC_' then CBParam:=@RUCState else
  if S='NUG_' then CBParam:=@NUGState else
  if S='NFG_' then CBParam:=@NFGState else
  if S='HUG_' then CBParam:=@HUGState else
  if S='HFG_' then CBParam:=@HFGState else
  if S='DFG_' then CBParam:=@DFGState else
  if S='RUG_' then CBParam:=@RUGState;
 if CBParam=nil then
  begin
   if not TryToSetTransformEffectDataParameter(Name,Value,HInCheckBoxEffect,'HighlightInCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutCheckBoxEffect,'HighlightOutCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckCheckBoxEffect,'CheckCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckCheckBoxEffect,'UncheckCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownCheckBoxEffect,'DownCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpCheckBoxEffect,'UpCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableCheckBoxEffect,'EnableCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableCheckBoxEffect,'DisableCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusCheckBoxEffect,'FocusCheckBox') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusCheckBoxEffect,'UnfocusCheckBox') then
    //
    if not TryToSetTransformEffectDataParameter(Name,Value,HInGlyphEffect,'HighlightInGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutGlyphEffect,'HighlightOutGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckGlyphEffect,'CheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckGlyphEffect,'UncheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownGlyphEffect,'DownGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpGlyphEffect,'UpGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableGlyphEffect,'EnableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableGlyphEffect,'DisableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusGlyphEffect,'FocusGlyph') then
     TryToSetTransformEffectDataParameter(Name,Value,UnfocusGlyphEffect,'UnfocusGlyph');
  end else
   begin
    S:=Copy(Name,5,MaxInt);
    if S='Style' then
     begin
      CBParam.Style:=Value;
      CBParam.SetStyle:=True;
     end else
     if S='OverStyle' then
      begin
       CBParam.OverStyle:=Value;
       CBParam.SetOverStyle:=True;
      end else
     if S='Glyph' then
      begin
       CBParam.GlyphStyle:=Value;
       CBParam.SetGlyphStyle:=True;
      end else
     if S='GlyphWidth' then
      begin
       CBParam.GlyphWidth:=StrToIntDef(Value,0);
       CBParam.SetGlyphWidth:=True;
      end else
     if S='GlyphHeight' then
      begin
       CBParam.GlyphHeight:=StrToIntDef(Value,0);
       CBParam.SetGlyphHeight:=True;
      end else
     if S='CaptionLeftOffset' then
      begin
       CBParam.SetCaptionLeftOffset:=True;
       CBParam.CaptionLeftOffset:=StrToIntDef(Value,0);
      end else
     if S='CaptionTopOffset' then
      begin
       CBParam.SetCaptionTopOffset:=True;
       CBParam.CaptionTopOffset:=StrToIntDef(Value,0);
      end else
     if S='CaptionRightOffset' then
      begin
       CBParam.SetCaptionRightOffset:=True;
       CBParam.CaptionRightOffset:=StrToIntDef(Value,0);
      end else
     if S='CaptionBottomOffset' then
      begin
       CBParam.SetCaptionBottomOffset:=True;
       CBParam.CaptionBottomOffset:=StrToIntDef(Value,0);
      end else
     if S='TextLeftOffset' then
      begin
       CBParam.SetTextLeftOffset:=True;
       CBParam.TextLeftOffset:=StrToIntDef(Value,0);
      end else
     if S='TextTopOffset' then
      begin
       CBParam.SetTextTopOffset:=True;
       CBParam.TextTopOffset:=StrToIntDef(Value,0);
      end else
     if S='TextRightOffset' then
      begin
       CBParam.SetTextRightOffset:=True;
       CBParam.TextRightOffset:=StrToIntDef(Value,0);
      end else
     if S='TextBottomOffset' then
      begin
       CBParam.SetTextBottomOffset:=True;
       CBParam.TextBottomOffset:=StrToIntDef(Value,0);
      end else
       TryToSetFontDataParameter(S,Value,CBParam.FD);
   end;
end;

procedure TSXSkinCheckBoxStyle.GetCurrentCBState(var CBState:TSXSkinCheckBoxStateParam;
           State:TCheckBoxState;Down,MouseOver,Focused,Enabled:Boolean);

 procedure SetCBStateFrom(T:TSXSkinCheckBoxStateParam);
 begin
  if CBState.Style='' then CBState.Style:=T.Style;
  if CBState.OverStyle='' then CBState.OverStyle:=T.OverStyle;
  if not CBState.SetCaptionLeftOffset and T.SetCaptionLeftOffset then
   begin
    CBState.SetCaptionLeftOffset:=True;
    CBState.CaptionLeftOffset:=T.CaptionLeftOffset;
   end;
  if not CBState.SetCaptionTopOffset and T.SetCaptionTopOffset then
   begin
    CBState.SetCaptionTopOffset:=True;
    CBState.CaptionTopOffset:=T.CaptionTopOffset;
   end;
  if not CBState.SetCaptionRightOffset and T.SetCaptionRightOffset then
   begin
    CBState.SetCaptionRightOffset:=True;
    CBState.CaptionRightOffset:=T.CaptionRightOffset;
   end;
  if not CBState.SetCaptionBottomOffset and T.SetCaptionBottomOffset then
   begin
    CBState.SetCaptionBottomOffset:=True;
    CBState.CaptionBottomOffset:=T.CaptionBottomOffset;
   end;
  if not CBState.SetTextLeftOffset and T.SetTextLeftOffset then
   begin
    CBState.SetTextLeftOffset:=True;
    CBState.TextLeftOffset:=T.TextLeftOffset;
   end;
  if not CBState.SetTextTopOffset and T.SetTextTopOffset then
   begin
    CBState.SetTextTopOffset:=True;
    CBState.TextTopOffset:=T.TextTopOffset;
   end;
  if not CBState.SetTextRightOffset and T.SetTextRightOffset then
   begin
    CBState.SetTextRightOffset:=True;
    CBState.TextRightOffset:=T.TextRightOffset;
   end;
  if not CBState.SetTextBottomOffset and T.SetTextBottomOffset then
   begin
    CBState.SetTextBottomOffset:=True;
    CBState.TextBottomOffset:=T.TextBottomOffset;
   end;
  if CBState.GlyphStyle='' then CBState.GlyphStyle:=T.GlyphStyle;
  if CBState.GlyphWidth=0 then CBState.GlyphWidth:=T.GlyphWidth;
  if CBState.GlyphHeight=0 then CBState.GlyphHeight:=T.GlyphHeight;
  AddFontData(CBState.FD,T.FD);
 end;

begin
 Finalize(CBState);
 FillChar(CBState,sizeof(CBState),0);
 case State of
  cbChecked:   begin
                if not Enabled then
                 begin
                  SetCBStateFrom(RUCState);
                  SetCBStateFrom(NUCState);
                  SetCBStateFrom(RUUState);
                  SetCBStateFrom(NUUState);
                 end else
                if Down then
                 begin
                  SetCBStateFrom(DFCState);
                  SetCBStateFrom(DFUState);
                  SetCBStateFrom(NFCState);
                  SetCBStateFrom(NFUState);
                  SetCBStateFrom(NUCState);
                  SetCBStateFrom(NUUState);
                 end else
                if MouseOver then
                 begin
                  if Focused then
                   begin
                    SetCBStateFrom(HFCState);
                    SetCBStateFrom(HUCState);
                    SetCBStateFrom(HFUState);
                    SetCBStateFrom(HUUState);
                    SetCBStateFrom(NFCState);
                    SetCBStateFrom(NUCState);
                    SetCBStateFrom(NFUState);
                    SetCBStateFrom(NUUState);
                   end else
                    begin
                     SetCBStateFrom(HUCState);
                     SetCBStateFrom(NUCState);
                     SetCBStateFrom(HUUState);
                     SetCBStateFrom(NUUState);
                    end;
                 end else
                  begin
                   if Focused then
                    begin
                     SetCBStateFrom(NFCState);
                     SetCBStateFrom(NUCState);
                     SetCBStateFrom(NFUState);
                     SetCBStateFrom(NUUState);
                    end else
                     begin
                      SetCBStateFrom(NUCState);
                      SetCBStateFrom(NUUState);
                     end;
                  end;
               end;
  cbGrayed:    begin
                if not Enabled then
                 begin
                  SetCBStateFrom(RUGState);
                  SetCBStateFrom(NUGState);
                  SetCBStateFrom(RUCState);
                  SetCBStateFrom(NUCState);
                  SetCBStateFrom(RUUState);
                  SetCBStateFrom(NUUState);
                 end else
                if Down then
                 begin
                  SetCBStateFrom(DFGState);
                  SetCBStateFrom(DFCState);
                  SetCBStateFrom(DFUState);
                  SetCBStateFrom(NFGState);
                  SetCBStateFrom(NFCState);
                  SetCBStateFrom(NFUState);
                  SetCBStateFrom(NUGState);
                  SetCBStateFrom(NUCState);
                  SetCBStateFrom(NUUState);
                 end else
                if MouseOver then
                 begin
                  if Focused then
                   begin
                    SetCBStateFrom(HFGState);
                    SetCBStateFrom(HUGState);
                    SetCBStateFrom(HFCState);
                    SetCBStateFrom(HUCState);
                    SetCBStateFrom(HFUState);
                    SetCBStateFrom(HUUState);
                    SetCBStateFrom(NFGState);
                    SetCBStateFrom(NUGState);
                    SetCBStateFrom(NFCState);
                    SetCBStateFrom(NUCState);
                    SetCBStateFrom(NFUState);
                    SetCBStateFrom(NUUState);
                   end else
                    begin
                     SetCBStateFrom(HUGState);
                     SetCBStateFrom(NUGState);
                     SetCBStateFrom(HUCState);
                     SetCBStateFrom(NUCState);
                     SetCBStateFrom(HUUState);
                     SetCBStateFrom(NUUState);
                    end;
                 end else
                  begin
                   if Focused then
                    begin
                     SetCBStateFrom(NFGState);
                     SetCBStateFrom(NUGState);
                     SetCBStateFrom(NFCState);
                     SetCBStateFrom(NUCState);
                     SetCBStateFrom(NFUState);
                     SetCBStateFrom(NUUState);
                    end else
                     begin
                      SetCBStateFrom(NUGState);
                      SetCBStateFrom(NUCState);
                      SetCBStateFrom(NUUState);
                     end;
                  end;
               end;
  cbUnchecked: begin
                if not Enabled then
                 begin
                  SetCBStateFrom(RUUState);
                  SetCBStateFrom(NUUState);
                 end else
                if Down then
                 begin
                  SetCBStateFrom(DFUState);
                  SetCBStateFrom(NFUState);
                  SetCBStateFrom(NUUState);
                 end else
                if MouseOver then
                 begin
                  if Focused then
                   begin
                    SetCBStateFrom(HFUState);
                    SetCBStateFrom(HUUState);
                    SetCBStateFrom(NFUState);
                    SetCBStateFrom(NUUState);
                   end else
                    begin
                     SetCBStateFrom(HUUState);
                     SetCBStateFrom(NUUState);
                    end;
                 end else
                  begin
                   if Focused then
                    begin
                     SetCBStateFrom(NFUState);
                     SetCBStateFrom(NUUState);
                    end else
                     begin
                      SetCBStateFrom(NUUState);
                     end;
                  end;
               end;
 end;
end;

procedure TSXSkinCheckBoxStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinCheckBoxStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,SetOverStyle,SetGlyphStyle,SetGlyphWidth,
                 SetGlyphHeight,False,False,False);
    if SetStyle then
     SaveString(S,Style);
    if SetOverStyle then
     SaveString(S,OverStyle);
    if SetGlyphStyle then
     SaveString(S,GlyphStyle);
    if SetGlyphWidth then
     SavePackedInteger(S,GlyphWidth);
    if SetGlyphHeight then
     SavePackedInteger(S,GlyphHeight);
    SaveFontData(S,FD);
    //
    Save8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     SavePackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     SavePackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     SavePackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     SavePackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     SavePackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     SavePackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     SavePackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     SavePackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 SaveState(NUUState);
 SaveState(NFUState);
 SaveState(HUUState);
 SaveState(HFUState);
 SaveState(DFUState);
 SaveState(RUUState);
 SaveState(NUCState);
 SaveState(NFCState);
 SaveState(HUCState);
 SaveState(HFCState);
 SaveState(DFCState);
 SaveState(RUCState);
 SaveState(NUGState);
 SaveState(NFGState);
 SaveState(HUGState);
 SaveState(HFGState);
 SaveState(DFGState);
 SaveState(RUGState);
 //
 SaveTransformEffectData(S,HInCheckBoxEffect);
 SaveTransformEffectData(S,HOutCheckBoxEffect);
 SaveTransformEffectData(S,CheckCheckBoxEffect);
 SaveTransformEffectData(S,UncheckCheckBoxEffect);
 SaveTransformEffectData(S,DownCheckBoxEffect);
 SaveTransformEffectData(S,UpCheckBoxEffect);
 SaveTransformEffectData(S,EnableCheckBoxEffect);
 SaveTransformEffectData(S,DisableCheckBoxEffect);
 SaveTransformEffectData(S,FocusCheckBoxEffect);
 SaveTransformEffectData(S,UnfocusCheckBoxEffect);
 //
 SaveTransformEffectData(S,HInGlyphEffect);
 SaveTransformEffectData(S,HOutGlyphEffect);
 SaveTransformEffectData(S,CheckGlyphEffect);
 SaveTransformEffectData(S,UncheckGlyphEffect);
 SaveTransformEffectData(S,DownGlyphEffect);
 SaveTransformEffectData(S,UpGlyphEffect);
 SaveTransformEffectData(S,EnableGlyphEffect);
 SaveTransformEffectData(S,DisableGlyphEffect);
 SaveTransformEffectData(S,FocusGlyphEffect);
 SaveTransformEffectData(S,UnfocusGlyphEffect);
end;

procedure TSXSkinCheckBoxStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);

 procedure LoadState(var State:TSXSkinCheckBoxStateParam);
 var B:Boolean;
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,SetOverStyle,SetGlyphStyle,SetGlyphWidth,
                 SetGlyphHeight,B,B,B);
    if SetStyle then
     LoadString(S,Style);
    if SetOverStyle then
     LoadString(S,OverStyle);
    if SetGlyphStyle then
     LoadString(S,GlyphStyle);
    if SetGlyphWidth then
     LoadPackedInteger(S,GlyphWidth);
    if SetGlyphHeight then
     LoadPackedInteger(S,GlyphHeight);
    LoadFontData(S,FD);
    //
    Load8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     LoadPackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     LoadPackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     LoadPackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     LoadPackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     LoadPackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     LoadPackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     LoadPackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     LoadPackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 LoadState(NUUState);
 LoadState(NFUState);
 LoadState(HUUState);
 LoadState(HFUState);
 LoadState(DFUState);
 LoadState(RUUState);
 LoadState(NUCState);
 LoadState(NFCState);
 LoadState(HUCState);
 LoadState(HFCState);
 LoadState(DFCState);
 LoadState(RUCState);
 LoadState(NUGState);
 LoadState(NFGState);
 LoadState(HUGState);
 LoadState(HFGState);
 LoadState(DFGState);
 LoadState(RUGState);
 //
 LoadTransformEffectData(S,HInCheckBoxEffect);
 LoadTransformEffectData(S,HOutCheckBoxEffect);
 LoadTransformEffectData(S,CheckCheckBoxEffect);
 LoadTransformEffectData(S,UncheckCheckBoxEffect);
 LoadTransformEffectData(S,DownCheckBoxEffect);
 LoadTransformEffectData(S,UpCheckBoxEffect);
 LoadTransformEffectData(S,EnableCheckBoxEffect);
 LoadTransformEffectData(S,DisableCheckBoxEffect);
 LoadTransformEffectData(S,FocusCheckBoxEffect);
 LoadTransformEffectData(S,UnfocusCheckBoxEffect);
 //
 LoadTransformEffectData(S,HInGlyphEffect);
 LoadTransformEffectData(S,HOutGlyphEffect);
 LoadTransformEffectData(S,CheckGlyphEffect);
 LoadTransformEffectData(S,UncheckGlyphEffect);
 LoadTransformEffectData(S,DownGlyphEffect);
 LoadTransformEffectData(S,UpGlyphEffect);
 LoadTransformEffectData(S,EnableGlyphEffect);
 LoadTransformEffectData(S,DisableGlyphEffect);
 LoadTransformEffectData(S,FocusGlyphEffect);
 LoadTransformEffectData(S,UnfocusGlyphEffect);
end;

{ TSXSkinRadioButtonStyle }

procedure TSXSkinRadioButtonStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinRadioButtonStyle then
  begin
   NUUState:=TSXSkinRadioButtonStyle(Style).NUUState;
   NFUState:=TSXSkinRadioButtonStyle(Style).NFUState;
   HUUState:=TSXSkinRadioButtonStyle(Style).HUUState;
   HFUState:=TSXSkinRadioButtonStyle(Style).HFUState;
   DFUState:=TSXSkinRadioButtonStyle(Style).DFUState;
   RUUState:=TSXSkinRadioButtonStyle(Style).RUUState;
   NUCState:=TSXSkinRadioButtonStyle(Style).NUCState;
   NFCState:=TSXSkinRadioButtonStyle(Style).NFCState;
   HUCState:=TSXSkinRadioButtonStyle(Style).HUCState;
   HFCState:=TSXSkinRadioButtonStyle(Style).HFCState;
   DFCState:=TSXSkinRadioButtonStyle(Style).DFCState;
   RUCState:=TSXSkinRadioButtonStyle(Style).RUCState;
   //
   HInRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).HInRadioButtonEffect;
   HOutRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).HOutRadioButtonEffect;
   CheckRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).CheckRadioButtonEffect;
   UncheckRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).UncheckRadioButtonEffect;
   DownRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).DownRadioButtonEffect;
   UpRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).UpRadioButtonEffect;
   EnableRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).EnableRadioButtonEffect;
   DisableRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).DisableRadioButtonEffect;
   FocusRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).FocusRadioButtonEffect;
   UnfocusRadioButtonEffect:=TSXSkinRadioButtonStyle(Style).UnfocusRadioButtonEffect;
   //
   HInGlyphEffect:=TSXSkinButtonStyle(Style).HInGlyphEffect;
   HOutGlyphEffect:=TSXSkinButtonStyle(Style).HOutGlyphEffect;
   CheckGlyphEffect:=TSXSkinButtonStyle(Style).CheckGlyphEffect;
   UncheckGlyphEffect:=TSXSkinButtonStyle(Style).UncheckGlyphEffect;
   DownGlyphEffect:=TSXSkinButtonStyle(Style).DownGlyphEffect;
   UpGlyphEffect:=TSXSkinButtonStyle(Style).UpGlyphEffect;
   EnableGlyphEffect:=TSXSkinButtonStyle(Style).EnableGlyphEffect;
   DisableGlyphEffect:=TSXSkinButtonStyle(Style).DisableGlyphEffect;
   FocusGlyphEffect:=TSXSkinButtonStyle(Style).FocusGlyphEffect;
   UnfocusGlyphEffect:=TSXSkinButtonStyle(Style).UnfocusGlyphEffect;
  end;
end;

procedure TSXSkinRadioButtonStyle.SetParameter(const Name,Value:String);
var RBParam:PSXSkinRadioButtonStateParam;
          S:String;
begin
 RBParam:=nil;
 S:=Copy(Name,1,4);
 if S='NUU_' then RBParam:=@NUUState else
  if S='NFU_' then RBParam:=@NFUState else
  if S='HUU_' then RBParam:=@HUUState else
  if S='HFU_' then RBParam:=@HFUState else
  if S='DFU_' then RBParam:=@DFUState else
  if S='RUU_' then RBParam:=@RUUState else
  if S='NUC_' then RBParam:=@NUCState else
  if S='NFC_' then RBParam:=@NFCState else
  if S='HUC_' then RBParam:=@HUCState else
  if S='HFC_' then RBParam:=@HFCState else
  if S='DFC_' then RBParam:=@DFCState else
  if S='RUC_' then RBParam:=@RUCState;
 if RBParam=nil then
  begin
   if not TryToSetTransformEffectDataParameter(Name,Value,HInRadioButtonEffect,'HighlightInRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutRadioButtonEffect,'HighlightOutRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckRadioButtonEffect,'CheckRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckRadioButtonEffect,'UncheckRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownRadioButtonEffect,'DownRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpRadioButtonEffect,'UpRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableRadioButtonEffect,'EnableRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableRadioButtonEffect,'DisableRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusRadioButtonEffect,'FocusRadioButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusRadioButtonEffect,'UnfocusRadioButton') then
    //
    if not TryToSetTransformEffectDataParameter(Name,Value,HInGlyphEffect,'HighlightInGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutGlyphEffect,'HighlightOutGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckGlyphEffect,'CheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckGlyphEffect,'UncheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownGlyphEffect,'DownGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpGlyphEffect,'UpGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableGlyphEffect,'EnableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableGlyphEffect,'DisableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusGlyphEffect,'FocusGlyph') then
     TryToSetTransformEffectDataParameter(Name,Value,UnfocusGlyphEffect,'UnfocusGlyph');
  end else
   begin
    S:=Copy(Name,5,MaxInt);
    if S='Style' then
     begin
      RBParam.Style:=Value;
      RBParam.SetStyle:=True;
     end else
    if S='OverStyle' then
     begin
      RBParam.OverStyle:=Value;
      RBParam.SetOverStyle:=True;
     end else
    if S='Glyph' then
     begin
      RBParam.GlyphStyle:=Value;
      RBParam.SetGlyphStyle:=True;
     end else
    if S='GlyphWidth' then
     begin
      RBParam.GlyphWidth:=StrToIntDef(Value,0);
      RBParam.SetGlyphWidth:=True;
     end else
    if S='GlyphHeight' then
     begin
      RBParam.GlyphHeight:=StrToIntDef(Value,0);
      RBParam.SetGlyphHeight:=True;
     end else
    if S='CaptionLeftOffset' then
     begin
      RBParam.SetCaptionLeftOffset:=True;
      RBParam.CaptionLeftOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionTopOffset' then
     begin
      RBParam.SetCaptionTopOffset:=True;
      RBParam.CaptionTopOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionRightOffset' then
     begin
      RBParam.SetCaptionRightOffset:=True;
      RBParam.CaptionRightOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionBottomOffset' then
     begin
      RBParam.SetCaptionBottomOffset:=True;
      RBParam.CaptionBottomOffset:=StrToIntDef(Value,0);
     end else
    if S='TextLeftOffset' then
     begin
      RBParam.SetTextLeftOffset:=True;
      RBParam.TextLeftOffset:=StrToIntDef(Value,0);
     end else
    if S='TextTopOffset' then
     begin
      RBParam.SetTextTopOffset:=True;
      RBParam.TextTopOffset:=StrToIntDef(Value,0);
     end else
    if S='TextRightOffset' then
     begin
      RBParam.SetTextRightOffset:=True;
      RBParam.TextRightOffset:=StrToIntDef(Value,0);
     end else
    if S='TextBottomOffset' then
     begin
      RBParam.SetTextBottomOffset:=True;
      RBParam.TextBottomOffset:=StrToIntDef(Value,0);
     end else
      TryToSetFontDataParameter(S,Value,RBParam.FD);
   end;
end;

procedure TSXSkinRadioButtonStyle.GetCurrentRBState(var RBState:TSXSkinRadioButtonStateParam;
           Checked,Down,MouseOver,Focused,Enabled:Boolean);

 procedure SetRBStateFrom(T:TSXSkinRadioButtonStateParam);
 begin
  if RBState.Style='' then RBState.Style:=T.Style;
  if RBState.OverStyle='' then RBState.OverStyle:=T.OverStyle;
  if not RBState.SetCaptionLeftOffset and T.SetCaptionLeftOffset then
   begin
    RBState.SetCaptionLeftOffset:=True;
    RBState.CaptionLeftOffset:=T.CaptionLeftOffset;
   end;
  if not RBState.SetCaptionTopOffset and T.SetCaptionTopOffset then
   begin
    RBState.SetCaptionTopOffset:=True;
    RBState.CaptionTopOffset:=T.CaptionTopOffset;
   end;
  if not RBState.SetCaptionRightOffset and T.SetCaptionRightOffset then
   begin
    RBState.SetCaptionRightOffset:=True;
    RBState.CaptionRightOffset:=T.CaptionRightOffset;
   end;
  if not RBState.SetCaptionBottomOffset and T.SetCaptionBottomOffset then
   begin
    RBState.SetCaptionBottomOffset:=True;
    RBState.CaptionBottomOffset:=T.CaptionBottomOffset;
   end;
  if not RBState.SetTextLeftOffset and T.SetTextLeftOffset then
   begin
    RBState.SetTextLeftOffset:=True;
    RBState.TextLeftOffset:=T.TextLeftOffset;
   end;
  if not RBState.SetTextTopOffset and T.SetTextTopOffset then
   begin
    RBState.SetTextTopOffset:=True;
    RBState.TextTopOffset:=T.TextTopOffset;
   end;
  if not RBState.SetTextRightOffset and T.SetTextRightOffset then
   begin
    RBState.SetTextRightOffset:=True;
    RBState.TextRightOffset:=T.TextRightOffset;
   end;
  if not RBState.SetTextBottomOffset and T.SetTextBottomOffset then
   begin
    RBState.SetTextBottomOffset:=True;
    RBState.TextBottomOffset:=T.TextBottomOffset;
   end;
  if RBState.GlyphStyle='' then RBState.GlyphStyle:=T.GlyphStyle;
  if RBState.GlyphWidth=0 then RBState.GlyphWidth:=T.GlyphWidth;
  if RBState.GlyphHeight=0 then RBState.GlyphHeight:=T.GlyphHeight;
  AddFontData(RBState.FD,T.FD);
 end;

begin
 Finalize(RBState);
 FillChar(RBState,sizeof(RBState),0);
 if Checked then
  begin
   if not Enabled then
    begin
     SetRBStateFrom(RUCState);
     SetRBStateFrom(NUCState);
     SetRBStateFrom(RUUState);
     SetRBStateFrom(NUUState);
    end else
   if Down then
    begin
     SetRBStateFrom(DFCState);
     SetRBStateFrom(DFUState);
     SetRBStateFrom(NFCState);
     SetRBStateFrom(NFUState);
     SetRBStateFrom(NUCState);
     SetRBStateFrom(NUUState);
    end else
   if MouseOver then
    begin
     if Focused then
      begin
       SetRBStateFrom(HFCState);
       SetRBStateFrom(HUCState);
       SetRBStateFrom(HFUState);
       SetRBStateFrom(HUUState);
       SetRBStateFrom(NFCState);
       SetRBStateFrom(NUCState);
       SetRBStateFrom(NFUState);
       SetRBStateFrom(NUUState);
      end else
       begin
        SetRBStateFrom(HUCState);
        SetRBStateFrom(NUCState);
        SetRBStateFrom(HUUState);
        SetRBStateFrom(NUUState);
       end;
    end else
     begin
      if Focused then
       begin
        SetRBStateFrom(NFCState);
        SetRBStateFrom(NUCState);
        SetRBStateFrom(NFUState);
        SetRBStateFrom(NUUState);
       end else
        begin
         SetRBStateFrom(NUCState);
         SetRBStateFrom(NUUState);
        end;
     end;
  end else
   begin
    if not Enabled then
     begin
      SetRBStateFrom(RUUState);
      SetRBStateFrom(NUUState);
     end else
    if Down then
     begin
      SetRBStateFrom(DFUState);
      SetRBStateFrom(NFUState);
      SetRBStateFrom(NUUState);
     end else
    if MouseOver then
     begin
      if Focused then
       begin
        SetRBStateFrom(HFUState);
        SetRBStateFrom(HUUState);
        SetRBStateFrom(NFUState);
        SetRBStateFrom(NUUState);
       end else
        begin
         SetRBStateFrom(HUUState);
         SetRBStateFrom(NUUState);
        end;
     end else
      begin
       if Focused then
        begin
         SetRBStateFrom(NFUState);
         SetRBStateFrom(NUUState);
        end else
         begin
          SetRBStateFrom(NUUState);
         end;
      end;
   end;
end;

procedure TSXSkinRadioButtonStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinRadioButtonStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,SetOverStyle,SetGlyphStyle,SetGlyphWidth,
                 SetGlyphHeight,False,False,False);
    if SetStyle then
     SaveString(S,Style);
    if SetOverStyle then
     SaveString(S,OverStyle);
    if SetGlyphStyle then
     SaveString(S,GlyphStyle);
    if SetGlyphWidth then
     SavePackedInteger(S,GlyphWidth);
    if SetGlyphHeight then
     SavePackedInteger(S,GlyphHeight);
    SaveFontData(S,FD);
    //
    Save8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     SavePackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     SavePackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     SavePackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     SavePackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     SavePackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     SavePackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     SavePackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     SavePackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 SaveState(NUUState);
 SaveState(NFUState);
 SaveState(HUUState);
 SaveState(HFUState);
 SaveState(DFUState);
 SaveState(RUUState);
 SaveState(NUCState);
 SaveState(NFCState);
 SaveState(HUCState);
 SaveState(HFCState);
 SaveState(DFCState);
 SaveState(RUCState);
 //
 SaveTransformEffectData(S,HInRadioButtonEffect);
 SaveTransformEffectData(S,HOutRadioButtonEffect);
 SaveTransformEffectData(S,CheckRadioButtonEffect);
 SaveTransformEffectData(S,UncheckRadioButtonEffect);
 SaveTransformEffectData(S,DownRadioButtonEffect);
 SaveTransformEffectData(S,UpRadioButtonEffect);
 SaveTransformEffectData(S,EnableRadioButtonEffect);
 SaveTransformEffectData(S,DisableRadioButtonEffect);
 SaveTransformEffectData(S,FocusRadioButtonEffect);
 SaveTransformEffectData(S,UnfocusRadioButtonEffect);
 //
 SaveTransformEffectData(S,HInGlyphEffect);
 SaveTransformEffectData(S,HOutGlyphEffect);
 SaveTransformEffectData(S,CheckGlyphEffect);
 SaveTransformEffectData(S,UncheckGlyphEffect);
 SaveTransformEffectData(S,DownGlyphEffect);
 SaveTransformEffectData(S,UpGlyphEffect);
 SaveTransformEffectData(S,EnableGlyphEffect);
 SaveTransformEffectData(S,DisableGlyphEffect);
 SaveTransformEffectData(S,FocusGlyphEffect);
 SaveTransformEffectData(S,UnfocusGlyphEffect);
end;

procedure TSXSkinRadioButtonStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;           

 procedure LoadState(var State:TSXSkinRadioButtonStateParam);
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,SetOverStyle,SetGlyphStyle,SetGlyphWidth,
                 SetGlyphHeight,B,B,B);
    if SetStyle then
     LoadString(S,Style);
    if SetOverStyle then
     LoadString(S,OverStyle);
    if SetGlyphStyle then
     LoadString(S,GlyphStyle);
    if SetGlyphWidth then
     LoadPackedInteger(S,GlyphWidth);
    if SetGlyphHeight then
     LoadPackedInteger(S,GlyphHeight);
    LoadFontData(S,FD);
    //
    Load8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     LoadPackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     LoadPackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     LoadPackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     LoadPackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     LoadPackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     LoadPackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     LoadPackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     LoadPackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 LoadState(NUUState);
 LoadState(NFUState);
 LoadState(HUUState);
 LoadState(HFUState);
 LoadState(DFUState);
 LoadState(RUUState);
 LoadState(NUCState);
 LoadState(NFCState);
 LoadState(HUCState);
 LoadState(HFCState);
 LoadState(DFCState);
 LoadState(RUCState);
 //
 LoadTransformEffectData(S,HInRadioButtonEffect);
 LoadTransformEffectData(S,HOutRadioButtonEffect);
 LoadTransformEffectData(S,CheckRadioButtonEffect);
 LoadTransformEffectData(S,UncheckRadioButtonEffect);
 LoadTransformEffectData(S,DownRadioButtonEffect);
 LoadTransformEffectData(S,UpRadioButtonEffect);
 LoadTransformEffectData(S,EnableRadioButtonEffect);
 LoadTransformEffectData(S,DisableRadioButtonEffect);
 LoadTransformEffectData(S,FocusRadioButtonEffect);
 LoadTransformEffectData(S,UnfocusRadioButtonEffect);
 //
 LoadTransformEffectData(S,HInGlyphEffect);
 LoadTransformEffectData(S,HOutGlyphEffect);
 LoadTransformEffectData(S,CheckGlyphEffect);
 LoadTransformEffectData(S,UncheckGlyphEffect);
 LoadTransformEffectData(S,DownGlyphEffect);
 LoadTransformEffectData(S,UpGlyphEffect);
 LoadTransformEffectData(S,EnableGlyphEffect);
 LoadTransformEffectData(S,DisableGlyphEffect);
 LoadTransformEffectData(S,FocusGlyphEffect);
 LoadTransformEffectData(S,UnfocusGlyphEffect);
end;

{ TSXSkinGroupBoxStyle }

procedure TSXSkinGroupBoxStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinGroupBoxStyle then
  begin
   SetLabelStyle:=TSXSkinGroupBoxStyle(Style).SetLabelStyle;
   SetGlyphWidth:=TSXSkinGroupBoxStyle(Style).SetGlyphWidth;
   SetGlyphHeight:=TSXSkinGroupBoxStyle(Style).SetGlyphHeight;
   SetClientRect:=TSXSkinGroupBoxStyle(Style).SetClientRect;
   //
   LabelStyle:=TSXSkinGroupBoxStyle(Style).LabelStyle;
   NUState:=TSXSkinGroupBoxStyle(Style).NUState;
   NFState:=TSXSkinGroupBoxStyle(Style).NFState;
   HUState:=TSXSkinGroupBoxStyle(Style).HUState;
   HFState:=TSXSkinGroupBoxStyle(Style).HFState;
   RUState:=TSXSkinGroupBoxStyle(Style).RUState;
   GlyphWidth:=TSXSkinGroupBoxStyle(Style).GlyphWidth;
   GlyphHeight:=TSXSkinGroupBoxStyle(Style).GlyphHeight;
   ClientRect:=TSXSkinGroupBoxStyle(Style).ClientRect;
  end;
end;

procedure TSXSkinGroupBoxStyle.SetParameter(const Name,Value:String);
var GBParam:PSXSkinGroupBoxStateParam;
          S:String;
begin
 GBParam:=nil;
 S:=Copy(Name,1,3);
 if S='NU_' then GBParam:=@NUState else
  if S='NF_' then GBParam:=@NFState else
  if S='HU_' then GBParam:=@HUState else
  if S='HF_' then GBParam:=@HFState else
  if S='RU_' then GBParam:=@RUState;
 if GBParam=nil then
  begin
   if Name='LabelStyle' then
    begin
     LabelStyle:=Value;
     SetLabelStyle:=True;
    end else
   if Name='GlyphWidth' then
    begin
     GlyphWidth:=StrToIntDef(Value,0);
     SetGlyphWidth:=True;
    end else
   if Name='GlyphHeight' then
    begin
     GlyphHeight:=StrToIntDef(Value,0);
     SetGlyphHeight:=True;
    end else
   if Name='ClientRect' then
    begin
     ClientRect:=WithoutAllSpaces(Value);
     SetClientRect:=True;
    end;
  end else
   begin
    S:=Copy(Name,4,MaxInt);
    if S='Style' then
     begin
      GBParam.Style:=Value;
      GBParam.SetStyle:=True;
     end else
    if S='OverStyle' then
     begin
      GBParam.OverStyle:=Value;
      GBParam.SetOverStyle:=True;
     end else
    if S='CaptionPosition' then
     begin
      GBParam.SetCaptionPosition:=True;
      GBParam.CaptionPosition:=StrToIntDef(Value,0);
     end else
    if S='CaptionLeftOffset' then
     begin
      GBParam.SetCaptionLeftOffset:=True;
      GBParam.CaptionLeftOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionTopOffset' then
     begin
      GBParam.SetCaptionTopOffset:=True;
      GBParam.CaptionTopOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionRightOffset' then
     begin
      GBParam.SetCaptionRightOffset:=True;
      GBParam.CaptionRightOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionBottomOffset' then
     begin
      GBParam.SetCaptionBottomOffset:=True;
      GBParam.CaptionBottomOffset:=StrToIntDef(Value,0);
     end else
    if S='TextLeftOffset' then
     begin
      GBParam.SetTextLeftOffset:=True;
      GBParam.TextLeftOffset:=StrToIntDef(Value,0);
     end else
    if S='TextTopOffset' then
     begin
      GBParam.SetTextTopOffset:=True;
      GBParam.TextTopOffset:=StrToIntDef(Value,0);
     end else
    if S='TextRightOffset' then
     begin
      GBParam.SetTextRightOffset:=True;
      GBParam.TextRightOffset:=StrToIntDef(Value,0);
     end else
    if S='TextBottomOffset' then
     begin
      GBParam.SetTextBottomOffset:=True;
      GBParam.TextBottomOffset:=StrToIntDef(Value,0);
     end else
    if S='TransparentRect' then
     begin
      GBParam.TransparentRect:=WithoutAllSpaces(Value);
      GBParam.SetTransparentRect:=True;
     end;
   end;
end;

procedure TSXSkinGroupBoxStyle.GetCurrentGBState(var GBState:TSXSkinGroupBoxStateParam;
           MouseOver,Focused,Enabled:Boolean);

 procedure SetGBStateFrom(T:TSXSkinGroupBoxStateParam);
 begin
  if GBState.Style='' then GBState.Style:=T.Style;
  if GBState.OverStyle='' then GBState.OverStyle:=T.OverStyle;
  if not GBState.SetCaptionPosition and T.SetCaptionPosition then
   begin
    GBState.SetCaptionPosition:=True;
    GBState.CaptionPosition:=T.CaptionPosition;
   end;
  if not GBState.SetCaptionLeftOffset and T.SetCaptionLeftOffset then
   begin
    GBState.SetCaptionLeftOffset:=True;
    GBState.CaptionLeftOffset:=T.CaptionLeftOffset;
   end;
  if not GBState.SetCaptionTopOffset and T.SetCaptionTopOffset then
   begin
    GBState.SetCaptionTopOffset:=True;
    GBState.CaptionTopOffset:=T.CaptionTopOffset;
   end;
  if not GBState.SetCaptionRightOffset and T.SetCaptionRightOffset then
   begin
    GBState.SetCaptionRightOffset:=True;
    GBState.CaptionRightOffset:=T.CaptionRightOffset;
   end;
  if not GBState.SetCaptionBottomOffset and T.SetCaptionBottomOffset then
   begin
    GBState.SetCaptionBottomOffset:=True;
    GBState.CaptionBottomOffset:=T.CaptionBottomOffset;
   end;
  if not GBState.SetTextLeftOffset and T.SetTextLeftOffset then
   begin
    GBState.SetTextLeftOffset:=True;
    GBState.TextLeftOffset:=T.TextLeftOffset;
   end;
  if not GBState.SetTextTopOffset and T.SetTextTopOffset then
   begin
    GBState.SetTextTopOffset:=True;
    GBState.TextTopOffset:=T.TextTopOffset;
   end;
  if not GBState.SetTextRightOffset and T.SetTextRightOffset then
   begin
    GBState.SetTextRightOffset:=True;
    GBState.TextRightOffset:=T.TextRightOffset;
   end;
  if not GBState.SetTextBottomOffset and T.SetTextBottomOffset then
   begin
    GBState.SetTextBottomOffset:=True;
    GBState.TextBottomOffset:=T.TextBottomOffset;
   end;
  if GBState.TransparentRect='' then GBState.TransparentRect:=T.TransparentRect;
 end;

begin
 Finalize(GBState);
 FillChar(GBState,sizeof(GBState),0);
 if not Enabled then
  begin
   SetGBStateFrom(RUState);
   SetGBStateFrom(NUState);
  end else
 if MouseOver then
  begin
   if Focused then
    begin
     SetGBStateFrom(HFState);
     SetGBStateFrom(HUState);
     SetGBStateFrom(NFState);
     SetGBStateFrom(NUState);
    end else
     begin
      SetGBStateFrom(HUState);
      SetGBStateFrom(NUState);
     end;
  end else
   begin
    if Focused then
     begin
      SetGBStateFrom(NFState);
      SetGBStateFrom(NUState);
     end else
      begin
       SetGBStateFrom(NUState);
      end;
   end;
end;

procedure TSXSkinGroupBoxStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinGroupBoxStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,SetOverStyle,SetCaptionPosition,SetTransparentRect,
                 SetCaptionPosition,False,False,False);
    if SetStyle then
     SaveString(S,Style);
    if SetOverStyle then
     SaveString(S,OverStyle);
    if SetCaptionPosition then
     SavePackedInteger(S,CaptionPosition);
    //
    Save8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     SavePackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     SavePackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     SavePackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     SavePackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     SavePackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     SavePackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     SavePackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     SavePackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 SaveState(NUState);
 SaveState(NFState);
 SaveState(HUState);
 SaveState(HFState);
 SaveState(RUState);
 Save8Flags(S,SetLabelStyle,SetGlyphWidth,SetGlyphHeight,SetClientRect,
              False,False,False,False);
 if SetLabelStyle then
  SaveString(S,LabelStyle);
 if SetGlyphWidth then
  SavePackedInteger(S,GlyphWidth);
 if SetGlyphHeight then
  SavePackedInteger(S,GlyphHeight);
 if SetClientRect then
  SaveString(S,ClientRect);
end;

procedure TSXSkinGroupBoxStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;           

 procedure LoadState(var State:TSXSkinGroupBoxStateParam);
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,SetOverStyle,SetCaptionPosition,SetTransparentRect,
                 SetCaptionPosition,B,B,B);
    if SetStyle then
     LoadString(S,Style);
    if SetOverStyle then
     LoadString(S,OverStyle);
    if SetCaptionPosition then
     LoadPackedInteger(S,CaptionPosition);
    //
    Load8Flags(S,SetCaptionLeftOffset,SetCaptionTopOffset,SetCaptionRightOffset,SetCaptionBottomOffset,
                 SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,SetTextBottomOffset);
    if SetCaptionLeftOffset then
     LoadPackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     LoadPackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     LoadPackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     LoadPackedInteger(S,CaptionBottomOffset);
    //
    if SetTextLeftOffset then
     LoadPackedInteger(S,TextLeftOffset);
    if SetTextTopOffset then
     LoadPackedInteger(S,TextTopOffset);
    if SetTextRightOffset then
     LoadPackedInteger(S,TextRightOffset);
    if SetTextBottomOffset then
     LoadPackedInteger(S,TextBottomOffset);
   end;
 end;

begin
 inherited;
 LoadState(NUState);
 LoadState(NFState);
 LoadState(HUState);
 LoadState(HFState);
 LoadState(RUState);
 Load8Flags(S,SetLabelStyle,SetGlyphWidth,SetGlyphHeight,SetClientRect,B,B,B,B);
 if SetLabelStyle then
  LoadString(S,LabelStyle);
 if SetGlyphWidth then
  LoadPackedInteger(S,GlyphWidth);
 if SetGlyphHeight then
  LoadPackedInteger(S,GlyphHeight);
 if SetClientRect then
  LoadString(S,ClientRect);
end;

{ TSXSkinMultiStateStyle }

procedure TSXSkinMultiStateStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinMultiStateStyle then
  begin
   SetNStyle:=TSXSkinMultiStateStyle(Style).SetNStyle;
   SetHStyle:=TSXSkinMultiStateStyle(Style).SetHStyle;
   SetDStyle:=TSXSkinMultiStateStyle(Style).SetDStyle;
   SetRStyle:=TSXSkinMultiStateStyle(Style).SetRStyle;
   //
   NStyle:=TSXSkinMultiStateStyle(Style).NStyle;
   HStyle:=TSXSkinMultiStateStyle(Style).HStyle;
   DStyle:=TSXSkinMultiStateStyle(Style).DStyle;
   RStyle:=TSXSkinMultiStateStyle(Style).RStyle;
  end;
end;

procedure TSXSkinMultiStateStyle.SetParameter(const Name,Value:String);
begin
 if Name='NStyle' then
  begin
   NStyle:=Value;
   SetNStyle:=True;
  end else
 if Name='HStyle' then
  begin
   HStyle:=Value;
   SetHStyle:=True;
  end else
 if Name='DStyle' then
  begin
   DStyle:=Value;
   SetDStyle:=True;
  end else
 if Name='RStyle' then
  begin
   RStyle:=Value;
   SetRStyle:=True;
  end;
end;

procedure TSXSkinMultiStateStyle.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 Save8Flags(S,SetNStyle,SetHStyle,SetDStyle,SetRStyle,False,False,False,False);
 if SetNStyle then
  SaveString(S,NStyle);
 if SetHStyle then
  SaveString(S,HStyle);
 if SetDStyle then
  SaveString(S,DStyle);
 if SetRStyle then
  SaveString(S,RStyle);
end;

procedure TSXSkinMultiStateStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;           
begin
 inherited;
 Load8Flags(S,SetNStyle,SetHStyle,SetDStyle,SetRStyle,B,B,B,B);
 if SetNStyle then
  LoadString(S,NStyle);
 if SetHStyle then
  LoadString(S,HStyle);
 if SetDStyle then
  LoadString(S,DStyle);
 if SetRStyle then
  LoadString(S,RStyle);
end;

{ TSXSkinMultiStateCheckStyle }

procedure TSXSkinMultiStateCheckStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinMultiStateCheckStyle then
  begin
   SetNUStyle:=TSXSkinMultiStateCheckStyle(Style).SetNUStyle;
   SetHUStyle:=TSXSkinMultiStateCheckStyle(Style).SetHUStyle;
   SetDUStyle:=TSXSkinMultiStateCheckStyle(Style).SetDUStyle;
   SetRUStyle:=TSXSkinMultiStateCheckStyle(Style).SetRUStyle;
   SetNCStyle:=TSXSkinMultiStateCheckStyle(Style).SetNCStyle;
   SetHCStyle:=TSXSkinMultiStateCheckStyle(Style).SetHCStyle;
   SetDCStyle:=TSXSkinMultiStateCheckStyle(Style).SetDCStyle;
   SetRCStyle:=TSXSkinMultiStateCheckStyle(Style).SetRCStyle;
   //
   NUStyle:=TSXSkinMultiStateCheckStyle(Style).NUStyle;
   HUStyle:=TSXSkinMultiStateCheckStyle(Style).HUStyle;
   DUStyle:=TSXSkinMultiStateCheckStyle(Style).DUStyle;
   RUStyle:=TSXSkinMultiStateCheckStyle(Style).RUStyle;
   NCStyle:=TSXSkinMultiStateCheckStyle(Style).NCStyle;
   HCStyle:=TSXSkinMultiStateCheckStyle(Style).HCStyle;
   DCStyle:=TSXSkinMultiStateCheckStyle(Style).DCStyle;
   RCStyle:=TSXSkinMultiStateCheckStyle(Style).RCStyle;
  end;
end;

procedure TSXSkinMultiStateCheckStyle.SetParameter(const Name,Value:String);
begin
 if Name='NUStyle' then
  begin
   NUStyle:=Value;
   SetNUStyle:=True;
  end else
 if Name='HUStyle' then
  begin
   HUStyle:=Value;
   SetHUStyle:=True;
  end else
 if Name='DUStyle' then
  begin
   DUStyle:=Value;
   SetDUStyle:=True;
  end else
 if Name='RUStyle' then
  begin
   RUStyle:=Value;
   SetRUStyle:=True;
  end else
 if Name='NCStyle' then
  begin
   NCStyle:=Value;
   SetNCStyle:=True;
  end else
 if Name='HCStyle' then
  begin
   HCStyle:=Value;
   SetHCStyle:=True;
  end else
 if Name='DCStyle' then
  begin
   DCStyle:=Value;
   SetDCStyle:=True;
  end else
 if Name='RCStyle' then
  begin
   RCStyle:=Value;
   SetRCStyle:=True;
  end; 
end;

procedure TSXSkinMultiStateCheckStyle.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 Save8Flags(S,SetNUStyle,SetHUStyle,SetDUStyle,SetRUStyle,
              SetNCStyle,SetHCStyle,SetDCStyle,SetRCStyle);
 if SetNUStyle then
  SaveString(S,NUStyle);
 if SetHUStyle then
  SaveString(S,HUStyle);
 if SetDUStyle then
  SaveString(S,DUStyle);
 if SetRUStyle then
  SaveString(S,RUStyle);
 if SetNCStyle then
  SaveString(S,NCStyle);
 if SetHCStyle then
  SaveString(S,HCStyle);
 if SetDCStyle then
  SaveString(S,DCStyle);
 if SetRCStyle then
  SaveString(S,RCStyle);
end;

procedure TSXSkinMultiStateCheckStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
begin
 inherited;
 Load8Flags(S,SetNUStyle,SetHUStyle,SetDUStyle,SetRUStyle,
              SetNCStyle,SetHCStyle,SetDCStyle,SetRCStyle);
 if SetNUStyle then
  LoadString(S,NUStyle);
 if SetHUStyle then
  LoadString(S,HUStyle);
 if SetDUStyle then
  LoadString(S,DUStyle);
 if SetRUStyle then
  LoadString(S,RUStyle);
 if SetNCStyle then
  LoadString(S,NCStyle);
 if SetHCStyle then
  LoadString(S,HCStyle);
 if SetDCStyle then
  LoadString(S,DCStyle);
 if SetRCStyle then
  LoadString(S,RCStyle);
end;

{ TSXSkinLabelStyle }

procedure TSXSkinLabelStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinLabelStyle then
  begin
   NState:=TSXSkinLabelStyle(Style).NState;
   HState:=TSXSkinLabelStyle(Style).HState;
   RState:=TSXSkinLabelStyle(Style).RState;
  end;
end;

procedure TSXSkinLabelStyle.SetParameter(const Name,Value:String);
var LParam:PSXSkinLabelStateParam;
         S:String;
begin
 LParam:=nil;
 S:=Copy(Name,1,2);
 if S='N_' then LParam:=@NState else
  if S='H_' then LParam:=@HState else
  if S='R_' then LParam:=@RState;
 if LParam=nil then
  begin
   //
  end else
   begin
    S:=Copy(Name,3,MaxInt);
    TryToSetFontDataParameter(S,Value,LParam.FD);
   end;
end;

procedure TSXSkinLabelStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinLabelStateParam);
 begin
  SaveFontData(S,State.FD);
 end;

begin
 inherited;
 SaveState(NState);
 SaveState(HState);
 SaveState(RState);
end;

procedure TSXSkinLabelStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);

 procedure LoadState(var State:TSXSkinLabelStateParam);
 begin
  LoadFontData(S,State.FD);
 end;

begin
 inherited;
 LoadState(NState);
 LoadState(HState);
 LoadState(RState);
end;

{ TSXSkinButtonStyle }

procedure TSXSkinButtonStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinButtonStyle then
  begin
   NUUState:=TSXSkinButtonStyle(Style).NUUState;
   NFUState:=TSXSkinButtonStyle(Style).NFUState;
   HUUState:=TSXSkinButtonStyle(Style).HUUState;
   HFUState:=TSXSkinButtonStyle(Style).HFUState;
   DFUState:=TSXSkinButtonStyle(Style).DFUState;
   RUUState:=TSXSkinButtonStyle(Style).RUUState;
   NUCState:=TSXSkinButtonStyle(Style).NUCState;
   NFCState:=TSXSkinButtonStyle(Style).NFCState;
   HUCState:=TSXSkinButtonStyle(Style).HUCState;
   HFCState:=TSXSkinButtonStyle(Style).HFCState;
   DFCState:=TSXSkinButtonStyle(Style).DFCState;
   RUCState:=TSXSkinButtonStyle(Style).RUCState;
   //
   HInButtonEffect:=TSXSkinButtonStyle(Style).HInButtonEffect;
   HOutButtonEffect:=TSXSkinButtonStyle(Style).HOutButtonEffect;
   CheckButtonEffect:=TSXSkinButtonStyle(Style).CheckButtonEffect;
   UncheckButtonEffect:=TSXSkinButtonStyle(Style).UncheckButtonEffect;
   DownButtonEffect:=TSXSkinButtonStyle(Style).DownButtonEffect;
   UpButtonEffect:=TSXSkinButtonStyle(Style).UpButtonEffect;
   EnableButtonEffect:=TSXSkinButtonStyle(Style).EnableButtonEffect;
   DisableButtonEffect:=TSXSkinButtonStyle(Style).DisableButtonEffect;
   FocusButtonEffect:=TSXSkinButtonStyle(Style).FocusButtonEffect;
   UnfocusButtonEffect:=TSXSkinButtonStyle(Style).UnfocusButtonEffect;
   //
   HInGlyphEffect:=TSXSkinButtonStyle(Style).HInGlyphEffect;
   HOutGlyphEffect:=TSXSkinButtonStyle(Style).HOutGlyphEffect;
   CheckGlyphEffect:=TSXSkinButtonStyle(Style).CheckGlyphEffect;
   UncheckGlyphEffect:=TSXSkinButtonStyle(Style).UncheckGlyphEffect;
   DownGlyphEffect:=TSXSkinButtonStyle(Style).DownGlyphEffect;
   UpGlyphEffect:=TSXSkinButtonStyle(Style).UpGlyphEffect;
   EnableGlyphEffect:=TSXSkinButtonStyle(Style).EnableGlyphEffect;
   DisableGlyphEffect:=TSXSkinButtonStyle(Style).DisableGlyphEffect;
   FocusGlyphEffect:=TSXSkinButtonStyle(Style).FocusGlyphEffect;
   UnfocusGlyphEffect:=TSXSkinButtonStyle(Style).UnfocusGlyphEffect;
   //
   HInDDGlyphEffect:=TSXSkinButtonStyle(Style).HInDDGlyphEffect;
   HOutDDGlyphEffect:=TSXSkinButtonStyle(Style).HOutDDGlyphEffect;
   CheckDDGlyphEffect:=TSXSkinButtonStyle(Style).CheckDDGlyphEffect;
   UncheckDDGlyphEffect:=TSXSkinButtonStyle(Style).UncheckDDGlyphEffect;
   DownDDGlyphEffect:=TSXSkinButtonStyle(Style).DownDDGlyphEffect;
   UpDDGlyphEffect:=TSXSkinButtonStyle(Style).UpDDGlyphEffect;
   EnableDDGlyphEffect:=TSXSkinButtonStyle(Style).EnableDDGlyphEffect;
   DisableDDGlyphEffect:=TSXSkinButtonStyle(Style).DisableDDGlyphEffect;
   FocusDDGlyphEffect:=TSXSkinButtonStyle(Style).FocusDDGlyphEffect;
   UnfocusDDGlyphEffect:=TSXSkinButtonStyle(Style).UnfocusDDGlyphEffect;
   //
   DropDownGlyphWidth:=TSXSkinButtonStyle(Style).DropDownGlyphWidth;
   DropDownGlyphHeight:=TSXSkinButtonStyle(Style).DropDownGlyphHeight;
   DropDownGlyphOffset:=TSXSkinButtonStyle(Style).DropDownGlyphOffset;
  end;
end;

procedure TSXSkinButtonStyle.SetParameter(const Name,Value:String);
var BParam:PSXSkinButtonStateParam;
      S,S2:String;
begin
 BParam:=nil;
 S:=Copy(Name,1,3);
 S2:=Copy(Name,1,4);
 if (S='NU_') or (S2='NUU_') then BParam:=@NUUState else
  if (S='NF_') or (S2='NFU_') then BParam:=@NFUState else
  if (S='HU_') or (S2='HUU_') then BParam:=@HUUState else
  if (S='HF_') or (S2='HFU_') then BParam:=@HFUState else
  if (S='DF_') or (S2='DFU_') then BParam:=@DFUState else
  if (S='RU_') or (S2='RUU_') then BParam:=@RUUState else
  if S2='NUC_' then BParam:=@NUCState else
  if S2='NFC_' then BParam:=@NFCState else
  if S2='HUC_' then BParam:=@HUCState else
  if S2='HFC_' then BParam:=@HFCState else
  if S2='DFC_' then BParam:=@DFCState else
  if S2='RUC_' then BParam:=@RUCState;
 if BParam=nil then
  begin
   if not TryToSetTransformEffectDataParameter(Name,Value,HInButtonEffect,'HighlightInButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutButtonEffect,'HighlightOutButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckButtonEffect,'CheckButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckButtonEffect,'UncheckButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownButtonEffect,'DownButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpButtonEffect,'UpButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableButtonEffect,'EnableButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableButtonEffect,'DisableButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusButtonEffect,'FocusButton') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusButtonEffect,'UnfocusButton') then
    //
    if not TryToSetTransformEffectDataParameter(Name,Value,HInGlyphEffect,'HighlightInGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutGlyphEffect,'HighlightOutGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckGlyphEffect,'CheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckGlyphEffect,'UncheckGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownGlyphEffect,'DownGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpGlyphEffect,'UpGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableGlyphEffect,'EnableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableGlyphEffect,'DisableGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusGlyphEffect,'FocusGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusGlyphEffect,'UnfocusGlyph') then
    //
    if not TryToSetTransformEffectDataParameter(Name,Value,HInDDGlyphEffect,'HighlightInDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutDDGlyphEffect,'HighlightOutDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,CheckDDGlyphEffect,'CheckDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UncheckDDGlyphEffect,'UncheckDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownDDGlyphEffect,'DownDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpDDGlyphEffect,'UpDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableDDGlyphEffect,'EnableDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableDDGlyphEffect,'DisableDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusDDGlyphEffect,'FocusDDGlyph') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusDDGlyphEffect,'UnfocusDDGlyph') then
    //
    if Name='DropDownGlyphWidth' then
     begin
      DropDownGlyphWidth:=StrToIntDef(Value,0);
      SetDropDownGlyphWidth:=True;
     end else
    if Name='DropDownGlyphHeight' then
     begin
      DropDownGlyphHeight:=StrToIntDef(Value,0);
      SetDropDownGlyphHeight:=True;
     end else
    if Name='DropDownGlyphOffset' then
     begin
      DropDownGlyphOffset:=StrToIntDef(Value,0);
      SetDropDownGlyphOffset:=True;
     end;
  end else
   begin
    if Name[3]='_' then S:=Copy(Name,4,MaxInt) else
     S:=Copy(Name,5,MaxInt);
    if S='Style' then
     begin
      BParam.Style:=Value;
      BParam.SetStyle:=True;
     end else
    if S='OverStyle' then
     begin
      BParam.OverStyle:=Value;
      BParam.SetOverStyle:=True;
     end else
    if S='CaptionLeftOffset' then
     begin
      BParam.SetCaptionLeftOffset:=True;
      BParam.CaptionLeftOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionTopOffset' then
     begin
      BParam.SetCaptionTopOffset:=True;
      BParam.CaptionTopOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionRightOffset' then
     begin
      BParam.SetCaptionRightOffset:=True;
      BParam.CaptionRightOffset:=StrToIntDef(Value,0);
     end else
    if S='CaptionBottomOffset' then
     begin
      BParam.SetCaptionBottomOffset:=True;
      BParam.CaptionBottomOffset:=StrToIntDef(Value,0);
     end else
    if not TryToSetFilterDataParameter(S,Value,BParam.DefGlyphFilter,'DefGlyph') then
     TryToSetFontDataParameter(S,Value,BParam.FD);
   end;
end;

procedure TSXSkinButtonStyle.GetCurrentBState(var BState:TSXSkinButtonStateParam;
           FChecked,Enabled,FMouseOver,FDown,FLastFocused:Boolean);

 procedure SetBStateFrom(const T:TSXSkinButtonStateParam);
 begin
  if BState.Style='' then BState.Style:=T.Style;
  if BState.OverStyle='' then BState.OverStyle:=T.OverStyle;
  if not BState.SetCaptionLeftOffset and T.SetCaptionLeftOffset then
   begin
    BState.SetCaptionLeftOffset:=True;
    BState.CaptionLeftOffset:=T.CaptionLeftOffset;
   end;
  if not BState.SetCaptionTopOffset and T.SetCaptionTopOffset then
   begin
    BState.SetCaptionTopOffset:=True;
    BState.CaptionTopOffset:=T.CaptionTopOffset;
   end;
  if not BState.SetCaptionRightOffset and T.SetCaptionRightOffset then
   begin
    BState.SetCaptionRightOffset:=True;
    BState.CaptionRightOffset:=T.CaptionRightOffset;
   end;
  if not BState.SetCaptionBottomOffset and T.SetCaptionBottomOffset then
   begin
    BState.SetCaptionBottomOffset:=True;
    BState.CaptionBottomOffset:=T.CaptionBottomOffset;
   end;
  if not BState.DefGlyphFilter.SetAType and T.DefGlyphFilter.SetAType then
   begin
    BState.DefGlyphFilter.SetAType:=True;
    BState.DefGlyphFilter.AType:=T.DefGlyphFilter.AType;
    BState.DefGlyphFilter.Color:=T.DefGlyphFilter.Color;
    BState.DefGlyphFilter.Color2:=T.DefGlyphFilter.Color2;
    BState.DefGlyphFilter.Value:=T.DefGlyphFilter.Value;
   end;
  AddFontData(BState.FD,T.FD);
 end;

begin
 Finalize(BState);
 FillChar(BState,sizeof(BState),0);
 if FChecked then
  begin
   if not Enabled then
    begin
     SetBStateFrom(RUCState);
     SetBStateFrom(NUCState);
     SetBStateFrom(RUUState);
     SetBStateFrom(NUUState);
    end else
   if FDown then
    begin
     SetBStateFrom(DFCState);
     SetBStateFrom(DFUState);
     SetBStateFrom(NFCState);
     SetBStateFrom(NFUState);
     SetBStateFrom(NUCState);
     SetBStateFrom(NUUState);
    end else
   if FMouseOver then
    begin
     if FLastFocused then
      begin
       SetBStateFrom(HFCState);
       SetBStateFrom(HUCState);
       SetBStateFrom(HFUState);
       SetBStateFrom(HUUState);
       SetBStateFrom(NFCState);
       SetBStateFrom(NUCState);
       SetBStateFrom(NFUState);
       SetBStateFrom(NUUState);
      end else
       begin
        SetBStateFrom(HUCState);
        SetBStateFrom(NUCState);
        SetBStateFrom(HUUState);
        SetBStateFrom(NUUState);
       end;
    end else
     begin
      if FLastFocused then
       begin
        SetBStateFrom(NFCState);
        SetBStateFrom(NUCState);
        SetBStateFrom(NFUState);
        SetBStateFrom(NUUState);
       end else
        begin
         SetBStateFrom(NUCState);
         SetBStateFrom(NUUState);
        end;
     end;
  end else
   begin
    if not Enabled then
     begin
      SetBStateFrom(RUUState);
      SetBStateFrom(NUUState);
     end else
    if FDown then
     begin
      SetBStateFrom(DFUState);
      SetBStateFrom(NFUState);
      SetBStateFrom(NUUState);
     end else
    if FMouseOver then
     begin
      if FLastFocused then
       begin
        SetBStateFrom(HFUState);
        SetBStateFrom(HUUState);
        SetBStateFrom(NFUState);
        SetBStateFrom(NUUState);
       end else
        begin
         SetBStateFrom(HUUState);
         SetBStateFrom(NUUState);
        end;
     end else
      begin
       if FLastFocused then
        begin
         SetBStateFrom(NFUState);
         SetBStateFrom(NUUState);
        end else
         begin
          SetBStateFrom(NUUState);
         end;
      end;
   end;
end;

procedure TSXSkinButtonStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinButtonStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,SetOverStyle,SetCaptionLeftOffset,SetCaptionTopOffset,
                 SetCaptionRightOffset,SetCaptionBottomOffset,False,False);
    if SetStyle then
     SaveString(S,Style);
    if SetOverStyle then
     SaveString(S,OverStyle);
    SaveFontData(S,FD);
    //
    if SetCaptionLeftOffset then
     SavePackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     SavePackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     SavePackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     SavePackedInteger(S,CaptionBottomOffset);
    SaveFilterData(S,DefGlyphFilter);
   end;
 end;

begin
 inherited;
 SaveState(NUUState);
 SaveState(NFUState);
 SaveState(HUUState);
 SaveState(HFUState);
 SaveState(DFUState);
 SaveState(RUUState);
 SaveState(NUCState);
 SaveState(NFCState);
 SaveState(HUCState);
 SaveState(HFCState);
 SaveState(DFCState);
 SaveState(RUCState);
 //
 SaveTransformEffectData(S,HInButtonEffect);
 SaveTransformEffectData(S,HOutButtonEffect);
 SaveTransformEffectData(S,CheckButtonEffect);
 SaveTransformEffectData(S,UncheckButtonEffect);
 SaveTransformEffectData(S,DownButtonEffect);
 SaveTransformEffectData(S,UpButtonEffect);
 SaveTransformEffectData(S,EnableButtonEffect);
 SaveTransformEffectData(S,DisableButtonEffect);
 SaveTransformEffectData(S,FocusButtonEffect);
 SaveTransformEffectData(S,UnfocusButtonEffect);
 //
 SaveTransformEffectData(S,HInGlyphEffect);
 SaveTransformEffectData(S,HOutGlyphEffect);
 SaveTransformEffectData(S,CheckGlyphEffect);
 SaveTransformEffectData(S,UncheckGlyphEffect);
 SaveTransformEffectData(S,DownGlyphEffect);
 SaveTransformEffectData(S,UpGlyphEffect);
 SaveTransformEffectData(S,EnableGlyphEffect);
 SaveTransformEffectData(S,DisableGlyphEffect);
 SaveTransformEffectData(S,FocusGlyphEffect);
 SaveTransformEffectData(S,UnfocusGlyphEffect);
 //
 SaveTransformEffectData(S,HInDDGlyphEffect);
 SaveTransformEffectData(S,HOutDDGlyphEffect);
 SaveTransformEffectData(S,CheckDDGlyphEffect);
 SaveTransformEffectData(S,UncheckDDGlyphEffect);
 SaveTransformEffectData(S,DownDDGlyphEffect);
 SaveTransformEffectData(S,UpDDGlyphEffect);
 SaveTransformEffectData(S,EnableDDGlyphEffect);
 SaveTransformEffectData(S,DisableDDGlyphEffect);
 SaveTransformEffectData(S,FocusDDGlyphEffect);
 SaveTransformEffectData(S,UnfocusDDGlyphEffect);
 //
 Save8Flags(S,SetDropDownGlyphWidth,SetDropDownGlyphHeight,SetDropDownGlyphOffset,
              False,False,False,False,False); 
 if SetDropDownGlyphWidth then
  SavePackedInteger(S,DropDownGlyphWidth);
 if SetDropDownGlyphHeight then
  SavePackedInteger(S,DropDownGlyphHeight);
 if SetDropDownGlyphOffset then
  SavePackedInteger(S,DropDownGlyphOffset);
end;

procedure TSXSkinButtonStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;

 procedure LoadState(var State:TSXSkinButtonStateParam);
 var B:Boolean;
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,SetOverStyle,SetCaptionLeftOffset,SetCaptionTopOffset,
                 SetCaptionRightOffset,SetCaptionBottomOffset,B,B);
    if SetStyle then
     LoadString(S,Style);
    if SetOverStyle then
     LoadString(S,OverStyle);
    LoadFontData(S,FD);
    //
    if SetCaptionLeftOffset then
     LoadPackedInteger(S,CaptionLeftOffset);
    if SetCaptionTopOffset then
     LoadPackedInteger(S,CaptionTopOffset);
    if SetCaptionRightOffset then
     LoadPackedInteger(S,CaptionRightOffset);
    if SetCaptionBottomOffset then
     LoadPackedInteger(S,CaptionBottomOffset);
    LoadFilterData(S,DefGlyphFilter);
   end;
 end;

begin
 inherited;
 LoadState(NUUState);
 LoadState(NFUState);
 LoadState(HUUState);
 LoadState(HFUState);
 LoadState(DFUState);
 LoadState(RUUState);
 LoadState(NUCState);
 LoadState(NFCState);
 LoadState(HUCState);
 LoadState(HFCState);
 LoadState(DFCState);
 LoadState(RUCState);
 //
 LoadTransformEffectData(S,HInButtonEffect);
 LoadTransformEffectData(S,HOutButtonEffect);
 LoadTransformEffectData(S,CheckButtonEffect);
 LoadTransformEffectData(S,UncheckButtonEffect);
 LoadTransformEffectData(S,DownButtonEffect);
 LoadTransformEffectData(S,UpButtonEffect);
 LoadTransformEffectData(S,EnableButtonEffect);
 LoadTransformEffectData(S,DisableButtonEffect);
 LoadTransformEffectData(S,FocusButtonEffect);
 LoadTransformEffectData(S,UnfocusButtonEffect);
 //
 LoadTransformEffectData(S,HInGlyphEffect);
 LoadTransformEffectData(S,HOutGlyphEffect);
 LoadTransformEffectData(S,CheckGlyphEffect);
 LoadTransformEffectData(S,UncheckGlyphEffect);
 LoadTransformEffectData(S,DownGlyphEffect);
 LoadTransformEffectData(S,UpGlyphEffect);
 LoadTransformEffectData(S,EnableGlyphEffect);
 LoadTransformEffectData(S,DisableGlyphEffect);
 LoadTransformEffectData(S,FocusGlyphEffect);
 LoadTransformEffectData(S,UnfocusGlyphEffect);
 //
 LoadTransformEffectData(S,HInDDGlyphEffect);
 LoadTransformEffectData(S,HOutDDGlyphEffect);
 LoadTransformEffectData(S,CheckDDGlyphEffect);
 LoadTransformEffectData(S,UncheckDDGlyphEffect);
 LoadTransformEffectData(S,DownDDGlyphEffect);
 LoadTransformEffectData(S,UpDDGlyphEffect);
 LoadTransformEffectData(S,EnableDDGlyphEffect);
 LoadTransformEffectData(S,DisableDDGlyphEffect);
 LoadTransformEffectData(S,FocusDDGlyphEffect);
 LoadTransformEffectData(S,UnfocusDDGlyphEffect);
 //
 Load8Flags(S,SetDropDownGlyphWidth,SetDropDownGlyphHeight,SetDropDownGlyphOffset,B,B,B,B,B);
 if SetDropDownGlyphWidth then
  LoadPackedInteger(S,DropDownGlyphWidth);
 if SetDropDownGlyphHeight then
  LoadPackedInteger(S,DropDownGlyphHeight);
 if SetDropDownGlyphOffset then
  LoadPackedInteger(S,DropDownGlyphOffset);
end;

{ TSXSkinEditStyle }

procedure TSXSkinEditStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinEditStyle then
  begin
   NUState:=TSXSkinEditStyle(Style).NUState;
   NFState:=TSXSkinEditStyle(Style).NFState;
   HUState:=TSXSkinEditStyle(Style).HUState;
   HFState:=TSXSkinEditStyle(Style).HFState;
   RUState:=TSXSkinEditStyle(Style).RUState;
   //
   SetTextLeftOffset:=TSXSkinEditStyle(Style).SetTextLeftOffset;
   SetTextTopOffset:=TSXSkinEditStyle(Style).SetTextTopOffset;
   SetTextRightOffset:=TSXSkinEditStyle(Style).SetTextRightOffset;
   SetTextBottomOffset:=TSXSkinEditStyle(Style).SetTextBottomOffset;
   //
   TextLeftOffset:=TSXSkinEditStyle(Style).TextLeftOffset;
   TextTopOffset:=TSXSkinEditStyle(Style).TextTopOffset;
   TextRightOffset:=TSXSkinEditStyle(Style).TextRightOffset;
   TextBottomOffset:=TSXSkinEditStyle(Style).TextBottomOffset;
   //
   HInEditEffect:=TSXSkinEditStyle(Style).HInEditEffect;
   HOutEditEffect:=TSXSkinEditStyle(Style).HOutEditEffect;
   EnableEditEffect:=TSXSkinEditStyle(Style).EnableEditEffect;
   DisableEditEffect:=TSXSkinEditStyle(Style).DisableEditEffect;
   FocusEditEffect:=TSXSkinEditStyle(Style).FocusEditEffect;
   UnfocusEditEffect:=TSXSkinEditStyle(Style).UnfocusEditEffect;
  end;
end;

procedure TSXSkinEditStyle.SetParameter(const Name,Value:String);
var EParam:PSXSkinEditStateParam;
         S:String;
begin
 EParam:=nil;
 S:=Copy(Name,1,3);
 if S='NU_' then EParam:=@NUState else
  if S='NF_' then EParam:=@NFState else
  if S='HU_' then EParam:=@HUState else
  if S='HF_' then EParam:=@HFState else
  if S='RU_' then EParam:=@RUState;
 if EParam=nil then
  begin
   if not TryToSetTransformEffectDataParameter(Name,Value,HInEditEffect,'HighlightInEdit') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutEditEffect,'HighlightOutEdit') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableEditEffect,'EnableEdit') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableEditEffect,'DisableEdit') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusEditEffect,'FocusEdit') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusEditEffect,'UnfocusEdit') then
    if Name='TextLeftOffset' then
     begin
      TextLeftOffset:=StrToIntDef(Value,0);
      SetTextLeftOffset:=True;
     end else
    if Name='TextTopOffset' then
     begin
      TextTopOffset:=StrToIntDef(Value,0);
      SetTextTopOffset:=True;
     end else
    if Name='TextRightOffset' then
     begin
      TextRightOffset:=StrToIntDef(Value,0);
      SetTextRightOffset:=True;
     end else
    if Name='TextBottomOffset' then
     begin
      TextBottomOffset:=StrToIntDef(Value,0);
      SetTextBottomOffset:=True;
     end; 
  end else
   begin
    S:=Copy(Name,4,MaxInt);
    if S='Style' then
     begin
      EParam.Style:=Value;
      EParam.SetStyle:=True;
     end else
      TryToSetFontDataParameter(S,Value,EParam.FD);
   end;
end;

procedure TSXSkinEditStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinEditStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,False,False,False,False,False,False,False);
    if SetStyle then
     SaveString(S,Style);
    SaveFontData(S,FD);
   end;
 end;

begin
 inherited;
 SaveState(NUState);
 SaveState(NFState);
 SaveState(HUState);
 SaveState(HFState);
 SaveState(RUState);
 //
 Save8Flags(S,SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,
              SetTextBottomOffset,False,False,False,False);
 if SetTextLeftOffset then
  SavePackedInteger(S,TextLeftOffset);
 if SetTextTopOffset then
  SavePackedInteger(S,TextTopOffset);
 if SetTextRightOffset then
  SavePackedInteger(S,TextRightOffset);
 if SetTextBottomOffset then
  SavePackedInteger(S,TextBottomOffset);
 //
 SaveTransformEffectData(S,HInEditEffect);
 SaveTransformEffectData(S,HOutEditEffect);
 SaveTransformEffectData(S,EnableEditEffect);
 SaveTransformEffectData(S,DisableEditEffect);
 SaveTransformEffectData(S,FocusEditEffect);
 SaveTransformEffectData(S,UnfocusEditEffect);
end;

procedure TSXSkinEditStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;           

 procedure LoadState(var State:TSXSkinEditStateParam);
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,B,B,B,B,B,B,B);
    if SetStyle then
     LoadString(S,Style);
    LoadFontData(S,FD);
   end;
 end;

begin
 inherited;
 LoadState(NUState);
 LoadState(NFState);
 LoadState(HUState);
 LoadState(HFState);
 LoadState(RUState);
 //
 Load8Flags(S,SetTextLeftOffset,SetTextTopOffset,SetTextRightOffset,
              SetTextBottomOffset,B,B,B,B);
 if SetTextLeftOffset then
  LoadPackedInteger(S,TextLeftOffset);
 if SetTextTopOffset then
  LoadPackedInteger(S,TextTopOffset);
 if SetTextRightOffset then
  LoadPackedInteger(S,TextRightOffset);
 if SetTextBottomOffset then
  LoadPackedInteger(S,TextBottomOffset);
 //
 LoadTransformEffectData(S,HInEditEffect);
 LoadTransformEffectData(S,HOutEditEffect);
 LoadTransformEffectData(S,EnableEditEffect);
 LoadTransformEffectData(S,DisableEditEffect);
 LoadTransformEffectData(S,FocusEditEffect);
 LoadTransformEffectData(S,UnfocusEditEffect);
end;

{ TSXSkinFormStyle }

procedure TSXSkinFormStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinFormStyle then
  begin
   NUState:=TSXSkinFormStyle(Style).NUState;
   NFState:=TSXSkinFormStyle(Style).NFState;
   MUState:=TSXSkinFormStyle(Style).MUState;
   MFState:=TSXSkinFormStyle(Style).MFState;
   XUState:=TSXSkinFormStyle(Style).XUState;
   XFState:=TSXSkinFormStyle(Style).XFState;
   //
   SetCaptionHeight:=TSXSkinFormStyle(Style).SetCaptionHeight;
   SetLeftFrameWidth:=TSXSkinFormStyle(Style).SetLeftFrameWidth;
   SetRightFrameWidth:=TSXSkinFormStyle(Style).SetRightFrameWidth;
   SetBottomFrameHeight:=TSXSkinFormStyle(Style).SetBottomFrameHeight;
   //
   CaptionHeight:=TSXSkinFormStyle(Style).CaptionHeight;
   LeftFrameWidth:=TSXSkinFormStyle(Style).LeftFrameWidth;
   RightFrameWidth:=TSXSkinFormStyle(Style).RightFrameWidth;
   BottomFrameHeight:=TSXSkinFormStyle(Style).BottomFrameHeight;
   //
   ZipFilePath:=TSXSkinFormStyle(Style).ZipFilePath;
   SkinFilePath:=TSXSkinFormStyle(Style).SkinFilePath;
  end;
end;

procedure TSXSkinFormStyle.SetParameter(const Name,Value:String);
var FParam:PSXSkinFormStateParam;
         S:String;
begin
 FParam:=nil;
 S:=Copy(Name,1,3);
 if S='NU_' then FParam:=@NUState else
  if S='NF_' then FParam:=@NFState else
  if S='MU_' then FParam:=@MUState else
  if S='MF_' then FParam:=@MFState else
  if S='XU_' then FParam:=@XUState else
  if S='XF_' then FParam:=@XFState;
 if FParam=nil then
  begin
   if Name='CaptionHeight' then
    begin
     CaptionHeight:=Value;
     SetCaptionHeight:=True;
    end else
   if Name='LeftFrameWidth' then
    begin
     LeftFrameWidth:=StrToIntDef(Value,0);
     SetLeftFrameWidth:=True;
    end else
   if Name='RightFrameWidth' then
    begin
     RightFrameWidth:=StrToIntDef(Value,0);
     SetRightFrameWidth:=True;
    end else
   if Name='BottomFrameHeight' then
    begin
     BottomFrameHeight:=StrToIntDef(Value,0);
     SetBottomFrameHeight:=True;
    end;
  end else
   begin
    S:=Copy(Name,4,MaxInt);
    if not TryToSetFontDataParameter(S,Value,FParam.TextFont) then
    if S='MaskLeftWidth' then
     begin
      FParam.MaskLeftWidth:=StrToIntDef(Value,0);
      FParam.SetMaskLeftWidth:=True;
     end else
    if S='MaskRightWidth' then
     begin
      FParam.MaskRightWidth:=StrToIntDef(Value,0);
      FParam.SetMaskRightWidth:=True;
     end else
    if S='MaskTopHeight' then
     begin
      FParam.MaskTopHeight:=StrToIntDef(Value,0);
      FParam.SetMaskTopHeight:=True;
     end else
    if S='MaskBottomHeight' then
     begin
      FParam.MaskBottomHeight:=StrToIntDef(Value,0);
      FParam.SetMaskBottomHeight:=True;
     end else
    if S='MaskTopLeft' then
     begin
      FParam.MaskTopLeft:=Value;
      FParam.SetMaskTopLeft:=True;
     end else
    if S='MaskTopRight' then
     begin
      FParam.MaskTopRight:=Value;
      FParam.SetMaskTopRight:=True;
     end else
    if S='MaskBottomLeft' then
     begin
      FParam.MaskBottomLeft:=Value;
      FParam.SetMaskBottomLeft:=True;
     end else
    if S='MaskBottomRight' then
     begin
      FParam.MaskBottomRight:=Value;
      FParam.SetMaskBottomRight:=True;
     end else
    if S='MaskLeft' then
     begin
      FParam.MaskLeft:=Value;
      FParam.SetMaskLeft:=True;
     end else
    if S='MaskRight' then
     begin
      FParam.MaskRight:=Value;
      FParam.SetMaskRight:=True;
     end else
    if S='MaskTop' then
     begin
      FParam.MaskTop:=Value;
      FParam.SetMaskTop:=True;
     end else
    if S='MaskBottom' then
     begin
      FParam.MaskBottom:=Value;
      FParam.SetMaskBottom:=True;
     end else
    if S='FullMask' then
     begin
      FParam.FullMask:=Value;
      FParam.SetFullMask:=True;
     end else
    if S='ResizeTopLeft' then
     begin
      FParam.ResizeTopLeft:=Value;
      FParam.SetResizeTopLeft:=True;
     end else
    if S='ResizeTopRight' then
     begin
      FParam.ResizeTopRight:=Value;
      FParam.SetResizeTopRight:=True;
     end else
    if S='ResizeBottomLeft' then
     begin
      FParam.ResizeBottomLeft:=Value;
      FParam.SetResizeBottomLeft:=True;
     end else
    if S='ResizeBottomRight' then
     begin
      FParam.ResizeBottomRight:=Value;
      FParam.SetResizeBottomRight:=True;
     end else
    if S='ResizeLeft' then
     begin
      FParam.ResizeLeft:=Value;
      FParam.SetResizeLeft:=True;
     end else
    if S='ResizeRight' then
     begin
      FParam.ResizeRight:=Value;
      FParam.SetResizeRight:=True;
     end else
    if S='ResizeTop' then
     begin
      FParam.ResizeTop:=Value;
      FParam.SetResizeTop:=True;
     end else
    if S='ResizeBottom' then
     begin
      FParam.ResizeBottom:=Value;
      FParam.SetResizeBottom:=True;
     end else
    if S='CaptionRegion' then
     begin
      FParam.CaptionRegion:=Value;
      FParam.SetCaptionRegion:=True;
     end else
    if S='Caption' then
     begin
      FParam.CaptionStyle:=Value;
      FParam.SetCaptionStyle:=True;
     end else
    if S='LeftFrame' then
     begin
      FParam.LeftFrameStyle:=Value;
      FParam.SetLeftFrameStyle:=True;
     end else
    if S='RightFrame' then
     begin
      FParam.RightFrameStyle:=Value;
      FParam.SetRightFrameStyle:=True;
     end else
    if S='BottomFrame' then
     begin
      FParam.BottomFrameStyle:=Value;
      FParam.SetBottomFrameStyle:=True;
     end else
    if S='IconRect' then
     begin
      FParam.IconRect:=Value;
      FParam.SetIconRect:=True;
     end else
    if S='TextRect' then
     begin
      FParam.TextRect:=Value;
      FParam.SetTextRect:=True;
     end else
    if S='CloseRect' then
     begin
      FParam.CloseRect:=Value;
      FParam.SetCloseRect:=True;
     end else
    if S='MaximizeRect' then
     begin
      FParam.MaximizeRect:=Value;
      FParam.SetMaximizeRect:=True;
     end else
    if S='MinimizeRect' then
     begin
      FParam.MinimizeRect:=Value;
      FParam.SetMinimizeRect:=True;
     end else
    if S='HelpRect' then
     begin
      FParam.HelpRect:=Value;
      FParam.SetHelpRect:=True;
     end else
    if S='CloseButton' then
     begin
      FParam.CloseButton:=Value;
      FParam.SetCloseButton:=True;
     end else
    if S='MaximizeButton' then
     begin
      FParam.MaximizeButton:=Value;
      FParam.SetMaximizeButton:=True;
     end else
    if S='MinimizeButton' then
     begin
      FParam.MinimizeButton:=Value;
      FParam.SetMinimizeButton:=True;
     end else
    if S='RestoreButton' then
     begin
      FParam.RestoreButton:=Value;
      FParam.SetRestoreButton:=True;
     end else
    if S='HelpButton' then
     begin
      FParam.HelpButton:=Value;
      FParam.SetHelpButton:=True;
     end else
    if S='TextAlignment' then
     begin
      FParam.TextAlignment:=GetAlignmentFromString(Value);
      FParam.SetTextAlignment:=True;
     end;
   end;
end;

procedure TSXSkinFormStyle.GetCurrentFState(var FState:TSXSkinFormStateParam;
           Maximized,Minimized,Focused:Boolean);

 procedure SetFStateFrom(T:TSXSkinFormStateParam);
 begin
  if not FState.SetMaskLeftWidth and T.SetMaskLeftWidth then
   begin
    FState.SetMaskLeftWidth:=True;
    FState.MaskLeftWidth:=T.MaskLeftWidth;
   end;
  if not FState.SetMaskRightWidth and T.SetMaskRightWidth then
   begin
    FState.SetMaskRightWidth:=True;
    FState.MaskRightWidth:=T.MaskRightWidth;
   end;
  if not FState.SetMaskTopHeight and T.SetMaskTopHeight then
   begin
    FState.SetMaskTopHeight:=True;
    FState.MaskTopHeight:=T.MaskTopHeight;
   end;
  if not FState.SetMaskBottomHeight and T.SetMaskBottomHeight then
   begin
    FState.SetMaskBottomHeight:=True;
    FState.MaskBottomHeight:=T.MaskBottomHeight;
   end;
  //
  if not FState.SetMaskTopLeft and T.SetMaskTopLeft then
   begin
    FState.SetMaskTopLeft:=True;
    FState.MaskTopLeft:=T.MaskTopLeft;
   end;
  if not FState.SetMaskTopRight and T.SetMaskTopRight then
   begin
    FState.SetMaskTopRight:=True;
    FState.MaskTopRight:=T.MaskTopRight;
   end;
  if not FState.SetMaskBottomLeft and T.SetMaskBottomLeft then
   begin
    FState.SetMaskBottomLeft:=True;
    FState.MaskBottomLeft:=T.MaskBottomLeft;
   end;
  if not FState.SetMaskBottomRight and T.SetMaskBottomRight then
   begin
    FState.SetMaskBottomRight:=True;
    FState.MaskBottomRight:=T.MaskBottomRight;
   end;
  if not FState.SetMaskLeft and T.SetMaskLeft then
   begin
    FState.SetMaskLeft:=True;
    FState.MaskLeft:=T.MaskLeft;
   end;
  if not FState.SetMaskRight and T.SetMaskRight then
   begin
    FState.SetMaskRight:=True;
    FState.MaskRight:=T.MaskRight;
   end;
  if not FState.SetMaskTop and T.SetMaskTop then
   begin
    FState.SetMaskTop:=True;
    FState.MaskTop:=T.MaskTop;
   end;
  if not FState.SetMaskBottom and T.SetMaskBottom then
   begin
    FState.SetMaskBottom:=True;
    FState.MaskBottom:=T.MaskBottom;
   end;
  if not FState.SetFullMask and T.SetFullMask then
   begin
    FState.SetFullMask:=True;
    FState.FullMask:=T.FullMask;
   end;
  //
  if not FState.SetResizeTopLeft and T.SetResizeTopLeft then
   begin
    FState.SetResizeTopLeft:=True;
    FState.ResizeTopLeft:=T.ResizeTopLeft;
   end;
  if not FState.SetResizeTopRight and T.SetResizeTopRight then
   begin
    FState.SetResizeTopRight:=True;
    FState.ResizeTopRight:=T.ResizeTopRight;
   end;
  if not FState.SetResizeBottomLeft and T.SetResizeBottomLeft then
   begin
    FState.SetResizeBottomLeft:=True;
    FState.ResizeBottomLeft:=T.ResizeBottomLeft;
   end;
  if not FState.SetResizeBottomRight and T.SetResizeBottomRight then
   begin
    FState.SetResizeBottomRight:=True;
    FState.ResizeBottomRight:=T.ResizeBottomRight;
   end;
  if not FState.SetResizeLeft and T.SetResizeLeft then
   begin
    FState.SetResizeLeft:=True;
    FState.ResizeLeft:=T.ResizeLeft;
   end;
  if not FState.SetResizeRight and T.SetResizeRight then
   begin
    FState.SetResizeRight:=True;
    FState.ResizeRight:=T.ResizeRight;
   end;
  if not FState.SetResizeTop and T.SetResizeTop then
   begin
    FState.SetResizeTop:=True;
    FState.ResizeTop:=T.ResizeTop;
   end;
  if not FState.SetResizeBottom and T.SetResizeBottom then
   begin
    FState.SetResizeBottom:=True;
    FState.ResizeBottom:=T.ResizeBottom;
   end;
  //
  if not FState.SetCaptionRegion and T.SetCaptionRegion then
   begin
    FState.SetCaptionRegion:=True;
    FState.CaptionRegion:=T.CaptionRegion;
   end;
  if not FState.SetCaptionStyle and T.SetCaptionStyle then
   begin
    FState.SetCaptionStyle:=True;
    FState.CaptionStyle:=T.CaptionStyle;
   end;
  if not FState.SetLeftFrameStyle and T.SetLeftFrameStyle then
   begin
    FState.SetLeftFrameStyle:=True;
    FState.LeftFrameStyle:=T.LeftFrameStyle;
   end;
  if not FState.SetRightFrameStyle and T.SetRightFrameStyle then
   begin
    FState.SetRightFrameStyle:=True;
    FState.RightFrameStyle:=T.RightFrameStyle;
   end;
  if not FState.SetBottomFrameStyle and T.SetBottomFrameStyle then
   begin
    FState.SetBottomFrameStyle:=True;
    FState.BottomFrameStyle:=T.BottomFrameStyle;
   end;
  //
  if not FState.SetIconRect and T.SetIconRect then
   begin
    FState.SetIconRect:=True;
    FState.IconRect:=T.IconRect;
   end;
  if not FState.SetTextRect and T.SetTextRect then
   begin
    FState.SetTextRect:=True;
    FState.TextRect:=T.TextRect;
   end;
  if not FState.SetCloseRect and T.SetCloseRect then
   begin
    FState.SetCloseRect:=True;
    FState.CloseRect:=T.CloseRect;
   end;
  if not FState.SetMaximizeRect and T.SetMaximizeRect then
   begin
    FState.SetMaximizeRect:=True;
    FState.MaximizeRect:=T.MaximizeRect;
   end;
  if not FState.SetMinimizeRect and T.SetMinimizeRect then
   begin
    FState.SetMinimizeRect:=True;
    FState.MinimizeRect:=T.MinimizeRect;
   end;
  if not FState.SetHelpRect and T.SetHelpRect then
   begin
    FState.SetHelpRect:=True;
    FState.HelpRect:=T.HelpRect;
   end;
  //
  if not FState.SetCloseButton and T.SetCloseButton then
   begin
    FState.SetCloseButton:=True;
    FState.CloseButton:=T.CloseButton;
   end;
  if not FState.SetMaximizeButton and T.SetMaximizeButton then
   begin
    FState.SetMaximizeButton:=True;
    FState.MaximizeButton:=T.MaximizeButton;
   end;
  if not FState.SetMinimizeButton and T.SetMinimizeButton then
   begin
    FState.SetMinimizeButton:=True;
    FState.MinimizeButton:=T.MinimizeButton;
   end;
  if not FState.SetRestoreButton and T.SetRestoreButton then
   begin
    FState.SetRestoreButton:=True;
    FState.RestoreButton:=T.RestoreButton;
   end;
  if not FState.SetHelpButton and T.SetHelpButton then
   begin
    FState.SetHelpButton:=True;
    FState.HelpButton:=T.HelpButton;
   end;
  //
  if not FState.SetTextAlignment and T.SetTextAlignment then
   begin
    FState.SetTextAlignment:=True;
    FState.TextAlignment:=T.TextAlignment;
   end;
  //
  AddFontData(FState.TextFont,T.TextFont);
 end;

begin
 Finalize(FState);
 FillChar(FState,sizeof(FState),0);
 if Maximized then
  begin
   if Focused then
    begin
     SetFStateFrom(XFState);
     SetFStateFrom(XUState);
     SetFStateFrom(NFState);
     SetFStateFrom(NUState);
    end else
     begin
      SetFStateFrom(XUState);
      SetFStateFrom(NUState);
     end;
  end else
 if Minimized then
  begin
   if Focused then
    begin
     SetFStateFrom(MFState);
     SetFStateFrom(MUState);
     SetFStateFrom(NFState);
     SetFStateFrom(NUState);
    end else
     begin
      SetFStateFrom(MUState);
      SetFStateFrom(NUState);
     end;
  end else
   begin
    if Focused then
     begin
      SetFStateFrom(NFState);
      SetFStateFrom(NUState);
     end else SetFStateFrom(NUState);
   end;
end;

procedure TSXSkinFormStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinFormStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetMaskLeftWidth,SetMaskRightWidth,SetMaskTopHeight,
                 SetMaskBottomHeight,SetMaskTopLeft,SetMaskTopRight,
                 SetMaskBottomLeft,SetMaskBottomRight);
    Save8Flags(S,SetMaskLeft,SetMaskRight,SetMaskTop,SetMaskBottom,
                 SetFullMask,SetCaptionRegion,False,False);
    Save8Flags(S,SetResizeTopLeft,SetResizeTopRight,SetResizeBottomLeft,
                 SetResizeBottomRight,SetResizeLeft,SetResizeRight,
                 SetResizeTop,SetResizeBottom);
    Save8Flags(S,SetCaptionStyle,SetLeftFrameStyle,SetRightFrameStyle,
                 SetBottomFrameStyle,SetIconRect,SetTextRect,SetTextAlignment,
                 SetCloseRect);
    Save8Flags(S,SetMaximizeRect,SetMinimizeRect,SetHelpRect,SetCloseButton,
                 SetMaximizeButton,SetMinimizeButton,SetRestoreButton,
                 SetHelpButton);
    if SetMaskLeftWidth then
     S.Write(MaskLeftWidth,sizeof(MaskLeftWidth));
    if SetMaskRightWidth then
     S.Write(MaskRightWidth,sizeof(MaskRightWidth));
    if SetMaskTopHeight then
     S.Write(MaskTopHeight,sizeof(MaskTopHeight));
    if SetMaskBottomHeight then
     S.Write(MaskBottomHeight,sizeof(MaskBottomHeight));
    //
    if SetMaskTopLeft then
     SaveString(S,MaskTopLeft);
    if SetMaskTopRight then
     SaveString(S,MaskTopRight);
    if SetMaskBottomLeft then
     SaveString(S,MaskBottomLeft);
    if SetMaskBottomRight then
     SaveString(S,MaskBottomRight);
    if SetMaskLeft then
     SaveString(S,MaskLeft);
    if SetMaskRight then
     SaveString(S,MaskRight);
    if SetMaskTop then
     SaveString(S,MaskTop);
    if SetMaskBottom then
     SaveString(S,MaskBottom);
    if SetFullMask then
     SaveString(S,FullMask);
    //
    if SetResizeTopLeft then
     SaveString(S,ResizeTopLeft);
    if SetResizeTopRight then
     SaveString(S,ResizeTopRight);
    if SetResizeBottomLeft then
     SaveString(S,ResizeBottomLeft);
    if SetResizeBottomRight then
     SaveString(S,ResizeBottomRight);
    if SetResizeLeft then
     SaveString(S,ResizeLeft);
    if SetResizeRight then
     SaveString(S,ResizeRight);
    if SetResizeTop then
     SaveString(S,ResizeTop);
    if SetResizeBottom then
     SaveString(S,ResizeBottom);
    //
    if SetCaptionRegion then
     SaveString(S,CaptionRegion);
    if SetCaptionStyle then
     SaveString(S,CaptionStyle);
    if SetLeftFrameStyle then
     SaveString(S,LeftFrameStyle);
    if SetRightFrameStyle then
     SaveString(S,RightFrameStyle);
    if SetBottomFrameStyle then
     SaveString(S,BottomFrameStyle);
    //
    if SetIconRect then
     SaveString(S,IconRect);
    if SetTextRect then
     SaveString(S,TextRect);
    if SetCloseRect then
     SaveString(S,CloseRect);
    if SetMaximizeRect then
     SaveString(S,MaximizeRect);
    if SetMinimizeRect then
     SaveString(S,MinimizeRect);
    if SetHelpRect then
     SaveString(S,HelpRect);
    if SetTextAlignment then
     S.Write(TextAlignment,sizeof(TextAlignment));
    //
    if SetCloseButton then
     SaveString(S,CloseButton);
    if SetMaximizeButton then
     SaveString(S,MaximizeButton);
    if SetMinimizeButton then
     SaveString(S,MinimizeButton);
    if SetRestoreButton then
     SaveString(S,RestoreButton);
    if SetHelpButton then
     SaveString(S,HelpButton);
    //
    SaveFontData(S,State.TextFont);
   end;
 end;

begin
 inherited;
 SaveState(NUState);
 SaveState(NFState);
 SaveState(MUState);
 SaveState(MFState);
 SaveState(XUState);
 SaveState(XFState);
 //
 Save8Flags(S,SetCaptionHeight,SetLeftFrameWidth,SetRightFrameWidth,
              SetBottomFrameHeight,False,False,False,False);
 if SetCaptionHeight then
  SaveString(S,CaptionHeight);
 if SetLeftFrameWidth then
  S.Write(LeftFrameWidth,sizeof(LeftFrameWidth));
 if SetRightFrameWidth then
  S.Write(RightFrameWidth,sizeof(RightFrameWidth));
 if SetBottomFrameHeight then
  S.Write(BottomFrameHeight,sizeof(BottomFrameHeight));
end;

procedure TSXSkinFormStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;

 procedure LoadState(var State:TSXSkinFormStateParam);
 begin
  with State do
   begin
    Load8Flags(S,SetMaskLeftWidth,SetMaskRightWidth,SetMaskTopHeight,
                 SetMaskBottomHeight,SetMaskTopLeft,SetMaskTopRight,
                 SetMaskBottomLeft,SetMaskBottomRight);
    Load8Flags(S,SetMaskLeft,SetMaskRight,SetMaskTop,SetMaskBottom,
                 SetFullMask,SetCaptionRegion,B,B);
    Load8Flags(S,SetResizeTopLeft,SetResizeTopRight,SetResizeBottomLeft,
                 SetResizeBottomRight,SetResizeLeft,SetResizeRight,
                 SetResizeTop,SetResizeBottom);
    Load8Flags(S,SetCaptionStyle,SetLeftFrameStyle,SetRightFrameStyle,
                 SetBottomFrameStyle,SetIconRect,SetTextRect,SetTextAlignment,
                 SetCloseRect);
    Load8Flags(S,SetMaximizeRect,SetMinimizeRect,SetHelpRect,SetCloseButton,
                 SetMaximizeButton,SetMinimizeButton,SetRestoreButton,
                 SetHelpButton);
    if SetMaskLeftWidth then
     S.Read(MaskLeftWidth,sizeof(MaskLeftWidth));
    if SetMaskRightWidth then
     S.Read(MaskRightWidth,sizeof(MaskRightWidth));
    if SetMaskTopHeight then
     S.Read(MaskTopHeight,sizeof(MaskTopHeight));
    if SetMaskBottomHeight then
     S.Read(MaskBottomHeight,sizeof(MaskBottomHeight));
    //
    if SetMaskTopLeft then
     LoadString(S,MaskTopLeft);
    if SetMaskTopRight then
     LoadString(S,MaskTopRight);
    if SetMaskBottomLeft then
     LoadString(S,MaskBottomLeft);
    if SetMaskBottomRight then
     LoadString(S,MaskBottomRight);
    if SetMaskLeft then
     LoadString(S,MaskLeft);
    if SetMaskRight then
     LoadString(S,MaskRight);
    if SetMaskTop then
     LoadString(S,MaskTop);
    if SetMaskBottom then
     LoadString(S,MaskBottom);
    if SetFullMask then
     LoadString(S,FullMask);
    //
    if SetResizeTopLeft then
     LoadString(S,ResizeTopLeft);
    if SetResizeTopRight then
     LoadString(S,ResizeTopRight);
    if SetResizeBottomLeft then
     LoadString(S,ResizeBottomLeft);
    if SetResizeBottomRight then
     LoadString(S,ResizeBottomRight);
    if SetResizeLeft then
     LoadString(S,ResizeLeft);
    if SetResizeRight then
     LoadString(S,ResizeRight);
    if SetResizeTop then
     LoadString(S,ResizeTop);
    if SetResizeBottom then
     LoadString(S,ResizeBottom);
    //
    if SetCaptionRegion then
     LoadString(S,CaptionRegion);
    if SetCaptionStyle then
     LoadString(S,CaptionStyle);
    if SetLeftFrameStyle then
     LoadString(S,LeftFrameStyle);
    if SetRightFrameStyle then
     LoadString(S,RightFrameStyle);
    if SetBottomFrameStyle then
     LoadString(S,BottomFrameStyle);
    //
    if SetIconRect then
     LoadString(S,IconRect);
    if SetTextRect then
     LoadString(S,TextRect);
    if SetCloseRect then
     LoadString(S,CloseRect);
    if SetMaximizeRect then
     LoadString(S,MaximizeRect);
    if SetMinimizeRect then
     LoadString(S,MinimizeRect);
    if SetHelpRect then
     LoadString(S,HelpRect);
    if SetTextAlignment then
     S.Read(TextAlignment,sizeof(TextAlignment));
    //
    if SetCloseButton then
     LoadString(S,CloseButton);
    if SetMaximizeButton then
     LoadString(S,MaximizeButton);
    if SetMinimizeButton then
     LoadString(S,MinimizeButton);
    if SetRestoreButton then
     LoadString(S,RestoreButton);
    if SetHelpButton then
     LoadString(S,HelpButton);
    //
    LoadFontData(S,State.TextFont);
   end;
 end;

begin
 inherited;
 Self.ZipFilePath:=ZipFilePath;
 SkinFilePath:=RootPath;
 LoadState(NUState);
 LoadState(NFState);
 LoadState(MUState);
 LoadState(MFState);
 LoadState(XUState);
 LoadState(XFState);
 //
 Load8Flags(S,SetCaptionHeight,SetLeftFrameWidth,SetRightFrameWidth,
              SetBottomFrameHeight,B,B,B,B);
 if SetCaptionHeight then
  LoadString(S,CaptionHeight);
 if SetLeftFrameWidth then
  S.Read(LeftFrameWidth,sizeof(LeftFrameWidth));
 if SetRightFrameWidth then
  S.Read(RightFrameWidth,sizeof(RightFrameWidth));
 if SetBottomFrameHeight then
  S.Read(BottomFrameHeight,sizeof(BottomFrameHeight));
end;

{ TSXSkinUpDownStyle }

procedure TSXSkinUpDownStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinUpDownStyle then
  begin
   NUState:=TSXSkinUpDownStyle(Style).NUState;
   NFState:=TSXSkinUpDownStyle(Style).NFState;
   HUState:=TSXSkinUpDownStyle(Style).HUState;
   HFState:=TSXSkinUpDownStyle(Style).HFState;
   DFState:=TSXSkinUpDownStyle(Style).DFState;
   RUState:=TSXSkinUpDownStyle(Style).RUState;
   //
   HInUpDownEffect:=TSXSkinUpDownStyle(Style).HInUpDownEffect;
   HOutUpDownEffect:=TSXSkinUpDownStyle(Style).HOutUpDownEffect;
   DownUpDownEffect:=TSXSkinUpDownStyle(Style).DownUpDownEffect;
   UpUpDownEffect:=TSXSkinUpDownStyle(Style).UpUpDownEffect;
   EnableUpDownEffect:=TSXSkinUpDownStyle(Style).EnableUpDownEffect;
   DisableUpDownEffect:=TSXSkinUpDownStyle(Style).DisableUpDownEffect;
   FocusUpDownEffect:=TSXSkinUpDownStyle(Style).FocusUpDownEffect;
   UnfocusUpDownEffect:=TSXSkinUpDownStyle(Style).UnfocusUpDownEffect;
   //
   SetUpButton:=TSXSkinUpDownStyle(Style).SetUpButton;
   SetDownButton:=TSXSkinUpDownStyle(Style).SetDownButton;
   SetUpButtonRect:=TSXSkinUpDownStyle(Style).SetUpButtonRect;
   SetDownButtonRect:=TSXSkinUpDownStyle(Style).SetDownButtonRect;
   //
   UpButton:=TSXSkinUpDownStyle(Style).UpButton;
   DownButton:=TSXSkinUpDownStyle(Style).DownButton;
   UpButtonRect:=TSXSkinUpDownStyle(Style).UpButtonRect;
   DownButtonRect:=TSXSkinUpDownStyle(Style).DownButtonRect;
  end;
end;

procedure TSXSkinUpDownStyle.SetParameter(const Name,Value:String);
var FParam:PSXSkinUpDownStateParam;
         S:String;
begin
 FParam:=nil;
 S:=Copy(Name,1,3);
 if S='NU_' then FParam:=@NUState else
  if S='NF_' then FParam:=@NFState else
  if S='HU_' then FParam:=@HUState else
  if S='HF_' then FParam:=@HFState else
  if S='DF_' then FParam:=@DFState else
  if S='RU_' then FParam:=@RUState;
 if FParam=nil then
  begin
   if not TryToSetTransformEffectDataParameter(Name,Value,HInUpDownEffect,'HighlightInUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,HOutUpDownEffect,'HighlightOutUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DownUpDownEffect,'DownUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UpUpDownEffect,'UpUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,EnableUpDownEffect,'EnableUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,DisableUpDownEffect,'DisableUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,FocusUpDownEffect,'FocusUpDown') then
    if not TryToSetTransformEffectDataParameter(Name,Value,UnfocusUpDownEffect,'UnfocusUpDown') then
    //
    if Name='UpButton' then
     begin
      UpButton:=Value;
      SetUpButton:=True;
     end else
    if Name='DownButton' then
     begin
      DownButton:=Value;
      SetDownButton:=True;
     end else
    if Name='UpButtonRect' then
     begin
      UpButtonRect:=Value;
      SetUpButtonRect:=True;
     end else
    if Name='DownButtonRect' then
     begin
      DownButtonRect:=Value;
      SetDownButtonRect:=True;
     end;
  end else
   begin
    S:=Copy(Name,4,MaxInt);
    if S='Style' then
     begin
      FParam.Style:=Value;
      FParam.SetStyle:=True;
     end else
    if S='OverStyle' then
     begin
      FParam.OverStyle:=Value;
      FParam.SetOverStyle:=True;
     end;
   end;
end;

procedure TSXSkinUpDownStyle.GetCurrentUDState(var UDState:TSXSkinUpDownStateParam;
              Enabled,MouseOver,Down,Focused:Boolean);

 procedure SetUDStateFrom(T:TSXSkinUpDownStateParam);
 begin
  if not UDState.SetStyle and T.SetStyle then
   begin
    UDState.SetStyle:=True;
    UDState.Style:=T.Style;
   end;
  if not UDState.SetOverStyle and T.SetOverStyle then
   begin
    UDState.SetOverStyle:=True;
    UDState.OverStyle:=T.OverStyle;
   end;
 end;

begin
 Finalize(UDState);
 FillChar(UDState,sizeof(UDState),0);
 if not Enabled then
  begin
   SetUDStateFrom(RUState);
   SetUDStateFrom(NUState);
  end else
 if Down then
  begin
   SetUDStateFrom(DFState);
   SetUDStateFrom(NFState);
   SetUDStateFrom(NUState);
  end else
 if MouseOver then
  begin
   if Focused then
    begin
     SetUDStateFrom(HFState);
     SetUDStateFrom(HUState);
     SetUDStateFrom(NFState);
     SetUDStateFrom(NUState);
    end else
     begin
      SetUDStateFrom(HUState);
      SetUDStateFrom(NUState);
     end;
  end else
   begin
    if Focused then
     begin
      SetUDStateFrom(NFState);
      SetUDStateFrom(NUState);
     end else
      SetUDStateFrom(NUState);
   end;
end;

procedure TSXSkinUpDownStyle.SaveToStream(S:TStream;const RootPath:String);

 procedure SaveState(const State:TSXSkinUpDownStateParam);
 begin
  with State do
   begin
    Save8Flags(S,SetStyle,SetOverStyle,False,False,False,False,False,False);
    if SetStyle then
     SaveString(S,Style);
    if SetOverStyle then
     SaveString(S,OverStyle);
   end;
 end;

begin
 inherited;
 SaveState(NUState);
 SaveState(NFState);
 SaveState(HUState);
 SaveState(HFState);
 SaveState(DFState);
 SaveState(RUState);
 //
 SaveTransformEffectData(S,HInUpDownEffect);
 SaveTransformEffectData(S,HOutUpDownEffect);
 SaveTransformEffectData(S,DownUpDownEffect);
 SaveTransformEffectData(S,UpUpDownEffect);
 SaveTransformEffectData(S,EnableUpDownEffect);
 SaveTransformEffectData(S,DisableUpDownEffect);
 SaveTransformEffectData(S,FocusUpDownEffect);
 SaveTransformEffectData(S,UnfocusUpDownEffect);
 //
 Save8Flags(S,SetUpButton,SetDownButton,SetUpButtonRect,SetDownButtonRect,
              False,False,False,False);
 if SetUpButton then
  SaveString(S,UpButton);
 if SetDownButton then
  SaveString(S,DownButton);
 if SetUpButtonRect then
  SaveString(S,UpButtonRect);
 if SetDownButtonRect then
  SaveString(S,DownButtonRect);
end;

procedure TSXSkinUpDownStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;

 procedure LoadState(var State:TSXSkinUpDownStateParam);
 begin
  with State do
   begin
    Load8Flags(S,SetStyle,SetOverStyle,B,B,B,B,B,B);
    if SetStyle then
     LoadString(S,Style);
    if SetOverStyle then
     LoadString(S,OverStyle);
   end;
 end;

begin
 inherited;
 LoadState(NUState);
 LoadState(NFState);
 LoadState(HUState);
 LoadState(HFState);
 LoadState(DFState);
 LoadState(RUState);
 //
 LoadTransformEffectData(S,HInUpDownEffect);
 LoadTransformEffectData(S,HOutUpDownEffect);
 LoadTransformEffectData(S,DownUpDownEffect);
 LoadTransformEffectData(S,UpUpDownEffect);
 LoadTransformEffectData(S,EnableUpDownEffect);
 LoadTransformEffectData(S,DisableUpDownEffect);
 LoadTransformEffectData(S,FocusUpDownEffect);
 LoadTransformEffectData(S,UnfocusUpDownEffect);
 //
 Load8Flags(S,SetUpButton,SetDownButton,SetUpButtonRect,SetDownButtonRect,B,B,B,B);
 if SetUpButton then
  LoadString(S,UpButton);
 if SetDownButton then
  LoadString(S,DownButton);
 if SetUpButtonRect then
  LoadString(S,UpButtonRect);
 if SetDownButtonRect then
  LoadString(S,DownButtonRect);
end;

{ TSXSkinSpinEditStyle }

procedure TSXSkinSpinEditStyle.AssignStyle(Style:TSXSkinStyle);
begin
 inherited;
 if Style is TSXSkinSpinEditStyle then
  begin
   SetEdit:=TSXSkinSpinEditStyle(Style).SetEdit;
   SetUpDown:=TSXSkinSpinEditStyle(Style).SetUpDown;
   SetEditRect:=TSXSkinSpinEditStyle(Style).SetEditRect;
   SetUpDownRect:=TSXSkinSpinEditStyle(Style).SetUpDownRect;
   //
   Edit:=TSXSkinSpinEditStyle(Style).Edit;
   UpDown:=TSXSkinSpinEditStyle(Style).UpDown;
   EditRect:=TSXSkinSpinEditStyle(Style).EditRect;
   UpDownRect:=TSXSkinSpinEditStyle(Style).UpDownRect;
  end;
end;

procedure TSXSkinSpinEditStyle.SetParameter(const Name,Value:String);
begin
 if Name='Edit' then
  begin
   Edit:=Value;
   SetEdit:=True;
  end else
 if Name='UpDown' then
  begin
   UpDown:=Value;
   SetUpDown:=True;
  end else
 if Name='EditRect' then
  begin
   EditRect:=Value;
   SetEditRect:=True;
  end else
 if Name='UpDownRect' then
  begin
   UpDownRect:=Value;
   SetUpDownRect:=True;
  end;
end;

procedure TSXSkinSpinEditStyle.SaveToStream(S:TStream;const RootPath:String);
begin
 inherited;
 Save8Flags(S,SetEdit,SetUpDown,SetEditRect,SetUpDownRect,False,False,False,False);
 if SetEdit then
  SaveString(S,Edit);
 if SetUpDown then
  SaveString(S,UpDown);
 if SetEditRect then
  SaveString(S,EditRect);
 if SetUpDownRect then
  SaveString(S,UpDownRect);
end;

procedure TSXSkinSpinEditStyle.LoadFromStream(S:TStream;Version:Integer;
           const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList);
var B:Boolean;
begin
 inherited;
 Load8Flags(S,SetEdit,SetUpDown,SetEditRect,SetUpDownRect,B,B,B,B);
 if SetEdit then
  LoadString(S,Edit);
 if SetUpDown then
  LoadString(S,UpDown);
 if SetEditRect then
  LoadString(S,EditRect);
 if SetUpDownRect then
  LoadString(S,UpDownRect);
end;

{ TSXSkinCustomLibrary }

function TSXSkinCustomLibrary.GetCanBeUsed:Boolean;
begin
 Result:=FIsLoaded and (FActive or (csDesigning in ComponentState));
end;

procedure TSXSkinCustomLibrary.LoadFromINIFile(const FilePath:String);
var IniFile:TIniFile;
  SkinStyle:TSXSkinStyle;
     GStyle:TSXSkinGeneralStyle;
    Element:TSXSkinStyleElement;
 SL,SL2,SL3:TStringList;
      A,B,C:Integer;
       S,S2:String;
       Skin:TSXStoredSkin;
begin
 IniFile:=TIniFile.Create(FilePath);
 SL:=TStringList.Create;
 SL2:=TStringList.Create;
 try
  IniFile.ReadSections(SL);
  for A:=0 to SL.Count-1 do
   begin
    if SL[A]='BasedOn' then
     begin
      SL3:=TStringList.Create;
      try
       IniFile.ReadSectionValues(SL[A],SL3);
       for C:=0 to SL3.Count-1 do
        begin
         BasedOnList.Add(ValueFromIndex(SL3,C));
         S:=ValueFromIndex(SL3,C);
         Skin:=GetStoredSkinByZIPName(S);
         if Skin<>nil then
          LoadFromZIPStream(Skin.Stream,S) else
           begin
            S:=WithLastSlash(FSkinDir)+S;
            if FileExists(S) then
             LoadFromFile(S);
           end;
        end;
      finally
       SL3.Free;
      end;
     end else
    if SL[A]='Strings' then
     begin
      SL3:=TStringList.Create;
      try
       IniFile.ReadSectionValues(SL[A],SL3);
       for C:=0 to SL3.Count-1 do
        begin
         B:=Strings.IndexOfName(SL3.Names[C]);
         if B<0 then Strings.Add(SL3[C]) else
          SetValueFromIndex(Strings,B,ValueFromIndex(SL3,C));
        end;
      finally
       SL3.Free;
      end;
     end else
      begin
       SkinStyle:=nil;
       if Copy(SL[A],1,6)='_Label' then
        SkinStyle:=TSXSkinLabelStyle.Create else
       if Copy(SL[A],1,7)='_Button' then
        SkinStyle:=TSXSkinButtonStyle.Create else
       if Copy(SL[A],1,9)='_CheckBox' then
        SkinStyle:=TSXSkinCheckBoxStyle.Create else
       if Copy(SL[A],1,12)='_RadioButton' then
        SkinStyle:=TSXSkinRadioButtonStyle.Create else
       if Copy(SL[A],1,5)='_Edit' then
        SkinStyle:=TSXSkinEditStyle.Create else
       if Copy(SL[A],1,9)='_GroupBox' then
        SkinStyle:=TSXSkinGroupBoxStyle.Create else
       if Copy(SL[A],1,16)='_MultiStateCheck' then
        SkinStyle:=TSXSkinMultiStateCheckStyle.Create else
       if Copy(SL[A],1,11)='_MultiState' then
        SkinStyle:=TSXSkinMultiStateStyle.Create else
       if Copy(SL[A],1,10)='_Selective' then
        SkinStyle:=TSXSkinSelectiveStyle.Create else
       if Copy(SL[A],1,7)='_UpDown' then
        SkinStyle:=TSXSkinUpDownStyle.Create else
       if Copy(SL[A],1,9)='_SpinEdit' then
        SkinStyle:=TSXSkinSpinEditStyle.Create else
       if Copy(SL[A],1,5)='_Form' then
        begin
         SkinStyle:=TSXSkinFormStyle.Create;
         TSXSkinFormStyle(SkinStyle).SkinFilePath:=FilePath;
        end;
       if SkinStyle<>nil then
        begin
         SkinStyle.Name:=SL[A];
         Styles.AddUnique(SkinStyle);
         IniFile.ReadSectionValues(SL[A],SL2);
         for B:=0 to SL2.Count-1 do
          begin
           if SL2.Names[B]='BaseSkin' then
            begin
             C:=Styles.GetIndexByName(VarValue(ValueFromIndex(SL2,B)));
             if C>=0 then SkinStyle.AssignStyle(Styles[C]) else
              SkinStyle.BasedOnList.Add(VarValue(ValueFromIndex(SL2,B)));
            end else SkinStyle.SetParameter(SL2.Names[B],VarValue(ValueFromIndex(SL2,B)));
          end;
        end else
         begin
          GStyle:=TSXSkinGeneralStyle.Create;
          GStyle.Name:=SL[A];
          GStyle.SkinFilePath:=FilePath;
          Styles.AddUnique(GStyle);
          IniFile.ReadSectionValues(SL[A],SL2);
          for B:=0 to SL2.Count-1 do
           begin
            S:=VarValue(ValueFromIndex(SL2,B));
            if SL2.Names[B]='BaseSkin' then
             begin
              C:=Styles.GetIndexByName(S);
              if C>=0 then GStyle.AssignStyle(Styles[C]) else
               GStyle.BasedOnList.Add(VarValue(S));
             end else
            if ((length(SL2.Names[B])=8) or (length(SL2.Names[B])=9)) and (Copy(SL2[B],1,7)='Element') then
             begin
              C:=Pos(',',S);
              if C>1 then
               begin
                S2:=Copy(S,C+1,MaxInt);
                Element:=CreateSkinStyleElementByType(S2);
                Element.Name:=Copy(S,1,C-1);
                GStyle.Elements.Add(Element);
               end;
             end else
              begin
               S2:=SL2.Names[B];
               C:=Pos('.',S2);
               if C>1 then
                begin
                 S2:=Copy(S2,1,C-1);
                 C:=GStyle.Elements.GetIndexByName(S2);
                 if C>=0 then
                  GStyle.Elements[C].SetProperty(Copy(SL2.Names[B],length(S2)+2,MaxInt),S,FilePath);
                end else GStyle.SetParameter(S2,S);
              end;
           end;
         end;
      end;
   end;
 finally
  IniFile.Free;
  SL.Free;
  SL2.Free;
 end;
end;

procedure TSXSkinCustomLibrary.LoadFromZIPFile(const FilePath:String);
var FS:TFileStream;
begin
 FS:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromZIPStream(FS,FilePath);
 finally
  FS.Free;
 end;
end;

procedure TSXSkinCustomLibrary.LoadFromZIPStream(Stream:TStream;
           const FilePath:String);
var StrStream:TStringStream;
      ZipFile:TZipFile;
            A:Integer;
begin
 ZipFile:=PreloadZipFile(Stream,FilePath,Self);
 try
  A:=0;
  while (A<ZipFile.Count) and not SameText(ZipFile.Name[A],'skin.sxs') do Inc(A);
  if A<ZipFile.Count then
   begin
    StrStream:=TStringStream.Create(ZipFile.Data[A]);
    try
     LoadFromSXSStream(StrStream,'',FilePath);
    finally
     StrStream.Free;
    end;
   end;
 except
 end;
end;

procedure TSXSkinCustomLibrary.LoadFromSXSStream(Stream:TStream;
           const FilePath,ZipFilePath:String);
var     SL:TStringList;
         S:String;
       B,C:Integer;
   Version:Integer;
 StyleList:TSXSkinStyleList;
      Skin:TSXStoredSkin;
begin
 //Header
 SetLength(S,length(SXS_Header));
 Stream.Read(S[1],28);
 if S<>SXS_Header then exit;
 Stream.Read(Version,sizeof(Version));
 if not VersionSupported(Version) then exit;
 //Strings
 SL:=TStringList.Create;
 try
  LoadListFromStream(Stream,SL);
  for C:=0 to SL.Count-1 do
   begin
    B:=Strings.IndexOfName(SL.Names[C]);
    if B<0 then Strings.Add(SL[C]) else
     SetValueFromIndex(Strings,B,ValueFromIndex(SL,C));
   end;
 finally
  SL.Free;
 end;
 //BasedOn
 SL:=TStringList.Create;
 try
  LoadListFromStream(Stream,SL);
  for C:=0 to SL.Count-1 do
   begin
    S:=SL[C];
    Skin:=GetStoredSkinByZIPName(S);
    if Skin<>nil then
     begin
      BasedOnList.Add(S);
      Skin.Stream.Seek(0,soFromBeginning);
      LoadFromZIPStream(Skin.Stream,S);
     end else
      begin
       BasedOnList.Add(S);
       S:=WithLastSlash(FSkinDir)+S;
       if FileExists(S) then
        LoadFromFile(S);
      end;
   end;
 finally
  SL.Free;
 end;
 //Styles
 StyleList:=TSXSkinStyleList.Create;
 try
  StyleList.LoadFromStream(Stream,Version,FilePath,ZipFilePath,StyleList,Styles);
  for C:=0 to StyleList.Count-1 do
   begin
    Styles.AddUnique(StyleList[C]);
   end;
  StyleList.ClearLinks;
 finally
  StyleList.Free;
 end;
end;

procedure TSXSkinCustomLibrary.LoadFromSXSFile(const FilePath:String);
var FS:TFileStream;
    MS:TMemoryStream;
begin
 FS:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
 try
  MS:=TMemoryStream.Create;
  try
   MS.LoadFromStream(FS);
   MS.Seek(0,soFromBeginning);
   LoadFromSXSStream(MS,FilePath,'');
  finally
   MS.Free;
  end;
 finally
  FS.Free;
 end;
end;

procedure TSXSkinCustomLibrary.SaveToSXSFile(const FilePath:String);
var FS:TFileStream;
     S:String;
begin
 FS:=TFileStream.Create(FilePath,fmCreate);
 try
  //Header
  S:='SXSkinComponents Binary Skin';
  FS.Write(S[1],28);
  FS.Write(SXSkinComponents_Version,sizeof(SXSkinComponents_Version));
  //Strings
  SaveListToStream(FS,Strings);
  //BasedOn
  SaveListToStream(FS,BasedOnList);
  //Styles
  Styles.SaveToStream(FS,FilePath);
 finally
  FS.Free;
 end;
end;

procedure TSXSkinCustomLibrary.LoadFromFile(const FilePath:String);
begin
 if FilePath='' then
  begin
   Styles.Clear;
   BasedOnList.Clear;
   DeletePreloadedBitmaps(Self);
   DeletePreloadedRegions(Self);
   DeletePreloadedZipFiles(Self);
   if csDesigning in ComponentState then
    begin
     InternalSkin.Stream.Seek(0,soFromBeginning);
     LoadFromZIPStream(InternalSkin.Stream,'_internal.zip');
    end;
  end;
 if not FileExists(FilePath) then exit;
 if SameText(ExtractFileExt(FilePath),'.zip') then
  LoadFromZIPFile(FilePath) else
 if SameText(ExtractFileExt(FilePath),'.ini') then
  LoadFromINIFile(FilePath) else
   LoadFromSXSFile(FilePath);
end;

function TSXSkinCustomLibrary.CanSetActive:Boolean;
var SkinSet,Skin2Set:Boolean;
begin
 SkinSet:=False; Skin2Set:=False;
 if (FStoredSkin<>nil) or (FSkinFile<>'') then SkinSet:=True;
 if (FStoredSkin=nil) and (FSkinFile<>'') and not FileExists(GetFullSkinFile) then
  begin
   Result:=False;
   exit;
  end;
 if (FStoredSkin2<>nil) or (FSkinFile2<>'') then Skin2Set:=True;
 if (FStoredSkin2=nil) and (FSkinFile2<>'') and not FileExists(GetFullSkinFile2) then
  begin
   Result:=False;
   exit;
  end;
 Result:=SkinSet or Skin2Set; 
end;

procedure TSXSkinCustomLibrary.SetActive(Value:Boolean);
begin
 if Value<>FActive then
  begin
   if not Value or (Value and CanSetActive) then
    begin
     FActive:=Value;
     if not (csLoading in ComponentState) then
      begin
       if Value or (csDesigning in ComponentState) then
        ReloadSkinStyles(True);
      end;
    end;
  end;
end;

procedure TSXSkinCustomLibrary.SetSkinDir(const Value:String);
begin
 if Value<>FSkinDir then
  begin
   FSkinDir:=Value;
   if not CanSetActive then Active:=False;
   if not (csLoading in ComponentState) then
    ReloadSkinStyles;
  end;
end;

procedure TSXSkinCustomLibrary.SetSkinFile(const Value:String);
begin
 if (Value<>FSkinFile) or (FStoredSkin<>nil) then
  begin
   FSkinFile:=Value;
   if not (csLoading in ComponentState) and (FStoredSkin<>nil) then
    begin
     FStoredSkin.RemoveFreeNotification(Self);
     FStoredSkin:=nil;
    end;
   if not CanSetActive then Active:=False;
   if not (csLoading in ComponentState) then
    ReloadSkinStyles;
  end;
end;

procedure TSXSkinCustomLibrary.SetSkinFile2(const Value:String);
begin
 if (Value<>FSkinFile2) or (FStoredSkin2<>nil) then
  begin
   FSkinFile2:=Value;
   if not (csLoading in ComponentState) and (FStoredSkin2<>nil) then
    begin
     FStoredSkin2.RemoveFreeNotification(Self);
     FStoredSkin2:=nil;
    end;
   if not CanSetActive then Active:=False;
   if not (csLoading in ComponentState) then
    ReloadSkinStyles;
  end;
end;

procedure TSXSkinCustomLibrary.SetStoredSkin(const Value:TSXStoredSkin);
begin
 if FStoredSkin<>Value then
  begin
   if FStoredSkin<>nil then
    FStoredSkin.RemoveFreeNotification(Self);
   FStoredSkin:=Value;
   if FStoredSkin<>nil then
    FStoredSkin.FreeNotification(Self);
   if not CanSetActive then Active:=False; 
   if not (csDestroying in ComponentState) then
    ReloadSkinStyles;
  end;
end;

procedure TSXSkinCustomLibrary.SetStoredSkin2(const Value:TSXStoredSkin);
begin
 if FStoredSkin2<>Value then
  begin
   if FStoredSkin2<>nil then
    FStoredSkin2.RemoveFreeNotification(Self);
   FStoredSkin2:=Value;
   if FStoredSkin2<>nil then
    FStoredSkin2.FreeNotification(Self);
   if not CanSetActive then Active:=False; 
   if not (csDestroying in ComponentState) then
    ReloadSkinStyles;
  end;
end;

procedure TSXSkinCustomLibrary.ReloadSkinStyles(IfNotActive:Boolean=False);
begin
 if FActive or IfNotActive then
  begin
   LoadFromFile('');
   if FActive then
    begin
     if FStoredSkin=nil then
      begin
       if FSkinFile<>'' then
        LoadFromFile(GetFullSkinFile);
      end else
       begin
        FStoredSkin.FStream.Seek(0,soFromBeginning);
        LoadFromZIPStream(FStoredSkin.FStream,FStoredSkin.FileName);
       end;
     if FStoredSkin2=nil then
      begin
       if FSkinFile2<>'' then
        LoadFromFile(GetFullSkinFile2);
      end else
       begin
        FStoredSkin2.FStream.Seek(0,soFromBeginning);
        LoadFromZIPStream(FStoredSkin2.FStream,FStoredSkin2.FileName);
       end;
    end;
   InvalidateSkinComponents;
  end;
end;

procedure TSXSkinCustomLibrary.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
  begin
   if AComponent=FStoredSkin then
    FStoredSkin:=nil;
   if AComponent=FStoredSkin2 then
    FStoredSkin2:=nil;
  end;
end;

function TSXSkinCustomLibrary.VarValue(const Value:String):String;
begin
 Result:=Value;
 if (Value<>'') and (Value[1]='$') then
  Result:=GetStringsValue(Copy(Value,2,MaxInt));
end;

function TSXSkinCustomLibrary.GetFullSkinFile:String;
begin
 if FSkinFile='' then Result:='' else
 if PathIsRelative(FSkinFile) then
  begin
   if FSkinDir<>'' then
    Result:=GetFullPath(FSkinFile,WithLastSlash(FSkinDir)) else
     Result:=GetFullPath(FSkinFile,Application.ExeName);
  end else Result:=FSkinFile;
end;

function TSXSkinCustomLibrary.GetFullSkinFile2:String;
begin
 if FSkinFile2='' then Result:='' else
 if PathIsRelative(FSkinFile2) then
  begin
   if FSkinDir<>'' then
    Result:=GetFullPath(FSkinFile2,WithLastSlash(FSkinDir)) else
     Result:=GetFullPath(FSkinFile2,Application.ExeName);
  end else Result:=FSkinFile2;
end;

procedure TSXSkinCustomLibrary.AddSkinComponent(Component:TComponent);
begin
 SkinComponents.Add(Component);
end;

procedure TSXSkinCustomLibrary.RemoveSkinComponent(Component:TComponent);
begin
 SkinComponents.Remove(Component);
 RemoveSkinComponentData(Component);
end;

procedure TSXSkinCustomLibrary.RemoveSkinComponentData(Component:TComponent);
var A,B,C:Integer;
   GStyle:TSXSkinGeneralStyle;
   Figure:TSXSkinStyleFigureElement;
begin
 for A:=0 to Styles.Count-1 do
  if Styles[A] is TSXSkinGeneralStyle then
   begin
    GStyle:=TSXSkinGeneralStyle(Styles[A]);
    for B:=0 to GStyle.Elements.Count-1 do
     if GStyle.Elements[B] is TSXSkinStyleFigureElement then
      begin
       Figure:=TSXSkinStyleFigureElement(GStyle.Elements[B]);
       for C:=Figure.ControlsData.Count-1 downto 0 do
        if Figure.ControlsData[C].Control=Component then
         Figure.ControlsData.Delete(C);
      end;
   end;
end;

procedure TSXSkinCustomLibrary.InvalidateSkinComponents;
var A:Integer;
begin
 for A:=0 to SkinComponents.Count-1 do
  if TComponent(SkinComponents[A]) is TSXSkinCustomControl then
   TSXSkinCustomControl(SkinComponents[A]).SkinChanged else
  if TComponent(SkinComponents[A]) is TSXSkinCustomForm then
   TSXSkinCustomForm(SkinComponents[A]).SkinChanged else
  if TComponent(SkinComponents[A]) is TControl then
    TControl(SkinComponents[A]).Invalidate;
end;

procedure TSXSkinCustomLibrary.RuntimeCreatedSetLoaded;
begin
 FIsLoaded:=True;
end;

function TSXSkinCustomLibrary.GetStringsValue(const Name:String):String;
begin
 Result:=Strings.Values[Name];
end;

procedure TSXSkinCustomLibrary.Loaded;
begin
 inherited;
 FIsLoaded:=True;
 ReloadSkinStyles(csDesigning in ComponentState);
end;

{procedure TSXSkinCustomLibrary.PaletteCreated;
begin
 inherited;
 FIsLoaded:=True;
 ReloadSkinStyles(True);
end;}

constructor TSXSkinCustomLibrary.Create(AOwner:TComponent);
begin
 inherited;
 Styles:=TSXSkinStyleList.Create;
 SkinComponents:=TList.Create;
 Strings:=TStringList.Create;
 BasedOnList:=TStringList.Create;
 if csDesigning in ComponentState then
  begin
   FIsLoaded:=True;
   ReloadSkinStyles(True);
  end;
end;

destructor TSXSkinCustomLibrary.Destroy;
begin
 DeletePreloadedBitmaps(Self);
 DeletePreloadedRegions(Self);
 DeletePreloadedZipFiles(Self);
 BasedOnList.Free;
 Strings.Free;
 Styles.Free;
 SkinComponents.Free;
 inherited Destroy;
end;

{ TSXStoredSkin }

procedure TSXStoredSkin.SetFileName(const Value:String);
begin
 if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
   FFileName:=ExtractFileName(Value);
   LoadFromFile(Value);
  end else FFileName:=Value;
end;

procedure TSXStoredSkin.ReadData(Reader:TStream);
begin
 FStream.LoadFromStream(Reader);
end;

procedure TSXStoredSkin.WriteData(Writer:TStream);
begin
 FStream.SaveToStream(Writer);
end;

procedure TSXStoredSkin.DefineProperties(Filer:TFiler);
begin
 inherited;
 Filer.DefineBinaryProperty('FileData',ReadData,WriteData,True);
end;

procedure TSXStoredSkin.LoadFromFile(const AFileName:String);
var FS:TFileStream;
begin
 FS:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
 try
  FStream.LoadFromStream(FS);
 finally
  FS.Free;
 end;
end;

procedure TSXStoredSkin.SaveToFile(const AFileName:String);
var FS:TFileStream;
begin
 FS:=TFileStream.Create(AFileName,fmCreate);
 try
  FStream.SaveToStream(FS);
 finally
  FS.Free;
 end; 
end;

procedure TSXStoredSkin.LoadStrings(SL:TStringList);
var ZipFile:TZipFile;
  StrStream:TStringStream;
  A,Version:Integer;
          S:String;
begin
 SL.Clear;
 ZipFile:=TZipFile.Create;
 try
  try
   FStream.Seek(0,soFromBeginning);
   ZipFile.LoadFromStream(FStream);
   A:=0;
   while (A<ZipFile.Count) and not SameText(ZipFile.Name[A],'skin.sxs') do Inc(A);
   if A<ZipFile.Count then
    begin
     StrStream:=TStringStream.Create(ZipFile.Data[A]);
     try
      //Header
      SetLength(S,length(SXS_Header));
      StrStream.Read(S[1],28);
      if S<>SXS_Header then exit;
      StrStream.Read(Version,sizeof(Version));
      if not VersionSupported(Version) then exit;
      //Strings
      LoadListFromStream(StrStream,SL);
     finally
      StrStream.Free;
     end;
    end;
  except
  end;
 finally
  ZipFile.Free;
 end;
end;

constructor TSXStoredSkin.Create(AOwner:TComponent);
begin
 inherited;
 FStream:=TMemoryStream.Create;
 SXStoredSkins.Add(Self);
end;

destructor TSXStoredSkin.Destroy;
begin
 SXStoredSkins.Remove(Self);
 FStream.Free;
 inherited;
end;

//////////////////////////////////////////////////////////////////////////

function VersionSupported(Version:Integer):Boolean;
var A:Integer;
begin
 for A:=High(SXSuppVersion) downto Low(SXSuppVersion) do
  if Version=SXSuppVersion[A] then
   begin
    Result:=True;
    exit;
   end;
 Result:=False;
end;

function CreateSkinStyleElementByType(const TypeName:String):TSXSkinStyleElement;
begin
 Result:=nil;
 if TypeName='IMAGE' then
  begin
   Result:=TSXSkinStyleImageElement.Create;
   TSXSkinStyleImageElement(Result).ImageType:=ssitAutoDetect;
  end else
 if TypeName='PNG_IMAGE' then
  begin
   Result:=TSXSkinStyleImageElement.Create;
   TSXSkinStyleImageElement(Result).ImageType:=ssitPNG;
  end else
 if TypeName='JPEG_IMAGE' then
  begin
   Result:=TSXSkinStyleImageElement.Create;
   TSXSkinStyleImageElement(Result).ImageType:=ssitJPEG;
  end else
 if TypeName='STYLE' then
  begin
   Result:=TSXSkinStyleStyleElement.Create;
  end else  
 if TypeName='BOX_TILE' then
  begin
   Result:=TSXSkinStyleBoxTileElement.Create;
  end else
 if TypeName='RECT' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftRectangle;
  end else
 if (TypeName='ELLIPSE') or (TypeName='CIRCLE') then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftEllipse;
  end else
 if TypeName='ROUND_RECT' then
  begin                                          
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftRoundRectangle;
  end else
 if TypeName='LINE' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftLine;
   TSXSkinStyleFigureElement(Result).HasBorder:=True;
  end else
 if TypeName='POLYGON' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftPolygon;
  end else
 if TypeName='SOLID_FILL' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftSolidFill;
  end else
 if TypeName='FOCUS_RECT' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftFocusRectangle;
  end else
 if TypeName='ERASE_RECT' then
  begin
   Result:=TSXSkinStyleFigureElement.Create;
   TSXSkinStyleFigureElement(Result).FigureType:=ssftEraseRect;
  end else
 if TypeName='TEXT' then
  begin
   Result:=TSXSkinStyleTextElement.Create;
  end;
 if Result=nil then Result:=TSXSkinStyleElement.Create;
end;

function GetFilterTypeByName(const Name:String):TSXImageFilterType;
begin
 Result:=iftNone;
 if Name='Lighten' then Result:=iftLighten else
  if Name='LightenHorizG' then Result:=iftLightenHorizG else
  if Name='LightenVertG' then Result:=iftLightenVertG else
  if Name='Darken' then Result:=iftDarken else
  if Name='DarkenHorizG' then Result:=iftDarkenHorizG else
  if Name='DarkenVertG' then Result:=iftDarkenVertG else
  if Name='Alpha' then Result:=iftAlpha else
  if Name='Monochrome' then Result:=iftMonochrome else
  if Name='ColorOverlay' then Result:=iftColorOverlay else
  if Name='ColorOverlayHorizG' then Result:=iftColorOverlayHorizG else
  if Name='ColorOverlayVertG' then Result:=iftColorOverlayVertG;
end;

function HasTransformEffect(const Effect:TSXTransformEffectData):Boolean;
begin
 Result:=((Effect.OldType<>tetNone) or (Effect.NewType<>tetNone)) and (Effect.StepsNum>1);
end;

function GetTransformEffectTypeByName(const Name:String):TSXTransformEffectType;
begin
 Result:=tetNone;
 if Name='Blend' then Result:=tetBlend else
  if Name='Clear' then Result:=tetClear else
  if Name='Fade' then Result:=tetFade else
  if Name='OverDraw' then Result:=tetOverDraw else
  if Name='Slide' then Result:=tetSlide;
end;

function GetTransformEffectDirectionByName(const Name:String):TSXTransformEffectDirection;
begin
 Result:=tedLeft;
 if Name='Right' then Result:=tedRight else
  if Name='Top' then Result:=tedTop else
  if Name='Bottom' then Result:=tedBottom else
  if Name='LeftRight' then Result:=tedLeftRight else
  if Name='TopBottom' then Result:=tedTopBottom else
  if Name='LeftTop' then Result:=tedLeftTop else
  if Name='LeftBottom' then Result:=tedLeftBottom else
  if Name='RightTop' then Result:=tedRightTop else
  if Name='RightBottom' then Result:=tedRightBottom;
end;

procedure GetTransformEffectByName(const Name:String;var Effect:TSXTransformEffectData);
var EType:(etNone,
           //Slide&Slide
           etSlideLeft,etSlideTop,etSlideRight,etSlideBottom,etSlideLeftTop,
           etSlideLeftBottom,etSlideRightTop,etSlideRightBottom,
           //OverDraw&OverDraw
           etOverDrawLeft,etOverDrawTop,etOverDrawRight,etOverDrawBottom,
           etOverDrawLeftTop,etOverDrawLeftBottom,etOverDrawRightTop,
           etOverDrawRightBottom,etOverDrawLeftRight,etOverDrawTopBottom,
           etOverDrawLeftRightOut,etOverDrawTopBottomOut);
begin
 EType:=etNone;
 if Name='None' then
  begin
   Effect.SetOldType:=True;
   Effect.OldType:=tetNone;
   Effect.SetNewType:=True;
   Effect.NewType:=tetNone;
   exit;
  end;
 if Name='SlideLeft' then EType:=etSlideLeft else
  if Name='SlideTop' then EType:=etSlideTop else
  if Name='SlideRight' then EType:=etSlideRight else
  if Name='SlideBottom' then EType:=etSlideBottom else
  if Name='SlideLeftTop' then EType:=etSlideLeftTop else
  if Name='SlideLeftBottom' then EType:=etSlideLeftBottom else
  if Name='SlideRightTop' then EType:=etSlideRightTop else
  if Name='SlideRightBottom' then EType:=etSlideRightBottom else
  if Name='OverDrawLeft' then EType:=etOverDrawLeft else
  if Name='OverDrawTop' then EType:=etOverDrawTop else
  if Name='OverDrawRight' then EType:=etOverDrawRight else
  if Name='OverDrawBottom' then EType:=etOverDrawBottom else
  if Name='OverDrawLeftTop' then EType:=etOverDrawLeftTop else
  if Name='OverDrawLeftBottom' then EType:=etOverDrawLeftBottom else
  if Name='OverDrawRightTop' then EType:=etOverDrawRightTop else
  if Name='OverDrawRightBottom' then EType:=etOverDrawRightBottom else
  if Name='OverDrawLeftRight' then EType:=etOverDrawLeftRight else
  if Name='OverDrawTopBottom' then EType:=etOverDrawTopBottom else
  if Name='OverDrawLeftRightOut' then EType:=etOverDrawLeftRightOut else
  if Name='OverDrawTopBottomOut' then EType:=etOverDrawTopBottomOut;
 if EType in [etSlideLeft,etSlideTop,etSlideRight,etSlideBottom,etSlideLeftTop,
              etSlideLeftBottom,etSlideRightTop,etSlideRightBottom] then
  begin
   Effect.SetOldType:=True;
   Effect.OldType:=tetSlide;
   Effect.SetOldDirOut:=True;
   Effect.OldDirOut:=True;
   Effect.SetNewType:=True;
   Effect.NewType:=tetSlide;
   case EType of
    etSlideLeft:        begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedLeft;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedRight;
                         Effect.SetOffset:=True;
                         Effect.Offset:=10;
                        end;
    etSlideTop:         begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedTop;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedBottom;
                         Effect.SetOffset:=True;
                         Effect.Offset:=5;
                        end;
    etSlideRight:       begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedRight;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedLeft;
                         Effect.SetOffset:=True;
                         Effect.Offset:=10;
                        end;
    etSlideBottom:      begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedBottom;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedTop;
                         Effect.SetOffset:=True;
                         Effect.Offset:=5;
                        end;
    etSlideLeftTop:     begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedLeftTop;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedRightBottom;
                         Effect.SetOffset:=True;
                         Effect.Offset:=1;
                        end;
    etSlideLeftBottom:  begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedLeftBottom;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedRightTop;
                         Effect.SetOffset:=True;
                         Effect.Offset:=1;
                        end;
    etSlideRightTop:    begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedRightTop;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedLeftBottom;
                         Effect.SetOffset:=True;
                         Effect.Offset:=1;
                        end;
    etSlideRightBottom: begin
                         Effect.SetOldDirection:=True;
                         Effect.OldDirection:=tedRightBottom;
                         Effect.SetNewDirection:=True;
                         Effect.NewDirection:=tedLeftTop;
                         Effect.SetOffset:=True;
                         Effect.Offset:=1;
                        end;
   end;
  end else
 if EType in [etOverDrawLeft,etOverDrawTop,etOverDrawRight,etOverDrawBottom,
              etOverDrawLeftTop,etOverDrawLeftBottom,etOverDrawRightTop,
              etOverDrawRightBottom,etOverDrawLeftRight,etOverDrawTopBottom,
              etOverDrawLeftRightOut,etOverDrawTopBottomOut] then
  begin
   Effect.SetOldType:=True;
   Effect.OldType:=tetOverDraw;
   Effect.SetOldDirOut:=True;
   Effect.OldDirOut:=True;
   Effect.SetNewType:=True;
   Effect.NewType:=tetOverDraw;
   case EType of
    etOverDrawLeft:         begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedLeft;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedRight;
                            end;
    etOverDrawTop:          begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedTop;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedBottom;
                            end;
    etOverDrawRight:        begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedRight;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedLeft;
                            end;
    etOverDrawBottom:       begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedBottom;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedTop;
                            end;
    etOverDrawLeftTop:      begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedLeftTop;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedLeftTop;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
    etOverDrawLeftBottom:   begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedLeftBottom;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedLeftBottom;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
    etOverDrawRightTop:     begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedRightTop;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedRightTop;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
    etOverDrawRightBottom:  begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedRightBottom;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedRightBottom;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
    etOverDrawLeftRight:    begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedLeftRight;
                             Effect.SetOldDirOut:=True;
                             Effect.OldDirOut:=False;
                             Effect.SetOldInverted:=True;
                             Effect.OldInverted:=True;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedLeftRight;
                            end;
    etOverDrawTopBottom:    begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedTopBottom;
                             Effect.SetOldDirOut:=True;
                             Effect.OldDirOut:=False;
                             Effect.SetOldInverted:=True;
                             Effect.OldInverted:=True;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedTopBottom;
                            end;
    etOverDrawLeftRightOut: begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedLeftRight;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedLeftRight;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
    etOverDrawTopBottomOut: begin
                             Effect.SetOldDirection:=True;
                             Effect.OldDirection:=tedTopBottom;
                             Effect.SetNewDirection:=True;
                             Effect.NewDirection:=tedTopBottom;
                             Effect.SetNewDirOut:=True;
                             Effect.NewDirOut:=True;
                             Effect.SetNewInverted:=True;
                             Effect.NewInverted:=True;
                            end;
   end;
  end;
end;

procedure ApplyFilterToBitmap(var Bitmap:TBitmap32;const Filter:TSXFilterData);
begin
 case Filter.AType of
  iftLighten:            Lighten(Bitmap,Filter.Color);
  iftLightenHorizG:      LightenHorizG(Bitmap,Filter.Color,Filter.Color2);
  iftLightenVertG:       LightenVertG(Bitmap,Filter.Color,Filter.Color2);
  iftDarken:             Darken(Bitmap,Filter.Color);
  iftDarkenHorizG:       DarkenHorizG(Bitmap,Filter.Color,Filter.Color2);
  iftDarkenVertG:        DarkenVertG(Bitmap,Filter.Color,Filter.Color2);
  iftAlpha:              MultiplyAlpha(Bitmap,Filter.Value);
  iftMonochrome:         Monochrome(Bitmap,Filter.Color,Filter.Color2);
  iftColorOverlay:       ColorOverlay(Bitmap,Filter.Color);
  iftColorOverlayHorizG: ColorOverlayHorizG(Bitmap,Filter.Color,Filter.Color2);
  iftColorOverlayVertG:  ColorOverlayVertG(Bitmap,Filter.Color,Filter.Color2);
 end;
end;

procedure ApplyTransformEffectToBitmaps(B1,B2:TBitmap32;const Effect:TSXTransformEffectData;
           CurrentStep:Integer;var B:TBitmap32);
var A:Integer;

 procedure DrawBitmap(New:Boolean;CurrentStep:Integer);
 var   AType:TSXTransformEffectType;
  ADirection:TSXTransformEffectDirection;
     ADirOut:Boolean;
     AInvert:Boolean;
      Bitmap:TBitmap32;
         X,Y:Integer;
        R,R1:TRect;
 begin
  if New then
   begin
    AType:=Effect.NewType;
    ADirection:=Effect.NewDirection;
    ADirOut:=Effect.NewDirOut;
    AInvert:=Effect.NewInverted;
    Bitmap:=B2;
   end else
    begin
     AType:=Effect.OldType;
     ADirection:=Effect.OldDirection;
     ADirOut:=Effect.OldDirOut;
     AInvert:=Effect.OldInverted;
     Bitmap:=B1;
    end;
  case AType of
   tetNone:     Bitmap.DrawTo(B);
   tetClear:    ;
   tetFade:     begin
                 if New then
                  A:=RcTable[Effect.StepsNum,CurrentStep] else
                   A:=255-RcTable[Effect.StepsNum,CurrentStep];
                 DrawToAsAlpha(Bitmap,B,0,0,A);
                end;
   tetOverDraw: begin
                 if ADirOut then CurrentStep:=Effect.StepsNum-CurrentStep;
                 if ADirection=tedLeftRight then
                  begin
                   X:=round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep/2);
                   if AInvert then
                    begin
                     R1:=Rect(X,0,B.Width-X,B.Height);
                     Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                    end else
                     begin
                      R1:=Rect(0,0,X,B.Height);
                      Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                      R1:=Rect(B.Width-X,0,B.Width,B.Height);
                      Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                     end;
                  end else
                 if ADirection=tedTopBottom then
                  begin
                   Y:=round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep/2);
                   if AInvert then
                    begin
                     R1:=Rect(0,Y,B.Width,B.Height-Y);
                     Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                    end else
                     begin
                      R1:=Rect(0,0,B.Width,Y);
                      Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                      R1:=Rect(0,B.Height-Y,B.Width,B.Height);
                      Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                     end;
                  end else
                   begin
                    R:=Rect(0,0,B.Width,B.Height);
                    case ADirection of
                     tedLeft,
                     tedLeftTop,
                     tedLeftBottom:  R.Right:=round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep)-Effect.Offset;
                     tedRight,
                     tedRightTop,
                     tedRightBottom: R.Left:=-round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep)+B.Width+Effect.Offset;
                    end;
                    case ADirection of
                     tedTop,
                     tedLeftTop,
                     tedRightTop:    R.Bottom:=round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep)-Effect.Offset;
                     tedBottom,
                     tedLeftBottom,
                     tedRightBottom: R.Top:=-round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep)+B.Height+Effect.Offset;
                    end;
                    if AInvert then
                     begin
                      R1:=Rect(0,0,B.Width,R.Top);
                      if (R1.Right>R1.Left) and (R1.Bottom>R1.Top) then
                       Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                      //
                      R1:=Rect(0,R.Top,R.Left,R.Bottom);
                      if (R1.Right>R1.Left) and (R1.Bottom>R1.Top) then
                       Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                      //
                      R1:=Rect(R.Right,R.Top,B.Width,R.Bottom);
                      if (R1.Right>R1.Left) and (R1.Bottom>R1.Top) then
                       Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                      //
                      R1:=Rect(0,R.Bottom,B.Width,B.Height);
                      if (R1.Right>R1.Left) and (R1.Bottom>R1.Top) then
                       Bitmap.DrawTo(B,R1.Left,R1.Top,R1);
                     end else Bitmap.DrawTo(B,R.Left,R.Top,R);
                   end;
                end;
   tetSlide:    begin
                 if not ADirOut then CurrentStep:=Effect.StepsNum-CurrentStep;
                 if ADirection=tedLeftRight then
                  begin
                   X:=round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep/2);
                   Bitmap.DrawTo(B,-X,0,Rect(0,0,B.Width div 2,B.Height));
                   Bitmap.DrawTo(B,(B.Width div 2)+X,0,Rect(B.Width div 2,0,B.Width,B.Height));
                  end else
                 if ADirection=tedTopBottom then
                  begin
                   Y:=round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep/2);
                   Bitmap.DrawTo(B,0,-Y,Rect(0,0,B.Width,B.Height div 2));
                   Bitmap.DrawTo(B,0,(B.Height div 2)+Y,Rect(0,B.Height div 2,B.Width,B.Height));
                  end else
                   begin
                    X:=0; Y:=0;
                    case ADirection of
                     tedLeft,
                     tedLeftTop,
                     tedLeftBottom:  X:=-round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep);
                     tedRight,
                     tedRightTop,
                     tedRightBottom: X:=round((B.Width+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep);
                    end;
                    case ADirection of
                     tedTop,
                     tedLeftTop,
                     tedRightTop:    Y:=-round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep);
                     tedBottom,
                     tedLeftBottom,
                     tedRightBottom: Y:=round((B.Height+Effect.Offset+0.1)/Effect.StepsNum*CurrentStep);
                    end;
                    Bitmap.DrawTo(B,X,Y);
                   end;
                end;
  end;
 end;

begin
 if Effect.NewType=tetBlend then
  begin
   A:=RcTable[Effect.StepsNum,CurrentStep];
   DrawMixedBitmap(B,B1,B2,A);
   exit;
  end;
 DrawBitmap(Effect.OldOnTop,CurrentStep);
 DrawBitmap(not Effect.OldOnTop,CurrentStep);
end;

function CreateSkinStyleFromStream(S:TStream;Version:Integer;
          const RootPath,ZipFilePath:String;DestList1,DestList2:TSXSkinStyleList):TSXSkinStyle;
var AType:TSXSkinStyleType;
begin
 S.Read(AType,sizeof(AType));
 case AType of
  sstGeneral:         Result:=TSXSkinGeneralStyle.Create;
  sstSelective:       Result:=TSXSkinSelectiveStyle.Create;
  sstLabel:           Result:=TSXSkinLabelStyle.Create;
  sstCheckBox:        Result:=TSXSkinCheckBoxStyle.Create;
  sstRadioButton:     Result:=TSXSkinRadioButtonStyle.Create;
  sstGroupBox:        Result:=TSXSkinGroupBoxStyle.Create;
  sstMultiState:      Result:=TSXSkinMultiStateStyle.Create;
  sstMultiStateCheck: Result:=TSXSkinMultiStateCheckStyle.Create;
  sstButton:          Result:=TSXSkinButtonStyle.Create;
  sstEdit:            Result:=TSXSkinEditStyle.Create;
  sstForm:            Result:=TSXSkinFormStyle.Create;
  sstUpDown:          Result:=TSXSkinUpDownStyle.Create;
  sstSpinEdit:        Result:=TSXSkinSpinEditStyle.Create;
  else                Result:=TSXSkinStyle.Create;
 end;
 Result.LoadFromStream(S,Version,RootPath,ZipFilePath,DestList1,DestList2);
end;

function CreateSkinStyleElementFromStream(S:TStream;Version:Integer;
          const RootPath,ZipFilePath:String):TSXSkinStyleElement;
var AType:TSXSkinStyleElementType;
begin
 S.Read(AType,sizeof(AType));
 case AType of
  ssetImage:   Result:=TSXSkinStyleImageElement.Create;
  ssetBoxTile: Result:=TSXSkinStyleBoxTileElement.Create;
  ssetFigure:  Result:=TSXSkinStyleFigureElement.Create;
  ssetText:    Result:=TSXSkinStyleTextElement.Create;
  ssetStyle:   Result:=TSXSkinStyleStyleElement.Create;
  else         Result:=TSXSkinStyleElement.Create;
 end;
 Result.LoadFromStream(S,Version,RootPath,ZipFilePath);
end;

procedure LoadFontData(S:TStream;var FD:TSXFontData);
var B:Boolean;
begin
 with FD do
  begin
   Load8Flags(S,SetFontName,SetFontSize,SetFontStyle,SetFontColor,
                SetHasShadow,SetShadowColor,SetSmoothLevel,SetDoPrepaint);
   if SetHasShadow or SetDoPrepaint then
    Load8MaskedFlags(S,HasShadow,DoPrepaint,B,B,B,B,B,B,
                       SetHasShadow,SetDoPrepaint,False,False,False,False,False,False);
   if SetFontName then
    LoadString(S,FontName);
   if SetFontSize then
    S.Read(FontSize,sizeof(FontSize));
   if SetFontStyle then
    S.Read(FontStyle,sizeof(FontStyle));
   if SetFontColor then
    S.Read(FontColor,sizeof(FontColor));
   if SetShadowColor then
    S.Read(ShadowColor,sizeof(ShadowColor));
   if SetSmoothLevel then
    S.Read(SmoothLevel,sizeof(SmoothLevel));
  end;
end;

procedure SaveFontData(S:TStream;const FD:TSXFontData);
begin
 with FD do
  begin
   Save8Flags(S,SetFontName,SetFontSize,SetFontStyle,SetFontColor,
                SetHasShadow,SetShadowColor,SetSmoothLevel,SetDoPrepaint);
   if SetHasShadow or SetDoPrepaint then
    Save8Flags(S,HasShadow,DoPrepaint,False,False,False,False,False,False);
   if SetFontName then
    SaveString(S,FontName);
   if SetFontSize then
    S.Write(FontSize,sizeof(FontSize));
   if SetFontStyle then
    S.Write(FontStyle,sizeof(FontStyle));
   if SetFontColor then
    S.Write(FontColor,sizeof(FontColor));
   if SetShadowColor then
    S.Write(ShadowColor,sizeof(ShadowColor));
   if SetSmoothLevel then
    S.Write(SmoothLevel,sizeof(SmoothLevel));
  end;
end;

procedure LoadFilterData(S:TStream;var Filter:TSXFilterData);
var StdLighten,B:Boolean;
begin
 with Filter do
  begin
   Load8Flags(S,SetAType,SetValue,SetColor,SetColor2,StdLighten,B,B,B);
   if StdLighten then
    begin
     SetAType:=True;
     AType:=iftLighten;
     SetColor:=True;
     Color:=$424203;
    end else
     begin
      if SetAType then
       S.Read(AType,sizeof(AType));
      if SetValue then
       S.Read(Value,sizeof(Value));
      if SetColor then
       S.Read(Color,sizeof(Color));
      if SetColor2 then
       S.Read(Color2,sizeof(Color2));
     end;
  end;
end;

procedure SaveFilterData(S:TStream;const Filter:TSXFilterData);
var StdLighten:Boolean;
begin
 with Filter do
  begin
   StdLighten:=(AType=iftLighten) and (Color=$424203);
   Save8Flags(S,SetAType,SetValue,SetColor,SetColor2,StdLighten,False,False,False);
   if not StdLighten then
    begin
     if SetAType then
      S.Write(AType,sizeof(AType));
     if SetValue then
      S.Write(Value,sizeof(Value));
     if SetColor then
      S.Write(Color,sizeof(Color));
     if SetColor2 then
      S.Write(Color2,sizeof(Color2));
    end;
  end;
end;

procedure SaveTransformEffectData(S:TStream;const Effect:TSXTransformEffectData);
var BlendEffect:Boolean;
begin
 with Effect do
  begin
   Save8Flags(S,SetOldType,SetOldDirection,SetOldDirOut,SetOldInverted,
                SetOldOnTop,SetNewType,SetNewDirection,SetNewDirOut);
   Save8Flags(S,SetNewInverted,SetStepsNum,SetOffset,SetDrawCaption,False,False,False,False);
   BlendEffect:=(NewType=tetBlend);
   Save8Flags(S,OldDirOut,OldInverted,OldOnTop,NewDirOut,NewInverted,DrawCaption,BlendEffect,False);
   if not BlendEffect then
    begin
     if SetOldType then
      S.Write(OldType,sizeof(OldType));
     if SetOldDirection then
      S.Write(OldDirection,sizeof(OldDirection));
     if SetNewType then
      S.Write(NewType,sizeof(NewType));
     if SetNewDirection then
      S.Write(NewDirection,sizeof(NewDirection));
    end;
   if SetStepsNum then
    SavePackedInteger(S,StepsNum);
   if SetOffset then
    SavePackedInteger(S,Offset);
  end;
end;

procedure LoadTransformEffectData(S:TStream;var Effect:TSXTransformEffectData);
var BlendEffect,B:Boolean;
begin
 with Effect do
  begin
   Load8Flags(S,SetOldType,SetOldDirection,SetOldDirOut,SetOldInverted,
                SetOldOnTop,SetNewType,SetNewDirection,SetNewDirOut);
   Load8Flags(S,SetNewInverted,SetStepsNum,SetOffset,SetDrawCaption,B,B,B,B);
   Load8MaskedFlags(S,OldDirOut,OldInverted,OldOnTop,NewDirOut,
                      NewInverted,DrawCaption,BlendEffect,B,
                      SetOldDirOut,SetOldInverted,SetOldOnTop,SetNewDirOut,
                      SetNewInverted,SetDrawCaption,BlendEffect,False);
   if BlendEffect then
    begin
     SetNewType:=True;
     NewType:=tetBlend;
    end else
     begin
      if SetOldType then
       S.Read(OldType,sizeof(OldType));
      if SetOldDirection then
       S.Read(OldDirection,sizeof(OldDirection));
      if SetNewType then
       S.Read(NewType,sizeof(NewType));
      if SetNewDirection then
       S.Read(NewDirection,sizeof(NewDirection));
     end;
   if SetStepsNum then
    LoadPackedInteger(S,StepsNum);
   if SetOffset then
    LoadPackedInteger(S,Offset);
  end;
end;

procedure LoadStringsFromZIPSkinStream(Stream:TStream;SL:TStringList);
var       S:String;
          A:Integer;
    Version:Integer;
    ZipFile:TZipFile;
  StrStream:TStringStream;
begin
 ZipFile:=TZipFile.Create;
 try
  try
   Stream.Seek(0,soFromBeginning);
   ZipFile.LoadFromStream(Stream);
   A:=0;
   while (A<ZipFile.Count) and not SameText(ZipFile.Name[A],'skin.sxs') do Inc(A);
   if A<ZipFile.Count then
    begin
     StrStream:=TStringStream.Create(ZipFile.Data[A]);
     try
      //Header
      SetLength(S,length(SXS_Header));
      StrStream.Read(S[1],28);
      if S<>SXS_Header then exit;
      StrStream.Read(Version,sizeof(Version));
      if not VersionSupported(Version) then exit;
      //Strings
      LoadListFromStream(StrStream,SL);
     finally
      StrStream.Free;
     end;
    end;
  except
  end;
 finally
  ZipFile.Free;
 end;
end;

procedure LoadStringsFromSkinFile(const FilePath:String;SL:TStringList);
var IniFile:TIniFile;
         FS:TFileStream;
          S:String;
    Version:Integer;
begin
 SL.Clear;
 if not FileExists(FilePath) then exit;
 if SameText(ExtractFileExt(FilePath),'.zip') then
  begin
   FS:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
   try
    LoadStringsFromZIPSkinStream(FS,SL);
   finally
    FS.Free;
   end;
  end else
 if SameText(ExtractFileExt(FilePath),'.ini') then
  begin
   IniFile:=TIniFile.Create(FilePath);
   try
    IniFile.ReadSectionValues('Strings',SL);
   finally
    IniFile.Free;
   end;
  end else
   begin
    FS:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
    try
     //Header
     SetLength(S,length(SXS_Header));
     FS.Read(S[1],28);
     if S<>SXS_Header then exit;
     FS.Read(Version,sizeof(Version));
     if not VersionSupported(Version) then exit;
     //Strings
     LoadListFromStream(FS,SL);
    finally
     FS.Free;
    end;
   end;
end;

function GetImageResizeModeByName(const Name:String):TSXImageResizeMode;
begin
 if Name='Tile' then Result:=irmTile else
  if Name='Stretch' then Result:=irmStretch else
   Result:=irmNone;
end;

function GetImageStretchFilterByName(const Name:String):TSXImageStretchFilter;
begin
 if Name='Linear' then Result:=isfLinear else
  if Name='Spline' then Result:=isfSpline else
  if Name='Lanczos' then Result:=isfLanczos else
  if Name='Mitchell' then Result:=isfMitchell else
   Result:=isfNearest;
end;

function GetNewCElementID:Integer;
begin
 if LastCElementID=MaxInt then
  LastCElementID:=1 else
   Inc(LastCElementID);
 Result:=LastCElementID;
end;

initialization

 SXStoredSkins:=TList.Create;
 CreateResourceSkins;
 StdVariableComparer:=TSXStdVariableComparer.Create;

finalization

 InternalSkin.Free;
 SXStoredSkins.Free;
 StdVariableComparer.Free;

end.
