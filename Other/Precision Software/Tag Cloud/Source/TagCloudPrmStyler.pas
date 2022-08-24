{------------------------------------------------------------------------------
  TagCloudPrmStyler.pas

  TTagCloudPrmStyler, a part of "TagCloud for VCL" package

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Generic text file styler for TTagCloud component

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. The complete source code remains property of the author
  and may not be distributed, published, given or sold in any form as such.
  No parts of the source code can be included in any other component
  or application without written authorization of the author.

  Copyright (c) 2008 - 2014  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 2.1 (2014-09-29)
  - added: Delphi XE6/XE7 support

  Version 2.0 (2013-11-16)
  - added: Delphi XE4/XE5 support

  Version 1.9.5 (2013-01-01)
  - added: Delphi XE3 support

  Version 1.5 (16.3.2011)
  - The first release of TTagCloudPrmStyler
}

{ This unit contains TTagCloudPrmStyler component declaration and implementation. }
unit TagCloudPrmStyler;

interface

uses
  Classes, TagCloud;

type
  TTagCloudPrmStyler = class(TCustomTagCloudStyler)
  private
    FTCParams: TStrings;
    FFileName: string;
    FStyleName: string;
    procedure SetTCParams(Value: TStrings);
    procedure TCParamsChange(Sender: TObject);
    procedure SetFileName(const Value:string);
  protected
    // Called immediately after the component is created and loaded from VCL stream
    procedure Loaded; override;
    // Applies the current style to the passed TagCloud component
    procedure Apply(Sender: TCustomTagCloud); override;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    // Loads the style definition from the stream
    function LoadFromStream(Stream:TStream):Boolean;
    // Loads the style definition from the file
    function LoadFromFile(const aFileName:string):Boolean;
    // Saves the style definition into the stream
    function SaveToStream(Stream:TStream):Boolean;
    { Saves the style definition into the file. UpdateFileName parameter allows you to automatically set the FileName property
      to the name specified in aFileName parameter. }
    function SaveToFile(const aFileName:string; UpdateFileName:Boolean=true):Boolean;
    // Fill the styler parameters from passed TCustomTagCloud component. If Sender is nil, default style values will be used.
    procedure LoadFromTagCloud(Sender: TCustomTagCloud); override;
  published
    // This property holds the style definition. It is stored in a simple 'key=value' format. }
    property Params: TStrings read FTCParams write SetTCParams;
    // By setting this property, you can load the style from specified file.
    property FileName: string read FFileName write SetFileName;
    // An informative property, that reads the name of the style from its definition (the key name is '_stylename').
    property StyleName: string read FStyleName write FStyleName;
  end;

implementation

uses
  {$IF CompilerVersion >= 24} System.UITypes, {$IFEND}
  SysUtils, Graphics, Controls;

const
  // Key and values identifiers, that are used in a style definition
  tcs_true = 'true';
  tcs_bold = 'bold';
  tcs_italic = 'italic';
  tcs_underline = 'underline';
  tcs_strikeout = 'strikeout';
  tcs_ParentColor = 'ParentColor';
  tcs_Color = 'Color';
  tcs_ParentFont = 'ParentFont';
  tcs_Font_Name = 'Font_Name';
  tcs_Font_Size = 'Font_Size';
  tcs_Font_Color = 'Font_Color';
  tcs_Font_Style = 'Font_Style';
  tcs_HoverStyle = 'HoverStyle';
  tcs_HoverColor = 'HoverColor';
  tcs_SelectedColor = 'SelectedColor';
  tcs_AutoShrinkRows = 'AutoShrinkRows';
  tcs_ShrinkDiacritic = 'ShrinkDiacritic';
  tcs_HoverEnlarge = 'HoverEnlarge';
  tcs_HoverColSpace = 'HoverColSpace';
  tcs_LogScale = 'LogScale';
  tcs_Transparent = 'Transparent';
  tcs_ColSpacing = 'ColSpacing';
  tcs_RowSpacing = 'RowSpacing';
  tcs_FixedColCount = 'FixedColCount';
  tcs_FixedColFullFrame = 'FixedColFullFrame';
  tcs_FixedColWidth = 'FixedColWidth';
  tcs_GlowSize = 'GlowSize';
  tcs_MaxFontSize = 'MaxFontSize';
{$IF CompilerVersion>=18}
  tcs_Padding_Left = 'Padding_Left';
  tcs_Padding_Top = 'Padding_Top';
  tcs_Padding_Right = 'Padding_Right';
  tcs_Padding_Bottom = 'Padding_Bottom';
{$IFEND}
  tcs_Colors = 'Colors_';
  tcs_CustomScale = 'CustomScale_';
  tcs_ValueFrom = 'ValueFrom';
  tcs_ValueTo = 'ValueTo';
  tcs_HoverCursor = 'HoverCursor';
  tcs_Alignment = 'Alignment';
  tcs_Direction = 'Direction';
  tcs_ItemFrame = 'ItemFrame_';
  tcs_HoverFrame = 'HoverFrame_';
  tcs_SelectedFrame = 'SelectedFrame_';
  tcs_BackColor = 'BackColor';
  tcs_FrameColor = 'FrameColor';
  tcs_FrameMargin = 'FrameMargin';
  tcs_FrameSize = 'FrameSize';
  tcs_FrameStyle = 'FrameStyle';
  tcs_RoundedSize = 'RoundedSize';


constructor TTagCloudPrmStyler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName:='';
  FStyleName:='';
  FTCParams:=TStringList.Create;
  TStringList(FTCParams).OnChange:=TCParamsChange;
end;

destructor TTagCloudPrmStyler.Destroy;
begin
  FTCParams.Free;
  inherited;
end;

procedure TTagCloudPrmStyler.Loaded;
begin
  inherited Loaded;
  if Length(FileName)>0 then
    LoadFromFile(FileName)
end;

procedure TTagCloudPrmStyler.SetTCParams(Value: TStrings);
begin
  FTCParams.Assign(Value);
end;

procedure TTagCloudPrmStyler.TCParamsChange(Sender: TObject);
begin
  FStyleName:=trim(Params.Values['_stylename']);
  ApplyStyle;
end;

procedure TTagCloudPrmStyler.SetFileName(const Value:string);
begin
  if FFileName<>Value then
    if not LoadFromFile(Value) then
      FFileName:='';
end;

function TTagCloudPrmStyler.LoadFromStream(Stream:TStream):Boolean;
begin
  Result:=False;
  if Assigned(Stream) then
  begin
    Params.LoadFromStream(Stream);
    FFileName:='';
    Result:=True;
  end;
end;

function TTagCloudPrmStyler.LoadFromFile(const aFileName:string):Boolean;
begin
  Result:=False;
  if (Length(aFileName)>0) and fileexists(aFileName) then
  begin
    Params.LoadFromFile(aFileName);
    FFileName:=aFileName;
    Result:=True;
  end;
end;

function TTagCloudPrmStyler.SaveToStream(Stream:TStream):Boolean;
begin
  Result:=False;
  if Assigned(Stream) then
  begin
    Params.SaveToStream(Stream);
    Result:=True;
  end;
end;

function TTagCloudPrmStyler.SaveToFile(const aFileName:string; UpdateFileName:Boolean=true):Boolean;
begin
  Result:=False;
  if Length(aFileName)>0 then
  begin
    Params.SaveToFile(aFileName);
    if UpdateFileName then
      FFileName:=aFileName;
    Result:=True;
  end;
end;

procedure TTagCloudPrmStyler.LoadFromTagCloud(Sender: TCustomTagCloud);
var
  i:Integer;
  tmpScale:string;

  function _StyleToParam(const aStyle:TFontStyles):string;
  begin
    Result:='';
    if fsBold in aStyle then Result:=Result+','+tcs_bold;
    if fsItalic in aStyle then Result:=Result+','+tcs_italic;
    if fsUnderline in aStyle then Result:=Result+','+tcs_underline;
    if fsStrikeOut in aStyle then Result:=Result+','+tcs_strikeout;
    if Length(Result)>0 then
      Delete(Result,1,1);
  end;

begin
  Params.BeginUpdate;
  try
    Params.Clear;
    if Sender=nil then
    begin
      Params.Add(tcs_Alignment+'='+IntToStr(Integer(taCenter)));
      Params.Add(tcs_AutoShrinkRows+'=');
      Params.Add(tcs_Color+'=');
      Params.Add(tcs_ColSpacing+'=');
      Params.Add(tcs_Direction+'=');
      Params.Add(tcs_FixedColCount+'=0');
      Params.Add(tcs_FixedColFullFrame+'=0');
      Params.Add(tcs_FixedColWidth+'=0');
      Params.Add(tcs_Font_Color+'=');
      Params.Add(tcs_Font_Name+'=');
      Params.Add(tcs_Font_Size+'=');
      Params.Add(tcs_Font_Style+'=');
      Params.Add(tcs_GlowSize+'=');
      Params.Add(tcs_HoverColor+'=');
      Params.Add(tcs_HoverCursor+'='+IntToStr(Integer(crHandPoint)));
      Params.Add(tcs_HoverEnlarge+'=');
      Params.Add(tcs_HoverColSpace+'=');
      Params.Add(tcs_HoverFrame + tcs_BackColor+'=');
      Params.Add(tcs_HoverFrame + tcs_FrameColor +'=');
      Params.Add(tcs_HoverFrame + tcs_FrameMargin +'=');
      Params.Add(tcs_HoverFrame + tcs_FrameSize +'=');
      Params.Add(tcs_HoverFrame + tcs_FrameStyle +'=');
      Params.Add(tcs_HoverFrame + tcs_RoundedSize +'=');
      Params.Add(tcs_HoverStyle+'='+tcs_bold+','+tcs_underline);
      Params.Add(tcs_ItemFrame + tcs_BackColor+'=');
      Params.Add(tcs_ItemFrame + tcs_FrameColor +'=');
      Params.Add(tcs_ItemFrame + tcs_FrameMargin +'=');
      Params.Add(tcs_ItemFrame + tcs_FrameSize +'=');
      Params.Add(tcs_ItemFrame + tcs_FrameStyle +'=');
      Params.Add(tcs_ItemFrame + tcs_RoundedSize +'=');
      Params.Add(tcs_LogScale+'='+tcs_true);
      Params.Add(tcs_MaxFontSize+'=');
      {$IF CompilerVersion>=18}
      Params.Add(tcs_Padding_Bottom+'=');
      Params.Add(tcs_Padding_Left+'=');
      Params.Add(tcs_Padding_Right+'=');
      Params.Add(tcs_Padding_Top+'=');
      {$IFEND}
      Params.Add(tcs_ParentColor+'='+tcs_true);
      Params.Add(tcs_ParentFont+'='+tcs_true);
      Params.Add(tcs_RowSpacing+'=');
      Params.Add(tcs_SelectedColor+'=');
      Params.Add(tcs_SelectedFrame + tcs_BackColor+'=');
      Params.Add(tcs_SelectedFrame + tcs_FrameColor +'=');
      Params.Add(tcs_SelectedFrame + tcs_FrameMargin +'=');
      Params.Add(tcs_SelectedFrame + tcs_FrameSize +'=');
      Params.Add(tcs_SelectedFrame + tcs_FrameStyle +'=');
      Params.Add(tcs_SelectedFrame + tcs_RoundedSize +'=');
      Params.Add(tcs_ShrinkDiacritic+'=');
      Params.Add(tcs_Transparent+'=');
    end
    else
    begin
      Params.Add(tcs_Alignment+'='+IntToStr(Integer(Sender.Alignment)));
      Params.Add(tcs_AutoShrinkRows+'='+IntToStr(Integer(Sender.AutoShrinkRows)));
      Params.Add(tcs_Color+'='+ColorToString(Sender.Color));
      for i:=0 To Sender.Colors.Count-1 do
      begin
        tmpScale:=tcs_Colors+IntToStr(i)+'_';
        Params.Add(tmpScale + tcs_Color +'='+ColorToString(Sender.Colors[i].Color));
        Params.Add(tmpScale + tcs_BackColor+'=' + ColorToString(Sender.Colors[i].BackColor));
        Params.Add(tmpScale + tcs_FrameColor +'=' + ColorToString(Sender.Colors[i].FrameColor));
      end;
      for i:=0 To Sender.CustomScale.Count-1 do
      begin
        tmpScale:=tcs_CustomScale+IntToStr(i)+'_';
        Params.Add(tmpScale + tcs_ValueFrom+'='+IntToStr(Sender.CustomScale[i].ValueFrom));
        Params.Add(tmpScale + tcs_ValueTo+'='+IntToStr(Sender.CustomScale[i].ValueTo));
        Params.Add(tmpScale + tcs_Font_Size+'='+IntToStr(Sender.CustomScale[i].FontSize));
        Params.Add(tmpScale + tcs_Color+'='+ColorToString(Sender.CustomScale[i].Color));
        Params.Add(tmpScale + tcs_BackColor+'=' + ColorToString(Sender.CustomScale[i].BackColor));
        Params.Add(tmpScale + tcs_FrameColor +'=' + ColorToString(Sender.CustomScale[i].FrameColor));
      end;
      Params.Add(tcs_ColSpacing+'='+IntToStr(Sender.ColSpacing));
      Params.Add(tcs_Direction+'='+IntToStr(Integer(Sender.Direction)));
      Params.Add(tcs_FixedColCount+'='+IntToStr(Sender.FixedColCount));
      Params.Add(tcs_FixedColFullFrame+'='+IntToStr(Integer(Sender.FixedColFullFrame)));
      Params.Add(tcs_FixedColWidth+'='+IntToStr(Sender.FixedColWidth));
      Params.Add(tcs_Font_Color+'='+ColorToString(Sender.Font.Color));
      Params.Add(tcs_Font_Name+'='+Sender.Font.Name);
      Params.Add(tcs_Font_Size+'='+IntToStr(Sender.Font.Size));
      Params.Add(tcs_Font_Style+'='+_StyleToParam(Sender.Font.Style));
      Params.Add(tcs_GlowSize+'='+IntToStr(Sender.GlowSize));
      Params.Add(tcs_HoverColor+'='+ColorToString(Sender.HoverColor));
      Params.Add(tcs_HoverCursor+'='+IntToStr(Integer(Sender.HoverCursor)));
      Params.Add(tcs_HoverEnlarge+'='+IntToStr(Integer(Sender.HoverEnlarge)));
      Params.Add(tcs_HoverColSpace+'='+IntToStr(Integer(Sender.HoverColSpace)));
      Params.Add(tcs_HoverFrame + tcs_BackColor+'=' + ColorToString(Sender.HoverFrame.BackColor));
      Params.Add(tcs_HoverFrame + tcs_FrameColor +'=' + ColorToString(Sender.HoverFrame.FrameColor));
      Params.Add(tcs_HoverFrame + tcs_FrameMargin +'=' + IntToStr(Sender.HoverFrame.FrameMargin));
      Params.Add(tcs_HoverFrame + tcs_FrameSize +'=' + IntToStr(Sender.HoverFrame.FrameSize));
      Params.Add(tcs_HoverFrame + tcs_FrameStyle +'=' + IntToStr(Integer(Sender.HoverFrame.FrameStyle)));
      Params.Add(tcs_HoverFrame + tcs_RoundedSize +'=' + IntToStr(Sender.HoverFrame.RoundedSize));
      Params.Add(tcs_HoverStyle+'='+_StyleToParam(Sender.HoverStyle));
      Params.Add(tcs_ItemFrame + tcs_BackColor+'=' + ColorToString(Sender.ItemFrame.BackColor));
      Params.Add(tcs_ItemFrame + tcs_FrameColor +'=' + ColorToString(Sender.ItemFrame.FrameColor));
      Params.Add(tcs_ItemFrame + tcs_FrameMargin +'=' + IntToStr(Sender.ItemFrame.FrameMargin));
      Params.Add(tcs_ItemFrame + tcs_FrameSize +'=' + IntToStr(Sender.ItemFrame.FrameSize));
      Params.Add(tcs_ItemFrame + tcs_FrameStyle +'=' + IntToStr(Integer(Sender.ItemFrame.FrameStyle)));
      Params.Add(tcs_ItemFrame + tcs_RoundedSize +'=' + IntToStr(Sender.ItemFrame.RoundedSize));
      Params.Add(tcs_LogScale+'='+IntToStr(Integer(Sender.LogScale)));
      Params.Add(tcs_MaxFontSize+'='+IntToStr(Sender.MaxFontSize));
      {$IF CompilerVersion>=18}
      Params.Add(tcs_Padding_Bottom+'='+IntToStr(Sender.Padding.Bottom));
      Params.Add(tcs_Padding_Left+'='+IntToStr(Sender.Padding.Left));
      Params.Add(tcs_Padding_Right+'='+IntToStr(Sender.Padding.Right));
      Params.Add(tcs_Padding_Top+'='+IntToStr(Sender.Padding.Top));
      {$IFEND}
      Params.Add(tcs_ParentColor+'='+IntToStr(Integer(Sender.ParentColor)));
      Params.Add(tcs_ParentFont+'='+IntToStr(Integer(Sender.ParentFont)));
      Params.Add(tcs_RowSpacing+'='+IntToStr(Sender.RowSpacing));
      Params.Add(tcs_SelectedColor+'='+ColorToString(Sender.SelectedColor));
      Params.Add(tcs_SelectedFrame + tcs_BackColor+'=' + ColorToString(Sender.SelectedFrame.BackColor));
      Params.Add(tcs_SelectedFrame + tcs_FrameColor +'=' + ColorToString(Sender.SelectedFrame.FrameColor));
      Params.Add(tcs_SelectedFrame + tcs_FrameMargin +'=' + IntToStr(Sender.SelectedFrame.FrameMargin));
      Params.Add(tcs_SelectedFrame + tcs_FrameSize +'=' + IntToStr(Sender.SelectedFrame.FrameSize));
      Params.Add(tcs_SelectedFrame + tcs_FrameStyle +'=' + IntToStr(Integer(Sender.SelectedFrame.FrameStyle)));
      Params.Add(tcs_SelectedFrame + tcs_RoundedSize +'=' + IntToStr(Sender.SelectedFrame.RoundedSize));
      Params.Add(tcs_ShrinkDiacritic+'='+IntToStr(Integer(Sender.ShrinkDiacritic)));
      Params.Add(tcs_Transparent+'='+IntToStr(Integer(Sender.Transparent)));
    end;
  finally
    Params.EndUpdate;
  end;
end;

procedure TTagCloudPrmStyler.Apply(Sender: TCustomTagCloud);
var
  i:Integer;
  FS:TFontStyles;
  tmpScale,tmpClrs:string;

type
  { Auxiliary structure to support TColor manipulation }
  TColorRec = packed record
    case Integer of
      0: (Value: Longint);
      1: (Red, Green, Blue: Byte);
      2: (R, G, B, Flag: Byte);
      {$IFDEF MSWINDOWS}
      3: (Index: Word); // GetSysColor, PaletteIndex
      {$ENDIF MSWINDOWS}
  end;

  function _StrToColor(const Color: string; DefColor:TColor):TColor;
  var
    Temp:TColorRec;
  begin
    if Length(Color)>0 then
    begin
      if (Length(Color)>1) and (Color[1]='#') then  // html to TColor
      begin
        Temp.Red:=StrToIntDef('$'+uppercase(Copy(Color,2,2)),0);
        Temp.Green:=StrToIntDef('$'+uppercase(Copy(Color,4,2)),0);
        Temp.Blue:=StrToIntDef('$'+uppercase(Copy(Color,6,2)),0);
        Result:=Temp.Value;
      end
      else
      begin
        if StrToIntDef(Color,-1)=-1 then
        begin
          if lowercase(Copy(Color,1,2))<>'cl' then
            IdentToColor('cl'+Color,Longint(Result))
          else
            IdentToColor(Color,Longint(Result))
        end
        else
          Result:=StrToIntDef(Color,0);
      end
    end
    else
      Result:=DefColor;
  end;

  function _StrToBool(Value:string; DefValue:Boolean):Boolean;
  begin
    Value:=lowercase(trim(Value));
    if Length(Value)=0 then
      Result:=DefValue
    else
      Result:=SameText(Value,'1') or SameText(Value,tcs_true);
  end;

  function _ParamToStyle(const aParam:string; var aStyle:TFontStyles):Boolean;
  var
    tmp:string;
  begin
    Result:=Params.IndexOfName(aParam)>=0;
    if Result then
    begin
      tmp:=lowercase(Params.Values[aParam])+',';
      aStyle:=[];
      if Pos(tcs_bold+',',tmp)>0 then aStyle:=aStyle+[fsBold];
      if Pos(tcs_italic+',',tmp)>0 then aStyle:=aStyle+[fsItalic];
      if Pos(tcs_underline+',',tmp)>0 then aStyle:=aStyle+[fsUnderline];
      if Pos(tcs_strikeout+',',tmp)>0 then aStyle:=aStyle+[fsStrikeOut];
    end;
  end;

begin
  if Assigned(Sender) then
  begin
    Sender.ParentColor:=_StrToBool(Params.Values[tcs_ParentColor],Sender.ParentColor);
    if not Sender.ParentColor then
      Sender.Color:=_StrToColor(Params.Values[tcs_Color],Sender.Color);
    Sender.ParentFont:=_StrToBool(Params.Values[tcs_ParentFont],Sender.ParentFont);
    if not Sender.ParentFont then
    begin
      tmpClrs:=trim(Params.Values[tcs_Font_Name]);
      if Length(tmpClrs)>0 then Sender.Font.Name:=tmpClrs;
      if not IgnoreDimensions then
        Sender.Font.Size:=StrToIntDef(Params.Values[tcs_Font_Size],Sender.Font.Size);
      Sender.Font.Color:=_StrToColor(Params.Values[tcs_Font_Color],Sender.Font.Color);
      if _ParamToStyle(tcs_Font_Style,FS) then Sender.Font.Style:=FS;
    end;
    if _ParamToStyle(tcs_HoverStyle,FS) then Sender.HoverStyle:=FS;
    Sender.HoverColor:=_StrToColor(Params.Values[tcs_HoverColor],Sender.HoverColor);
    Sender.SelectedColor:=_StrToColor(Params.Values[tcs_SelectedColor],Sender.SelectedColor);
    Sender.AutoShrinkRows:=_StrToBool(Params.Values[tcs_AutoShrinkRows],Sender.AutoShrinkRows);
    Sender.HoverEnlarge:=_StrToBool(Params.Values[tcs_HoverEnlarge],Sender.HoverEnlarge);
    Sender.HoverColSpace:=_StrToBool(Params.Values[tcs_HoverColSpace],Sender.HoverColSpace);
    Sender.Transparent:=_StrToBool(Params.Values[tcs_Transparent],Sender.Transparent);
    if not IgnoreDimensions then
    begin
      Sender.ShrinkDiacritic:=_StrToBool(Params.Values[tcs_ShrinkDiacritic],Sender.ShrinkDiacritic);
      Sender.LogScale:=_StrToBool(Params.Values[tcs_LogScale],Sender.LogScale);
      Sender.ColSpacing:=StrToIntDef(Params.Values[tcs_ColSpacing],Sender.ColSpacing);
      Sender.RowSpacing:=StrToIntDef(Params.Values[tcs_RowSpacing],Sender.RowSpacing);
      Sender.FixedColCount:=StrToIntDef(Params.Values[tcs_FixedColCount],Sender.FixedColCount);
      if Sender.FixedColCount<=0 then
        Sender.FixedColWidth:=StrToIntDef(Params.Values[tcs_FixedColWidth],Sender.FixedColWidth);
      Sender.MaxFontSize:=StrToIntDef(Params.Values[tcs_MaxFontSize],Sender.MaxFontSize);
      {$IF CompilerVersion>=18}
      Sender.Padding.Left:=StrToIntDef(Params.Values[tcs_Padding_Left],Sender.Padding.Left);
      Sender.Padding.Top:=StrToIntDef(Params.Values[tcs_Padding_Top],Sender.Padding.Top);
      Sender.Padding.Right:=StrToIntDef(Params.Values[tcs_Padding_Right],Sender.Padding.Right);
      Sender.Padding.Bottom:=StrToIntDef(Params.Values[tcs_Padding_Bottom],Sender.Padding.Bottom);
      {$IFEND}
    end;
    Sender.FixedColFullFrame:=_StrToBool(Params.Values[tcs_FixedColFullFrame],Sender.FixedColFullFrame);
    Sender.GlowSize:=StrToIntDef(Params.Values[tcs_GlowSize],Sender.GlowSize);

    Sender.ItemFrame.BackColor:=_StrToColor(Params.Values[tcs_ItemFrame + tcs_BackColor],Sender.ItemFrame.BackColor);
    Sender.ItemFrame.FrameColor:=_StrToColor(Params.Values[tcs_ItemFrame + tcs_FrameColor],Sender.ItemFrame.FrameColor);
    Sender.ItemFrame.FrameMargin:=StrToIntDef(Params.Values[tcs_ItemFrame + tcs_FrameMargin],Sender.ItemFrame.FrameMargin);
    Sender.ItemFrame.FrameSize:=StrToIntDef(Params.Values[tcs_ItemFrame + tcs_FrameSize],Sender.ItemFrame.FrameSize);
    Sender.ItemFrame.RoundedSize:=StrToIntDef(Params.Values[tcs_ItemFrame + tcs_RoundedSize],Sender.ItemFrame.RoundedSize);

    Sender.HoverFrame.BackColor:=_StrToColor(Params.Values[tcs_HoverFrame + tcs_BackColor],Sender.HoverFrame.BackColor);
    Sender.HoverFrame.FrameColor:=_StrToColor(Params.Values[tcs_HoverFrame + tcs_FrameColor],Sender.HoverFrame.FrameColor);
    Sender.HoverFrame.FrameMargin:=StrToIntDef(Params.Values[tcs_HoverFrame + tcs_FrameMargin],Sender.HoverFrame.FrameMargin);
    Sender.HoverFrame.FrameSize:=StrToIntDef(Params.Values[tcs_HoverFrame + tcs_FrameSize],Sender.HoverFrame.FrameSize);
    Sender.HoverFrame.RoundedSize:=StrToIntDef(Params.Values[tcs_HoverFrame + tcs_RoundedSize],Sender.HoverFrame.RoundedSize);

    Sender.SelectedFrame.BackColor:=_StrToColor(Params.Values[tcs_SelectedFrame + tcs_BackColor],Sender.SelectedFrame.BackColor);
    Sender.SelectedFrame.FrameColor:=_StrToColor(Params.Values[tcs_SelectedFrame + tcs_FrameColor],Sender.SelectedFrame.FrameColor);
    Sender.SelectedFrame.FrameMargin:=StrToIntDef(Params.Values[tcs_SelectedFrame + tcs_FrameMargin],Sender.SelectedFrame.FrameMargin);
    Sender.SelectedFrame.FrameSize:=StrToIntDef(Params.Values[tcs_SelectedFrame + tcs_FrameSize],Sender.SelectedFrame.FrameSize);
    Sender.SelectedFrame.RoundedSize:=StrToIntDef(Params.Values[tcs_SelectedFrame + tcs_RoundedSize],Sender.SelectedFrame.RoundedSize);

    try
      if not IgnoreDimensions then
      begin
        Sender.Alignment:=TAlignment(StrToIntDef(Params.Values[tcs_Alignment],Integer(Sender.Alignment)));
        Sender.Direction:=TTagCloudDirection(StrToIntDef(Params.Values[tcs_Direction],Integer(Sender.Direction)));
      end;
      Sender.HoverCursor:=TCursor(StrToIntDef(Params.Values[tcs_HoverCursor],Integer(Sender.HoverCursor)));
      Sender.ItemFrame.FrameStyle:=TPenStyle(StrToIntDef(Params.Values[tcs_ItemFrame + tcs_FrameStyle],Integer(Sender.ItemFrame.FrameStyle)));
      Sender.HoverFrame.FrameStyle:=TPenStyle(StrToIntDef(Params.Values[tcs_HoverFrame + tcs_FrameStyle],Integer(Sender.HoverFrame.FrameStyle)));
      Sender.SelectedFrame.FrameStyle:=TPenStyle(StrToIntDef(Params.Values[tcs_SelectedFrame + tcs_FrameStyle],Integer(Sender.SelectedFrame.FrameStyle)));
    except
      // nop
    end;

    Sender.Colors.BeginUpdate;
    try
      Sender.Colors.Clear;
      i:=0;
      tmpClrs:=tcs_Colors+IntToStr(i)+'_';
      while Params.IndexOfName(tmpClrs+tcs_Color)>=0 do
      begin
        Sender.Colors.AddColor(
          _StrToColor(Params.Values[tmpClrs+tcs_Color],Sender.Font.Color),
          _StrToColor(Params.Values[tmpClrs+tcs_BackColor],clNone),
          _StrToColor(Params.Values[tmpClrs+tcs_FrameColor],clNone));
        Inc(i);
        tmpClrs:=tcs_Colors+IntToStr(i)+'_';
      end;
    finally
      Sender.Colors.EndUpdate;
    end;

    Sender.CustomScale.BeginUpdate;
    try
      tmpScale:=tcs_CustomScale+'0_';
      tmpClrs:=trim(Params.Values[tmpScale+tcs_Font_Size]);
      if Length(tmpClrs)>0 then
      begin
        if Sender.CustomScale.Count=0 then
        begin
          i:=0;
          repeat
            Sender.CustomScale.AddScale(
              StrToIntDef(Params.Values[tmpScale+tcs_ValueFrom],-maxint),
              StrToIntDef(Params.Values[tmpScale+tcs_ValueTo],maxint),
              StrToIntDef(tmpClrs,Sender.Font.Size),
              _StrToColor(Params.Values[tmpScale+tcs_Color],Sender.Font.Color),
              _StrToColor(Params.Values[tmpScale+tcs_BackColor],clNone),
              _StrToColor(Params.Values[tmpScale+tcs_FrameColor],clNone));
            Inc(i);
            tmpScale:=tcs_CustomScale+IntToStr(i)+'_';
            tmpClrs:=trim(Params.Values[tmpScale+tcs_Font_Size]);
          until Length(tmpClrs)=0;
        end
        else
        begin
          for i:=0 To Sender.CustomScale.Count-1 do
          begin
            tmpScale:=tcs_CustomScale+IntToStr(i)+'_';
            tmpClrs:=trim(Params.Values[tmpScale+tcs_Font_Size]);
            if Length(tmpClrs)>0 then
            begin
              if not IgnoreDimensions then
                Sender.CustomScale[i].FontSize:=StrToIntDef(tmpClrs,Sender.Font.Size);
              Sender.CustomScale[i].Color:=_StrToColor(Params.Values[tmpScale+tcs_Color],Sender.Font.Color);
              Sender.CustomScale[i].BackColor:=_StrToColor(Params.Values[tmpScale+tcs_BackColor],clNone);
              Sender.CustomScale[i].FrameColor:=_StrToColor(Params.Values[tmpScale+tcs_FrameColor],clNone);
            end
            else
              break;
          end;
        end;
      end
      else
      if (Sender.Colors.Count>0) and (Sender.CustomScale.Count>0) then
        for i:=0 To Sender.CustomScale.Count-1 do
          if i<Sender.Colors.Count then
          begin
            Sender.CustomScale[i].Color:=Sender.Colors[i].Color;
            Sender.CustomScale[i].BackColor:=Sender.Colors[i].BackColor;
            Sender.CustomScale[i].FrameColor:=Sender.Colors[i].FrameColor;
          end;
    finally
      Sender.CustomScale.EndUpdate;
    end;

  end;
  inherited;
end;

end.

