{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmLibrary
Purpose  : This unit provides commonly used routines not specific to any
           component or control in the "rmComponent" set and is also required by
           various other "rm" Controls
Date     : 03-05-2000
Author   : Ryan J. Mills
Version  : 1.80
================================================================================}

unit rmLibrary;

interface

{$I CompilerDefines.INC}

uses windows, graphics, controls, classes, forms, sysutils, consts, Math, dialogs;

{$IFDEF BD5}
  const
     {$EXTERNALSYM COLOR_HOTLIGHT}
     COLOR_HOTLIGHT = 26;

     {$EXTERNALSYM COLOR_GRADIENTACTIVECAPTION}
     COLOR_GRADIENTACTIVECAPTION = 27;

     {$EXTERNALSYM COLOR_GRADIENTINACTIVECAPTION}
     COLOR_GRADIENTINACTIVECAPTION = 28;

     clHotLight = TColor(COLOR_HOTLIGHT or $80000000) ;
     clGradientActiveCaption = TColor(COLOR_GRADIENTACTIVECAPTION or $80000000) ;
     clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or $80000000) ;
{$endif}

{Conversion Functions}
function strtochar(st: string) : char;
function chartostr(ch: char) : string;
function BoolToStr(B: Boolean) : string;
function StrToBool(st: String) : boolean;
function SizeInt(x: comp) : string;

{Math functions}
function IntInRange(Item, Low, High: integer) : boolean;
function CompInRange(Item, Low, High: Comp) : Boolean;
function SetInRange(Item, Low, High: integer) : integer;
function GreaterThanInt(x1, x2: integer) : integer;
function LessThanInt(x1, x2: integer) : integer;
function GreaterThanFloat(x1, x2: Double) : Double;
function LessThanFloat(x1, x2: Double) : Double;

{ Rect }
function RectWidth(Rect:TRect):integer;
function RectHeight(Rect:TRect):integer;
function RectDiameter(Rect:TRect) : integer; //Was CalculateImageDiameter

{ MediaID }
const
     //File System IDs
   fstFat = 1; //Fat
   fstHPFS = 2; //High Performance File System
   fstNTFS = 3; //NT File System
   fstUnknown = 0; //UnknownFileSystem;

     //Media Info IDs
   mispc = 1; //sectorsperclustor
   mibps = 2; //bytespersector
   mifc = 3; //freeclusters
   mitc = 4; //totalclustors
   mids = 5; //disksize
   midf = 6; //diskfree
   midsn = 7; //diskserialnumber
   mics = 8; //clustersize
   mifst = 9; //filesystemtype
   mifsf = 10; //filesystemflags
   mifsfnl = 11; //filesystemfilenamelength

function GetMediaInfo(Drive: byte; info: byte) : longint;

{ Graphiks }
type
   TColorsArray = array of TColor;
   TGradientFill = (gfLinear, gfRectangle, gfRoundRect, gfOval, gfRadial);

procedure CreateColorArray(Color1, Color2 : TColor; Steps:Integer; var Colors:TColorsArray);
procedure GradientFillColors(Canvas: TCanvas; Colors:TColorsArray; R:TRect);
procedure GradientFill(Canvas: TCanvas; FBeginColor, FEndColor: TColor; R: TRect; FillStyle:TGradientFill = gfLinear) ;
procedure RotateImage(BitmapRotated, BitmapOriginal: tbitmap; angle: integer) ;
procedure ReplaceColors(var bmp: TBitmap; BackGrnd, ForeGrnd: TColor) ;
procedure DrawGrayText(Canvas: TCanvas; Text: string; var R: TRect; Flags: Integer) ;
procedure RotateText(Canvas: TCanvas; Text: string;  var R: TRect; Angle: Integer);

{ CRC32 }
function GetFileCRC32(filename: string) : longint;
function GetStrCRC32(Data: string) : longint;
function GetStrmCRC32(Data: TStream) : longint;

{WinSock Functions}
function LocalIP: string;
function LocalName: string;

{ Strings }
type
   TrmCharSet = set of char;

function ParseSection(ParseLine: string; ParseNum: Integer; ParseSep: Char) : string;
function CountSections(St: string; ParseSep: char) : integer;
function LeadingZero(Value, PadWidth: integer) : string;
function PadLeft(Data: string; PadWidth: integer) : string;
function PadRight(Data: string; PadWidth: integer) : string;
function StripString(StrData: string; StripChars: TrmCharSet; ReplaceChar: char) : string;
function rmDateTimeToStr(x: TDateTime) : string;
function MaskStrCmp(mask, s: string) : boolean;
function Soundex(InStr: string; StandardResult: boolean = True; Precision : integer = 4) : string;

{ Shell }
type
   TShortcutDetails = record
      Arguments: string;
      Description: string;
      HotKey: TShortCut;
      IconFile: string;
      IconIndex: Integer;
      LinkName: string;
      ShowCommand: TWindowState;
      WorkingDirectory: string;
   end;

function ReadShortCutFile(filename: string) : TShortcutDetails;
procedure WriteShortCutFile(filename: string; Details: TShortcutDetails) ;

function GetFileType(FileName: string) : string;
function GetFileIcon(FileName: string; SmallImage: Boolean) : TIcon;
function GetFileImageIndex(Filename: string; SmallImage: boolean) : integer;
function GetFileImages(AImageList: TImageList; SmallImage: boolean) : boolean;

{ OS }
type
   TWinOSVersion = (wosWin3x, wosWin95, wosWin98, wosWinNT3x, wosWinNT4x, wosWinNT2k, wosWinNTX, wosUnknown) ;
   TrmHkey = (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS,
      HKEY_PERFORMANCE_DATA, HKEY_CURRENT_CONFIG, HKEY_DYN_DATA) ;

function WinOSVersion: TWinOSVersion;

implementation

uses shellapi, ShlObj, WinSock, ActiveX;

var
   ficon: TIcon;

{ Generic }

const
   HOTKEYF_SHIFT = $01;
   HOTKEYF_CONTROL = $02;
   HOTKEYF_ALT = $04;
   HOTKEYF_EXT = $08;

function GetWindowState(State: integer) : TWindowState;
begin
   case State of
      SW_SHOWNORMAL, SW_SHOW, SW_RESTORE: Result := wsNormal;
      SW_SHOWMINIMIZED, SW_MINIMIZE, SW_SHOWMINNOACTIVE: Result := wsMinimized;
      SW_SHOWMAXIMIZED, SW_MAX: Result := wsMaximized;
   else
      Result := wsNormal;
   end;
end;

function KeyToShortCut(HotKey: word) : TShortCut;
begin
   Result := WordRec(HotKey) .Lo;
   if ((HotKey shr 8) and HOTKEYF_SHIFT) <> 0 then Inc(Result, scShift) ;
   if ((HotKey shr 8) and HOTKEYF_CONTROL) <> 0 then Inc(Result, scCtrl) ;
   if ((HotKey shr 8) and HOTKEYF_ALT) <> 0 then Inc(Result, scAlt) ;
end;

function ShortCutToKey(ShortCut: TShortCut) : Word;
var
   Key: byte;
   Shift: byte;
begin
   Key := ShortCut and not (scShift + scCtrl + scAlt) ;
   Shift := 0;
   if ShortCut and scShift <> 0 then Inc(Shift, HOTKEYF_SHIFT) ;
   if ShortCut and scCtrl <> 0 then Inc(Shift, HOTKEYF_CONTROL) ;
   if ShortCut and scAlt <> 0 then Inc(Shift, HOTKEYF_ALT) ;
   Result := MakeWord(Key, Shift) ;
end;

{Conversion Functions}

function strtochar(st: string) : char;
var
   wStr: string;
   wInt: integer;
begin
   wstr := trim(st) ;
   if wstr[1] = '#' then
   begin
      delete(wstr, 1, 1) ;

      wInt := strtoint(wstr) ;

      if (wInt >= 0) and (wInt < 256) then
         result := chr(byte(wInt) )
      else
         raise EConvertError.create('Value must be between 0 and 255') ;
   end
   else
   begin
      if length(wstr) > 1 then
         raise EConvertError.create('Invalid property value')
      else
         result := wstr[1];
   end;
end;

function chartostr(ch: char) : string;
begin
   if (integer(ch) < 33) or (integer(ch) > 255) then
      result := '#' + inttostr(byte(ch) )
   else
      result := ch;
end;

function BoolToStr(B: Boolean) : string;
begin
   case b of
      true: result := 'True';
      false: result := 'False';
   end;
end;

function StrToBool(st: String) : boolean;
begin
   st := lowercase(trim(st) ) ;
   result := (st = 'true') ;
end;

function SizeInt(x: comp) : string;
begin
   if CompInRange(x, 1023, 0) then
      result := floattostrf(x, fffixed, 5, 0)
   else if CompInRange(x, 1048575, 1024) then
      result := floattostrf((x / 1024) , fffixed, 5, 0) + ' K'
   else if CompInRange(x, 1073741823, 1048576) then
      result := floattostrf((x / 1048576) , fffixed, 5, 1) + ' M'
   else if x >= 1073741824 then
      result := floattostrf((x / 1073741824) , fffixed, 5, 2) + ' G'
   else
      result := '';
end;

{ Rect }
function RectWidth(Rect:TRect):integer;
begin
   result := (rect.Right-rect.Left);
end;

function RectHeight(Rect:TRect):integer;
begin
   result := (rect.Bottom-rect.Top);
end;

function RectDiameter(Rect:TRect) : integer;
begin
   try
      result := round( RectHeight(Rect) / Sin( arcTan( RectHeight(Rect) / RectWidth(Rect) ) ) ) + 2
   except
      result := 0;
   end;
end;

{MediaID}

function GetMediaInfo(Drive: byte; info: byte) : longint;
var
   path, volumename, filesystem: string;
   n1, n2, n3, n4: Cardinal;
begin
   result := -1;
   if drive > 0 then
      path := chr(drive + 64) + ':\';
   if info in [mispc, mibps, mifc, mitc, mids, midf, mics] then
   begin
      if getdiskfreespace(pchar(path) , n1, n2, n3, n4) = true then
      begin
         case info of
            mispc: Result := n1;
            mibps: Result := n2;
            mifc: Result := n3;
            mitc: Result := n4;
            mids: Result := n1 * n2 * n4;
            midf: Result := n1 * n2 * n3;
            mics: Result := n1 * n2;
         end
      end
      else
         Result := -1;
   end
   else if info in [midsn] then
   begin
      setlength(volumename, 255) ;
      setlength(filesystem, 10) ;
      getvolumeinformation(pchar(path) , pchar(volumename) , 255, @n1, n2, n3, pchar(filesystem) , 10) ;
      case info of
         midsn: Result := n1;
         mifsf: Result := n3;
         mifsfnl: Result := n2;
         mifst:
            begin
               if filesystem = 'FAT' then
                  Result := fstFAT
               else if filesystem = 'HPFS' then
                  Result := fstHPFS
               else if filesystem = 'NTFS' then
                  Result := fstNTFS
               else
                  Result := fstUnknown;
            end;
      else
         Result := -1;
      end;
   end;
end;

{Graphiks}

procedure CreateColorArray(Color1, Color2 : TColor; Steps:Integer; var Colors:TColorsArray);
var
   BeginRGBValue: array[0..2] of Byte;
   RGBDifference: array[0..2] of integer;

   I: Integer;
   Red: Byte;
   Green: Byte;
   Blue: Byte;
begin
   SetLength(Colors, 0);
   SetLength(Colors, Steps);

   BeginRGBValue[0] := GetRValue(ColorToRGB(Color1)) ;
   BeginRGBValue[1] := GetGValue(ColorToRGB(Color1)) ;
   BeginRGBValue[2] := GetBValue(ColorToRGB(Color1)) ;

   RGBDifference[0] := GetRValue(ColorToRGB(Color2)) - BeginRGBValue[0];
   RGBDifference[1] := GetGValue(ColorToRGB(Color2)) - BeginRGBValue[1];
   RGBDifference[2] := GetBValue(ColorToRGB(Color2)) - BeginRGBValue[2];

   for I := 0 to Steps - 1 do
   begin
      Red := BeginRGBValue[0] + MulDiv(I, RGBDifference[0], Steps - 1) ;
      Green := BeginRGBValue[1] + MulDiv(I, RGBDifference[1], Steps - 1) ;
      Blue := BeginRGBValue[2] + MulDiv(I, RGBDifference[2], Steps - 1) ;

      Colors[I] := rgb(Red, Green, Blue);
   end;
end;

procedure GradientFillColors(Canvas: TCanvas; Colors:TColorsArray; R:TRect);
var
   ColorBand: TRect; { Color band rectangular coordinates }
   I: Integer; { Color band index }
   Brush, OldBrush: HBrush;
   wWidth, wHeight : integer;
   Steps : integer;
begin
   ColorBand.Top := R.Top;
   ColorBand.Bottom := R.Bottom;
   ColorBand.Left := R.Left;

   wWidth := RectWidth(R);
   wHeight := RectHeight(ColorBand);

   Steps := high(Colors);
   for I := 0 to Steps-1 do
   begin { iterate through the color bands }
      ColorBand.Right := R.Left + MulDiv(I + 1, wWidth, Steps);

      Brush := CreateSolidBrush(Colors[I]) ;
      OldBrush := SelectObject(Canvas.handle, Brush) ;
      try
         PatBlt(Canvas.handle, ColorBand.Left, ColorBand.Top, ColorBand.Right - ColorBand.Left, wHeight, PATCOPY) ;
      finally
         SelectObject(Canvas.handle, OldBrush) ;
         DeleteObject(Brush) ;
      end;

      ColorBand.Left := ColorBand.Right;
   end; { iterate through the color bands }
end;

const
   FNumColors = $FF; //8-bit

type
   pTRGBArray = ^TRGBArray;
   TRGBArray = array[0..0] of TRGBTriple; {This syntax is as bad as C}

procedure GradientFill(Canvas: TCanvas; FBeginColor, FEndColor: TColor; R: TRect; FillStyle:TGradientFill = gfLinear) ;
var
  { Set up working variables }
   wBeginRGBValue: array[0..2] of Byte; { Begin RGB values }
   wRGBDifference: array[0..2] of integer; { Difference between begin and end }
                                           { RGB values                       }
   wColorBand: TRect; { Color band rectangular coordinates }
   wIndex: Integer; { Color band index }
   wRed: Byte; { Color band Red value }
   wGreen: Byte; { Color band Green value }
   wBlue: Byte; { Color band Blue value }
   wBrush, wOldBrush: HBrush;
   wPen, wOldPen: HPen;
   wWidth, wHeight : integer;
   wSteps : integer;
   wAngle : Double;
   wLastx, wLasty : integer;
   wIncAngle : double;
begin
  { Extract the begin RGB values }
  { Set the Red, Green and Blue colors }
   wBeginRGBValue[0] := GetRValue(ColorToRGB(FBeginColor) ) ;
   wBeginRGBValue[1] := GetGValue(ColorToRGB(FBeginColor) ) ;
   wBeginRGBValue[2] := GetBValue(ColorToRGB(FBeginColor) ) ;
  { Calculate the difference between begin and end RGB values }
   wRGBDifference[0] := GetRValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[0];
   wRGBDifference[1] := GetGValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[1];
   wRGBDifference[2] := GetBValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[2];

  wSteps := 0;
  wHeight := 0;
  wWidth := 0;
  wLastx := 0;
  wLasty := 0;

  case FillStyle of
    gfLinear :
       begin
          { Calculate the color band's top and bottom coordinates }
          { for Left To Right fills }
           wColorBand.Top := R.Top;
           wColorBand.Bottom := R.Bottom;
           wColorBand.Left := R.Left;
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wSteps := fNumColors;
       end;
    gfRectangle,
    gfOval,
    gfRoundRect :
       begin
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wSteps := lessthanint(wHeight div 2, wWidth div 2);
           wColorBand := R;
       end;
    gfRadial :
       begin
           wSteps := 360;
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wColorBand := R;
           wLastx := (wWidth shr 1);
           wLasty := (wheight shr 1);
       end;
  end;

  { Perform the fill }
   for wIndex := 0 to wSteps - 1 do
   begin { iterate through the color bands }
    { Calculate the color band's color }
      if FillStyle <> gfRadial then
      begin
         wRed := wBeginRGBValue[0] + MulDiv(wIndex, wRGBDifference[0], wSteps - 1) ;
         wGreen := wBeginRGBValue[1] + MulDiv(wIndex, wRGBDifference[1], wSteps - 1) ;
         wBlue := wBeginRGBValue[2] + MulDiv(wIndex, wRGBDifference[2], wSteps - 1) ;
      end
      else
      begin
         if wIndex < (wSteps shr 1)-1 then
         begin
            wRed := wBeginRGBValue[0] + MulDiv(wIndex, wRGBDifference[0], (wSteps shr 1) - 1) ;
            wGreen := wBeginRGBValue[1] + MulDiv(wIndex, wRGBDifference[1], (wSteps shr 1) - 1) ;
            wBlue := wBeginRGBValue[2] + MulDiv(wIndex, wRGBDifference[2], (wSteps shr 1) - 1) ;
         end
         else
         begin
            wRed := wBeginRGBValue[0] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[0], (wSteps shr 1)) ;
            wGreen := wBeginRGBValue[1] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[1], (wSteps shr 1)) ;
            wBlue := wBeginRGBValue[2] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[2], (wSteps shr 1)) ;
         end;
      end;

      wBrush := CreateSolidBrush(RGB(wRed, wGreen, wBlue) ) ;
      wOldBrush := SelectObject(Canvas.handle, wBrush) ;
      try
         case FillStyle of
           gfLinear :
              begin
                 wColorBand.Right := R.Left + MulDiv(wIndex + 1, wWidth, wSteps) ;
                 PatBlt(Canvas.handle, wColorBand.Left, wColorBand.Top, wColorBand.Right - wColorBand.Left, wHeight, PATCOPY) ;
                 wColorBand.Left := wColorBand.Right;
              end;
           gfRectangle :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    Rectangle(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfRoundRect :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    RoundRect(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom, 30, 30);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfOval :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    Ellipse(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfRadial :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    if wIndex < wSteps then
                      wincangle := 0.027
                    else
                      wincangle := 0;
                    wAngle := (2 * Pi * ((wIndex+1) / wSteps));
                    pie(Canvas.handle, 0, 0, wWidth, wHeight, Round(((wWidth shr 1)) * (1-Cos(wAngle+wIncAngle))), Round(((wheight shr 1)) * (1-Sin(wAngle+wIncAngle))), wlastx, wlasty);
                    wlastx := Round(((wWidth shr 1)) * (1-Cos(wAngle)));
                    wlasty := Round(((wheight shr 1)) * (1-Sin(wAngle)));
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
         end;
      finally
         SelectObject(Canvas.handle, wOldBrush) ;
         DeleteObject(wBrush) ;
      end;
   end; { iterate through the color bands }
end; { GradientFill }

procedure RotateImage(BitmapRotated, BitmapOriginal: tbitmap; angle: integer) ;
var
   cosTheta: DOUBLE;
   i: INTEGER;
   iRotationAxis: INTEGER;
   iOriginal: INTEGER;
   iPrime: INTEGER;
   iPrimeRotated: INTEGER;
   j: INTEGER;
   jRotationAxis: INTEGER;
   jOriginal: INTEGER;
   jPrime: INTEGER;
   jPrimeRotated: INTEGER;
   RowOriginal: pTRGBArray;
   RowRotated: pTRGBArray;
   sinTheta: DOUBLE;
   Theta: DOUBLE; {radians}

begin
   BitmapRotated.assign(Bitmaporiginal) ;
   BitmapRotated.canvas.fillrect(rect(0, 0, bitmaprotated.width, bitmaprotated.height) ) ;
   iRotationAxis := BitmapRotated.Width div 2;
   jRotationAxis := BitmapRotated.Height div 2;
   Theta := angle * PI / 180;
   sinTheta := SIN(Theta) ;
   cosTheta := COS(Theta) ;
   for j := BitmapRotated.Height - 1 downto 0 do
   begin
      RowRotated := pTRGBArray(BitmapRotated.Scanline[j]) ;
      jPrime := 2 * (j - jRotationAxis) + 1;
      for i := BitmapRotated.Width - 1 downto 0 do
      begin
         iPrime := 2 * (i - iRotationAxis) + 1;
         iPrimeRotated := ROUND(iPrime * CosTheta - jPrime * sinTheta) ;
         jPrimeRotated := ROUND(iPrime * sinTheta + jPrime * cosTheta) ;
         iOriginal := (iPrimeRotated - 1) div 2 + iRotationAxis;
         jOriginal := (jPrimeRotated - 1) div 2 + jRotationAxis;
{$R-}
         if (iOriginal >= 0) and (iOriginal <= BitmapOriginal.Width - 1) and
            (jOriginal >= 0) and (jOriginal <= BitmapOriginal.Height - 1) then
         begin
            RowOriginal := pTRGBArray(BitmapOriginal.Scanline[jOriginal]) ;
            RowRotated[i].rgbtBlue := RowOriginal[iOriginal].rgbtBlue;
            RowRotated[i].rgbtGreen := RowOriginal[iOriginal].rgbtGreen;
            RowRotated[i].rgbtRed := RowOriginal[iOriginal].rgbtRed;
         end
         else
         begin
            RowRotated[i].rgbtBlue := 192; {assign "corner" color}
            RowRotated[i].rgbtGreen := 192;
            RowRotated[i].rgbtRed := 192;
         end
{$R+}
      end
   end;
end;

procedure RotateText(Canvas: TCanvas; Text: string; var R: TRect; Angle: Integer) ;
var
   wMetric: TTextMetric;
   LogFont: TLogFont;
   NewFont, OldFont: TFont;
   bigger: integer;
   bmp1, bmp2: tbitmap;
   tw, th: integer;
   OldTA : integer;
   NewTA : integer;
begin
   GetTextMetrics(Canvas.handle, wMetric) ;

   Bigger := RectDiameter(rect(0,0,canvas.textwidth(text),canvas.TextHeight(text))) ;

   if (wMetric.tmPitchAndFamily and tmpf_TrueType <> 0) then
   begin
      OldFont := tFont.create;
      NewFont := tfont.create;
      try
         OldFont.assign(canvas.Font);
         NewFont.assign(canvas.Font);
         windows.GetObject(NewFont.Handle, SizeOf(TLogFont) , @LogFont) ;
         try
            LogFont.lfEscapement := Angle * 10;
            LogFont.lfOrientation := Angle * 10;
            NewFont.handle := CreateFontIndirect(LogFont) ;
            Canvas.Font.Assign(NewFont);

            NewTA := TA_CENTER or TA_BASELINE or TA_NOUPDATECP;
            OldTA := GetTextAlign(Canvas.Handle);
            try
               SetTextAlign(Canvas.Handle, NewTA);
               TextOut(Canvas.Handle, r.Left + (bigger shr 1),  r.Top + (bigger shr 1), pchar(text), length(text));
            finally
               SetTextAlign(Canvas.Handle, OldTA);
            end;
            Canvas.Font.Assign(OldFont);
         finally
            windows.DeleteObject(NewFont.handle) ;
         end;
      finally
         NewFont.free;
         OldFont.free;
      end;
   end
   else
   begin
      bmp1 := tbitmap.create;
      try
         bmp1.pixelformat := pf24bit;
         bmp1.Canvas.Brush.Assign(Canvas.Brush);
         bmp1.Canvas.Font.Assign(Canvas.font);

         th := bmp1.canvas.TextHeight(text) ;
         tw := bmp1.canvas.textwidth(text) ;

         Bigger := RectDiameter(rect(0,0,tw,th)) ;

         bmp1.width := bigger;
         bmp1.Height := bigger;

         NewTA := TA_CENTER or TA_BASELINE or TA_NOUPDATECP;
         OldTA := GetTextAlign(bmp1.Canvas.Handle);
         try
            SetTextAlign(bmp1.Canvas.Handle, NewTA);
            TextOut(bmp1.Canvas.Handle, r.left + (bigger shr 1), r.top + (bigger shr 1), pchar(text), length(text));
         finally
            SetTextAlign(bmp1.Canvas.Handle, OldTA);
         end;

         bmp2 := tbitmap.create;
         try
            rotateimage(bmp2, bmp1, angle) ;
            bmp2.Transparent := true;
            canvas.Draw(0,0, bmp2);
         finally
            bmp2.free;
         end;
      finally
         bmp1.free;
      end;
   end;
end;

procedure ReplaceColors(var bmp: TBitmap; BackGrnd, ForeGrnd: TColor) ;
var
   x, y: integer;
   P: PByteArray;
   wColor: TColor;
   wValue: integer;
begin
   bmp.PixelFormat := pf24bit;

   for y := 0 to bmp.height - 1 do
   begin
      P := bmp.ScanLine[y];
      x := 0;
      while x < (bmp.width * 3) do
      begin
         wColor := rgb(p[x + 2], p[x + 1], p[x]) ;
         case wColor of
            clwhite: wValue := ColorToRGB(backGrnd) ;
            clBlack: wValue := ColorToRGB(ForeGrnd) ;
         else
            wValue := ColorToRGB(backGrnd) ;
         end;
         p[x] := getBValue(wValue) ;
         p[x + 1] := getGValue(wValue) ;
         p[x + 2] := getRvalue(wValue) ;
         inc(x, 3)
      end;
   end;
end;

procedure DrawGrayText(Canvas: TCanvas; Text: string; var R: TRect; Flags: Integer) ;
var
   oldStyle: TBrushStyle;
   oldFontColor: TColor;
begin
   oldStyle := Canvas.Brush.Style;
   oldFontColor := Canvas.Font.Color;
   try
      if flags and dt_calcRect = 0 then
      begin
         with Canvas do
         begin
            Brush.Style := bsClear;
            OffsetRect(R, 1, 1) ;
            Font.Color := clBtnHighlight;
            DrawText(Handle, PChar(Text) , Length(Text), R, Flags) ;
            OffsetRect(R, -1, -1) ;
            Font.Color := clBtnShadow;
            DrawText(Handle, PChar(Text) , Length(text), R, flags) ;
         end;
      end
      else
      begin
         DrawText(Canvas.Handle, PChar(Text) , Length(Text), R, Flags) ;
      end;
      R.Bottom := r.Bottom+1;
      r.Right := r.Right+1;
   finally
      Canvas.Brush.Style := oldStyle;
      Canvas.Font.Color := oldFontColor;
   end;
end;

{ CRC32 }

type
   crc32tabletype = array[0..255] of longint;

var
   fcrctable: crc32tabletype;

function crc32gen: crc32tabletype;
var
   crc, poly: longint;
   i, j: integer;
   crc32table: crc32tabletype;
begin
   fillchar(crc32table, sizeof(crc32table) , 0) ;
   poly := longint($EDB88320) ;
   for i := 0 to 255 do
   begin
      crc := i;
      for j := 8 downto 1 do
      begin
         if (crc and 1) = 1 then
            crc := (crc shr 1) xor poly
         else
            crc := crc shr 1;
      end;
      crc32table[i] := crc;
   end;
   result := crc32table;
end;

function GetFileCRC32(filename: string) : longint;
var
   crc: longint;
   bytesread, checked: integer;
   buffer: array[0..10239] of byte;
   fin: file;
begin
   assign(fin, filename) ;
   filemode := 0;
   reset(fin, 1) ;

   crc := longint($FFFFFFFF) ;

   while eof(fin) = false do
   begin
      blockread(fin, buffer, sizeof(buffer) , bytesread) ;

      checked := 0;

      while checked < bytesread do
      begin
         crc := ((crc shr 8) and $FFFFFF) xor fcrctable[(crc xor buffer[checked]) and $FF];
         inc(checked) ;
      end;
   end;
   close(fin) ;
   result := (crc xor longint($FFFFFFFF) ) ;
end;

function GetStrCRC32(Data: string) : longint;
var
   crc: longint;
   index, datalength: integer;
begin
   crc := longint($FFFFFFFF) ;

   datalength := length(data) ;
   index := 1;
   while index <= datalength do
   begin
      crc := ((crc shr 8) and $FFFFFF) xor fcrctable[(crc xor byte(data[index]) ) and $FF];
      inc(index) ;
   end;
   result := (crc xor Integer($FFFFFFFF) ) ;
end;

function GetStrmCRC32(Data: TStream) : longint;
var
   crc: longint;
   db: byte;
begin
   Data.Position := 0;

   crc := longint($FFFFFFFF) ;

   while data.Position < data.size do
   begin
      Data.ReadBuffer(db, 1) ;
      crc := ((crc shr 8) and $FFFFFF) xor fcrctable[(crc xor db) and $FF];
   end;
   result := (crc xor longint($FFFFFFFF) ) ;
end;

{Winsock functions}

function LocalIP: string;
type
   TaPInAddr = array[0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
var
   phe: PHostEnt;
   pptr: PaPInAddr;
   Buffer: array[0..63] of char;
   I: Integer;
   GInitData: TWSADATA;
begin
   Result := '';
   WSAStartup($101, GInitData) ;
   GetHostName(Buffer, SizeOf(Buffer) ) ;
   phe := GetHostByName(buffer) ;
   if phe = nil then Exit;
   pptr := PaPInAddr(Phe^.h_addr_list) ;
   I := 0;
   while pptr^[I] <> nil do
   begin
      result := StrPas(inet_ntoa(pptr^[I]^) ) ;
      Inc(I) ;
   end;
   WSACleanup;
end;

function LocalName: string;
type
   TaPInAddr = array[0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
var
   phe: PHostEnt;
   Buffer: array[0..63] of char;
   GInitData: TWSADATA;
begin
   Result := '';
   WSAStartup($101, GInitData) ;
   GetHostName(Buffer, SizeOf(Buffer) ) ;
   phe := GetHostByName(buffer) ;
   if phe = nil then Exit;
   result := StrPas(phe^.h_name) ;
   WSACleanup;
end;

{ Strings }

function CountSections(St: string; ParseSep: char) : integer;
var
   iPos: LongInt;
begin
   result := 0;
   while (st <> '') do
   begin
      iPos := Pos(ParseSep, st) ;
      if iPos > 0 then
      begin
         Delete(st, 1, iPos) ;
         inc(result) ;
      end
      else
      begin
         if st <> '' then
         begin
            inc(result) ;
            st := '';
         end;
      end;
   end;
end;

function ParseSection(ParseLine: string; ParseNum: Integer; ParseSep: Char) : string;
var
   iPos: LongInt;
   i: Integer;
   tmp: string;

begin
   tmp := ParseLine;
   for i := 1 to ParseNum do
   begin
      iPos := Pos(ParseSep, tmp) ;
      if iPos > 0 then
      begin
         if i = ParseNum then
         begin
            Result := Copy(tmp, 1, iPos - 1) ;
            Exit;
         end
         else
         begin
            Delete(tmp, 1, iPos) ;
         end;
      end
      else if ParseNum > i then
      begin
         Result := '';
         Exit;
      end
      else
      begin
         Result := tmp;
         Exit;
      end;
   end;
end; { ParseSection }

function LeadingZero(Value, PadWidth: integer) : string;
begin
   try
      result := inttostr(value) ;
      while length(result) < PadWidth do
         result := '0' + result;
   except
      result := '';
   end;
end;

function PadLeft(Data: string; PadWidth: integer) : string;
begin
   result := data;
   while length(result) < PadWidth do
      result := ' ' + result;
end;

function PadRight(Data: string; PadWidth: integer) : string;
begin
   result := data;
   while length(result) < PadWidth do
      result := result + ' ';
end;

function StripString(StrData: string; StripChars: TrmCharSet; ReplaceChar: char) : string;
var
   loop: integer;
   wlen: integer;
begin
   result := '';
   loop := 0;
   wLen := length(StrData) ;
   while loop < wLen do
   begin
      inc(loop) ;
      if StrData[loop] in StripChars then
         result := result + ReplaceChar
      else
         result := result + StrData[loop];
   end;
end;

function rmDateTimeToStr(x: TDateTime) : string;
var
   y, mth, d, h, m, s, ms: word;
begin
   DecodeDate(x, y, mth, d) ;
   DecodeTime(x, h, m, s, ms) ;
   result := inttostr(y) + inttostr(mth) + inttostr(d) + inttostr(h) + inttostr(m) + inttostr(s) + inttostr(ms) ;
end;

function MaskStrCmp(mask, s: string) : boolean;
begin
   while mask <> '' do
   begin
      case mask[1] of
         '?':
            begin
               if s = '' then
               begin
                  result := false;
                  exit;
               end;
               delete(s, 1, 1) ;
               delete(mask, 1, 1) ;
            end;
         '*':
            begin
               while (mask <> '') and (mask[1] = '*') do
                  delete(mask, 1, 1) ;
               if (mask = '') then
               begin
                  result := true;
                  exit;
               end;
               if (mask <> '?') then
               begin
                  while (s <> '') and (s[1] <> mask[1]) do
                  begin
                     if (s = '') then
                     begin
                        result := false;
                        exit;
                     end
                     else
                        delete(s, 1, 1) ;
                  end;
                  delete(s, 1, 1) ;
                  delete(mask, 1, 1) ;
               end;
            end;
      else
         if ((s = '') and (mask <> '') ) or
            ((s <> '') and (mask = '') ) or
            (s[1] <> mask[1]) then
         begin
            result := false;
            exit;
         end;
         delete(s, 1, 1) ;
         delete(mask, 1, 1) ;
      end;
   end;
   if ((s = '') and (mask <> '') ) or
      ((s <> '') and (mask = '') ) then
      result := false
   else
      result := true;
end;

function Soundex(InStr: string; StandardResult: boolean = True; Precision : integer = 4) : string;
const
   Table1: array[0..25] of char = '01230120022455012623010202';
                                 { ABCDEFGHIJKLMNOPQRSTUVWXYZ }
var
   lchar: char;
   wchar: char;
   count: integer;
begin
   InStr := UpperCase(Trim(Instr)) ;
   result := '';
   count := length(instr) ;

   while (Instr <> '') and (count > 0) do
   begin
      if not (Instr[count] in ['A'..'Z']) then
         delete(Instr, count, 1) ;
      dec(count) ;
   end;

   if Instr = '' then
      exit;

   if (length(instr) > 1) and (instr[1] = 'P') and (instr[2] = 'H') then
   begin
      instr[1] := 'F';
      instr[2] := 'A';
   end;

   lChar := #0;

   if StandardResult then
   begin
      result := instr[1];
      lchar := instr[1];
      delete(instr, 1, 1) ;
   end;

   Count := 0;
   while (instr <> '') and (count < Precision) do
   begin
      if (instr[1] in ['A'..'Z']) and (instr[1] <> lchar) then
      begin
         wchar := Table1[ord(instr[1]) - 65];
         if wchar <> '0' then
         begin
            result := result + wchar;
            inc(count) ;
         end;
         lchar := instr[1];
      end;
      delete(instr, 1, 1) ;
   end;
   while length(result) < Precision do
      result := result + '0';
end;

{ Shell }

const
   CLSID_ShellLink: TGUID = (D1: $00021401; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46) ) ;
   IID_IShellLink: TGUID = (D1: $000214EE; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46) ) ;
   IID_IPersistFile: TGUID = (D1: $0000010B; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46) ) ;

   ShowCommands: array[TWindowState] of Integer = (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED) ;

function GetFileType(FileName: string) : string;
var
   sfi: SHFILEINFO;
   wname: string;
   wattr: DWord;
   wExt: string;
begin
   if filename = '' then
      raise exception.create('No file specified') ;

   result := '';
   wattr := GetFileAttributes(pchar(filename) ) ;
   if wattr = $FFFFFFFF then
   begin
      wattr := FILE_ATTRIBUTE_NORMAL;
      wname := extractfileext(filename) ;
   end
   else
      wname := filename;

   fillchar(sfi, sizeof(SHFILEINFO) , 0) ;
   SHGetFileInfo(pchar(wname) , wattr, sfi, sizeof(sfi) , SHGFI_USEFILEATTRIBUTES or SHGFI_TYPENAME) ;
   result := Trim(sfi.szTypeName) ;
   if result = '' then
   begin
      wExt := uppercase(extractfileext(wname) ) + ' File';
      delete(wExt, 1, 1) ;
      result := Trim(wExt) ;
   end;
end;

function GetFileIcon(FileName: string; SmallImage: Boolean) : TIcon;
var
   fext: string;
   sfi: SHFILEINFO;
   ShellImages: TImageList;
   wname: string;
   wattr: DWord;
begin
   result := nil;

   if Filename = '' then
      raise exception.create('No file specified') ;

   wattr := GetFileAttributes(pchar(Filename) ) ;
   if wattr = $FFFFFFFF then
   begin
      wattr := FILE_ATTRIBUTE_NORMAL;

      fext := extractfileext(Filename) ;
      while ansilowercase(fext) = '.lnk' do
      begin
         with ReadShortCutFile(filename) do
         begin
            fext := extractfileext(LinkName) ;
         end;

         if fext = '' then
            exit;

      end;
      wname := fext;
   end
   else
      wname := Filename;

   ShellImages := timagelist.create(nil) ;
   try
      ShellImages.ShareImages := true;
      fillchar(sfi, sizeof(SHFILEINFO) , 0) ;

      if smallImage then
         ShellImages.handle := SHGetFileInfo(pchar(wname) , wattr, sfi, sizeof(sfi) , SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_SMALLICON)
      else
         ShellImages.handle := SHGetFileInfo(pchar(wname) , wattr, sfi, sizeof(sfi) , SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_LARGEICON) ;

      ShellImages.GetIcon(sfi.iIcon, fIcon) ;
      result := ficon
   finally
      ShellImages.free;
   end;
end;

function GetFileImageIndex(Filename: string; SmallImage: boolean) : integer;
var
   fext: string;
   sfi: SHFILEINFO;
   wname: string;
   wattr: DWord;
begin
   result := -1;

   if Filename = '' then
      raise exception.create('No file specified') ;

   wattr := GetFileAttributes(pchar(Filename) ) ;
   if wattr = $FFFFFFFF then
   begin
      wattr := FILE_ATTRIBUTE_NORMAL;

      fext := extractfileext(Filename) ;
      while ansilowercase(fext) = '.lnk' do
      begin
         with ReadShortCutFile(filename) do
         begin
            fext := extractfileext(LinkName) ;
         end;

         if fext = '' then
            exit;

      end;
      wname := fext;
   end
   else
      wname := Filename;

   fillchar(sfi, sizeof(SHFILEINFO) , 0) ;

   if smallImage then
      SHGetFileInfo(pchar(wname) , wattr, sfi, sizeof(sfi) , SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_SMALLICON)
   else
      SHGetFileInfo(pchar(wname) , wattr, sfi, sizeof(sfi) , SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_LARGEICON) ;

   result := sfi.iIcon;
end;

function GetFileImages(AImageList: TImageList; SmallImage: boolean) : boolean;
var
   sfi: SHFILEINFO;
   wname: string;
begin
  {I want all file images}
   wname := '';
   AImageList.ShareImages := true;
   fillchar(sfi, sizeof(SHFILEINFO) , 0) ;

   if smallImage then
      AImageList.handle := SHGetFileInfo(pchar(wname) , 0, sfi, sizeof(sfi) , SHGFI_SYSICONINDEX or SHGFI_SMALLICON)
   else
      AImageList.handle := SHGetFileInfo(pchar(wname) , 0, sfi, sizeof(sfi) , SHGFI_SYSICONINDEX or SHGFI_LARGEICON) ;

   Result := AImageList.handle <> 0;
end;

function ReadShortCutFile(filename: string) : TShortcutDetails;
var
   Str: array[0..MAX_PATH] of Char;
   Index: integer;
   ShellLink: IShellLink;
   PersistFile: IPersistFile;
   FindData: TWin32FindData;
begin
   FillChar(result, sizeof(TShortcutdetails) , 0) ;

   CoInitialize(nil) ;
   if Succeeded(COCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLink, ShellLink) ) then
   begin
      try
         if Succeeded(ShellLink.QueryInterface(IID_IPersistFile, PersistFile) ) then
         begin
            StringToWideChar(FileName, @Str, SizeOf(Str) ) ;
            if PersistFile.Load(@Str, STGM_READ) = NOERROR then
            begin
               ;
               ShellLink.GetArguments(Str, MAX_PATH) ;
               result.Arguments := Str;
               ShellLink.GetDescription(Str, MAX_PATH) ;
               result.Description := Str;
               ShellLink.GetHotKey(Word(result.HotKey) ) ;
               result.HotKey := KeyToShortCut(result.HotKey) ;
               ShellLink.GetIconLocation(Str, MAX_PATH, Index) ;
               result.IconFile := Str;
               result.IconIndex := Index;
               ShellLink.GetPath(Str, MAX_PATH, FindData, SLGP_UNCPRIORITY) ;
               result.LinkName := Str;
               ShellLink.GetShowCmd(Index) ;
               result.ShowCommand := GetWindowState(Index) ;
               ShellLink.GetWorkingDirectory(Str, MAX_PATH) ;
               result.WorkingDirectory := Str;
            end
            else
               raise exception.create('Error opening Shortcut.') ;
         end
         else
            raise exception.create('Error opening Shortcut.') ;

      finally
         CoUninitialize;
      end;
   end
   else
      raise exception.create('Error opening Shortcut.') ;
end;

procedure WriteShortCutFile(filename: string; Details: TShortcutDetails) ;
var
   Str: array[0..MAX_PATH] of Char;
   ShellLink: IShellLink;
   PersistFile: IPersistFile;
begin
   CoInitialize(nil) ;
   if Succeeded(COCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLink, ShellLink) ) then
   begin
      try
         if Succeeded(ShellLink.QueryInterface(IID_IPersistFile, PersistFile) ) then
         begin
            StrPCopy(@Str, Details.Arguments) ;
            ShellLink.SetArguments(Str) ;
            StrPCopy(@Str, Details.Description) ;
            ShellLink.SetDescription(Str) ;
            ShellLink.SetHotKey(ShortCutToKey(Details.HotKey) ) ;
            StrPCopy(@Str, Details.IconFile) ;
            ShellLink.SetIconLocation(Str, Details.IconIndex) ;
            StrPCopy(@Str, Details.LinkName) ;
            ShellLink.SetPath(Str) ;
            ShellLink.SetShowCmd(ShowCommands[Details.ShowCommand]) ;
            StrPCopy(@Str, Details.WorkingDirectory) ;
            ShellLink.SetWorkingDirectory(Str) ;
            StringToWideChar(FileName, @Str, SizeOf(Str) ) ;
            if not (PersistFile.Save(@Str, false) = NOERROR) then
               raise Exception.create('Error creating Shortcut.') ;
         end;
      finally
         CoUninitialize;
      end;
   end;
end;

{ OS }

function WinOSVersion: TWinOSVersion;
var
   verinfo: TOSVersionInfo;
begin
   verinfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo) ;
   GetVersionEX(verinfo) ;

   case verinfo.dwPlatformId of
      VER_PLATFORM_WIN32s:
         Result := wosWin3x;
      VER_PLATFORM_WIN32_WINDOWS:
         begin
            if ((verinfo.dwMajorVersion > 4) or
               ((verinfo.dwMajorVersion = 4) and (verinfo.dwMinorVersion >= 10) ) ) then
               Result := wosWin98
            else
               Result := wosWin95;
         end;
      VER_PLATFORM_WIN32_NT:
         begin
            if (verinfo.dwMajorVersion = 3) then
               Result := wosWinNT3x
            else if (verinfo.dwMajorVersion = 4) then
               Result := wosWinNT4x
            else if (verinfo.dwMajorVersion = 5) then
               result := wosWinNT2k
            else
               Result := wosWinNTX;
         end;
   else
      Result := wosUnknown;
   end;
end;

{ Math Functions }

function IntInRange(Item, Low, High: integer) : boolean;
begin
   result := (item >= low) and (item <= high) ;
end;

function CompInRange(Item, Low, High: Comp) : boolean;
begin
   result := (item >= low) and (item <= high) ;
end;

function SetInRange(Item, Low, High: integer) : integer;
begin
   if low > high then
      raise ERangeError.Create(inttostr(low) + ' is not less than or equal to ' + inttostr(high) ) ;

   result := item;

   if item < low then
      result := low;

   if item > high then
      result := high;
end;

function GreaterThanInt(x1, x2: integer) : integer;
begin
   if x1 > x2 then
      result := x1
   else
      result := x2;
end;

function LessThanInt(x1, x2: integer) : integer;
begin
   if x1 < x2 then
      result := x1
   else
      result := x2;
end;

function GreaterThanFloat(x1, x2: Double) : Double;
begin
   if x1 > x2 then
      result := x1
   else
      result := x2;
end;

function LessThanFloat(x1, x2: Double) : Double;
begin
   if x1 < x2 then
      result := x1
   else
      result := x2;
end;

initialization
   fIcon := TIcon.create;
   fcrctable := crc32gen;

finalization
   fIcon.free;

end.

