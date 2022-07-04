unit ConverterUnit;

interface

{$DEFINE _DEBUGNAMES}
{$DEFINE FIXMSSTYLESBUGS}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, SXPNGUtils, SXSkinLibrary, SXZipUtils,
  SXSkinUtils;

const
{$IFDEF DEBUGNAMES}
  _button           = 'button';
  _groupbox         = 'groupbox';
  _checkbox         = 'checkbox';
  _radiobutton      = 'radiobutton';
  _edit             = 'edit';
  _form             = 'form';
  _formbuttons      = 'formbuttons';
  _leftframe        = 'leftframe';
  _rightframe       = 'rightframe';
  _bottomframe      = 'bottomframe';
  _caption          = 'caption';
  _resize           = 'resize';
  _smallresize      = 'smallresize';
  _other            = 'other';
  _dropdown         = 'dropdown';
  _smallcaption     = 'smallcaption';
  _smallleftframe   = 'smallleftframe';
  _smallrightframe  = 'smallrightframe';
  _smallbottomframe = 'smallbottomframe';
  _minimize         = 'minimize';
  _maximize         = 'maximize';
  _restore          = 'restore';
  _close            = 'close';
  _help             = 'help';
  _smallclose       = 'smallclose';
  _glyph            = 'glyph';
  _updown           = 'updown';
  _upbutton         = 'upbutton';
  _downbutton       = 'downbutton';
  _leftbutton       = 'leftbutton';
  _rightbutton      = 'rightbutton';
{$ELSE}
  _button           = 'b';
  _groupbox         = 'g';
  _checkbox         = 'cb';
  _radiobutton      = 'rb';
  _edit             = 'e';
  _form             = 'f';
  _formbuttons      = 'fb';
  _leftframe        = 'lf';
  _rightframe       = 'rf';
  _bottomframe      = 'bf';
  _caption          = 'c';
  _resize           = 'rs';
  _smallresize      = 'sr';
  _other            = 'o';
  _dropdown         = 'dd';
  _smallcaption     = 'sc';
  _smallleftframe   = 'slf';
  _smallrightframe  = 'srf';
  _smallbottomframe = 'sbf';
  _minimize         = 'm';
  _maximize         = 'x';
  _restore          = 'r';
  _close            = 'c';
  _help             = 'h';
  _smallclose       = 'sc';
  _glyph            = 'g';
  _updown           = 'ud';
  _upbutton         = 'udu';
  _downbutton       = 'udd';
  _leftbutton       = 'udl';
  _rightbutton      = 'udr';
{$ENDIF}

type

  TSXSavedBitmap=class
   public
    Bitmap:TBitmap;
    Color:String;
    Path:String;
    RedirectPath:String;
    destructor Destroy; override;
  end;

  TSXSavedBitmapList=class
   protected
    FItem:TList;
    function Get(Index:Integer):TSXSavedBitmap;
    procedure Put(Index:Integer;Item:TSXSavedBitmap);
    function GetCount:Integer;
   public
    function GetIndexByPath(const Path:String):Integer;
    procedure Add(SXSavedBitmap:TSXSavedBitmap);
    function FastSaveBitmap(Bitmap:TBitmap;const Path:String;CanRedirect:Boolean):Boolean;
    function GetSavedBitmapPath(const Path:String):String;
    function GetSavedBitmapColor(const Path:String):String;
    procedure Delete(Index:Integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property Item[Index:Integer]:TSXSavedBitmap read Get write Put; default;
    property Count:Integer read GetCount;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox5: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DirPath:String;
    Module:HModule;
    function LoadBitmap(Name:String):TBitmap;
    procedure ProcessFile(const FileName:String);
    procedure ProcessSkinScheme(const Name,FileName:String);
    procedure ProcessGeneral(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessLabel(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessButton(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessCheckBox(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessRadioButton(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessGroupBox(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessEdit(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessForm(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessFormSmallCaption(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessFormButtons(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessUpDown(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure ProcessSpinEdit(const MyName:String;SkinIni,TmpIni:TIniFile);
    procedure SaveFormCaptionButton(const APathPrefix,ButtonName,Section,MyName:String;
               SkinIni,TmpIni:TIniFile;var ButtonWidth,ButtonHeight:Integer);
    procedure SaveFormCaptionButtonGlyphs(const APathPrefix,FName,Layout:String;
               Count:Integer;SkinIni,TmpIni:TIniFile;var ImageWidth:Integer;
               GTransp:Boolean;TranspColor:TColor);
    procedure ProcessFormSmallCaptionButtons(const MyName:String;SkinIni,TmpIni:TIniFile);
    function ShortPath(const FullPath,MyName:String):String;
    function FullPath(const ShortPath,MyName:String):String;
    function SavedShort(const ShortPath,MyName:String):String;
    function BoxTileShort(const ShortPath,Name,MyName:String):String;
    procedure SaveRedirectedBoxTile(const Section,EIndex,ShortPath,MyName:String;SkinIni:TIniFile);
  end;

var
  Form1: TForm1;
  SavedBitmaps:TSXSavedBitmapList;

implementation

{$R *.dfm}

{ TSXSavedBitmap }

destructor TSXSavedBitmap.Destroy;
begin
 Bitmap.Free;
 inherited;
end;

{ TSXSavedBitmapList }

function TSXSavedBitmapList.Get(Index:Integer):TSXSavedBitmap;
begin
 Result:=TSXSavedBitmap(FItem[Index]);
end;

procedure TSXSavedBitmapList.Put(Index:Integer;Item:TSXSavedBitmap);
begin
 FItem[Index]:=Item;
end;

function TSXSavedBitmapList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

function TSXSavedBitmapList.GetIndexByPath(const Path:String):Integer;
var A:Integer;
begin
 for A:=0 to Count-1 do
  if Item[A].Path=Path then
   begin
    Result:=A;
    exit;
   end;
 Result:=-1;  
end;

procedure TSXSavedBitmapList.Add(SXSavedBitmap:TSXSavedBitmap);
begin
 FItem.Add(SXSavedBitmap);
end;

procedure TSXSavedBitmapList.Delete(Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

function SameBitmaps(B1,B2:TBitmap):Boolean;
var   Y:Integer;
   Size:Integer;
  P1,P2:Pointer;
begin
 if (B1=nil) or (B2=nil) or (B1.PixelFormat<>B2.PixelFormat) then
  begin
   Result:=False;
   exit;
  end;
 Result:=(B1.Width=B2.Width) and (B1.Height=B2.Height);
 if not Result then exit;
 Size:=1;
 case B1.PixelFormat of
  pf1bit:  Size:=B1.Width div 8;
  pf4bit:  Size:=B1.Width div 2;
  pf8bit:  Size:=B1.Width;
  pf15bit: Size:=B1.Width*2;
  pf16bit: Size:=B1.Width*2;
  pf24bit: Size:=B1.Width*3;
  pf32bit: Size:=B1.Width*4;
 end;
 for Y:=0 to B1.Height-1 do
  begin
   P1:=B1.ScanLine[Y];
   P2:=B2.ScanLine[Y];
   Result:=CompareMem(P1,P2,Size);
   if not Result then exit;
  end;
{ for X:=0 to B1.Width-1 do
  for Y:=0 to B1.Height-1 do
   if B1.Canvas.Pixels[X,Y]<>B2.Canvas.Pixels[X,Y] then
    begin
     Result:=False;
     exit;
    end;}
end;

function TSXSavedBitmapList.FastSaveBitmap(Bitmap:TBitmap;const Path:String;CanRedirect:Boolean):Boolean;
var A:Integer;
   SB:TSXSavedBitmap;
begin
 SB:=TSXSavedBitmap.Create;
 SB.Path:=Path;
 if CanRedirect then
  begin
   for A:=0 to Count-1 do
    if (Item[A].RedirectPath='') and (Item[A].Color='') and SameBitmaps(Bitmap,Item[A].Bitmap) then
     begin
      SB.RedirectPath:=Item[A].Path;
      Add(SB);
      Result:=True;
      exit;
     end;
  end;
 SB.Bitmap:=TBitmap.Create;
 SB.Bitmap.Assign(Bitmap);
 Add(SB); 
 Result:=False;
end;

function TSXSavedBitmapList.GetSavedBitmapPath(const Path:String):String;
var A:Integer;
begin
 A:=GetIndexByPath(Path);
 if A<0 then
  begin
   Result:='';
   exit;
  end;
 if Item[A].RedirectPath='' then Result:=Path else
  Result:=Item[A].RedirectPath;
end;

function TSXSavedBitmapList.GetSavedBitmapColor(const Path:String):String;
var A:Integer;
begin
 A:=GetIndexByPath(Path);
 if A<0 then
  begin
   Result:='';
   exit;
  end;
 Result:=Item[A].Color;
end;

procedure TSXSavedBitmapList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

constructor TSXSavedBitmapList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXSavedBitmapList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

/////////////////////////////////////////////////////////////////////////

function ConvertColor(S:String):String;
var A,R,G,B:Integer;
begin
 A:=Pos(' ',S);
 R:=StrToIntDef(Copy(S,1,A-1),0);
 Delete(S,1,A);
 A:=Pos(' ',S);
 G:=StrToIntDef(Copy(S,1,A-1),0);
 B:=StrToIntDef(Copy(S,A+1,MaxInt),0);
 Result:='#'+IntToHex(R,2)+IntToHex(G,2)+IntToHex(B,2);
end;

function GetTColor(S:String):TColor;
var A,R,G,B:Integer;
begin
 A:=Pos(' ',S);
 R:=StrToIntDef(Copy(S,1,A-1),0);
 Delete(S,1,A);
 A:=Pos(' ',S);
 G:=StrToIntDef(Copy(S,1,A-1),0);
 B:=StrToIntDef(Copy(S,A+1,MaxInt),0);
 Result:=R or (G shl 8) or (B shl 16);
end;

function ConvertRect(S:String):TRect;
var A:Integer;
begin
 S:=StringReplace(S,' ','',[rfReplaceAll]);
 A:=Pos(',',S);
 Result.Left:=StrToIntDef(Copy(S,1,A-1),0);
 Delete(S,1,A);
 A:=Pos(',',S);
 Result.Right:=StrToIntDef(Copy(S,1,A-1),0);
 Delete(S,1,A);
 A:=Pos(',',S);
 Result.Top:=StrToIntDef(Copy(S,1,A-1),0);
 Result.Bottom:=StrToIntDef(Copy(S,A+1,MaxInt),0);
end;

function ConvertPoint(S:String):TPoint;
var A:Integer;
begin
 S:=StringReplace(S,' ','',[rfReplaceAll]);
 A:=Pos(',',S);
 Result.X:=StrToIntDef(Copy(S,1,A-1),0);
 Result.Y:=StrToIntDef(Copy(S,A+1,MaxInt),0);
end;

procedure ConvertFont(S:String;var FontName:String;var FontSize:Integer;var FontStyle:TFontStyles);
var A:Integer;
begin
 FontSize:=10;
 FontStyle:=[];
 A:=Pos(',',S);
 if A<=0 then
  begin
   FontName:=Trim(S);
   exit;
  end;
 FontName:=Trim(Copy(S,1,A-1));
 Delete(S,1,A);
 S:=Trim(S);
 A:=Pos(',',S);
 if A>0 then
  begin
   FontSize:=StrToIntDef(Trim(Copy(S,1,A-1)),10);
   Delete(S,1,A);
   S:=LowerCase(Trim(S));
   if Pos('bold',S)>0 then
    Include(FontStyle,fsBold);
   if Pos('italic',S)>0 then
    Include(FontStyle,fsItalic);
   if Pos('underline',S)>0 then
    Include(FontStyle,fsUnderline);
  end else FontSize:=StrToIntDef(S,10);
end;

function ConvertFontStyle(FS:TFontStyles):String;
begin
 Result:='';
 if fsBold in FS then Result:=Result+'b';
 if fsItalic in FS then Result:=Result+'i';
 if fsUnderline in FS then Result:=Result+'u';
end;

function CorrectFont(const S:String):String;
const GF:array[1..29]of String = ('Arial',
                                  'Arial Black',
                                  'Arial Narrow',
                                  'Book Antiqua',
                                  'Bookman Old Style',
                                  'Century Gothic',
                                  'Comic Sans MS',
                                  'Courier',
                                  'Courier New',
                                  'Franklin Gothic Book',
                                  'Franklin Gothic Medium',
                                  'Garamond',
                                  'Gautami',
                                  'Georgia',
                                  'Lucida Console',
                                  'Lucida Sans',
                                  'Microsoft Sans Serif',
                                  'MS Sans Serif',
                                  'Monotype Corsiva',
                                  'Symbol',
                                  'Small Fonts',
                                  'Tahoma',
                                  'Times New Roman',
                                  'Trebuchet MS',
                                  'Verdana',
                                  'Webdings',
                                  'WingDings',
                                  'Wingdings 2',
                                  'Wingdings 3');
var A:Integer;
begin
 for A:=Low(GF) to High(GF) do
  if SameText(S,GF[A]) then
   begin
    Result:=S;
    exit;
   end;
 Result:='Tahoma';
end;

function CorrectAlignment(S:String):String;
begin
 S:=LowerCase(Trim(S));
 if S='center' then Result:='Center' else
  if S='right' then Result:='Right' else
   Result:='Left';
end;

function TForm1.ShortPath(const FullPath,MyName:String):String;
begin
 Result:=Copy(FullPath,length(DirPath+MyName)+2,MaxInt);
end;

function TForm1.FullPath(const ShortPath,MyName:String):String;
begin
 Result:=DirPath+MyName+'\'+ShortPath;
end;

function TForm1.SavedShort(const ShortPath,MyName:String):String;
begin
 Result:=FullPath(ShortPath,MyName);
 Result:=SavedBitmaps.GetSavedBitmapPath(Result);
 Result:=Self.ShortPath(Result,MyName);
end;

function TForm1.BoxTileShort(const ShortPath,Name,MyName:String):String;
var S:String;
    A:Integer;
  Fnd:Boolean;
begin
 S:=StringReplace(ShortPath,Name,'*',[rfReplaceAll]);
 Fnd:=False;
 for A:=length(S) downto 1 do
  if S[A]='*' then
   begin
    if not Fnd then Fnd:=True else
     S:=Copy(S,1,A-1)+Name+Copy(S,A+1,MaxInt);
   end;
 Result:=S;  
end;

procedure TForm1.SaveRedirectedBoxTile(const Section,EIndex,ShortPath,MyName:String;SkinIni:TIniFile);
const Names:array[1..9]of String=('TopLeft','Top','TopRight',
                                  'Left','Center','Right',
                                  'BottomLeft','Bottom','BottomRight');
var RedirShort:array[1..9]of String;
    RedirColor:array[1..9]of String;
    RedirCount:array[1..9]of Integer;
      A,B,MaxI:Integer;
begin
 for A:=1 to 9 do
  begin
   RedirShort[A]:=BoxTileShort(SavedShort(StringReplace(ShortPath,'*',LowerCase(Names[A]),[]),MyName),LowerCase(Names[A]),MyName);
   RedirColor[A]:=SavedBitmaps.GetSavedBitmapColor(FullPath(StringReplace(ShortPath,'*',LowerCase(Names[A]),[]),MyName));
  end;
 for A:=1 to 9 do
  RedirCount[A]:=0;
 for A:=1 to 9 do
  if RedirColor[A]='' then
   begin
    for B:=1 to 9 do
     if (RedirColor[B]='') and (RedirShort[B]=RedirShort[A]) then
      Inc(RedirCount[A]);
   end;
 MaxI:=1;
 for A:=2 to 9 do
  if (RedirShort[MaxI]='') or ((RedirShort[A]<>'') and (RedirCount[A]>RedirCount[MaxI])) then
   MaxI:=A;
 SkinIni.WriteString(Section,'E'+EIndex+'.Paths',RedirShort[MaxI]);
 for A:=1 to 9 do
  if RedirColor[A]<>'' then
   SkinIni.WriteString(Section,'E'+EIndex+'.'+Names[A],ConvertColor(RedirColor[A])) else
  if (RedirShort[A]<>'') and (RedirShort[A]<>RedirShort[MaxI]) then
   SkinIni.WriteString(Section,'E'+EIndex+'.'+Names[A],StringReplace(RedirShort[A],'*',LowerCase(Names[A]),[]));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
  Edit1.Text:=OpenDialog1.FileName;
end;

function BitmapIsOpaque(Bitmap:TBitmap):Boolean;
type PDWORDArray=array of DWORD;
var X,Y:Integer;
begin
 if Bitmap.PixelFormat=pf32bit then
  begin
   for X:=0 to Bitmap.Width-1 do
    for Y:=0 to Bitmap.Height-1 do
     if PDWORDArray(Bitmap.Scanline[Y])[X] shr 24<>255 then
      begin
       Result:=False;
       exit;
      end;
  end;
 for X:=0 to Bitmap.Width-1 do
  for Y:=0 to Bitmap.Height-1 do
   if Bitmap.Canvas.Pixels[X,Y]=clFuchsia then
    begin
     Result:=False;
     exit;
    end;
 Result:=True;
end;

function TForm1.LoadBitmap(Name:String):TBitmap;
type PDWORDArray=array of DWORD;
var A:Integer;
  X,Y:Integer;
  IsT:Boolean;
begin
 Name:=UpperCase(Name);
 for A:=1 to length(Name) do
  if Name[A] in ['.','\','/'] then
   Name[A]:='_';
 Result:=TBitmap.Create;
 try
  Result.LoadFromResourceName(Module,Name);
  if Result.PixelFormat<>pf32bit then
   Result.PixelFormat:=pf24bit else
    begin
     IsT:=True;
     for X:=0 to Result.Width-1 do
      for Y:=0 to Result.Height-1 do
       if PDWORDArray(Result.Scanline[Y])[X] shr 24<>0 then
        begin
         IsT:=False;
         break;
        end;
     if IsT then
      Result.PixelFormat:=pf24bit;
    end;
 except
  Result.Free;
  Result:=nil;
 end;
end;

function IniProp(IniFile:TIniFile;const Sections:array of String;const Name,DefValue:String):String;
const NoValue='~~~';
var A:Integer;
begin
 Result:=NoValue;
 for A:=Low(Sections) to High(Sections) do
  if Sections[A]<>'' then
   begin
    Result:=IniFile.ReadString(Sections[A],Name,NoValue);
    if Result<>NoValue then exit;
   end;
 Result:=DefValue;
end;

function IniClassProp(IniFile:TIniFile;const Section,Name,DefValue:String):String;
var S:array of String;
    A:Integer;
begin
 SetLength(S,1);
 S[0]:=Section;
 for A:=length(Section) downto 1 do
  if (Section[A]='.') or (Section[A]='(') then
   begin
    SetLength(S,High(S)+2);
    S[High(S)]:=Copy(Section,1,A-1);
   end;
 Result:=IniProp(IniFile,S,Name,DefValue);  
end;

function GetSubBitmap(Bitmap:TBitmap;const Layout:String;Count,Index:Integer):TBitmap;
var ARect:TRect;
      W,H:Integer;
begin
 W:=Bitmap.Width;
 H:=Bitmap.Height;
 if SameText(Layout,'vertical') then
  H:=H div Count else
   W:=W div Count;
 Result:=TBitmap.Create;
 Result.PixelFormat:=Bitmap.PixelFormat;
 Result.Width:=W;
 Result.Height:=H;
 ARect:=Rect(0,0,W,H);
 if SameText(Layout,'vertical') then
  OffsetRect(ARect,0,Index*H) else
   OffsetRect(ARect,Index*W,0);
 Result.Canvas.Draw(-ARect.Left,-ARect.Top,Bitmap);
end;

function RR(C:Cardinal):Cardinal;
begin
 Result:= (C and $00FF00) or
         ((C and $FF0000) shr 16) or
         ((C and $0000FF) shl 16);
end;

procedure SaveBitmapRect(Bitmap:TBitmap;ARect:TRect;const FileName:String;
           IsTransp:Boolean;TranspColor:TColor;CanRedirect:Boolean=True);
type PDWORDArray=array of DWORD;
var   T:TPNGObject;
     BB:TBitmap;
    X,Y:Integer;
    PB2:PByte;
 Opaque:Boolean;
      B:Boolean;
begin
 T:=TPNGObject.Create;
 try
  BB:=TBitmap.Create;
  try
   BB.PixelFormat:=Bitmap.PixelFormat;
   BB.Width:=ARect.Right-ARect.Left;
   BB.Height:=ARect.Bottom-ARect.Top;
   BB.Canvas.Draw(-ARect.Left,-ARect.Top,Bitmap);
   if not SavedBitmaps.FastSaveBitmap(BB,FileName,CanRedirect) then
    begin
     Opaque:=BitmapIsOpaque(BB);
     T.Assign(BB);
     if not Opaque then
      begin
       T.CreateAlpha;
       FillChar(T.AlphaScanline[0]^,BB.Width*BB.Height,0);
       for Y:=ARect.Top to ARect.Bottom-1 do
        begin
         PB2:=Pointer(T.AlphaScanline[Y-ARect.Top]);
         for X:=ARect.Left to ARect.Right-1 do
          begin
           if Bitmap.PixelFormat=pf32bit then
            begin
             PB2^:=PDWORDArray(Bitmap.Scanline[Y])[X] shr 24;
            end else
             begin
              if Bitmap.Canvas.Pixels[X,Y]=clFuchsia then
               PB2^:=0 else PB2^:=255;
             end;
           Inc(PB2);
          end;
        end;
      end else
     if IsTransp then
      begin
       B:=False;
       for Y:=ARect.Top to ARect.Bottom-1 do
        begin
         for X:=ARect.Left to ARect.Right-1 do
          if Bitmap.Canvas.Pixels[X,Y]=TranspColor then
           begin
            B:=True;
            break;
           end;
         if B then break;
        end;
       if B then
        begin
         T.CreateAlpha;
         FillChar(T.AlphaScanline[0]^,BB.Width*BB.Height,0);
         for Y:=ARect.Top to ARect.Bottom-1 do
          begin
           PB2:=Pointer(T.AlphaScanline[Y-ARect.Top]);
           for X:=ARect.Left to ARect.Right-1 do
            begin
             if Bitmap.Canvas.Pixels[X,Y]=TranspColor then
              PB2^:=0 else PB2^:=255;
             Inc(PB2);
            end;
          end;
        end;
      end;
     T.SaveToFile(FileName);
    end;
  finally
   BB.Free;
  end;
 finally
  T.Free;
 end;
end;

procedure SaveBitmap(Bitmap:TBitmap;const FileName:String;IsTransp:Boolean;
           TranspColor:TColor;CanRedirect:Boolean=True);
begin
 SaveBitmapRect(Bitmap,Rect(0,0,Bitmap.Width,Bitmap.Height),FileName,IsTransp,TranspColor);
end;

procedure SaveBitmapAsColor(const FileName,Color:String);
var SB:TSXSavedBitmap;
begin
 SB:=TSXSavedBitmap.Create;
 SB.Path:=FileName;
 SB.Color:=Color;
 SavedBitmaps.Add(SB);
end;

function BitmapRectIsSolid(Bitmap:TBitmap;ARect:TRect;var Color:String):Boolean;
var BB:TBitmap;
     C:TColor;
   X,Y:Integer;  
begin
 BB:=TBitmap.Create;
 try
  BB.PixelFormat:=Bitmap.PixelFormat;
  BB.Width:=ARect.Right-ARect.Left;
  BB.Height:=ARect.Bottom-ARect.Top;
  BB.Canvas.Draw(-ARect.Left,-ARect.Top,Bitmap);
  if not BitmapIsOpaque(BB) then
   begin
    Result:=False;
    exit;
   end;
  C:=BB.Canvas.Pixels[0,0];
  for X:=0 to BB.Width-1 do
   for Y:=0 to BB.Height-1 do
    if BB.Canvas.Pixels[X,Y]<>C then
     begin
      Result:=False;
      exit;
     end;
  Color:=inttostr(C and $FF)+' '+inttostr((C shr 8) and $FF)+' '+inttostr((C shr 16) and $FF);
  Result:=True;
 finally
  BB.Free;
 end;
end;

procedure SaveBoxTileImages(Bitmap:TBitmap;Left,Right,Top,Bottom:Integer;
           const Path:String;IsTransp:Boolean;TranspColor:TColor;
           CanRedirect:Boolean=True);
var A:Integer;
    C:String;
begin
 if Left+Right>Bitmap.Width then
  begin
   A:=Left+Right-Bitmap.Width;
   Dec(Left,A div 2);
   Dec(Right,A-A div 2);
  end;
 if Top+Bottom>Bitmap.Height then
  begin
   A:=Top+Bottom-Bitmap.Height;
   Dec(Top,A div 2);
   Dec(Bottom,A-A div 2);
  end;
 if Left+Right=Bitmap.Width then
  begin
   if Left>Right then Dec(Left) else Dec(Right);
  end;
 if Top+Bottom=Bitmap.Height then
  begin
   if Top>Bottom then Dec(Top) else Dec(Bottom);
  end;
 if Top>0 then
  begin
   if Left>0 then
    SaveBitmapRect(Bitmap,Rect(0,0,Left,Top),StringReplace(Path,'*','topleft',[]),IsTransp,TranspColor,CanRedirect);
   if Bitmap.Width-Right-Left>0 then
    begin
     if CanRedirect and BitmapRectIsSolid(Bitmap,Rect(Left,0,Bitmap.Width-Right,Top),C) then
      SaveBitmapAsColor(StringReplace(Path,'*','top',[]),C) else
       SaveBitmapRect(Bitmap,Rect(Left,0,Bitmap.Width-Right,Top),StringReplace(Path,'*','top',[]),IsTransp,TranspColor,CanRedirect);
    end;
   if Right>0 then
    SaveBitmapRect(Bitmap,Rect(Bitmap.Width-Right,0,Bitmap.Width,Top),StringReplace(Path,'*','topright',[]),IsTransp,TranspColor,CanRedirect);
  end;
 //
 if Bitmap.Height-Bottom-Top>0 then
  begin
   if Left>0 then
    begin
     if CanRedirect and BitmapRectIsSolid(Bitmap,Rect(0,Top,Left,Bitmap.Height-Bottom),C) then
      SaveBitmapAsColor(StringReplace(Path,'*','left',[]),C) else
       SaveBitmapRect(Bitmap,Rect(0,Top,Left,Bitmap.Height-Bottom),StringReplace(Path,'*','left',[]),IsTransp,TranspColor,CanRedirect);
    end;
   if Bitmap.Width-Right-Left>0 then
    begin
     if CanRedirect and BitmapRectIsSolid(Bitmap,Rect(Left,Top,Bitmap.Width-Right,Bitmap.Height-Bottom),C) then
      SaveBitmapAsColor(StringReplace(Path,'*','center',[]),C) else
       SaveBitmapRect(Bitmap,Rect(Left,Top,Bitmap.Width-Right,Bitmap.Height-Bottom),StringReplace(Path,'*','center',[]),IsTransp,TranspColor,CanRedirect);
    end;
   if Right>0 then
    begin
     if CanRedirect and BitmapRectIsSolid(Bitmap,Rect(Bitmap.Width-Right,Top,Bitmap.Width,Bitmap.Height-Bottom),C) then
      SaveBitmapAsColor(StringReplace(Path,'*','right',[]),C) else
       SaveBitmapRect(Bitmap,Rect(Bitmap.Width-Right,Top,Bitmap.Width,Bitmap.Height-Bottom),StringReplace(Path,'*','right',[]),IsTransp,TranspColor,CanRedirect);
    end;
  end;
 //
 if Bottom>0 then
  begin
   if Left>0 then
    SaveBitmapRect(Bitmap,Rect(0,Bitmap.Height-Bottom,Left,Bitmap.Height),StringReplace(Path,'*','bottomleft',[]),IsTransp,TranspColor,CanRedirect);
   if Bitmap.Width-Right-Left>0 then
    begin
     if CanRedirect and BitmapRectIsSolid(Bitmap,Rect(Left,Bitmap.Height-Bottom,Bitmap.Width-Right,Bitmap.Height),C) then
      SaveBitmapAsColor(StringReplace(Path,'*','bottom',[]),C) else
       SaveBitmapRect(Bitmap,Rect(Left,Bitmap.Height-Bottom,Bitmap.Width-Right,Bitmap.Height),StringReplace(Path,'*','bottom',[]),IsTransp,TranspColor,CanRedirect);
    end;
   if Right>0 then
    SaveBitmapRect(Bitmap,Rect(Bitmap.Width-Right,Bitmap.Height-Bottom,Bitmap.Width,Bitmap.Height),StringReplace(Path,'*','bottomright',[]),IsTransp,TranspColor,CanRedirect);
  end;
end;

procedure TForm1.ProcessGeneral(const MyName:String;SkinIni,TmpIni:TIniFile);
var S:String;
begin
 SkinIni.WriteString('Strings','SkinName',MyName);
 S:=TmpIni.ReadString('SysMetrics','Btnface','');
 if S<>'' then
  begin
   SkinIni.WriteString('Background','Transparent','0');
   SkinIni.WriteString('Background','Element1','E1,SOLID_FILL');
   SkinIni.WriteString('Background','E1.FillColor',ConvertColor(S));
  end;
 //
 SkinIni.WriteString('StdIcon16','Element1','E1,IMAGE');
 SkinIni.WriteString('StdIcon16','E1.Path','_general.zip\stdicon16.png');
 SkinIni.WriteString('StdIcon32','Element1','E1,IMAGE');
 SkinIni.WriteString('StdIcon32','E1.Path','_general.zip\stdicon32.png');
 SkinIni.WriteString('StdIcon48','Element1','E1,IMAGE');
 SkinIni.WriteString('StdIcon48','E1.Path','_general.zip\stdicon48.png');
 SkinIni.WriteString('_Selective_StdIcon','Style1','W<=16,StdIcon16');
 SkinIni.WriteString('_Selective_StdIcon','Style2','W<=32,StdIcon32');
 SkinIni.WriteString('_Selective_StdIcon','Default','StdIcon48');
end;

procedure TForm1.ProcessLabel(const MyName:String;SkinIni,TmpIni:TIniFile);
var S1,S2:String;
begin
 S1:=TmpIni.ReadString('SysMetrics','GrayText','');
 S2:=TmpIni.ReadString('SysMetrics','HighlightText','');
 SkinIni.WriteString('_Label','R_FontColor',ConvertColor(S1));
 SkinIni.WriteString('_Label','R_HasTextShadow','1');
 SkinIni.WriteString('_Label','R_ShadowColor',ConvertColor(S2));
end;

procedure TForm1.ProcessButton(const MyName:String;SkinIni,TmpIni:TIniFile);
var    FName:String;
      Bitmap:TBitmap;
          B2:TBitmap;
          S1:String;
      Layout:String;
       Count:Integer;
      Sizing:String;
          R1:TRect;
    IsTransp:Boolean;
 TranspColor:TColor;
begin
 FName:=IniClassProp(TmpIni,'button.pushbutton','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('BUTTON image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'button.pushbutton','ImageLayout','');
   Sizing:=LowerCase(IniClassProp(TmpIni,'button.pushbutton','SizingType',''));
   Count:=StrToIntDef(IniClassProp(TmpIni,'button.pushbutton','imageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'button.pushbutton','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'button.pushbutton','TransparentColor','255 0 255'));
   R1:=ConvertRect(IniClassProp(TmpIni,'button.pushbutton','SizingMargins',''));
   if SameText(Layout,'vertical') and (Bitmap.Height div Count<=R1.Top+R1.Bottom) then
    R1.Bottom:=Bitmap.Height div Count-R1.Top-1;
   if Count>=1 then
    begin
     //Normal Unfocused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_button+'\nu_*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('Button_NU','Transparent','0') else
     if CheckBox6.Checked then
       SkinIni.WriteString('Button_NU','MouseCapture','ByTransparency');
     B2.Free;
     SkinIni.WriteString('Button_NU','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Button_NU','1',_button+'\nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Button_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Button','NU_Style','Button_NU');
    end;
   if Count>=2 then
    begin
     //Highlighted Unfocused
     B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_button+'\hu_*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('Button_HU','Transparent','0') else
     if CheckBox6.Checked then
       SkinIni.WriteString('Button_HU','MouseCapture','ByTransparency');
     B2.Free;
     SkinIni.WriteString('Button_HU','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Button_HU','1',_button+'\hu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Button_HU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Button','HU_Style','Button_HU');
    end;
   if Count>=3 then
    begin
     //Down Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,2);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_button+'\df_*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('Button_DF','Transparent','0') else
     if CheckBox6.Checked then
       SkinIni.WriteString('Button_DF','MouseCapture','ByTransparency');
     B2.Free;
     SkinIni.WriteString('Button_DF','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Button_DF','1',_button+'\df_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Button_DF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Button','DF_Style','Button_DF');
    end;
   if Count>=4 then
    begin
     //Read-only Unfocused
     B2:=GetSubBitmap(Bitmap,Layout,Count,3);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_button+'\ru_*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('Button_RU','Transparent','0') else
     if CheckBox6.Checked then
       SkinIni.WriteString('Button_RU','MouseCapture','ByTransparency');
     B2.Free;
     SkinIni.WriteString('Button_RU','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Button_RU','1',_button+'\ru_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Button_RU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Button','RU_Style','Button_RU');
    end;
   if Count>=5 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,4);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_button+'\nf_*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('Button_NF','Transparent','0') else
     if CheckBox6.Checked then
       SkinIni.WriteString('Button_NF','MouseCapture','ByTransparency');
     B2.Free;
     SkinIni.WriteString('Button_NF','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Button_NF','1',_button+'\nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Button_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Button','NF_Style','Button_NF');
    end;
   Bitmap.Free;
   //
   R1:=ConvertRect(IniClassProp(TmpIni,'button.pushbutton','ContentMargins',''));
   SkinIni.WriteString('_Button','NU_CaptionLeftOffset',inttostr(R1.Left));
   SkinIni.WriteString('_Button','NU_CaptionRightOffset',inttostr(R1.Right));
   SkinIni.WriteString('_Button','NU_CaptionTopOffset',inttostr(R1.Top));
   SkinIni.WriteString('_Button','NU_CaptionBottomOffset',inttostr(R1.Bottom));
   Inc(R1.Left);  Inc(R1.Top);
   Dec(R1.Right); Dec(R1.Bottom);
   SkinIni.WriteString('_Button','DF_CaptionLeftOffset',inttostr(R1.Left));
   SkinIni.WriteString('_Button','DF_CaptionRightOffset',inttostr(R1.Right));
   SkinIni.WriteString('_Button','DF_CaptionTopOffset',inttostr(R1.Top));
   SkinIni.WriteString('_Button','DF_CaptionBottomOffset',inttostr(R1.Bottom));
   //
   S1:=IniClassProp(TmpIni,'button.pushbutton','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_Button','NU_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.pushbutton(disabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_Button','RU_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.pushbutton(hot)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_Button','HU_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.pushbutton(defaulted)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_Button','NF_FontColor',ConvertColor(S1));
   //
   SkinIni.WriteString('_Button','RU_DefGlyphFilter','Monochrome');
   SkinIni.WriteString('_Button','RU_DefGlyphFilterColor1','#60000000');
   SkinIni.WriteString('_Button','RU_DefGlyphFilterColor2','#60FFFFFF');
   SkinIni.WriteString('_Button','DropDownGlyphOffset','6');
  end else
   Memo1.Lines.Add('Can not process Button::ImageFile=unknown');
 //
 FName:=IniClassProp(TmpIni,'Toolbar.SplitButtonDropdown','GlyphImageFile','');
 Count:=StrToIntDef(IniClassProp(TmpIni,'Toolbar.SplitButtonDropdown','imageCount',''),0);
 IsTransp:=SameText(IniClassProp(TmpIni,'Toolbar.SplitButtonDropdown','Transparent',''),'true');
 TranspColor:=GetTColor(IniClassProp(TmpIni,'Toolbar.SplitButtonDropdown','TransparentColor','255 0 255'));
 Layout:=IniClassProp(TmpIni,'Toolbar.SplitButtonDropdown','imagelayout','');
 Bitmap:=LoadBitmap(FName);
 if Bitmap=nil then
  begin
   Memo1.Lines.Add('DROPDOWN image not found!');
   exit;
  end;
 if Count>=1 then
  begin
   //Normal
   B2:=GetSubBitmap(Bitmap,Layout,Count,0);
   SaveBitmap(B2,DirPath+MyName+'\'+_other+'\'+_dropdown+'_n.png',IsTransp,TranspColor);
   if BitmapIsOpaque(B2) then
    SkinIni.WriteString('DropDown_N','Transparent','0');
   B2.Free;
   SkinIni.WriteString('DropDown_N','Element1','E1,IMAGE');
   SkinIni.WriteString('DropDown_N','E1.Path',SavedShort(_other+'\'+_dropdown+'_n.png',MyName));
   SkinIni.WriteString('_MultiState_DropDown','NStyle','DropDown_N');
  end;
 if Count>=2 then
  begin
   //Highlighted
   B2:=GetSubBitmap(Bitmap,Layout,Count,1);
   SaveBitmap(B2,DirPath+MyName+'\'+_other+'\'+_dropdown+'_h.png',IsTransp,TranspColor);
   if BitmapIsOpaque(B2) then
    SkinIni.WriteString('DropDown_H','Transparent','0');
   B2.Free;
   SkinIni.WriteString('DropDown_H','Element1','E1,IMAGE');
   SkinIni.WriteString('DropDown_H','E1.Path',SavedShort(_other+'\'+_dropdown+'_h.png',MyName));
   SkinIni.WriteString('_MultiState_DropDown','HStyle','DropDown_H');
  end;
 if Count>=3 then
  begin
   //Down
   B2:=GetSubBitmap(Bitmap,Layout,Count,2);
   SaveBitmap(B2,DirPath+MyName+'\'+_other+'\'+_dropdown+'_d.png',IsTransp,TranspColor);
   if BitmapIsOpaque(B2) then
    SkinIni.WriteString('DropDown_D','Transparent','0');
   B2.Free;
   SkinIni.WriteString('DropDown_D','Element1','E1,IMAGE');
   SkinIni.WriteString('DropDown_D','E1.Path',SavedShort(_other+'\'+_dropdown+'_d.png',MyName));
   SkinIni.WriteString('_MultiState_DropDown','DStyle','DropDown_D');
  end;
 if Count>=4 then
  begin
   //Read-only
   B2:=GetSubBitmap(Bitmap,Layout,Count,3);
   SaveBitmap(B2,DirPath+MyName+'\'+_other+'\'+_dropdown+'_r.png',IsTransp,TranspColor);
   if BitmapIsOpaque(B2) then
    SkinIni.WriteString('DropDown_R','Transparent','0');
   B2.Free;
   SkinIni.WriteString('DropDown_R','Element1','E1,IMAGE');
   SkinIni.WriteString('DropDown_R','E1.Path',SavedShort(_other+'\'+_dropdown+'_r.png',MyName));
   SkinIni.WriteString('_MultiState_DropDown','RStyle','DropDown_R');
  end;
 Bitmap.Free;
 if CheckBox7.Checked then
  begin
   SkinIni.WriteString('_Button','HighlightInButtonStepsNum','8');
   SkinIni.WriteString('_Button','HighlightOutButtonStepsNum','4');
   SkinIni.WriteString('_Button','CheckButtonStepsNum','8');
   SkinIni.WriteString('_Button','UncheckButtonStepsNum','6');
   SkinIni.WriteString('_Button','DownButtonStepsNum','4');
   SkinIni.WriteString('_Button','UpButtonStepsNum','2');
   SkinIni.WriteString('_Button','EnableButtonStepsNum','8');
   SkinIni.WriteString('_Button','DisableButtonStepsNum','8');
   SkinIni.WriteString('_Button','FocusButtonStepsNum','8');
   SkinIni.WriteString('_Button','UnfocusButtonStepsNum','4');
   SkinIni.WriteString('_Button','HighlightInGlyphStepsNum','8');
   SkinIni.WriteString('_Button','HighlightOutGlyphStepsNum','4');
   SkinIni.WriteString('_Button','CheckGlyphStepsNum','8');
   SkinIni.WriteString('_Button','UncheckGlyphStepsNum','6');
   SkinIni.WriteString('_Button','DownGlyphStepsNum','4');
   SkinIni.WriteString('_Button','UpGlyphStepsNum','2');
   SkinIni.WriteString('_Button','EnableGlyphStepsNum','8');
   SkinIni.WriteString('_Button','DisableGlyphStepsNum','8');
   SkinIni.WriteString('_Button','FocusGlyphStepsNum','8');
   SkinIni.WriteString('_Button','UnfocusGlyphStepsNum','4');
  end;
end;

procedure TForm1.ProcessCheckBox(const MyName:String;SkinIni,TmpIni:TIniFile);
var    FName:String;
      Bitmap:TBitmap;
          B2:TBitmap;
          S1:String;
      Layout:String;
       Count:Integer;
    IsTransp:Boolean;
 TranspColor:TColor;
begin
 FName:=IniClassProp(TmpIni,'button.checkbox','ImageFile1','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('CHECKBOX image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'button.checkbox','imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'button.checkbox','imageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'button.checkbox','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'button.checkbox','TransparentColor','255 0 255'));
   if Count>=1 then
    begin
     //Normal Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\nuu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_NUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_NUU','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_NUU','E1.Path',SavedShort(_checkbox+'\nuu.png',MyName));
     SkinIni.WriteString('_CheckBox','NUU_Glyph','CheckBoxGlyph_NUU');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','NUStyle','CheckBoxGlyph_NUU');
    end;
   if Count>=2 then
    begin
     //Highlighted Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\huu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_HUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_HUU','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_HUU','E1.Path',SavedShort(_checkbox+'\huu.png',MyName));
     SkinIni.WriteString('_CheckBox','HUU_Glyph','CheckBoxGlyph_HUU');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','HUStyle','CheckBoxGlyph_HUU');
    end;
   if Count>=3 then
    begin
     //Down Focused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,2);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\dfu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_DFU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_DFU','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_DFU','E1.Path',SavedShort(_checkbox+'\dfu.png',MyName));
     SkinIni.WriteString('_CheckBox','DFU_Glyph','CheckBoxGlyph_DFU');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','DUStyle','CheckBoxGlyph_DFU');
    end;
   if Count>=4 then
    begin
     //Read-only Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,3);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\ruu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_RUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_RUU','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_RUU','E1.Path',SavedShort(_checkbox+'\ruu.png',MyName));
     SkinIni.WriteString('_CheckBox','RUU_Glyph','CheckBoxGlyph_RUU');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','RUStyle','CheckBoxGlyph_RUU');
    end;
   //
   if Count>=5 then
    begin
     //Normal Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,4);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\nuc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_NUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_NUC','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_NUC','E1.Path',SavedShort(_checkbox+'\nuc.png',MyName));
     SkinIni.WriteString('_CheckBox','NUC_Glyph','CheckBoxGlyph_NUC');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','NCStyle','CheckBoxGlyph_NUC');
    end;
   if Count>=6 then
    begin
     //Highlighted Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,5);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\huc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_HUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_HUC','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_HUC','E1.Path',SavedShort(_checkbox+'\huc.png',MyName));
     SkinIni.WriteString('_CheckBox','HUC_Glyph','CheckBoxGlyph_HUC');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','HCStyle','CheckBoxGlyph_HUC');
    end;
   if Count>=7 then
    begin
     //Down Focused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,6);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\dfc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_DFC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_DFC','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_DFC','E1.Path',SavedShort(_checkbox+'\dfc.png',MyName));
     SkinIni.WriteString('_CheckBox','DFC_Glyph','CheckBoxGlyph_DFC');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','DCStyle','CheckBoxGlyph_DFC');
    end;
   if Count>=8 then
    begin
     //Read-only Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,7);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\ruc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_RUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_RUC','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_RUC','E1.Path',SavedShort(_checkbox+'\ruc.png',MyName));
     SkinIni.WriteString('_CheckBox','RUC_Glyph','CheckBoxGlyph_RUC');
     SkinIni.WriteString('_MultiStateCheck_CheckBox','RCStyle','CheckBoxGlyph_RUC');
    end;
   //
   if Count>=9 then
    begin
     //Normal Unfocused Grayed
     B2:=GetSubBitmap(Bitmap,Layout,Count,8);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\nug.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_NUG','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_NUG','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_NUG','E1.Path',SavedShort(_checkbox+'\nug.png',MyName));
     SkinIni.WriteString('_CheckBox','NUG_Glyph','CheckBoxGlyph_NUG');
    end;
   if Count>=10 then
    begin
     //Highlighted Unfocused Grayed
     B2:=GetSubBitmap(Bitmap,Layout,Count,9);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\hug.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_HUG','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_HUG','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_HUG','E1.Path',SavedShort(_checkbox+'\hug.png',MyName));
     SkinIni.WriteString('_CheckBox','HUG_Glyph','CheckBoxGlyph_HUG');
    end;
   if Count>=11 then
    begin
     //Down Focused Grayed
     B2:=GetSubBitmap(Bitmap,Layout,Count,10);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\dfg.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_DFG','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_DFG','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_DFG','E1.Path',SavedShort(_checkbox+'\dfg.png',MyName));
     SkinIni.WriteString('_CheckBox','DFG_Glyph','CheckBoxGlyph_DFG');
    end;
   if Count>=12 then
    begin
     //Read-only Unfocused Grayed
     B2:=GetSubBitmap(Bitmap,Layout,Count,11);
     SaveBitmap(B2,DirPath+MyName+'\'+_checkbox+'\rug.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('CheckBoxGlyph_RUG','Transparent','0');
     B2.Free;
     SkinIni.WriteString('CheckBoxGlyph_RUG','Element1','E1,IMAGE');
     SkinIni.WriteString('CheckBoxGlyph_RUG','E1.Path',SavedShort(_checkbox+'\rug.png',MyName));
     SkinIni.WriteString('_CheckBox','RUG_Glyph','CheckBoxGlyph_RUG');
    end;
   Bitmap.Free;
   //
   SkinIni.WriteString('CheckBox_Focus','Element1','E1,FOCUS_RECT');
   SkinIni.WriteString('CheckBox_Focus','E1.Rect','TL,TT,TR,TB');
   //
   SkinIni.WriteString('_CheckBox','NUU_TextLeftOffset','2');
   SkinIni.WriteString('_CheckBox','NUU_TextTopOffset','2');
   SkinIni.WriteString('_CheckBox','NUU_TextRightOffset','2');
   SkinIni.WriteString('_CheckBox','NUU_TextBottomOffset','2');
   SkinIni.WriteString('_CheckBox','NFU_OverStyle','CheckBox_Focus');
   //
   S1:=IniClassProp(TmpIni,'button.checkbox(uncheckeddisabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_CheckBox','RUU_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.checkbox(checkeddisabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_CheckBox','RUC_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.checkbox(mixeddisabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_CheckBox','RUG_FontColor',ConvertColor(S1));
  end;
 if CheckBox7.Checked then
  begin
   SkinIni.WriteString('_CheckBox','HighlightInGlyphStepsNum','8');
   SkinIni.WriteString('_CheckBox','HighlightOutGlyphStepsNum','4');
   SkinIni.WriteString('_CheckBox','CheckGlyphStepsNum','8');
   SkinIni.WriteString('_CheckBox','UncheckGlyphStepsNum','6');
   SkinIni.WriteString('_CheckBox','DownGlyphStepsNum','4');
   SkinIni.WriteString('_CheckBox','UpGlyphStepsNum','2');
   SkinIni.WriteString('_CheckBox','EnableGlyphStepsNum','8');
   SkinIni.WriteString('_CheckBox','DisableGlyphStepsNum','8');
  end;
end;

procedure TForm1.ProcessRadioButton(const MyName:String;SkinIni,TmpIni:TIniFile);
var    FName:String;
      Bitmap:TBitmap;
          B2:TBitmap;
          S1:String;
      Layout:String;
       Count:Integer;
    IsTransp:Boolean;
 TranspColor:TColor;
begin
 FName:=IniClassProp(TmpIni,'button.radiobutton','ImageFile1','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('RADIOBUTTON image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'button.radiobutton','imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'button.radiobutton','imageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'button.radiobutton','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'button.radiobutton','TransparentColor','255 0 255'));
   if Count>=1 then
    begin
     //Normal Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\nuu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_NUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_NUU','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_NUU','E1.Path',SavedShort(_radiobutton+'\nuu.png',MyName));
     SkinIni.WriteString('_RadioButton','NUU_Glyph','RadioButtonGlyph_NUU');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','NUStyle','RadioButtonGlyph_NUU');
    end;
   if Count>=2 then
    begin
     //Highlighted Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\huu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_HUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_HUU','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_HUU','E1.Path',SavedShort(_radiobutton+'\huu.png',MyName));
     SkinIni.WriteString('_RadioButton','HUU_Glyph','RadioButtonGlyph_HUU');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','HUStyle','RadioButtonGlyph_HUU');
    end;
   if Count>=3 then
    begin
     //Down Focused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,2);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\dfu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_DFU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_DFU','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_DFU','E1.Path',SavedShort(_radiobutton+'\dfu.png',MyName));
     SkinIni.WriteString('_RadioButton','DFU_Glyph','RadioButtonGlyph_DFU');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','DUStyle','RadioButtonGlyph_DFU');
    end;
   if Count>=4 then
    begin
     //Read-only Unfocused Unchecked
     B2:=GetSubBitmap(Bitmap,Layout,Count,3);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\ruu.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_RUU','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_RUU','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_RUU','E1.Path',SavedShort(_radiobutton+'\ruu.png',MyName));
     SkinIni.WriteString('_RadioButton','RUU_Glyph','RadioButtonGlyph_RUU');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','RUStyle','RadioButtonGlyph_RUU');
    end;
   //
   if Count>=5 then
    begin
     //Normal Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,4);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\nuc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_NUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_NUC','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_NUC','E1.Path',SavedShort(_radiobutton+'\nuc.png',MyName));
     SkinIni.WriteString('_RadioButton','NUC_Glyph','RadioButtonGlyph_NUC');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','NCStyle','RadioButtonGlyph_NUC');
    end;
   if Count>=6 then
    begin
     //Highlighted Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,5);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\huc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_HUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_HUC','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_HUC','E1.Path',SavedShort(_radiobutton+'\huc.png',MyName));
     SkinIni.WriteString('_RadioButton','HUC_Glyph','RadioButtonGlyph_HUC');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','HCStyle','RadioButtonGlyph_HUC');
    end;
   if Count>=7 then
    begin
     //Down Focused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,6);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\dfc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_DFC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_DFC','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_DFC','E1.Path',SavedShort(_radiobutton+'\dfc.png',MyName));
     SkinIni.WriteString('_RadioButton','DFC_Glyph','RadioButtonGlyph_DFC');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','DCStyle','RadioButtonGlyph_DFC');
    end;
   if Count>=8 then
    begin
     //Read-only Unfocused Checked
     B2:=GetSubBitmap(Bitmap,Layout,Count,7);
     SaveBitmap(B2,DirPath+MyName+'\'+_radiobutton+'\ruc.png',IsTransp,TranspColor);
     if BitmapIsOpaque(B2) then
      SkinIni.WriteString('RadioButtonGlyph_RUC','Transparent','0');
     B2.Free;
     SkinIni.WriteString('RadioButtonGlyph_RUC','Element1','E1,IMAGE');
     SkinIni.WriteString('RadioButtonGlyph_RUC','E1.Path',SavedShort(_radiobutton+'\ruc.png',MyName));
     SkinIni.WriteString('_RadioButton','RUC_Glyph','RadioButtonGlyph_RUC');
     SkinIni.WriteString('_MultiStateCheck_RadioButton','RCStyle','RadioButtonGlyph_RUC');
    end;
   Bitmap.Free;
   //
   SkinIni.WriteString('RadioButton_Focus','Element1','E1,FOCUS_RECT');
   SkinIni.WriteString('RadioButton_Focus','E1.Rect','TL,TT,TR,TB');
   //
   SkinIni.WriteString('_RadioButton','NUU_TextLeftOffset','2');
   SkinIni.WriteString('_RadioButton','NUU_TextTopOffset','2');
   SkinIni.WriteString('_RadioButton','NUU_TextRightOffset','2');
   SkinIni.WriteString('_RadioButton','NUU_TextBottomOffset','2');
   SkinIni.WriteString('_RadioButton','NFU_OverStyle','RadioButton_Focus');
   //
   S1:=IniClassProp(TmpIni,'button.radiobutton(uncheckeddisabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_RadioButton','RUU_FontColor',ConvertColor(S1));
   //
   S1:=IniClassProp(TmpIni,'button.radiobutton(checkeddisabled)','TextColor','');
   if S1<>'' then
    SkinIni.WriteString('_RadioButton','RUC_FontColor',ConvertColor(S1));
  end;
 if CheckBox7.Checked then
  begin
   SkinIni.WriteString('_RadioButton','HighlightInGlyphStepsNum','8');
   SkinIni.WriteString('_RadioButton','HighlightOutGlyphStepsNum','4');
   SkinIni.WriteString('_RadioButton','CheckGlyphStepsNum','8');
   SkinIni.WriteString('_RadioButton','UncheckGlyphStepsNum','6');
   SkinIni.WriteString('_RadioButton','DownGlyphStepsNum','4');
   SkinIni.WriteString('_RadioButton','UpGlyphStepsNum','2');
   SkinIni.WriteString('_RadioButton','EnableGlyphStepsNum','8');
   SkinIni.WriteString('_RadioButton','DisableGlyphStepsNum','8');
  end;
end;

procedure TForm1.ProcessGroupBox(const MyName:String;SkinIni,TmpIni:TIniFile);
var     FName:String;
       Bitmap:TBitmap;
    S1,Sizing:String;
           R1:TRect;
     IsTransp:Boolean;
  TranspColor:TColor;
begin
 FName:=IniClassProp(TmpIni,'button.groupbox','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('GROUPBOX image not found!');
     exit;
    end;
   R1:=ConvertRect(IniClassProp(TmpIni,'button.groupbox','SizingMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'button.groupbox','SizingType',''));
   IsTransp:=SameText(IniClassProp(TmpIni,'button.groupbox','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'button.groupbox','TransparentColor','255 0 255'));
   SaveBoxTileImages(Bitmap,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_groupbox+'\*.png',IsTransp,TranspColor);
   Bitmap.Free;
   SkinIni.WriteString('GroupBox_NU','UseBuffering','1');
   SkinIni.WriteString('GroupBox_NU','Element1','E1,BOX_TILE');
   SaveRedirectedBoxTile('GroupBox_NU','1',_groupbox+'\*.png',MyName,SkinIni);
   if Sizing='stretch' then
    SkinIni.WriteString('GroupBox_NU','E1.ResizeMode','Stretch');
   SkinIni.WriteString('GroupBox_NU','E1.Rect','0,(CT+CB)|2,W,H');
   SkinIni.WriteString('GroupBox_NU','Element2','E2,ERASE_RECT');
   SkinIni.WriteString('GroupBox_NU','E2.Rect','CL-1,CT,CR+1,CB');
   //
   SkinIni.WriteString('GroupBox_Focus','Element1','E1,FOCUS_RECT');
   SkinIni.WriteString('GroupBox_Focus','E1.Rect','TL,TT,TR,TB');
   //
   S1:=IniClassProp(TmpIni,'button.groupbox','TextColor','');
   SkinIni.WriteString('_Label_GroupBox','N_FontColor',ConvertColor(S1));
   //
   SkinIni.WriteString('_GroupBox','LabelStyle','_Label_GroupBox');
   SkinIni.WriteString('_GroupBox','NU_Style','GroupBox_NU');
   SkinIni.WriteString('_GroupBox','NU_CaptionPosition','8');
   SkinIni.WriteString('_GroupBox','NU_CaptionLeftOffset','2');
   SkinIni.WriteString('_GroupBox','NU_CaptionRightOffset','2');
   SkinIni.WriteString('_GroupBox','NU_TextLeftOffset','2');
   SkinIni.WriteString('_GroupBox','NU_TextTopOffset','2');
   SkinIni.WriteString('_GroupBox','NU_TextRightOffset','2');
   SkinIni.WriteString('_GroupBox','NU_TextBottomOffset','2');
   SkinIni.WriteString('_GroupBox','NF_OverStyle','GroupBox_Focus');
   SkinIni.WriteString('_GroupBox','ClientRect','3,CB+2,W-3,H-3');
   //
   S1:=IniClassProp(TmpIni,'button.groupbox','borderOnly','');
   if SameText(S1,'true') then
    begin
     DeleteFile(DirPath+MyName+'\'+_groupbox+'\center.png');
     SkinIni.WriteString('_GroupBox','NU_TransparentRect',inttostr(R1.Left)+
          ',CB+1,W-'+inttostr(R1.Right)+',H-'+inttostr(R1.Bottom));
    end;
  end;
end;

procedure TForm1.ProcessEdit(const MyName:String;SkinIni,TmpIni:TIniFile);
var   BgType:String;
    FName,S1:String;
      Bitmap:TBitmap;
          R1:TRect;
       BSize:Integer;
       C1,C2:String;
       C3,C4:String;
      Sizing:String;
    IsTransp:Boolean;
 TranspColor:TColor;
begin
 BgType:=IniClassProp(TmpIni,'edit','BgType','');
 if SameText(BgType,'ImageFile') then
  begin
   FName:=IniClassProp(TmpIni,'edit','Imagefile','');
   if FName<>'' then
    begin
     Bitmap:=LoadBitmap(FName);
     if Bitmap=nil then
      begin
       Memo1.Lines.Add('EDIT image not found!');
       exit;
      end;
     R1:=ConvertRect(IniClassProp(TmpIni,'edit','SizingMargins',''));
     Sizing:=LowerCase(IniClassProp(TmpIni,'edit','SizingType',''));
     IsTransp:=SameText(IniClassProp(TmpIni,'edit','Transparent',''),'true');
     TranspColor:=GetTColor(IniClassProp(TmpIni,'edit','TransparentColor','255 0 255'));
     SaveBoxTileImages(Bitmap,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_edit+'\*.png',IsTransp,TranspColor);
     if BitmapIsOpaque(Bitmap) then
      SkinIni.WriteString('Edit_NU','Transparent','0');
     Bitmap.Free;
     SkinIni.WriteString('Edit_NU','Element1','E1,BOX_TILE');
     SaveRedirectedBoxTile('Edit_NU','1',_edit+'\*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Edit_NU','E1.ResizeMode','Stretch');
    end;
  end else
 if SameText(BgType,'BorderFill') then
  begin
   BSize:=StrToIntDef(IniClassProp(TmpIni,'edit','BorderSize',''),1);
   C1:=TmpIni.ReadString('edit','FillColor','');
   C2:=TmpIni.ReadString('edit','BorderColor','');
   SkinIni.WriteString('Edit_NU','Transparent','0');
   SkinIni.WriteString('Edit_NU','Element1','E1,RECT');
   SkinIni.WriteString('Edit_NU','E1.Rect','0,0,W-1,H-1');
   if C1<>'' then
    begin
     SkinIni.WriteString('Edit_NU','E1.HasFill','1');
     SkinIni.WriteString('Edit_NU','E1.FillColor',ConvertColor(C1));
    end;
   if C2<>'' then
    begin
     SkinIni.WriteString('Edit_NU','E1.HasBorder','1');
     SkinIni.WriteString('Edit_NU','E1.BorderColor',ConvertColor(C2));
    end;
   if BSize<>1 then
    SkinIni.WriteString('Edit_NU','E1.BorderThickness',inttostr(BSize));
   //
   C3:=IniClassProp(TmpIni,'edit.edittext(Disabled)','FillColor','');
   C4:=IniClassProp(TmpIni,'edit.edittext(Disabled)','BorderColor','');
   if (C3<>'') or (C4<>'') then
    begin
     if C3<>'' then C1:=C3;
     if C4<>'' then C2:=C4;
     SkinIni.WriteString('Edit_RU','Transparent','0');
     SkinIni.WriteString('Edit_RU','Element1','E1,RECT');
     SkinIni.WriteString('Edit_RU','E1.Rect','0,0,W-1,H-1');
     if C1<>'' then
      begin
       SkinIni.WriteString('Edit_RU','E1.HasFill','1');
       SkinIni.WriteString('Edit_RU','E1.FillColor',ConvertColor(C1));
      end;
     if C2<>'' then
      begin
       SkinIni.WriteString('Edit_RU','E1.HasBorder','1');
       SkinIni.WriteString('Edit_RU','E1.BorderColor',ConvertColor(C2));
      end;
     if BSize<>1 then
      SkinIni.WriteString('Edit_RU','E1.BorderThickness',inttostr(BSize));
     //
     SkinIni.WriteString('_Edit','RU_Style','Edit_RU');
    end;
  end;
 //
 SkinIni.WriteString('_Edit','NU_Style','Edit_NU');
 SkinIni.WriteString('_Edit','TextLeftOffset','3');
 SkinIni.WriteString('_Edit','TextTopOffset','5');
 SkinIni.WriteString('_Edit','TextRightOffset','3');
 SkinIni.WriteString('_Edit','TextBottomOffset','5');
 //
 S1:=TmpIni.ReadString('edit','TextColor','');
 if S1<>'' then
  SkinIni.WriteString('_Edit','NU_FontColor',ConvertColor(S1));
 //
 S1:=IniClassProp(TmpIni,'edit.edittext(Disabled)','TextColor','');
 if S1<>'' then
  SkinIni.WriteString('_Edit','RU_FontColor',ConvertColor(S1));
end;

procedure TForm1.ProcessForm(const MyName:String;SkinIni,TmpIni:TIniFile);
var    FName:String;
   Bitmap,B2:TBitmap;
 Layout,S,S2:String;
       Count:Integer;
       R1,R2:TRect;
      Sizing:String;
      LeftRS:TRect;
     RightRS:TRect;
    BottomRS:TRect;
    LFSizing:String;
    RFSizing:String;
    BFSizing:String;
     COpaque:Boolean;
     LOpaque:Boolean;
     ROpaque:Boolean;
     BOpaque:Boolean;
    MinCount:Integer;
  OffsetType:String;
      Offset:String;
    OffsetPT:TPoint;
    FontName:String;
    FontSize:Integer;
   FontStyle:TFontStyles;
       Align:String;
    IsTransp:Boolean;
 TranspColor:TColor;
          CH:String;
begin
 CH:=inttostr(StrToIntDef(IniClassProp(TmpIni,'SysMetrics','CaptionBarHeight',''),25)+5);
 MinCount:=100;
 LeftRS:=Rect(0,0,0,0);
 RightRS:=Rect(0,0,0,0);
 BottomRS:=Rect(0,0,0,0);
 COpaque:=False; LOpaque:=False; ROpaque:=False; BOpaque:=False;
 //Normal Caption
 FName:=IniClassProp(TmpIni,'Window.Caption','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.CAPTION image not found!');
     exit;
    end;
   COpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.Caption','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.Caption','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.Caption','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.Caption','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.Caption','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.Caption','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.Caption','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_NF','1',_form+'\'+_caption+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NF_Caption','Form_Caption_NF');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','NF_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_nf_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_NU','1',_form+'\'+_caption+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NU_Caption','Form_Caption_NU');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','NU_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_nu_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   Align:=CorrectAlignment(IniClassProp(TmpIni,'Window.Caption','ContentAlignment',''));
   SkinIni.WriteString('_Form','NU_TextAlignment',Align);
  end;
 //Minimized Caption
 FName:=IniClassProp(TmpIni,'Window.MinCaption','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.MINCAPTION image not found!');
     exit;
    end;
   COpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.MinCaption','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.MinCaption','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.MinCaption','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.MinCaption','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.MinCaption','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.MinCaption','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.MinCaption','SizingType',''));
   if Count>1 then
    begin
     //Minimized Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_mf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_MF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_MF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_MF','1',_form+'\'+_caption+'_mf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_MF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','MF_Caption','Form_Caption_MF');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','MF_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_mf_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   if Count>=1 then
    begin
     //Minimized Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_mu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_MU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_MU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_MU','1',_form+'\'+_caption+'_mu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_MU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','MU_Caption','Form_Caption_MU');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','MU_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_mu_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   Align:=CorrectAlignment(IniClassProp(TmpIni,'Window.MinCaption','ContentAlignment',''));
   SkinIni.WriteString('_Form','MU_TextAlignment',Align);
  end;
 //Maximized Caption
 FName:=IniClassProp(TmpIni,'Window.MaxCaption','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.MAXCAPTION image not found!');
     exit;
    end;
   COpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.MaxCaption','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.MaxCaption','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.MaxCaption','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.MaxCaption','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.MaxCaption','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.MaxCaption','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.MaxCaption','SizingType',''));
   if Count>1 then
    begin
     //Maximized Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_xf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_XF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_XF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_XF','1',_form+'\'+_caption+'_xf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_XF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','XF_Caption','Form_Caption_XF');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','XF_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_xf_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   if Count>=1 then
    begin
     //Maximized Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_caption+'_xu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_Caption_XU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_Caption_XU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_Caption_XU','1',_form+'\'+_caption+'_xu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_Caption_XU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','XU_Caption','Form_Caption_XU');
     //
     if not COpaque then
      SkinIni.WriteString('_Form','XU_FullMask','Full,-BoxTile,'+_form+'\'+_caption+'_xu_*.png,'+
          Sizing+',W,CH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   Align:=CorrectAlignment(IniClassProp(TmpIni,'Window.MaxCaption','ContentAlignment',''));
   SkinIni.WriteString('_Form','XU_TextAlignment',Align);
  end;
 //LeftFrame
 FName:=IniClassProp(TmpIni,'Window.FrameLeft','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.FRAMELEFT image not found!');
     exit;
    end;
   LOpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.FrameLeft','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.FrameLeft','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.FrameLeft','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.FrameLeft','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.FrameLeft','SizingMargins',''));
   LeftRS:=R1;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.FrameLeft','SizingType',''));
   LFSizing:=Sizing;
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_leftframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_LeftFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_LeftFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_LeftFrame_NF','1',_form+'\'+_leftframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_LeftFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NF_LeftFrame','Form_LeftFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_Form','LeftFrameWidth',inttostr(B2.Width));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_leftframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_LeftFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_LeftFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_LeftFrame_NU','1',_form+'\'+_leftframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_LeftFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NU_LeftFrame','Form_LeftFrame_NU');
    end;
  end;
 //RightFrame
 FName:=IniClassProp(TmpIni,'Window.FrameRight','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.FRAMERIGHT image not found!');
     exit;
    end;
   ROpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.FrameRight','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.FrameRight','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.FrameRight','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.FrameRight','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.FrameRight','SizingMargins',''));
   RightRS:=R1;
   RFSizing:=Sizing;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.FrameRight','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_rightframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_RightFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_RightFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_RightFrame_NF','1',_form+'\'+_rightframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_RightFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NF_RightFrame','Form_RightFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_Form','RightFrameWidth',inttostr(B2.Width));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_rightframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_RightFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_RightFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_RightFrame_NU','1',_form+'\'+_rightframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_RightFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NU_RightFrame','Form_RightFrame_NU');
    end;
  end;
 Count:=0; 
 //BottomFrame
 FName:=IniClassProp(TmpIni,'Window.FrameBottom','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   BOpaque:=BitmapIsOpaque(Bitmap);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.FRAMEBOTTOM image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'Window.FrameBottom','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.FrameBottom','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.FrameBottom','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.FrameBottom','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.FrameBottom','SizingMargins',''));
   BottomRS:=R1;
   BFSizing:=Sizing;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.FrameBottom','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_bottomframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_BottomFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_BottomFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_BottomFrame_NF','1',_form+'\'+_bottomframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_BottomFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NF_BottomFrame','Form_BottomFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_Form','BottomFrameHeight',inttostr(B2.Height));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_bottomframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_BottomFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_BottomFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_BottomFrame_NU','1',_form+'\'+_bottomframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_BottomFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_Form','NU_BottomFrame','Form_BottomFrame_NU');
    end;
  end;
 //
 if MinCount>=2 then
  begin
   S:=SkinIni.ReadString('_Form','NF_FullMask','Full');
   if not LOpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_leftframe+'_nf_*.png,'+LFSizing+',0,CH,'+
        SkinIni.ReadString('_Form','LeftFrameWidth','')+','+
        'H-CH-'+SkinIni.ReadString('_Form','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(LeftRS.Left)+','+inttostr(LeftRS.Right)+','+
        inttostr(LeftRS.Top)+','+inttostr(LeftRS.Bottom);
    end;
   if not ROpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_rightframe+'_nf_*.png,'+RFSizing+',W-'+
        SkinIni.ReadString('_Form','RightFrameWidth','')+',CH,'+
        SkinIni.ReadString('_Form','RightFrameWidth','')+','+
        'H-CH-'+SkinIni.ReadString('_Form','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(RightRS.Left)+','+inttostr(RightRS.Right)+','+
        inttostr(RightRS.Top)+','+inttostr(RightRS.Bottom);
    end;
   if not BOpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_bottomframe+'_nf_*.png,'+BFSizing+',0,H-'+
        SkinIni.ReadString('_Form','BottomFrameHeight','')+',W,'+
        SkinIni.ReadString('_Form','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(BottomRS.Left)+','+inttostr(BottomRS.Right)+','+
        inttostr(BottomRS.Top)+','+inttostr(BottomRS.Bottom);
    end;
   if S<>'Full' then
    SkinIni.WriteString('_Form','NF_FullMask',S);
  end;
 //
 S:=SkinIni.ReadString('_Form','NU_FullMask','Full');
 if not LOpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_leftframe+'_nu_*.png,'+LFSizing+',0,CH,'+
      SkinIni.ReadString('_Form','LeftFrameWidth','')+','+
      'H-CH-'+SkinIni.ReadString('_Form','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(LeftRS.Left)+','+inttostr(LeftRS.Right)+','+
      inttostr(LeftRS.Top)+','+inttostr(LeftRS.Bottom);
  end;
 if not ROpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_rightframe+'_nu_*.png,'+RFSizing+',W-'+
      SkinIni.ReadString('_Form','RightFrameWidth','')+',CH,'+
      SkinIni.ReadString('_Form','RightFrameWidth','')+','+
      'H-CH-'+SkinIni.ReadString('_Form','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(RightRS.Left)+','+inttostr(RightRS.Right)+','+
      inttostr(RightRS.Top)+','+inttostr(RightRS.Bottom);
  end;
 if not BOpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_bottomframe+'_nu_*.png,'+BFSizing+',0,H-'+
      SkinIni.ReadString('_Form','BottomFrameHeight','')+',W,'+
      SkinIni.ReadString('_Form','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(BottomRS.Left)+','+inttostr(BottomRS.Right)+','+
      inttostr(BottomRS.Top)+','+inttostr(BottomRS.Bottom);
  end;
 if S<>'Full' then
  SkinIni.WriteString('_Form','NU_FullMask',S);
 //
 if (SkinIni.ReadString('_Form','NF_FullMask','Full')<>'Full') and (Count=1) then
  SkinIni.WriteString('_Form','NF_FullMask',S);
 //
 FName:=IniClassProp(TmpIni,'Window.CaptionSizingTemplate','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.CAPTIONSIZINGTEMPLATE image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'Window.CaptionSizingTemplate','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.CaptionSizingTemplate','ImageCount',''),0);
   if Count=0 then Count:=1;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.CaptionSizingTemplate','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.CaptionSizingTemplate','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.CaptionSizingTemplate','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R2.Left,R2.Right,R2.Top,R2.Bottom,DirPath+MyName+'\'+_form+'\'+_resize+'_nf_*.png',False,0,False);
     B2.Free;
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R2.Left,R2.Right,R2.Top,R2.Bottom,DirPath+MyName+'\'+_form+'\'+_resize+'_nu_*.png',False,0,False);
     B2.Free;
    end;
   //
   SkinIni.WriteString('_Form','NU_CaptionRegion','Rect,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Right)+',CH');
   SkinIni.WriteString('_Form','NU_ResizeTopLeft','Image,'+_form+'\'+_resize+'_nu_topleft.png,0,0,#000000,#FFFFFF');
   SkinIni.WriteString('_Form','NU_ResizeTopRight','ImageO,'+_form+'\'+_resize+'_nu_topright.png,W-'+inttostr(R2.Right)+',0,0,0,#000000,#FFFFFF');
   SkinIni.WriteString('_Form','NU_ResizeBottomLeft','Rect,0,H-3,18,H');
   SkinIni.WriteString('_Form','NU_ResizeBottomRight','Rect,W-18,H-3,W,H');
{$IFDEF FIXMSSTYLESBUGS}
   if (Copy(MyName,1,5)='Wisp_') or (Copy(MyName,1,7)='Bounce_') or
      (Copy(MyName,1,7)='Fiesta_') or (Copy(MyName,1,9)='LoneStar_') or
      (Copy(MyName,1,2)='Q_') or (Copy(MyName,1,6)='Rogue_') or
      (Copy(MyName,1,5)='Trek_') or (Copy(MyName,1,9)='Trekgrey_') then
    SkinIni.WriteString('_Form','NU_ResizeTop','Rect,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Left+R2.Right)+',4') else
{$ENDIF}
   SkinIni.WriteString('_Form','NU_ResizeTop','ImageO,'+_form+'\'+_resize+'_nu_top.png,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Left+R2.Right)+',0,#000000,#FFFFFF');
   SkinIni.WriteString('_Form','NU_ResizeLeft','Rect,0,CH,3,H-3');
   SkinIni.WriteString('_Form','NU_ResizeRight','Rect,W-3,CH,W,H-3');
   SkinIni.WriteString('_Form','NU_ResizeBottom','Rect,18,H-3,W-18,H');
   //
   if Count>1 then
    begin
     SkinIni.WriteString('_Form','NF_ResizeTopLeft','Image,'+_form+'\'+_resize+'_nf_topleft.png,0,0,#000000,#FFFFFF');
     SkinIni.WriteString('_Form','NF_ResizeTopRight','ImageO,'+_form+'\'+_resize+'_nf_topright.png,W-'+inttostr(R2.Right)+',0,0,0,#000000,#FFFFFF');
{$IFDEF FIXMSSTYLESBUGS}
   if (Copy(MyName,1,5)='Wisp_') or (Copy(MyName,1,7)='Bounce_') or
      (Copy(MyName,1,7)='Fiesta_') or (Copy(MyName,1,9)='LoneStar_') or
      (Copy(MyName,1,2)='Q_') or (Copy(MyName,1,6)='Rogue_') or
      (Copy(MyName,1,5)='Trek_') or (Copy(MyName,1,9)='Trekgrey_') then
     else
{$ENDIF}
     SkinIni.WriteString('_Form','NF_ResizeTop','ImageO,'+_form+'\'+_resize+'_nf_top.png,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Left+R2.Right)+',0,#000000,#FFFFFF');
    end;
  end;
 //
 SkinIni.WriteString('_Form','CaptionHeight',CH);
 //
 Layout:=IniClassProp(TmpIni,'Window.Caption','ContentMargins','');
 R2:=ConvertRect(Layout);
 SkinIni.WriteString('_Form','NU_IconRect','6,CH/5+2,CH*3/5+4,CH*4/5');
 if Layout='' then
  SkinIni.WriteString('_Form','NU_TextRect','CH+8,2,W,CH') else
   begin
    if R2.Left<10 then R2.Left:=10;
    SkinIni.WriteString('_Form','NU_TextRect',inttostr(R2.Left)+',2,W-'+inttostr(R2.Right)+',CH');
   end;
 //TextShadow
 S:=IniClassProp(TmpIni,'Window.Caption(Active)','TextShadowColor','');
 if S<>'' then
  begin
   S2:=IniClassProp(TmpIni,'Window.Caption(Active)','TextShadowType','');
   if SameText(S2,'Single') then
    begin
     SkinIni.WriteString('_Form','NF_ShadowColor',ConvertColor(S));
     SkinIni.WriteString('_Form','NF_HasTextShadow','1');
    end;
  end;
 S:=IniClassProp(TmpIni,'Window.Caption(Inctive)','TextShadowColor','');
 if S<>'' then
  begin
   S2:=IniClassProp(TmpIni,'Window.Caption(Inctive)','TextShadowType','');
   if SameText(S2,'Single') then
    begin
     SkinIni.WriteString('_Form','NU_ShadowColor',ConvertColor(S));
     SkinIni.WriteString('_Form','NU_HasTextShadow','1');
    end;
  end;
 //CaptionText
 S:=IniClassProp(TmpIni,'SysMetrics','CaptionText','255 255 255');
 SkinIni.WriteString('_Form','NF_FontColor',ConvertColor(S));
 //InactiveCaptionText
 S:=IniClassProp(TmpIni,'SysMetrics','InactiveCaptionText','216 228 248');
 SkinIni.WriteString('_Form','NU_FontColor',ConvertColor(S));
 //CaptionFont
 S:=IniClassProp(TmpIni,'SysMetrics','CaptionFont','Trebuchet MS, 10, bold');
 ConvertFont(S,FontName,FontSize,FontStyle);
 SkinIni.WriteString('_Form','NU_FontName',CorrectFont(FontName));
 SkinIni.WriteString('_Form','NU_FontSize',inttostr(FontSize));
 SkinIni.WriteString('_Form','NU_FontStyle',ConvertFontStyle(FontStyle));
end;

procedure TForm1.ProcessFormSmallCaption(const MyName:String;SkinIni,TmpIni:TIniFile);
var    FName:String;
   Bitmap,B2:TBitmap;
    Layout,S:String;
       Count:Integer;
       R1,R2:TRect;
   Sizing,S2:String;
      LeftRS:TRect;
     RightRS:TRect;
    BottomRS:TRect;
    LFSizing:String;
    RFSizing:String;
    BFSizing:String;
     COpaque:Boolean;
     LOpaque:Boolean;
     ROpaque:Boolean;
     BOpaque:Boolean;
    MinCount:Integer;
   SCH,Align:String;
    FontName:String;
    FontSize:Integer;
   FontStyle:TFontStyles;
    IsTransp:Boolean;
 TranspColor:TColor;
begin
 SCH:=inttostr(StrToIntDef(IniClassProp(TmpIni,'SysMetrics','SMCaptionBarHeight',''),17)+5);
 MinCount:=100;
 LeftRS:=Rect(0,0,0,0);
 RightRS:=Rect(0,0,0,0);
 BottomRS:=Rect(0,0,0,0);
 COpaque:=False; LOpaque:=False; ROpaque:=False; BOpaque:=False;
 //
 FName:=IniClassProp(TmpIni,'Window.SmallCaption','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.SMALLCAPTION image not found!');
     exit;
    end;
   COpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.SmallCaption','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.SmallCaption','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.SmallCaption','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.SmallCaption','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.SmallCaption','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.SmallCaption','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.SmallCaption','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallcaption+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallCaption_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallCaption_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallCaption_NF','1',_form+'\'+_smallcaption+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallCaption_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NF_Caption','Form_SmallCaption_NF');
     //
     if not COpaque then
      SkinIni.WriteString('_FormSmallCaption','NF_FullMask','Full,-BoxTile,'+_form+'\'+_smallcaption+'_nf_*.png,'+
          Sizing+',W,SCH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallcaption+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallCaption_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallCaption_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallCaption_NU','1',_form+'\'+_smallcaption+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallCaption_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NU_Caption','Form_SmallCaption_NU');
     //
     if not COpaque then
      SkinIni.WriteString('_FormSmallCaption','NU_FullMask','Full,-BoxTile,'+_form+'\'+_smallcaption+'_nu_*.png,'+
          Sizing+',W,SCH,#FF000000,#FF000000,'+inttostr(R1.Left)+','+inttostr(R1.Right)+','+
          inttostr(R1.Top)+','+inttostr(R1.Bottom));
    end;
   Align:=CorrectAlignment(IniClassProp(TmpIni,'Window.SmallCaption','ContentAlignment',''));
   SkinIni.WriteString('_FormSmallCaption','NU_TextAlignment',Align);
  end;
 //LeftFrame
 FName:=IniClassProp(TmpIni,'Window.SmallFrameLeft','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.SMALLFRAMELEFT image not found!');
     exit;
    end;
   LOpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.SmallFrameLeft','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.SmallFrameLeft','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.SmallFrameLeft','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.SmallFrameLeft','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.SmallFrameLeft','SizingMargins',''));
   LeftRS:=R1;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.SmallFrameLeft','SizingType',''));
   LFSizing:=Sizing;
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallleftframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallLeftFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallLeftFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallLeftFrame_NF','1',_form+'\'+_smallleftframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallLeftFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NF_LeftFrame','Form_SmallLeftFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_FormSmallCaption','LeftFrameWidth',inttostr(B2.Width));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallleftframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallLeftFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallLeftFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallLeftFrame_NU','1',_form+'\'+_smallleftframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallLeftFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NU_LeftFrame','Form_SmallLeftFrame_NU');
    end;
  end;
 //RightFrame
 FName:=IniClassProp(TmpIni,'Window.SmallFrameRight','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.SMALLFRAMERIGHT image not found!');
     exit;
    end;
   ROpaque:=BitmapIsOpaque(Bitmap);
   Layout:=IniClassProp(TmpIni,'Window.SmallFrameRight','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.SmallFrameRight','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.SmallFrameRight','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.SmallFrameRight','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.SmallFrameRight','SizingMargins',''));
   RightRS:=R1;
   RFSizing:=Sizing;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.SmallFrameRight','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallrightframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallRightFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallRightFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallRightFrame_NF','1',_form+'\'+_smallrightframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_RightFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NF_RightFrame','Form_SmallRightFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_FormSmallCaption','RightFrameWidth',inttostr(B2.Width));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallrightframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallRightFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallRightFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallRightFrame_NU','1',_form+'\'+_smallrightframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallRightFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NU_RightFrame','Form_SmallRightFrame_NU');
    end;
  end;
 Count:=0;
 //BottomFrame
 FName:=IniClassProp(TmpIni,'Window.SmallFrameBottom','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   BOpaque:=BitmapIsOpaque(Bitmap);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.SMALLFRAMEBOTTOM image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'Window.SmallFrameBottom','ImageLayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.SmallFrameBottom','ImageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,'Window.SmallFrameBottom','Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,'Window.SmallFrameBottom','TransparentColor','255 0 255'));
   if Count=0 then Count:=1;
   if Count<MinCount then MinCount:=Count;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.SmallFrameBottom','SizingMargins',''));
   BottomRS:=R1;
   BFSizing:=Sizing;
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.SmallFrameBottom','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallbottomframe+'_nf_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallBottomFrame_NF','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallBottomFrame_NF','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallBottomFrame_NF','1',_form+'\'+_smallbottomframe+'_nf_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallBottomFrame_NF','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NF_BottomFrame','Form_SmallBottomFrame_NF');
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SkinIni.WriteString('_FormSmallCaption','BottomFrameHeight',inttostr(B2.Height));
     SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_form+'\'+_smallbottomframe+'_nu_*.png',IsTransp,TranspColor,False);
     B2.Free;
     SkinIni.WriteString('Form_SmallBottomFrame_NU','Element1','E1,BOX_TILE');
     SkinIni.WriteString('Form_SmallBottomFrame_NU','E1.Transparent','0');
     SaveRedirectedBoxTile('Form_SmallBottomFrame_NU','1',_form+'\'+_smallbottomframe+'_nu_*.png',MyName,SkinIni);
     if Sizing='stretch' then
      SkinIni.WriteString('Form_SmallBottomFrame_NU','E1.ResizeMode','Stretch');
     SkinIni.WriteString('_FormSmallCaption','NU_BottomFrame','Form_SmallBottomFrame_NU');
    end;
  end;
 //
 if MinCount>=2 then
  begin
   S:=SkinIni.ReadString('_FormSmallCaption','NF_FullMask','Full');
   if not LOpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_smallleftframe+'_nf_*.png,'+LFSizing+',0,SCH,'+
        SkinIni.ReadString('_FormSmallCaption','LeftFrameWidth','')+','+
        'H-SCH-'+SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(LeftRS.Left)+','+inttostr(LeftRS.Right)+','+
        inttostr(LeftRS.Top)+','+inttostr(LeftRS.Bottom);
    end;
   if not ROpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_smallrightframe+'_nf_*.png,'+RFSizing+',W-'+
        SkinIni.ReadString('_FormSmallCaption','RightFrameWidth','')+',SCH,'+
        SkinIni.ReadString('_FormSmallCaption','RightFrameWidth','')+','+
        'H-SCH-'+SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(RightRS.Left)+','+inttostr(RightRS.Right)+','+
        inttostr(RightRS.Top)+','+inttostr(RightRS.Bottom);
    end;
   if not BOpaque then
    begin
     S:=S+',-BoxTileO,'+_form+'\'+_smallbottomframe+'_nf_*.png,'+BFSizing+',0,H-'+
        SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+',W,'+
        SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
        ',#FF000000,#FF000000,'+inttostr(BottomRS.Left)+','+inttostr(BottomRS.Right)+','+
        inttostr(BottomRS.Top)+','+inttostr(BottomRS.Bottom);
    end;
   if S<>'Full' then
    SkinIni.WriteString('_FormSmallCaption','NF_FullMask',S);
  end;
 //
 S:=SkinIni.ReadString('_FormSmallCaption','NU_FullMask','Full');
 if not LOpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_smallleftframe+'_nu_*.png,'+LFSizing+',0,SCH,'+
      SkinIni.ReadString('_FormSmallCaption','LeftFrameWidth','')+','+
      'H-SCH-'+SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(LeftRS.Left)+','+inttostr(LeftRS.Right)+','+
      inttostr(LeftRS.Top)+','+inttostr(LeftRS.Bottom);
  end;
 if not ROpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_smallrightframe+'_nu_*.png,'+RFSizing+',W-'+
      SkinIni.ReadString('_FormSmallCaption','RightFrameWidth','')+',SCH,'+
      SkinIni.ReadString('_FormSmallCaption','RightFrameWidth','')+','+
      'H-SCH-'+SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(RightRS.Left)+','+inttostr(RightRS.Right)+','+
      inttostr(RightRS.Top)+','+inttostr(RightRS.Bottom);
  end;
 if not BOpaque then
  begin
   S:=S+',-BoxTileO,'+_form+'\'+_smallbottomframe+'_nu_*.png,'+BFSizing+',0,H-'+
      SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+',W,'+
      SkinIni.ReadString('_FormSmallCaption','BottomFrameHeight','')+
      ',#FF000000,#FF000000,'+inttostr(BottomRS.Left)+','+inttostr(BottomRS.Right)+','+
      inttostr(BottomRS.Top)+','+inttostr(BottomRS.Bottom);
  end;
 if S<>'Full' then
  SkinIni.WriteString('_FormSmallCaption','NU_FullMask',S);
 //
 if (SkinIni.ReadString('_FormSmallCaption','NF_FullMask','Full')<>'Full') and (Count=1) then
  SkinIni.WriteString('_FormSmallCaption','NF_FullMask',S);
 //
 FName:=IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','ImageFile','');
 if FName<>'' then
  begin
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.SMALLCAPTIONSIZINGTEMPLATE image not found!');
     exit;
    end;
   Layout:=IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','Imagelayout','');
   Count:=StrToIntDef(IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','ImageCount',''),0);
   if Count=0 then Count:=1;
   R1:=ConvertRect(IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','ContentMargins',''));
   Sizing:=LowerCase(IniClassProp(TmpIni,'Window.SmallCaptionSizingTemplate','SizingType',''));
   if Count>1 then
    begin
     //Normal Focused
     B2:=GetSubBitmap(Bitmap,Layout,Count,0);
     SaveBoxTileImages(B2,R2.Left,R2.Right,R2.Top,R2.Bottom,DirPath+MyName+'\'+_form+'\'+_smallresize+'_nf_*.png',False,0,False);
     B2.Free;
    end;
   if Count>=1 then
    begin
     //Normal Unfocused
     if Count=1 then
      B2:=GetSubBitmap(Bitmap,Layout,Count,0) else
       B2:=GetSubBitmap(Bitmap,Layout,Count,1);
     SaveBoxTileImages(B2,R2.Left,R2.Right,R2.Top,R2.Bottom,DirPath+MyName+'\'+_form+'\'+_smallresize+'_nu_*.png',False,0,False);
     B2.Free;
    end;
   //
   SkinIni.WriteString('_FormSmallCaption','NU_CaptionRegion','Rect,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Right)+',CH');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeTopLeft','Image,'+_form+'\'+_smallresize+'_nu_topleft.png,0,0,#000000,#FFFFFF');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeTopRight','ImageO,'+_form+'\'+_smallresize+'_nu_topright.png,W-'+inttostr(R2.Right)+',0,0,0,#000000,#FFFFFF');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeBottomLeft','Rect,0,H-3,18,H');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeBottomRight','Rect,W-18,H-3,W,H');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeTop','ImageO,'+_form+'\'+_smallresize+'_nu_top.png,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Left+R2.Right)+',0,#000000,#FFFFFF');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeLeft','Rect,0,SCH,3,H-3');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeRight','Rect,W-3,SCH,W,H-3');
   SkinIni.WriteString('_FormSmallCaption','NU_ResizeBottom','Rect,18,H-3,W-18,H');
   //
   if Count>1 then
    begin
     SkinIni.WriteString('_FormSmallCaption','NF_ResizeTopLeft','Image,'+_form+'\'+_smallresize+'_nf_topleft.png,0,0,#000000,#FFFFFF');
     SkinIni.WriteString('_FormSmallCaption','NF_ResizeTopRight','ImageO,'+_form+'\'+_smallresize+'_nf_topright.png,W-'+inttostr(R2.Right)+',0,0,0,#000000,#FFFFFF');
     SkinIni.WriteString('_FormSmallCaption','NF_ResizeTop','ImageO,'+_form+'\'+_smallresize+'_nf_top.png,'+inttostr(R2.Left)+',0,W-'+inttostr(R2.Left+R2.Right)+',0,#000000,#FFFFFF');
    end;
  end;
 //
 SkinIni.WriteString('_FormSmallCaption','CaptionHeight',SCH);
 //
 Layout:=IniClassProp(TmpIni,'Window.SmallCaption','ContentMargins','');
 R2:=ConvertRect(Layout);
 if Layout='' then
  SkinIni.WriteString('_FormSmallCaption','NU_TextRect','6,2,W,CH') else
   begin
    if R2.Left<6 then R2.Left:=6;
    SkinIni.WriteString('_FormSmallCaption','NU_TextRect',inttostr(R2.Left)+',2,W-'+inttostr(R2.Right)+',CH');
   end;
 //TextShadow
 S:=IniClassProp(TmpIni,'Window.SmallCaption(Active)','TextShadowColor','');
 if S<>'' then
  begin
   S2:=IniClassProp(TmpIni,'Window.SmallCaption(Active)','TextShadowType','');
   if SameText(S2,'Single') then
    begin
     SkinIni.WriteString('_FormSmallCaption','NF_ShadowColor',ConvertColor(S));
     SkinIni.WriteString('_FormSmallCaption','NF_HasTextShadow','1');
    end;
  end;
 S:=IniClassProp(TmpIni,'Window.SmallCaption(Inctive)','TextShadowColor','');
 if S<>'' then
  begin
   S2:=IniClassProp(TmpIni,'Window.SmallCaption(Inctive)','TextShadowType','');
   if SameText(S2,'Single') then
    begin
     SkinIni.WriteString('_FormSmallCaption','NU_ShadowColor',ConvertColor(S));
     SkinIni.WriteString('_FormSmallCaption','NU_HasTextShadow','1');
    end;
  end;
 //CaptionText
 S:=IniClassProp(TmpIni,'SysMetrics','CaptionText','255 255 255');
 SkinIni.WriteString('_FormSmallCaption','NF_FontColor',ConvertColor(S));
 //InactiveCaptionText
 S:=IniClassProp(TmpIni,'SysMetrics','InactiveCaptionText','216 228 248');
 SkinIni.WriteString('_FormSmallCaption','NU_FontColor',ConvertColor(S));
 //CaptionFont
 S:=IniClassProp(TmpIni,'SysMetrics','SmallCaptionFont','Tahoma, 8, bold');
 ConvertFont(S,FontName,FontSize,FontStyle);
 SkinIni.WriteString('_FormSmallCaption','NU_FontName',CorrectFont(FontName));
 SkinIni.WriteString('_FormSmallCaption','NU_FontSize',inttostr(FontSize));
 SkinIni.WriteString('_FormSmallCaption','NU_FontStyle',ConvertFontStyle(FontStyle));
end;

procedure TForm1.ProcessFormButtons(const MyName:String;SkinIni,TmpIni:TIniFile);
var BW,BH:Integer;
 OffsetPT:TPoint;
   TCH,TN:Integer;
   CCH,CN:Integer;

 function CHN(CH,N:Integer):String;
 begin
  if CH>0 then Result:='-CH*'+inttostr(CH) else
   if CH<0 then Result:='+CH*'+inttostr(-CH) else
    Result:='';
  if N>0 then Result:=Result+'-'+inttostr(N) else
   if N<0 then Result:=Result+'+'+inttostr(-N);
 end;

begin
 TCH:=0; TN:=6;
 //
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_close,'Close','Window.CloseButton',MyName,SkinIni,TmpIni,BW,BH);
 OffsetPT:=ConvertPoint(IniClassProp(TmpIni,'Window.CloseButton','Offset','0,5'));
 if OffsetPT.Y=0 then OffsetPT.Y:=5;
 CN:=BW; CCH:=0;
 if CN=0 then
  begin
   CCH:=1;
   CN:=-9;
  end;
 if BH=0 then
  SkinIni.WriteString('_Form','NU_CloseRect','W'+CHN(TCH+CCH,TN+CN)+','+inttostr(OffsetPT.Y+1)+',W'+CHN(TCH,TN)+',CH-3') else
   SkinIni.WriteString('_Form','NU_CloseRect','W'+CHN(TCH+CCH,TN+CN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'-'+inttostr(BH)+')|2,W'+CHN(TCH,TN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'+'+inttostr(BH)+')|2');
 Inc(TCH,CCH); Inc(TN,CN+2);
 //
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_help,'Help','Window.HelpButton',MyName,SkinIni,TmpIni,BW,BH);
{$IFDEF FIXMSSTYLESBUGS}
 if Copy(MyName,1,6)='Peony_' then
  BW:=16;
{$ENDIF}
 OffsetPT:=ConvertPoint(IniClassProp(TmpIni,'Window.HelpButton','Offset','0,5'));
 if OffsetPT.Y=0 then OffsetPT.Y:=5;
 CN:=BW; CCH:=0;
 if CN=0 then
  begin
   CCH:=1;
   CN:=-9;
  end;
 if BH=0 then
  SkinIni.WriteString('_Form','NU_HelpRect','W'+CHN(TCH+CCH,TN+CN)+','+inttostr(OffsetPT.Y+1)+',W'+CHN(TCH,TN)+',CH-3') else
   SkinIni.WriteString('_Form','NU_HelpRect','W'+CHN(TCH+CCH,TN+CN)+
        ',(CH+'+inttostr(OffsetPT.Y+1)+'-'+inttostr(BH)+')|2,W'+CHN(TCH,TN)+
        ',(CH+'+inttostr(OffsetPT.Y+1)+'+'+inttostr(BH)+')|2');
 //
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_restore,'Restore','Window.RestoreButton',MyName,SkinIni,TmpIni,BW,BH);
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_maximize,'Maximize','Window.MaxButton',MyName,SkinIni,TmpIni,BW,BH);
{$IFDEF FIXMSSTYLESBUGS}
 if Copy(MyName,1,6)='Peony_' then
  BW:=16;
{$ENDIF}
 OffsetPT:=ConvertPoint(IniClassProp(TmpIni,'Window.MaxButton','Offset','0,5'));
 if OffsetPT.Y=0 then OffsetPT.Y:=5;
 CN:=BW; CCH:=0;
 if CN=0 then
  begin
   CCH:=1;
   CN:=-9;
  end;
 if BH=0 then
  SkinIni.WriteString('_Form','NU_MaximizeRect','W'+CHN(TCH+CCH,TN+CN)+','+inttostr(OffsetPT.Y+1)+',W'+CHN(TCH,TN)+',CH-3') else
   SkinIni.WriteString('_Form','NU_MaximizeRect','W'+CHN(TCH+CCH,TN+CN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'-'+inttostr(BH)+')|2,W'+CHN(TCH,TN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'+'+inttostr(BH)+')|2');
 Inc(TCH,CCH); Inc(TN,CN+2);
 //
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_minimize,'Minimize','Window.MinButton',MyName,SkinIni,TmpIni,BW,BH);
{$IFDEF FIXMSSTYLESBUGS}
 if Copy(MyName,1,6)='Peony_' then
  BW:=16;
 if Copy(MyName,1,8)='x-port2_' then
  begin
   BW:=19;
   BH:=16;
  end;
{$ENDIF}
 OffsetPT:=ConvertPoint(IniClassProp(TmpIni,'Window.MinButton','Offset','0,5'));
 if OffsetPT.Y=0 then OffsetPT.Y:=5;
 CN:=BW; CCH:=0;
 if CN=0 then
  begin
   CCH:=1;
   CN:=-9;
  end;
 if BH=0 then
  SkinIni.WriteString('_Form','NU_MinimizeRect','W'+CHN(TCH+CCH,TN+CN)+','+inttostr(OffsetPT.Y+1)+',W'+CHN(TCH,TN)+',CH-3') else
   SkinIni.WriteString('_Form','NU_MinimizeRect','W'+CHN(TCH+CCH,TN+CN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'-'+inttostr(BH)+')|2,W'+CHN(TCH,TN)+
        ',(CH+'+inttostr(OffsetPT.Y)+'+'+inttostr(BH)+')|2');
 //
 if SkinIni.SectionExists('_Button_CaptionU_Close') then
  begin
   SkinIni.WriteString('_Form','NU_CloseButton','_Button_CaptionU_Close');
   SkinIni.WriteString('_Form','NU_MaximizeButton','_Button_CaptionU_Maximize');
   SkinIni.WriteString('_Form','NU_MinimizeButton','_Button_CaptionU_Minimize');
   SkinIni.WriteString('_Form','NU_RestoreButton','_Button_CaptionU_Restore');
   SkinIni.WriteString('_Form','NU_HelpButton','_Button_CaptionU_Help');
  end;
 //
 if SkinIni.SectionExists('_Button_CaptionF_Close') then
  begin
   SkinIni.WriteString('_Form','NF_CloseButton','_Button_CaptionF_Close');
   SkinIni.WriteString('_Form','NF_MaximizeButton','_Button_CaptionF_Maximize');
   SkinIni.WriteString('_Form','NF_MinimizeButton','_Button_CaptionF_Minimize');
   SkinIni.WriteString('_Form','NF_RestoreButton','_Button_CaptionF_Restore');
   SkinIni.WriteString('_Form','NF_HelpButton','_Button_CaptionF_Help');
  end;
end;

procedure TForm1.SaveFormCaptionButtonGlyphs(const APathPrefix,FName,Layout:String;
           Count:Integer;SkinIni,TmpIni:TIniFile;var ImageWidth:Integer;GTransp:Boolean;
           TranspColor:TColor);
const Postfix:array[1..8]of String=('nf','hf','df','rf','nu','hu','du','ru');
var Bitmap:TBitmap;
        B2:TBitmap;
 CurItem,A:Integer;
       W,H:Integer;
begin
 Bitmap:=LoadBitmap(FName);
 ImageWidth:=0;
 if Bitmap=nil then
  Memo1.Lines.Add('WINDOW.CAPTIONBUTTONGLYPH image ('+FName+') not found!') else
   begin
    CurItem:=0;
    for A:=1 to 8 do
     if (Count>4) or (A>4) then
      begin
       if CurItem<Count then
        begin
         B2:=GetSubBitmap(Bitmap,Layout,Count,CurItem);
         //
         if B2.Width>B2.Height then
          begin
           W:=B2.Width; H:=B2.Height;
          end else
           begin
            W:=B2.Height; H:=B2.Width;
           end;
         //
         if (H=0) or (W div H>=3) then exit;
         ImageWidth:=B2.Width;
         SaveBitmap(B2,APathPrefix+'_'+Postfix[A]+'.png',GTransp,TranspColor);
         B2.Free;
         Inc(CurItem);
        end;
      end;
    Bitmap.Free;
   end;
end;

procedure MakeNonZero(var T:TRect);
begin
 if T.Left<0 then T.Left:=0;
 if T.Top<0 then T.Top:=0;
 if T.Right<0 then T.Right:=0;
 if T.Bottom<0 then T.Bottom:=0;
end;

procedure TForm1.SaveFormCaptionButton(const APathPrefix,ButtonName,Section,MyName:String;
           SkinIni,TmpIni:TIniFile;var ButtonWidth,ButtonHeight:Integer);
const Postfix:array[1..8]of String=('nf','hf','df','rf','nu','hu','du','ru');
     Postfix2:array[1..8]of String=('NU','HU','DF','RU','NU','HU','DF','RU');
     Postfix3:array[1..8]of String=('F','F','F','F','U','U','U','U');
var    FName:String;
      FName2:String;
      Bitmap:TBitmap;
          B2:TBitmap;
        S,S2:String;
      Layout:String;
  Count,DefB:Integer;
     A,B,C,D:Integer;
      Sizing:String;
       R1,R2:TRect;
          WW:array[1..5]of Integer;
    IsTransp:Boolean;
     GTransp:Boolean;
 TranspColor:TColor;
 SimpleGlyph:Boolean;
          SL:TStringList;
    TrueSize:Boolean;
begin
 ButtonWidth:=0;
 ButtonHeight:=0;
 FName:=IniClassProp(TmpIni,Section,'ImageFile','');
 if FName<>'' then
  begin
   Layout:=IniClassProp(TmpIni,Section,'ImageLayout','');
   Sizing:=LowerCase(IniClassProp(TmpIni,Section,'SizingType',''));
   Count:=StrToIntDef(IniClassProp(TmpIni,Section,'imageCount',''),0);
   IsTransp:=SameText(IniClassProp(TmpIni,Section,'Transparent',''),'true');
   TranspColor:=GetTColor(IniClassProp(TmpIni,Section,'TransparentColor','255 0 255'));
   GTransp:=SameText(IniClassProp(TmpIni,Section,'GlyphTransparent',''),'true');
   TrueSize:=SameText(IniClassProp(TmpIni,Section,'SizingType',''),'TrueSize');
   R1:=ConvertRect(IniClassProp(TmpIni,Section,'SizingMargins',''));
   R2:=ConvertRect(IniClassProp(TmpIni,Section,'ContentMargins',''));
   //
   SimpleGlyph:=False;
   FName2:=IniClassProp(TmpIni,Section,'GlyphImageFile','');
{$IFDEF FIXMSSTYLESBUGS}
   if (Copy(MyName,1,10)='PennyLane_') and (ButtonName='Close') then
    FName2:='PennyLane\CloseGlyph.bmp';
{$ENDIF}
   if FName2<>'' then
    begin
     SimpleGlyph:=True;
     SaveFormCaptionButtonGlyphs(APathPrefix+_glyph,FName2,Layout,Count,SkinIni,TmpIni,WW[1],GTransp,TranspColor);
    end else
     begin
      //Glyphs
      SL:=TStringList.Create;
      try
       for A:=5 downto 1 do
        begin
         WW[A]:=0;
         FName2:=IniClassProp(TmpIni,Section,'ImageFile'+inttostr(A),'');
         if (FName2<>'') and (SL.IndexOf(FName2)<0) then
          begin
           SL.Add(FName2);
{$IFDEF FIXMSSTYLESBUGS}
           if ((MyName='BYT_BYT') or (Copy(MyName,1,10)='GoldCoast_') or
               (Copy(MyName,1,9)='NiteLite_') or (Copy(MyName,1,7)='Simplex') or
               (Copy(MyName,1,4)='VS7_') or (Copy(MyName,1,7)='X21Bv8_')) and
              (ButtonName='Close') then
            begin
             if (A=5) or (A=4) then
              TranspColor:=RGB(253,0,233) else
               TranspColor:=RGB(255,0,255);
            end;
           if (Copy(MyName,1,9)='GuiStyle_') and (ButtonName='Close') then
            begin
             if (A=5) or (A=4) or (A=3) then
              TranspColor:=RGB(255,255,255) else
               TranspColor:=RGB(255,0,255);
            end;
{$ENDIF}
           SaveFormCaptionButtonGlyphs(APathPrefix+_glyph+inttostr(A),FName2,Layout,
                                       Count,SkinIni,TmpIni,WW[A],GTransp,TranspColor);
          end;
        end;
      finally
       SL.Free;
      end;
      //WW[1]:=6; WW[2]:=9; WW[3]:=13; WW[4]:=19; WW[5]:=23;
      C:=0;
      for A:=1 to 8 do
       if ((A>4) or (Count>4)) and (C<Count) then
        begin
         Inc(C);
         D:=1;
         DefB:=1;
         for B:=1 to 5 do
          if WW[B]=0 then DefB:=B+1;
         for B:=5 downto 1 do
          if WW[B]>0 then
           begin
            S:=ButtonName+'Glyph'+inttostr(B)+'_'+UpperCase(Postfix[A]);
            SkinIni.WriteString(S,'Element1','E1,IMAGE');
            SkinIni.WriteString(S,'E1.Path',
               SavedShort(ShortPath(APathPrefix+_glyph+inttostr(B)+'_'+Postfix[A]+'.png',MyName),MyName));
            SkinIni.WriteString(S,'E1.Centered','1');
            //
            S2:='_Selective_'+ButtonName+'Glyph_'+UpperCase(Postfix[A]);
            if B<>DefB then
             SkinIni.WriteString(S2,'Style'+inttostr(D),'W>='+inttostr(WW[B])+','+S) else
              SkinIni.WriteString(S2,'Default',S);
            Inc(D);
           end;
        end;
     end;
   //
   Bitmap:=LoadBitmap(FName);
   if Bitmap=nil then
    begin
     Memo1.Lines.Add('WINDOW.CAPTIONBUTTON image ('+FName+') not found!');
     exit;
    end;
   C:=0;
   for A:=1 to 8 do
    if ((A>4) or (Count>4)) and (C<Count) then
     begin
      B2:=GetSubBitmap(Bitmap,Layout,Count,C);
      if TrueSize then
       begin
        ButtonWidth:=B2.Width;
        for B:=1 to 5 do
         if WW[B]>ButtonWidth then
          ButtonWidth:=WW[B];
        ButtonHeight:=B2.Height;
       end;
      SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,APathPrefix+'_'+Postfix[A]+'_*.png',IsTransp,TranspColor);
      B2.Free;
      Inc(C);
      //
      S:=ButtonName+'Button'+'_'+UpperCase(Postfix[A]);
      SkinIni.WriteString(S,'Element1','E1,BOX_TILE');
      SaveRedirectedBoxTile(S,'1',ShortPath(APathPrefix+'_'+Postfix[A]+'_*.png',MyName),MyName,SkinIni);
      if Sizing='stretch' then
       SkinIni.WriteString(S,'E1.ResizeMode','Stretch');
      if SimpleGlyph then
       begin
        SkinIni.WriteString(S,'Element2','E2,IMAGE');
        SkinIni.WriteString(S,'E2.Path',
           SavedShort(ShortPath(APathPrefix+_glyph+'_'+Postfix[A]+'.png',MyName),MyName));
        SkinIni.WriteString(S,'E2.Centered','1');
       end else
        begin
         SkinIni.WriteString(S,'Element2','E2,STYLE');
         SkinIni.WriteString(S,'E2.Style','_Selective_'+ButtonName+'Glyph_'+UpperCase(Postfix[A]));
         MakeNonZero(R2);
         SkinIni.WriteString(S,'E2.Rect',inttostr(R2.Left)+','+inttostr(R2.Top)+',W-'+
                               inttostr(R2.Right)+',H-'+inttostr(R2.Bottom));
        end;
      SkinIni.WriteString('_Button_Caption'+Postfix3[A]+'_'+ButtonName,Postfix2[A]+'_Style',S);
     end;
   Bitmap.Free;
  end;
end;

procedure TForm1.ProcessFormSmallCaptionButtons(const MyName:String;SkinIni,TmpIni:TIniFile);
var BW,BH:Integer;
 OffsetPT:TPoint;
   TCH,TN:Integer;
   CCH,CN:Integer;

 function CHN(CH,N:Integer):String;
 begin
  if CH>0 then Result:='-CH*'+inttostr(CH) else
   if CH<0 then Result:='+CH*'+inttostr(-CH) else
    Result:='';
  if N>0 then Result:=Result+'-'+inttostr(N) else
   if N<0 then Result:=Result+'+'+inttostr(-N);
 end;

begin
 TCH:=0; TN:=6;
 //
 SaveFormCaptionButton(DirPath+MyName+'\'+_formbuttons+'\'+_smallclose,'SmallClose','Window.SmallCloseButton',MyName,SkinIni,TmpIni,BW,BH);
 OffsetPT:=ConvertPoint(IniClassProp(TmpIni,'Window.SmallCloseButton','Offset','0,5'));
 if OffsetPT.Y=0 then OffsetPT.Y:=5;
 CN:=BW; CCH:=0;
 if CN=0 then
  begin
   CCH:=1;
   CN:=-9;
  end;
 if BH=0 then
  SkinIni.WriteString('_FormSmallCaption','NU_CloseRect','W'+CHN(TCH+CCH,TN+CN)+','+inttostr(OffsetPT.Y+1)+',W'+CHN(TCH,TN)+',CH-3') else
   SkinIni.WriteString('_FormSmallCaption','NU_CloseRect','W'+CHN(TCH+CCH,TN+CN)+
        ',(CH+'+inttostr(OffsetPT.Y+1)+'-'+inttostr(BH)+')|2,W'+CHN(TCH,TN)+
        ',(CH+'+inttostr(OffsetPT.Y+1)+'+'+inttostr(BH)+')|2');
 //
 if SkinIni.SectionExists('_Button_CaptionU_SmallClose') then
  SkinIni.WriteString('_FormSmallCaption','NU_CloseButton','_Button_CaptionU_SmallClose');
 if SkinIni.SectionExists('_Button_CaptionF_SmallClose') then
  SkinIni.WriteString('_FormSmallCaption','NF_CloseButton','_Button_CaptionF_SmallClose');
end;

procedure TForm1.ProcessUpDown(const MyName:String;SkinIni,TmpIni:TIniFile);
const BNames:array[1..4]of String=('Up','Down','Left','Right');
     BNames2:array[1..4]of String=('Up','Down','DownHorz','UpHorz');
     BNames3:array[1..4]of String=(_upbutton,_downbutton,_leftbutton,_rightbutton);
     Postfix:array[1..4]of String=('NU','HU','DF','RU');
var       A,B:Integer;
    B2,Bitmap:TBitmap;
 FName,FName2:String;
     Layout,S:String;
        Count:Integer;
     IsTransp:Boolean;
      GTransp:Boolean;
  TranspColor:TColor;
       Sizing:String;
        R1,R2:TRect;
      Section:String;
begin
 for A:=1 to 4 do
  begin
   Section:='Spin.'+BNames2[A];
   FName:=IniClassProp(TmpIni,Section,'ImageFile','');
   if FName<>'' then
    begin
     Layout:=IniClassProp(TmpIni,Section,'ImageLayout','');
     Sizing:=LowerCase(IniClassProp(TmpIni,Section,'SizingType',''));
{$IFDEF FIXMSSTYLESBUGS}
     Sizing:='stretch';
{$ENDIF}
     Count:=StrToIntDef(IniClassProp(TmpIni,Section,'imageCount',''),0);
     IsTransp:=SameText(IniClassProp(TmpIni,Section,'Transparent',''),'true');
     TranspColor:=GetTColor(IniClassProp(TmpIni,Section,'TransparentColor','255 0 255'));
     GTransp:=SameText(IniClassProp(TmpIni,Section,'GlyphTransparent',''),'true');
     R1:=ConvertRect(IniClassProp(TmpIni,Section,'SizingMargins',''));
     R2:=ConvertRect(IniClassProp(TmpIni,Section,'ContentMargins',''));
     //
     FName2:=IniClassProp(TmpIni,Section,'GlyphImageFile','');
     if FName2<>'' then
      begin
       Bitmap:=LoadBitmap(FName2);
       if Bitmap=nil then
        Memo1.Lines.Add('SPIN.'+BNames2[A]+' GLYPH image ('+FName2+') not found!') else
         begin
          for B:=1 to Count do
           if B<=4 then
            begin
             B2:=GetSubBitmap(Bitmap,Layout,Count,B-1);
             SaveBitmap(B2,DirPath+MyName+'\'+_updown+'\'+BNames3[A]+'_'+LowerCase(Postfix[B])+_glyph+'.png',GTransp,TranspColor);
             B2.Free;
            end;
         end;
       Bitmap.Free;
      end;
     //
     Bitmap:=LoadBitmap(FName);
     if Bitmap=nil then
      begin
       Memo1.Lines.Add('SPIN.'+BNames2[A]+' image ('+FName+') not found!');
       exit;
      end;
     for B:=1 to Count do
      if B<=4 then
       begin
        B2:=GetSubBitmap(Bitmap,Layout,Count,B-1);
        SaveBoxTileImages(B2,R1.Left,R1.Right,R1.Top,R1.Bottom,DirPath+MyName+'\'+_updown+'\'+BNames3[A]+'_'+LowerCase(Postfix[B])+'_*.png',IsTransp,TranspColor);
        B2.Free;
        //
        S:=BNames[A]+'Button'+'_'+Postfix[B];
        SkinIni.WriteString(S,'Element1','E1,BOX_TILE');
        SaveRedirectedBoxTile(S,'1',ShortPath(DirPath+MyName+'\'+_updown+'\'+BNames3[A]+'_'+LowerCase(Postfix[B])+'_*.png',MyName),MyName,SkinIni);
        if Sizing='stretch' then
         SkinIni.WriteString(S,'E1.ResizeMode','Stretch');
        SkinIni.WriteString(S,'Element2','E2,IMAGE');
        SkinIni.WriteString(S,'E2.Path',
            SavedShort(ShortPath(DirPath+MyName+'\'+_updown+'\'+BNames3[A]+'_'+LowerCase(Postfix[B])+_glyph+'.png',MyName),MyName));
        SkinIni.WriteString(S,'E2.Centered','1');
        SkinIni.WriteString('_Button_'+BNames[A]+'Button',Postfix[B]+'_Style',S);
       end;
     Bitmap.Free;
    end;
  end;
 if CheckBox7.Checked then
  begin
   SkinIni.WriteString('_Button_UpButton','HighlightInButtonStepsNum','8');
   SkinIni.WriteString('_Button_UpButton','HighlightOutButtonStepsNum','4');
   SkinIni.WriteString('_Button_UpButton','DownButtonStepsNum','4');
   SkinIni.WriteString('_Button_UpButton','UpButtonStepsNum','2');
   SkinIni.WriteString('_Button_UpButton','EnableButtonStepsNum','8');
   SkinIni.WriteString('_Button_UpButton','DisableButtonStepsNum','8');
   //
   SkinIni.WriteString('_Button_DownButton','HighlightInButtonStepsNum','8');
   SkinIni.WriteString('_Button_Downutton','HighlightOutButtonStepsNum','4');
   SkinIni.WriteString('_Button_DownButton','DownButtonStepsNum','4');
   SkinIni.WriteString('_Button_DownButton','UpButtonStepsNum','2');
   SkinIni.WriteString('_Button_DownButton','EnableButtonStepsNum','8');
   SkinIni.WriteString('_Button_DownButton','DisableButtonStepsNum','8');
   //
   SkinIni.WriteString('_Button_LeftButton','HighlightInButtonStepsNum','8');
   SkinIni.WriteString('_Button_LeftButton','HighlightOutButtonStepsNum','4');
   SkinIni.WriteString('_Button_LeftButton','DownButtonStepsNum','4');
   SkinIni.WriteString('_Button_LeftButton','UpButtonStepsNum','2');
   SkinIni.WriteString('_Button_LeftButton','EnableButtonStepsNum','8');
   SkinIni.WriteString('_Button_LeftButton','DisableButtonStepsNum','8');
   //
   SkinIni.WriteString('_Button_RightButton','HighlightInButtonStepsNum','8');
   SkinIni.WriteString('_Button_RightButton','HighlightOutButtonStepsNum','4');
   SkinIni.WriteString('_Button_RightButton','DownButtonStepsNum','4');
   SkinIni.WriteString('_Button_RightButton','UpButtonStepsNum','2');
   SkinIni.WriteString('_Button_RightButton','EnableButtonStepsNum','8');
   SkinIni.WriteString('_Button_RightButton','DisableButtonStepsNum','8');
  end;
 //
 SkinIni.WriteString('_UpDown','UpButton','_Button_UpButton');
 SkinIni.WriteString('_UpDown','DownButton','_Button_DownButton');
 SkinIni.WriteString('_UpDown','UpButtonRect','0,0,W,H|2');
 SkinIni.WriteString('_UpDown','DownButtonRect','0,H|2+1,W,H');
 //
 SkinIni.WriteString('_UpDownH','UpButton','_Button_RightButton');
 SkinIni.WriteString('_UpDownH','DownButton','_Button_LeftButton');
 SkinIni.WriteString('_UpDownH','UpButtonRect','W|2+1,0,W,H');
 SkinIni.WriteString('_UpDownH','DownButtonRect','0,0,W|2,H');
end;

procedure TForm1.ProcessSpinEdit(const MyName:String;SkinIni,TmpIni:TIniFile);
begin
 SkinIni.WriteString('_Edit_SpinEditRV','BaseSkin','_Edit');
 SkinIni.WriteString('_Edit_SpinEditRV','TextRightOffset','20');
 //
 SkinIni.WriteString('_Edit_SpinEditLV','BaseSkin','_Edit');
 SkinIni.WriteString('_Edit_SpinEditLV','TextLeftOffset','20');
 //
 SkinIni.WriteString('_Edit_SpinEditRH','BaseSkin','_Edit');
 SkinIni.WriteString('_Edit_SpinEditRH','TextRightOffset','25');
 //
 SkinIni.WriteString('_Edit_SpinEditLH','BaseSkin','_Edit');
 SkinIni.WriteString('_Edit_SpinEditLH','TextLeftOffset','25');
 //
 SkinIni.WriteString('_SpinEditRV','Edit','_Edit_SpinEditRV');
 SkinIni.WriteString('_SpinEditRV','UpDown','_UpDown');
 SkinIni.WriteString('_SpinEditRV','EditRect','0,0,W,H');
 SkinIni.WriteString('_SpinEditRV','UpDownRect','W-16,2,W-3,H-2');
 //
 SkinIni.WriteString('_SpinEditLV','Edit','_Edit_SpinEditLV');
 SkinIni.WriteString('_SpinEditLV','UpDown','_UpDown');
 SkinIni.WriteString('_SpinEditLV','EditRect','0,0,W,H');
 SkinIni.WriteString('_SpinEditLV','UpDownRect','3,2,16,H-2');
 //
 SkinIni.WriteString('_SpinEditRH','Edit','_Edit_SpinEditRH');
 SkinIni.WriteString('_SpinEditRH','UpDown','_UpDownH');
 SkinIni.WriteString('_SpinEditRH','EditRect','0,0,W,H');
 SkinIni.WriteString('_SpinEditRH','UpDownRect','W-22,H|2-6,W-3,H|2+6');
 //
 SkinIni.WriteString('_SpinEditLH','Edit','_Edit_SpinEditLH');
 SkinIni.WriteString('_SpinEditLH','UpDown','_UpDownH');
 SkinIni.WriteString('_SpinEditLH','EditRect','0,0,W,H');
 SkinIni.WriteString('_SpinEditLH','UpDownRect','3,H|2-6,22,H|2+6');
end;

procedure PackFolder(const Folder:String;const ZipFileName:String);
var T:TZipFile;

 procedure DoPack(const FolderName:String);
 var F:TSearchRec;
   A,B:Integer;
     S:String;
    FS:TFileStream;
 begin
  A:=0;
  repeat
   if A=0 then
    B:=FindFirst(FolderName+'\*.*',faAnyFile,F) else
     B:=FindNext(F);
   if (B=0) and (F.Name<>'.') and (F.Name<>'..') then
    begin
     if F.Attr and faDirectory<>0 then
      begin
       DoPack(FolderName+'\'+F.Name);
      end else
       begin
        T.AddFile(Copy(FolderName+'\'+F.Name,length(Folder)+2,MaxInt));
        FS:=TFileStream.Create(FolderName+'\'+F.Name,fmOpenRead);
        try
         SetLength(S,FS.Size);
         FS.Read(S[1],FS.Size);
         T.Data[High(T.Files)]:=S;
        finally
         FS.Free;
        end;
       end;
    end;
   Inc(A);
  until B<>0;
  FindClose(F);
 end;

begin
 T:=TZipFile.Create;
 try
  DoPack(Folder);
  T.SaveToFile(ZipFileName);
 finally
  T.Free;
 end;
end;

procedure DeleteDirectory(const S:String);
var  SR:TSearchRec;
    A,B:Integer;
      L:TStringList;
     S1:String;
begin
 S1:=WithlastSlash(S);
 L:=TStringList.Create;
 try
  A:=0;
  repeat
   Inc(A);
   if A=1 then B:=FindFirst(S1+'*.*',faAnyFile,SR) else
    B:=FindNext(SR);
   if (B=0) and (SR.Name<>'.') and (SR.Name<>'..') then
    begin
     L.Add(S1+SR.Name);
     if (SR.Attr and faDirectory)<>0 then
      L.Objects[L.Count-1]:=TObject(1) else
       L.Objects[L.Count-1]:=TObject(0);
    end;
  until B<>0;
  FindClose(SR);
  for A:=0 to L.Count-1 do
   if Integer(L.Objects[A])=0 then
    DeleteFile(L.Strings[A]) else
     DeleteDirectory(L.Strings[A]);
  RemoveDir(WithoutlastSlash(S));
 finally
  L.Free;
 end;
end;

procedure TForm1.ProcessSkinScheme(const Name,FileName:String);
var MyName:String;
         F:TextFile;
        FS:TFileStream;
        RS:TResourceStream;
   SkinIni:TIniFile;
    TmpIni:TIniFile;
         L:TSXSkinLibrary;
begin
 SavedBitmaps.Clear;
 Memo1.Lines.Add('--------------------------------------------');
 Memo1.Lines.Add('Processing skin scheme '+Name+'...');
 Application.ProcessMessages;
 MyName:=Name;
 if SameText(Copy(MyName,length(MyName)-3,4),'_INI') then
  Delete(MyName,length(MyName)-3,4);
 Memo1.Lines.Add('Creating SXSkin "'+MyName+'"...');
 if SameText(Copy(MyName,1,6),'NORMAL') then
  Delete(MyName,1,6);
 if FileName<>'' then
  MyName:=FileName+'_'+MyName;
 ForceDirectories(DirPath+MyName);
 ForceDirectories(DirPath+MyName+'\'+_button);
 ForceDirectories(DirPath+MyName+'\'+_checkbox);
 ForceDirectories(DirPath+MyName+'\'+_radiobutton);
 ForceDirectories(DirPath+MyName+'\'+_groupbox);
 ForceDirectories(DirPath+MyName+'\'+_edit);
 ForceDirectories(DirPath+MyName+'\'+_other);
 ForceDirectories(DirPath+MyName+'\'+_form);
 ForceDirectories(DirPath+MyName+'\'+_formbuttons);
 ForceDirectories(DirPath+MyName+'\'+_updown);
 AssignFile(F,DirPath+MyName+'\skin.ini');
 Rewrite(F);
 CloseFile(F);
 FS:=TFileStream.Create(DirPath+MyName+'\tmp.ini',fmCreate);
 RS:=TResourceStream.Create(Module,Name,'TEXTFILE');
 try
  FS.CopyFrom(RS,RS.Size);
 finally
  FS.Free;
  RS.Free;
 end;
 TmpIni:=TIniFile.Create(DirPath+MyName+'\tmp.ini');
 SkinIni:=TIniFile.Create(DirPath+MyName+'\skin.ini');
 try
  ProcessGeneral(MyName,SkinIni,TmpIni);
  ProcessLabel(MyName,SkinIni,TmpIni);
  ProcessButton(MyName,SkinIni,TmpIni);
  ProcessCheckBox(MyName,SkinIni,TmpIni);
  ProcessRadioButton(MyName,SkinIni,TmpIni);
  ProcessGroupBox(MyName,SkinIni,TmpIni);
  ProcessEdit(MyName,SkinIni,TmpIni);
  ProcessForm(MyName,SkinIni,TmpIni);
  ProcessFormSmallCaption(MyName,SkinIni,TmpIni);
  ProcessFormButtons(MyName,SkinIni,TmpIni);
  ProcessFormSmallCaptionButtons(MyName,SkinIni,TmpIni);
  ProcessUpDown(MyName,SkinIni,TmpIni);
  ProcessSpinEdit(MyName,SkinIni,TmpIni);
 finally
  SkinIni.Free;
  TmpIni.Free;
 end;
 DeleteFile(DirPath+MyName+'\tmp.ini');
 Application.ProcessMessages;
 if CheckBox2.Checked then
  begin
   L:=TSXSkinLibrary.Create(nil);
   try
    L.LoadFromFile(DirPath+MyName+'\skin.ini');
    L.SaveToSXSFile(DirPath+MyName+'\skin.sxs');
   finally
    L.Free;
   end;
   Application.ProcessMessages;
   if CheckBox4.Checked then
    DeleteFile(DirPath+MyName+'\skin.ini');
  end;
 if CheckBox2.Checked and CheckBox3.Checked then
  begin
   PackFolder(DirPath+MyName,DirPath+MyName+'.zip');
   Application.ProcessMessages;
   if CheckBox5.Checked then
    DeleteDirectory(DirPath+MyName);
  end;
end;

procedure TForm1.ProcessFile(const FileName:String);
var RS:TResourceStream;
    SL:TStringList;
    WS:WideString;
     S:String;
     A:Integer;
     B:Boolean;
begin
 Memo1.Lines.Add('====================================================');
 Memo1.Lines.Add('Processing file '+ExtractFileName(FileName)+'...');
 DirPath:=ExtractFilePath(FileName);
 Module:=LoadLibraryEx(PChar(FileName),0,LOAD_LIBRARY_AS_DATAFILE);
 RS:=TResourceStream.Create(Module,'#1','FILERESNAMES');
 SL:=TStringList.Create;
 try
  SetString(WS,nil,RS.Size div 2);
  RS.Read(Pointer(WS)^,RS.Size);
  S:='';
  for A:=1 to length(WS) do
   if WS[A]=#0 then S:=S+#13#10 else
    S:=S+WS[A];
  SL.SetText(PChar(S));
  for A:=SL.Count-1 downto 0 do
   if SL[A]='' then SL.Delete(A);
  Memo1.Lines.Add('Found these skin schemes in msstyles file:');
  Memo1.Lines.AddStrings(SL);
  S:=ExtractFileName(FileName);
  if SameText(Copy(S,length(S)-8,9),'.msstyles') then
   Delete(S,length(S)-8,9);
  for A:=0 to SL.Count-1 do
   begin
    B:=True;
    if CheckBox1.Checked then
     B:=SameText(Copy(SL[A],1,6),'NORMAL');
    if B then
     ProcessSkinScheme(SL[A],S);
   end;
 finally
  RS.Free;
  SL.Free;
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var F:TSearchRec;
  A,B:Integer;
begin
 Memo1.Clear;
 if Pos('*',Edit1.Text)=0 then
  begin
   if not FileExists(Edit1.Text) then
    begin
     Memo1.Lines.Add('File "'+Edit1.Text+'" does not exist!!!');
     exit;
    end;
   ProcessFile(Edit1.Text);
  end else
   begin
    B:=0;
    repeat
     if B=0 then
      A:=FindFirst(Edit1.Text,faAnyFile,F) else
       A:=FindNext(F);
     Inc(B);
     if A=0 then
      begin
       ProcessFile(ExtractFilePath(Edit1.Text)+F.Name);
      end;
    until A<>0;
    FindClose(F);
   end;
end;

initialization

 SavedBitmaps:=TSXSavedBitmapList.Create;

finalization

 SavedBitmaps.Free; 

end.
