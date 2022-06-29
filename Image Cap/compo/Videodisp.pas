unit VideoDisp;

interface
 uses Windows, Messages, SysUtils, Classes, Graphics, Controls,stdctrls,
  ExtCtrls,vfw,mmsystem;



//////////////////////////////////////////////////////////////////////////////////
// Video  Display

type ENoHDD  = class(Exception);



type
  TVideoDisp = class(TCustomControl)
   private
    Hdd:HDrawDib;   // Handle of the DrawDibDC
    fBitmapInfoHeader:TBitmapinfoHeader; // Info Header of Frames
    fstreaming:Boolean; // True when Video Stream is running
    frate:integer; // Streaming Rate
    fscale:boolean;   // Scale Bitmap to window
    fprop:boolean;
    fBiWidth:integer; // Height and Width for DrawDibDraw
    fbiHeight:integer;
    procedure SetInfoHeader(Header:TBitmapInfoHeader);  // Setting BitmapInfo Header
    procedure SetStreaming(streaming:Boolean);    // Streaming On / Off
    procedure SetRate(rate:integer);            // Rate of Streaming
    procedure SetSize(var Msg:TMessage); message wm_size; // Handling Sizing
    procedure calcSize(w,h:integer);  // calc size of Output
    procedure SetScale(scaling:Boolean);  // Set Scaling
    procedure SetProp(prop:Boolean);


  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure DrawStream(Frame:Pointer; KeyFrame:Boolean);
    property  BitMapInfoHeader:TBitmapInfoHeader read fbitmapInfoHeader write SetInfoHeader;
    property  Streaming:boolean read fstreaming write SetStreaming;


  published
    property ScaleToWindow:boolean read FScale write setScale;
    property StreamRate:integer read frate write setRate;
    property ScaleProportional:boolean read fprop  write SetProp;

    property align;
    property color;
    property visible;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnClick;
    Property OnDblClick;
 end;

 Procedure Register;

implementation

constructor TVideoDisp.Create(AOwner: TComponent);
var e:Exception;
 begin
   inherited Create(aOwner);
   Width:= 100;
   height:=75;
   color := clblack;
   fstreaming:= false;
   frate:= 66667;
   hdd:=DrawDibOpen;
   fbitmapinfoheader.biWidth := 100;
   fbitmapinfoheader.biHeight:= 100;
   fbitmapInfoHeader.biSize:=0;
   if hdd = 0 then
    begin
     e:=ENoHDD.Create('Can not Create HDRAWDIB');
     raise e;
    end;
  end;


destructor TVideoDisp.Destroy;
begin
 DrawDibClose(hdd);
 inherited Destroy;
end;

procedure TVideoDisp.SetInfoHeader(Header:TBitmapInfoHeader);
begin
 fBitmapInfoHeader:= header;
 calcSize(width,height);
end;


// Draw a new Picture of the Frame

procedure TVideoDisp.DrawStream(Frame:Pointer;KeyFrame:Boolean);

var Flags:word;
  //  e:Exception;

begin
 if bitmapinfoHeader.bisize = 0 then exit;
 flags := DDF_SAME_HDC or DDF_SAME_DRAW;
 if not Keyframe then Flags:= flags or DDF_NOTKEYFRAME  ;
 DrawDibDraw(hdd,canvas.handle,0,0,fbiwidth,fbiheight,@fBitmapInfoHeader,
             frame,0,0,fBitmapInfoHeader.biWidth,fbitmapInfoHeader.biheight,flags);

end;

// Set Streaming Rate
procedure TVideoDisp.SetRate(rate:integer);
begin
  if fstreaming then DrawDibStop(hdd);
  frate := rate;
  if Streaming then DrawDibStart(hdd,frate);
end;

// Toggeling Streaming mode
 procedure TVideoDisp.SetStreaming(streaming:boolean);
 begin
   if streaming = fstreaming then exit;
   if fstreaming then
     DrawDibStop(hdd)
   else
     DrawDibStart(hdd,frate);
    fstreaming := streaming;
 end;

procedure TVideoDisp.SetSize(var Msg:TMessage);

begin
   calcsize(LOWORD(msg.lParam),HIWORD(msg.lParam));
end;


procedure TVideoDisp.calcSize(w,h:integer);
 var f,cf:double;
begin
 if fscale then
   begin
     if fprop then
        begin
        f:= W/h;
        cf:= fBitmapInfoHeader.biWidth/fbitmapInfoHeader.biHeight;
        if cf  <  f then
         begin
          fbiWidth:= round(h*cf);
          fbiHeight:= h;
         end
        else
          begin
          fbiWidth:= w;
          fbiHeight:= round(w*1/cf);
         end
   end
   else
    begin
     fbiheight:= h;
     fbiwidth:=  w;
    end
  end
   else
     begin
      fbiheight:=fbitmapInfoHeader.biHeight;
      fbiwidth:= fbitmapInfoHeader.biWidth;
    end;
  if fbitmapInfoHeader.biSize <> 0 then
   DrawDibBegin(hdd,canvas.handle,fbiwidth,fbiheight,@fBitmapInfoHeader,
            fBitmapInfoHeader.biWidth,fbitmapInfoHeader.biheight,0);
end;

procedure TVideoDisp.SetScale(scaling:Boolean);
 begin
  if scaling = fscale then exit;
  fscale:= scaling;
  calcSize(width,height);
end;


procedure TVideoDisp.SetProp(prop:Boolean);
 begin
  if fprop = prop then exit;
  fprop:=prop;
  calcSize(width,height);
end;





procedure Register;
begin
  RegisterComponents( 'Video', [TVideoDisp]);
end;

end.
 