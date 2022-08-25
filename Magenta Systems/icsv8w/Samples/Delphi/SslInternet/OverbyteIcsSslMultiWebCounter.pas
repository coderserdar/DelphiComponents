{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The purpose is to implement a counter for the web application.
              The counter is actually a dynamic image generator: it
              implement a request which returns a jpged image build on the fly
              with the counter value whose name is specified in the
              request parameter.
Version:      8.64
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2020 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslMultiWebCounter;
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils,
  {$IFDEF FMX}
    System.Types, System.UITypes, System.UIConsts, FMX.Types,
  {$IF Compilerversion >= 25}
    FMX.Graphics,
  {$IFEND}
  {$ELSE}
    Graphics, Jpeg,
  {$ENDIF}
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    OverbyteIcsSslMultiWebDataModule,
    OverbyteIcsSslMultiWebHttpHandlerBase,
    OverbyteIcsSslMultiWebUrlDefs,
    OverbyteIcsSslMultiWebSessionData;

type
    TUrlHandlerCounterJpg = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation

{ THttpHandlerCounterJpg }

procedure TUrlHandlerCounterJpg.Execute;
var
    BitMapImg     : TBitMap;
    CounterString : String;
    Counter       : Integer;
    CounterRef    : String;
  {$IFNDEF FMX}
    JpegImg       : TJPEGImage;
  {$ENDIF}
begin
    ExtractURLEncodedValue(Params, 'Ref', CounterRef);
    if CounterRef = '' then
        CounterRef := 'Counter';   // Not found, use default value 'Counter'

    // Use a separate counter for not logged access
    if not ValidateSession then
        CounterRef := 'NotLogged_' + CounterRef;

    Counter := SslMultiWebDataModule.CounterIncrement(CounterRef);

    // We only display text. Convert the counter value to text
    CounterString := IntToStr(Counter);

    // Now build the JPEG image
    if Assigned(DocStream) then
        DocStream.Free;
    DocStream := TMemoryStream.Create;
  {$IFDEF FMX}
    BitMapImg := TBitmap.Create(64, 32);
    try
        with BitMapImg.Canvas do begin
            BeginScene;
            try
                StrokeThickness := 2;
                Fill.Color      := claGray;
                FillRect(RectF(0, 0, BitMapImg.Width, BitMapImg.Height),
                         16, 16, AllCorners, 1.0);
                DrawRect(RectF(1, 1, BitMapImg.Width -1, BitMapImg.Height -1),
                         16, 16, AllCorners, 1.0);
                Font.Family := 'Arial';
                Font.Size   := 16;
                Fill.Color  := claWhite;
                FillText(RectF(1, 1, BitMapImg.Width -1, BitMapImg.Height -1),
                         CounterString, False, 1.0, [], TTextAlign.taCenter,
                         TTextAlign.taCenter);
            finally
                EndScene;
            end;
        end;
        {with DefaultBitmapCodecClass.Create do begin
            try
                SaveToStream(DocStream, BitMapImg, 'jpeg');
            finally
                Free;
            end;
        end;}
        BitMapImg.SaveToStream(DocStream); // defaults to png
    finally
        BitMapImg.Free;
    end;
    AnswerStream('', 'image/png', NO_CACHE);
  {$ELSE}
    JpegImg := TJPEGImage.Create;
    try
        BitMapImg := TBitMap.Create;
        try
            BitMapImg.Width  := 64;
            BitMapImg.Height := 32;
            BitMapImg.Canvas.Pen.Color   := clBlack;
            BitMapImg.Canvas.Brush.Color := clGray;
            BitMapImg.Canvas.RoundRect(0, 0,
                BitMapImg.Width - 1, BitMapImg.Height - 1, 16, 16);
            BitMapImg.Canvas.Font.Name  := 'arial';
            BitMapImg.Canvas.Font.Size  := 14;
            BitMapImg.Canvas.Font.Color := clWhite;
            BitMapImg.Canvas.TextOut(
  (BitMapImg.Width  - BitMapImg.Canvas.TextWidth(CounterString))  div 2 - 1,
  (BitMapImg.Height - BitMapImg.Canvas.TextHeight(CounterString)) div 2 - 1,
               CounterString);
            JpegImg.Assign(BitMapImg);
            JpegImg.SaveToStream(DocStream);
        finally
            BitMapImg.Destroy;
        end;
    finally
        JpegImg.Destroy;
    end;
    AnswerStream('', 'image/jpeg', NO_CACHE);
  {$ENDIF FMX}
    Finish;
end;

end.
