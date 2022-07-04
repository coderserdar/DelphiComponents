How to fix AV in PngImage (1.564) with 32bpp .png images
(when fit is off):

procedure TPngObject.DrawPartialTrans(DC: HDC; Rect: TRect); 
... 
        {Process all the pixels in this line} 
        FOR i := 0 TO W - 1 DO 
        begin 
          if Stretch then i2 := trunc(i / FactorX) else i2 := i; 
          {Optmize when we don_t have transparency} 
          if (AlphaSource[i2] <> 0) then 
            if (AlphaSource[i2] = 255) then 
//              ImageData[i] := pRGBQuad(@ImageSource[i2 * 3])^ // remove
              CopyMemory(@ImageData[i], @ImageSource[i2 * 3], 3) // add

Thanx Dec

