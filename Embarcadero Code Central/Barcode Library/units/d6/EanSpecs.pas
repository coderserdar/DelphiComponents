unit EanSpecs;

interface

{$I ean.inc}

type
    TGraphicsType     = (gtBmp,gtWmf,gtEmf, gtJPeg, gtGif);

    TTypBarCode = ( bcEan8,bcEan13,bcCodabar,
               bcCode39Standard,bcCode39Full,
               bcCode93Standard,bcCode93Full,
               bcCode128,bcABCCodabar,
               bc25Datalogic,bc25Interleaved,bc25Matrix,
               bc25Industrial,bc25IATA,bc25Invert,

               { type of barcode added in new version}
               bcITF, bcISBN, bcISSN, bcISMN,
               bcUPCA,
               bcUPCE0,bcUPCE1,
               bcUPCShipping,
               bcJAN8, bcJAN13,

               {bcPlessey,
               bcMSI,}
               bcMSIPlessey,
               {bcADSPlessey,}

               bcPostNet,
               bcOPC,
               bcEan128,
               bc25Coop, bcCode11,
               bcPZN

               {$ifdef PSOFT_PDF417}
                        , bcPDF417
               {$endif}
               );

               {Show Indicator - char > right from barcode

               inserted into type of barcode, but now not implemented
                in next version we implemented
                bcEan11(Code11 ?), (ITF14?),
                Code B, Cstate4
                PM4SCC	Royal Mail and European 4 state code
                FIM-A	FIM-A
                FIM-B	FIM-B
                FIM-C	FIM-C
                4-State	European 4 state code
                RM4SCC	Royal Mail 4 state code
                TelepenA	Telepen ASCII
                Telepen	Telepen Numeric
                TelepenN	Telepen Numeric + begin & end
                Code 128A, Code128C
                ASDA 8 - kod odvodeny od EAN8
                Nixdorf barcode - odvodeny od EAN 13
                Glaxo Welcome company
                GW Dartford - odvodene od Pharma
                GW Code 39
                GW MSI
                GW Ean 13
                GW Ean 8
                GW Pharma

                M&S7 - odvodene od EAN 8

                Novartis Pharma
                Code 25
                ASDA 8,  M&S 7 MSI, Wickes 8, Woolworth 8
                Pharma Code, Glaxo Wellcome IMH Code 39, Kurandt
                Novartis Pharma, PZN Code 39
                HIBC Code 39 , HIBC/HIBC-LIC
                LOGMARS
	        German Post Office AG
	        Identity and Lead Code

                OCR-A, OCR-B, MICR, and CMC7}

  TBarCodeInfo = record
     Name           : String;
     LongName       : String;
     InitValue      : String;
     Chars          : String;
     ParentCode     : TTypBarCode;
     Year           : Word;
     Country        : String[3];
     EnabledAdd     : Boolean;
     AutoCaption    : Boolean;
     OptCharWidthMM : Double;
     Length         : Word;
  end;


       function BarcodeInfo(T:TTypBarcode):TBarCodeInfo;

const
    grExtensions : array [TGraphicsType] of string=
      ('.Bmp','.Wmf','.Emf','.Jpg','.Gif');

implementation

function BarcodeInfo(T:TTypBarcode):TBarCodeInfo;
const _numbers='0123456789';
begin
   with Result do
        case T of
             bcEan8          : begin
                                  Name           := 'EAN 8';
                                  LongName       := 'EAN 8 (2 or 5 digit supplement)';
                                  InitValue      := '1234567';
                                  Chars          := _Numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 1977;
                                  Country        := 'EU';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcEan13         : begin
                                  Name           := 'EAN 13';
                                  LongName       := 'EAN 13 (2 or 5 digit supplement)';
                                  InitValue      := '9771210107001';
                                  Chars          := _Numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 1977;
                                  Country        := 'EU';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcCodabar       : begin
                                  Name           := 'Codabar';
                                  LongName       := 'Codabar (Monarch, NW-7, USD-4, 2 of 7 code)';
                                  InitValue      := 'A0123+-$/:68A';
                                  Chars          := '0123456789-$:/.+ABCD';
                                  ParentCode     := bcCodabar;
                                  Year           := 1972;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode39Standard: begin
                                  Name           := 'Code 39 Standard';
                                  LongName       := 'Code 39 Standard';
                                  InitValue      := 'PSOFT';
                                  Chars          := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%';
                                  ParentCode     := bcCode39Standard;
                                  Year           := 1972;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode39Full    : begin
                                  Name           := 'Code 39 Extended';
                                  LongName       := 'Code 39 Extended';
                                  InitValue      := 'PSOFT';
                                  Chars          :={ #1#2#3#4#5#6#7#8#9#10
                                     + #11#12#13#14#15#16#17#18
                                     + #19#20#21#22#23#24#25#26
                                     + #27#28#29#30#31
                                     +}' !"#$%&''()*+,-./0123456789'
                                     +':;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                     +'[\]^_`'
                                     +'abcdefghijklmnopqrstuvwxyz{|}~'#128;
                                  ParentCode     := bcCode39Standard;
                                  Year           := 1974;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode93Standard: begin
                                  Name           := 'Code 93 Standard';
                                  LongName       := 'Code 93 Standard';
                                  InitValue      := 'PSOFT';
                                  Chars          := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%&"()';
                                  ParentCode     := bcCode93Standard;
                                  Year           := 1982;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode93Full    : begin
                                  Name           := 'Code 93 Extended';
                                  LongName       := 'Code 93 Extended';
                                  InitValue      := 'PSOFT';
                                  Chars          := ' !"#$%&''()*+,-./'
                                    +'0123456789'
                                    +':;<=>?@'
                                    +'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                    +'[\]^_`'
                                    +'abcdefghijklmnopqrstuvwxyz'
                                    +'{|}~';
                                  ParentCode     := bcEan13;
                                  Year           := 1974;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode128       : begin
                                  Name           := 'Code 128';
                                  LongName       := 'Code 128';
                                  InitValue      := 'PSOFT';
                                  Chars          := ' !"#$%&''()*+,-./0123456789:;<=>?@'
                                    +'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                    +'[\]^_`'
                                    +'abcdefghijklmnopqrstuvwxyz'
                                    +'{|}~'+ 'θύανι';
                                    
                                  ParentCode     := bcCode128;
                                  Year           := 1981;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcABCCodabar    : begin
                                  Name           := 'AbcCodabar';
                                  LongName       := 'AbcCodabar';
                                  InitValue      := 'ABCCBA';
                                  Chars          := '0123456789-$:/.+ABCD ';
                                  ParentCode     := bcCodabar;
                                  Year           := 1977;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Datalogic   : begin
                                  Name           := '2/5 Datalogic';
                                  LongName       := '2/5 Datalogic, Code25, 2 of 5';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 1968;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Interleaved : begin
                                  Name           := '2/5 Interleaved (ITF)';
                                  LongName       := 'Code25, 2 of 5, ITF';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 1972;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Matrix      : begin
                                  Name           := '2/5 Matrix ';
                                  LongName       := 'Code25, 2/5 Matrix';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 1968;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Industrial  : begin
                                  Name           := '2/5 Industrial';
                                  LongName       := 'Code25, 2/5 Industrial';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 1972;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25IATA        : begin
                                  Name           := '2/5 IATA';
                                  LongName       := 'Code25, 2/5 IATA';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Invert      : begin
                                  Name           := '2/5 INVERT';
                                  LongName       := 'Code25, 2/5 INVERT';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Datalogic;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcITF           : begin
                                  Name           := 'ITF';
                                  LongName       := 'ITF6, ITF14 (SSC14), ITF16';
                                  InitValue      := '0977121010700';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Interleaved;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 3;
                                  Length         := 0;
                               end;
             bcISBN          : begin
                                  Name           := 'ISBN (Bookland)';
                                  LongName       := 'ISBN (International Standard Book Number)';
                                  InitValue      := '80-7226-102';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcISSN          : begin
                                  Name           := 'ISSN';
                                  LongName       := 'ISSN (International Standard Serial Number)';
                                  InitValue      := '80-7226-102';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcISMN          : begin
                                  Name           := 'ISMN';
                                  LongName       := 'ISMN (International Standard Music Number)';
                                  InitValue      := '80-7226-102';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcUPCA          : begin
                                  Name           := 'UPC-A';
                                  LongName       := 'UPC-A (with or without supplements)';
                                  InitValue      := '01234567890';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcUPCE0         : begin
                                  Name           := 'UPC-E0';
                                  LongName       := 'UPC-E0 (with or without supplements)';
                                  InitValue      := '0123456';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcUPCE1         : begin
                                  Name           := 'UPC-E1';
                                  LongName       := 'UPC-E1 (with or without supplements)';
                                  InitValue      := '0123456';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcUPCShipping   : begin
                                  Name           := 'UPC-Shipping';
                                  LongName       := 'UPC-Shipping';
                                  InitValue      := '1234567890123';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Interleaved;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcJAN8          : begin
                                  Name           := 'JAN8';
                                  LongName       := 'Japanese version of  EAN 8';
                                  InitValue      := '49123456';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcJAN13         : begin
                                  Name           := 'JAN13';
                                  LongName       := 'Japanese version of  EAN 13';
                                  InitValue      := '491234567890';
                                  Chars          := _numbers;
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := True;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcMSIPlessey    : begin
                                  Name           := 'MSI/Plessey';
                                  LongName       := 'MSI/Plessey (Modified)';
                                  InitValue      := '';
                                  Chars          := '0123456789ABCDEF';
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := 'USA';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 2.5;
                                  Length         := 0;
                               end;
             bcPostNet       : begin
                                  Name           := 'PostNet';
                                  LongName       := 'PostNet (ZIP, ZIP+4, DPBC)';
                                  InitValue      := '123456789';
                                  Chars          := '0123456789ABCDEF';
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := 'USA';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 6;
                                  Length         := 0;
                               end;
             bcOPC           : begin
                                  Name           := 'OPC';
                                  LongName       := 'OPC (Optical Industry Association)';
                                  InitValue      := '1234567897';
                                  Chars          := '0123456789ABCDEF';
                                  ParentCode     := bcEan13;
                                  Year           := 0;
                                  Country        := 'USA';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 6;
                                  Length         := 0;
                               end;
             bcEan128       : begin
                                  Name           := 'UCC/EAN 128';
                                  LongName       := 'UCC/EAN 128';
                                  InitValue      := 'EAN 128';
                                  Chars          := ' !"#$%&''()*+,-./0123456789:;<=>?@'
                                    +'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                    +'[\]^_`'
                                    +'abcdefghijklmnopqrstuvwxyz'
                                    +'{|}~θύανι';
                                  ParentCode     := bcCode128;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bc25Coop : begin
                                  Name           := '2/5 Coop';
                                  LongName       := 'Code25, 2/5 Matrix';
                                  InitValue      := '123456';
                                  Chars          := _numbers;
                                  ParentCode     := bc25Matrix;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcCode11 : begin
                                  Name           := 'Code 11';
                                  LongName       := 'Code 11 (USD-8)';
                                  InitValue      := '123456';
                                  Chars          := '0123456789-';
                                  // ParentCode     := bc25Matrix;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
             bcPZN : begin
                                  Name           := 'PZN';
                                  LongName       := 'Pharma-Zentral-Nummer';
                                  InitValue      := '123456';
                                  Chars          := '0123456789';
                                  Parentcode     := bcCode39Standard;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
        {$ifdef PSOFT_PDF417}
             bcPDF417 : begin
                                  Name           := 'PDF417';
                                  LongName       := '';
                                  InitValue      := 'PSOFT, http://www.psoft.sk, email: peter@psoft.sk';
                                  Chars          := '';
                                  // Parentcode     := bcCode39Standard;
                                  Year           := 0;
                                  Country        := '';
                                  EnabledAdd     := False;
                                  AutoCaption    := False;
                                  // OptCharWidthMM := 4;
                                  Length         := 0;
                               end;
        {$endif}
        end;

end;

end.
