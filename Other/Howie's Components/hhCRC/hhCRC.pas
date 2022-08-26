unit hhCRC;

 { hhCRC calculates a cyclic redundancy code (CRC) in three formats:
     CRC-16 (X^16 + X^15 + X^2 + 1)
     CCITT  (X^16 + X^12 + X^5 + 1)
     CRC-32 (X^32+X^26+X^23+X^22+X^16+X^12+X^11+X^11+X^10+X^8+X^7+X^5+X^4+X^2 +X+1)
   by using a byte-wise algorithm.

  (C) Copyright 1989, 1996 Earl F. Glynn, Overland Park, KS.
  (C) Copyleft  2002, Howard Harvey, Houghton, South Australia

  This UNIT was derived from the CRCT FORTRAN 77 program given in
  "Byte-wise CRC Calculations" by Aram Perez in IEEE Micro, June 1983,
  pp. 40-50.
  While the constants are specific to the CRC16, CCITT or CRC32
  polynomial, the algorithm is general and will calculate the CRC for
  whatever set of constants is present.

  This CRC algorithm emphasizes speed at the expense of the size of the
  lookup tables.}

interface

uses
  Windows,Classes;

type
  TCRCmode = (CRC16,CCITT,CRC32) ;

  ThhCRC = class(TComponent)
  private
    { Private declarations }
    fZeroise  : boolean ;
    fCRCmode  : TCRCmode ;
    fCRCvalue : Dword ;
    fBytes    : integer ;
  protected
    { Protected declarations }
    procedure SetZeroise( mode : boolean ) ;
    procedure SetCRCmode( mode : TCRCmode ) ;
    procedure SetCRCvalue( value : Dword ) ;
    procedure SetBytes( value : integer ) ;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcCRC( p : pointer );
  published
    property Zeroise : boolean  read fZeroise  write SetZeroise  default true ;
    property CRCmode : TCRCmode read fCRCmode  write SetCRCmode  default CRC16 ;
    property CRC     : Dword    read fCRCvalue write SetCRCvalue default 0 ;
    property Bytes   : integer  read fbytes    write SetBytes    default 1 ;
    { Published declarations }
  end;

procedure Register;

implementation

CONST

{ Table data for CRC16 calculations }

  CRC16table:  array[0..255] of word =
   ($0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040);

{ Table data for CCITT calculations }

  CCITTtable:  array[0..255] of word =
   ($0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7, 
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6, 
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE, 
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D, 
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4, 
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC, 
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B, 
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12, 
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A, 
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49, 
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70, 
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78, 
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067, 
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E, 
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D, 
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634, 
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3, 
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A, 
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9, 
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8, 
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0);

{ Table data for CRC32 calculations }

  CRC32table:  array [0..255] of Dword =
   ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

{ -------------------------------------------------------------------- }

constructor ThhCRC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fZeroise := true ;
  fCRCmode := CRC16 ;
  fCRCvalue := 0 ;
  fBytes := 1 ;
end;

{ -------------------------------------------------------------------- }

destructor ThhCRC.Destroy;
begin
  inherited Destroy;
end;

{ -------------------------------------------------------------------- }

procedure ThhCRC.CalcCRC( p : pointer );

{ Calculate the CRC.
  Note: The form of the call depends on how the buffer is created.

  If the array is declared, then the code will be of the form:
        VAR buffer: array[0..16383] of byte ;
            ....
            hhCRC.Bytes := 16384 ;
            hhCRC.CalcCRC( @buffer ) ;

  If the array is open, then the code will be of the form:
        VAR buffer: array of byte ;
            bufflen : integer ;
            ....
            bufflen := 16384 ;
            SetLength( buffer , bufflen ) ;
            ....
            hhCRC.Bytes := bufflen ;
            hhCRC.CalcCRC( buffer ) ; }

var
  index : integer ;
  pthis : ^byte ;
  this  : byte ;

{The following is a little cryptic (but executes very quickly).
    The algorithm is as follows:
      1.  exclusive-or the input byte with the low-order byte of
          the CRC register to get an INDEX
      2.  shift the CRC register eight bits to the right
      3.  exclusive-or the CRC register with the contents of
          the appropriate table[INDEX]
      4.  repeat steps 1 through 3 for all bytes}

begin

{ Initialise the CRC store }

  if fZeroise then
    if fCRCmode = CRC32
    then fCRCvalue := $FFFFFFFF { Zeroise for CRC32 is $FFFFFFFF }
    else fCRCvalue := 0 ;       { Zeroise for CRC16, CCITT is $0000 }

{ Equate the untyped pointer to a byte pointer }
  pthis := p ;

{ Loop through the buffer byte by byte }

  for index := 1 to fbytes do
  begin
    this := pthis^ ;            { Pick up the byte }
    case fCRCmode of
      CRC16: fCRCvalue := Hi(fCRCvalue)
                          XOR CRC16table[ this XOR Lo(fCRCvalue) ] ;
      CCITT: fCRCvalue := Hi(fCRCvalue)
                          XOR CCITTtable[ this XOR Lo(fCRCvalue) ] ;
      CRC32: fCRCvalue := (fCRCvalue SHR 8)
                          XOR CRC32table[this XOR (fCRCvalue AND $000000FF)] ;
    else ;                      { Catchall }
    end ;
    INC(pthis) ;                { Bump buffer pointer }
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhCRC.SetZeroise( mode : boolean ) ;
begin
  fZeroise := mode ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhCRC.SetCRCvalue( value : Dword ) ;
begin
  fCRCvalue := value ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhCRC.SetBytes( value : integer ) ;
begin
  fBytes := value ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhCRC.SetCRCmode( mode : TCRCmode ) ;
begin
  fCRCmode := mode ;
end ;

{ -------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('Howie', [ThhCRC]);
end;

end.

