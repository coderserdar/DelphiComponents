unit Extractor;
{
  TArchiver by Morgan Martinet (C) 1998 - mmm@imaginet.fr or mmm@mcom.fr

  COPYRIGHT
  ---------

  This component is email-ware. You may use it, distribute it and modify it, but
  you may not charge for it. Please send me a mail if you use it, I'll be happy
  to see in which country it is used, and I'll be able to mail you the updates.

  In case of modifications you must mail me a copy of the modifications.
  The reason are simple: Any changes that improve this free-ware component should
  be to benefit for everybody, not only you. That way you can be pretty sure,
  that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath.

  DESCRIPTION
  -----------

  This component lets you add/extract files to/from an archive.

}

interface
uses
  Windows,
  SysUtils,
  Classes,
  Cryptcon,
  ArchiverMisc,
  ArchiverRoot,
  CustExtractor;

type
  TExtractor = class( TCustomExtractor )
  protected
    FCryptoObject : TCrypto;

    procedure InitCrypting; override;
    procedure DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer); override;
    function  NeededBlockSize : Integer; override;
    function  UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean; override;
  public
  end;

procedure Register;

implementation
uses Blowunit, zUnCompr, BZLib, ZUtil, aDiff;

procedure Register;
begin
  RegisterComponents('Backup Tools', [TExtractor]);
end;

procedure TExtractor.InitCrypting;
begin
  FCryptoObject := TBlowFish.Create( Self );
  with FCryptoObject do
    begin
      InputType := SourceByteArray;
      CipherMode  := ECBMode;
    end;
end;

procedure TExtractor.DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer);
var
  ShouldContinue : Boolean;
  tmpSrc, tmpDest : PChar;
  blockSize, processed, diff : Integer;
begin
  if Assigned(FOnDecryptBlock) then
    FOnDecryptBlock( Self, DestBlock, SrcBlock, DestSize, SrcSize )
  else
    with FCryptoObject do
    begin
      ShouldContinue := False;
      Key := FCryptKey;
      tmpSrc := SrcBlock;
      tmpDest := DestBlock;
      processed := 0;
      diff := Integer(SrcBlock[SrcSize-1]);
      Dec( SrcSize );
      DestSize := SrcSize - diff;
      repeat
        BlockSize := Min( kMaxCryptBuffer, SrcSize - processed );
        InputLength  := BlockSize;
        pInputArray  := Pointer(tmpSrc);
        pOutputArray := Pointer(tmpDest);
        DecipherData(ShouldContinue);
        ShouldContinue := True;
        Inc( processed, BlockSize );
        tmpSrc  := tmpSrc  + BlockSize;
        tmpDest := tmpDest + BlockSize;
      until processed >= SrcSize;
    end;
end;

function  TExtractor.NeededBlockSize : Integer;
begin
  // We need 1% more, of the Source Block for the Dest Block.
  Result := FHeader.BlockSize + FHeader.BlockSize div 100;
end;

function  TExtractor.UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean;
type
  T = array [0..1024] of Byte;
  PT = ^T;
var
  p : PT;
  tmpDestSize : uLong;
begin
  if Assigned( FOnUncompressBlock ) then
    Result := FOnUncompressBlock( Self, DestBlock, DestSize, SrcBlock, SrcSize )
  else
    begin
      p := PT(SrcBlock);
      tmpDestSize := DestSize; // This is needed for Delphi2, because it believes
                               // that the Integer/uLong are different type,
                               // even if uLong is a Longint, and of course
                               // an integer for Delphi 2 and later !
                               // Delphi 3 accepts it without any problem !
      Result := zUnCompr.uncompress (pBytef(DestBlock), tmpDestSize, p^, SrcSize) = Z_OK;
      DestSize := tmpDestSize;
    end;
end;

end.
