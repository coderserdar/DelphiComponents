unit Archiver;
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
  CustArchiver;

type
  TArchiver = class( TCustomArchiver )
  protected
    FCryptoObject : TCrypto;

    procedure InitCrypting; override;
    procedure CryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer); override;
    procedure DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer); override;
    function  NeededBlockSize : Integer; override;
    function  CompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer ) : Boolean; override;
    function  UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean; override;
    function  SelectDirectory(var Directory: string; Options: TMySelectDirOpts; HelpCtx: Longint):Boolean; override;
    function  SelectFile( const aTitle : String; var aFileName : String ) : Boolean; override;
    function  CompressionLevelAsInteger : Integer; override;
  public
  end;

procedure Register;

implementation
uses Blowunit, zCompres, zUnCompr, bZLib, ZUtil, FileCtrl, Dialogs, Controls;

procedure Register;
begin
  RegisterComponents('Backup Tools', [TArchiver]);
end;

procedure TArchiver.InitCrypting;
begin
  FCryptoObject := TBlowFish.Create( Self );
  with FCryptoObject do
    begin
      InputType := SourceByteArray;
      CipherMode  := ECBMode;
    end;
end;

procedure TArchiver.CryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer);

  function Compl( val, modulo : Integer ) : Integer;
  var
    tmp : Integer;
  begin
    tmp := Abs(val) mod modulo;
    if tmp > 0 then
      Result := modulo - tmp
    else
      Result := 0;
  end;

var
  ShouldContinue : Boolean;
  tmpSrc, tmpDest : PChar;
  blockSize, processed, diff : Integer;
begin
  if Assigned(FOnCryptBlock) then
    FOnCryptBlock( Self, DestBlock, SrcBlock, DestSize, SrcSize )
  else
    with FCryptoObject do
    begin
      ShouldContinue := False;
      Key := FCryptKey;
      tmpSrc := SrcBlock;
      tmpDest := DestBlock;
      processed := 0;
      diff := Compl(SrcSize, kMaxCryptBuffer);
      DestSize := SrcSize + diff;
      repeat
        BlockSize := Min( kMaxCryptBuffer, DestSize - processed );
        InputLength  := BlockSize;
        pInputArray  := Pointer(tmpSrc);
        pOutputArray := Pointer(tmpDest);
        EncipherData(ShouldContinue);
        ShouldContinue := True;
        Inc( processed, BlockSize );
        tmpSrc  := tmpSrc  + BlockSize;
        tmpDest := tmpDest + BlockSize;
      until processed >= DestSize;
      DestBlock[DestSize] := Char(diff);
      Inc( DestSize );
    end;
end;

procedure TArchiver.DecryptBlock( DestBlock, SrcBlock : PChar; var DestSize : Integer; SrcSize : Integer);
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

function  TArchiver.NeededBlockSize : Integer;
begin
  // We need 1% more, of the Source Block for the Dest Block.
  Result := FHeader.BlockSize + FHeader.BlockSize div 100;
end;

function TArchiver.CompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer ) : Boolean;
type
  T = array [0..1024] of Byte;
  PT = ^T;
var
  p : PT;
  tmpDestSize : uLong;
begin
  if Assigned( FOnCompressBlock ) then
    Result := FOnCompressBlock( Self, DestBlock, DestSize, SrcBlock, SrcSize, CompressionLevel )
  else
    begin
      p := PT(SrcBlock);
      tmpDestSize := DestSize; // This is needed for Delphi2, because it believes
                               // that the Integer/uLong are different type,
                               // even if uLong is a Longint, and of course
                               // an integer for Delphi 2 and later !
                               // Delphi 3 accepts it without any problem !
      Result := zCompres.compress2(pBytef(DestBlock), tmpDestSize, p^, SrcSize, CompressionLevelAsInteger) = Z_OK;
      DestSize := tmpDestSize;
    end;
end;

function  TArchiver.UncompressBlock( DestBlock : PChar; var DestSize : Integer; SrcBlock : PChar; SrcSize : Integer) : Boolean;
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

function  TArchiver.SelectDirectory(var Directory: string; Options: TMySelectDirOpts; HelpCtx: Longint):Boolean;
var
  opt : TSelectDirOpts;
begin
  opt := [];
  if ArchiverRoot.sdAllowCreate in Options then
    Include( opt, sdAllowCreate );
  if ArchiverRoot.sdPerformCreate in Options then
    Include( opt, sdPerformCreate );
  if ArchiverRoot.sdPrompt in Options then
    Include( opt, sdPrompt );
  Result := FileCtrl.SelectDirectory( Directory, Opt, HelpCtx );
end;

function  TArchiver.SelectFile( const aTitle : String; var aFileName : String ) : Boolean;
begin
   with TOpenDialog.Create(Self) do
      try
        Title := aTitle;
        FileName := aFileName;
        Result := Execute;
        if Result then
          aFileName := FileName;
      finally
        Free;
      end;
end;

function  TArchiver.CompressionLevelAsInteger : Integer;
begin
  case CompressionLevel of
    clMaximum:    Result := Z_BEST_COMPRESSION;
    clNormal:     Result := Z_DEFAULT_COMPRESSION;
    clFast:       Result := 5;
    clSuperFast:  Result := Z_BEST_SPEED;
    clNone:       Result := Z_NO_COMPRESSION;
  else
    Result := Z_DEFAULT_COMPRESSION;
  end;
end;

end.
