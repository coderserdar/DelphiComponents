
{**********************************************************}
{                                                          }
{  This unit is a sample of custom encryption algothrim.   }
{  As a sample, the algorithm is very simple .             }
{                                                          }
{**********************************************************}

unit MyEncAlgo;

interface

uses
  Classes, Windows, SysUtils, TinyDB;

type

  //TMyEncAlgo must derive from class TEncryptAlgo.
  TMyEncAlgo = class(TEncryptAlgo)
  private
    FKey: string;

    procedure XorBuffer(const Source; var Dest; DataSize: Integer); 
  public
    procedure InitKey(const Key: string); override;

    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer); override;
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer); override;
  end;

implementation

//-----------------------------------------------------------------------------
//Init password
//-----------------------------------------------------------------------------
procedure TMyEncAlgo.InitKey(const Key: string);
begin
  //Hash original key, and assign result to FKey.
  //NOTE: HashSHA is a routine in TinyDatabase, it used SHA algorithm.
  FKey := string(HashSHA(AnsiString(Key)));
end;

//-----------------------------------------------------------------------------
//Use XOR to encode & decode the buffer.
//This algorithm is very simple.
//-----------------------------------------------------------------------------
procedure TMyEncAlgo.XorBuffer(const Source; var Dest; DataSize: Integer);
var
  I, J, KeyLen: Integer;
  Ptr: PChar;
begin
  Move(Source, Dest, DataSize);
  Ptr := PChar(@Dest);
  KeyLen := Length(FKey);
  J := 1;
  for I := 0 to DataSize - 1 do
  begin
    Ptr[I] := Chr(Ord(Ptr[I]) xor Ord(FKey[J]));
    Inc(J);
    if J > KeyLen then J := 1;
  end;
end;

//-----------------------------------------------------------------------------
//Encrypt the buffer.
//THE PROCEDURE MUST BE OVERRIDED!
//Parameters:
//  Source -- Source buffer
//  Dest -- Destination buffer
//  DataSize -- The size of buffer will be encrypted
//-----------------------------------------------------------------------------
procedure TMyEncAlgo.EncodeBuffer(const Source; var Dest; DataSize: Integer);
begin
  XorBuffer(Source, Dest, DataSize);
end;

//-----------------------------------------------------------------------------
//Decrypt the buffer.
//THE PROCEDURE MUST BE OVERRIDED!
//Parameters:
//  Source -- Source buffer
//  Dest -- Destination buffer
//  DataSize -- The size of buffer will be decrypted
//-----------------------------------------------------------------------------
procedure TMyEncAlgo.DecodeBuffer(const Source; var Dest; DataSize: Integer);
begin
  XorBuffer(Source, Dest, DataSize);
end;

//-----------------------------------------------------------------------------
//At last, register the encryption algorithm class!
//
//procedure RegisterEncryptClass(AClass: TEncryptAlgoClass; AlgoName: string);
//Parameters:
//  AClass -- The encryption algorithm class
//  AlgoName -- Give a nickname for this encryption algorithm
//-----------------------------------------------------------------------------
initialization
  RegisterEncryptClass(TMyEncAlgo, 'MyEncAlgo');

end.
