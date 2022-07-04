unit fonctions_array;

{$I ..\extends.inc}
{$I ..\Compilers.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils; 

function fstr_getStringInArray ( const aAr_StringArray : array of Widestring ; const ai_NumeroOfString : Integer ) : WideString ;
function fb_getBooleanInArray ( const aAr_BooleanArray : array of Boolean ; const ai_NumeroOfBoolean : Integer ) : Boolean ;
function fvar_getVariantInArray ( const aAr_VariantArray : array of Variant ; const ai_NumeroOfVariant : Integer ) : Variant ;
function fi_getIntegerInArray( const aT_IntegerArray : array of Integer ; const ai_NumeroOfInteger : Integer ): Integer;


implementation

uses Variants;

// Gestion des array of string
// aEv_EventString array of String
// ai_NumeroOfString Numéro du String
function fstr_getStringInArray ( const aAr_StringArray : array of Widestring ; const ai_NumeroOfString : Integer ) : WideString ;
begin
  if           ( ai_NumeroOfString <= high ( aAr_StringArray ))
  and          ( ai_NumeroOfString >= low  ( aAr_StringArray ))
   Then
    Result := aAr_StringArray [ ai_NumeroOfString ];
end;
// Gestion des array of Boolean
// aEv_EventString array of Boolean
// ai_NumeroOfString Numéro du Boolean
function fb_getBooleanInArray ( const aAr_BooleanArray : array of Boolean ; const ai_NumeroOfBoolean : Integer ) : Boolean ;
begin
  Result := False ;
  if           ( ai_NumeroOfBoolean <= high ( aAr_BooleanArray ))
  and          ( ai_NumeroOfBoolean >= low  ( aAr_BooleanArray ))
   Then
    Result := aAr_BooleanArray [ ai_NumeroOfBoolean ];
end;
// Recherche d'un entier dans un array of Variant
// aEv_EventString array of Variant
// ai_NumeroOfString Numéro du Variant
function fvar_getVariantInArray ( const aAr_VariantArray : array of Variant ; const ai_NumeroOfVariant : Integer ) : Variant ;
begin
  Result := Null ;
  if           ( ai_NumeroOfVariant <= high ( aAr_VariantArray ))
  and          ( ai_NumeroOfVariant >= low  ( aAr_VariantArray ))
   Then
    Result := aAr_VariantArray [ ai_NumeroOfVariant ];
end;

// Recherche d'un entier dans un array of Variant
// aEv_EventString array of Variant
// ai_NumeroOfString Numéro du Variant
function fi_getIntegerInArray( const aT_IntegerArray : array of Integer ; const ai_NumeroOfInteger : Integer ): Integer;
begin
  Result := -1 ;
  if           ( ai_NumeroOfInteger <= high ( aT_IntegerArray ))
  and          ( ai_NumeroOfInteger >= low  ( aT_IntegerArray ))
   Then
    Result := aT_IntegerArray [ ai_NumeroOfInteger ];

end;

end.

