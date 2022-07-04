{ Uncomment next line in order to turn off version checking of OpenSSL         }
{ library, not recommended.                                                    }
{.$DEFINE NO_OSSL_VERSION_CHECK}

{ Uncomment next line in order to support OpenSSL v0.9.7g thru v0.9.8e         }
{ using our own 'hacks' to emulate some C-macros not exported by OpenSSL       }
{ in previous versions. Leave it commented if you want to support OpenSSL      }
{ v0.9.8e only without using our 'hacks' but newly exported functions, default }
{.$DEFINE BEFORE_OSSL_098E}

{ Uncommented next line will create files containing debug information if the  }
{ OpenSSL libraries failed to load.                                            }
{ File names: FailedIcsSSLEAY.txt and FailedIcsLIBEAY.txt                      }
{.$DEFINE LOADSSL_ERROR_FILE}

{ Comment next line to include experimental Engine support.                    }
{ http://openssl.org/docs/crypto/engine.html                                   }
{ Unless someone tested it successfully with hardware accelerators the default }
{ should be "OPENSSL_NO_ENGINE"                                                }
{$DEFINE OPENSSL_NO_ENGINE}

{ Do not modify below this line                                                }
{ -----------------------------------------------------------------------------}
 
{$IFDEF BEFORE_OSSL_098E}
  {$IFNDEF OPENSSL_NO_ENGINE}
    {$DEFINE OPENSSL_NO_ENGINE}  // ENGINE code tested since V0.98E
  {$ENDIF}
{$ENDIF}
