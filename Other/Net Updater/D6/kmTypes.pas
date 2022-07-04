unit kmTypes;

interface

type
  TnuVersionControl = (byDate, byVersion);
  TnuShowMessages = set of (mAskUpgrade, mPromptCancel, mConnLost, mHostUnreachable, mNoFile, mNoUpdateAvailable, mPasswordRequest);
  TnuAccessType = (atPreconfig, atDirect, atUseProxy);
  TnuProtocol = (pHTTP, pHTTPS ,pFTP, pFILE);
  TnuRunMode = (rmNormal, rmSilent, rmHide);
  TnuSchedule = (schStart, schDay, schWeek, schNone);
  TnuDay = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

  TnuCacheOption   = (coAlwaysReload, coReloadIfNoExpireInformation,
                      coReloadUpdatedObjects, coPragmaNoCache,
                      coNoCacheWrite, coCreateTempFilesIfCantCache,
                      coUseCacheIfNetFail);
  TnuCacheOptions = set of TnuCacheOption;

  TnuInternetOption = (ioIgnoreCertificateInvalid, ioIgnoreCertificateDateInvalid,
                       ioIgnoreRedirectToHTTP, ioIgnoreRedirectToHTTPS,
                       ioKeepConnection, ioNoAuthentication,
                       ioNoAutoRedirect, ioNoCookies, ioNoCookieDialog, ioSecure);
  TnuInternetOptions = set of TnuInternetOption;

  TThreadPriority = (tpLow, tpNormal, tpHigh);

  TCallBack = procedure(Position,Size,sTime:LongInt); { export; }

  
implementation

end.
