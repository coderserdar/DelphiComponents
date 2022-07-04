unit _CHTypes;

interface

type
  { Component }
  TCompType = (ctLabel, ctButton, ctRadiobutton, ctCheckbox);
  TFillStyle = (fsNormal, fsBitmap, fsGradient, fsEffect);
  TCaptionEffect = (ceGradient, ceBitmap);

  { Controls }
  TFocusStyle = (csDot, csSolid);
  TFocusMode = (cmAuto, cmCustom);


  { CHApplication }
  TRestart = (rsTimer, rsFunction);
  TStartUpKey = (ruRUN, ruRUNSERVICES, ruRUNONCE);
  TRegKey = (rkLocalMachine, rkCurrentUser);
  TRegEvent = (reAdd, reDelete);


  { CHMouse }
  TMousePosition = (mpCenter, mpCenterBottom, mpCenterTop, mpCenterLeft, mpCenterRight,
    mpTopLeft, mpTopRight, mpBottomLeft, mpBottomRight);
  TMouseAreaMode = (maControl, maCustom);
  TMousePosMode = (mpControl, mpCustom);
  TMouseSpeed = (msSystem, msSlow, msNormal, msFast);


  { CHForm }
  TFormMoveMode = (foOnlyCaption, foAll, foNone);
  TOnTopMode = (otNormalOnTop, otForceOnTop);
  TFormLook = (flNormal, flElliptic, flRoundRect, flTriangle, flRing);


  { CHLed }
  TLedMaskMode = (lmNone, lmClock, lmCustom, lmThousandSep, lmCommaOne, lmCommaTwo, lmCommaThree);
  TLedSeperator = (lsPoint, lsColon);
  TLedAlignment = (laLeft, laCenter, laRight);
  TLedPlaceholder = 0..9;


  { CHLabel, CHButton }
  TAngle = 0..360;
  TDirection = (drNone, drUp, drDown, drLeft, drRight, drUpLeft,
    drUpRight, drDownLeft, drDownRight);
  TTextStyle = (tsNone, tsCustom, tsRaised, tsRaisedColor, tsRaisedShadow,
    tsRecessed, tsRecessedColor, tsRecessedShadow, tsShadow);
  TTextAlign = (tgCenter, tgAuto, tgCustom, tgCenterBottom, tgCenterTop, tgCenterLeft, tgCenterRight,
    tgTopLeft, tgTopRight, tgBottomLeft, tgBottomRight);
  TExecuteType = (etNone, etWWW, etEMail, etNews, etEXE);
  TBorderStyle = (bsFlat, bsNormal, bsExtended);
  TButtonMode = (bmButton, bmSpeedbutton);


  { CHGradient }
  TGradientStyle = (gsHorizontal_L, gsHorizontal_R, gsVertical_T, gsVertical_B,
    gsArrow_L, gsArrow_R, gsArrow_B, gsArrow_T, gsDiagonal_L, gsDiagonal_R, gsRotation);
  TRGBArray = array[0..2] of Byte;
  TGradientStep = -100..100;


  { CHImage }
  TGlyphAlignment = (gaLeft, gaTop, gaRight, gaBottom, gaCenter);
  TGlyphAlignMode = (gmCaption, gmControl, gmCustom);
  TTransMode = (tmAuto, tmCustom);
  TBitmapMode = (bmNormal, bmStretch, bmTile, bmCenter);
  TFrameStyle = (fsSolid, fsDot);
  TImageEffect = (ieDarkness, ieLightness, ieContrast, ieBlur, ieSaturation, iePixel,
    ieSolorize, iePosterize, ieFlashLight);


  { CHPanel }
  TMoveSpeed = 0..10000;
  TMoveMode = (mmAll, mmControl, mmNone);
  TCollapseDirection = (cdUp, cdDown, cdLeft, cdRight);


  { CHEdit }
  TTextTyp = (ttNormal, ttInteger, ttDouble);
  TSelectMode = (smAll, smFirst, smLast, smCustom);
  TCaseMode = (cmNormal, cmUppercase, cmLowerCase, cmFirstUpper);
  TKeyPress = (vkEnter, vkESC, vkStrg, vkAlt, vkShift);
  TEditBeep = (ebStandard, ebNoEnterBeep, ebAlwaysBeep);


  { CHCheckbox, CHRadiobutton }
  TBoxStyle = (bxNormal, bxFlat);
  TCheckBoxFill = (cfSolid, cfCross, cfHook);
  TRadioButtonFill = (rfSolid, rfDot, rfCross, rfHLine);
  TBoxFillColor = (bcBlack, bcRed, bcGreen, bcBlue, bcYellow, bcLime, bcGray, bcNone);


  { CHThread }
  TBreakMode = (thSuspend, thTerminate);


  { CHCrypt }
  TCryptKey = 1..100;
  TCryptBase = 1..10;
  TCryptMode = (cmStandard, cmExtended, cmIniStandard, cmIniExtended);
  TCryptChar = (ccAll, ccUpper, ccLower);

  { CHPerformance }
  TPerformanceResult = (prMillisecond, prSecond);

  { CHMoveContainer }
  TMoveContDirection = (mdUp, mdDown, mdLeft, mdRight, mdUpLeft,
    mdUpRight, mdDownLeft, mdDownRight);

  { CHTrayIcon }
  TTrayAction = (paLeftDown, paRightDown, paLeftUp, paRightUp,
    paLeftDblClick, paRightDblClick, paNone);
  TTrayShow = (tyAllways, tyNever, tyOnRestore, tyOnMinimized);
  TTrayMode = (trmIcon, trmText);
  TTrayTextAlign = (ttgCenter, ttgCustom, ttgCenterBottom, ttgCenterTop,
    ttgCenterLeft, ttgCenterRight);

  { CHAdvancedTimer }
  TTimerResolution = (trSecond, trMillisecond, trMicrosecond);
  TTimerMode = (timHighSolution, timCompatible);

  { CHShape }
  TShapeType = (stRectangle, stRectangleRound, stEllipse, stCircle, stLine,
    stTriangle, stArrowSmall, stArrowLarge, stTrapezoid, stHexagon, stOctagon);
  TShapeDirection = (sdLeft, sdRight, sdUp, sdDown);

  { CHLogfile }
  TLogMode = (logDirectWrite, logMemoryWrite);


implementation

end.
