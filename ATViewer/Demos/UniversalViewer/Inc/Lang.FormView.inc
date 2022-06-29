  //Messages
  MsgViewerCaption:= 'Universal Viewer';
  MsgViewerShowCfm:= msgString(108);
  MsgViewerErrCannotFindFile:= MsgString(101);
  MsgViewerErrCannotFindFolder:= MsgString(107);
  MsgViewerErrCannotOpenFile:= MsgString(102);
  MsgViewerErrCannotLoadFile:= MsgString(104);
  MsgViewerErrCannotReadFile:= MsgString(105);
  MsgViewerErrCannotReadPos:= MsgString(109);
  MsgViewerErrImage:= MsgString(115);
  MsgViewerErrMedia:= MsgString(116);
  MsgViewerErrInitControl:= MsgString(106);
  MsgViewerErrCannotFindText:= MsgString(120);
  MsgViewerErrCannotCopyData:= MsgString(121);
  MsgViewerErrInvalidHex:= MsgString(122);
  MsgViewerErrInvalidFilelist:= MsgString(110);
  MsgViewerSearchProgress:= MsgString(123);
  MsgViewerPageHint:= MsgString(129);

  MsgViewerWlxException:= MsgString(200);
  MsgViewerWlxParentNotSpecified:= MsgString(202);
  MsgViewerPluginUnsup:= MsgString(203);
  MsgViewerPluginSup:= MsgString(204);
  MsgViewerPluginsNameNotFound:= MsgString(214);
  MsgViewerPluginsInstalledZip:= MsgString(209);
  MsgViewerPluginsNone:= MsgString(211);

  MsgViewerJumpToFirst:= MsgString(250);
  MsgViewerJumpToLast:= MsgString(251);
  MsgViewerJumpNotFound:= MsgString(252);
  MsgViewerJumpDirEmpty:= MsgString(253);
  MsgViewerJumpSingleFile:= MsgString(254);
  MsgViewerDirEmpty:= MsgString(257);

  MsgViewerDeleteCaption:= MsgString(260);
  MsgViewerDeleteWarningRecycle:= MsgString(261);
  MsgViewerDeleteWarningPermanent:= MsgString(262);
  MsgViewerDeleteError:= MsgString(263);
  MsgViewerCopyMoveError:= MsgString(264);

  //Captions
  Viewer.Caption:= MsgCaption(180);
  Viewer.TextPopupCaption[vpCmdCopy] := MsgCaption(122);
  Viewer.TextPopupCaption[vpCmdCopyHex] := MsgCaption(127);
  Viewer.TextPopupCaption[vpCmdCopyLink] := MsgCaption(140);
  Viewer.TextPopupCaption[vpCmdSelectLine] := MsgCaption(128);
  Viewer.TextPopupCaption[vpCmdSelectAll] := MsgCaption(123);
  Viewer.TextPopupCaption[vpCmdEncMenu] := MsgCaption(169);

  //Captions
  mnuFile.Caption:= MsgCaption(100);
  mnuFileOpen.Caption:= MsgCaption(101);
  mnuFileOpenRecent.Caption:= MsgCaption(106);
  mnuRecentClear.Caption:= MsgCaption(107);
  mnuBarRecentClear.Caption:= mnuRecentClear.Caption;
  mnuFileReload.Caption:= MsgCaption(102);
  mnuFileSaveAs.Caption:= MsgCaption(103);
  mnuFileClose.Caption:= MsgCaption(104);
  mnuFilePrint.Caption:= MsgCaption(110);
  mnuFilePrintPreview.Caption:= MsgCaption(111);
  mnuFilePrintSetup.Caption:= MsgCaption(112);
  mnuFilePrev.Caption:= MsgCaption(115);
  mnuFileNext.Caption:= MsgCaption(116);
  mnuFileRename.Caption:= MsgCaption(651);
  mnuFileCopy.Caption:= MsgCaption(653);
  mnuFileMove.Caption:= MsgCaption(654);
  mnuFileDelete.Caption:= MsgCaption(117);
  mnuFileCopyFN.Caption:= MsgCaption(109);
  mnuFileEmail.Caption:= MsgCaption(108);
  mnuFileProp.Caption:= MsgCaption(118);
  mnuFileExit.Caption:= MsgCaption(119);

  mnuEdit.Caption:= MsgCaption(121);
  mnuEditCopy.Caption:= MsgCaption(122);
  mnuEditCopyHex.Caption:= MsgCaption(127);
  mnuEditCopyToFile.Caption:= MsgCaption(129);
  mnuEditPaste.Caption:= MsgCaption(120);
  mnuEditSelectAll.Caption:= MsgCaption(123);
  mnuEditFind.Caption:= MsgCaption(124);
  mnuEditFindNext.Caption:= MsgCaption(125);
  mnuEditFindPrev.Caption:= MsgCaption(167);
  mnuEditGoto.Caption:= MsgCaption(126);

  mnuView.Caption:= MsgCaption(150);
  mnuOptions.Caption:= MsgCaption(130);

  mnuViewMode1.Caption:= MsgCaption(131);
  mnuViewMode2.Caption:= MsgCaption(132);
  mnuViewMode3.Caption:= MsgCaption(133);
  mnuViewMode4.Caption:= MsgCaption(134);
  mnuViewMode5.Caption:= MsgCaption(135);
  mnuViewMode6.Caption:= MsgCaption(136);
  mnuViewMode7.Caption:= MsgCaption(137);

  mnuViewModeMenu.Caption:= mnuView.Caption;
  mnuModes1.Caption:= mnuViewMode1.Caption;
  mnuModes2.Caption:= mnuViewMode2.Caption;
  mnuModes3.Caption:= mnuViewMode3.Caption;
  mnuModes4.Caption:= mnuViewMode4.Caption;
  mnuModes5.Caption:= mnuViewMode5.Caption;
  mnuModes6.Caption:= mnuViewMode6.Caption;
  mnuModes7.Caption:= mnuViewMode7.Caption;
  mnuModes8.Caption:= mnuViewMode8.Caption;

  mnuViewTextEncSubmenu.Caption:= MsgCaption(168);
  mnuViewTextEncPrev.Caption:= MsgCaption(170);
  mnuViewTextEncNext.Caption:= MsgCaption(171);
  mnuViewTextEncMenu.Caption:= MsgCaption(169);
  mnuViewTextWrap.Caption:= MsgCaption(142);
  mnuViewTextNonPrint.Caption:= MsgCaption(172);
  mnuViewTextTail.Caption:= MsgCaption(327);

  mnuViewWebGoBack.Caption:= MsgCaption(176);
  mnuViewWebGoForward.Caption:= MsgCaption(177);
  mnuViewWebOffline.Caption:= MsgCaption(144);

  mnuViewTextMenu.Caption:= MsgCaption(178);
  mnuViewImageMenu.Caption:= MsgCaption(151);
  mnuViewWebMenu.Caption:= MsgCaption(179);

  mnuViewImageFit.Caption:= MsgCaption(143);
  mnuViewImageFitOnlyBig.Caption:= MsgCaption(161);
  mnuViewImageCenter.Caption:= MsgCaption(163);
  mnuViewImageFitWindow.Caption:= MsgCaption(165);
  mnuViewImageShowEXIF.Caption:= MsgCaption(881);
  mnuViewImageShowLabel.Caption:= MsgCaption(162);
  mnuViewImageRotateRight.Caption:= MsgCaption(157);
  mnuViewImageRotateLeft.Caption:= MsgCaption(158);
  mnuViewImageGrayscale.Caption:= MsgCaption(159);
  mnuViewImageNegative.Caption:= MsgCaption(160);
  mnuViewImageFlipVert.Caption:= MsgCaption(224);
  mnuViewImageFlipHorz.Caption:= MsgCaption(225);

  mnuViewMediaEffect.Caption:= MsgCaption(219);
  mnuViewMediaPlayback.Caption:= MsgCaption(220);
  mnuViewMediaPlayPause.Caption:= MsgCaption(166);
  mnuViewMediaLoop.Caption:= MsgCaption(803);
  mnuViewMediaVolumeUp.Caption:= MsgCaption(221);
  mnuViewMediaVolumeDown.Caption:= MsgCaption(222);
  mnuViewMediaVolumeMute.Caption:= MsgCaption(223);

  mnuViewAlwaysOnTop.Caption:= MsgCaption(145);
  mnuViewFullScreen.Caption:= MsgCaption(146);

  mnuViewZoomMenu.Caption:= MsgCaption(164);
  mnuViewZoomIn.Caption:= MsgCaption(152);
  mnuViewZoomOut.Caption:= MsgCaption(153);
  mnuViewZoomOriginal.Caption:= MsgCaption(154);

  mnuViewInterfaceMenu.Caption:= MsgCaption(304);
  mnuViewShowMenu.Caption:= MsgCaption(344);
  mnuViewShowToolbar.Caption:= MsgCaption(337);
  mnuViewShowStatusbar.Caption:= MsgCaption(347);
  mnuViewShowNav.Caption:= MsgCaption(348);

  mnuOptionsConfigure.Caption:= MsgCaption(147);
  mnuOptionsPlugins.Caption:= MsgCaption(148);
  mnuOptionsToolbar.Caption:= MsgCaption(149);
  mnuOptionsUserTools.Caption:= MsgCaption(182);
  mnuOptionsAdvanced.Caption:= MsgCaption(183);
  mnuOptionsEditIni.Caption:= MsgCaption(184);
  mnuOptionsEditIniHistory.Caption:= MsgCaption(185);
  mnuOptionsSavePos.Caption:= MsgCaption(186);

  mnuTools.Caption:= MsgCaption(181);
  mnuToolbarCustomize.Caption:= mnuOptionsToolbar.Caption;

  mnuHelp.Caption:= MsgCaption(155);
  mnuHelpWebMenu.Caption:= MsgCaption(230);
  mnuHelpWebHomepage.Caption:= MsgCaption(231);
  mnuHelpWebEmail.Caption:= MsgCaption(232);
  mnuHelpWebPlugins.Caption:= MsgCaption(233);
  mnuHelpContents.Caption:= MsgCaption(234);
  mnuHelpAbout.Caption:= MsgCaption(156);

