  Caption:= MsgCaption(300);
  tabIntf.Caption:= MsgCaption(304);
  tabFile.Caption:= MsgCaption(305);
  tabText.Caption:= MsgCaption(302);
  tabMedia.Caption:= MsgCaption(303);
  tabShortcuts.Caption:= MsgCaption(307);
  tabHistory.Caption:= MsgCaption(286);
  tabMisc.Caption:= MsgCaption(280);

  boxExt.Caption:= ' ' + MsgCaption(301) + ' ';
  boxTextFont.Caption:= ' ' + MsgCaption(308) + ' ';
  boxText.Caption:= ' ' + MsgCaption(306) + ' ';
  boxTextReload.Caption:= ' ' + MsgCaption(309) + ' ';
  boxTextSearch.Caption:= ' ' + MsgCaption(420) + ' ';
  boxImage.Caption:= ' ' + MsgStrip(MsgCaption(311)) + ' ';
  boxMedia.Caption:= ' ' + MsgStrip(MsgCaption(312)) + ' ';
  boxInternet.Caption:= ' ' + MsgStrip(MsgCaption(179)) + ' ';
  boxIntf.Caption:= ' ' + MsgCaption(304) + ' ';
  boxMisc.Caption:= ' ' + MsgCaption(280) + ' ';
  boxHistory.Caption:= ' ' + MsgCaption(286) + ' ';
  boxPrint.Caption := ' ' + MsgCaption(287) + ' ';

  btnTextOptions.Caption:= MsgCaption(380) + '...';
  btnImageOptions.Caption:= MsgCaption(740) + '...';
  btnGutterOptions.Caption:= MsgCaption(269);

  labText.Caption:= MsgCaption(310);
  labImages.Caption:= MsgCaption(311);
  labMedia.Caption:= MsgCaption(312);
  labInternet.Caption:= MsgCaption(313);
  labRTF.Caption:= MsgCaption(314);

  labTextFont1.Caption:= MsgCaption(320);
  labTextColors.Caption:= MsgCaption(321);
  chkTextWidthFit.Caption:= MsgCaption(322);
  chkTextOemSpecial.Caption:= MsgCaption(317);

  chkTextWrap.Caption:= MsgCaption(142);
  chkTextURLs.Caption:= MsgCaption(141);
  chkTextNonPrint.Caption:= MsgCaption(172);
  labTextFixedWidth.Caption:= MsgCaption(323);
  labTextLength.Caption:= MsgCaption(318);
  labTabSize.Caption:= MsgCaption(319);
  labSearchIndent.Caption:= MsgCaption(324);
  chkSearchSel.Caption:= MsgCaption(325);
  chkSearchNoMsg.Caption:= MsgCaption(275);
  labFontFooter.Caption:= MsgCaption(290);

  btnTextColor.Hint:= MsgCaption(302);
  btnTextColorHexBack.Hint:= MsgCaption(316);
  btnTextColorHex1.Hint:= MsgCaption(315) + ' 1';
  btnTextColorHex2.Hint:= MsgCaption(315) + ' 2';
  btnTextColorGutter.Hint:= MsgStrip(MsgCaption(274));

  labMediaMode.Caption:= MsgCaption(288);
  labMediaPlayCount.Caption:= MsgCaption(282);
  labColorImage.Caption:= labTextColors.Caption;
  chkMediaStart.Caption:= MsgCaption(334);
  chkMediaLoop.Caption:= MsgCaption(803);

  chkImageFit.Caption:= MsgCaption(143);
  chkImageFitBig.Caption:= MsgCaption(161);
  chkImageCenter.Caption:= MsgCaption(163);
  chkImageFitWindow.Caption:= MsgCaption(165);
  chkImageLabel.Caption:= MsgCaption(162);
  chkImageResample.Caption:= MsgCaption(331);
  chkImageTransp.Caption:= MsgCaption(333);
  btnMediaColor.Hint:= MsgStrip(MsgCaption(332));
  btnMediaColorLabel.Hint:= MsgCaption(330) + ' 1';
  btnMediaColorLabelErr.Hint:= MsgCaption(330) + ' 2';

  chkSaveRecents.Caption:= MsgCaption(345);
  chkSaveSearch.Caption:= MsgCaption(289);
  chkSavePosition.Caption:= MsgCaption(284);
  chkSaveFolder.Caption:= MsgCaption(285);

  chkResolveLinks.Caption:= MsgCaption(281);
  chkShowHidden.Caption:= MsgCaption(283);

  labLang.Caption:= MsgCaption(335);
  labIcon.Caption:= MsgCaption(800);
  chkShell.Caption:= MsgCaption(336);
  chkMenu.Caption:= MsgCaption(344);
  chkMenuIcons.Caption:= MsgCaption(343);
  chkToolbar.Caption:= MsgCaption(337);
  chkBorder.Caption:= MsgCaption(338);
  chkSingleInst.Caption:= MsgCaption(346);
  chkStatusBar.Caption:= MsgCaption(347);
  chkNav.Caption:= MsgCaption(348);
  chkshowCfm.Caption:= MsgCaption(349);

  chkTextAutoCopy.Caption:= MsgCaption(329);
  chkTextReload.Caption:= MsgCaption(326);
  chkTextReloadBeep.Caption:= MsgCaption(328);
  chkTextReloadTail.Caption:= MsgCaption(327);

  labShortcut.Caption:= MsgCaption(342);
  ListKeys.Columns[0].Caption:= MsgCaption(340);
  ListKeys.Columns[1].Caption:= MsgCaption(341);

  MsgViewerIconDef:= MsgCaption(801);
  MsgViewerIconSave:= MsgCaption(802);

  //Buttons
  btnOk.Caption:= MsgCaption(350);
  btnCancel.Caption:= MsgCaption(351);
  btnKeyOk.Caption:= btnOk.Caption;

  btnClearRecent.Caption:= MsgCaption(291);
  btnClearSearch.Caption:= MsgCaption(292);

  //Options
  labFileSort.Caption:= MsgCaption(805);
  N:= edFileSort.ItemIndex;
  edFileSort.Items[0]:= MsgCaption(807);
  edFileSort.Items[1]:= MsgCaption(807) + MsgCaption(806);
  edFileSort.Items[2]:= MsgCaption(808);
  edFileSort.Items[3]:= MsgCaption(808) + MsgCaption(806);
  edFileSort.ItemIndex:= N;

  labViewerTitle.Caption:= MsgCaption(264);
  N:= edViewerTitle.ItemIndex;
  edViewerTitle.Items[0]:= MsgCaption(265);
  edViewerTitle.Items[1]:= MsgCaption(266);
  edViewerTitle.ItemIndex:= N;

  labViewerMode.Caption:= MsgCaption(267);
  N:= edViewerMode.ItemIndex;
  edViewerMode.Items[0]:= MsgCaption(268);
  edViewerMode.Items[1]:= MsgStrip(MsgCaption(131));
  edViewerMode.Items[2]:= MsgStrip(MsgCaption(132));
  edViewerMode.Items[3]:= MsgStrip(MsgCaption(133));
  edViewerMode.Items[4]:= MsgStrip(MsgCaption(134));
  edViewerMode.Items[5]:= MsgStrip(MsgCaption(135));
  edViewerMode.Items[6]:= MsgStrip(MsgCaption(136));
  edViewerMode.Items[7]:= MsgStrip(MsgCaption(137));
  edViewerMode.ItemIndex:= N;

  chkWebAcceptAll.Caption:= MsgCaption(277);
  chkWebOffline.Caption:= MsgCaption(144);
  