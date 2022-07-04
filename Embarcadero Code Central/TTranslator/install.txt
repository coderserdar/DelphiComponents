    Installing

Since TTranslator is Open Source, the installing procedure might not be
the fanciest around, but following the three steps below the translator
should install itself rather smoothly.

    * Step One: Download
      Download translator.zip and extract the files on your hard disk in
      any directory of your choice.

    * Step Two: Install the component
      Either (works at least with Delphi 5):
      Choose File|Open... and choose the package file units
      DTranslator.dpk. Then click on compile and install.

      Or (works better with Delphi 6&7):
      Choose Component|Install component and select the file
      TranslatorReg.pas from the directory you unzipped the files. Then
      choose the package for user components dclusrXX.dpk and click OK.

    * Step Three: Use the component
      You should have a new tab Polycon in your component palette, under
      which there are two new components: the TTranslator and the
      TTranslatorClient.


    Common problems

    * Delphi 3 notes
      TTranslator has been known to compile using Delphi 3 -- but we
      give no warranties what so ever. We have some conditional defines
      for Delphi 3, but there has been some problems. The older release
      v2.0 <download/translator20.zip> might compile better with Delphi3.


    * Delphi 4 notes
      TTranslator has also been known to compile using Delphi 4. We have
      some conditional defines for Delphi 4, but there has been some
      problems. The older release v2.10 <download/translator21.zip> will
      work with Delphi4.


    * Delphi 7 notes
      We are also having problems with Delphi 7. Mostly there should be
      some hints and warnings, which should cause no problems. The
      design-time interfaces has been somewhat changed in Delphi 7 and
      the TTranslator is not very thouroughly tested with it. You have
      been warned.


    * Delphi Personal users
      TTranslator uses features, like Excel COM wrappers, not present in
      the personal versions of Delphi. Define the compiler directive
      PERSONALDELPHI in your .dpk file (or in the commonly included file
      common.inc) to compile TTranslator without these.

