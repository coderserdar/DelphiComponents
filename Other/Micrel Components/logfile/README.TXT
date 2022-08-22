TLogFile Component
------------------

** IMPORTANT ******************************************************
* This VCL is freeware only if you follow the terms stated in the *
* end of this document!                                           *
*******************************************************************

* TLogFile *
------------

This is a non-visible component.  It has one property which is the
LogFile and it has only one method which is LogToFile.

----
LogFile: String;
procedure LogToFile(outstr: String);


- DISCLAIMER -
Song Weng Sam makes no representations or warranty that the software
or documentation are "error free" or meet any user's particular standards,
requirements, or needs. Therefore, Song Weng Sam can not be held liable for
any error or claims that arises directly or indirectly from the use of this
software.

-----------
* HISTORY *
-----------

Version   Comments
-=-=-=-   -=-=-=-=
1.00      Initial Release that supports 16 and 32 bits
1.01 JB   added OnLog event (can be encoded the text)
          added property LogName as ident od user or machine.
          added new format with milisecond. 

--------------------
* REPORTING ERRORS *
--------------------

If you found any error while using this component, kindly email to me at:

   songws@pacific.net.sg


