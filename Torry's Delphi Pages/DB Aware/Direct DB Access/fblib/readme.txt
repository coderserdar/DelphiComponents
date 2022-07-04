Compilers supported
  
  Delphi 4 standard ,pro ,ent
  Delphi 5 standard ,pro ,ent
  Delphi 6 personal ,pro ,ent
  Delphi 7 personal ,pro ,ent
  
  Kylix 2  open, pro, ent
  Kylix 3  open, pro, ent


  freepascal 2.0 or above 
  lazarus ide 0.9.10 or above

version 0.80 is thread safe (experimental feature) 
components  thread safe are TFBLDatabase,TFBLTransaction,TFBLDsql 
for default fblib is NOT thread safe for
enable this feature activate compiler directive
FBL_THREADSAFE in fbl.inc.


for generate documetation follow these steps:

1) download pasdoc from http://pasdoc.sourceforge.net
2) compile pasdoc ,I used freepascal 2.0
3) go to fblib source directory
4) in win run  makehelp.bat, in linux run makehelp.sh 
5) this script will create a directory name FblibHelp with docs, open index.html with your favorite browser. 

   
    