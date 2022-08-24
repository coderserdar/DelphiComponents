  Starting with C++ Builder 6 Borland does not allow static linking to the designide package.
  As result now the components and the design packages must be separated.

   Creating component package:

1. Create a C++ Builder package.
2. Add the BMShapedButton.cpp, BMWave.cpp and BMMediaDialogs.cpp files.
3. Compile and install the package.

   Creating design editors package:

4. Create a second C++ Builder package.
5. Add the BMWaveEditors.cpp .
6. Add the designide.bpi to the Requires clause.
7. Compile and install the second package.

Warning! If you are using some other Boian Mitov’s components, you may have to install them into a common package, or package hierarchy. 


