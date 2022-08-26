@Echo off
Del /s %1*.dcu
Del /s %1*.dcuil
Del /s %1*.~*
Del /s %1*.identcache
Del /s %1*.local
Del /s %1*.pdb
Del /s %1*.map
Del /s %1*.elf
Del /s %1*.dsk
Del /s %1*.vlb
Del /s %1*.Drc
Del /s %1*.tmp
Del /s %1*.rsm
Del /s %1*.tvsconfig
Del /s %1*.ddp
Del /s %1*.otares
Del /s %1*.pec2bac
RMDIR "__history" /s /q
RMDIR "iOSSimulator" /s /q
RMDIR "iOSDevice" /s /q

