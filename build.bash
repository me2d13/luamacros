LAZ_HOME=/c/Apps/lazarus/lazarus184x64
LMC_HOME=/c/Work/lmc/2/luamacros
DIST_HOME=/c/Work/lmc/dist
$LAZ_HOME/lazbuild.exe --compiler="C:\Apps\FPC\3.0.0\bin\i386-Win32\fpc.exe" $LMC_HOME/src/LuaMacros.lpi
$LAZ_HOME/lazbuild.exe --compiler="C:\Apps\Lazarus\lazarus184x64\fpc\3.0.4\bin\x86_64-win64\fpc.exe" $LMC_HOME/src/XplPlugin.lpi
cp -v $LMC_HOME/out/LuaMacros.exe $DIST_HOME
cp -v $LMC_HOME/out/win.xpl $DIST_HOME/xpl_lmc_plugin/64
cd /c/work/lmc/dist
zip -r ../luamacros.zip *