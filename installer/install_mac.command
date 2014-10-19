#!/bin/bash

# File created based on examples from http://getmacapps.com/
cd $HOME/Desktop

# Installing Dropbox
curl -L -o Dropbox.dmg "https://www.dropbox.com/download?plat=mac"
hdiutil mount -nobrowse Dropbox.dmg
cp -R "/Volumes/Dropbox Installer/Dropbox.app" /Applications
hdiutil unmount "/Volumes/Dropbox Installer"
rm Dropbox.dmg

# Installing R
if [ sw_vers -productVersion -lt 10.9 ]; then
  curl -L -O "http://cran.cnr.berkeley.edu/bin/macosx/R-3.1.1-snowleopard.pkg"
  open R-3.1.1-snowleopard.pkg
else
  curl -L -O "http://cran.stat.ucla.edu/bin/macosx/R-3.1.1-mavericks.pkg"
  open R-3.1.1-mavericks.pkg
fi

# Installing MacTex basic
curl -L -O http://mirror.ctan.org/systems/mac/mactex/mactex-basic.pkg
# open mactex-basic.pkg

# Installing Rstudio
curl -O http://download1.rstudio.org/RStudio-0.98.1074.dmg
hdiutil mount -nobrowse RStudio-0.98.1074.dmg
cp -R "/Volumes/RStudio-0.98.1074/RStudio.app" /Applications
hdiutil unmount "/Volumes/RStudio-0.98.1074"
rm RStudio-0.98.1074.dmg

# Installing Chrome
curl -L -O "https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg"
hdiutil mount -nobrowse googlechrome.dmg
cp -R "/Volumes/Google Chrome/Google Chrome.app" /Applications
hdiutil unmount "/Volumes/Google Chrome"
rm googlechrome.dmg
