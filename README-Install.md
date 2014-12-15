## How to install Radiant from Dropbox (requires an invitation-to-share)

### Step 1: Install the desktop version of Dropbox on your computer

Download the Dropbox software from <https://www.dropbox.com/downloading> and install it. If you already have Dropbox installed on your computer, this step will ensure that you are running the latest version. If you do not have a Dropbox account yet, you will be asked to create one. Once you have installed Dropbox, make sure that it is running. In Windows, the icon is typically on the bottom right of the screen (i.e., the system tray). In Mac OSX, it is typically on the top right of the screen in the menu bar. If Dropbox is running make sure that you are logged in with the username and password you normally use for Dropbox. For example, if you always login to Dropbox with a gmail account make sure you are logged in with that account now as well.

### Step 2: Accept the invitation to share the Dropbox folder (at the bottom of the invitation email from Dropbox)

Accept the invitation in the email to share the Radiant folder (see the button at the bottom of the invitation e-mail). If dropbox does not ask you to share the Radiant folter send an email to radiant@rady.ucsd.edu from the email address where you would like the invitation to be sent. Wait until the folder has fully synced to your computer. You may notice that the Dropbox icon changes from a green check-mark to a different form that indicates it is syncing files. Note that the Radiant folder is read-only.

### Step 3: Download and install R (version 3.1.2) for your operating system.

* For Windows: http://vnijs.rady.ucsd.edu/site_media/R/R-3.1.2-win.exe
* For Mac (10.9 and above): http://vnijs.rady.ucsd.edu/site_media/R/R-3.1.2-mavericks.pkg
* For Mac (10.6 - 10.8): http://vnijs.rady.ucsd.edu/site_media/R/R-3.1.2-snowleopard.pkg

<!-- hosting binaries on personal server because links to 3.1.2 on CRAN
may disappear when a new version comes out

* For Windows: http://cran.cnr.berkeley.edu/bin/windows/base/R-3.1.2-win.exe
* For Mac (10.9 and above): http://cran.cnr.berkeley.edu/bin/macosx/R-3.1.2-mavericks.pkg
* For Mac (10.6 - 10.8): http://cran.cnr.berkeley.edu/bin/macosx/R-3.1.2-snowleopard.pkg
-->

### Step 4: Download and install additional software for your operating system.

For Mac:

* http://download1.rstudio.org/RStudio-0.98.1091.dmg
* http://mirror.ctan.org/systems/mac/mactex/mactex-basic.pkg

For Windows:

* http://download1.rstudio.org/RStudio-0.98.1091.exe
* http://mirrors.ctan.org/systems/win32/miktex/setup/basic-miktex-2.9.5105.exe

### Step 5: Use a modern browser

We recommend that you install Chrome or Firefox for better performance. On a Mac, Safari is also fine. It will be easiest if you make Chrome, Firefox, or Safari your default browser.

* Chrome: https://www.google.com/chrome/browser/
* Firefox: https://www.mozilla.org/en-US/firefox/new/

### Step 6: Start the app

We included tools to create _launchers_ on your Desktop to make it easy to start Radiant. For the quant class (MGT403), the tools are in the directory Dropbox/radiant/launchers/quant. For the Marketing Research class (MGT475), the tools are in the directory Dropbox/radiant/launchers/marketing.

On Windows you create a launcher for Radiant on your Desktop by double-clicking the make\_win.bat file. Find the new file on your Desktop (i.e., radiant\_quant.bat or radiant_marketing.bat). Double click the .bat file and Radiant will start. The first time you start the app a number of required packages will be installed, and this may take a few minute

For Mac, double-click the make\_mac.command file to create a launcher for Radiant on your Desktop. Find the new file on your Desktop (i.e., radiant\_quant.command or radiant_marketing.command). Double click the .command file and Radiant will start. The first time you start the app a number of required packages will be installed, and this may take a few minute

When you start Radiant a browser window will open and you will see the web application running. You should see data on diamond prices. To close the application click on `Quit` in the Navigation bar and then click the `Quit` button. The Radiant process will stop and you can close the browser.
