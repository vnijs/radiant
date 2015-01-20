---
title: Data > Manage
---

***

> Load data from disk, Save data to disk, Remove data from memory, or Save/Load the full state of the app

### Load data

The best way to get data in and out of Radiant (and R) is to use the R-data format (rda). These are binary files that can be stored compactly and that can be read into R quickly. Choose the rda radio button and click `Choose Files` to locate the file you want to load from your computer.

You can get data from Excel into Radiant in two ways. First, you can save data from Excel to a csv format and then, in Radiant, choose the .csv radio button. Most likely you will have a header row in the csv file for variable names. If the data are not comma separated you can choose semicolon or tab separated. Then click 'Choose file' and locate the csv file you created in Excel on your computer.

Alternatively, you can select and copy the data from Excel using CTRL-C (or CMD-C on mac), go to Radiant, choose the clipboard radio-button, and then click the `Paste data` button. This is a short-cut that can be convenient for smaller datasets that are cleanly formatted. If the data is not transferred cleanly to Radiant try saving the data to a csv format and loading it into Radiant as described above.

To access all data files bundled with the Radiant app choose the examples radio button and click `Load examples`. These files are used to illustrate of the various analysis tools built into Radiant. For example, the catalog sales data is used to demonstrate regression (i.e., Regression > Linear (OLS)).

### Save data

As mentioned above, the best way to get data in and out of Radiant (and R) is to use the R-data format (rda). Choose the rda radio button and click the Save data button to store the data on your computer.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of Radiant you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add such a description for your own data check the 'Add/edit data description' check-box. A window will open below that data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with Radiant should serve as a good starting point. When you save the data as an rda file the description you created (or edited) will automatically be added to the file.

Getting data from Radiant into Excel can also be achieved in two ways. First, you can save data in csv format and load it into Excel (i.e., choose the csv radio button and click 'Save data'). Alternatively, you can copy the data from Radiant into the clipboard by choosing the clipboard radio button and clicking the 'Copy data' button. In Excel you can then paste the data from Radiant into a worksheet using CTRL-V (or CMD-V on mac).

### Save and load state

You can save and load the state of the Radiant app just as you would a data file. The state file (extension rda) will contain (1) the data you have loaded, (2) the analyses you were running last, (3) and any reports or code you may have created through the R-menu. This is convenient if you want to save your work to complete it at another time or to review any assignments for which you used Radiant. Save the state-file to your hard-disk and when you are ready to continue simply load it by selecting the `state` radio button and then clicking the `Choose file` button.

A related feature in Radiant is that state is also maintained if you close (and reopen) the browser and/or hit refresh on the browser. Loading and saving state now also works with Rstudio. If you start Radiant from Rstudio and use Quit > Quit to stop the app, a list called `values` and a list called `state_list` will be put into Rstudio's global workspace. If you start radiant again it will use these lists (i.e., `values` and `state_list`) to restore state. This can be convenient if you want to make changes to a data file and load it back into Radiant.

You can also open a state file directly in Rstudio. When you start Radiant from Rstudio it will use the state files to recreate a previous app state.

If you would like to reset Radiant to a clean state choose `Quit` in the navbar and hit the `Reset` button.

### Remove data from memory

If data are loaded that you no longer need access to in Radiant you can select them and click the 'Remove data' button. One datafile will always remain open.
