> Manage data and state: Load data into Radiant, Save data to disk, Remove a dataset from memory, or Save/Load the full state of the app

#### Load data

An easy way to store data and get in and out of Radiant (and R) is to use the R-data format (rda). These are binary files that can be stored compactly and read into R quickly. Choose the rda radio button and click `Choose Files` to locate the file you want to load.

You can get data from Excel into Radiant in two ways. First, you can save data from Excel in csv format and then, in Radiant, choose the `csv` radio button. Most likely you will have a header row in the csv file for variable names. If the data are not comma separated you can choose semicolon or tab separated. To load a csv file click 'Choose files' and locate the file on your computer.

Alternatively, you can select and copy the data in Excel using CTRL-C (or CMD-C on mac), go to Radiant, choose the `clipboard` radio-button, and click the `Paste data` button. This is a short-cut that can be convenient for smaller datasets that are cleanly formatted. If the data is not transferred cleanly to Radiant try saving the data in csv format and loading it into Radiant as described above.

To access all data files bundled with Radiant choose the examples radio button and click `Load examples`. These files are used to illustrate the various analysis tools in Radiant. For example, the catalog sales data is used as an example in the help file for regression (i.e., Regression > Linear (OLS)).

#### Save data

As mentioned above, the most convenient way to get data in and out of Radiant is to use the R-data format (rda). Choose the rda radio button and click the `Save data` button to store the data on your computer.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of Radiant you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add a description for your own data check the 'Add/edit data description' check-box. A window will open below that data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with Radiant should serve as a good starting point. When you save the data as an rda file the description you created (or edited) will automatically be added to the file.

Getting data from Radiant into Excel can also be achieved in two ways. First, you can save data in csv format and load it into Excel (i.e., choose the csv radio button and click `Save data`). Alternatively, you can copy the data from Radiant into the clipboard by choosing the clipboard radio button and clicking the `Copy data` button. Paste the data from Radiant into a worksheet in Excel using CTRL-V (or CMD-V on mac).

#### Save and load state

You can save and load the state of the Radiant app just as you would a data file. The state file (extension rda) will contain (1) the data loaded in Radiant, (2) settings for the analyses you were working on, (3) and any reports or code from the R-menu. Save the state-file to your hard-disk and when you are ready to continue simply load it by selecting the state radio button and clicking the `Choose file` button.

This is convenient if you want to save your work to be completed at another time, perhaps on another computer, or to review any assignments you completed using Radiant. You can also share the file with others that would like to replicate your analyses. As an example, download and then load the state_file [`RadiantState.rda`](https://github.com/mostly-harmless/radiant/blob/master/inst/examples/RadiantState.rda?raw=true). Go to `Data > View`, `Data > Visualize` to see some of the settings loaded from the statefile. There is also a report in `R > Report` created using the Radiant interface. The html file [`RadiantState.html`](https://github.com/mostly-harmless/radiant/blob/master/inst/examples/RadiantState.html?raw=true) contains the output.

A related feature in Radiant is that state is maintained if you accidentally navigate to another page, close (and reopen) the browser, and/or hit refresh. Use `Quit > Reset` to return to a clean/new state.

Loading and saving state also works with R(studio). If you start Radiant from Rstudio and use `Quit > Quit` to stop the app, lists called `r_data` and `r_state` will be put into Rstudio's global workspace. If you start radiant again using `radiant()` it will use these lists (i.e., `r_data` and `r_state`) to restore state. This can be convenient if you want to make changes to a data file in Rstudio and load it back into Radiant. Also, if you load a statefile in Rstudio it will be used when you start Radiant to recreate a previous state.

#### Remove data from memory

If data are loaded that you no longer need access to in the current session you can select them and click the `Remove data` button. One datafile will always remain open.
