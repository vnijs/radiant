> Manage data and state: Load data into Radiant, Save data to disk, Remove a dataset from memory, or Save/Load the full state of the app

### Datasets

When you first start Radiant a dataset (`diamonds`) with information on diamond prices is shown.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of Radiant you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add a description for your own data check the 'Add/edit data description' check-box. A window will open below the data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with Radiant should serve as a good starting point.

If you would like to rename a dataset loaded in Radiant check the `Rename data` box, enter a new name for the data, and click the `Rename` button

### Load data

The best way to load and save data for use in Radiant (and R) is to use the R-data format (rda). These are binary files that can be stored compactly and read into R quickly. Choose `rda` from the `Load data of type` dropdown and click `Choose Files` to locate the file(s) you want to load. If the `rda` file is available online choose `rda (url)` from the dropdown, paste the url into the text input, and press `Load`.

You can get data from a spreadsheet (e.g., Excel or Google sheets) into Radiant in two ways. First, you can save data from the spreadsheet in csv format and then, in Radiant, choose `csv` from the `Load data of type` dropdown. Most likely you will have a header row in the csv file with variable names. If the data are not comma separated you can choose semicolon or tab separated. To load a csv file click 'Choose files' and locate the file on your computer. If the `csv` data is available online choose `csv (url)` from the dropdown, paste the url into the text input shown, and press `Load`.

> **Note:** For Windows users with data that contain multibyte characters please make sure your data are in ANSI format so R(adiant) can load the characters correctly.

Alternatively, you can select and copy the data in the spreadsheet using CTRL-C (or CMD-C on mac), go to Radiant, choose `clipboard` from the dropdown, and click the `Paste data` button. This is a short-cut that can be convenient for smaller datasets that are cleanly formatted. If you see a message in Radiant that the data were not transferred cleanly try saving the data in csv format and loading it into Radiant as described above.

To access all data files bundled with Radiant choose `examples` from the `Load data of type` dropdown and click `Load examples`. These files are used to illustrate the various analysis tools accessible in Radiant. For example, the catalog sales data is used as an example in the help file for regression (i.e., Regression > Linear (OLS)).

### Save data

As mentioned above, the most convenient way to get data in and out of Radiant is to use the R-data format (rda). Choose `rda` from the `Save data` dropdown and click the `Save data` button to save selected dataset to file.

It is good practice to add a description of the data and variables to each file you use. For the files that are part of Radiant you will see a brief overview of the variables etc. below the table of the first 10 rows of the data. If you would like to add a description for your own data check the 'Add/edit data description' check-box. A window will open below that data table where you can add text in
<a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a> format. The descriptions of the data included with Radiant should serve as a good starting point. When you save the data as an rda file the description you created (or edited) will automatically be added to the file.

Getting data from Radiant into a spreadsheet can be achieved in two ways. First, you can save data in csv format and load the file into the spreadsheet (i.e., choose `csv` from the `Save data` dropdown and click the `Save data` button). Alternatively, you can copy the data from Radiant into the clipboard by choosing `clipboard` from the dropdown and clicking the `Copy data` button, open the spreadsheet, and paste the data from Radiant using CTRL-V (or CMD-V on mac).

### Save and load state

You can save and load the state of the Radiant app just as you would a data file. The state file (extension rda) will contain (1) the data loaded in Radiant, (2) settings for the analyses you were working on, (3) and any reports or code from the R-menu. Save the state-file to your hard-disk and when you are ready to continue simply load it by selecting the state radio button and clicking the `Choose file` button.

To save your analyses save the state of the app to a file by clicking on the <i title='Save' class='fa fa-save'></i> icon in the navbar and then on `Save state`. Similar functionality is available in `Data > Manage` tab.

This is convenient if you want to save your work to be completed at another time, perhaps on another computer, or to review any assignments you completed using Radiant. You can also share the file with others that would like to replicate your analyses. As an example, download and then load the state_file [`RadiantState.rda`](https://vnijs.github.io/radiant/examples/RadiantState.rda). Go to `Data > View`, `Data > Visualize` to see some of the settings loaded from the statefile. There is also a report in `R > Report` created using the Radiant interface. The html file <a href="https://vnijs.github.io/radiant/examples/RadiantState.html" target="_blank">`RadiantState.html`</a> contains the output.

A related feature in Radiant is that state is maintained if you accidentally navigate to another page, close (and reopen) the browser, and/or hit refresh. Use `Reset` in the <i title='Power off' class='fa fa-power-off'></i> menu in the navigation bar to return to a clean/new state.

Loading and saving state also works with Rstudio. If you start Radiant from Rstudio and use <i title='Power off' class='fa fa-power-off'></i> > `Stop` to stop the app, lists called `r_data` and `r_state` will be put into Rstudio's global workspace. If you start radiant again using `radiant()` it will use these lists to restore state. This can be convenient if you want to make changes to a data file in Rstudio and load it back into Radiant. Also, if you load a state file directly into Rstudio it will be used when you start Radiant to recreate a previous state.

### Remove data from memory

If data are loaded that you no longer need access to in the current session check the `Remove data from memory` box. Then select the data to remove and click the `Remove data` button. One datafile will always remain open.
