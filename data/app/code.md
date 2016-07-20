> Run R-code

The code feature allows you to run R-code with access to all functions and data in Radiant. By clicking the `Run code` button, the code will be evaluated and the output will be shown on the right of the _R > Code_ page. To evaluate only a part of the code use the cursor to select a section and press CTRL-return (CMD-return on mac).

You can load an R-code file into Radiant by clicking the `Choose File` button and selecting an .r or .R file. If you started Radiant from Rstudio you can save a report in HTML, Word, or PDF format by selecting the desired format from the drop-down manu and clicking `Save`. To save just the code choose `R-code` from the dropdown and press the `Save` button.

As an example you can copy-and-paste the code below into the editor and press `Run code` to generate results.

```r## get the active dataset and show the first few observations
.getdata() %>% head

## access a specific dataset by name
r_data[['diamonds']] %>% select(price, clarity) %>% head

## add a variable to the diamonds data
dat <- r_data[['diamonds']]
dat$log_price <- log(dat$price)

## show the first observations
dat %>% select(price, log_price) %>% head

## create a histogram of prices
dat %>% ggplot(aes(x = price)) + geom_histogram()

## and a histogram of log-prices
dat %>% ggplot(aes(x = log_price)) + geom_histogram()

## open help in the R-studio viewer from Radiant
help(package = 'radiant')

## if you are familiar with Shiny you can call reactives here
## for example, if you just transformed some variables in Data > Transform
## you can call the transform_main reacive to see the latest result
## this can very useful for debugging
# transform_main() %>% head
```
