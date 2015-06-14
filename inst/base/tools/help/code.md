> Run R-code

The code feature allows you to enter and run R-code with access to all functions and data in Radiant. By clicking the `Run` button, the code will be evaluated and the output will be shown on the right of the R > Code page. To evaluate only a part of the code use the cursor to select a section and press CTRL-return (CMD-return on mac) to create the (partial) output.

You can load an R-code file into Radiant by clicking the `Choose File` button and selecting an .r or .R file or save the R-code shown in the editor by pressing the `Save` button.

As an example you can copy-and-paste the text below into the editor and press `Run` to generate results.

<pre>## get the active dataset and show the first few observations
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
transform_main() %>% head
</pre>
