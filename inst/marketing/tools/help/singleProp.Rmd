## Markdown Quick Reference

Markdown is an easy-to-write plain text format for creating web content.

### Emphasis

*italic*   **bold**

_italic_   __bold__

### Headers

# Header 1
## Header 2
### Header 3
#### Header 4

### Unordered List

* Item 1
* Item 2
  * Item 2a
  * Item 2b

### Ordered List
1. Item 1
2. Item 2
3. Item 3
   * Item 3a
   * Item 3b

### Manual Line Breaks

End a line with two or more spaces:

Roses are red,   
Violets are blue.

### Links

Use a plain http address or add a link to a phrase:

http://rstudio.com

[Rstudio](http://rstudio.com)

### Images

Images on the web or local files in the same directory:

![alt text](http://upload.chinaz.com/2012/0720/1342775570137.jpg)

![alt text](figures/view.png)

For the help files using a chrome screen-capture tool is recommended:

[Awesome screenshot](https://chrome.google.com/webstore/detail/awesome-screenshot-captur/alelhddbbhepgpmgidjdcjakblofbmce?hl=en)

### Blockquotes

A friend once said:

> It's always better to give 
> than to receive.

### R Code Blocks

R code will be evaluated and printed
```{r}
summary(cars$dist)
summary(cars$speed)
```

#### Inline R Code

There were `r nrow(cars)` cars studied

#### Plain Code Blocks

Plain code blocks are displayed in a fixed-width font but not evaulated
```
This text is displayed verbatim / preformatted
```

### Horizontal Rule / Page Break

Three or more asterisks or dashes:

******
------

### Links

A [linked phrase][id]. 

At the bottom of the document:

[id]: http://Rstudio.com/ "Title"

### Miscellaneous

superscript^2

~~strikethrough~~

## Math

$$ y_t = \alpha + \beta X + \epsilon_t $$

An inline version looks like this: $y_t = \alpha + \beta X + \epsilon_t$
