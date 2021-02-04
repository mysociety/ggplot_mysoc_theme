# mySociety ggplot theme

Theme to standardise ggplots charge using mySociety/SocietyWorks brand elements and logos.

Matches the altair theme in the `research_common` repository. Major updates should be matched in both.

## To use

For moment, source the theme file:

```source("ggplot_mysoc_theme//mysoc_theme.R")```

and then add: 

```+ mysoc_theme() + mysoc_discreet_scale()```
to the ggplot2 description theme and colours. 

`sworks_discreet_scale()` and `sworks_theme()` also avaliable.

Use `save_and_show(plot, filename)` to render properly with logo. There is a `no_logo` arguiment to remove this. 

## To do

* Format as a proper R package so it can be imported directly. 

## Licence

Code is under an MIT Licence. 

[Logos](https://www.mysociety.org/press/) are copyright mySociety/SocietyWorks.
