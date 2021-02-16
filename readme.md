# mySociety ggplot theme

Theme to standardise ggplots charge using mySociety/SocietyWorks brand elements and logos.

Matches the altair theme in the `research_common` repository. Major updates should be matched in both.

## To use

Can be installed with:

```
library(devtools)
install_github("mysociety/ggplot_mysoc_theme")
```

and then loaded with:

```
library(ggplotmysoc)
```

To use with a ggplot, add: 

```+ mysoc_theme() + mysoc_discreet_scale()```

to the ggplot2 description theme and colours. 

`sworks_discreet_scale()` and `sworks_theme()` also avaliable.

Use `save_and_show(plot, filename)` to render properly with logo. There is a `no_logo` arguiment to remove this. 

