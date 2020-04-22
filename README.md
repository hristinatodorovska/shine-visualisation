# Shiny

Shiny is an R package that makes it easy to build interactive web apps straight from R. You can host standalone apps on a webpage or embed them in R Markdown documents or build dashboards. You can also extend your Shiny apps with CSS themes, htmlwidgets, and JavaScript actions

## Installation

If you still haven’t installed the Shiny package, open an R session, connect to the internet, and run

```bash
install.packages("shiny")
```

## Usage

```python
library(shiny)
```

## Structure of a Shiny App
Shiny apps are contained in a single script called ```app.R``` The script ```app.R``` lives in a directory (for example, newdir/) and the app can be run with ```runApp("newdir")```


## Deploy Your App
https://www.linode.com/docs/development/r/how-to-deploy-rshiny-server-on-ubuntu-and-debian/

## ui.R

Create user interface;
Control the layout, appearance, Widgets that capture user inputs. Also, displays the output.
Eg: the title, page layout, text input, radio buttons, drop down menus, graph etc.

## server.R
Set of instructions that uses the input provided by the user, process them and produces the required output which is further displayed by ui.R script


```bash
install.packages("shiny")
install.packages("shinydashboard")
```