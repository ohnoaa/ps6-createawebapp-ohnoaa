# ps6-createawebapp-ohnoaa

*Instructions: Your github repo should include a readme file that
contains a brief "user documentation" of your project: a brief
description of the data, and explanation what are the widgets and panels
doing. This file should contain a link to your project on the
<https://www.shinyapps.io> server.*

## Shiny Webpage:

<https://ohnoa.shinyapps.io/SeattleWeatherData/>

## User documentation

### Brief description of the data:

The data that I used for this problem set is about the daily weather in
Seattle from 1948 to 2017. It contains information like the specific
date, maximum daily temperature, minimum daily temperature, whether it
rained or not, and the amount of precipitation for that day, if it
rained. I got the data from the website called Kaggle.

### Widgets explanation:

#### Plot Tab:

In the plot section, I have three separate widgets for the plot. The
first is a checkbox that asks the user if they have a preference to see
the trend line or not. The second is the radio buttons that change the
color of the points on the plot, without changing the data. The third,
and last, one is a date range input widget that correlates with the
dates in the data. With this widget, the user can choose to see data
between different selected time periods.

In the plot section, I have a standard side and main panel set up, where
the side panel has information about the plot and the widgets and the
main panel depicts the labeled plot itself and the textual output that
reacts with the date range input widget.

#### Table Tab:

In the table section, I focused on one widget for the plot. This widget
is a radio button, where users can select the season they want to see
the data of the amount of precipitation for. There are no visual widgets
for this section.

In the table section, I used the same layout to the plot tab, where the
side panel has informative instructions and the widget and the main
panel contains the data table itself with the textual output that reacts
with the season widget.
