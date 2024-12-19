# finalproj

A data-driven tool for analyzing Diabetes cases in New Jersey counties. Through the use of an interactive dashboard, the software allows users to examine Diabetes data by year, age group, and county.


Important Features
- Adaptive Views: A line chart illustrating the temporal patterns in diabetes cases is included in the dataset summary.
- Statistics: Statistical summaries that include county-by-county mean and median cases.
- Rankings: A bar graph that shows the number of diabetes cases in each county.
- Heatmap: A heatmap that shows cases by county and age group.
- Top Counties: A list of the ten counties with the greatest number of diabetes cases.
- Options for Filtering: Users can filter data by county and year for a more individualized study.
-Switch between views to concentrate on particular data points and total cases of diabetes and historical trends.
- Age Distribution: Diabetes cases by age group are shown in a bar chart.
- County Table: A list of diabetes cases for particular counties and years in tabular form.


Restrictions
- Visualizing Maps: The goal of a map-based visualization was to show data spatially, however, it was not properly executed. 
- 2019 data: For 2019, the data is incomplete. This is one of the interesting aspects of data. The rows that included the 2019 data were deleted, as this flaw caused false/unrealistic visualizations.
- Analysis by Per Capita: Despite having the data, the ability to show the data per capita, cases per 100,000 people, is not fully loading. The chart displays, although the data is not coming up. 

- Status of Submission: We are submitting an updated version of the app we presented. We filtered the data to remove 2019. This made the chart more accurate. There is also a tab for Per Capita data, but results are not displayed when clicked on. There are multiple iterations of the code, some with better visualization, in the repository. 

Final Projects Folder: Final App (App5), Population Excel, and Combined Data (csv)
App Versions: *App3 and 4: Attempting the Per Capita Data to be seen throughout all of the different tabs/visualizations. 
App3: Most complex visualization
It could be that the data is taking time to load.

How It Operates
The following elements are included in the application, which was created in R using the Shiny framework:

Interface for Users (UI)
- Dropdown choices for choosing the preferred view, year or years, and county or counties are located in the sidebar.
- Main Panel: Updates dynamically to show the chosen view, complete with tables and charts.

The server
- interprets user input and applies the appropriate filter to the dataset.
- creates tables and graphics for every view that is chosen.
- makes use of the ggplot2, DT, and dplyr packages for tabular outputs, data processing, and visualization, respectively.


Summary:
- shows the overall number of instances for each county and year.
- displays the total number of instances over time as a trend line.
Distribution of Ages: a bar graph that breaks down cases by county and age group.
Table of County: An interactive diabetes case table for certain counties and years.
Patterns: Diabetes case trends for a few chosen counties are displayed in a line graph.
Data: Mean and median cases by age group for each county are summarized in this table.
Rankings: Counties are ranked by the number of diabetes cases in a bar chart.
Heatmap: A heatmap that displays the distribution of cases by county and age group.
Leading Counties: identifies the ten counties with the highest rates of diabetes cases.


Recognized Problems
- Absent 2019 Data: 2019 interpolated or imputed data is not included in the app.
- Metric per capita: There is no way to switch between per capita statistics and raw figures. There are display issues due to unknown complications.
- Unreliable Visualization: Some views might not refresh properly because of missing or insufficient data.
- Difficulties collaborating: Working with others on this platform proved to be difficult. It was challenging to actively code/commit/push/pull and not have errors.


Instructions for Use
- Use shinyApp(ui, server) in RStudio to launch the application.
- Choose a view, year or years, and county or counties using the Sidebar.
- Examine the Main Panel's dynamically produced tables and visuals.
- Take note of the per capita analysis and missing data restrictions.

Modifications Considering Code changes
- There is a clear explanation of the new Per Capita table and graphic.
- The computations and blending of population data are indicated.
- The ability to dynamically filter by counties and years is also highlighted.


