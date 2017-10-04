Energy Consumption and Emissions README

This Shiny/Leaflet app utilizes data obtained from U.S. EIA (Energy Information Administration) to create maps of

1. Monthly energy consumption data per power plant for a subset of the power plants in the U.S. (the ones who were presented with and returned the survey on that month).  The units are MMBTUs (one million British Thermal Units).  As of now, I only have power plants for coal and natural gas.  I will add power plants for petroleum.  I have the raw data for petroleum in the same JSON file that gave me the data in this map.  The power plants are colored and sized based on the energy being consumed from a particular plant.  The tab for this map allows the user to choose what year and month to look at and what energy source they are interested in viewing.  If the user clicks on a particular plant, a popup graph will show a time series of the energy consumption for that plant (for the chosen fuel type).

2. Yearly greenhouse gas emissions per state.  The tab for this map allows the user to choose the year to view as well as the emission type (Carbon Dioxide, Sulfur Dioxide, or Nitrogen Oxides) and fuel source of the emissions (All Sources, Coal, Natural Gas, Other, or Petroleum).  The units for the emissions are metric tons.


To do:

1. There is a strange bug that I need to fix in the second tab.  The colors for the map only show up after I click on one of the options in the drop down menu.  I need to find the bug and remove it.

2. Add tabs for yearly energy consumption per plant and per state.  I either have the data in the JSON file that I retrieved the other power plant data or will do my best to find it elsewhere.

3. Add popup graphs for second tab.  These popup graphs will show the time series data for the state that is clicked and the chosen emission and source types.

4. Find other sources of Energy information.  Possibilities include the Department of Energy.

5. Perform more research on the data that I am presenting, so I can explain how the EIA is choosing which power plants to present the surveys to each month.  This effort will also uncover other important details of the data that I am visualizing in this app.
