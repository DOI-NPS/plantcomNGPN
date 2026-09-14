# plantcomNGPN

## Description

R package for importing, compiling, and querying Northern Great Plains Network (NGPN) plant community 
monitoring data as collected in the FFI database.

This package includes the following functions:
<ul>
<li>importData: Imports tables from FFI SQL Server database or csvs of FFI
database tables using the schema designed for the Northern Great Plains
Network plant community monitoring protocol.</li>
<li>importViews: Imports flattened views of the FFI data for NGPN plant 
community monitoring, which should make up the data package views. This is a 
much faster way to import data into R than the importData function, and
does not require special software (eg SQL).</li>
<br>
<li>getMacroPlot: query macroplot data</li>
<li>getSampleEvent: query sample event data</li>
<li>getCoverPointData: <DELETED - PRETTY SURE NOT USED ANYMORE.></li>     
<li>getCoverPoints: query line intercept data</li>        
<li>getCoverSpeciesComp: query species composition data</li>   
<li>getDensityBelts: query density belt data</li>      
<li>getDensityQuadrats: query density belt quadrat data</li>    
<li>getDisturbanceHistory: query disturbance history data</li> 
<li>getFuels1000: query 1000hr fuels data</li>
<li>getFuelsDuff: query duff fuels data</li>         
<li>getFuelsFine: query fine fuels data </li> 
<li>getTaxa: query species lists</li>              
<li>getTrees: query tree data</li>        
<ul>

        

