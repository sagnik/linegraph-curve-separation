## LineGraph Curve Separation

Input to this system is a SVG file for a color line graph (curves are drawn with separate colors) as produced by https://github.com/sagnik/svgimagesfromallenaipdffigures. The output is a set of SVG files, each corresponding to a curve. 

For the **big picture**, see http://personal.psu.edu/szr163/hassan/hassan-Figure-2.html.

#### Application and Test

Remove the directory `src/test/resources/hassan-Figure-2` if exists. On SBT console, run `edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.CreateCurves` and the directory just removed will be re-created. The directory would contain one SVG file per curve. Currently, we do color based segmentation, but we recognize axes, grid lines and tick marks and remove them.      

#### Work in Progress

Black and white and greyscale line graphs. We can now detect four markers: rectangles, stars, diamonds and triangles.
                     
                     

    
