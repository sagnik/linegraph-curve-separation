## LineGraph Curve Separation

Input to this system is a SVG file for a line graph as produced by https://github.com/sagnik/svgimagesfromallenaipdffigures. The output is a set of SVG files, each corresponding to a curve. 

For the **big picture**, see http://personal.psu.edu/szr163/hassan/hassan-Figure-2.html.

#### Application and Test

Remove the directory `src/test/resources/hassan-Figure-2` if exists. On SBT console, run `edu.ist.psu.sagnik.research.linegraphcurveseparation.impl.CreateCurvesColor` and the directory just removed will be re-created. 

#### Work in Progress

Black and white and greyscale line graphs. 

We can now detect six  markers: rectangles, stars, diamonds, triangles, plus and cross. See `src/test/resources/10.1.1.105.5053-Figure-1-*.svg` for an example. Note that these are the markers drawn with paths that don't have a "fill" element.

#### Working examples

Black and white curves: http://personal.psu.edu/szr163/test/test.html

Color curves: http://personal.psu.edu/szr163/hassanfig2.html                     
                     

    
