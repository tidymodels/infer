# print method fits linewidth with many predictors (#543)

    Code
      specify(mtcars, mpg ~ cyl + disp + hp + drat + wt + qsec)
    Output
      Response: mpg (numeric)
      Explanatory: cyl (numeric), disp (numeric), hp (numer...
      # A tibble: 32 x 7
           mpg   cyl  disp    hp  drat    wt  qsec
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21       6  160    110  3.9   2.62  16.5
       2  21       6  160    110  3.9   2.88  17.0
       3  22.8     4  108     93  3.85  2.32  18.6
       4  21.4     6  258    110  3.08  3.22  19.4
       5  18.7     8  360    175  3.15  3.44  17.0
       6  18.1     6  225    105  2.76  3.46  20.2
       7  14.3     8  360    245  3.21  3.57  15.8
       8  24.4     4  147.    62  3.69  3.19  20  
       9  22.8     4  141.    95  3.92  3.15  22.9
      10  19.2     6  168.   123  3.92  3.44  18.3
      # i 22 more rows

