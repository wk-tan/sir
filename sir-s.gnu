reset
set terminal pngcairo dashed enhanced size 480,360 font 'arial,12' fontscale 1.0
set encoding utf8
set output 's.png'
set xrange [0:100]
set yrange [0:200]
set xlabel "{/Arial:Bold Time}"
set ylabel "{/Arial:Bold Number of susceptible}"
plot for [i=1:100:1] 'output_'.i.'.txt' u 1:2 w l notitle,\
     'sir-deterministic.txt' u 1:2 lt 1 lw 4.0 lc "black" w l notitle