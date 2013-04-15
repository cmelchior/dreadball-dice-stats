
To install Troll on your computer, you first need to install Moscow
ML.  There are instructions for this on the Moscow ML homepage
(http://www.itu.dk/people/sestoft/mosml.html).  When this
accomplished, go to the Troll webpage
(http://www.diku.dk/~torbenm/Troll/) and download the Troll.zip file.
You have probably already done so if you are reading this file.

Unpack the zip-file in the directory where you want to install Troll
and run one of the compile scripts: compile.sh for Linux/Unix and
compile.bat for Windows.  This will create a binary program called
troll (Unixes) or troll.exe (Windows).  The compiler will issue a
warning about a non-standard feature used in the parser.  This can
safely be ignored.

When you have Troll installed, you can run it from a command line.
You need to have a terminal or shell window open to do this.

Troll is invoked by the command troll.  If run without arguments, it
reads a Trollprogram from the terminal window (standard input) and
prints a single random sample of the roll described by the Troll
program.  Example:

shell> troll
2d6
<eof character>
1 6
shell>

where <eof character> is what you need to type to end terminal input
on your system.  For Unixes, this is ctrl-D and on Windows it is
ctrl-Z.

Multisets are shown as sorted lists of numbers separated by spaces.

You can add a number to the troll command to get several samples:

shell> troll 5
2d6
<eof character>
1 4 
5 5 
2 6 
2 5 
4 6 
shell>

Each sample is shown on a separate line.  If you specify 0 as the
number to the troll command, you get a probability distribution:

shell> troll 0
2d6
<eof character>
    Value    % =                 % >=              
     1 1 :    2.77777777778     100.0               
     1 2 :    5.55555555556      97.2222222222      
     1 3 :    5.55555555556      91.6666666667      
     1 4 :    5.55555555556      86.1111111111      
     1 5 :    5.55555555556      80.5555555556      
     1 6 :    5.55555555556      75.0               
     2 2 :    2.77777777778      69.4444444444      
     2 3 :    5.55555555556      66.6666666667      
     2 4 :    5.55555555556      61.1111111111      
     2 5 :    5.55555555556      55.5555555556      
     2 6 :    5.55555555556      50.0               
     3 3 :    2.77777777778      44.4444444444      
     3 4 :    5.55555555556      41.6666666667      
     3 5 :    5.55555555556      36.1111111111      
     3 6 :    5.55555555556      30.5555555556      
     4 4 :    2.77777777778      25.0               
     4 5 :    5.55555555556      22.2222222222      
     4 6 :    5.55555555556      16.6666666667      
     5 5 :    2.77777777778      11.1111111111      
     5 6 :    5.55555555556       8.33333333333     
     6 6 :    2.77777777778       2.77777777778     
shell>

The probabilities are shown both for the result being equal to a value
and for the result being at least a value.  Lexicographic ordering of
the multisets (as sorted lists) is used.  By default, the
probabilities are shown in percent, but if you add the -p option to
the troll command, you get the probabilities as numbers between 0 and
1.

If the roll uses the accumulate loop or recursive functions, you can
limit the iteration/recursion depth by specifying a negative number to
the troll command.  If you specify, for example, -5, you limit
iterations and call depth to 5:


shell> troll -5
sum accumulate x:=d2 until x=2
<eof character>
  Value    % =                 % >=              
     2 :   50.0                96.875             
     3 :   25.0                46.875             
     4 :   12.5                21.875             
     5 :    6.25                9.375             
     6 :    3.125               3.125             

Average = 2.75  Spread = 1.17260393996  Mean deviation = 0.8359375
shell>

Note that average, spread and mean deviation are calculated as all
possible values are single numbers.  The default limit is 12, so troll
0 and troll -12 are equivalent:

shell> troll 0
sum accumulate x:=d2 until x=2
<eof character>
   Value    % =                 % >=              
      2 :   50.0                99.9755859375      
      3 :   25.0                49.9755859375      
      4 :   12.5                24.9755859375      
      5 :    6.25               12.4755859375      
      6 :    3.125               6.2255859375      
      7 :    1.5625              3.1005859375      
      8 :    0.78125             1.5380859375      
      9 :    0.390625            0.7568359375      
     10 :    0.1953125           0.3662109375      
     11 :    0.09765625          0.1708984375      
     12 :    0.048828125         0.0732421875      
     13 :    0.0244140625        0.0244140625      

Average = 2.99633789062  Spread = 1.40233352785  Mean deviation = 0.99706941843
shell>

You can also show the probability distribution as a histogram (in
ASCII graphics) by using the -g<x> option, where <x> is the
number of characters per percent in the histogram:

shell> troll 0 -g4
sum largest 3 4d6
<eof character>
   Value   4.0 bars per %
      3 :  
      4 :  |
      5 :  |||
      6 :  ||||||
      7 :  ||||||||||||
      8 :  |||||||||||||||||||
      9 :  ||||||||||||||||||||||||||||
     10 :  ||||||||||||||||||||||||||||||||||||||
     11 :  ||||||||||||||||||||||||||||||||||||||||||||||
     12 :  ||||||||||||||||||||||||||||||||||||||||||||||||||||
     13 :  |||||||||||||||||||||||||||||||||||||||||||||||||||||
     14 :  |||||||||||||||||||||||||||||||||||||||||||||||||
     15 :  ||||||||||||||||||||||||||||||||||||||||
     16 :  |||||||||||||||||||||||||||||
     17 :  |||||||||||||||||
     18 :  ||||||

Average = 12.2445987654  Spread = 2.84684444531  Mean deviation = 2.31853947569
shell>

You can also take the Troll program from a file by specifying a file
name on the command line.  If the file d20stat.t contains the Troll
program sum largest 3 4d6, the command line

troll 0 -g4 d20stat.t

will produce the same output as above.  File names can not start with
digits nor contain the = character.


You can also specify the value of a variable from the command line.
If, for example, the file Nd6.t contains the Troll program sum N d6,
the command 

troll 0 Nd6.t N=5

will set N to 5 in the program, so you get the probability
distribution for sum 5 d6.


See manual.pdf for a more detailed description of how to use Troll.
