# test-burglar

So the task essentially is a path finding on a weighted graph. 
The path finding algorithm would be the A*, for its effectiveness, 
speed, me implemented it once before and being available on Hackage. 
There's also room for improvement left by improving heuristic distance 
function -- it is exact distance at the moment, since I couldn't come up 
with anything better.

Now, the slight issue here was that the detection chance is defined for 
the node at `(x, y)`, not for the cell-to-cell transition. This has been solved 
by making edge weight a difference of destination detection probability 
and source, effectively making particular transition more likely if detection 
chance decreases and less likely otherwise. In order to fix to Dijkstra 
algorithm's failure to process negative edge weights, I'm adding `2.0` 
to the difference, so the edge weight will always be positive. 

## Usage

### Compiling

Having [`stack`](http://haskellstack.com/) installed, simply run `stack build`.

### Running

*   Run `stack exec test-burglar-exe` to run previously compiled executable
*   Following the prompt, enter space-separated room width & length. I.e. "10 10" 
    means room of 10×10
*   Enter space-separated alarm definitions, one per line
*   Read & use action plan printed for you.

Happy burglary and good luck!
