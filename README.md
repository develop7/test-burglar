# test-burglar

So the task essentially is a path finding on a weighted graph. 
The path finding algorithm would be the A*, for its effectiveness and speed; 
also it should fit our purposes as well, because the room doesn't really have 
any obstacles and detection chance is inversely proportional to the distance 
from the sensor.

Now, the slight issue here was that the detection chance is defined for 
the node at `(x, y)`, not for the cell-to-cell transition. This has been solved 
by making edge weight a difference of destination detection probability 
and source, effectively making particular transition more likely if detection 
chance decreases and less likely otherwise. 

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
