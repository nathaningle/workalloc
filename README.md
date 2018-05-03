# workalloc

This is a tool to help me better understand how I’m spending my work day.  Given a text file listing the time I started working on something, and a description of the something, calculate the total amount of time I spent on that something.

## Input file format

Each line of the input file should look like this:

    YYYY-mm-dd HHMM␉what␉comment

where:

* ␉ is a horizontal tab,
* `YYYY-mm-dd HHMM` is the timestamp for starting this piece of work,
* `what` is the description of this piece of work (this will be the aggregation term), and
* `comment` can be anything and is ignored.
