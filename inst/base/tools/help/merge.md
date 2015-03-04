> Merge two data file loaded in the Radiant app

There are four merge options in Radiant at this time:

* inner_join return all rows from x where there are matching values in y, and all columns from
x and y. If there are multiple matches between x and y, all combination of the matches are
returned.

* left_join return all rows from x, and all columns from x and y. If there are multiple matches
 between x and y, all combination of the matches are returned.
semi_join return all rows from x where there are matching values in y, keeping just columns from x.

* A semi join differs from an inner join because an inner join will return one row of x for each
matching row of y, where a semi join will never duplicate rows of x.

* anti_join return all rows from x where there are not matching values in y, keeping just columns
from x
