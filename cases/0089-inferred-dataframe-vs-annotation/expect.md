`data_frame(units = c(...), price = c(...))` is inferred as `DataFrame[3, {price: num, units: num}]`,
which is rejected where `dataframe[3]{ units: num, price: num }` is expected
("No signature of function 'total' matches this call"). Row count and column names agree; the
mismatch comes from the column types: the inferred side stores `Vec[3, num]` per column, the
annotation `num`. The inferred form should be accepted (a `data_frame(...)` call is the natural
way to build one for R users).

Code: `type_checking/mod.rs`, `Lang::DataFrame` arm.
Found while checking the landing-page examples (typr.github.io, "What R lets through").

Fixed: the constructor now stores each column by its element type (`field_access::column_element_type`),
the same shape an annotation has; the row count lives once, in the dataframe index.
