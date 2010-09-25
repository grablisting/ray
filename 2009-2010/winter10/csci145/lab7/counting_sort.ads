GENERIC
   TYPE ElementType IS (<>);
   TYPE IndexType IS (<>);
   TYPE ArrayType IS ARRAY (IndexType RANGE <>) OF ElementType;
PROCEDURE Counting_Sort (Source : IN OUT ArrayType);
