GENERIC
   TYPE ElementType IS PRIVATE;
   TYPE IndexType IS (<>);
   TYPE ArrayType IS ARRAY (IndexType RANGE <>) OF ElementType;
   WITH PROCEDURE Sort (Source : IN OUT ArrayType; Key : IN Integer);
PROCEDURE radix_sort (Source : IN OUT ArrayType; d : IN Integer);
