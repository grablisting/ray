GENERIC
   TYPE ElementType IS (<>);
   TYPE IndexType IS (<>);
   TYPE KeyType IS (<>);
   TYPE ArrayType IS ARRAY (IndexType RANGE <>) OF ElementType;
   WITH FUNCTION GetKey (Element : ElementType; Key : KeyType) RETURN KeyType;
PROCEDURE Bucket_Sort (Source : IN OUT ArrayType; Key : IN KeyType);
