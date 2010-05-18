WITH unchecked_deallocation;

GENERIC
   TYPE ElementType IS (<>);
   TYPE IndexType IS (<>);
PACKAGE CommonHelpers IS

   TYPE DataArray IS ARRAY (IndexType RANGE <>) OF ElementType;
   TYPE pDataArray IS ACCESS DataArray;
   PROCEDURE DestroyArray IS NEW Unchecked_Deallocation (Object => DataArray, Name => pDataArray);

   NewFlag : ElementType := ElementType'Val(0);
   EmptyFlag : ElementType := ElementType'Val(-1);
   DirtyFlag : ElementType := ElementType'Val(1);

   PROCEDURE PrintArray (Source : IN pDataArray; ExistenceCheck : Boolean);
   FUNCTION BuildEmptyArray (Min, Max : IndexType) RETURN pDataArray;
   FUNCTION BuildInitializedArray (Min, Max : IndexType; InitialValue : ElementType) RETURN pDataArray;
   FUNCTION BuildArrayFromFileName (Source : String) RETURN pDataArray;

END CommonHelpers;
