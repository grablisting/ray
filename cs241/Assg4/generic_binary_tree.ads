generic

   type Item is private;

   with procedure Visit (I : in Item);

   with function Compare (
         I1,
         I2 : Item)
     return Boolean;

package Generic_Binary_Tree is

   type Tree is private;

-- Inserts a value into a named binary tree
   procedure Insert (
         T : in out Tree;
         V : in     Item);

 -- Deletes a value into a named binary tree
   procedure Delete (
         T : in out Tree;
         V : in     Item);

   -- performs an InOrder traversal of a named binary tree
   procedure In_Order_Traverse (T : in Tree);

   function avgLengthToLeaves ( T : Tree ) return Float;
   function difMaxMinLeaves ( T : Tree ) return Integer;
   function minLeaf (
          T     : Tree;
                     Depth : Natural := 0 ) return Natural;

   function maxLeaf (
          T     : Tree;
          Depth : Natural := 0 ) return Natural;

private

   type Tree_Node is
      record
         Stored        : Item;
         Right_Subtree,
         Left_Subtree  : Tree;
      end record;

   type Tree is access Tree_Node;

end Generic_Binary_Tree ;

