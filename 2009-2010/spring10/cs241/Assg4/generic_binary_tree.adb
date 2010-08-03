--------------------------------------------------------
--------------------------------------------------------
-- Generic_Binary_Tree package body
-- Written by Ray Peters
-- June 11, 2010
--------------------------------------------------------
--------------------------------------------------------

package body Generic_Binary_Tree is

   -- This function determines if the treenode is a leafnode
   function isLeaf (
         T : Tree ) return Boolean is
   begin
      -- Protection from recursive inquiries from other functions
      if T = null then
         return false;
      end if;

      -- If both the Left_subtree and right_subtree
      -- are null, it is a leaf node.
      RETURN (T.ALL.Left_Subtree = NULL AND T.ALL.Right_Subtree = NULL);
   end isLeaf;

   -- This function determines the number of leaves in a tree.
   function numberOfLeaves (
         T : Tree ) return Integer is
   begin
      -- Calls passing null trees are discarded
      -- to protect against recursive inquiries.
      if T = null then
         return 0;
      end if;

      -- If the treenode is a leaf, it will return a value.
      -- No other types of nodes return values that increment
      -- the resulting count of leaves.
      if isLeaf (T) then
         return 1;

      else
         -- Add the number of leaves of both subtrees of the current tree.
         return numberOfLeaves (T.ALL.Left_Subtree)
           + numberOfLeaves (T.ALL.Right_Subtree);

      end if;
   end numberOfLeaves;

   -- This function returns the sum of all paths to every leaf
   -- much like how numberOfLeaves returns the number of leaves
   -- This function also records a depth count of the treenodes
   -- and returns the current depth of leaf nodes.
   function sumOfAllPathsToLeaves (
         T    : Tree;
         depth : Natural ) return Integer is
   begin
      -- Calls passing null trees are discarded
      -- to protect against recursive inquiries.
      if T = null then
         return 0;
      end if;

      -- Return the depth at this node to be added
      -- to the sum of the paths.
      if isLeaf (T) then
         return depth;

      else
         return sumOfAllPathsToLeaves (T.ALL.Left_Subtree, depth + 1)
           + sumOfAllPathsToLeaves (T.ALL.Right_Subtree, depth + 1);

      end if;
   end sumOfAllPathsToLeaves;

   -- This function determines the average path to leaf nodes
   function avgLengthToLeaves (
         T : Tree ) return Float is
   begin
      return float(sumOfAllPathsToLeaves(T, 0)) / float(numberOfLeaves(T));
   end avgLengthToLeaves;

   -- This function determines the depth of a node by
   -- searching for an element while keeping track of the depth
   -- and then returns the depth upon arrival to the node of interest
   function nodeDepth (
         T    : Tree;
         V    : Tree;
         Depth : Natural := 0 ) return Natural is
   begin
      -- Recursive protection
      if T = null then
         return 0;
      end if;

      -- Recursive exit condition: arrival at node of interest
      -- Return depth count.
      if T = V then
         return Depth;

      elsif Compare (T.ALL.Stored, V.ALL.Stored) then
         -- Search right subtree
         return nodeDepth (T.ALL.Right_Subtree, V, Depth + 1);

      else
         -- Search left subtree
         return nodeDepth (T.ALL.Left_Subtree, V, Depth + 1);

      end if;
   end nodeDepth;

   -- Returns the length of the path to the minimum leaf
   function minLeaf (
          T     : Tree;
          Depth : Natural := 0 ) return Natural is

      fromLeft,
      fromRight : Natural;
   begin
      -- Calls passing null trees are discarded
      -- to protect against recursive inquiries.
      if T = null then
         return Natural'Last;
      end if;

      -- Return the depth at each left node
      if isLeaf (T) then
         return depth;

      else
         fromLeft := minLeaf (T.ALL.Left_Subtree, Depth + 1);
         fromRight := minLeaf (T.ALL.Right_Subtree, Depth + 1);
         -- Keep the minimum path
         if fromLeft < fromRight then
            return fromLeft;
         else
            return fromRight;
         end if;

      end if;
   end minLeaf;


   -- Returns the length of the path to the maximum leaf
   function maxLeaf (
         T     : Tree;
         Depth : Natural := 0 ) return Natural is

      fromLeft,
      fromRight : Natural;
   begin
      -- Calls passing null trees are discarded
      -- to protect against recursive inquiries.
      if T = null then
         return Natural'First;
      end if;

      -- Return the depth at each left node
      if isLeaf (T) then
         return depth;

      else
         fromLeft := maxLeaf (T.ALL.Left_Subtree, Depth + 1);
         fromRight := maxLeaf (T.ALL.Right_Subtree, Depth + 1);
         -- Keep the maximum path
         if fromLeft > fromRight then
            return fromLeft;
         else
            return fromRight;
         end if;

      end if;
   end maxLeaf;

   -- This function returns the minimum value of the tree
   function treeMin (
         T : Tree ) return Tree is

      temp : Tree := T;
   begin
      -- The minimum key of the tree is located
      -- at the left-most node of the entire tree.
      while temp.ALL.Left_Subtree /= null loop
         temp := temp.ALL.Left_Subtree;
      end loop;

      return temp;
   end treeMin;

   -- This function returns the maximum value of the tree
   function treeMax (
         T : Tree ) return Tree IS

      temp : Tree := T;
   begin
      -- The maximum key of the tree is located
      -- at the right-most node of the entire tree.
      while temp.ALL.Right_Subtree /= null loop
         temp := temp.ALL.Right_Subtree;
      end loop;
      return temp;
   end treeMax;

   -- This function returns the height difference between the longest
   -- and shortest leaves of the tree
   function difMaxMinLeaves (
         T : Tree ) return Integer is
   begin
      return maxLeaf(T, 0) - minLeaf(T, 0);
   end difMaxMinLeaves;

   --  Inserts a value into a named binary tree
   procedure Insert (
         T : in out Tree;
         V : in     Item) is
   begin
      -- Find an empty spot and create a new tree_node
      IF T = NULL THEN
         T := NEW Tree_Node;
         T.ALL.Stored := V;

      ELSIF Compare (T.ALL.Stored, V) THEN
         -- Current node is occupied, must find a different node
         -- and still fulfill BST property.
         -- If compare is true, go right!
         Insert (T.ALL.Right_Subtree, V);

      ELSE
         -- Else, go left!
         Insert (T.ALL.Left_Subtree, V);

      END IF;
   end Insert ;


   --  Deletes a value from a named binary tree
   procedure Delete (
         T : in out Tree;
         V : in     Item) is

      temp : Tree;
   begin
      -- Make sure we've got a treeNode.
      if T /= null then
         if T.ALL.Stored = V then
            -- When T.ALL.Stored = V, we've arrived at the node
            -- to be deleted. Check children for deletion instructions.

            -- Single children case: replace deleted node with child node.
            if T.Left_Subtree = null then
               T := T.ALL.Right_Subtree;

            elsif T.Right_Subtree = null then
               T := T.ALL.Left_Subtree;

            else
               -- No children and Two children case:
               -- get the in order successor, and then
               -- replace it with the node to be deleted
               -- and delete the successor from it's subtree.
               temp := treeMin(T.ALL.Right_Subtree);
               T.ALL.Stored := temp.ALL.Stored;
               Delete (T.ALL.Right_Subtree, T.ALL.Stored);

            end if;
         else
            -- If T.ALL.Stored /= V, continue traversing the tree.
            if Compare (T.ALL.Stored, V) then
               Delete (T.ALL.Right_Subtree, V);

            else
               Delete (T.ALL.Left_Subtree, V);

            end if;
         end if;
      end if;
   end Delete;

   --  performs an In-Order traversal of a named binary tree
   procedure In_Order_Traverse (
         T : in     Tree) is

   begin
      -- Recursive exit condition
      IF T /= NULL THEN
         -- Visit left subtree
         In_Order_Traverse (T.ALL.Left_Subtree);

         -- Visit root
         Visit (T.ALL.Stored);

         -- Visit right subtree
         In_Order_Traverse (T.ALL.Right_Subtree);
      END IF;
   end In_Order_Traverse;

end Generic_Binary_Tree ;

