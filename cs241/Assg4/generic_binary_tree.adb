
with Ada.Text_IO;
package body Generic_Binary_Tree is


   -- Inserts a value into a named binary tree
   procedure Insert (
         T : in out Tree;
         V : in     Item) is

   begin
      IF T = NULL THEN
         T := NEW Tree_Node;
         T.ALL.Stored := V;
      ELSIF Compare (T.ALL.Stored, V) THEN
         Insert(T.ALL.Right_Subtree, V);
      ELSE
         Insert(T.ALL.Left_Subtree, V);
      END IF;
   end Insert ;

   function findNode (
         T : Tree;
         V : Item) return Tree IS
   begin
      IF T.ALL.Stored = V THEN
         RETURN T;
      ELSIF Compare (T.ALL.Stored, V) THEN
         RETURN findNode (T.ALL.Right_Subtree, V);
      ELSE
         RETURN findNode (T.ALL.Left_Subtree, V);
      END IF;
   end findNode ;

   -- Deletes a value from a named binary tree
   procedure Delete (
         T : in out Tree;
         V : in     Item) is
      treeNode : Tree;
      nodeClass : Integer;
      --tempNode : Tree;
   begin
      --Find the node in the tree.
      treeNode := findNode (T, V);

      IF treeNode.ALL.Left_Subtree /= NULL THEN
         nodeClass := nodeClass + 1;
      END IF;
      IF treeNode.ALL.Right_Subtree /= NULL THEN
         nodeClass := nodeClass + 1;
      END IF;


      --Delete the node and refix children.
      IF nodeClass = 2 THEN
         treeNode.ALL := treeNode.ALL.Left_Subtree.ALL;
      ELSIF nodeClass = 1 THEN
         --tempNode := NEW Tree_Node;
         IF treeNode.ALL.Left_Subtree /= NULL THEN
            treeNode.ALL := treeNode.ALL.Left_Subtree.ALL;
         ELSIF treeNode.ALL.Right_Subtree /= NULL THEN
            treeNode.ALL := treeNode.ALL.Right_Subtree.ALL;
         END IF;
      ELSE

      END IF;
   end Delete ;


   -- performs an In-Order traversal of a named
   -- binary tree
   procedure In_Order_Traverse (
         T : in     Tree) is

   begin
      IF T /= NULL THEN
         In_Order_Traverse (T.ALL.Left_Subtree);
         Visit (T.ALL.Stored);
         In_Order_Traverse (T.ALL.Right_Subtree);
      END IF;
   end In_Order_Traverse;

end Generic_Binary_Tree ;

