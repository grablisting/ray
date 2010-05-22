with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Stacks_Generic;
--Assg2 acquires an unbounded expression, converts the expression to Reverse Polish Notation,
--and then evaluates the expression. It refuses to compute malformed arguments.
--Assg2 supports unbounded arguments, but not big numbers.
--Written by Ray Peters on February 14, 2010.
PROCEDURE Assg2 IS
   --Rename Strings Unbounded for easy referral
   PACKAGE Str RENAMES Ada.Strings.Unbounded;

   --Tokens are used in the output queue.
   TYPE TokenType IS (Number, Operator);
   TYPE Token(ObjectType : TokenType := Number) IS RECORD
      CASE ObjectType IS
         WHEN Number =>
            Value : Float := 0.0;
         WHEN Operator =>
            Op : Character;
      END CASE;
   END RECORD;

   PACKAGE CharacterStack IS NEW Stacks_Generic(StackElement => Character);
   PACKAGE OutputStack IS NEW Stacks_Generic(StackElement => Token);

   --Takes a LIFO Stack and flips it upside-down, dubbed queue
   --Displays and solves the queue (now FIFO) stack in RPN notation
   --Displays the final result
   PROCEDURE EvalRPN (Queue : IN OUT OutputStack.Stack) IS
      FlippedQueue : OutputStack.Stack;
      First, Second, Result : Token(Number);
      Sign : Token(Operator);
   BEGIN --EvalRPN
      WHILE NOT OutputStack.IsEmpty(Queue) LOOP
         CASE OutputStack.Top(Queue).ObjectType IS
            WHEN Number =>
               OutputStack.Pop(Queue, First);
               OutputStack.Push(FlippedQueue, First);
            WHEN Operator =>
               OutputStack.Pop(Queue, Sign);
               OutputStack.Push(FlippedQueue, Sign);
         END CASE;
      END LOOP;

      Ada.Text_IO.Put (Item => "RPN: ");
      LOOP
         CASE OutputStack.Top(FlippedQueue).ObjectType IS
            WHEN Number =>
               OutputStack.Pop(FlippedQueue, First);
               OutputStack.Push(Queue, First);
               Ada.Integer_Text_IO.Put (Integer(First.Value), Width => 1);
               Ada.Text_IO.Put (" ");
            WHEN Operator =>
               OutputStack.Pop(FlippedQueue, Sign);
               Ada.Text_IO.Put (Sign.Op & " ");

               OutputStack.Pop(Queue, Second);
               OutputStack.Pop(Queue, First);
               CASE Sign.Op IS
                  WHEN '-' =>
                     Result.Value := First.Value - Second.Value;
                  WHEN '+' =>
                     Result.Value := First.Value + Second.Value;
                  WHEN '/' =>
                     Result.Value := First.Value / Second.Value;
                  WHEN '*' =>
                     Result.Value := First.Value * Second.Value;
                  WHEN '^' =>
                     Result.Value := First.Value ** (integer(Second.Value));
                  WHEN OTHERS =>
                     NULL;
               END CASE;
               OutputStack.Push(Queue, Result);
            END CASE;
         EXIT WHEN OutputStack.IsEmpty(FlippedQueue);
      END LOOP;

      OutputStack.Pop(Queue, Result);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "Out: ");
      Ada.Integer_Text_IO.Put (Integer(Result.Value), Width => 1);
   END EvalRPN;

   --Builds a number from an Unbounded String, starting at a specific index.
   --Returns a new Index and the Number it built.
   PROCEDURE GetNum (Expression: IN OUT Str.Unbounded_String; AtIndex : IN OUT Integer; Found : IN OUT Float) IS
      Number : Float := 0.0;
      NextCh : Character;
   BEGIN --GetNum
      WHILE AtIndex <= Str.Length(Expression) LOOP
         NextCh := Str.Element(Expression, AtIndex);
         IF Ada.Characters.Handling.Is_Digit(NextCh) THEN
            Number := (Number * 10.0) + Float(Character'Pos(NextCh) - Character'Pos('0'));
         ELSE
            AtIndex := AtIndex - 1;
            EXIT;
         END IF;
         AtIndex := AtIndex + 1;
      END LOOP;
      Found := Number;
   END GetNum;

   --Returns the precedence of recognized operators.
   FUNCTION GetOpTier (Source : Character) RETURN Integer IS
   BEGIN --GetOpTier
      CASE Source IS
         WHEN '+' | '-' =>
            RETURN 1;
         WHEN '/' | '*' =>
            RETURN 2;
         WHEN '^' =>
            RETURN 3;
         WHEN OTHERS =>
            RETURN 0;
      END CASE;
   END GetOpTier;

   --Evaluates the user's expression on several planes: matching parenthesis,
   --correct tokentypes, and then evaluates successfully formed arguments.
   --Throws an error on bad-input.
   PROCEDURE Eval (Expression : IN OUT Str.Unbounded_String) IS
      ParenStack, OpStack : CharacterStack.Stack;
      Queue, FlippedQueue : OutputStack.Stack;
      NextChar, OpenParen : Character;
      NextNum : Token(Number);
      NextOp : Token(Operator);
      Index : Positive := 1;
      Balanced : Boolean := True;
   BEGIN --Eval
      WHILE Index <= Str.Length(Expression) AND THEN Balanced LOOP
         NextChar := Str.Element(Expression, Index);
         CASE NextChar IS
            WHEN '0'..'9' =>
               GetNum(Expression, Index, NextNum.Value);
               OutputStack.Push(Queue, NextNum);
            WHEN '+' | '-' | '*' | '/' =>
               IF NOT CharacterStack.IsEmpty(OpStack) THEN
                  WHILE NOT CharacterStack.IsEmpty(OpStack) AND THEN GetOpTier(NextChar) <= GetOpTier(CharacterStack.Top(OpStack)) LOOP
                     CharacterStack.Pop(OpStack, NextOp.Op);
                     OutputStack.Push(Queue, NextOp);
                  END LOOP;
               END IF;
               CharacterStack.Push(OpStack, NextChar);
            WHEN '^' =>
               CharacterStack.Push(OpStack, NextChar);
            WHEN '(' | '[' | '{' =>
               CharacterStack.Push(ParenStack, NextChar);
               CharacterStack.Push(OpStack, NextChar);
            WHEN ')' | ']' | '}' =>
               IF CharacterStack.IsEmpty(ParenStack) THEN
                  Balanced := False;
               ELSE
                  CharacterStack.Pop(ParenStack, OpenParen);
                  CASE NextChar IS
                     WHEN ')' =>
                        Balanced := OpenParen = '(';
                        LOOP
                           CharacterStack.Pop(OpStack, NextOp.Op);
                           EXIT WHEN NextOp.Op = '(';
                           OutputStack.Push(Queue, NextOp);
                        END LOOP;
                     WHEN ']' =>
                        Balanced := OpenParen = '[';
                        LOOP
                           CharacterStack.Pop(OpStack, NextOp.Op);
                           EXIT WHEN NextOp.Op = '[';
                           OutputStack.Push(Queue, NextOp);
                        END LOOP;
                     WHEN '}' =>
                        Balanced := OpenParen = '{';
                        LOOP
                           CharacterStack.Pop(OpStack, NextOp.Op);
                           EXIT WHEN NextOp.Op = '{';
                           OutputStack.Push(Queue, NextOp);
                        END LOOP;
                     WHEN OTHERS =>
                        NULL;
                  END CASE;
               END IF;
            WHEN OTHERS =>
               NULL;
         END CASE;
         Index := Index + 1;
      END LOOP;

      WHILE NOT CharacterStack.IsEmpty(OpStack) LOOP
         CharacterStack.Pop(OpStack, NextOp.Op);
         OutputStack.Push(Queue, NextOp);
      END LOOP;

      IF Balanced AND THEN CharacterStack.IsEmpty(ParenStack) THEN
         EvalRPN(Queue);
      ELSE
         Ada.Text_IO.Put (Item => "**Malformed argument**");
      END IF;
   END Eval;

   Expression, Input, Clean : Str.Unbounded_String;

   --Testcases: from the prompt, type  ">#" where # is the testcase digit
   Test0 : Str.Unbounded_String := Str.To_Unbounded_String("(3 + 4 * (3 / (8 - 5))) + (6 / 3)");
   Test1 : Str.Unbounded_String := Str.To_Unbounded_String("(6 + 4 * (4 / (8 - 4))) + (9 / 3)");
   Test2 : Str.Unbounded_String := Str.To_Unbounded_String("(12 + 4 * (21 / (8 - 1))) + (3 / 3)");
   Test3 : Str.Unbounded_String := Str.To_Unbounded_String("(23 + 14 * (16 / (8 * 1))) + (12 / 3)");
   Test4 : Str.Unbounded_String := Str.To_Unbounded_String("(3253 + 4 * (3 / (8 - 5))) + (6 / 3)");
   Test5 : Str.Unbounded_String := Str.To_Unbounded_String("(6 - 20 * 230 + 439 - 239 + 10 + 1)");
   Test6 : Str.Unbounded_String := Str.To_Unbounded_String("10 + 10");
   Test7 : Str.Unbounded_String := Str.To_Unbounded_String("(1 * 1 * (2 * 1 - 1) * 1 * 1)");
   Test8 : Str.Unbounded_String := Str.To_Unbounded_String("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3");

   Ch : Character;
BEGIN --Assg2
   LOOP
      Input := Clean;
      Expression := Clean;
      Ch := '0';

      Ada.Text_IO.Put (Item => "> ");

      WHILE NOT Ada.Text_IO.End_of_Line LOOP
         Ada.Text_IO.Get (Item => Ch);
         Str.Append(Expression, Ch);
      END LOOP;

      --Checks for testcase marker, if true then instantiates the case to Eval.
      IF Str.Element(Expression, 1) = '>' THEN
         CASE Str.Element(Expression, 2) IS
            WHEN '0' =>     Expression := Test0;
            WHEN '1' =>     Expression := Test1;
            WHEN '2' =>     Expression := Test2;
            WHEN '3' =>     Expression := Test3;
            WHEN '4' =>     Expression := Test4;
            WHEN '5' =>     Expression := Test5;
            WHEN '6' =>     Expression := Test6;
            WHEN '7' =>     Expression := Test7;
            WHEN '8' =>     Expression := Test8;
            WHEN 'E' =>     EXIT;
            WHEN OTHERS => NULL;
         END CASE;
      END IF;

      Ada.Text_IO.Put (Item => "In: ");
      Ada.Text_IO.Unbounded_IO.Put_Line (Item => Expression);

      Eval(Expression);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Skip_Line;
   END LOOP;
END Assg2;
