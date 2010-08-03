-- Lab 7 creates a deck of 52 unique cards inside of an array
-- It then "shuffles" the deck and deals 5 cards to the user
-- and the user can ask for a new hand as they wish

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

PROCEDURE Lab7 IS
   -- information concerning ranks
   TYPE Ranks IS (Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace);
   PACKAGE Ranks_IO IS NEW Ada.Text_IO.Enumeration_IO (Enum => Ranks);

   -- information concerning suits
   TYPE Suits IS (Diamonds, Clubs, Hearts, Spades);
   PACKAGE Suits_IO IS NEW Ada.Text_IO.Enumeration_IO (Enum => Suits);

   TYPE Card IS RECORD -- individual card records
      Rank: Ranks;
      Suit: Suits;
   END RECORD;

   ShuffleCount : Integer := 70; -- number of shuffles
   max : Integer := 5; -- Handsize maximum

   -- restrict the deck and handsize based on master variables
   SUBTYPE DeckRange IS Integer RANGE 0..51;
   SUBTYPE HandSize IS Integer RANGE 1..max;

   -- create arrays for both the deck and the hand of card records
   TYPE DeckArray IS ARRAY (DeckRange) OF Card;
   TheDeck : DeckArray;

   TYPE HandArray IS ARRAY (HandSize) OF Card;
   TheHand : HandArray;

   -- random number generators
   PACKAGE RandomCard IS NEW Ada.Numerics.Discrete_Random(DeckRange);
   G : RandomCard.Generator;
   I, J : DeckRange;

   -- procedure to fill the deck array
   PROCEDURE FillDeck IS
      pos : Integer := DeckArray'First;
   BEGIN
      FOR s IN Suits'Pos(Suits'First)..Suits'Pos(Suits'Last)LOOP
         FOR r IN Ranks'Pos(Ranks'First)..Ranks'Pos(Ranks'Last) LOOP
            TheDeck(pos).Rank := Ranks'Val(r);
            TheDeck(pos).Suit := Suits'Val(s);
            pos := pos + 1;
         END LOOP;
      END LOOP;
   END FillDeck;

   TempCard : Card;
   Go : Character;
   Count : Integer := 1;

BEGIN -- Lab7
   FillDeck; -- build deck

   -- greeting and aesthetic line break
   Ada.Text_IO.Put (Item => "-------------------------------------");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Random Hand Generator, begin? (y/n) ");
   Ada.Text_IO.Get (Item => Go);
   Ada.Text_IO.New_Line;

   -- Loop for as long as the user wants to play
   WHILE NOT (Go = 'n') LOOP
      RandomCard.Reset(G); -- reset my random number generator

      FOR y IN 1..ShuffleCount LOOP
         I := RandomCard.Random(G); -- get a random number in the deckrange
         J := RandomCard.Random(G); -- get another random number in the deckrange

         IF I /= J THEN -- change cards as long as I and J are not equivalent
            TempCard := TheDeck(I);
            TheDeck(I) := TheDeck(J);
            TheDeck(J) := TempCard;
         END IF;

      END LOOP;

      FOR z IN HandSize LOOP -- build handarray after shuffle
         TheHand(z) := TheDeck(z);
      END LOOP;

      -- aesthetic breaks and # of hands dealt counter
      Ada.Text_IO.Put (Item => "-------------------------------------");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "-------------------------------------");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "Hand #");
      Ada.Integer_Text_IO.Put (Item => Count, Width => 1);
      Ada.Text_IO.Put (Item => ". Dealing a new ");
      Ada.Integer_Text_IO.Put (Item => max, Width => 1);
      Ada.Text_IO.Put (Item => " card hand...");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "-------------------------------------");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "-------------------------------------");
      Ada.Text_IO.New_Line;

      -- display handarray to user
      FOR x IN HandSize LOOP
         Ranks_IO.Put (Item => TheHand(x).Rank);
         Ada.Text_IO.Put (Item => " of ");
         Suits_IO.Put (Item => TheHand(x).Suit);
         Ada.Text_IO.New_Line;
      END LOOP;

      Count := Count + 1; -- number of hands dealt counter

      -- ask the user if they want to repeat
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Item => "Deal again? (y/n) ");
      Ada.Text_IO.Get (Item => Go);
   END LOOP;

   -- aesthetic break
   Ada.Text_IO.Put (Item => "-------------------------------------");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "-------------------------------------");
   Ada.Text_IO.New_Line;

   -- farewell
   Ada.Text_IO.Put (Item => "Thanks for playing... Farewell!");
   Ada.Text_IO.New_Line;

END Lab7;
