package body A is
   procedure Test_Call is
      function F1 (X1, X2 : Integer := 1; X3 : Integer := 3) return Integer;
      function F2 (X1 : Integer := 1; X2 : Integer := 2; X3, X4 : Integer := 4)
         return Integer;

      type F1Ptr is access function (X1, X2 : Integer; X3 : Integer := 3)
         return Integer;
      type F2Ptr is access function (X1, X2 : Integer; X3, X4 : Integer := 3)
         return Integer;

      X : Integer;

      P1 : constant F1Ptr := F1'Access;
      P2 : constant F2Ptr := F2'Access;
   begin
      X := F1 (X2 => 2, X3 => 3, X1 => 1);
      X := F1 (1, 2, 3);
      X := F1 (1, X3 => 3, X2 => 2);
      X := F1;
      X := F1 (1, 2, X3 => 3);
      X := F1 (1);
      X := F1 (X2 => 2);

      X := F2 (X3 => 3);
      X := F2;
   end Test_Call;

   procedure Test_Deref is
      type Int_Ptr is access all Integer;

      X : Integer;
      X_Ptr : Int_Ptr;
   begin
      X := X_Ptr.all;
   end Test_Deref;

   procedure Test_Field is
      type Rec is record
         X : Integer;
         Y : Integer;
      end record;

      type Rec2 is record
         R : Rec;
         Z : Integer;
      end record;

      function F return Rec;
      function F (A1, A2 : Integer) return Rec2;

      X : Integer;
      R1 : Rec;
      R2 : Rec2;
   begin
      X := R1.X;
      X := R1.Y;
      X := R2.R.X;
      X := R2.R.Y;
      X := R2.Z;
      X := F.X;
      X := F.Y;
      X := F (1, 2).R.X;
      X := F (1, 2).R.Y;
      X := F (1, 2).Z;
   end Test_Field;

   procedure Test_Implicit_Deref_Field is
      type Rec_Ptr;

      type Rec is record
         Value : Integer;
         Next : Rec_Ptr;
         Prev : access Rec;
      end record;

      type Rec_Ptr is access all Rec;


      R : Rec;
      X : Integer;
   begin
      X := R.Next.Prev.Next.all.Next.Value;
   end Test_Implicit_Deref_Field;

   procedure Test_Array_Index is
      type Arr is array (Integer range <>) of Integer;
      type Arr2D is array (Integer range <>, Integer range <>) of Integer;

      type Rec is record
         X : Integer;
         Y : Integer;
      end record;

      type Rec2 is record
         FArr : Arr (1 .. 10);
      end record;

      type ArrRec is array (Integer range <>) of Rec;

      subtype SmallInt is Integer range 1 .. 5;

      subtype Arr1_10 is Arr (1 .. 10);

      subtype ArrRec1_10 is ArrRec (1 .. 10);

      X : Integer;
      Arr1 : Arr (1 .. 10);
      Arr2 : Arr2D (1 .. 10, 1 .. 10);
      Arr3 : access Arr1_10;
      Arr4 : access ArrRec1_10;
      Arr5 : access Rec2;
      Arr6 : Arr (1 .. 5);
   begin
      X := Integer (42);

      X := Arr1 (8);
      X := Arr2 (8, 12);
      X := Arr4 (7).X;
      X := Arr5.FArr (8);

      Arr6 := Arr1 (1 .. 5);
      Arr6 := Arr1 (SmallInt);
      Arr6 := Arr1 (Integer range 1 .. 5);
   end Test_Array_Index;
end A;
