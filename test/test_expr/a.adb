package body A is
   procedure Test_Call is
      function F1 (X1, X2 : Integer := 1; X3 : Integer := 3) return Integer
         with Import;
      function F2 (X1 : Integer := 1; X2 : Integer := 2; X3, X4 : Integer := 4)
         return Integer with Import;

      type F1Ptr is access function (X1, X2 : Integer; X3 : Integer := 3)
         return Integer;
      type F2Ptr is access function (X1, X2 : Integer; X3, X4 : Integer := 3)
         return Integer;

      X : Integer;

      P1 : F1Ptr;
      P2 : F2Ptr;
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

      X := P1 (X2 => 2, X1 => 1);

      X := P2 (X2 => 2, X1 => 1, X4 => 4);
   end Test_Call;

   procedure Test_Deref is
      type Int_Ptr is access all Integer;

      X : aliased Integer;
      X_Ptr : Int_Ptr := X'Access;
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

      function F return Rec with Import;
      function F (A1, A2 : Integer) return Rec2 with Import;

      X : Integer;
      R1 : Rec := F;
      R2 : Rec2 := F (1, 2);
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
      type Rec;

      type Rec_Ptr is access Rec;

      type Rec is record
         Value : Integer;
         Next : Rec_Ptr;
         Prev : access Rec;
      end record;

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

      type accessArr1_10 is access Arr1_10;

      type ArrAccessArr is array (Integer range <>) of accessArr1_10;

      X : Integer;
      Arr1 : Arr (1 .. 10);
      Arr2 : Arr2D (1 .. 10, 1 .. 10);
      Arr3 : access Arr1_10;
      Arr4 : access ArrRec1_10;
      Arr5 : access Rec2;
      Arr6 : Arr (1 .. 5);
      Arr7 : ArrAccessArr (1 .. 10);
   begin
      X := Integer (42);

      X := Arr1 (8);
      X := Arr2 (8, 9);
      X := Arr4 (7).X;
      X := Arr5.FArr (8);

      Arr6 := Arr1 (1 .. 5);
      Arr6 := Arr1 (SmallInt);
      Arr6 := Arr1 (Integer range 1 .. 5);
      Arr6 := Arr1 (SmallInt'Range);
      Arr6 := Arr1 (Arr6'Range);
      Arr6 := Arr1 (Arr2'Range(1));

      X := Arr6(3 .. 5)(3);

      X := Arr7 (2 .. 4)(3)(10);
   end Test_Array_Index;

   procedure Test_Membership_Expr is
      subtype SmallInt is Integer range 1 .. 5;

      X : Integer;
      Y : Integer;
      Z : Integer;

      B : Boolean;
   begin
      B := X in Y | Z;
      B := X not in SmallInt;
      B := X in SmallInt'Range;
   end Test_Membership_Expr;

   procedure Test_Qualified_Expr is
      subtype SmallInt is Integer range 1 .. 5;

      X : SmallInt;
   begin
      X := SmallInt'(1);
   end Test_Qualified_Expr;

   type MyInt is new Integer;

   function "&" (X, Y : MyInt) return MyInt is
   begin
      return X * 10 + Y;
   end "&";

   procedure Test_Operator_Symbol is
      X, Y : MyInt := 42;
   begin
      X := "&" (X, Y);
   end Test_Operator_Symbol;

   procedure Test_Raise_Expression is
      A : exception;

      X : Integer;
   begin
      X := raise Program_Error;
      X := raise A;
   end Test_Raise_Expression;

   procedure Test_Op is
      X, Y : MyInt;
      B : Boolean;
   begin
      X := X & Y;
      X := X + Y;
      X := -X;
      X := +X;
      X := abs X;

      B := X = 10 and then Y > 10;
   end Test_Op;

   procedure Test_Record_Aggregate is
      type Rec is record
         X, Y : Integer;
         B : Boolean;
      end record;

      R : Rec;
   begin
      R := (X | Y => 42, B => True);
      R := (X | Y => 42, B => <>);
   end Test_Record_Aggregate;

   procedure Test_Array_Aggregate is
      type Arr is array (1 .. 10) of Integer;
      subtype Int3_5 is Integer range 3 .. 5;

      A : Arr;
   begin
      A := (1 .. 5 | 6 => 42, others => 13);
      A := (1, 2, 3, 4, others => 13);
      A := (Integer range 1 .. 2 => 12, Int3_5 => 21, others => 212);
   end Test_Array_Aggregate;

   procedure Test_Allocator is
      type Rec is record
         X, Y : Integer;
         B : Boolean;
      end record;

      type Arr is array (1 .. 10) of Integer;
      subtype Int1_5 is Integer range 3 .. 5;

      R : access Rec;
      A : access Arr;
   begin
      R := new Rec;
      A := new Arr;
      R := new Rec'(X => 1, Y => 2, B => False);
      A := new Arr'(others => 42);
   end Test_Allocator;

   procedure Test_Case_Expression is
      subtype Int1_5 is Integer range 1 .. 5;

      X : Integer;
   begin
      X := (case 5 is
              when Int1_5 => 42,
              when 12 | 21 => 35,
              when others => 31);
   end Test_Case_Expression;

   procedure Test_Quantified is
      type Arr is array (1 .. 10) of Integer;

      A : Arr;

      B : Boolean;
   begin
      B := (for some I in A'Range => A (I) = 42);
      B := (for all I in A'Range => A (I) = 42);
      B := (for some I in 1 .. 10 => A (I) = 42);
      B := (for all I in Integer => A (I) = 42);
      B := (for some I of A => A (I) = 42);
      B := (for all E of A => E = 42);
   end Test_Quantified;

   procedure Test_Attribute is
      type Arr is array (1 .. 10) of Integer;
      type MyInt is new Integer range 1 .. 10;

      A : Arr;

      X : Integer;
   begin
      X := A'First + A'Last + A'Length;

      X := Arr'First + MyInt'Last + A'Length;
   end Test_Attribute;
end A;
