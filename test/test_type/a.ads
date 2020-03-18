package A is
   --  Ada RM 3.5 Scalar types

   --  Ada RM 3.5.1 Enumeration types
   type My_Enum is (Red, Blue, Green, Black, White, Yellow, Orange);

   --  Ada RM 3.5.2 Chacater types
   type My_Char_Enum is ('0', '1', None);

   --  Ada RM 3.5.3 Boolean types
   type My_Bool is new Boolean;

   --  Ada RM 3.5.4 Integer types
   --  Ada RM 3.5.4 (3)
   type My_Int is range -10 .. 10;

   --  Ada RM 3.5.4 (4)
   type My_Mod is mod 12345;

   --  Ada RM 3.5.6 Real types

   --  Ada RM 3.5.7 Floating point
   type My_Float is digits 8;
   type My_Float_1_10 is digits 8 range 1.0 .. 10.0;

   --  Ada RM 3.5.9 Fixed point types

   --  Ada RM 3.5.9 (3) Ordinary fixed point
   type My_Ordinary_Fixed is delta 0.01 range 1.0 .. 10.0;

   --  Ada RM 3.5.9 (4) Decimal fixed point
   type My_Decimal_Fixed is delta 0.01 digits 8;
   type My_Decimal_Fixed_1_10 is delta 0.01 digits 8 range 1.0 .. 10.0;

   --  Ada RM 3.6 Array types

   --  Ada RM 3.6 (3) Unconstrained array
   type Unconstrained_Array is array (Positive range <>) of Integer;
   type Unconstrained_Array_2D is array (Positive range <>, Natural range <>) of Integer;

   --  Ada RM 3.6 (4) Constrained array
   --  Ada RM 3.6 (6) range discrete_subtype_indication with range
   type Constrained_Array is array (Positive range 1 .. 10) of Integer;
   --  Ada RM 3.6 (6) range simple_expression .. simple_expression
   type Constrained_Array_2D is array (1 .. 10, 11 .. 20) of Integer;
   --  Ada RM 3.6 (6) range range_attribute_reference
   type Constrained_Array_Range is array (My_Int'Range) of Integer;
   --  Ada RM 3.6 (6) range discrete_subtype_indication with attribute_reference range
   type Constrained_Array_Range2 is array (Integer range Positive'Range) of Integer;

   --  Ada RM 3.6.3 String types
   type My_String is array (Positive range <>) of Character;

   --  Ada RM 3.8 Record types
   type My_Rec is record
      A, B : Integer;
   end record;

   --  Ada RM 3.8.1 Variant part
   type My_Rec_Variant (C1, C2 : My_Enum) is record
      A, B : Integer;
      case C1 is
         when Red =>
            C : Integer;
         when Blue | Green =>
            D : Integer;
         when others =>
            E : Integer;
            case C2 is
               when Green =>
                  F : Integer;
                  G : Integer;
               --when My_Enum range Black .. White => Not working for now (T302-029)
               --   H : Integer;
               when Yellow .. Orange =>
                  I : Integer;
               when others =>
                  null;
            end case;
      end case;
   end record;

   --  Ada RM 3.9 Tagged types
   type My_Tag is tagged record
      A, B : Integer;
   end record;

   --  Ada RM 3.9.1 Type extensions
   type My_Child is new My_Tag with record
      C, D : Integer;
   end record;

   --  Ada RM 3.9.4 Interface types
   type My_Interface is interface;

   --  Ada RM 3.10 Access types
   --  Ada RM 3.10 (3) access to object
   type My_Access_To_Object is access My_Int;

   --  Ada RM 3.10 (5) access to subprogram
   type My_Access_To_Subprogram1 is access procedure;
   type My_Access_To_Subprogram2 is access
      procedure (A, B : My_Int;
                 C : in My_Child;
                 D : in out My_Rec;
                 E : out My_Rec_Variant);
   type My_Access_To_Subprogram3 is access
      function (A, B : My_Int) return Constrained_Array;

   --  Ada RM 3.2.2 Subtype declarations
   --  Ada RM 3.2.2 (3/2)
   subtype My_Int_Subtype is My_Int;

   --  Ada RM 3.2.2 (6) scalar constraint, range constraint on discrete type
   --  Integer
   --  simple_expression .. simple_expression
   subtype My_Int_Subtype_Dot is My_Int range 1 .. 10;
   --  attribute_reference
   subtype My_Int_Subtype_Range is My_Int range My_Int_Subtype'Range;
   --  Mod
   --  simple_expression .. simple_expression
   subtype My_Mod_Subtype_Dot is My_Mod range 1 .. 10;
   --  attribute_reference
   subtype My_Mod_Subtype_Range is My_Mod range My_Mod_Subtype_Dot'Range;
   --  Enum
   --  simple_expression .. simple_expression
   subtype My_Enum_Subtype_Dot is My_Enum range Black .. Yellow;
   --  attribute_reference
   subtype My_Enum_Subtype_Range is My_Enum range My_Enum_Subtype_Dot'Range;
   --  Real
   --  simple_expression .. simple_expression
   subtype My_Fixed_Subtype_Dot is My_Ordinary_Fixed range 1.0 .. 5.0;
   --  attribute_reference
   subtype My_Fixed_Subtype_Range is My_Ordinary_Fixed
      range My_Fixed_Subtype_Dot'Range;

   --  Ada RM 3.2.2 (6) scalar constraint, digits constraint
   subtype My_Decimal_Subtype_Digits is My_Decimal_Fixed digits 4;
   --  with range_constraint simple_expression .. simple_expression
   subtype My_Decimal_Subtype_Digits_Dot is My_Decimal_Fixed digits 4
      range 1.0 .. 4.0;
   --  with range_constraint attribute_reference
   subtype My_Decimal_Subtype_Digits_Range is My_Decimal_Fixed digits 4
      range My_Decimal_Subtype_Digits_Dot'Range; 

   --  Ada RM 3.2.2 (6) scalar constraint, delta constraint
   subtype My_Ordinary_Subtype_Digits is My_Ordinary_Fixed delta 0.4;
   --  with range_constraint simple_expression .. simple_expression
   subtype My_Ordinary_Subtype_Digits_Dot is My_Ordinary_Fixed delta 0.4
      range 1.0 .. 4.0;
   --  with range_constraint attribute_reference
   subtype My_Ordinary_Subtype_Digits_Range is My_Ordinary_Fixed delta 0.4
      range My_Ordinary_Subtype_Digits_Dot'Range;
end A;
