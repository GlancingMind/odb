with Ada.Text_IO;				use Ada.Text_IO;
use  Ada;

with ODB.Memory_Stream;				use ODB.Memory_Stream;
with ODB.Classes;				use ODB.Classes;
with Util.String_Array;				use Util.String_Array;
use  Util;

package body Person  is 

   type Handle is access all Person.Object;

   Class_ID : Natural := 0;
   D_Name   : constant Natural := 1;
   D_Zip    : constant Natural := 2;
   D_Street : constant Natural := 3;
   D_Used   : constant Natural := 4;

   -------------
   -- Factory --
   -------------
   function Factory return Persistent.Reference is
   begin
      return new Object;
   end;

   ---------------
   -- Serialize --
   ---------------
   procedure Serialize(
      Item   : in out Object;
      Header : in out Storage_Header.Object;
      S      : in Stream_IO.Stream_Access ) is
      -- when serializing an object we store the offset for each field in the 
      -- header which will be written out in front of the object it self.
   begin
      Register_Attribute( Header, D_Zip, Write_Offset( S ), Object'Tag );
      Natural'Output( S, Item.Zip );

      Register_Attribute( Header, D_Street, Write_Offset( S ), Object'Tag );
      Unbounded_String'Output( S, Item.Street );      

      Register_Attribute( Header, D_Name, Write_Offset( S ), Object'Tag );
      Unbounded_String'Output( S, Item.Name );

      Register_Attribute( Header, D_Used, Write_Offset( S ), Object'Tag );
      Natural'Output( S, Item.Used );
   end Serialize;

   -----------------
   -- Deserialize --
   -----------------
   procedure Deserialize(
      Item   : in out Object;
      Header : in out Storage_Header.Object;
      S      : in Stream_IO.Stream_Access ) is
      Field  : String_Array.Handle := Attributes( Header ); 	
   begin
      for i in Field'Range loop
         declare
           ID     : Natural;
           Offset : Natural;
	   Name   : constant String := To_String( Field(i) );	
	 begin
            ID := Classes.Attribute( Object'Tag, Name );
--	    Text_IO.Put_Line("ID" & Natural'Image(ID) & ", " & Name );

	    if ID /= 0 then    
               Offset := Storage_Header.Lookup_Attribute( Header, Name );
               Read_Offset( S, Offset );

               case ID is
                  when D_Name  =>
                     Item.Name := Unbounded_String'Input(S);

                  when D_Street =>
	             Item.Street := Unbounded_String'Input(S);

                  when D_Zip =>
                     Item.Zip := Positive'Input(S);

                  when D_Used =>
                     Item.Used := Natural'Input(S);

                  when Others =>
                     null;
               end case;
            end if;
	 exception
	    when Storage_Header.Unknown_Attribute =>
	       null;
         end;
      end loop;

      String_Array.Free( Field );
   end Deserialize;

   ------------
   -- Create --
   ------------
   function Create( 
      Name   : in String;
      Street : in String ) return Reference is
      Result : Reference := Lookup_Object( Name ); 
      H      : Handle := Handle( Result );
   begin
      if Result = null then
         Put_Line("New instance of person created" );
         Result := new Object;
	 Name_Object( Result, Name );
      
         H := Handle( Result );

         H.Name   := To_Unbounded_String(Name);
         H.Street := To_Unbounded_String(Street);
         H.Zip    := 9999;
      end if;
      return Result;
   end Create;

   ----------
   -- Name --
   ----------
   procedure Name(
      This  : in Reference;
      Value : in String ) is 
      H     : Handle := Handle(This);
   begin
      H.Name := To_Unbounded_String( Value );
   end Name;

   ------------
   -- Street --
   ------------
   procedure Street(
      This  : in Reference;
      Value : in String ) is 
      H     : Handle := Handle(This);
   begin
      H.Street := To_Unbounded_String( Value );
   end Street;

   ---------
   -- Zip --
   ---------
   procedure Zip( 
      This  : in Reference;
      Value : in Natural ) is
      H     : Handle := Handle(This);
   begin
      H.Zip := Value;
   end Zip;

   procedure Increment_Used(
      This : in Reference ) is 
      H    : Handle := Handle(This);
   begin
      H.Used := H.Used + 1;
   end Increment_Used;



   -------------
   -- Display --
   -------------
   procedure Display(
      This : in Reference ) is 
      H    : Handle := Handle(This);
   begin
      Put_Line("Used   :" & Natural'Image( H.Used ) );
      Put_Line("Name   : " & To_String( H.Name ) );
      Put_Line("Street : " & To_String( H.Street ) );
   end Display;

begin
   Class_ID := Classes.Register_Factory( Object'Tag, Factory'Access );

   Classes.Attribute( Class_ID, "D_Name",    	D_Name );
   Classes.Attribute( Class_ID, "D_Street",     D_Street );
   Classes.Attribute( Class_ID, "D_Zip", 	D_Zip );
   Classes.Attribute( Class_ID, "D_Used", 	D_Used );
end Person;

