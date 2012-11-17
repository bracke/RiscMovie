--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;
with RASCAL.Utility;        use RASCAL.Utility;
with RASCAL.Memory;         use RASCAL.Memory;

with Kernel;                use Kernel;
with Interfaces.C;          use Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Reporter;

package body RASCAL.ToolboxMenu is

   Toolbox_ObjectMiscOp : constant Interfaces.C.Unsigned :=16#44EC6#;

   --

   procedure Set_Available (Menu      : in Object_ID;
                            Available : in String;
                            Items     : in out Natural;
                            Event     : in Integer := 16#828C3#;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      ID      : Component_ID := 0;
      Entries : Unbounded_String := U(Available);
      Single  : Unbounded_String;
      Pos     : Integer;
   begin
      if Items > 0 then
         Remove_Entries (Menu,0,Items);
      end if;
      
      Pos := Index (Entries,",");
      while Pos > 0 loop
         Single := U(Slice (Entries,1,Pos-1));
         Entries := U(Slice (Entries,Pos+1,Length(Entries)));
         Add_Last_Entry (Menu,S(Single),ID,"",Event);
         ID := ID + 1;
         Pos := Index (Entries,",");
      end loop;
      Add_Last_Entry (Menu,S(Entries),ID,"",Event);
      Items := Natural(ID+1);
   end Set_Available;

   --

   procedure Add_Last_Entry (Menu          : in Object_ID;
                             Name          : in String;
                             Id            : in Component_ID;
                             Help          : in String := "";
                             Click_Event   : in integer := 16#828C3#;
                             Click_Show    : in Object_ID := Object_ID'Val(0);
                             Submenu_Event : in integer := 16#828C2#;
                             Submenu_Show  : in Object_ID := Object_ID'Val(0);
                             Flags         : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Line     : Menu_Entry_Type;
      Name_0   : String := Name & ASCII.NUL;
      Help_0   : String := Help & ASCII.NUL;
   begin
      Line.Flags           := 0;
      Line.ComponentID     := Id;
      Line.Name            := Name_0'Address;
      Line.Max_Text        := Name_0'Length;
      Line.Click_Show      := integer(Click_Show);
      Line.Submenu_Show    := integer(Submenu_Show);
      Line.Submenu_Event   := Submenu_Event;
      Line.Click_Event     := Click_Event;
      Line.Help_Message    := Help_0'Address;
      Line.Max_Entry_Help  := Help_0'Length;

      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(integer(Menu));
      Register.R(2) := 20; -- menu_add_entry
      Register.R(3) := -2;
      Register.R(4) := Adr_To_Int(Line'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Add_Last_Entry: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Add_Last_Entry;

   --

   procedure Add_After_Entry (Menu         : in Object_ID;
                             Name          : in String;
                             Id            : in Component_ID;
                             Next_ID       : in Component_ID;
                             Help          : in String := "";
                             Click_Event   : in integer := 16#828C3#;
                             Click_Show    : in Object_ID := Object_ID'Val(0);
                             Submenu_Event : in integer := 16#828C2#;
                             Submenu_Show  : in Object_ID := Object_ID'Val(0);
                             Flags         : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;

      Line      : Menu_Entry_Type;

      Name_0    : String := Name & ASCII.NUL;
      Help_0    : String := Help & ASCII.NUL;
   begin
      Line.Flags           := 0;
      Line.ComponentID     := Id;
      Line.Name            := Name_0'Address;
      Line.Max_Text        := Name_0'Length;
      Line.Click_Show      := integer(Click_Show);
      Line.Submenu_Show    := integer(Submenu_Show);
      Line.Submenu_Event   := Submenu_Event;
      Line.Click_Event     := Click_Event;
      Line.Help_Message    := Help_0'Address;
      Line.Max_Entry_Help  := Help_0'Length;

      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(integer(Menu));
      Register.R(2) := 20; -- menu_add_entry
      Register.R(3) := int(Next_ID);
      Register.R(4) := Adr_To_Int(Line'Address);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Add_After_Entry: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Add_After_Entry;

   --

   function Get_Click_Event (Menu      : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 15;
      Register.R(3) := int(Component);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Click_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Toolbox_Event_Code_Type(Register.R(4));
      
   end Get_Click_Event;

   --

   function Get_Click_Show (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 13;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Click_Show: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Click_Show;

   --

   function Get_Click_Show_Type (Menu      : in Object_ID;
                                 Component : in Component_ID;
                                 Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Show_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Flag           : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 13;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Click_Show_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Flag := integer(Register.R(1));

      if "And" (Flag,1) = 1 then
         return Transient;
      else
         return Persistent;
      end if;
   end Get_Click_Show_Type;
   
   --

   function Get_Entry_Help (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return string is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 19;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := integer(Register.R(5)) + 1;
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := 0;
         Register.R(1) := int(Menu);
         Register.R(2) := 19;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Help: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Entry_Help;

   --

   function Get_Entry_Sprite (Menu      : in Object_ID;
                              Component : in Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) return string is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 7;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Sprite: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := integer(Register.R(5)) + 1;
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Menu);
         Register.R(2) := 7;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Sprite: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Entry_Sprite;

   --

   function Get_Entry_Text (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Buffer_Size : Size_T;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 5;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_SIze := Size_T(Register.R(5));
      declare
         Buffer : Char_Array(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Menu);
         Register.R(2) := 5;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);         
         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxMenu.Get_Entry_Text: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return To_Ada(Buffer);
      end;
   end Get_Entry_Text;

   --

   function Get_Fade (Menu      : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Fade_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 3;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Fade: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      if integer(Register.R(0)) /= 0 then
         return Faded;
      else
         return Unfaded;
      end if;
   end Get_Fade;

   --

   function Get_Height (Menu      : in Object_ID;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 22;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Height: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_Height;

   --

   function Get_Help (Menu      : in Object_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return string is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 17;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := integer(Register.R(4)) + 1;
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Menu);
         Register.R(2) := 17;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxMenu.Add_Last_Entry: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Help;

   --

   function Get_Sub_Menu_Event (Menu      : in Object_ID;
                                Component : in Component_ID;
                                Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 11;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Sub_Menu_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Toolbox_Event_Code_Type(Register.R(0));
   end Get_Sub_Menu_Event;

   --

   function Get_Sub_Menu_Show (Menu      : in Object_ID;
                               Component : in Component_ID;
                               Flags     : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 9;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Sub_Menu_Show: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Sub_Menu_Show;

   --

   function Get_Tick (Menu      : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Tick_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 1;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Tick: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      if integer(Register.R(0)) /= 0 then
         return Ticked;
      else
         return UnTicked;
      end if;
   end Get_Tick;

   --

   function Get_Title (Menu      : in Object_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return string is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 25;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := integer(Register.R(4)) + 1;
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Menu);
         Register.R(2) := 25;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxMenu.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

   --

   function Get_Width (Menu  : in Object_ID;
                       Flags : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 23;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Get_Width: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_Width;

   --

   procedure Remove_Entry (Menu : in Object_ID;
                           ID   : in integer;
                           Flags: in System.Unsigned_Types.Unsigned := 0)   is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(integer(Menu));
      Register.R(2) := 21;
      Register.R(3) := int(ID);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Remove_Entry: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Remove_Entry;

   --

   procedure Remove_Entries (Menu : in Object_ID;
                             Nr   : in integer;
                             Start: in integer :=0;
                             Flags: in System.Unsigned_Types.Unsigned := 0)  is
   begin
      for i in Start..(Start+Nr-1) loop
         Remove_Entry(Menu,i);
      end loop;
   end Remove_Entries;

   --

   procedure Set_Click_Event (Menu        : in Object_ID;
                              Component   : in Component_ID;
                              Event       : in Toolbox_Event_Code_Type;
                              Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 14;
      Register.R(3) := int(Component);
      Register.R(4) := int(Event);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Click_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Click_Event;

   --

   procedure Set_Click_Show (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Object      : in Object_ID;
                             Show        : in Menu_Show_Type := Persistent;
                             Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 12;
      Register.R(3) := int(Component);
      Register.R(4) := int(Object);
      Register.R(5) := int(Menu_Show_Type'Pos(Show));
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Click_Show: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Click_Show;

   --

   procedure Set_Entry_Help (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Help        : in string;
                             Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Null_Help : String := Help & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 18;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Help'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Entry_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Entry_Help;

   --

   procedure Set_Entry_Sprite (Menu        : in Object_ID;
                               Component   : in Component_ID;
                               Sprite      : in string;
                               Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Null_Sprite : String := Sprite & ASCII.NUL;

   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 6;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Sprite'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Entry_Sprite: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Entry_Sprite;

   --

   procedure Set_Entry_Text (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Text        : in string;
                             Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Null_Text : String := Text & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 4;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Text'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Entry_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Entry_Text;

   --

   procedure Set_Fade (Menu        : in Object_ID;
                       Component   : in Component_ID;
                       Fade        : in Menu_Fade_Type;
                       Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 2;
      Register.R(3) := int(Component);
      Register.R(4) := int(Menu_Fade_Type'Pos(Fade));
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Fade: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Fade;

   --

   procedure Set_Help (Menu        : in Object_ID;
                       Help        : in string;
                       Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Help      : String := Help & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 16;
      Register.R(3) := Adr_To_Int(Null_Help'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Help;

   --

   procedure Set_Sub_Menu_Event (Menu        : in Object_ID;
                                 Component   : in Component_ID;
                                 Event       : in Toolbox_Event_Code_Type;
                                 Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 10;
      Register.R(3) := int(Component);
      Register.R(4) := int(Event);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Sub_Menu_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Sub_Menu_Event;

   --

   procedure Set_Sub_Menu_Show (Menu        : in Object_ID;
                                Component   : in Component_ID;
                                Object      : in Object_ID;
                                Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 8;
      Register.R(3) := int(Component);
      Register.R(4) := int(Object);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Sub_Menu_Show: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Sub_Menu_Show;

   --

   procedure Set_Tick (Menu        : in Object_ID;
                       Component   : in Component_ID;
                       Tick        : in Menu_Tick_Type;
                       Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 0;
      Register.R(3) := int(Component);
      Register.R(4) := int(Menu_Tick_Type'Pos(Tick));
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Tick: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Tick;

   --

   procedure Set_Title (Menu         : in Object_ID;
                        Title        : in string;
                        Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Title      : String := Title & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Menu);
      Register.R(2) := 24;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxMenu.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

end RASCAL.ToolboxMenu;
