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

-- @brief Collects information about the system - OS and hardware.
-- $Author$
-- $Date$
-- $Revision$

with System.Unsigned_Types;      use System.Unsigned_Types;

package RASCAL.SystemInfo is

   unknown_cache_state : Exception;

   type Cache_Status_Type is (On,Off,W,IW);
   type Version_Info_Type is (Version_Name,Part_Number,Build_Date,Dealer_Name,Registered_To,Registered_Address);
   type Memory_Info_Type is (DRAM,VRAM,ROM,IO,SoftROM);
   type CPU_Type is (Old,ARM6_7,StrongARM,New_32bitonly);

   -- You must tell the OS when a code area changes (OS_SynchroniseCodeAreas)
   Code_Feature_Syncronise : unsigned := 1;
   -- Enabling then disabling interrupts does *not* allow them a chance to occur.
   Code_Feature_Interrupts : unsigned := 2;
   -- Hardware vectors are only readable in 32 bit mode.
   Code_Feature_Vectors    : unsigned := 4;
   -- When storing PC, PC+8 is stored (not PC+12)
   Code_Feature_PC8        : unsigned := 8;
   -- Data aborts occur with 'full early' timing
   Code_Feature_FullEarly  : unsigned := 16;

   -- 32 bit RISC OS
   Code_Feature_32bitOS    : unsigned := 64;
   -- 26 bit mode is unavailable on this platform
   Code_Feature_No26bit    : unsigned := 128;

   --
   -- Returns cache status.
   --
   function Get_CacheStatus return Cache_Status_Type;

   --
   -- Returns the version number of the window manager * 100. Returns 200 with RISC OS older than 3.00
   --
   function Get_WindowManager_Version return Integer;

   --
   -- Returns OS declaration.
   --
   function Get_OS_Declaration return String;

   --
   -- Return information about the OS. Returns an empty string on ancient OS versions.
   --
   function Get_OS_Info (Info : in Version_Info_Type := Version_Name) return String;

   --
   -- Return the size of the memory in bytes. Returns 0 on OS versions older than 3.5
   --
   function Get_Memory_Size (Info : in Memory_Info_Type) return Integer;

   --
   -- Returns the machine id. Returns 0,0 if no suitable chip is fitted to the computer.
   --
   procedure Get_Machine_ID (Low  : out Integer;
                             High : out Integer);

   --
   -- Returns the machine id as a string. Returns '00' if no suitable chip is fitted to the computer.
   --
   function Get_Machine_ID return String;

   --
   -- Returns CPU type.
   --
   function Get_CPU return CPU_Type;
   
   --
   -- Returns ethernet address - 0,0 for none.
   --
   procedure Get_Ethernet_Address (Low  : out Integer;
                                   High : out Integer);

   --
   -- Returns ethernet address as a string - '00' for none.
   --
   function Get_Ethernet_Address return String;

   --
   -- Print system info into Reporter window.
   --
   procedure Report;

private
end RASCAL.SystemInfo;