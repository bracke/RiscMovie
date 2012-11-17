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

-- @brief OS events and types. Abstract task definition.
-- $Author$
-- $Date$
-- $Revision$

with Kernel;                     use Kernel;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Ada.Unchecked_Conversion;

package RASCAL.OS is

   type Event_Type is (Wimp,Message,Toolbox);
   type Event_Listener (K : Event_Type) is abstract tagged record
   Kind : Event_Type := K;
   end record;

   type Event_Pointer is access all Event_Listener'Class;

   procedure Handle (The : in Event_Listener) is abstract;

   type Byte is mod 2**8;

   type Wimp_Handle_Type is new Integer;
   type Icon_Handle_Type is new Integer;
   
   type Reason_Event_Code_Type is new System.Unsigned_Types.Unsigned;

   Reason_Event_NullReason             : constant Reason_Event_Code_Type := 0;
   Reason_Event_RedrawWindow           : constant Reason_Event_Code_Type := 1;
   Reason_Event_OpenWindow             : constant Reason_Event_Code_Type := 2;
   Reason_Event_CloseWindow            : constant Reason_Event_Code_Type := 3;
   Reason_Event_PointerLeavingWindow   : constant Reason_Event_Code_Type := 4;
   Reason_Event_PointerEnteringWindow  : constant Reason_Event_Code_Type := 5;
   Reason_Event_MouseClick             : constant Reason_Event_Code_Type := 6;
   Reason_Event_UserDrag               : constant Reason_Event_Code_Type := 7;
   Reason_Event_KeyPressed             : constant Reason_Event_Code_Type := 8;
   Reason_Event_MenuSelection          : constant Reason_Event_Code_Type := 9;
   Reason_Event_ScrollRequest          : constant Reason_Event_Code_Type := 10;
   Reason_Event_LoseCaret              : constant Reason_Event_Code_Type := 11;
   Reason_Event_GainCaret              : constant Reason_Event_Code_Type := 12;
   Reason_Event_PollWordNonZero        : constant Reason_Event_Code_Type := 13;

   Reason_Event_UserMessage            : constant Reason_Event_Code_Type := 17;
   Reason_Event_UserMessageRecorded    : constant Reason_Event_Code_Type := 18;
   Reason_Event_UserMessageAcknowledge : constant Reason_Event_Code_Type := 19;

   Reason_Event_ToolboxEvent           : constant Reason_Event_Code_Type := 16#200#;

   type Wimp_EventListener (E : Reason_Event_Code_Type;
                            W : Wimp_Handle_Type;
                            I : Icon_Handle_Type) is abstract new Event_Listener(Wimp) with
   record
      Event_Code : Reason_Event_Code_Type := E;
      Window     : Wimp_Handle_Type  := W;
      Icon       : Icon_Handle_Type  := I;
   end record;

   type Message_Event_Code_Type is new System.Unsigned_Types.Unsigned;
   
   Message_Event_Quit                   : constant Message_Event_Code_Type := 0;
   Message_Event_DataSave               : constant Message_Event_Code_Type := 1;
   Message_Event_DataSaveAck            : constant Message_Event_Code_Type := 2;
   Message_Event_DataLoad               : constant Message_Event_Code_Type := 3;
   Message_Event_DataLoadAck            : constant Message_Event_Code_Type := 4;
   Message_Event_DataOpen               : constant Message_Event_Code_Type := 5;
   Message_Event_RAMFetch               : constant Message_Event_Code_Type := 6;
   Message_Event_RAMTransmit            : constant Message_Event_Code_Type := 7;
   
   Message_Event_PreQuit                : constant Message_Event_Code_Type := 8;
   Message_Event_PaletteChange          : constant Message_Event_Code_Type := 9;
   Message_Event_SaveDesktop            : constant Message_Event_Code_Type := 10;
   Message_Event_DeviceClaim            : constant Message_Event_Code_Type := 11;
   Message_Event_DeviceInUse            : constant Message_Event_Code_Type := 12;
   Message_Event_DataSaved              : constant Message_Event_Code_Type := 13;
   Message_Event_Shutdown               : constant Message_Event_Code_Type := 14;
   
   Message_Event_FilerOpenDir           : constant Message_Event_Code_Type := 16#400#;
   Message_Event_FilerCloseDir          : constant Message_Event_Code_Type := 16#401#;
   Message_Event_FilerOpenDirAt         : constant Message_Event_Code_Type := 16#402#;
   Message_Event_FilerSelectionDirectory: constant Message_Event_Code_Type := 16#403#;
   Message_Event_FilerAddSelection      : constant Message_Event_Code_Type := 16#404#;
   Message_Event_FilerAction            : constant Message_Event_Code_Type := 16#405#;
   Message_Event_FilerControlAction     : constant Message_Event_Code_Type := 16#406#;
   Message_Event_FilerSelection         : constant Message_Event_Code_Type := 16#407#;
   
   Message_Event_AlarmSet               : constant Message_Event_Code_Type := 16#500#;
   Message_Event_AlarmGoneOff           : constant Message_Event_Code_Type := 16#501#;
   Message_Event_HelpEnable             : constant Message_Event_Code_Type := 16#504#;
   
   Message_Event_Notify                 : constant Message_Event_Code_Type := 16#40040#;
   Message_Event_MenuWarning            : constant Message_Event_Code_Type := 16#400c0#;
   Message_Event_ModeChange             : constant Message_Event_Code_Type := 16#400c1#;
   
   Message_Event_TaskInitialise         : constant Message_Event_Code_Type := 16#400c2#;
   Message_Event_TaskCloseDown          : constant Message_Event_Code_Type := 16#400c3#;
   Message_Event_SlotSize               : constant Message_Event_Code_Type := 16#400c4#;
   Message_Event_SetSlot                : constant Message_Event_Code_Type := 16#400c5#;
   Message_Event_TaskNameRq             : constant Message_Event_Code_Type := 16#400c6#;
   Message_Event_TaskNameIs             : constant Message_Event_Code_Type := 16#400c7#;
   Message_Event_TaskStarted            : constant Message_Event_Code_Type := 16#400c8#;
   
   Message_Event_MenusDeleted           : constant Message_Event_Code_Type := 16#400c9#;
   Message_Event_Iconize                : constant Message_Event_Code_Type := 16#40c10#;
   Message_Event_IconizeAt              : constant Message_Event_Code_Type := 16#400D0#;
   Message_Event_WindowInfo             : constant Message_Event_Code_Type := 16#40c11#;
   Message_Event_WindowClosed           : constant Message_Event_Code_Type := 16#40c12#;
   Message_Event_FontChanged            : constant Message_Event_Code_Type := 16#400CF#;

   Message_Event_PrintFile              : constant Message_Event_Code_Type := 16#80140#;
   Message_Event_WillPrint              : constant Message_Event_Code_Type := 16#80141#;
   Message_Event_PrintSave              : constant Message_Event_Code_Type := 16#80142#;
   Message_Event_PrintInit              : constant Message_Event_Code_Type := 16#80143#;
   Message_Event_PrintError             : constant Message_Event_Code_Type := 16#80144#;
   Message_Event_PrintTypeOdd           : constant Message_Event_Code_Type := 16#80145#;
   Message_Event_PrintTypeKnown         : constant Message_Event_Code_Type := 16#80146#;
   Message_Event_SetPrinter             : constant Message_Event_Code_Type := 16#80147#;
   Message_Event_PSPrinterQuery         : constant Message_Event_Code_Type := 16#8014c#;
   Message_Event_PSPrinterAck           : constant Message_Event_Code_Type := 16#8014d#;
   Message_Event_PSPrinterModified      : constant Message_Event_Code_Type := 16#8014e#;
   Message_Event_PSPrinterDefaults      : constant Message_Event_Code_Type := 16#8014f#;
   Message_Event_PSPrinterDefaulted     : constant Message_Event_Code_Type := 16#80150#;
   Message_Event_PSPrinterNotPS         : constant Message_Event_Code_Type := 16#80151#;
   Message_Event_ResetPrinter           : constant Message_Event_Code_Type := 16#80152#;
   Message_Event_PSIsFontPrintRunning   : constant Message_Event_Code_Type := 16#80153#;
   
   Message_Event_HelpRequest            : constant Message_Event_Code_Type := 16#502#;
   Message_Event_HelpReply              : constant Message_Event_Code_Type := 16#503#;
   Message_Event_Help_Word              : constant Message_Event_Code_Type := 16#43B00#;
   
   Message_Event_TW_Input               : constant Message_Event_Code_Type := 16#808C0#;
   Message_Event_TW_Output              : constant Message_Event_Code_Type := 16#808C1#;
   Message_Event_TW_Ego                 : constant Message_Event_Code_Type := 16#808C2#;
   Message_Event_TW_Morio               : constant Message_Event_Code_Type := 16#808C3#;
   Message_Event_TW_Morite              : constant Message_Event_Code_Type := 16#808C4#;
   Message_Event_TW_NewTask             : constant Message_Event_Code_Type := 16#808C5#;
   Message_Event_TW_Suspend             : constant Message_Event_Code_Type := 16#808C6#;
   Message_Event_TW_Resume              : constant Message_Event_Code_Type := 16#808C7#;

   Message_Event_PlugInQuit             : constant Message_Event_Code_Type := 16#50D80#;
   Message_Event_PlugInQuitContinue     : constant Message_Event_Code_Type := 16#50D81#;
   Message_Event_PlugInQuitAbort        : constant Message_Event_Code_Type := 16#50D82#;
   Message_Event_OpenConfigWindow       : constant Message_Event_Code_Type := 16#50D83#;
                                        
   Message_Event_Bugz_Query             : constant Message_Event_Code_Type := 16#53B80#;
   Message_Event_Bugz_BugzFile          : constant Message_Event_Code_Type := 16#53B81#;
                                        
   Message_Event_OLE_FileChanged        : constant Message_Event_Code_Type := 16#80E1E#;
   Message_Event_OLEOpenSession         : constant Message_Event_Code_Type := 16#80E21#;
   Message_Event_OLEOpenSessionAck      : constant Message_Event_Code_Type := 16#80E22#;
   Message_Event_OLECloseSession        : constant Message_Event_Code_Type := 16#80E23#;

   Message_Event_ConfiX                 : constant Message_Event_Code_Type := 16#40D50#;
   
   Message_Event_StrongEDModeFileChanged : constant Message_Event_Code_Type := 16#43b06#;
   Message_Event_StrongEDInsertText      : constant Message_Event_Code_Type := 16#43b04#;

   Message_Event_InetSuite_Open_URL     : constant Message_Event_Code_Type := 16#4AF80#;
   
   type Message_EventListener (E : Message_Event_Code_Type) is abstract new Event_Listener(Message) with
   record
   Event_Code : Message_Event_Code_Type := E;
   end record;

   type Message_Event_Header is
   record
   Size       : System.Unsigned_Types.Unsigned;
   Sender     : Integer;
   MyRef      : System.Unsigned_Types.Unsigned;
   YourRef    : System.Unsigned_Types.Unsigned;
   Event_Code : Message_Event_Code_Type;
   end record;
   pragma Convention (C, Message_Event_Header);
   
   type ToolBox_Event_Code_Type is new System.Unsigned_Types.Unsigned;

   Toolbox_Event_Error                         : constant ToolBox_Event_Code_Type := 16#44EC0#;
   Toolbox_Event_ObjectAutoCreated             : constant ToolBox_Event_Code_Type := 16#44EC1#;
   Toolbox_Event_ObjectDeleted                 : constant ToolBox_Event_Code_Type := 16#44EC2#;
   
   Toolbox_Event_Menu_AboutToBeShown           : constant ToolBox_Event_Code_Type := 16#828C0#;
   Toolbox_Event_Menu_HasBeenHidden            : constant ToolBox_Event_Code_Type := 16#828C1#;
   Toolbox_Event_Menu_SubMenu                  : constant ToolBox_Event_Code_Type := 16#828C2#;
   Toolbox_Event_Menu_Selection                : constant ToolBox_Event_Code_Type := 16#828C3#;

   Toolbox_Event_ColourDbox_AboutToBeShown     : constant ToolBox_Event_Code_Type := 16#829C0#;
   Toolbox_Event_ColourDbox_DialogueCompleted  : constant ToolBox_Event_Code_Type := 16#829C1#;
   Toolbox_Event_ColourDbox_ColourSelected     : constant ToolBox_Event_Code_Type := 16#829C2#;
   Toolbox_Event_ColourDbox_ColourChanged      : constant ToolBox_Event_Code_Type := 16#829C3#;
   Toolbox_Event_ColourMenu_AboutToBeShown     : constant ToolBox_Event_Code_Type := 16#82980#;
   Toolbox_Event_ColourMenu_HasBeenHidden      : constant ToolBox_Event_Code_Type := 16#82981#;
   Toolbox_Event_ColourMenu_Selection          : constant ToolBox_Event_Code_Type := 16#82982#;
  
   Toolbox_Event_DCS_AboutToBeShown            : constant ToolBox_Event_Code_Type := 16#82A80#;
   Toolbox_Event_DCS_Discard                   : constant ToolBox_Event_Code_Type := 16#82A81#;
   Toolbox_Event_DCS_Save                      : constant ToolBox_Event_Code_Type := 16#82A82#;
   Toolbox_Event_DCS_DialogueCompleted         : constant ToolBox_Event_Code_Type := 16#82A83#;
   Toolbox_Event_DCS_Cancel                    : constant ToolBox_Event_Code_Type := 16#82A84#;

   Toolbox_Event_FileInfo_AboutToBeShown       : constant ToolBox_Event_Code_Type := 16#82AC0#;
   Toolbox_Event_FileInfo_DialogueCompleted    : constant ToolBox_Event_Code_Type := 16#82AC1#;
 
   Toolbox_Event_FontDbox_AboutToBeShown       : constant ToolBox_Event_Code_Type := 16#82A00#;
   Toolbox_Event_FontDbox_DialogueCompleted    : constant ToolBox_Event_Code_Type := 16#82A01#;
   Toolbox_Event_FontDbox_ApplyFont            : constant ToolBox_Event_Code_Type := 16#82A02#;
   Toolbox_Event_FontMenu_AboutToBeShown       : constant ToolBox_Event_Code_Type := 16#82A40#;
   Toolbox_Event_FontMenu_HasBeenHidden        : constant ToolBox_Event_Code_Type := 16#82A41#;
   Toolbox_Event_FontMenu_Selection            : constant ToolBox_Event_Code_Type := 16#82A42#;

   Toolbox_Event_Iconbar_Clicked               : constant ToolBox_Event_Code_Type := 16#82900#;
   Toolbox_Event_Iconbar_SelectAboutToBeShown  : constant ToolBox_Event_Code_Type := 16#82901#;
   Toolbox_Event_Iconbar_AdjustAboutToBeShown  : constant ToolBox_Event_Code_Type := 16#82902#;

   Toolbox_Event_PrintDbox_AboutToBeShown      : constant ToolBox_Event_Code_Type := 16#82B00#;
   Toolbox_Event_PrintDbox_DialogueCompleted   : constant ToolBox_Event_Code_Type := 16#82B01#;
   Toolbox_Event_PrintDbox_SetupAboutToBeShown : constant ToolBox_Event_Code_Type := 16#82B02#;
   Toolbox_Event_PrintDbox_Save                : constant ToolBox_Event_Code_Type := 16#82B03#;
   Toolbox_Event_PrintDbox_SetUp               : constant ToolBox_Event_Code_Type := 16#82B04#;
   Toolbox_Event_PrintDbox_Print               : constant ToolBox_Event_Code_Type := 16#82B05#;

   Toolbox_Event_ProgInfo_AboutToBeShown       : constant ToolBox_Event_Code_Type := 16#82B40#;
   Toolbox_Event_ProgInfo_DialogueCompleted    : constant ToolBox_Event_Code_Type := 16#82B41#;
   Toolbox_Event_ProgInfo_LaunchWebPage        : constant ToolBox_Event_Code_Type := 16#82B42#;

   Toolbox_Event_Quit_AboutToBeShown           : constant ToolBox_Event_Code_Type := 16#82A90#;
   Toolbox_Event_Quit_Quit                     : constant ToolBox_Event_Code_Type := 16#82A91#;
   Toolbox_Event_Quit_DialogueCompleted        : constant ToolBox_Event_Code_Type := 16#82A92#;
   Toolbox_Event_Quit_Cancel                   : constant ToolBox_Event_Code_Type := 16#82A93#;
 
   Toolbox_Event_SaveAs_AboutToBeShown         : constant ToolBox_Event_Code_Type := 16#82BC0#;
   Toolbox_Event_SaveAs_DialogueCompleted      : constant ToolBox_Event_Code_Type := 16#82BC1#;
   Toolbox_Event_SaveAs_SaveToFile             : constant ToolBox_Event_Code_Type := 16#82BC2#;
   Toolbox_Event_SaveAs_FillBuffer             : constant ToolBox_Event_Code_Type := 16#82BC3#;
   Toolbox_Event_SaveAs_SaveCompleted          : constant ToolBox_Event_Code_Type := 16#82BC4#;

   Toolbox_Event_Scale_AboutToBeShown          : constant ToolBox_Event_Code_Type := 16#82C00#;
   Toolbox_Event_Scale_DialogueCompleted       : constant ToolBox_Event_Code_Type := 16#82C01#;
   Toolbox_Event_Scale_ApplyFactor             : constant ToolBox_Event_Code_Type := 16#82C02#;

   Toolbox_Event_Window_AboutToBeShown         : constant ToolBox_Event_Code_Type := 16#82880#;
   Toolbox_Event_ActionButton_Selected         : constant ToolBox_Event_Code_Type := 16#82881#;
   Toolbox_Event_OptionButton_StateChanged     : constant ToolBox_Event_Code_Type := 16#82882#;
   Toolbox_Event_RadioButton_StateChanged      : constant ToolBox_Event_Code_Type := 16#82883#;
   Toolbox_Event_DisplayField_ValueChanged     : constant ToolBox_Event_Code_Type := 16#82884#;
   Toolbox_Event_WritableField_ValueChanged    : constant ToolBox_Event_Code_Type := 16#82885#;
   Toolbox_Event_Slider_ValueChanged           : constant ToolBox_Event_Code_Type := 16#82886#;
   Toolbox_Event_Draggable_DragStarted         : constant ToolBox_Event_Code_Type := 16#82887#;
   Toolbox_Event_Draggable_DragEnded           : constant ToolBox_Event_Code_Type := 16#82888#;
   Toolbox_Event_PopUp_AboutToBeShown          : constant ToolBox_Event_Code_Type := 16#8288B#;
   Toolbox_Event_Adjuster_Clicked              : constant ToolBox_Event_Code_Type := 16#8288C#;
   Toolbox_Event_NumberRange_ValueChanged      : constant ToolBox_Event_Code_Type := 16#8288D#;
   Toolbox_Event_StringSet_ValueChanged        : constant ToolBox_Event_Code_Type := 16#8288E#;
   Toolbox_Event_StringSet_AboutToBeShown      : constant ToolBox_Event_Code_Type := 16#8288F#;
   Toolbox_Event_Window_HasBeenHidden          : constant ToolBox_Event_Code_Type := 16#82890#;
   ToolBox_Event_Quit                          : constant ToolBox_Event_Code_Type := 16#82A91#;
   Toolbox_Event_ScrollList_Selection          : constant ToolBox_Event_Code_Type := 16#140181#;

   Toolbox_Event_Scrollbar_PositionChanged     : constant ToolBox_Event_Code_Type := 16#140183#;

   Toolbox_Event_ToolAction_ButtonClicked      : constant ToolBox_Event_Code_Type := 16#140140#;

   TreeView_SWIBase                            : constant ToolBox_Event_Code_Type := 16#140280#;
   TreeView_EventBase                          : constant ToolBox_Event_Code_Type := TreeView_SWIBase;
   Toolbox_Event_TreeViewNodeSelected          : constant ToolBox_Event_Code_Type := TreeView_EventBase + 0;
   Toolbox_Event_TreeViewNodeExpanded          : constant ToolBox_Event_Code_Type := TreeView_EventBase + 1;
   Toolbox_Event_TreeViewNodeRenamed           : constant ToolBox_Event_Code_Type := TreeView_EventBase + 2;
   Toolbox_Event_TreeViewNodeDataRequired      : constant ToolBox_Event_Code_Type := TreeView_EventBase + 3;
   Toolbox_Event_TreeViewNodeDragged           : constant ToolBox_Event_Code_Type := TreeView_EventBase + 4;

   type Object_ID    is new Integer;
   type Component_ID is new Integer;

   subtype Error_Code_Type is Integer;

   Error_Escape                 : constant Error_Code_Type := 16#11#;
   Error_Bad_mode               : constant Error_Code_Type := 16#19#;
   Error_Is_adir                : constant Error_Code_Type := 16#A8#;
   Error_Types_dont_match       : constant Error_Code_Type := 16#AF#;
   Error_Bad_rename             : constant Error_Code_Type := 16#B0#;
   Error_Bad_copy               : constant Error_Code_Type := 16#B1#;
   Error_Outside_file           : constant Error_Code_Type := 16#B7#;
   Error_Access_violation       : constant Error_Code_Type := 16#BD#;
   Error_Too_many_open_files    : constant Error_Code_Type := 16#C0#;
   Error_Not_open_for_update    : constant Error_Code_Type := 16#C1#;
   Error_File_open              : constant Error_Code_Type := 16#C2#;
   Error_Object_locked          : constant Error_Code_Type := 16#C3#;
   Error_Already_exists         : constant Error_Code_Type := 16#C4#;
   Error_Bad_file_name          : constant Error_Code_Type := 16#CC#;
   Error_File_not_found         : constant Error_Code_Type := 16#D6#;
   Error_Syntax                 : constant Error_Code_Type := 16#DC#;
   Error_Channel                : constant Error_Code_Type := 16#DE#;
   Error_End_of_file            : constant Error_Code_Type := 16#DF#;
   Error_Buffer_Overflow        : constant Error_Code_Type := 16#E4#;
   Error_Bad_filing_system_name : constant Error_Code_Type := 16#F8#;
   Error_Bad_key                : constant Error_Code_Type := 16#FB#;
   Error_Bad_address            : constant Error_Code_Type := 16#FC#;
   Error_Bad_string             : constant Error_Code_Type := 16#FD#;
   Error_Bad_command            : constant Error_Code_Type := 16#FE#;
   Error_Bad_mac_val            : constant Error_Code_Type := 16#120#;
   Error_Bad_var_nam            : constant Error_Code_Type := 16#121#;
   Error_Bad_var_type           : constant Error_Code_Type := 16#122#;
   Error_Var_no_room            : constant Error_Code_Type := 16#123#;
   Error_Var_cant_find          : constant Error_Code_Type := 16#124#;
   Error_Var_too_long           : constant Error_Code_Type := 16#125#;
   Error_Redirect_fail          : constant Error_Code_Type := 16#140#;
   Error_Stack_full             : constant Error_Code_Type := 16#141#;
   Error_Bad_hex                : constant Error_Code_Type := 16#160#;
   Error_Bad_expr               : constant Error_Code_Type := 16#161#;
   Error_Bad_bra                : constant Error_Code_Type := 16#162#;
   Error_Stk_oflo               : constant Error_Code_Type := 16#163#;
   Error_Miss_opn               : constant Error_Code_Type := 16#164#;
   Error_Miss_opr               : constant Error_Code_Type := 16#165#;
   Error_Bad_bits               : constant Error_Code_Type := 16#166#;
   Error_Str_oflo               : constant Error_Code_Type := 16#167#;
   Error_Bad_itm                : constant Error_Code_Type := 16#168#;
   Error_Div_zero               : constant Error_Code_Type := 16#169#;
   Error_Bad_base               : constant Error_Code_Type := 16#16A#;
   Error_Bad_numb               : constant Error_Code_Type := 16#16B#;
   Error_Numb_too_big           : constant Error_Code_Type := 16#16C#;
   Error_Bad_claim_num          : constant Error_Code_Type := 16#1A1#;
   Error_Bad_release            : constant Error_Code_Type := 16#1A2#;
   Error_Bad_dev_no             : constant Error_Code_Type := 16#1A3#;
   Error_Bad_dev_vec_rel        : constant Error_Code_Type := 16#1A4#;
   Error_Bad_env_number         : constant Error_Code_Type := 16#1B0#;
   Error_Cant_cancel_quit       : constant Error_Code_Type := 16#1B1#;
   Error_Ch_dynam_cao           : constant Error_Code_Type := 16#1C0#;
   Error_Ch_dynam_not_all_moved : constant Error_Code_Type := 16#1C1#;
   Error_Apl_wspace_in_use      : constant Error_Code_Type := 16#1C2#;
   Error_Ram_fs_unchangeable    : constant Error_Code_Type := 16#1C3#;
   Error_Oscli_long_line        : constant Error_Code_Type := 16#1E0#;
   Error_Oscli_too_hard         : constant Error_Code_Type := 16#1E1#;
   Error_Rc_exc                 : constant Error_Code_Type := 16#1E2#;
   Error_Sys_heap_full          : constant Error_Code_Type := 16#1E3#;
   Error_Buff_overflow          : constant Error_Code_Type := 16#1E4#;
   Error_Bad_time               : constant Error_Code_Type := 16#1E5#;
   Error_No_such_swi            : constant Error_Code_Type := 16#1E6#;
   Error_Unimplemented          : constant Error_Code_Type := 16#1E7#;
   Error_Out_of_range           : constant Error_Code_Type := 16#1E8#;
   Error_No_oscli_specials      : constant Error_Code_Type := 16#1E9#;
   Error_Bad_parameters         : constant Error_Code_Type := 16#1EA#;
   Error_Arg_repeated           : constant Error_Code_Type := 16#1EB#;
   Error_Bad_read_sys_info      : constant Error_Code_Type := 16#1EC#;
   Error_Cdat_stack_overflow    : constant Error_Code_Type := 16#2C0#;
   Error_Cdat_buffer_overflow   : constant Error_Code_Type := 16#2C1#;
   Error_Cdat_bad_field         : constant Error_Code_Type := 16#2C2#;
   Error_Cant_start_application : constant Error_Code_Type := 16#600#;

   -- Toolbox errors
   Error_Tool_Action_Out_of_Memory      : constant Error_Code_Type := 16#80E920#;
   Error_Tool_Action_Cant_Create_Icon   : constant Error_Code_Type := 16#80E921#;
   Error_Tool_Action_Cant_Create_Object : constant Error_Code_Type := 16#80E922#;


   Exception_Tool_Action_Out_of_Memory     : Exception;
   Exception_Tool_Action_Cant_Create_Icon  : Exception;
   Exception_Tool_Action_Cant_Create_Object: Exception;
   
   Exception_Escape                        : Exception;
   Exception_Bad_mode                      : Exception;
   Exception_Is_adir                       : Exception;
   Exception_Types_dont_match              : Exception;
   Exception_Bad_rename                    : Exception;
   Exception_Bad_copy                      : Exception;
   Exception_Outside_file                  : Exception;
   Exception_Access_violation              : Exception;
   Exception_Too_many_open_files           : Exception;
   Exception_Not_open_for_update           : Exception;
   Exception_File_open                     : Exception;
   Exception_Object_locked                 : Exception;
   Exception_Already_exists                : Exception;
   Exception_Bad_file_name                 : Exception;
   Exception_File_not_found                : Exception;
   Exception_Syntax                        : Exception;
   Exception_Channel                       : Exception;
   Exception_End_of_file                   : Exception;
   Exception_Buffer_Overflow               : Exception;
   Exception_Bad_filing_system_name        : Exception;
   Exception_Bad_key                       : Exception;
   Exception_Bad_address                   : Exception;
   Exception_Bad_string                    : Exception;
   Exception_Bad_command                   : Exception;
   Exception_Bad_mac_val                   : Exception;
   Exception_Bad_var_nam                   : Exception;
   Exception_Bad_var_type                  : Exception;
   Exception_Var_no_room                   : Exception;
   Exception_Var_cant_find                 : Exception;
   Exception_Var_too_long                  : Exception;
   Exception_Redirect_fail                 : Exception;
   Exception_Stack_full                    : Exception;
   Exception_Bad_hex                       : Exception;
   Exception_Bad_expr                      : Exception;
   Exception_Bad_bra                       : Exception;
   Exception_Stk_oflo                      : Exception;
   Exception_Miss_opn                      : Exception;
   Exception_Miss_opr                      : Exception;
   Exception_Bad_bits                      : Exception;
   Exception_Str_oflo                      : Exception;
   Exception_Bad_itm                       : Exception;
   Exception_Div_zero                      : Exception;
   Exception_Bad_base                      : Exception;
   Exception_Bad_numb                      : Exception;
   Exception_Numb_too_big                  : Exception;
   Exception_Bad_claim_num                 : Exception;
   Exception_Bad_release                   : Exception;
   Exception_Bad_dev_no                    : Exception;
   Exception_Bad_dev_vec_rel               : Exception;
   Exception_Bad_env_number                : Exception;
   Exception_Cant_cancel_quit              : Exception;
   Exception_Ch_dynam_cao                  : Exception;
   Exception_Ch_dynam_not_all_moved        : Exception;
   Exception_Apl_wspace_in_use             : Exception;
   Exception_Ram_fs_unchangeable           : Exception;
   Exception_Oscli_long_line               : Exception;
   Exception_Oscli_too_hard                : Exception;
   Exception_Rc_exc                        : Exception;
   Exception_Sys_heap_full                 : Exception;
   Exception_Buff_overflow                 : Exception;
   Exception_Bad_time                      : Exception;
   Exception_No_such_swi                   : Exception;
   Exception_Unimplemented                 : Exception;
   Exception_Out_of_range                  : Exception;
   Exception_No_oscli_specials             : Exception;
   Exception_Bad_parameters                : Exception;
   Exception_Arg_repeated                  : Exception;
   Exception_Bad_read_sys_info             : Exception;
   Exception_Cdat_stack_overflow           : Exception;
   Exception_Cdat_buffer_overflow          : Exception;
   Exception_Cdat_bad_field                : Exception;
   Exception_Cant_start_application        : Exception;
   Exception_Unknown_Error                 : Exception;

   procedure Raise_Error (Error : OSError_Access);

   --
   -- Block filled in by the toolbox on WimpPoll
   --
   type ToolBox_Id_Block_Type is
   record
      Ancestor_Id       : Object_ID;
      Ancestor_Component: Component_ID;
      Parent_Id         : Object_ID;
      Parent_Component  : Component_ID;   
      Self_Id           : Object_ID;
      Self_Component    : Component_ID;
   end record;
   pragma Convention (C, ToolBox_Id_Block_Type);

   type ToolBox_Id_Block_Pointer is access ToolBox_Id_Block_Type;
   
   type Toolbox_EventListener (E : ToolBox_Event_Code_Type;
                               O : Object_ID;
                               C : Component_ID) is abstract new Event_Listener(Toolbox) with
   record      
   Event_Code   : ToolBox_Event_Code_Type   := E;
   Object       : Object_ID            := O;
   Component    : Component_ID         := C;
   ID_Block     : ToolBox_Id_Block_Pointer;
   end record;

   type Toolbox_UserEventListener (E : ToolBox_Event_Code_Type;
                                   O : Object_ID;
                                   C : Component_ID) is abstract new

   Toolbox_EventListener (E,O,C) with
   record
   Event : Event_Pointer;
   end record;

   type Toolbox_Event_Header is
   record
   Size             : System.Unsigned_Types.Unsigned;
   Reference_Number : Integer;
   Event_Code       : System.Unsigned_Types.Unsigned;
   Flags            : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Toolbox_Event_Header);

   Wimp_Block_Size        : constant integer := 63;
   type Wimp_Block_Type is array (0 .. Wimp_Block_Size) of integer;
   type Wimp_Block_Pointer is access Wimp_Block_Type;

   Number_Of_Messages       : integer  := 0;
   Max_Number_Of_Messages   : constant integer := 63;
   type Messages_List_Type is array (0 .. Max_Number_Of_Messages) of integer;
   type Messages_List_Pointer is access Messages_List_Type;

   type System_Sprite_Pointer is new Address;
   type Messages_Control_Block_Type is array (1 .. 6) of System.Unsigned_Types.Unsigned;
   type Messages_Handle_Type is access Messages_Control_Block_Type;

   type Wimp_Colour is new integer range 0..15;
   type Toolbox_Colour is new integer range -1..16;
      
end RASCAL.OS;
