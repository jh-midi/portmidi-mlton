(* for MLton
 no sig all is exported
 *)

structure PortmidiFFI = struct
 type hndl = MLton.Pointer.t
 type pointer =  MLton.Pointer.t
 type pm_stream_ptr = MLton.Pointer.t ref
 type pm_stream = MLton.Pointer.t
 type pm_event = Int64.int (* struct PmEvent {int32_t message, int32_t timestamp} *)
 type buffer = MLton.Pointer.t
		     
    val Pt_Started = _import "Pt_Started" : unit ->  int;
    val Pt_Start = _import "Pt_Start" : int * pointer * pointer -> int;
    val Pt_Stop = _import "Pt_Stop" : unit ->  int;
    val Pt_Time = _import "Pt_Time" : unit ->  int;
    val Pt_Sleep  = _import "Pt_Sleep" :int ->  unit;

    val Pm_Initialize = _import "Pm_Initialize" : unit ->  int;

    val Pm_Terminate = _import "Pm_Terminate" : unit ->  int;

    val Pm_HasHostError = _import "Pm_HasHostError" : pointer -> int;

    val Pm_GetHostErrorText = _import "Pm_GetHostErrorText" : string ref * int -> unit;

    val Pm_GetErrorText = _import "Pm_GetErrorText" : int -> pointer;

    val Pm_GetDeviceInfo  = _import "Pm_GetDeviceInfo" : int -> pointer;

    val Pm_CountDevices = _import "Pm_CountDevices" : unit -> int;
 				       
    val Pm_OpenInput =
         _import "Pm_OpenInput":pm_stream_ptr * int * pointer * int * pointer * pointer -> int; 				  
    val Pm_OpenOutput =
	_import "Pm_OpenOutput":pm_stream_ptr * int * pointer * int * pointer * pointer * int -> int;

    val Pm_CreateVirtualInput = _import "Pm_CreateVirtualInput": string * string * pointer -> int;
						 
    val Pm_CreateVirtualOutput = _import "Pm_CreateVirtualOutput": string * string * pointer -> int;

    val Pm_DeleteVirtualDevice = _import "Pm_DeleteVirtualDevice" :int -> int;
						 
  					
    val Pm_SetFilter = _import "Pm_SetFilter" : pm_stream * int -> int;

    val Pm_SetChannelMask = _import "Pm_SetChannelMask" : pm_stream * int ->  int;
					     
    val Pm_Abort = _import "Pm_Abort" : pointer -> int; 

    val Pm_Close = _import "Pm_Close" : pointer -> int;

    val Pm_Synchronise = _import "Pm_Synchronize" : pointer -> int;

    val Pm_Read = _import "Pm_Read" : pm_stream * buffer * int -> int;

    val Pm_Poll = _import "Pm_Poll" : pointer -> int;

    val Pm_Write = _import "Pm_Write" :  pm_stream * buffer * int -> int;
    				   
    val Pm_WriteShort = _import "Pm_WriteShort" : pointer * int * int -> int;
					
    val Pm_WriteSysEx = _import "Pm_WriteSysEx" : pointer * int *  Word8.word array  -> int; (* string for unsigned char* *)

    val malloc = _import "malloc" : int -> pointer;
end
				 


