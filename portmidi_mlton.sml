(* raw interface to port midi *)
structure PortmidiFFI = struct
 type pointer =  MLton.Pointer.t
 type pm_stream_ptr = MLton.Pointer.t ref
 type pm_stream = MLton.Pointer.t
(* struct PmEvent {int32_t message => 4 x word8, int32_t timestamp => 4 x word8} *)
 type pm_event1 = Word8.word array
 type pm_event = int array
 type buffer =  pm_event
		     
    val Pt_Started = _import "Pt_Started" : unit ->  int;
    val Pt_Start = _import "Pt_Start" : int * pointer * pointer -> int;
    val Pt_Stop = _import "Pt_Stop" : unit ->  int;
    val Pt_Time = _import "Pt_Time" : unit ->  int;
    val Pt_Sleep  = _import "Pt_Sleep" :int ->  unit;

    val Pm_Initialize = _import "Pm_Initialize" : unit ->  int;

    val Pm_Terminate = _import "Pm_Terminate" : unit ->  int;

    val Pm_HasHostError = _import "Pm_HasHostError" : pointer -> int;

    val Pm_GetHostErrorText = _import "Pm_GetHostErrorText" : string * int -> unit;

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
  					
    val Pm_SetFilter = _import "Pm_SetFilter" : pm_stream * word -> int;

    val Pm_SetChannelMask = _import "Pm_SetChannelMask" : pm_stream * word ->  int;
					     
    val Pm_Abort = _import "Pm_Abort" : pm_stream -> int; 

    val Pm_Close = _import "Pm_Close" : pm_stream -> int;

    val Pm_Synchronise = _import "Pm_Synchronize" : pm_stream -> int;

    val Pm_Read = _import "Pm_Read" : pm_stream * buffer * int -> int;
    val My_Pm_Read1 = _import "Pm_Read" : pm_stream * pm_event1 * int -> int; (* direct conversion word8 *)

    val Pm_Poll = _import "Pm_Poll" : pm_stream -> int;

    val Pm_Write = _import "Pm_Write" :  pm_stream * buffer * int -> int;
    val My_Pm_Write1 = _import "Pm_Write" :  pm_stream * pm_event1 * int -> int;  (* direct write word8 *)
    				   
    val Pm_WriteShort = _import "Pm_WriteShort" : pm_stream * int * int -> int;
					
    val Pm_WriteSysEx = _import "Pm_WriteSysEx" : pm_stream * int *  Word8.word array  -> int; 

 end
				 
(* convert cstring to sml string *)
signature C_STRING = sig
   type t
 
   val isNull: t -> bool
   val size: t -> int
   val sub: t * int -> char
   val toString: t -> string
end

structure CString : C_STRING = struct
   type t = MLton.Pointer.t
	     
   fun isNull s = s =  MLton.Pointer.null

   fun sub (s, i) = Byte.byteToChar ( MLton.Pointer.getWord8 (s, i))
      
   fun size s = let
      fun loop i = if #"\000" = sub (s, i) then i else loop (i + 1)
   in
      loop 0
   end
      
   fun toString s =
      if isNull s then
         raise Fail "CString.toString"
      else
         CharVector.tabulate (size s, fn i => sub (s, i))

end
				   
(* easy library interface  to portmidi *)
structure Portmidi =
struct


open PortmidiFFI 
	 
val mem_null = MLton.Pointer.null
		    
type PmDeviceInfo =
     {
       structVersion : int,
       interf : string,
       name : string,
       input : bool,
       output : bool,
       opened : bool,
       is_virtual : bool,
       id: int (* for use with getDeviceId  *)
     }

val countDevices  = Pm_CountDevices

fun getDeviceInfo deviceID = 
    if deviceID < (countDevices())
       andalso not (Pm_GetDeviceInfo deviceID = mem_null)
    then 
	let val dev_info = Pm_GetDeviceInfo deviceID
	    val struct_version = MLton.Pointer.getInt32(dev_info,0)
	    val interf = CString.toString (MLton.Pointer.getPointer(dev_info,1))
	    val name = CString.toString (MLton.Pointer.getPointer(dev_info,2))
	    val input = MLton.Pointer.getInt32(dev_info,6)
	    val output = MLton.Pointer.getInt32(dev_info,7)
	    val opened = MLton.Pointer.getInt32(dev_info,8)
	    val is_virtual = MLton.Pointer.getInt32(dev_info,9)
    in
	SOME {
	  structVersion = struct_version,
	  interf= interf,
	  name = name,
	  input =   (input = 1) ,
	  output =  (output = 1),
	  opened =  (opened = 1),
	  is_virtual =  (is_virtual = 1),
	  id=deviceID
	}
    end
    else NONE

			  
	 

(* 
here are vals to be evalued at run time
then don't convert to function
*)

val initialize  =  Pm_Initialize 
val terminate  =  Pm_Terminate 

fun hasHostError stream = (Pm_HasHostError stream) = 1

fun getHostErrorText taille = let
    val ar_text = List.tabulate (256,(fn x => #"\000"))
    val er_txt = String.implode ar_text
    val _ = Pm_GetHostErrorText( er_txt, taille)
in
    er_txt
end

							  
fun getErrorText errnum =  CString.toString (Pm_GetErrorText errnum)
  
type PmStream =  MLton.Pointer.t ref 		 

(*****************************************************)		   
fun close stream =  Pm_Close stream
			   
exception DeviceNotFound
fun listDevices () = let
    val _ = initialize ()
    val count = countDevices ()
    val iota = List.tabulate (count,fn x =>x)
in
    List.map (fn x => let val info = getDeviceInfo x
		      in
			  case info of
			      SOME info' => info'
			     | NONE => raise DeviceNotFound
	     end) iota
end

(* show one device to human *)			 
fun showDevice (dev : PmDeviceInfo ) =
   	let   fun fmt_bool lab = StringCvt.padRight #" " 6 ( Bool.toString (lab dev))
	      val input = fmt_bool #input
	      val output = fmt_bool #output 
	      val opened = fmt_bool #opened
	      val is_virtual = fmt_bool #is_virtual
	      val id = StringCvt.padLeft #" " 2  (Int.toString (#id dev))
	      val to_show = [StringCvt.padRight #" " 28 (#name dev),id,input,output,opened,is_virtual,#interf dev]
	      val labels = [ "" ,"  id=", " input="," output="," opened="," virtual="," interf="]
	in
	    print ( ( ListPair.foldr (fn (a,b,accu) => a^b^accu) "" (labels,to_show) ) ^ "\n" )
	end

(* show all devices to human *)
fun showDevices () =  List.app showDevice ( listDevices() )

(* get device id  : 'what' is  "#output | #input | #opened | #interf" 
*)
fun getDeviceId name what  = let
    val devices = listDevices()
    val dev = List.filter (fn x => (#name x) = name andalso (what x)) devices 
in
    if  List.null dev then [~1] else List.map #id  dev
end

fun getDeviceInputId name = List.hd (getDeviceId name #input)

fun getDeviceOutputId name = List.hd (getDeviceId name #output)

				     
(* return device id *)
fun createVirtualOutput name  = 
     Pm_CreateVirtualOutput (name, "ALSA", mem_null)
       				
fun createVirtualInput name  = 
      Pm_CreateVirtualInput (name, "ALSA", mem_null) 
   	

fun deleteVirtualDevice id =  Pm_DeleteVirtualDevice(id)


(* if latency > 0, we need a time reference. If none is provided,
       use PortTime library - comment extracted from portmidi.c - 
*)

type error_stream = {pm_error:int, pm_stream:MLton.Pointer.t}
						    
					    
fun openOutput id buffer_size latency :error_stream =
    let val stream = ref mem_null
	val error = Pm_OpenOutput (stream, id,MLton.Pointer.null, buffer_size , mem_null, mem_null,latency)
in
  {pm_error=error, pm_stream= !stream }
end

(* test one note immediate

val c3 = message (0x90, 60, 100);
val out_stream = openOutput 4 100 0; (* latence = 0 *)
writeShort  out_stream 0 c3;

(*  note-off *)
val c3' = message (0x80, 60, 0);
writeShort out_stream 0 c3';


*)

		   
fun openInput id buffer_size :error_stream =
    let val stream = ref mem_null
	val error =  Pm_OpenInput (stream, id, mem_null, buffer_size , mem_null, mem_null)
in
   {pm_error=error, pm_stream = !stream}
end

(* test 
val in_stream = openInput 3 100;

val buf2 =  bufferNew 4; (* 2 notes on  off *)

read  in_stream buf2 4;

poll in_stream; 

read  in_stream buf2 4;

*)

				   
fun message (status, data1, data2) =
    Word.toInt (
	Word.orb (
	    Word.orb(Word.andb((Word.<< (Word.fromInt data2, 0w16)), 0wxFF0000),
			Word.andb(Word.<< (Word.fromInt data1, 0w8),0wxFF00)),
	    Word.andb(Word.fromInt status,0wxFF))
    )

(* 
writeShort  out_stream 0  ( message (0x80, 60, 0) );
 *)
fun writeShort out_stream  when  msg  =  Pm_WriteShort (out_stream,when, msg)
 

(*
create sysex from hex blank separated string
val syx = createSysex "F0 00 21 1D 01 01 1F F7";
writeSysex out_stream 0 syx;
 *)

fun createSysex text = let
    val ltext = String.tokens Char.isSpace text
    val listWord8 = List.map (fn x => valOf(Word8.fromString x)) ltext
in
   Array.fromList listWord8
end

			
fun writeSysex out_stream when sysex = Pm_WriteSysEx (out_stream,when,sysex)

fun messageStatus msg = Word8.fromInt msg
fun messageData1 msg = Word8.fromLarge (Word.toLarge (Word.~>>(Word.fromInt msg,0w8)))
fun messageData2 msg = Word8.fromLarge  (Word.toLarge (Word.~>>(Word.fromInt msg,0w16)))
fun messageData3 msg = Word8.fromLarge  (Word.toLarge (Word.~>>(Word.fromInt msg,0w24)))
				  
fun messageType msg =  Word8.fromLarge  (Word.toLarge  (Word.andb (Word.fromInt msg, 0wxF0)))
				  
(*
  poll  in_stream;
*)
fun poll in_stream = (Pm_Poll in_stream) = 1

(*
type PmEvent = {
    message : int,
    timestamp : int
}
  
*)
		 
type My_Buffer = int array

(* buffer needed for read and write events by packets 
we can also use Array.fromList cf test below
 event struct len = 4xchar + 1xint32 *)
fun bufferNew taille = Array.array(2*taille, 0)
fun bufferElt buf place = ArraySlice.slice(buf,place * 2, SOME 2)  
(* buffer is considered as  (int32*int32) pointer array on C side *)
fun bufferSize buf = Array.length buf div 2
					      
type my_event = int*int (* tuple seem more convenient than array or list *)
fun bufferSet buffer index  (ev :my_event) =
    let val (msg,ts) =  ev
	val my_index = index * 2
    in
	Array.update (buffer,my_index,msg);
	Array.update (buffer,my_index+1,ts)
    end
	


(* test
val notes = bufferNew 2 ;
val _ = bufferSet notes 0 ( message(0x90,60,100),1000 ) ;
val _ = bufferSet notes 1 ( message(0x80,60,0),1200 ) ;
val err = write out_stream notes 2;
*** or ***
val buf_out = Array.fromList  [ message(0x90,60,100),1000
			    , message(0x80,60,0),1200 
			     ,message(0x90,80,50),1300
                             ,message(0x80,80,0),1300 ];
val err = write out_stream buf_out (bufferSize buf_out);
*)
		     
(* Pm_Write PortMidiStream *stream, PmEvent *buffer, long length ); *)
type buffer1 = Word8.word array 
fun write out_stream buffer len = Pm_Write(out_stream , (buffer : My_Buffer) ,len)
fun write1 out_stream buffer = My_Pm_Write1(out_stream , (buffer : buffer1) ,1)

fun read in_stream buffer len = Pm_Read(in_stream, (buffer : My_Buffer), len)
fun read1 in_stream buffer = My_Pm_Read1(in_stream, (buffer : buffer1), 1)

fun setFilter (in_stream, filter) = Pm_SetFilter(in_stream,filter)

(* logior list *)
fun logior list_word = List.foldr (fn (x,acc) => Word.orb (x,acc)) 0w0  list_word

(* filter for input stream I don't see effect on output *)
fun filter elem = Word.<< (0wx1,elem) 
				  
val filt_active = filter 0wx0E 
val filt_sysex = filter 0wx00 
val filt_clock = filter 0wx08
val filt_play = logior [filter 0wx0a,filter 0wx0C,filter 0wx0B] 
val filt_tick = filter 0wx09 
val filt_fd = filter 0wx0D
val filt_undefined = filt_fd
val filt_reset = filter 0wx0F
val filt_realtime =  logior [filt_active, filt_sysex,filt_clock, filt_play, filt_undefined, filt_reset,filt_tick]
val filt_note = logior [filter 0wx19,filter 0wx18] 
val filt_channel_aftertouch = filter 0wx1D
val filt_poly_aftertouch = filter 0wx1A
val filt_aftertouch = logior [filt_channel_aftertouch, filt_poly_aftertouch]
val filt_program = filter 0wx1C
val filt_control = filter 0wx1B 
val filt_pitchbend = filter 0wx1E 
val filt_mtc = filter 0wx01
val filt_song_position = filter 0wx02 
val filt_song_select = filter 0wx03
val filt_tune = filter 0wx06
val filt_systemcommon = logior [filt_mtc, filt_song_position, filt_song_select, filt_tune]

(* 16 bits mask *)
fun pmChannel channel =  Word.<< (0wx1,channel)
(* 
setChannelMask  (in_stream, pmChannel 0wx0);

" Note that channels are numbered 0 to 15 (not 1 to 16). Most 
    synthesizer and interfaces number channels starting at 1, but
    PortMidi numbers channels starting at 0."
*)
fun setChannelMask (in_stream, mask) =  Pm_SetChannelMask (in_stream, mask)

						    
(* portTime start *)
fun ptStarted () = Pt_Started () = 1 
fun ptStart resolution = Pt_Start (resolution,mem_null,mem_null)
fun ptStop () = Pt_Stop ()
fun ptTime () = Pt_Time ()
fun ptSleep duration = Pt_Sleep (duration)
    
				
end

		
