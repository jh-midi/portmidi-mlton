open Portmidi;

fun testWrite id_out =
    let val _ = if ptStarted () then ptStop() else 0
	val _ = ptStart 1
        val () = print ( concat [" Time is : ", Int.toString (ptTime()),"\n"])
	val {pm_error,pm_stream}:error_stream = openOutput id_out 100 1
	val _ = if pm_error < 0 then
		    raise Fail ( getErrorText  pm_error ^ " " ^ Int.toString id_out ^ " for output\n")
		else ""
	val my_time = ptTime()
	val buf = Array.fromList  [ message(0x90, 60,100),my_time
				    , message(0x90, 64, 100),my_time
				    , message(0x90, 67, 100),my_time
				    , message(0x80, 60, 0),my_time + 1000
			  	    , message(0x80, 64, 0),my_time + 1000
				    , message(0x80, 67, 0),my_time + 1000]
	val len  = bufferSize buf 
	val () = print ("send " ^ Int.toString len ^ " events  to midi output \n")
	val () = print ("message=" ^ Int.toString (message(0x90, 60,100)) ^
			" time=" ^ Int.toString ( my_time + 1000) ^ "\n")

	in
	    ( write pm_stream buf len ;
	      ptSleep 2000;
	      print "C chord played\n\n";
	      close pm_stream
	    )
    end
	      
fun testRead in_stream =		     
    let 
	val _ = print "\nplay a midi note and press enter";
	val _ = TextIO.inputLine TextIO.stdIn;
	val buf1: Word8.word array = Array.array (8,0wx0)
	val ji1 = read1 in_stream buf1
	val buf4 =  bufferNew 4;
	val ji = read in_stream buf4 4;
	val msg_ts = bufferElt buf4 0;
	val msg = ArraySlice.sub(msg_ts,0);
	val status = messageStatus  msg;
	val data1 = messageData1  msg;

    in
	(
	  print (concat( ["from read1 status=" ,
			 Word8.toString (Array.sub(buf1,0)),
			 " data1=" , Word8.toString (Array.sub(buf1,1)),
			 " data2=" , Word8.toString (Array.sub(buf1,2)),
			 " data3=" , Word8.toString (Array.sub(buf1,3)),
			 "\n"]));
			 
	  print ("from read status=" ^ Word8.toString status ^ " data1=" ^ Word8.toString data1 ^ "\n") 
		)
    end



(* getNumber "Please enter the id of your output : " *)
fun getNumber msg = (
print msg;
let
    val str = valOf (TextIO.inputLine TextIO.stdIn)
    val i : int = valOf (Int.fromString str)
in
   (i,str)
end
)

fun main() =
    let val _ = initialize()
	val () = print ("\n\ntest begin! \n")
	val () = print ("error text example => " ^ getErrorText ~9997 ^ "\n")
	val _ = writeShort mem_null 0 (message (0x90, 60, 100))
	val outs = List.filter  #output (listDevices())
	val ins = List.filter  #input (listDevices())
	val _ =  List.app showDevice outs 
	val (id_out, id_out_str) = getNumber "\nPlease enter the id of your output : "
	val () = print "\n"
	val _ =  List.app showDevice ins
	val (id_in, id_in_str) = getNumber "\nPlease enter the id of your input : "
	val out_info = case getDeviceInfo id_out of
			   NONE => raise Fail ("output device not found at id= " ^ Int.toString id_out ^ "\n")
			|  SOME out_info => out_info
	val in_info =  case getDeviceInfo id_in of
			   NONE => raise Fail ("input device not found at id= " ^ Int.toString id_in ^"\n" )
			|  SOME in_info => in_info
	val in_stream:error_stream  = openInput id_in 100
	val err= if #pm_error in_stream < 0 then
		     raise Fail ( getErrorText ( #pm_error in_stream) ^ " " ^ Int.toString id_out ^ " for input\n")
		 else ""
	in				 
     ( 
       print ( " name: " ^ #name out_info ^ " => id out:" ^ id_out_str );
       testWrite id_out;
       print ( " name: " ^ #name in_info ^ " => id in:" ^ id_in_str );
       testRead (#pm_stream in_stream);
       ptSleep 500;
       print ("has host error =>" ^ Bool.toString (hasHostError (#pm_stream in_stream)) ^ "\n");
       print ("error text =>" ^ getHostErrorText 100 ^ "end\n");
       terminate();
       ()
	     
	     )
 end


do main()








		     
