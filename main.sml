open Portmidi;

fun testWrite id_out =
    let val _ = if ptStarted () then ptStop() else 0
	val _ = ptStart 1
        val () = print ( concat [" Time is : ", Int.toString (ptTime()),"\n"])
	val kout = openOutput id_out 100 1
	val my_time = ptTime()
	val buf = Array.fromList  [ message(0x90, 60,100),my_time
				    , message(0x90, 64, 100),my_time
				    , message(0x90, 67, 100),my_time
				    , message(0x80, 60, 0),my_time + 1000
			  	    , message(0x80, 64, 0),my_time + 1000
				    , message(0x80, 67, 0),my_time + 1000]
	val len  = bufferSize buf 
	val () = print ("send " ^ Int.toString len ^ " events  to midi output \n")

	in
	    ( write kout buf len ;
	      ptSleep 2000;
	      print "C chord played\n\n";
	      pmClose kout
	    )
    end
	      
fun testRead in_stream =		     
    let 
	val _ = print "\nplay a midi note and press enter";
	val _ = TextIO.inputLine TextIO.stdIn;
	val buf2 =  bufferNew 4;
	val ji = read in_stream buf2 4;
	val msg_ts = bufferElt buf2 0;
	val msg = ArraySlice.sub(msg_ts,0);
	val status = messageStatus  msg;
	val data1 = messageData1  msg
    in
	 print ("status=" ^ Word8.toString status ^ " data1=" ^ Word8.toString data1 ^ "\n")
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
	val outs = List.filter  #output (listDevices())
	val ins = List.filter  #input (listDevices())
	val _ =  List.app showDevice outs 
	val (id_out, id_out_str) = getNumber "\nPlease enter the id of your output : "
	val () = print "\n"
	val _ =  List.app showDevice ins
	val (id_in, id_in_str) = getNumber "\nPlease enter the id of your input : "
	val out_info = valOf (getDeviceInfo id_out)
	val in_info = valOf (getDeviceInfo id_in)
	val in_stream = openInput id_in 100				   
	in				 
     ( 
       print ( " name: " ^ #name out_info ^ " => id out:" ^ id_out_str );
       testWrite id_out;
       print ( " name: " ^ #name in_info ^ " => id in:" ^ id_in_str );
       testRead in_stream;
       ptSleep 500;
       terminate();
       ()
	     
	     )
 end


do main()








		     
