open AtomSQ;
open Init;



fun main  tuning  =
    (
      fillKeyboard tuning;
      setProgrammable true; 
      ptSleep 100;
      setProgrammable true; 
      ptSleep 100;  (* I don't know why 2nd trying *)
      showActif tuning;
(* if args in cmd line then showDevices() else show nothing *)
      if List.null  (CommandLine.arguments())  then () else showDevices();
      showRelative();
      sendText (String ( #accord si_do), #position si_do, #color si_do, Centered);
      sendText (String ( #accord sol_do), #position sol_do, #color sol_do, Centered);
      
      let val _ = ()
	  val buf= malloc 8 
	  val _ = print "\n\n  ****   Play Atom SQ  ****\n"
	  val scan_latency' = Time.fromMicroseconds (Int.toLarge 80)
	  val err = setFilter(ATOMSQ_in,logior [filt_poly_aftertouch])
      in
	  while true do (
	      OS.Process.sleep scan_latency' ; (* near real time *)
	      if (read ATOMSQ_in buf 1) = 1
	      then discri  buf
	      else  5
			
	  )
		
      end
    )
   

    

val _ =  main si_do 

	 
