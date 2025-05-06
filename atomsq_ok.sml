structure AtomSQ =
struct
       
open Portmidi;
open Init;
open Display;
open Pitch;



(* velocity can be changed  *)
val velocity = ref 100
		   
fun noteOn note  =
    message (0x90,note,!velocity)

fun noteOff note  =
    message (0x80,note,0)
	    

type key = {  push: pitch,
	       push_midi: int,
	       push_on: int,
	       push_off: int,
	       draw: pitch,
	       draw_midi: int,
	       draw_on: int,
	       draw_off: int}

val transpose = 24	    
fun makeKey (push: pitch) (draw: pitch) :key =
    let val midi_push = ( pitch2midi push) + transpose
	val midi_draw = (pitch2midi draw) + transpose
    in
    {push =  push,
     push_midi= midi_push,
     push_on = noteOn midi_push,
     push_off= noteOff midi_push,
     draw=  draw,
     draw_midi= midi_draw,
     draw_on= noteOn midi_draw,
     draw_off= noteOff midi_draw}
    end
      

(*
make row of 24 buttons and take the first 14 
val rd = makeRow([(D,0),(F,0),(A,0),(B,0)],14);
val rc = makeRow([(C,0),(E,0),(G,0)],14);
*)
fun makeRow (tune :pitch list, n) =
    let val all_listed = List.tabulate (8,
		fn (xn) =>		   
		   List.map(fn(pitch1) => (#1 pitch1, #2 pitch1 + xn)) tune)
    in
	List.take(List.concat all_listed,Int.min(n,24)) (* 24 is the max len *)
    end

	
type row_tunings = {
    accord : string,
    position : int,
    color : int list,
    push2 : pitch list,
    draw2: pitch list,
    push1 : pitch list,
    draw1: pitch list
}

    
val sol_do = {
    accord = "Sol/Do - GC",
    position = Display.TopRight,
    color = [100,0x00,0x50],
    push2 =  makeRow([(G,0), (C,1), (E,1)],14),
    draw2 =  makeRow([(D,1), (F,1), (A,1), (B,1)],14),
    push1 =  makeRow([(D,0), (G,0), (B,0)],14),
    draw1 =  makeRow([(A,0), (C,1), (E,1), (Fs,1)],14) 
	     }

val si_do =  {
    accord = "Si/Do -  BC",
    position = Display.TopLeft,
    color = [0,0x6d,0],   
    push2 =  makeRow([(G,0), (C,1), (E,1)],14),
    draw2 =  makeRow([(D,1), (F,1), (A,1), (B,1)],14),
    push1 =  makeRow([(Fs,0), (B,0), (Ds,1)],14),
    draw1 =  makeRow([(Cs,1), (E,1), (Ab,1), (Bb,1)],14) 
	     }
    
val keyboard : key array =  Array.array (68, makeKey (C,0) (D,0)) (* on atom the keys go from 0 to 67 *)
val actives = BoolArray.array (68, false) (* set and check actives keys *)
					
(* fill the 14 keys
all notes are prepared for better latency
54 + (0 ... 13) = max 67
fillKeyboard sol_do

*)
fun fillKeyboard (tuning : row_tunings) =
    let val row2_col = ref 54  (* first note top row  *)
	val row1_col = ref  38 (* first note lower row *)
	val pos = ref 0 (* position *)
    in
	while !pos < 14 do (
	    let val push2 =  List.nth (#push2 tuning, !pos)
		val draw2 = List.nth (#draw2 tuning, !pos)
		val push1 =  List.nth (#push1 tuning, !pos)
		val draw1 = List.nth (#draw1 tuning, !pos)
	    in
		Array.update (keyboard, !row2_col, makeKey push2 draw2);
		Array.update (keyboard, !row1_col, makeKey push1 draw1);
		pos := !pos + 1;
		row2_col := !row2_col + 1;
		row1_col := !row1_col + 1
	    end
	)
    end

(* show notes ctrls key label on atomSQ top left
00 -> A
01 -> B
02 -> C
03 -> D
04 -> E
05 -> F
06 -> G
07 -> H

6F -> stop -> sharp
6D -> Play -> flat
 *)


		      
fun showNote ( ts, (note: pitch), velocity) =
    let val sharp =0x6B
	val flat = 0x6D
	val (note,_) = note
	val (ctrl_number, alteration) =
	    case note of
		   C => (2,0)
		|  Cs => (2,sharp)
		| Db => (3,flat)
		| D => (3,0)
		| Ds => (3,sharp)
		| Eb => (4,flat)      
		| E => (4,0)
		| F => (5,0)
		| Fs => (5,sharp)
		| Gb => (6,flat)
		| G => (6,0)
		| Gs => (6,sharp)
		| Ab => (0,flat)
		| A => (0,0)
		| As => (0,sharp)
		| Bb => (1,flat)
		| B => (1,0)
    in
	(writeShort ATOMSQ_out ts (message (0xB0,ctrl_number,velocity));
	if alteration > 0 then
	    writeShort ATOMSQ_out ts (message (0xB0,alteration,velocity))
	else 0
	)
    end

val pushG = ref false

fun pushOn nkey =
    # push_on (Array.sub(keyboard,  nkey))
      
fun pushOff nkey =
    # push_off (Array.sub(keyboard,  nkey))

fun drawOn nkey =
    # draw_on (Array.sub(keyboard,  nkey))

fun drawOff nkey =
    # draw_off (Array.sub(keyboard,  nkey))
      
fun  getPush nkey =
     # push (Array.sub(keyboard,  nkey))

fun  getDraw nkey =
     # draw (Array.sub(keyboard,  nkey))


		  
fun play nkey ts =
    (
      BoolArray.update(actives, nkey,true);
      if !pushG then
	(  writeShort DIATO_out ts (pushOn nkey);
	  showNote ( ts, getPush nkey, 0x7F) )
      else
	(  writeShort DIATO_out ts (drawOn nkey);
      showNote (ts, getDraw nkey, 0x7F ) )
    )
							     

fun mute nkey out_id ts =
    (
      BoolArray.update(actives, nkey,false);
      if !pushG then
	(  writeShort DIATO_out ts (pushOff nkey);
	   showNote (ts, getPush nkey,0x00) )
      else
	 ( writeShort DIATO_out ts (drawOff nkey);
	   showNote (ts, getDraw nkey,0x00) )
    )


fun getActive nkey =
    BoolArray.sub (actives, nkey)

fun noteChange (nkey, ts) =
    if getActive nkey
    then
	if !pushG then (
	    writeShort DIATO_out ts (pushOff nkey);
	    showNote (ts, getPush nkey,0x00); (* unlit key *)
	    writeShort DIATO_out ts  (drawOn nkey);
	    showNote (ts, getDraw nkey,0x7F) (* lite key *)
	)
	else (
	    writeShort DIATO_out ts (drawOff nkey);
	    showNote (ts, getDraw nkey,0x00);
	    writeShort DIATO_out ts (pushOn nkey);
	    showNote (ts, getPush nkey,0x7F)
	)
    else 0


val row1_min = 38
val row2_min = 54
val row1_max = 51
val row2_max = 67
		   
fun noteChangeAll  ts =
    let val r1 = ref row1_min
	val r2 = ref row2_min
    in
	while !r1 <= row1_max do (
	    noteChange (!r1, ts);
	    r1 := !r1 + 1
	);
	while (! r2) <= row2_max do (
	    noteChange (!r2,  ts);
	    r2 :=  !r2 + 1
	)
    end


fun setProgrammable  ok =
    let val _ =  initialize()
	val _ = if ptStarted () then 0 else ptStart 1
	val make_on = message (0x8F, 0x00, 0x7F)
	val make_off = message (0x8F, 0x00, 0x00)
    in (
	writeShort ATOMSQ_out (ptTime()) (if ok then make_on else make_off)
    )
    end

fun allNoteOff ts = (
    writeShort DIATO_out ts (message ( 0xB0,0x7B,  0x00)); (* all notes  off *)
    writeShort DIATO_out (1 + ts) (message (0xB0, 0x78, 0x00)) (* all sound off *)
	)		 


(* for showing the actual keyboard configuration *)
fun setActive pos =
    Display.actif := pos + 3

fun getActive () =  !Display.actif

(* showActif ATOMSQ_out *)
fun showActif  key_config =
    (
      sendText ( Int 32, getActive(), [0,0,0], Centered); (* clear old *)
      setActive  (#position key_config); (* set new *)
      sendText ( Int note_glyph, getActive(), #color key_config, Centered)
    )



(* simple increment and decrement relative *)
(* at 65 step is 1 and 66=>2 127= step is 63 *)
datatype knob = Knob of Word8.word * int
val knob_step = ref 1

(* knob from 1 to 8
 but array of 0 to 29 for direct access of
 0x0E = 14 and 0x1D = 28 for absolute  *)
val mem_knobs = Array.array(30, 0);
(* knob top right *)
		    
val relative = ref true
val bank =  ref 0

(* 64 is the middle of relative knob *)
	     
fun setKnobStep  step_increment =
   (
     knob_step := !knob_step + step_increment;
     if !knob_step > 0 andalso !knob_step < 63
     then 
	 sendText ( String ("knob step = " ^ Int.toString (!knob_step)),
		   Middle, [0,50,50], Centered) 
     else (
	  if !knob_step > 63 then knob_step := 63
	  else if !knob_step <= 0 then knob_step := 1
		      else ();
    	 print ("I have knob steps limits !\n");1
	       )
)	       

fun getKnob (encoder:int):int = Array.sub(mem_knobs, encoder)
fun setKnob (encoder, value) = Array.update(mem_knobs, encoder, value)

(* incr can be 1 to 64 *)
fun setMemKnob (encoder, value) =
    let val old_value =  getKnob encoder
    in
	(
	  let val new_value = getKnob encoder + value
	  in
	      if new_value > 127 then
		  setKnob(encoder, 127)
	      else if new_value < 0 then
		  setKnob(encoder, 0)
	      else  setKnob(encoder, new_value)
	  end
	)
    end

fun setEncoder encoder value ts =
    let val enc = Word8.toInt encoder
	val msg = if !relative then (* 8 banks for the encoder *)
		      if (value = 0wx01) then
			  message(0xB0 + !bank, enc,64 + !knob_step)
		      else
			  message(0xB0 + !bank, enc,64 - !knob_step)
		  else
		      if (value = 0wx01) then
			  (  setMemKnob(enc, !knob_step );
			     message(0xB0,enc,getKnob enc))
		      else
			  (  setMemKnob(enc, ~ (!knob_step));
			     message(0xB0,enc,getKnob enc))
				       
     in
	 writeShort DIATO_out ts msg
     end
(*
00 -> A
01 -> B
02 -> C
03 -> D
04 -> E
05 -> F
06 -> G
07 -> H
*)

	
fun showRelative () =
    let val knob_mode = if !relative then
		   "relative knob (User to change)"
	       else
		   "absolute knob (User to change)"
    in
	(sendText ( String knob_mode, Middle_, [100,50,50],  Centered) ;())
    end


    
fun  selectControl(buf,status,data1,data2,ts) =
     let fun isBankButton ()  = data1 >= 0wx00 andalso data1 <= 0wx07 andalso data2 = 0wx7F
	 fun setBankLetter () = ( bank :=  Word8.toInt data1;
		   sendText ( String ( "Bank " ^ Char.toString (chr (ord #"A" + !bank))),
			     BottomCenter,[100,0,80], Centered );())
		   		   
	 fun setTuning' tun =  if  data2  = 0wx7F then
			     (allNoteOff  ts;
			      showActif  tun;
			      fillKeyboard tun
			     ) else ()
					
	 fun isEncoder () =  data1 >= 0wx0E andalso data1 <= 0wx15  orelse data1 = 0wx1D				
	 fun setKnobStep' valeur =  if data2 = 0wx7F then
					(setKnobStep  valeur;())  else ()
	 fun changeRelative () =  if data2 = 0wx7F then
				      (relative := not (!relative);showRelative())  else ()
     in
	 
     if status = 0wxB0 then
	 if isEncoder()  then  (setEncoder data1 data2 ts;() )
		 (*set bank for relative encoder *)
	 else if isBankButton() then setBankLetter()
	 else
	     case data1 of 0wx24 => setTuning' si_do
			 | 0wx26  => setTuning' sol_do
			 | 0wx2B =>  setKnobStep' 1
			 | 0wx2A => setKnobStep'  ~1
			 | 0wx23 => changeRelative()
				     
			 | _ => (write DIATO_out buf 1; ())
     else (write DIATO_out buf 1;())
     end

 fun discri buf =
    let val ts =  MLton.Pointer.getInt32(buf,1)
	val status =  MLton.Pointer.getWord8(buf,0)
	val data1 =  MLton.Pointer.getWord8(buf,1)
	val data2 =  MLton.Pointer.getWord8(buf,2)

    (*  monitoring *)
	val _ = print (concat [Int.toString ts,
		       " status: ", Word8.toString status,
		       " data1: ",Word8.toString data1,
		       " data2: ", Word8.toString data2, "\n"]) 
    			    
    in
	
	      if status = 0wx80 andalso data1 > 0wx25 then (* note off *)
		 ( mute (Word8.toInt data1) DIATO_out ts; 0)
	      else if status = 0wx90 then
		  case  data1 of  0wx25 => (if not (!pushG) (* pad 2 *)
					    then noteChangeAll  ts
					    else (); pushG:= true;2)
					       
				| 0wx24  =>  (if !pushG (* pad 1 *)
					      then noteChangeAll  ts
					      else (); pushG := false;1)
						 
				| _  => (play (Word8.toInt data1) ts;3)
	      else (
             selectControl(buf,status, data1,data2,ts);3)
    end 
	
end (* structure *)
	


	      
